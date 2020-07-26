# Tests a concurrent server, by connecting multiple clients sending pre-set
# messages, and comparing the echoes with expected values.
#
# Run with -h for full usage.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import argparse
import logging
import subprocess
import threading
import time
import itertools

import queue
import socket
import sys
import inspect

# This function runs a server in a separate thread. It also takes a stop_event
# which can be signaled to terminate the process
def server_runner(path, args, stop_event):
    """Runs the server as a subprocess until stop is requested.

    Run this function in a separate thread!

    path is the path to the server to run, with the given args.
    If 'path' ends with .py, a python interpreter is prepended.
    The args have to be a (possibly empty) iterable.
    stop_event is a threading.Event object; when it's set, the subprocess is
    killed and this function returns.
    """
    runcmd = ['python3.6', '-u', path] if path.endswith('.py') else [path]
    runcmd.extend(args)

    logging.info('server_runner: executing subprocess "{0}"'.format(runcmd))
    proc = subprocess.Popen(runcmd) # this runs the shell command - now the server is up and running!

    logging.info('server_runner waiting for stop event')
    stop_event.wait()       # this is a blocking call
    logging.info('server_runner sending kill to subprocess')
    proc.terminate() # send sigterm to the process

    try:
        proc.wait(timeout=0.2) # we have sent SIGTERM above, now wait for the process to terminate
    except subprocess.TimeoutExpired:
        logging.info('server_runner: subprocess did not die within timeout')


def socket_reader(sockobj, outq, exit_event, client_name):
    """Reads from sockobj, 1 byte at a time; places results in outq.

    This function runs in a loop until the sockobj connection is closed or until
    exit_event is set.
    """
    while not exit_event.is_set():
        try:
            buf = sockobj.recv(1)
            if len(buf) < 1:
                break
            logging.info("{} received {}".format(client_name, buf));
            outq.put(buf)
        except socket.timeout:
            continue
        except OSError:
            break


def socket_sender(sockobj, buf):
    caller = inspect.stack()[1][3]; # third element in the stack frame is the name of the caller
    logging.info("{} sent {}".format(caller, buf));
    sockobj.send(buf)

def assert_queue_contains(q, val, timeout=0.1):
    try:
        v = q.get(timeout=timeout)
        assert v == val
    except queue.Empty:
        assert False, f'queue was empty with timeout={timeout}'


def assert_queue_empty(q, wait=0.1):
    time.sleep(wait)
    assert q.empty(), 'queue had {0} with wait={1}'.format(q.get(), wait)


def client_thread_runner(client_body_func, port, initial_timeout=0.1):
    """Abstracts the function running within a client thread.

    Connects to the port with a socket, launches a reading thread and makes sure
    to shut down properly. client_body_func is the actual interaction with a
    socket, once connected.
    """


    # create a socket to connect to the server
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM) # TCP socket
    # set a timeout on the blocking operation. If response not received within that interval
    # than an exception is thrown
    sockobj.settimeout(initial_timeout)

    sockobj.connect(('localhost', port))
    client_name = client_body_func.__name__;
    logging.info('{0} connected to server'.format(client_name))

    readq      = queue.Queue()
    exit_event = threading.Event()
    tread = threading.Thread(
            target=socket_reader,
            args=(sockobj, readq, exit_event, client_name))
    tread.start()

    try:
        client_body_func(sockobj, readq, initial_timeout)
    finally:
        # Closing the socket before killing the server helps the bound socket be
        # fully released on the server side; otherwise it may be kept alive by
        # the kernel for a while after the server process exits.
        sockobj.shutdown(socket.SHUT_RDWR)
        sockobj.close()
        exit_event.set()
        tread.join()


# client0 just checks that it did receive '*' from the server when it first connected
def client0(sock, readq, initial_timeout):
    # should we make the below call blocking so that client doesn't start to send/receive the
    # data before it receives *
    assert_queue_contains(readq, b'*', timeout=initial_timeout)
    assert_queue_empty(readq)


# client1 does more robust checking of the protocol mentioned on the page. It explores both
# the states of the server "wait for msg" and "in msg". But it sends one char at a time and checks
# the result
def client1(sock, readq, initial_timeout):
    assert_queue_contains(readq, b'*', timeout=initial_timeout)

    socket_sender(sock, b'abcdef')
    assert_queue_empty(readq)

    socket_sender(sock, b'^')
    assert_queue_empty(readq)

    socket_sender(sock, b'f')
    assert_queue_contains(readq, b'g')

    socket_sender(sock, b'1234')
    assert_queue_contains(readq, b'2')
    assert_queue_contains(readq, b'3')
    assert_queue_contains(readq, b'4')
    assert_queue_contains(readq, b'5')

    socket_sender(sock, b'$')
    assert_queue_empty(readq)
    socket_sender(sock, b'1234')
    assert_queue_empty(readq)

    socket_sender(sock, b'^')
    socket_sender(sock, b'xy')
    assert_queue_contains(readq, b'y')
    assert_queue_contains(readq, b'z')


# client2 continues the testing by sending a number of characters at the same moment
def client2(sock, readq, initial_timeout):
    assert_queue_contains(readq, b'*', timeout=initial_timeout)
    socket_sender(sock, b'^ab$^kl$^80$50')
    for b in [b'b', b'c', b'l', b'm', b'9', b'1']:
        assert_queue_contains(readq, b)
    assert_queue_empty(readq)


# client3 more testing
def client3(sock, readq, initial_timeout):
    assert_queue_contains(readq, b'*', timeout=initial_timeout)
    socket_sender(sock, b'^$^$^$^$^$^$$^$$$$foobarjoemoedoe^$$')
    assert_queue_empty(readq)


def test_main():
    # set up the basic command line argument parser
    argparser = argparse.ArgumentParser('Server test')
    argparser.add_argument('server_path', help='path to the server executable')
    argparser.add_argument('-p', '--server-port', default=9090, type=int,
                           help='the server listens on this port')
    argparser.add_argument('--timeout-bump', default=0.0, type=float,
                           help='amount of time (in sec) by which to bump the '
                                'timeout between consecutive clients')
    argparser.add_argument('-n', '--num-clients', default=2, type=int,
                           help='number of clients to launch simultaneously; ')
    argparser.add_argument('--loop', default=1, type=int,
                           help='launch test in a loop')
    args = argparser.parse_args()
    assert args.num_clients >= 1

    logging.basicConfig(
        level=logging.DEBUG,
        format='%(levelname)s:%(asctime)s:%(message)s')

    # Launch the server in a thread, listening on the port.
    # stop event is like a condition variable (cv) which can be used to terminate the process
    stop_event = threading.Event()
    server_thread = threading.Thread(
            target=server_runner,
            args=(args.server_path, [str(args.server_port)], stop_event))
    server_thread.start()
    time.sleep(0.3) # sleep for some time so that the server_thread can start and bring up the server

    # check here whether the server is actually up or not
    # get the server name from the server_path (it is the last subtring after /)
    server_name = args.server_path.split("/").pop()
    try:
        pid = subprocess.check_output(["pgrep", server_name]).strip().decode()
        logging.info("Server up with pid {}".format(pid))
    except subprocess.CalledProcessError:
        logging.error("Server was still not up. Exiting...")
        sys.exit(1);

    TIMEOUT = 0.5 + (args.num_clients - 1) * args.timeout_bump

    for i in range(args.loop):
        logging.info('** Test iteration {}'.format(i))

        # iterools.cycle will create an infinite iterator out of the given values
        # so no matter what the value of args.num_clients is, we always have functions
        # to put different clients onto
        client_iter = itertools.cycle([client0, client1, client2, client3])

        # save all the threads so that we can later do a join on them
        threads = []
        for _ in range(args.num_clients):
            # create a thread to do the socket work
            tester_thread = threading.Thread(
                    target=client_thread_runner,
                    args=(next(client_iter), args.server_port, TIMEOUT))
            tester_thread.start()
            threads.append(tester_thread)

        time.sleep(TIMEOUT)

        # wait for all the threads to finish in this iteration
        for thread in threads:
            thread.join()

    # after everything has been finished, kill the server via setting the server event (CV?)
    stop_event.set()

# __name__ is set to __main__ only it this is directly invoked
if __name__ == '__main__':
    test_main()
