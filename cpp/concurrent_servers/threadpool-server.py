# Threaded socket server - accepting multiple clients concurrently, by
# dispatching them into a thread pool.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import argparse
from concurrent.futures import ThreadPoolExecutor
from enum import Enum # python has enums!
import socket
import sys

ProcessingState = Enum('ProcessingState', 'WAIT_FOR_MSG IN_MSG')


def serve_connection(sockobj, client_address):
    print('{0} connected'.format(client_address))
    sockobj.sendall(b'*')
    state = ProcessingState.WAIT_FOR_MSG

    while True:
        try:
            buf = sockobj.recv(1024)
            if not buf:
                break
        except IOError as e:
            break
        for b in buf:
            if state == ProcessingState.WAIT_FOR_MSG:
                if b == ord(b'^'):
                    state = ProcessingState.IN_MSG
            elif state == ProcessingState.IN_MSG:
                if b == ord(b'$'):
                    state = ProcessingState.WAIT_FOR_MSG
                else:
                    sockobj.send(bytes([b + 1])) # b is the ascii representation of the character byte?
            else:
                assert False

    print('{0} done'.format(client_address))
    sys.stdout.flush()
    sockobj.close()


if __name__ == '__main__':
    argparser = argparse.ArgumentParser('Threadpool server')
    argparser.add_argument('--port', type=int, default=9090, help='Server port')
    argparser.add_argument('-n', type=int,
                           default=64, help='Number of threads in pool')
    args = argparser.parse_args()

    # create the listening socket. Just 4 lines!
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sockobj.bind(('localhost', args.port))
    sockobj.listen(15) # 15 is the backlog, s_backlog in our c++ example

    try:
        with ThreadPoolExecutor(args.n) as pool:
            while True:
                # sockobj.accept() gives the socket which was connected, and also the info
                # of the connection straightaway. that means we don't have to do getnameinfo!
                # python for the win!
                client_socket, client_address = sockobj.accept()
                pool.submit(serve_connection, client_socket, client_address) # this doesn't block!
    except KeyboardInterrupt as e:
        print(e)
        sockobj.close()














