#include "utils.h"

#include <netdb.h>  // NI_MAXHOST, NI_MAXPORT
#include <unistd.h> // close system call

#include <stdio.h>
#include <stdlib.h>

#include <fcntl.h> // F_GETFL

void perror_die(const char* msg)
{
    perror(msg);
    exit(EXIT_FAILURE);
}

// this is specially for a socket which has to listen to something
int create_listening_socket(int portnum, int backlog)
{
    // create a socket first via socket system call
    // - AF_INET = domain - what kind of socket we are interested in (4 vs 6)
    // - SOCKET_STREAM = type: TCP
    // - protocol = just set to 0, and it will automatically figure out
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0)
        perror_die("error opening socket");

    // Add the socket option SO_REUSEADDR to avoid the error EADDRINUSE while connecting to the socket
    int opt = 1; // to enable the option
    if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0)
        perror_die("error setsockopt");

    // create the address on which others can connect to us
    // - this is essentially server's ip address
    sockaddr_in serv_addr; // can we value initialize this?
    memset(&serv_addr, 0, sizeof(sockaddr_in)); // we are still writing C, remember?
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = INADDR_ANY; // any ip address on the given port number
    serv_addr.sin_port = htons(portnum); // host to network

    // bind the socket created above with the ip address which we just created
    // system calls like accept, bind take sockaddr instead of sockaddr_in, so casting
    if (bind(sockfd, (sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
        perror_die("bind error");

    // start listening on this socket now
    // and we are done!
    if (listen(sockfd, backlog) < 0)
        perror_die("listen error");

    return sockfd;
}

void report_connected_peer(const sockaddr_in* sa, socklen_t salen)
{
    char hostbuf[NI_MAXHOST] {};
    char portbuf[NI_MAXSERV] {};

    // print the hostname and port of the given socketaddr
    if (getnameinfo((sockaddr *)sa, salen, hostbuf, NI_MAXHOST, portbuf, NI_MAXSERV, 0) < 0)
        perror_die("error getnameinfo");

    fmt::print("peer ({} {}) connected\n", hostbuf, portbuf);
}

// This contains the core logic of the state machine
// sockfd is the socket file descriptor of the connection that has been accepted by the server
void serve_connection(int sockfd)
{
    enum class State
    {
        Wait_for_msg,
        In_msg
    };

    // first send a '*' to the client notifying them that we have accepted their connection
    if (send(sockfd, "*", 1, 0) < 0)
        perror_die("error send");

    // now in a tight loop, keep on receiving the data and sending back appropriate data
    State current_state = State::Wait_for_msg;
    while (true)
    {
        uint8_t buffer[1024] {};
        int len = recv(sockfd, buffer, sizeof(buffer), 0);
        if (len < 0)
        {
            // an error has occured, should we break or die?
            // let's die
            perror_die("error recv");
        }
        else if (len == 0)
        {
            // For TCP sockets, the return value 0 means the peer has
            // closed its half side of the connection.
            break;
        }

        // buffer has len bytes of data
        for (int i = 0; i < len; ++i)
        {
            switch (current_state)
            {
                case State::Wait_for_msg:
                    if (buffer[i] == '^') // everything else is ignored
                        current_state = State::In_msg;
                    break;
                case State::In_msg:
                    if (buffer[i] == '$')
                        current_state = State::Wait_for_msg;
                    else
                    {
                        buffer[i] += 1;
                        if (send(sockfd, &buffer[i], 1, 0) < 0)
                        {
                            perror("send error");
                            close(sockfd); // error checking?
                            return; // nothing much to do with this connection now
                        }
                    }
                    break;
            }
        }
    }

    // close the connection with the client
    // We come here if len == 0 in the recv which happens if client has closed her side
    // of the connection
    if (close(sockfd) < 0)
        perror_die("error close");

    fmt::print("peer done\n");
}

void mark_non_blocking(int sockfd)
{
    // get the existing flags
    int flags = fcntl(sockfd, F_GETFL, 0);
    if (flags == -1) {
        perror_die("fcntl F_GETFL");
    }

    // set the non blocking flag
    if (fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) == -1) {
        perror_die("fcntl F_SETFL O_NONBLOCK");
    }
}
