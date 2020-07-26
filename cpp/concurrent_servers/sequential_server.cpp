#include "sequential_server.h"
#include "utils.h"

#include "fmt/ostream.h"
#include <sys/socket.h>
#include <netinet/in.h>

#include <string.h> // memset

void SequentialServer::run()
{
    // create a socket to listen on the port number portnum_
    int sockfd = create_listening_socket(portnum_, s_backlog);

    // server is always accepting new connections - has to be killed via an external command like SIGKILL
    while (true)
    {
        // peer_addr is where the socket address of the connection will be stored
        sockaddr_in peer_addr;

        // peer_addr_len is passed to accept call telling it that no more this many number of bytes
        // should be read. If less than this many number of bytes are read, return them in this
        // variable
        socklen_t peer_addr_len = sizeof(peer_addr); // socklen_t is 'unsigned int'

        // accept takes sockaddr instead of sockaddr_in
        // but sockaddr_in can be cast to sockaddr. More details:
        // http://beej.us/guide/bgnet/output/html/singlepage/bgnet.html#structs
        //
        // If no pending connections are present on the queue, and the socket is not marked as nonblocking,
        // accept() blocks the caller until a connection is present.
        // If the socket is marked nonblocking and no pending connections are present on the queue,
        // accept() fails with the error EAGAIN or EWOULDBLOCK.
        //
        // In our case, we have not marked our socket as non-blocking, so it we will block here till we
        // receive a connection
        int newsockfd = accept(sockfd, (struct sockaddr*)&peer_addr, &peer_addr_len);
        if (newsockfd < 0)
            perror_die("error on accept");

        report_connected_peer(&peer_addr, peer_addr_len); // log the hostname and port of who has connected us
        serve_connection(newsockfd);
    }
}
