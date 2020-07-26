#include "threaded_server.h"

#include "utils.h"
#include <thread>

void ThreadedServer::run()
{
    int sockfd = create_listening_socket(portnum_, s_backlog);
    while (true)
    {
        sockaddr_in peer_addr;
        socklen_t peer_addr_len = sizeof(peer_addr);

        int newsockfd = accept(sockfd, (struct sockaddr*)&peer_addr, &peer_addr_len);
        if (newsockfd < 0)
            perror_die("error on accept");

        report_connected_peer(&peer_addr, peer_addr_len);

        // now instead of going in an infinite loop serving the accepted sorted, create a thread
        // and let that thread server this socket. Here we are creating a separate thread for each
        // connection. Good idea?
        std::thread worker_thread (serve_connection, newsockfd);

        // Resources will be freed once the thread exits
        // http://en.cppreference.com/w/cpp/thread/thread/detach
        //
        // We know it will never become zombied because main thread always exits after the worker thread
        // (main thread never dies!)
        worker_thread.detach();
    }
}
