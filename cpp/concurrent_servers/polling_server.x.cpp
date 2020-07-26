#include "utils.h"
#include <unistd.h> // close system call

#include <fmt/printf.h>
#include <fmt/time.h>

#include <thread>
#include <chrono>

// one shot server. Only accepts one client! For exhibition purposes only!
class PollingServer
{
    public:
    void run()
    {
        int sockfd = create_listening_socket(9090, 64); // this dies out if there is an error at any point
        sockaddr_in peer_addr;
        socklen_t peer_addr_len = sizeof(peer_addr); // socklen_t is 'unsigned int'
        int newsockfd = accept(sockfd, (struct sockaddr*)&peer_addr, &peer_addr_len);
        if (newsockfd < 0)
            perror_die("error on accept");

        // set newsockfd to be non-blocking
        mark_non_blocking(newsockfd);

        // do recv calls in a loop
        while (true)
        {
            char buffer[1024] {}; // value initialization
            timed_print("{}", "calling recv...\n");
            int len = recv(newsockfd, buffer, sizeof(buffer), 0);
            if (len < 0)
            {
                // check the errno
                if ((errno == EAGAIN) || (errno == EWOULDBLOCK)) // check for both
                {
                    using namespace std::chrono_literals;
                    std::this_thread::sleep_for(200ms); // 200ms! C++ for the win!
                    continue;
                }
            }
            else if (len == 0)
            {
                // client has disconnected, closed the socket
                close(newsockfd); // no error checking?
            }
            else
            {
                timed_print("received {} bytes of data\n", len);
                // print how many bytes we have received along with the time
            }
        }
    }
};

int main()
{
    PollingServer server;
    server.run();
}
