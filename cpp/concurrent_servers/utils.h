#pragma once

#include <sys/socket.h>
#include <netinet/in.h>

#include <fmt/ostream.h> // fmt::print
#include <fmt/time.h>    // time related formatting

#include <sys/time.h> // gettimeofday

// Directly from https://github.com/eliben/code-for-blog/blob/master/2017/async-socket-server/utils.h

// print the error message 'msg' then the perror status and then exit
void perror_die(const char* msg);

int create_listening_socket(int portnum, int backlog);

void report_connected_peer(const sockaddr_in* sa, socklen_t salen);

// This contains the core logic of the state machine
// sockfd is the socket file descriptor of the connection that has been accepted by the server
void serve_connection(int sockfd);

// mark a socket non blocking
// crashed if it is not possible
void mark_non_blocking(int sockfd);

// prints the timestamp (microsecond precision) along with the message
template <typename... Args>
void timed_print(const char *format, Args&& ...args) {
    struct timeval tv;
    if (gettimeofday(&tv, nullptr) < 0)
        perror_die("error getimeofday");

    std::time_t t = tv.tv_sec; // get the seconds since epoch
    fmt::print_colored(fmt::RED, "{:%Y-%m-%d %H:%M:%S}.{:d}: ", *std::localtime(&t), tv.tv_usec);
    fmt::print(format, std::forward<Args>(args)...);
}
