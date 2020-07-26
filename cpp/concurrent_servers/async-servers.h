#pragma once

#include <sys/select.h> // select call, duh
#include <sys/socket.h>
#include <netinet/in.h>

#include <array>

// code for both select server and async server is going to be here
class AsyncServer
{
    public:
    AsyncServer(int portnum)
        : m_portnum(portnum)
    {
        FD_ZERO(&m_readfds);
        FD_ZERO(&m_writefds);
    }

    // run based on select system call
    void select_run();

    // run based on epoll system call
    void epoll_run();

    private:

    // this is the return value of all the callbacks
    struct Notify
    {
        bool want_read = false;
        bool want_write = false;
    };

    // callbacks
    // All the callback return Notify struct telling the main loop what to
    // do with this socket fd.
    Notify on_new_connection(int sockfd, const sockaddr_in& addr, socklen_t len);
    Notify on_ready_to_read(int sockfd);
    Notify on_ready_to_write(int sockfd);

    // depending upon what is set in the Notify struct, add/remove from the
    // global set of fds
    void processNotify(int sockfd, Notify notify);

    int m_portnum;
    static const int s_backlog = 64;

    // our global set of fds
    fd_set m_readfds;
    fd_set m_writefds;

    // state of each file descriptor
    // Should we change this to a vector to allocate less memory?
    enum class State
    {
        InitialAck,
        WaitForMsg,
        InMsg
    };
    struct FDData
    {
        static const int s_buffer_size = 1024;

        char buffer[s_buffer_size]; // data to be written
        int end;   // till what point the data is to be written
        int start; // where to write the data from
        State state;
    };


    // all the fields of the m_fddata have to be initialized
    // when the new socket is connected
    std::array<FDData, FD_SETSIZE> m_fddata; // FD_SETSIZE is 1024 on my machine
};
