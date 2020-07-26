#include "async-servers.h"

#include "utils.h"
#include <unistd.h> // close system call

void AsyncServer::processNotify(int sockfd, Notify notify)
{
    // read
    if (notify.want_read)
        FD_SET(sockfd, &m_readfds);
    else
        FD_CLR(sockfd, &m_readfds);

    // write
    if (notify.want_write)
        FD_SET(sockfd, &m_writefds);
    else
        FD_CLR(sockfd, &m_writefds);

    // if the socket wants neither read, not write
    // close the socket
    if (!notify.want_read && !notify.want_write)
    {
        fmt::print("socket fd {} closing\n", sockfd);
        close(sockfd); // error checking?
    }
}

AsyncServer::Notify AsyncServer::on_new_connection(int sockfd, const sockaddr_in& addr, socklen_t len)
{
    assert(sockfd < FD_SETSIZE);

    // mark the new socket as non blocking because we are going to put
    // it into the set of fds
    mark_non_blocking(sockfd);

    // report that a new peer has joined
    report_connected_peer(&addr, len);

    // write the ack back in the buffer
    // should we write the ack directly back on the socket with blocking socket?
    // No! one should never use any blocking call in an async setup like this
    auto& data = m_fddata[sockfd];
    data.buffer[0] = '*';
    data.end = 1;
    data.start = 0;
    data.state = State::InitialAck;

    // data to send
    return {false, true};
}

AsyncServer::Notify AsyncServer::on_ready_to_read(int sockfd)
{
    assert(sockfd < FD_SETSIZE);

    auto& data = m_fddata[sockfd];
    if ((data.state == State::InitialAck) ||
            (data.end > data.start)) // we have data to send!
    {
        return {false, true};
    }

    char buffer[1024];
    int len = recv(sockfd, buffer, sizeof(buffer), 0);
    if (len < 0)
    {
        // it can happen that we were not able to recv any data even when we
        // get a notification from select
        if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
            return {true, false};

        perror_die("error recv");
    }

    if (len == 0)
        return {false, false}; // client socket was closed

    bool data_to_send = false;
    for (int i = 0; i < len; ++i)
    {
        switch (data.state)
        {
            case State::InitialAck:
                assert(false && "Not possible to be in initial ack state here");
                break;
            case State::WaitForMsg:
                if (buffer[i] == '^')
                    data.state = State::InMsg;
                break;
            case State::InMsg:
                if (buffer[i] == '$')
                    data.state = State::WaitForMsg;
                else
                {
                    // assert that we have not finished the buffer
                    assert(data.end < sizeof(data.buffer) - 1);
                    data.buffer[data.end] = buffer[i] + 1;
                    data.end++;
                    data_to_send = true;
                }
                break;
        }
    }

    if (data_to_send)
        return {false, true};

    return {true, false}; // read more
}

AsyncServer::Notify AsyncServer::on_ready_to_write(int sockfd)
{
    assert(sockfd < FD_SETSIZE);

    auto& data = m_fddata[sockfd];
    int sendlen = data.end - data.start;

    // if there is no data to send, then we bail out early
    if (sendlen == 0)
        return {true, true};

    int len = send(sockfd, &data.buffer[data.start], sendlen, 0);
    if (len < 0)
    {
        if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
        {
            // we need to try writing the data again, nothing was written
            return {false, true};
        }
        perror_die("error send");
        // technically, we could get an error message here ECONNRESET
        // which means that client disconnected before we were able to send
        // it data. This should not bring the server down!
    }

    if (len < sendlen)
    {
        // update the data structures, we have more stuff to write
        data.start = data.start + len;
        return {false, true}; // more data to send
    }

    // len == sendlen, everything was sent we are back to listening mode
    // let's clear all the content
    data.start = 0;
    data.end = 0;

    // additionally condition set the state
    if (data.state == State::InitialAck)
        data.state = State::WaitForMsg;

    return {true, false}; // only data to read now
}

void AsyncServer::select_run()
{
    // switch off the buffering
    setvbuf(stdout, NULL, _IONBF, 0); // verify this

    timed_print("Server is starting...\n");
    // create a listening socket the first thing
    int listener_sockfd = create_listening_socket(m_portnum, s_backlog);

    // mark the listening socket as non-blocking.
    // The select() manpage warns that select() can return a read notification
    // for a socket that isn't actually readable. Thus using blocking I/O isn't
    // safe.
    //
    // Basically we don't want any call to block if select has reported that something can be
    // done with this socket fd
    mark_non_blocking(listener_sockfd);

    // add the listener socket fd permanently to the set of readfds
    FD_SET(listener_sockfd, &m_readfds);

    // we don't want to loop over the entire fd range, so we keep track of the max fd that we have
    // seen so far.
    int fdset_max = listener_sockfd;

    while (true) // the loop which never ends
    {
        // From `man select` page
        // The first nfds descriptors are checked in each set; i.e., the descriptors from 0
        // through nfds-1 in the descriptor sets are examined.  (Example: If you
        // have set two file descriptors "4" and "17", nfds should  not be "2", but rather
        // "17 + 1" or "18".).
        //
        // so in out case, we are going to pass fdset_max + 1
        //
        // Also since select changes the fd_sets that we pass, we are going to create a copy and then
        // pass those into the call
        //
        // Since we are going to pass nullptr as the timeout, this select blocks till we have a
        // meaningful event!
        auto readfds  = m_readfds;
        auto writefds = m_writefds;
        int ncount = select(fdset_max + 1, &readfds, &writefds, nullptr, nullptr); // blocking
        if (ncount < 0)
            perror_die("error select"); // man select says error can be EAGAIN. misleading?

        // https://github.com/eliben/code-for-blog/blob/master/2017/async-socket-server/select-server.c
        // nready is total number of ready events. So it will be decremented both in case of
        // read and write events
        for (int fd = 0; fd <= fdset_max && ncount > 0; ++fd)
        {
            // check the read fds
            if (FD_ISSET(fd, &readfds))
            {
                --ncount;
                if (fd == listener_sockfd)
                {
                    // we got some new connection!
                    sockaddr_in peer_addr;
                    socklen_t peer_addr_len = sizeof(peer_addr);
                    int newsockfd = accept(listener_sockfd, (struct sockaddr*)&peer_addr, &peer_addr_len);
                    if (newsockfd < 0)
                    {
                        // because listen_sockfd is marked as non blocking, it can happen that accept
                        // returns a negative number even when the select has told us that there is some data!
                        // welcome to the world of low level programming!
                        if ((errno == EAGAIN) || (errno == EWOULDBLOCK))
                        {
                            fmt::print("{}", "accept returned EAGAIN || EWOULDBLOCK");
                            continue; // nothing much we can do if this happens, continue with the rest of the ncount
                        }
                        perror_die("error accept");
                    }

                    // check if the fdset_max needs to be increased or not
                    if (newsockfd > fdset_max)
                    {
                        if (newsockfd >= FD_SETSIZE) // fd has to stay within limits!
                        {
                            fmt::print(stderr, "socket fd {} is larger than max allowed {}", newsockfd, FD_SETSIZE);
                            exit(EXIT_FAILURE);
                        }
                        fdset_max = newsockfd; // new maximum fd we are interested in
                    }
                    auto notify = on_new_connection(newsockfd, peer_addr, peer_addr_len);
                    processNotify(newsockfd, notify);
                }
                else
                {
                    // one of the fds on which we were listening to got some data to be read
                    auto notify = on_ready_to_read(fd);
                    processNotify(fd, notify);
                }
            }

            // check the write fds
            if (FD_ISSET(fd, &writefds))
            {
                --ncount;
                auto notify = on_ready_to_write(fd);
                processNotify(fd, notify);
            }
        }
    }
}

void AsyncServer::epoll_run()
{
    timed_print("Server is starting...\n");
    int listener_sockfd = create_listening_socket(m_portnum, s_backlog);
    mark_non_blocking(listener_sockfd);
}
