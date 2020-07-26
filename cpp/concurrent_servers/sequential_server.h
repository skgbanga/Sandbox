#pragma once

// Create a sequential server listening to the port number portnum for clients
// What happens after clients make connection is described here:
// https://eli.thegreenplace.net/2017/concurrent-servers-part-1-introduction/
class SequentialServer
{
    public:
    SequentialServer(int portnum)
        : portnum_(portnum)
    {  }

    void run();

    private:
    int portnum_;
    static const int s_backlog = 64;
};
