#pragma once

class ThreadedServer
{
    public:
    ThreadedServer(int portnum)
        : portnum_(portnum)
    {  }

    void run();

    private:
    int portnum_;
    static const int s_backlog = 64;
};
