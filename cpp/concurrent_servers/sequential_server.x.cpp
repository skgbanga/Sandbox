#include "sequential_server.h"

#include <cstdlib>

int main(int argc, char *argv[])
{
    int portnum = 9090;
    if (argc >= 2)
        portnum = std::atoi(argv[1]);

    SequentialServer server(portnum);
    server.run();

    return 0;
}
