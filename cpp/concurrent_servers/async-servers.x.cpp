#include "utils.h"
#include "async-servers.h"

int main(int argc, char *argv[])
{
    int portnum = 9090;
    if (argc >= 2)
        portnum = std::atoi(argv[1]);

    AsyncServer server(portnum);

    // select based run
    server.select_run();

    return 0;
}
