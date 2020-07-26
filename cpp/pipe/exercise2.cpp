#include "exercise2.h"
#include <cassert>
#include <cstring>
#include <iostream>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>

// we need to keep track of FILE* with the pid since that will be used
// to wait on that pid when pclose it called. Index of this vector is the fileno
// obtained from the stream
std::vector<pid_t> pids;

FILE *mypopen(const char *command, const char *mode) {
  if ((std::strncmp(mode, "r", 1) != 0) && std::strncmp(mode, "w", 1)) {
    std::cerr << "Wrong input\n";
    return nullptr;
  }

  int fds[2]{};
  auto err = pipe(fds);
  if (err == -1) {
    std::cerr << "Pipe failed\n";
    return nullptr;
  }

  pid_t cpid;
  int to_close = 0;
  int to_save = 0;
  if (mode[0] == 'r') {
    cpid = fork();
    switch (cpid) {
    case -1:
      std::cerr << "Fork failed\n";
      return nullptr;

    case 0: // child
      if (close(fds[0] == -1)) {
        std::cerr << "Error closing the read end of pipe in child process";
      }

      if (dup2(fds[1], STDOUT_FILENO) == -1) {
        std::cerr << "Duplicate failed\n";
        return nullptr;
      }
      if (close(fds[1]) == -1) {
        std::cerr << "Close of write end failed\n";
        return nullptr;
      }

      execlp(command, command, nullptr);
      assert(false);
    default:
      // parent
      to_close = fds[1];
      to_save = fds[0];
      break;
    }
  }

  int rc;
  if (mode[0] == 'w') {
    cpid = fork();
    switch (cpid) {
    case -1:
      std::cerr << "Fork failed\n";
      return nullptr;

    case 0: // child
      if (close(fds[1]) == -1) {
        std::cerr << "Error closing the write end of pipe in child process";
      }

      if (dup2(fds[0], STDIN_FILENO) == -1) {
        perror("Duplicated failed ");
        return nullptr;
      }
      if (close(fds[0]) == -1) {
        std::cerr << "Close of write end failed\n";
        return nullptr;
      }

      rc = execlp(command, command, nullptr);
      if (rc == -1) {
        perror("Execlp failed ");
        return nullptr;
      }
      assert(false);

    default: // parent
      to_close = fds[0];
      to_save = fds[1];
      break;
    }
  }

  if (close(to_close) == -1) {
    std::cerr << "Error closing the write end of pipe in parent\n";
    return nullptr;
  }

  if (to_save >= (int)pids.size()) {
    pids.resize(to_save + 1);
  }
  pids[to_save] = cpid;
  return fdopen(to_save, mode);
}

int mypclose(FILE *stream) {
  if (fflush(stream) != 0) {
    perror("Flushing the stream failed");
    return -1;
  }

  int fd = fileno(stream);
  if (close(fd) == -1) {
    std::cerr << "Error closing the stream in pclose\n";
    return -1;
  }

  auto pid = pids.at(fd);
  int status = 0;
  waitpid(pid, &status, 0);
  return status;
}
