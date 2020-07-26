#include <stdio.h>
#include <unistd.h>

int main() {
  int rc = execlp("ls", "ls", NULL);
  if (rc == -1) {
    perror("Exec failed");
  }
}
