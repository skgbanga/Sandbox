#include "exercise2.h"
#include "unistd.h"

int main() {
  // read
  {
    auto fileptr = mypopen("ls", "r");
    char buffer[100]{};

    while (fgets(buffer, 100, fileptr) != nullptr) {
      printf("%s", buffer);
    }
    mypclose(fileptr);
  }

  // write
  {
    auto fileptr = mypopen("wc", "w");
    for (int i = 0; i < 10; ++i) {
      int rc = fputs("hello\n", fileptr);
      if (rc == EOF) {
        perror("fputs()");
      }
    }

    mypclose(fileptr);
  }
}
