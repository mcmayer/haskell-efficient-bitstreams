#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

const size_t BUFLEN = 32768;

int
main() {
  char* buf = (char*)malloc(BUFLEN);
  ssize_t len;
  size_t total = 0;
  while((len = read(STDIN_FILENO, buf, BUFLEN)) > 0)
    total += len;
  free(buf);
  printf("%zd\n", total);
  return 0;
}
