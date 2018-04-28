#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

const size_t BUFLEN = 32768;

int
main() {
  char* buf = (char*)malloc(BUFLEN);
  char sum=0;
  ssize_t len;
  while((len = read(STDIN_FILENO, buf, BUFLEN)) > 0)
  {
      for(ssize_t i=0; i<len; i++)
          sum += buf[i];
    }
  free(buf);
  printf("%d\n", (int)sum);
  return 0;
}
