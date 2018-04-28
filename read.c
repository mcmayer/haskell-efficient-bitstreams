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
      for(ssize_t i=0; i<len; i++) {
          char c = buf[i];
          for(short k=0; k  <8; sum += 0x01 & (c>>k++));
      }
    }
  free(buf);
  printf("%d\n", (int)sum);
  return 0;
}
