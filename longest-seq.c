#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

const size_t BUFLEN = 32768;

int
main() {
    char* buf = (char*)malloc(BUFLEN);
    ssize_t buflen;
    unsigned short int seq_len = 0, max_seq_len = 0;
    char last = 0;
    while((buflen = read(STDIN_FILENO, buf, BUFLEN)) > 0) {
        for(ssize_t i=0; i<buflen; i++) {
            char c = buf[i];
            for(short k=0; k<8; k++) {
                char bit = 0x01 & (c>>k);
                if(bit!=last) {
                    if(seq_len > max_seq_len) max_seq_len = seq_len;
                    seq_len = 0;
                    last = bit;
                } else
                    seq_len++;            
            }
        }
    }
    free(buf);
    printf("%d\n", (int)max_seq_len);
    return 0;
}
