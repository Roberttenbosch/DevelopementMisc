#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#define STDIN 0

int main(void)
{
    /* code */
    struct timeval tv;
    fd_set readsfds;
    tv.tv_sec=2;
    tv.tv_usec=500000;
    FD_ZERO(&readsfds);
    FD_SET(STDIN, &readsfds);

    select(STDIN + 1, &readsfds,NULL,NULL, &tv);

    if(FD_ISSET(STDIN, &readsfds)){
        printf("keyboard pressed\n");

    }else{
        printf("timeout\n");
    }

    return 0;
}
