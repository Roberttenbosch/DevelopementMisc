#include <sys/types.h>
#include <string.h>
#include<stdio.h>

int sendall(int s, char *buf, int *len)
{
    int total = 0;
    int bytesleft = *len;
    int n;

    while(total < *len)
    {
        n = send(s, buf+total, bytesleft, 0);
        if (n == -1){break;}
        total += n;
        bytesleft -= n;

    }
    *len = total;
    return  n == -1 ? 1 : 0;
}


int main(int argc, char const *argv[])
{
    char buf[10] = "booi";
    int len;
    len = strlen(buf);
    int s = 1; //moet eigen een filedescriptor zijn van een socket.
    if(sendall(s, buf, &len) == -1)
    {
        perror("sendall");
        printf("We only send %d bytes because of an error", len);

    }

    return 0;
}
