#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netdb.h>
#include <signal.h>
#include <arpa/inet.h>
#include <netinet/in.h>

#define MYPORT "3490"
#define MAXDATASIZE 100

void *get_in_addr(struct sockaddr *sa)
{
    if (sa->sa_family == AF_INET)
    {
        return &(((struct sockaddr_in *)sa)->sin_addr);
    }
    return &(((struct sockaddr_in6 *)sa)->sin6_addr);
}

int main(int argc, char* argv[])
{
    int sockfd, num_bytes;
    char buf[MAXDATASIZE];
    struct addrinfo hints, *serverinfo, *p;
    char ipstr[INET6_ADDRSTRLEN];
    int rv;
    char* port;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    port = MYPORT;
    if(argv[2] != NULL)
    {
        port = argv[2];
    }
    

    if ((rv = getaddrinfo(argv[1], port, &hints, &serverinfo)) != 0)
    {
        fprintf(stderr, "Addrinfo: %s\n", gai_strerror(rv));
        return 2;
    }
    for (p = serverinfo; p != NULL; p = p->ai_next)
    {
        if ((sockfd = socket(p->ai_family, p->ai_socktype,
                             p->ai_protocol)) == -1)
        {
            perror("client: socket");
            continue;
        }
        if (connect(sockfd, p->ai_addr, p->ai_addrlen) == -1)
        {
            close(sockfd);
            perror("client: connect");
            continue;
        }
        break;
    }
    if (p == NULL)
    {
        fprintf(stderr, "client: failed to bind\n");
        return 2;
    }

    inet_ntop(p->ai_family,
                get_in_addr((struct sockaddr *)p->ai_addr),
                ipstr, sizeof ipstr);
    printf("client connecting to %s\n", ipstr);

    freeaddrinfo(serverinfo); // free the linked list
    if((num_bytes = recv(sockfd, buf, MAXDATASIZE-1, 0))==-1)
    {
        perror("recv");
        exit(1);
    }
    buf[num_bytes] = '\0';
    printf("client reciveid %s\n", buf);
    close(sockfd);
    return 0;
}