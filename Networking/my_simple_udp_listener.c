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

#define MYPORT "4950"
#define MAXBUFLEN 100

void *get_in_addr(struct sockaddr *sa)
{
    if (sa->sa_family == AF_INET)
    {
        return &(((struct sockaddr_in *)sa)->sin_addr);
    }
    return &(((struct sockaddr_in6 *)sa)->sin6_addr);
}

int main(void)
{
    int sockfd;
    struct addrinfo hints, *serverinfo, *p;
    struct sockaddr_storage their_addr;
    int rv;
    int numbytes;
    char buf[MAXBUFLEN];
    socklen_t addr_len;
    char s[INET6_ADDRSTRLEN];

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_flags = AI_PASSIVE;

    if ((rv = getaddrinfo(NULL, MYPORT, &hints, &serverinfo)) != 0)
    {
        fprintf(stderr, "Addrinfo: %s\n", gai_strerror(rv));
        return 2;
    }
    for (p = serverinfo; p != NULL; p = p->ai_next)
    {
        if ((sockfd = socket(p->ai_family, p->ai_socktype,
                             p->ai_protocol)) == -1)
        {
            perror("listener: socket");
            continue;
        }
        if (bind(sockfd, p->ai_addr, p->ai_addrlen) == -1)
        {
            close(sockfd);
            perror("listener: bind");
            continue;
        }
        break;
    }

    if (p == NULL)
    {
        fprintf(stderr, "listener: failed to bind\n");
        return 2;
    }
    freeaddrinfo(serverinfo); // free the linked list

    printf("listener: waiting for connections...\non %s",p->ai_addr->sa_data);
    while (1)
    {
        addr_len = sizeof their_addr;
        if ((numbytes = recvfrom(sockfd, buf, MAXBUFLEN - 1, 0, (struct sockaddr *)&their_addr, &addr_len)) == -1)
        {
            perror("recvfrom");
            exit(1);
        }
        printf("listener: got packet from %s\n",
               inet_ntop(their_addr.ss_family,
                         get_in_addr((struct sockaddr *)&their_addr),
                         s, sizeof s));
        printf("listener: packet is %d bytes long\n", numbytes);
        buf[numbytes] = '\0';
        printf("listener: packet contains \"%s\"\n", buf);
    }
    close(sockfd);
    return 0;
}