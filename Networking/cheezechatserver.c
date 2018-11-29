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

#define MYPORT "9034"
#define BACKLOG 10

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
    fd_set master;
    fd_set read_fds;
    int fdmax;
    int listener;
    int newfd;
    struct sockaddr_storage remoteaddr;
    socklen_t addrlen;
    char buf[256];

    int nbytes;
    char remoteIp[INET6_ADDRSTRLEN];
    int yes = 1;
    int i, j, rv;

    struct addrinfo hints, *ai, *p;

    FD_ZERO(&master);
    FD_ZERO(&read_fds);

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    if ((rv = getaddrinfo(NULL, MYPORT, &hints, &ai)) != 0)
    {
        fprintf(stderr, "selectserver:: %s\n", gai_strerror(rv));
        exit(1);
    }
    for (p = ai; p != NULL; p = p->ai_next)
    {
        listener = socket(p->ai_family, p->ai_socktype,
                          p->ai_protocol);
        if (listener < 0)
        {
            perror("server: socket");
            continue;
        }
        if (setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes,
                       sizeof(int)) == -1)
        {
            perror("setsockopt");
            exit(1);
        }

        if (bind(listener, p->ai_addr, p->ai_addrlen) < 0)
        {
            close(listener);
            perror("server: bind");
            continue;
        }
        break;
    }
    if (p == NULL)
    {
        fprintf(stderr, "server: failed to bind\n");
        exit(2);
    }

    freeaddrinfo(ai); // free the linked list

    if (listen(listener, BACKLOG) == -1)
    {
        perror("listen");
        exit(3);
    }
    FD_SET(listener, &master);

    fdmax = listener;

    printf("server: waiting for connections...\n");

    for (;;)
    {
        printf("main loop\n");
        read_fds = master;
        if (select(fdmax + 1, &read_fds, NULL, NULL, NULL) == -1)
        {
            perror("select");
            exit(4);
        }
        printf("select\n");

        for (i = 0; i <= fdmax; i++)
        {
            printf("in for loop\n");
            if (FD_ISSET(i, &read_fds))
            {
                printf("handle new connections.\n");
                if (i == listener)
                {
                    addrlen = sizeof remoteaddr;
                    newfd = accept(listener, (struct sockaddr *)&remoteaddr, &addrlen);

                    if (newfd == -1)
                    {
                        perror("accept");
                    }
                    else
                    {
                        FD_SET(newfd, &master);
                        if (newfd > fdmax)
                        {
                            fdmax = newfd;
                        }
                        printf("selectserver: new connection from %s on "
                               "socket %d\n",
                               inet_ntop(remoteaddr.ss_family,
                                         get_in_addr((struct sockaddr *)&remoteaddr),
                                         remoteIp, INET6_ADDRSTRLEN),
                               newfd);
                    }
                }
                else
                {
                    printf("handle data from a client\n");
                    if ((nbytes = recv(i, buf, sizeof buf, 0)) <= 0)
                    {
                        if (nbytes == 0)
                        {
                            printf("selectserver: socket %d hung up\n", i);
                        }
                        else
                        {
                            perror("recv");
                        }
                        close(i);
                        FD_CLR(i, &master);
                    }
                    else
                    {
                        for (j = 0; j <= fdmax; j++)
                        {
                            if (FD_ISSET(j, &master))
                            {
                                if (j != listener && j != i)
                                {
                                    if (send(j, buf, nbytes, 0) == -1)
                                    {
                                        perror("send");
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return 0;
}