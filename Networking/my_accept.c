#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>

#define MYPORT "3490"
#define BACKLOG 10
#define BUFFERSEND 100;

int main(int argc, char *argv[])
{
    struct sockaddr_storage their_addr;
    socklen_t addr_size;
    char* messagesend = "hallo daar";
    char* messagerecv;
    int len, bytes_send;

    struct addrinfo hints, *res, *p;
    int sockfd, new_fd;
    int status;
    char ipstr[INET6_ADDRSTRLEN];

    // if (argc != 2)
    // {
    //     fprintf(stderr, "Usage: showip hostname\n");
    //     return 1;
    // }
    // messagesend = argv[1];

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;


    if ((status = getaddrinfo(NULL, MYPORT, &hints, &res)) != 0)
    {
        fprintf(stderr, "Addrinfo: %s\n", gai_strerror(status));
        return 2;
    }
    sockfd =  socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    
    bind(sockfd, res->ai_addr, res->ai_addrlen);
    listen(sockfd, BACKLOG);


    printf("IP addresses for %s:\n\n", argv[1]);
    for (p = res; p != NULL; p = p->ai_next)
    {
        void *addr;
        char *ipver;
        // get the pointer to the address itself,
        // different fields in IPv4 and IPv6:
        if (p->ai_family == AF_INET)
        { // IPv4
            struct sockaddr_in *ipv4 = (struct sockaddr_in *)p->ai_addr;
            addr = &(ipv4->sin_addr);
            ipver = "IPv4";
        }
        else
        { // IPv6
            struct sockaddr_in6 *ipv6 = (struct sockaddr_in6 *)p->ai_addr;
            addr = &(ipv6->sin6_addr);
            ipver = "IPv6";
        }
        // convert the IP to a string and print it:
        inet_ntop(p->ai_family, addr, ipstr, sizeof ipstr);
        printf(" %s: %s\n", ipver, ipstr);
    }
    
    addr_size = sizeof their_addr;
    new_fd = accept(sockfd, (struct sockaddr *) &their_addr, &addr_size);
    len = strlen(messagesend);
    bytes_send = send(new_fd, messagesend, len,0);
    printf("new filedis %i", new_fd);
    printf("message send = %s", messagesend);

    
    freeaddrinfo(res); // free the linked list
    return 0;
}