#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include "data_types.h"

#define SERVER_PORT 10573
#define SERVER_ADDR "127.0.0.1"
#define BUF_SIZE 1024

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <server IP address>\n", argv[0]);
    return 1;
  }

  int sock;
  struct sockaddr_in server_addr;
  char buf[BUF_SIZE];

  // create UDP socket
  sock = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock == -1) {
    perror("socket()");
    return 1;
  }

  // set up server address
  memset(&server_addr, 0, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_port = htons(SERVER_PORT);
  if (inet_pton(AF_INET, argv[1], &(server_addr.sin_addr)) != 1) {
    perror("inet_pton()");
    return 1;
  }

  // send "?" to server
  if (sendto(sock, "?", 1, 0, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {
    perror("sendto()");
    return 1;
  }

  // receive response from server
  socklen_t server_addr_len = sizeof(server_addr);
  ssize_t num_bytes = recvfrom(sock, buf, BUF_SIZE, 0, (struct sockaddr *)&server_addr, &server_addr_len);
  if (num_bytes == -1) {
    perror("recvfrom()");
    return 1;
  }

  // parse response using XDR
  child_set children;
  memset(&children, 0, sizeof(children));
  XDR xdr;
  xdrmem_create(&xdr, buf, num_bytes, XDR_DECODE);
  if (!xdr_child_set(&xdr, &children)) {
    fprintf(stderr, "Error: failed to parse response\n");
    return 1;
  }

  // print parsed data
  for (int i = 0; i < CHILD_NUMBER; i++) {
    sock_addr_type *child = &(children.child_set_array[i]);
    switch (child->family) {
      case FAMILY_UNIX:
        printf("Child %d: Unix socket, name = %s\n", i+1, child->sock_addr_type_u.name);
        break;
      case FAMILY_INET:{
        char inet_addr_str[INET_ADDRSTRLEN];
        inet_ntop(AF_INET, &(child->sock_addr_type_u.anp4.addr.my_inet_addr_u.sin_v4.sin_v4), inet_addr_str, INET_ADDRSTRLEN);
        printf("Child %d: IPv4 socket, address = %s, port = %u\n", i+1, inet_addr_str, ntohs(child->sock_addr_type_u.anp4.port));
        break;}
      case FAMILY_INET6:{
        char inet6_addr_str[INET6_ADDRSTRLEN];
        inet_ntop(AF_INET6, &(child->sock_addr_type_u.anp6.addr.my_inet_addr_u.sin_v6.sin_v6), inet6_addr_str, INET6_ADDRSTRLEN);
        printf("Child %d: IPv6 socket, address = %s, port = %u\n", i+1, inet6_addr_str, ntohs(child->sock_addr_type_u.anp6.port));
        break;}
      default:
        printf("Child %d: No socket\n", i+1);
        break;
    }
  }

  close(sock);
  return 0;
}
