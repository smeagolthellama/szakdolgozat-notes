#include <iostream>
#include <arpa/inet.h>
#include <cstring>
#include <thread>
#include <queue>

#include "../build_network/child.hpp"

using namespace std;

void printer(int sock,uint16_t port,in_addr host,int depth=0);

int main(int argc, char** argv)
{
	int sock = socket(AF_INET, SOCK_DGRAM, 0);
	uint16_t port=10573;
	for(int i=1; i<argc; i++) {
		in_addr host;
		if(!inet_aton(argv[i],&host)) {
			cerr<<argv[i]<<" was not recognised as an IPv4 address."<<endl;
		} else {
			printer(sock,port,host);
		}
	}
	return 0;
}

#define SEND(msg,msglen) sendto(sock,(msg),(msglen),0,reinterpret_cast<struct sockaddr*>(&addr),sizeof(addr))

void printer(int sock,uint16_t port,in_addr host,int depth)
{
	sockaddr_in addr;
	socklen_t addrLen=sizeof(addr);
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr = host;
	int num_children;
	child_t children[2];
	SEND("?",sizeof("?"));
	recvfrom(sock,&num_children,sizeof(num_children),0,reinterpret_cast<struct sockaddr*>(&addr),&addrLen);
	recvfrom(sock,children,sizeof(children),0,reinterpret_cast<struct sockaddr*>(&addr),&addrLen);
	if(num_children>=1) {
		printer(sock,port,children[0].addr,depth+1);
	}
	for(int i=0; i<depth; i++) {
		cout<<" ";
	}
	cout<<inet_ntoa(host)<<endl;
	if(num_children==2) {
		printer(sock,port,children[1].addr,depth+1);
	}
}
