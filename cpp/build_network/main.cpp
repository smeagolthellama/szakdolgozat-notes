#include <iostream>
#include <sys/socket.h>
#include <sys/types.h>
using namespace std;

/**
 * Get address from argv, or assume I am root
 * listen on port 10573
 * When recvd "?" reply with a list of clients
 * when recvd "join" add to list of clients
 * if not root:
 * add root to queue
 * for each host in queue:
 * send "?" to host
 * if 0 or 1 clients returned, send "join"
 * else add clients to queue
 */

#include <arpa/inet.h>
#include <cstring>
#include <thread>
#include <queue>

#include "child.hpp"

[[noreturn]] void rootProcess(uint16_t port);
void joinProcess(uint16_t port,queue<in_addr> hosts);

int main(int argc,char **argv)
{
	uint16_t port=10573;

	thread server(rootProcess,port);

	if(argc>1) {
		queue<in_addr> hosts;
		for(int i=1; i<argc; i++) {
			in_addr host;
			if(!inet_aton(argv[i],&host)) {
				cerr<<argv[i]<<" was not recognised as an IPv4 address."<<endl;
			} else {
				hosts.push(host);
			}
		}
		thread client(joinProcess,port,hosts);
		client.join();
	}
	server.join();
}

#define SEND(msg,msglen) sendto(sock,(msg),(msglen),0,reinterpret_cast<struct sockaddr*>(&from),fromLen)

// https://stackoverflow.com/a/13105769/7105391
[[noreturn]] void rootProcess(uint16_t port)
{
	int sock = socket(AF_INET, SOCK_DGRAM, 0);

	struct sockaddr_in addr;
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = INADDR_ANY;

	if(bind(sock, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr))) {
		perror("bind failed");
		throw errno;
	} else {
		char message[256];
		struct sockaddr_in from;
		socklen_t fromLen = sizeof(from);
		int num_children=0;
		struct child_t children[2];
		memset(children,0,sizeof(children));
		while(true) {
			ssize_t msglen=recvfrom(sock, message, sizeof(message), 0, reinterpret_cast<struct sockaddr*>(&from), &fromLen);
			message[msglen]=0;

			char ip[16];
			inet_ntop(AF_INET, &from.sin_addr, ip, sizeof(ip));

			std::cout << ip << ":" << ntohs(from.sin_port) << " - " << message << std::endl;
			if(strcmp(message,"?")==0 || strcmp(message,"?\n")==0) {
				cout<<"Sending list of children."<<endl;
				SEND(&num_children,sizeof(int));
				SEND(children,sizeof(children));
			} else if(strcmp(message,"join")==0 || strcmp(message,"join\n")==0) {
				cout<<"Join request from "<<ip<<"."<<endl;
				bool ok=1;
				if(num_children==2) {
					ok=0;
				}
				for(int i=0; i<num_children && ok; i++) {
					if(from.sin_addr.s_addr==children[i].addr.s_addr && from.sin_port==children[i].port) {
						ok=0;
					}
				}
				if(ok) {
					cout<<"Accepted, is child "<<num_children<<endl;
					children[num_children].addr=from.sin_addr;
					children[num_children++].port=from.sin_port;
					SEND("ok",sizeof("ok"));
				} else {
					cout<<"Rejected."<<endl;
					SEND("err",sizeof("err"));
				}
			}
		}
	}
}

#undef SEND
#define SEND(msg,msglen) sendto(sock,(msg),(msglen),0,reinterpret_cast<struct sockaddr*>(&addr),sizeof(addr))


void joinProcess(uint16_t port,queue<in_addr> hosts)
{
	int sock=socket(AF_INET,SOCK_DGRAM,0);
	while(!hosts.empty()) {
		auto host=hosts.front();
		hosts.pop();
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
		if(num_children<2){
			SEND("join",sizeof("join"));
			while(!hosts.empty()){
				hosts.pop();
			}
		}else{
			cout<<"No space on "<<inet_ntoa(host)<<", asking children."<<endl;
			hosts.push(children[0].addr);
			hosts.push(children[1].addr);
		}
	}
}
