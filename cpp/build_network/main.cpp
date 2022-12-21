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
#include <mutex>

#include "child.hpp"
#include "messenger.hpp"

#pragma clang diagnostic ignored "-Wc++98-compat"

[[noreturn]] void rootProcess(uint16_t port, uint16_t &msg_number, mutex &msg_number_guard);
void joinProcess(uint16_t port,queue<in_addr> &hosts, uint16_t &msg_number, mutex &msg_number_guard);

int main(int argc,char **argv)
{
	uint16_t port=10573;
	uint16_t msg_number=0;
	mutex msg_number_guard;

	thread server(rootProcess,port,ref(msg_number),ref(msg_number_guard));

	if(argc>1) {
		queue<in_addr> hosts;
		for(int i=1; i<argc; i++) {
			in_addr host;
			if(inet_aton(argv[i],&host) == 0) {
				cerr<<argv[i]<<" was not recognised as an IPv4 address."<<endl;
			} else {
				hosts.push(host);
			}
		}
		thread client(joinProcess,port,ref(hosts),ref(msg_number),ref(msg_number_guard));
		client.join();
	}
	server.join();
}

#define SEND(msg,msglen) sendto(sock,(msg),(msglen),0,reinterpret_cast<struct sockaddr*>(&from),fromLen)

// https://stackoverflow.com/a/13105769/7105391
[[noreturn]] void rootProcess(uint16_t port, uint16_t &msg_number, mutex &msg_number_guard)
{
	int sock = socket(AF_INET, SOCK_DGRAM, 0);

	struct sockaddr_in addr;
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = INADDR_ANY;

	if(bind(sock, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr)) != 0) {
		perror("bind failed");
		throw errno;
	}
	char message[256];
	struct sockaddr_in from;
	socklen_t fromLen = sizeof(from);
	int num_children=0;
	struct child_t children[2];
	memset(children,0,sizeof(children));
	while(true) {
		auto msglen=static_cast<size_t>(recvfrom(sock, message, sizeof(message), 0, reinterpret_cast<struct sockaddr*>(&from), &fromLen));
		message[msglen]=0;

		char ip[16];
		inet_ntop(AF_INET, &from.sin_addr, ip, sizeof(ip));

		std::cerr << ip << ":" << ntohs(from.sin_port) << " - ";
		cerr.write(message,static_cast<ssize_t>(msglen))<<endl;
		if(strcmp(message,"?")==0 || strcmp(message,"?\n")==0) {
			cerr<<"Sending list of children."<<endl;
			SEND(&num_children,sizeof(int));
			SEND(children,sizeof(children));
		} else if(strcmp(message,"join")==0 || strcmp(message,"join\n")==0) {
			cerr<<"Join request from "<<ip<<"."<<endl;
			bool ok=true;
			if(num_children==2) {
				ok=false;
			}
			for(int i=0; i<num_children && ok; i++) {
				if(from.sin_addr.s_addr==children[i].addr.s_addr) {
					ok=false;
				}
			}
			if(ok) {
				lock_guard<mutex> lock(msg_number_guard);
				cerr<<"Accepted, is child "<<num_children<<endl;
				char reply[sizeof("ok")+sizeof(msg_number)]="ok";
				memcpy(reply+2,&msg_number,sizeof(msg_number));
				children[num_children++].addr=from.sin_addr;
				SEND(reply,sizeof(reply));
			} else {
				cerr<<"Rejected."<<endl;
				SEND("err",sizeof("err"));
			}
		} else if(msglen>2) {
			cerr<<"message is to be sent on.";
			{
				lock_guard<mutex> lock(msg_number_guard);
				uint16_t new_msg_number=reinterpret_cast<uint16_t*>(message)[0];
				if(new_msg_number!=msg_number) {
					//TODO
					cerr<<"new message number is "<<new_msg_number<<", which is not one past "<<msg_number<<endl;
				}
			}//unlock msg_number for dispatch
			cout.write(message+sizeof(msg_number),static_cast<ssize_t>(msglen-sizeof(msg_number))).flush();
			dispatchMessage(sock,port,msglen-sizeof(msg_number),message+sizeof(msg_number),msg_number,msg_number_guard,children,num_children);
		}else{
			cerr<<"unrecognised message shape, ignoring.";
		}
	}
}


#undef SEND
#define SEND(msg,msglen) sendto(sock,(msg),(msglen),0,reinterpret_cast<struct sockaddr*>(&addr),sizeof(addr))

void joinProcess(uint16_t port,queue<in_addr> &hosts, uint16_t &msg_number, mutex &msg_number_guard)
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
		if(num_children<2) {
			char reply[0xff];
			SEND("join",sizeof("join"));
			ssize_t reply_len=recvfrom(sock,reply,sizeof(reply),0,reinterpret_cast<struct sockaddr*>(&addr),&addrLen);
			if(reply_len==-1) {
				perror("receipt failed");
			}
			if(memcmp(reply,"ok",sizeof("ok")) != 0) {
				if(static_cast<size_t>(reply_len)>=sizeof("ok")+sizeof(msg_number)) {
					lock_guard<mutex> lock(msg_number_guard);
					memcpy(&msg_number,reply+2,sizeof(msg_number));
				}
				while(!hosts.empty()) {
					hosts.pop();
				}
			}
		}
		if(!hosts.empty()) {
			cerr<<"No space on "<<inet_ntoa(host)<<", asking children."<<endl;
			hosts.push(children[0].addr);
			hosts.push(children[1].addr);
		}
	}
}
