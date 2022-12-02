#include "messenger.hpp"

void dispatchMessage(int sock,uint16_t port,const size_t len,const char* message, uint16_t &msg_number, std::mutex &msg_number_guard,const child_t children[], const int num_children){
	if(len>MAX_MSG_SIZE){
		for(size_t i=0;i<len;i+=MAX_MSG_SIZE){
			if(len-i<MAX_MSG_SIZE){
				dispatchMessage(sock,port,len-i,message+i,msg_number,msg_number_guard,children,num_children);
			}else{
				dispatchMessage(sock,port,MAX_MSG_SIZE,message+i,msg_number,msg_number_guard,children,num_children);
			}
		}
	}else{
		std::lock_guard<std::mutex> lock(msg_number_guard);
		char packaged_message[MAX_MSG_SIZE+sizeof(msg_number)];
		memcpy(packaged_message,&msg_number,sizeof(msg_number));
		memcpy(packaged_message+sizeof(msg_number),message,len);
		msg_number++;
		for(int i=0;i<num_children;i++){
			struct sockaddr_in addr;
			addr.sin_addr.s_addr=children[i].addr.s_addr;
			addr.sin_family=AF_INET;
			addr.sin_port=htons(port);
			sendto(sock,packaged_message,len+sizeof(msg_number),0,reinterpret_cast<sockaddr*>(&addr),static_cast<socklen_t>(sizeof(addr)));
		}
	}
}
