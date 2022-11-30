#ifndef CHILD_HPP_INCLUDED
#define CHILD_HPP_INCLUDED

#include<netinet/in.h>

struct child_t{
	in_addr addr;
	in_port_t port;
	char padding[2];
};

#endif // CHILD_HPP_INCLUDED
