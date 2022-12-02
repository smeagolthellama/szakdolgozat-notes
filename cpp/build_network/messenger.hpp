#ifndef MESSENGER_HPP_INCLUDED
#define MESSENGER_HPP_INCLUDED

#include "child.hpp"
#include <mutex>
#include <cstring>

#define MAX_MSG_SIZE static_cast<size_t>(1024)

void dispatchMessage(int sock,uint16_t port,const size_t len,const char* message, uint16_t &msg_number, std::mutex &msg_number_guard,const child_t children[], const int num_children);


#endif // MESSENGER_HPP_INCLUDED
