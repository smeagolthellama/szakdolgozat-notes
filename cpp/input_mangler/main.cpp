#include <iostream>
#include <unistd.h>

using namespace std;

int main()
{
	char buffer[1024+sizeof(uint16_t)];
	uint16_t *msg_number=reinterpret_cast<uint16_t*>(buffer);
	char *msg=buffer+sizeof(uint16_t);
	msg_number[0]=0;
	while(!cin.eof() && !cin.fail()){
		cin.read(msg,1000);
		msg_number[0]++;
		usleep(2000);
		cout.write(buffer,cin.gcount()+static_cast<long>(sizeof(uint16_t)));
	}
    return 0;
}

