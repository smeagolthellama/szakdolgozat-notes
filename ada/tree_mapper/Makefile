CFLAGS:= -I/usr/include/tirpc -Wall -Wextra -g
LDLIBS:=-ltirpc

query_tree: query_tree.o data_types_xdr.o

data_types_xdr.c data_types.h: data_types.x
	rpcgen data_types.x

query_tree.o: query_tree.c data_types.h

data_types_xdr.o: data_types_xdr.c data_types.h

