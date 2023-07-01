const CHILD_NUMBER = 2;

typedef unsigned int uint32;
typedef unsigned char uint8;

enum family_type {
  FAMILY_INET = 0,
  FAMILY_INET6 = 1,
  FAMILY_UNIX = 2,
  FAMILY_UNSPEC = 3
};

union inet_addr_type switch (family_type family) {
  case FAMILY_INET:
    opaque sin_v4[4];
  case FAMILY_INET6:
    opaque sin_v6[16];
  default:
    void;
};

struct address_and_port{
  inet_addr_type addr;
  unsigned int port;
};

union sock_addr_type switch (family_type family) {
  case FAMILY_UNIX:
    string name<>;
  case FAMILY_INET:
    address_and_port anp4;
  case FAMILY_INET6:
    address_and_port anp6;
  case FAMILY_UNSPEC:
    void;
};

struct child_set {
  sock_addr_type child_set_array[CHILD_NUMBER];
};
