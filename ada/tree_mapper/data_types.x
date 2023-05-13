const CHILD_NUMBER = 3;

typedef unsigned int uint32;
typedef unsigned char uint8;

enum family_type {
  FAMILY_INET = 0,
  FAMILY_INET6 = 1,
  FAMILY_UNIX = 2,
  FAMILY_UNSPEC = 3
};

enum child_index {
  CHILD_1 = 1,
  CHILD_2 = 2,
  CHILD_3 = 3
};


struct inet_addr_v4_type {
  uint8 sin_v4[4];
};

struct inet_addr_v6_type {
  uint8 sin_v6[16];
};

union inet_addr_type switch (family_type family) {
  case FAMILY_INET:
    inet_addr_v4_type sin_v4;
  case FAMILY_INET6:
    inet_addr_v6_type sin_v6;
  default:
    void;
};

struct inet_addr_bytes {
  uint8 inet_addr_comp<>;
};

union my_inet_addr switch (family_type family) {
  case FAMILY_INET:
    inet_addr_v4_type sin_v4;
  case FAMILY_INET6:
    inet_addr_v6_type sin_v6;
  default:
    void;
};

struct address_and_port{
  my_inet_addr addr;
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
