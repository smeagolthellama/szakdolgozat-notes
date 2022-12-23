with GNAT.Sockets;

generic
   family: in GNAT.Sockets.Family_Inet_4_6:=GNAT.Sockets.Family_Inet;
   port: in GNAT.Sockets.Port_Type:=10573;
package Network_Tree is
   package Sock renames GNAT.Sockets;

   task Server is
   end Server;

   procedure Client(addr: Sock.Inet_Addr_Type);
end Network_Tree;
