with GNAT.Sockets;
use GNAT.Sockets;

package body Network_Tree is

   type ChildNumber is range 0..2;
   subtype ChildIndex is ChildNumber range 1..ChildNumber'Last;

   type ChildSet is array (ChildIndex) of Socket_Type;

   protected Children is
      entry AddChild(child: Socket_Type);
      entry GetChildren(n:out ChildNumber; c:out ChildSet);
      -- entry RemoveChild
   private
      number: ChildNumber := 0;
      set: ChildSet;
      locked: boolean:=False;
   end Children;

   protected body Children is
      entry AddChild (child: Socket_Type)
        when not locked is
      begin
         locked:=True;
         null;
         locked:=False;
      end AddChild;

      entry GetChildren(n: out ChildNumber; c: out ChildSet)
        when not locked is
      begin
         locked:=True;
         n:=number;
         c:=set;
         locked:=False;
      end GetChildren;
   end Children;


   task body Server is
      listeningSocket: Socket_Type;
      listeningAddress: Sock_Addr_Type(family);
   begin
      Create_Socket(listeningSocket, family, Socket_Datagram, IP_Protocol_For_UDP_Level);
      listeningAddress.Addr:=(if family = Family_Inet then Any_Inet_Addr else Any_Inet6_Addr);
      Bind_Socket(listeningSocket,listeningAddress);
      null;
   end Server;

   procedure Client (addr: Inet_Addr_Type) is
   begin
      null;
   end Client;
end Network_Tree;
