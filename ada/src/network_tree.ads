with GNAT.Sockets;
with Ada.Streams;

generic
   family : in GNAT.Sockets.Family_Inet_4_6 := GNAT.Sockets.Family_Inet;
   port : in GNAT.Sockets.Port_Type := 10_573;
   maxMessageLength : in Ada.Streams.Stream_Element_Offset := 2_048;
package Network_Tree is
   pragma Assert (Ada.Streams.Stream_Element'Size = 8);

   package Sock renames GNAT.Sockets;

   task Server;

   procedure Client (addr : Sock.Inet_Addr_Type; port : Sock.Port_Type);
end Network_Tree;
