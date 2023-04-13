with GNAT.Sockets;
with Ada.Streams;
with Ada.Text_IO;

generic
   Package_Default_Network_Family : in GNAT.Sockets.Family_Inet_4_6 := GNAT.Sockets.Family_Inet;
   Package_Default_Port : in GNAT.Sockets.Port_Type := 10_573;
   Max_Message_Length : in Ada.Streams.Stream_Element_Offset := 2_048;
   Message_File : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
package Network_Tree is
   pragma Assert (Ada.Streams.Stream_Element'Size = 8);

   package Sock renames GNAT.Sockets;

   task Server;

   procedure Connect_To_Server
     (Addr : Sock.Inet_Addr_Type; Port : Sock.Port_Type);
end Network_Tree;
