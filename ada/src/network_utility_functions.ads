with Ada.Streams; use Ada.Streams;
with GNAT.Sockets;
package Network_Utility_Functions is
   function Image (Arr : Stream_Element_Array) return String;

   Format_Error : exception;
   procedure Parse_Address_And_Port
     (In_Str           : in String;
      Port             : in GNAT.Sockets.Port_Type;
      Parsed_Address   : out GNAT.Sockets.Inet_Addr_Type;
      Parsed_Port      : out GNAT.Sockets.Port_Type);
end Network_Utility_Functions;
