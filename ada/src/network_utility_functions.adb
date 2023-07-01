with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Network_Utility_Functions is
   function Image (Arr : Stream_Element_Array) return String is
      String_Index  : Integer := 1;
      Return_String : String (1 .. Arr'Length) := (others => ' ');
   begin
      for I in Arr'Range loop
         Return_String (String_Index) := Character'Val (Arr (I));
         String_Index := String_Index + 1;
      end loop;
      return Return_String;
   end Image;

   procedure Parse_Address_And_Port
     (In_Str         : in String; Port : in GNAT.Sockets.Port_Type;
      Parsed_Address : out GNAT.Sockets.Inet_Addr_Type;
      Parsed_Port    : out GNAT.Sockets.Port_Type)
   is
   begin
      pragma Debug (Put_Line (Standard_Error, "Parsing '" & In_Str &
                      "' (of length " & In_Str'Length'Image & ")."));
      if In_Str'Length /= 0 then
         if GNAT.Sockets.Is_IPv4_Address (In_Str)
           or else GNAT.Sockets.Is_IPv6_Address (In_Str)
         then
            -- note that Is_IPv4 accepts some addresses that Inet_Addr does not.
            -- In these cases, Inet_Addr raises a Socket_Error
            Parsed_Address := GNAT.Sockets.Inet_Addr (In_Str);
            Parsed_Port := Port;
         elsif In_Str (In_Str'First) = '[' then
            declare
               -- If there is not ']' in In_Str, Close_Bracket will be 0
               -- in which case, I will soon try to access
               -- In_Str (2 .. -1) which will raise a Constraint_Error.
               Close_Bracket : constant Natural := Index (In_Str, "]");
            begin
               if GNAT.Sockets.Is_IPv6_Address
                 (Name => In_Str (In_Str'First + 1 .. Close_Bracket - 1))
                 or else GNAT.Sockets.Is_IPv4_Address
                   (Name => In_Str (In_Str'First + 1 .. Close_Bracket - 1))
               then
                  if Close_Bracket = In_Str'Last then
                     -- looks like it's an [ipv6] or [ipv4]. If not, Socket_Error.
                     Parsed_Address :=
                       GNAT.Sockets.Inet_Addr
                         (In_Str (In_Str'First + 1 .. Close_Bracket - 1));
                     Parsed_Port := Port;
                  elsif In_Str (Close_Bracket + 1) = ':' then
                     -- We know it's [ipv*]:*.
                     -- We try to parse the port, and let Ada do it's thing.
                     Parsed_Address :=
                       GNAT.Sockets.Inet_Addr
                         (In_Str (In_Str'First + 1 .. Close_Bracket - 1));
                     Parsed_Port :=
                       GNAT.Sockets.Port_Type'Value
                         (In_Str (Close_Bracket + 2 .. In_Str'Last));
                     -- If the port is not numeric, 'Value will raise an exception.
                  else
                     -- It's [ipv*]\[^:\]* Not a clue what to do with it.
                     raise Format_Error
                       with "The closing bracket is not the last character, or followed by a colon.";
                  end if;
               else
                  raise Format_Error
                    with "The bracketed segment is not an IP address.";
               end if;
            end;
         else -- should be an ipv4 address with a port
            if Ada.Strings.Fixed.Count (In_Str, ":") > 1
            then -- too many colons
               raise Format_Error
                 with "'" & In_Str (Index (In_Str, ":") .. In_Str'Last) &
                 "' is not a Natural number (for IPv6, the expected format is along the lines of [::1]:1234)";
            elsif Ada.Strings.Fixed.Count (In_Str, ":") /= 1
            then --not enough colons
               raise Format_Error
                 with "In_Str '" & In_Str &
                 "' was not recognised as an IP address (with an optional port).";
            end if;
            declare
               Colon : constant Natural := Index (In_Str, ":");
            begin
               if GNAT.Sockets.Is_IPv4_Address
                 (In_Str (In_Str'First .. Colon - 1))
               then
                  Parsed_Address :=
                    GNAT.Sockets.Inet_Addr
                      (In_Str (In_Str'First .. Colon - 1));
                  Parsed_Port :=
                    GNAT.Sockets.Port_Type'Value
                      (In_Str (Colon + 1 .. In_Str'Last));
               end if;
            end;
         end if;
      end if;
   exception
      when E : GNAT.Sockets.Socket_Error =>
         raise Format_Error
           with "Error parsing IP address: " &
           Ada.Exceptions.Exception_Message (E);
      when E : Constraint_Error =>
         raise Format_Error
           with "Constraint_Error (no closing bracket?): " &
           Ada.Exceptions.Exception_Message (E);
   end Parse_Address_And_Port;
end Network_Utility_Functions;
