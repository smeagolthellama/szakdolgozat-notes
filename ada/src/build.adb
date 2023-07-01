with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Sockets;

with Network_Tree;
with Network_Utility_Functions; use Network_Utility_Functions;

procedure Build is
   -- These can be overwrittren by command line options.
   Print_Help : Boolean                  := False;
   Port       : GNAT.Sockets.Port_Type   := 10_573;
   Family     : GNAT.Sockets.Family_Type := GNAT.Sockets.Family_Inet;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line
        (Standard_Error,
         "Usage: " & Ada.Command_Line.Command_Name &
           " [options] host[:port] [...]");
      Put_Line
        (Standard_Error,
         "Starting in server-only mode. Listening on port" & Port'Image &
           " on family " & Family'Image & " (Inet: IPv4)");
   end if;
   -- parse simple command line options
   loop
      case GNAT.Command_Line.Getopt ("h p: f: 6 -help -port= -file=") is
         when 'h' =>
            Print_Help := True;
         when 'p' =>
            Port :=
              GNAT.Sockets.Port_Type'Value (GNAT.Command_Line.Parameter);
         when '6' =>
            Family := GNAT.Sockets.Family_Inet6;
         when 'f' =>
            null; --TODO
         when '-' =>
            if GNAT.Command_Line.Full_Switch = "--help" then
               Print_Help := True;
            elsif GNAT.Command_Line.Full_Switch = "--port" then
               Port :=
                 GNAT.Sockets.Port_Type'Value
                   (GNAT.Command_Line.Parameter);
            end if;
         when others =>
            exit;
      end case;
   end loop;

   if Print_Help then
      Ada.Text_IO.Put_Line ("Usage: ");
      Ada.Text_IO.Put_Line (Ada.Characters.Latin_1.HT & Ada.Command_Line.Command_Name & " [-h6] [-p <number>] [--port=number] [--help]");
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Put_Line ("-h/--help" & Ada.Characters.Latin_1.HT & ": Print this message and exit");
      Ada.Text_IO.Put_Line ("-6" & Ada.Characters.Latin_1.HT & ": Listen on IPv6");
      Ada.Text_IO.Put_Line ("-p/--port" & Ada.Characters.Latin_1.HT & ": Listen on this port");
   else
      -- Parse servers to try listening to, in the form of ip addresses with ports.
      -- accepted formats:
      -- ipv4, ipv6, [ipv4], [ipv6], ipv4:port, [ipv6]:port
      declare
         package My_Tree is new Network_Tree
           (Package_Default_Network_Family => Family, Package_Default_Port => Port);
         End_Of_Arguments : Boolean := False;
      begin
         while not End_Of_Arguments loop
            declare
               -- get next command line argument
               Argument        : constant String :=
                                   GNAT.Command_Line.Get_Argument
                                     (End_Of_Arguments => End_Of_Arguments);
               Address_To_Try  : GNAT.Sockets.Inet_Addr_Type;
               Port_To_Try     : GNAT.Sockets.Port_Type;
            begin
               if Argument'Length /= 0 then
                  Parse_Address_And_Port (Argument, Port, Address_To_Try, Port_To_Try);
                  My_Tree.Connect_To_Server (Address_To_Try, Port_To_Try);
               end if;
            exception
               when E : Format_Error =>
                  Put_Line
                    (Standard_Error,
                     "Error parsing argument '" & Argument & "':");
                  Put_Line
                    (Standard_Error,
                     Ada.Exceptions.Exception_Message (E));
               when others =>
                  Put_Line
                    (Standard_Error,
                     "Unknown error parsing argument '" & Argument &
                       "':");
            end;
         end loop;
      end;
   end if;
end Build;
