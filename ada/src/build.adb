with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets;
with Network_Tree;
with Ada.Command_Line;
with GNAT.Command_Line;
with Ada.Strings.Fixed;
with Ada.Exceptions;

procedure Build is
   -- These can be overwrittren by command line options.
   Print_Help: Boolean:=False;
   Port: GNAT.Sockets.Port_Type:=10573;
   Family: GNAT.Sockets.Family_Type:=GNAT.Sockets.Family_Inet;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line(Standard_Error,"Usage: "&Ada.Command_Line.Command_Name&" [options] host[:port] [...]");
   end if;
   loop
      case GNAT.Command_Line.Getopt("h p: 6 -help -port=") is
         when 'h' =>
            Print_Help:=True;
         when 'p' =>
            Port:=GNAT.Sockets.Port_Type(Integer'Value(GNAT.Command_Line.Parameter));
         when '6' =>
            Family:=GNAT.Sockets.Family_Inet6;
         when '-' =>
            if GNAT.Command_Line.Full_Switch = "--help" then
               Print_Help:=True;
            elsif GNAT.Command_Line.Full_Switch = "--port" then
               Port:=GNAT.Sockets.Port_Type(Integer'Value(GNAT.Command_Line.Parameter));
            end if;
         when others =>
            exit;
      end case;
   end loop;
   if Print_Help then
      null;
   else

      declare
         package my_tree is new Network_Tree(family=>Family ,port=>Port);
         End_Of_Arguments: Boolean:=False;
         Try_Again: exception;
         Format_Error: exception;
      begin
         while not End_Of_Arguments loop
            declare
               Argument: constant String:=GNAT.Command_Line.Get_Argument(End_Of_Arguments => End_Of_Arguments);
               use Ada.Strings.Fixed;
            begin
               pragma Debug (Put_Line(Standard_Error,"parsing '"&Argument&"'."));
               if Argument'Length = 0 then
                  raise Try_Again; -- continue the loop
               end if;
               if GNAT.Sockets.Is_IPv4_Address(Argument)
                 or else GNAT.Sockets.Is_IPv6_Address(Argument)
               then
                  my_tree.Client(GNAT.Sockets.Inet_Addr(Argument),Port);
               elsif Argument(1) = '[' then
                  declare
                     Close_Bracket: constant Natural :=Index(Argument,"]");
                  begin
                     if GNAT.Sockets.Is_IPv6_Address(Name => Argument(2..Close_Bracket-1))
                       or else GNAT.Sockets.Is_IPv4_Address(Name => Argument(2..Close_Bracket-1))
                     then
                        if Close_Bracket=Argument'Last then
                           my_tree.Client(GNAT.Sockets.Inet_Addr(Argument(2..Close_Bracket-1)),
                                          Port);
                        elsif Argument(Close_Bracket+1)=':' then
                           my_tree.Client(GNAT.Sockets.Inet_Addr(Argument(2..Close_Bracket-1)),
                                          GNAT.Sockets.Port_Type'Value(Argument(Close_Bracket+2..Argument'Last)));
                           -- If not numeric, will raise an exception.
                        else
                           raise Format_Error with "The closing bracket is not the last character, or followed by a colon.";
                        end if;
                     end if;
                  end;
               else -- should be an ipv4 address with a port
                  if Ada.Strings.Fixed.Count(Argument,":") > 1 then
                     raise Format_Error with "'" & Argument(Index(Argument,":")..Argument'Last) &
                       "' is not a Natural number (for IPv6, the expected format is along the lines of [::1]:1234)";
                  elsif Ada.Strings.Fixed.Count(Argument,":") /=1 then
                     raise Format_Error with "Argument '"&Argument&"' was not recognised as an IP address (with an optional port).";
                  end if;
                  declare
                     Colon: constant Natural:=Index(Argument,":");
                  begin
                     if GNAT.Sockets.Is_IPv4_Address(Argument(1..Colon-1)) then
                        my_tree.Client(GNAT.Sockets.Inet_Addr(Argument(1..Colon-1)),
                                       GNAT.Sockets.Port_Type'Value(Argument(Colon+1..Argument'Last)));
                     end if;
                  end;
               end if;
            exception
               when E: GNAT.Sockets.Socket_Error =>
                  Put_Line(Standard_Error,"Error parsing IP address in '"&Argument&"':");
                  Put_Line(Standard_Error,Ada.Exceptions.Exception_Message(E));
               when E: Constraint_Error =>
                  Put_Line(Standard_Error,"Error parsing argument '"&Argument&"':");
                  Put_Line(Standard_Error,Ada.Exceptions.Exception_Message(E));
               when E: Format_Error =>
                  Put_Line(Standard_Error,"Error parsing argument '"&Argument&"':");
                  Put_Line(Standard_Error,Ada.Exceptions.Exception_Message(E));
               when Try_Again =>
                  null; -- Used for a continue equivalent
               when others =>
                  Put_Line(Standard_Error,"Unknown error parsing argument '"&Argument&"':");
            end;
         end loop;
      end;
   end if;
end Build;
