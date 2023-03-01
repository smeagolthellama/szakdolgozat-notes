with Ada.Text_IO;
with GNAT.Sockets;
with Network_Tree;
with Ada.Command_Line;
with GNAT.Command_Line;

procedure Build is
   Print_Help: Boolean:=False;
   Port: GNAT.Sockets.Port_Type:=10573;
   Family: GNAT.Sockets.Family_Type:=GNAT.Sockets.Family_Inet;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error,"Usage: "&Ada.Command_Line.Command_Name&" [options] host[:port] [...]");
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
      begin
         while not End_Of_Arguments loop
            my_tree.Client(GNAT.Sockets.Inet_Addr(GNAT.Command_Line.Get_Argument(End_Of_Arguments => End_Of_Arguments)));
         end loop;
      end;
   end if;
end Build;
