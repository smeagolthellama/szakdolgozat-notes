with Ada.Exceptions;
with GNAT.Sockets; use GNAT.Sockets;
with Interfaces;   use Interfaces;
with Memory_Stream;
with Ada.Streams;  use Ada.Streams;
with Text_IO;
with Ada.Text_IO;

package body Network_Tree is
   type Child_Number is range 0 .. 2;
   subtype Child_Index is Child_Number range 1 .. Child_Number'Last;

   type Child_Set is array (Child_Index) of Sock_Addr_Type;

   type Child_Status is (Success, NotExist, Empty);
   protected Children is

      entry Add_Child (child : Sock_Addr_Type);
      entry Get_Children (n : out Child_Number; c : out Child_Set);
      entry Remove_Child (child : Sock_Addr_Type; status : out Child_Status);
      pragma Unreferenced (Remove_Child);
   private
      number : Child_Number := 0;
      set    : Child_Set    := (others => No_Sock_Addr);
      locked : Boolean      := False;
   end Children;

   protected body Children is
      entry Add_Child (child : Sock_Addr_Type) when not locked is
      begin
         locked       := True;
         number       := number + 1;
         set (number) := child;
         locked       := False;
      end Add_Child;

      entry Get_Children (n : out Child_Number; c : out Child_Set)
        when not locked is
      begin
         locked := True;
         n      := number;
         if number < 2 then
            set (2) := No_Sock_Addr;
         end if;
         if number < 1 then
            set (1) := No_Sock_Addr;
         end if;
         c      := set;
         locked := False;
      end Get_Children;

      entry Remove_Child (child : Sock_Addr_Type; status : out Child_Status)
        when not locked is
      begin
         if number = 0 then
            status := Empty;
            return;
         end if;
         status := NotExist;
         for i in set'Range loop
            if status = Success then
               if i /= set'Last then
                  set (i) := set (i + 1);
               end if;
            else

               if child = set (i) then
                  set (i) := No_Sock_Addr;
                  status  := Success;
               end if;
            end if;

         end loop;
      end Remove_Child;
   end Children;

   Message_Number : Unsigned_16 := 0;

   task listen_Request is
      entry new_Request
        (sock          : Socket_Type; address : Sock_Addr_Type;
         message       : Stream_Element_Array;
         messageLength : Stream_Element_Offset);
   end listen_Request;

   task body listen_Request is
      socket : Socket_Type;
      msg    : Stream_Element_Array (1 .. maxMessageLength);
      msgLen : Stream_Element_Offset;
      talker : Sock_Addr_Type;
      str    : constant Memory_Stream.Stream_Access :=
        new Memory_Stream.Memory_Buffer_Stream (maxMessageLength);
   begin
      loop
         select
            accept new_Request
              (sock          : Socket_Type; address : Sock_Addr_Type;
               message       : Stream_Element_Array;
               messageLength : Stream_Element_Offset)
            do
               socket := sock;
               msg    := message;
               msgLen := messageLength;
               talker := address;
            end new_Request;
            if msgLen >= 1 then
               declare
                  flags : constant Unsigned_8 := Unsigned_8 (msg (1));
               begin
                  case flags is
                     when Character'Pos ('?') =>
                        declare
                           number : Child_Number;
                           cSet   : Child_Set;
                           flags  : Unsigned_16 := 0;
                        begin
                           Children.Get_Children (number, cSet);
                           case number is
                              when 0 =>
                                 Unsigned_16'Write (str, flags);
                                 Unsigned_16'Write (str, 0);
                                 Child_Set'Write (str, cSet);
                              when 1 =>
                                 if cSet (1).Family /= Family_Inet then
                                    flags := flags + 1;
                                 end if;
                                 Unsigned_16'Write (str, flags);
                                 Unsigned_16'Write (str, 1);
                                 Child_Set'Write (str, cSet);
                              when 2 =>
                                 if cSet (1).Family /= Family_Inet then
                                    flags := flags + 1;
                                 end if;
                                 if cSet (2).Family /= Family_Inet then
                                    flags := flags + 2;
                                 end if;
                                 Unsigned_16'Write (str, flags);
                                 Unsigned_16'Write (str, 2);
                                 Child_Set'Write (str, cSet);
                           end case;
                        end;
                     when Character'Pos ('j') =>
                        declare
                           number : Child_Number;
                           cSet   : Child_Set;
                           buf    : constant Memory_Stream.Stream_Access :=
                             new Memory_Stream.Memory_Buffer_Stream
                               (maxMessageLength);
                           Child_Address : Sock_Addr_Type := talker;
                        begin
                           Children.Get_Children (number, cSet);
                           Memory_Stream.Write
                             (Memory_Stream.Memory_Buffer_Stream (buf.all),
                              msg (msg'First + 1 .. msg'Last));
                           Port_Type'Read (buf, Child_Address.Port);
                           if number = 2 then
                              String'Write (str, "err");
                           else
                              Children.Add_Child (child => Child_Address);
                              String'Write (str, "ok");
                              Unsigned_16'Write (str, Message_Number);
                           end if;
                           Memory_Stream.Free (buf);
                        exception
                           when E : Constraint_Error =>
                              String'Write (str, "err");
                              Memory_Stream.Free (buf);
                              Text_IO.Put_Line
                                ("Error when trying to add child: " &
                                 Ada.Exceptions.Exception_Message (E));
                        end;
                     when others =>
                        Message_Number := Message_Number + 1;
                  end case;
               end;
               declare
                  outbound : Stream_Element_Array (1 .. maxMessageLength);
               begin
                  Memory_Stream.Read
                    (Memory_Stream.Memory_Buffer_Stream (str.all), outbound,
                     msgLen);
                  Text_IO.Put_Line ("msgLen is " & msgLen'Image & ".");
                  if msgLen = 0 then
                     outbound (1) := 0;
                     msgLen       := 1;
                  end if;
                  Send_Socket (socket, outbound (1 .. msgLen), msgLen, talker);
               end;
            end if;
         or
            terminate;
         end select;
      end loop;
   exception
      when E : others =>
         Text_IO.Put_Line
           ("Mesenger thread error:" & Ada.Exceptions.Exception_Message (E));
   end listen_Request;

   task body Server is
      listeningSocket  : Socket_Type;
      listeningAddress : Sock_Addr_Type (family);
   begin
      Create_Socket
        (listeningSocket, family, Socket_Datagram, IP_Protocol_For_UDP_Level);
      listeningAddress.Addr :=
        (if family = Family_Inet then Any_Inet_Addr else Any_Inet6_Addr);
      listeningAddress.Port := port;
      Bind_Socket (listeningSocket, listeningAddress);
      loop
         declare
            talkingAddress : Sock_Addr_Type;
            message        : Stream_Element_Array (1 .. maxMessageLength);
            messageLength  : Stream_Element_Offset;
         begin
            Receive_Socket
              (listeningSocket, message, messageLength, talkingAddress);
            listen_Request.new_Request
              (sock    => listeningSocket, address => talkingAddress,
               message => message, messageLength => messageLength);
         end;
      end loop;
   exception
      when E : Socket_Error =>
         Text_IO.Put_Line
           ("Server thread error:" & Ada.Exceptions.Exception_Message (E));
   end Server;

   procedure Client (addr : Inet_Addr_Type; port : Port_Type) is
   begin
      pragma Debug
        (Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Client(" & Image (Value => addr) & "," & port'Image &
            ") called."));

   end Client;
end Network_Tree;
