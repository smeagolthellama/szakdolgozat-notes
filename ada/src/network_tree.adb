with Ada.Exceptions;
with GNAT.Sockets;
use GNAT.Sockets;
with Interfaces; use Interfaces;
with Memory_Stream;
with Ada.Streams; use Ada.Streams;
with Text_IO;

package body Network_Tree is

   type ChildNumber is range 0..2;
   subtype ChildIndex is ChildNumber range 1..ChildNumber'Last;

   type ChildSet is array (ChildIndex) of Sock_Addr_Type;

   protected Children is
      entry AddChild(child: Sock_Addr_Type);
      entry GetChildren(n:out ChildNumber; c:out ChildSet);
      -- entry RemoveChild
   private
      number: ChildNumber := 0;
      set: ChildSet:=(others=>No_Sock_Addr);
      locked: Boolean:=False;
   end Children;

   protected body Children is
      entry AddChild (child: Sock_Addr_Type)
        when not locked is
      begin
         locked:=True;
         number:=number+1;
         set(number):=child;
         locked:=False;
      end AddChild;

      entry GetChildren(n: out ChildNumber; c: out ChildSet)
        when not locked is
      begin
         locked:=True;
         n:=number;
         if number<2 then
            set(2):=No_Sock_Addr;
         end if;
         if number<1 then
            set(1):=No_Sock_Addr;
         end if;
         c:=set;
         locked:=False;
      end GetChildren;
   end Children;

   Message_Number: Unsigned_16:=0;

   task listenRequest is
      entry newRequest(sock: Socket_Type; address: Sock_Addr_Type; message: Stream_Element_Array; messageLength: Stream_Element_Offset);
   end listenRequest;

   task body listenRequest is
      socket: Socket_Type;
      msg: Stream_Element_Array(1..maxMessageLength);
      msgLen: Stream_Element_Offset;
      talker: Sock_Addr_Type;
      str: constant Memory_Stream.Stream_Access:=new Memory_Stream.Memory_Buffer_Stream(maxMessageLength);
   begin
      loop
         select
            accept newRequest
              (sock: Socket_Type;address: Sock_Addr_Type; message: Stream_Element_Array; messageLength: Stream_Element_Offset) do
               socket:=sock;
               msg:=message;
               msgLen:=messageLength;
               talker:=address;
            end newRequest;
            if msgLen>=1 then
               declare
                  flags: constant Unsigned_8:=Unsigned_8(msg(1));
               begin
                  case flags is
                  when Character'Pos('?') =>
                     declare
                        number: ChildNumber;
                        cSet: ChildSet;
                        flags: Unsigned_16:=0;
                     begin
                        Children.GetChildren(number,cSet);
                        case number is
                        when 0 =>
                           Unsigned_16'Write(str,flags);
                           Unsigned_16'Write(str,0);
                           ChildSet'Write(str,cSet);
                        when 1 =>
                           if cSet(1).Family/= Family_Inet then
                              flags:= flags + 1;
                           end if;
                           Unsigned_16'Write(str,flags);
                           Unsigned_16'Write(str,1);
                           ChildSet'Write(str,cSet);
                        when 2=>
                           if cSet(1).Family/= Family_Inet then
                              flags:= flags + 1;
                           end if;
                           if cSet(2).Family/=Family_Inet then
                              flags:=flags + 2;
                           end if;
                           Unsigned_16'Write(str,flags);
                           Unsigned_16'Write(str,2);
                           ChildSet'Write(str,cSet);
                        end case;
                     end;
                  when Character'Pos('j') =>
                     declare
                        number: ChildNumber;
                        cSet: ChildSet;
                     begin
                        Children.GetChildren(number,cSet);
                        if number=2 then
                           String'Write(str,"err");
                        else
                           Children.AddChild(talker);
                           String'Write(str,"ok");
                           Unsigned_16'Write(str,Message_Number);
                        end if;
                     exception
                        when Constraint_Error =>
                           String'Write(str,"err");
                     end;
                     when others =>
                        Message_Number:=Message_Number+1;
                  end case;
               end;
               declare
                  outbound: Stream_Element_Array(1..maxMessageLength);
               begin
                  Memory_Stream.Read(Memory_Stream.Memory_Buffer_Stream(str.all),outbound,msgLen);
                  Text_IO.Put_Line("msgLen is "&msgLen'Image&".");
                  if msgLen=0 then
                     outbound(1):=0;
                     msgLen:=1;
                  end if;
                  Send_Socket(socket,outbound(1..msgLen),msgLen,talker);
               end;
            end if;
         or
            terminate;
         end select;
      end loop;
   exception
      when E: others =>
         text_IO.put_Line("Mesenger thread error:"&Ada.Exceptions.Exception_Message(E));
   end listenRequest;

   task body Server is
      listeningSocket: Socket_Type;
      listeningAddress: Sock_Addr_Type(family);
   begin
      Create_Socket(listeningSocket, family, Socket_Datagram, IP_Protocol_For_UDP_Level);
      listeningAddress.Addr:=(if family = Family_Inet then Any_Inet_Addr else Any_Inet6_Addr);
      listeningAddress.Port:=port;
      Bind_Socket(listeningSocket, listeningAddress);
      loop
         declare
            talkingAddress: Sock_Addr_Type;
            message: Stream_Element_Array(1..maxMessageLength);
            messageLength: Stream_Element_Offset;
         begin
            Receive_Socket(listeningSocket,message,messageLength,talkingAddress);
            listenRequest.newRequest(sock=>listeningSocket, address => talkingAddress, message => message, messageLength => messageLength);
         end;
      end loop;
   exception
      when E: Socket_Error =>
         Text_IO.Put_Line("Server thread error:" & Ada.Exceptions.Exception_Message(E));
   end Server;

   procedure Client (addr: Inet_Addr_Type) is
   begin
      null;
   end Client;
end Network_Tree;
