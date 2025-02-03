with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;

with GNAT.Sockets; use GNAT.Sockets;

with Interfaces; use Interfaces;

with Memory_Stream;

with Network_Children;
with Network_Utility_Functions;

with Debug;

package body Network_Tree is
   use Network_Children;
   use Network_Utility_Functions;
   use Debug;

   Local_Message_Number : Unsigned_16 := 0;

   procedure Handle_Join_Request(
      Msg    : Stream_Element_Array;
      MsgLen : Stream_Element_Offset;
      Str    : Memory_Stream.Stream_Access :=
                 new Memory_Stream.Memory_Buffer_Stream (Max_Message_Length)
   ) is
       Number               : Child_Number;
       CSet                 : Child_Set;
       Buf                  : constant Memory_Stream.Stream_Access :=
                                new Memory_Stream.Memory_Buffer_Stream
                                  (Max_Message_Length);
       Child_Address_Family : Family_Inet_4_6;
       Child_Family_Number  : Unsigned_8;
   begin
       Children.Get_Children (Number, CSet);
       Memory_Stream.Write
         (Memory_Stream.Memory_Buffer_Stream (Buf.all),
          Msg (Msg'First + 1 .. Msg'First + MsgLen - 1));
       Unsigned_8'Read (Buf, Child_Family_Number);
       if Child_Family_Number = 4 then
          Child_Address_Family := Family_Inet;
       else
          Child_Address_Family := Family_Inet6;
       end if;
       declare
          Child_Address :
          Sock_Addr_Type (Child_Address_Family);
       begin
          Port_Type'Read (Buf,Child_Address.Port);
          if Number = 2 then
	     Debug.Handle_Event(Join_Request_Denied);
             String'Write (Str, "err");
          else
	     Debug.Handle_Event(Join_Request_Accepted);
             Children.Add_Child (Child => Child_Address);
             String'Write (Str, "ok");
             Unsigned_16'Write
               (Str, Local_Message_Number);
          end if;
       end;
       Memory_Stream.Free (Buf);
    exception
       when E : Constraint_Error =>
          String'Write (Str, "err");
          Memory_Stream.Free (Buf);
          Debug.Handle_Event(ERR_Exception, "Handle_Join_Request", Ada.Exceptions.Exception_Information(E));
   end Handle_Join_Request;

   task Request_Handler is
      entry New_Request
        (Sock          : Socket_Type; Address : Sock_Addr_Type;
         Message       : Stream_Element_Array;
         MessageLength : Stream_Element_Offset);
   end Request_Handler;

   task body Request_Handler is
      Socket : Socket_Type;
      Msg    : Stream_Element_Array (1 .. Max_Message_Length);
      MsgLen : Stream_Element_Offset;
      Talker : Sock_Addr_Type;
      Str    : constant Memory_Stream.Stream_Access :=
                 new Memory_Stream.Memory_Buffer_Stream (Max_Message_Length);
   begin
      Debug.Handle_Event(Routine_Called, "Request_Handler");
      loop
         select
            accept New_Request
              (Sock          : Socket_Type; Address : Sock_Addr_Type;
               Message       : Stream_Element_Array;
               MessageLength : Stream_Element_Offset)
            do
	       Debug.Handle_Event(
                       Routine_Called,
		       "Request_Handler.New_Request",
		       Image(Sock),
		       Image(Address),
		       Image(
			       Message(Message'First .. Message'First + MessageLength - 1)
			       ),
		       MessageLength'Image );
               Socket := Sock;
               Msg    := Message;
               MsgLen := MessageLength;
               Talker := Address;
            end New_Request;
            if MsgLen >= 1 then
               declare
                  Message_Type : constant Unsigned_8 := Unsigned_8 (Msg (1));
               begin
		  Debug.Handle_Event(Debug.Message_Type_Selector, Message_Type'Image, Character'Val(Message_Type)'Image);
                  case Message_Type is
                     when Character'Pos ('?') =>
			Debug.Handle_Event(Message_Type_Query);
                        declare
                           Number : Child_Number;
                           CSet   : Child_Set;
                        begin
                           Children.Get_Children (Number, CSet);
                           Unsigned_16'Write (Str, Unsigned_16 (Number));
                           Child_Set'Write (Str, CSet);
                        end;
                     when Character'Pos ('j') =>
			Debug.Handle_Event(Message_Type_Join);
                        Handle_Join_Request(Msg,MsgLen,Str);
                     when Character'Pos ('>') =>
			Debug.Handle_Event(Message_Type_Fwd);
                        Local_Message_Number := Local_Message_Number + 1;
                        declare
                           Message_Message_Number               : Unsigned_16;
                           Receive_Buf                          :
                           constant Memory_Stream.Stream_Access :=
                                                                    new Memory_Stream.Memory_Buffer_Stream
                                                                      (Max_Message_Length);
                           Send_Buf                             : constant Memory_Stream.Stream_Access :=
                                                                    new Memory_Stream.Memory_Buffer_Stream
                                                                      (Max_Message_Length);
                           Destinations                         : Child_Set;
                           Destination_Count                    : Child_Number;
                           Outbound_Message                     :
                           Stream_Element_Array (1 .. Max_Message_Length);
                           Outbound_Message_Length              : Stream_Element_Offset;
                        begin
                           Memory_Stream.Write
                             (Memory_Stream.Memory_Buffer_Stream
                                (Receive_Buf.all),
                              Msg (Msg'First + 1 .. Msg'First + MsgLen - 1));
                                                        Unsigned_16'Read (Receive_Buf,Message_Message_Number);
                           if Message_Message_Number /= Local_Message_Number
                           then
                              -- TODO: handle wrong order
			      Debug.Handle_Event(ERR_Wrong_Order,Local_Message_Number'Image, Message_Message_Number'Image);
                              null;
                           end if;
                           Stream_Element'Write (Send_Buf, Msg (Msg'First));
                           Unsigned_16'Write
                             (Send_Buf,
                              Local_Message_Number); --Might need rewriting for message order handling
                           Stream_Element_Array'Write
                             (Send_Buf,
                              Msg
                                (Msg'First + 1 + 2 .. Msg'First + MsgLen - 1));
			   Debug.Handle_Event(Message_Contents, Image(Msg(Msg'First+3 .. Msg'First +MsgLen -1)));
                           Ada.Text_IO.Put
                             (Message_File,
                              Image
                                (Msg
                                     (Msg'First + 3 .. Msg'First + MsgLen - 1)));
                           Memory_Stream.Read
                             (Memory_Stream.Memory_Buffer_Stream
                                (Send_Buf.all),
                              Outbound_Message, Outbound_Message_Length);
                           Children.Get_Children
                             (Destination_Count, Destinations);
                           if Destination_Count > 0 then
                              for Destination of Destinations
                                (1 .. Destination_Count)
                              loop
                                 declare
                                    Socket : Socket_Type;
                                 begin
                                    Create_Socket
                                      (Socket => Socket,
                                       Family => Destination.Family,
                                       Mode   => Socket_Datagram);
                                    Send_Socket
                                      (Socket,
                                       Outbound_Message
                                         (Outbound_Message'First ..
                                              Outbound_Message_Length),
                                       Outbound_Message_Length, Destination);
                                 end;
                              end loop;
                           end if;
                        end;
                     when others =>
			Debug.Handle_Event(ERR_Message_Type_Unknown,Image(Msg));
                  end case;
               end;
               declare
                  Outbound : Stream_Element_Array (1 .. Max_Message_Length);
               begin
		  Debug.Handle_Event(Sending_Reply);
                  Memory_Stream.Read
                    (Memory_Stream.Memory_Buffer_Stream (Str.all), Outbound,
                     MsgLen);
		  Debug.Handle_Event(Sending_Length,MsgLen'Image);
                  if MsgLen = 0 then
                     Outbound (1) := 0;
                     MsgLen       := 1;
                  end if;
                  Send_Socket (Socket, Outbound (1 .. MsgLen), MsgLen, Talker);
               end;
            end if;
         or
            terminate;
         end select;
      end loop;
   exception
      when E : others =>
	      Debug.Handle_Event(ERR_Exception,"Request_Handler",Ada.Exceptions.Exception_Information(E));
   end Request_Handler;

   task body Server is
      ListeningSocket  : Socket_Type;
      ListeningAddress : Sock_Addr_Type (Package_Default_Network_Family);
   begin
	   Debug.Handle_Event(Thread_Start,"Server");
      Create_Socket
        (ListeningSocket, Package_Default_Network_Family, Socket_Datagram,
         IP_Protocol_For_UDP_Level);
      Debug.Handle_Event(Socket_Created, Package_Default_Network_Family'Image);
      ListeningAddress.Addr :=
        (if Package_Default_Network_Family = Family_Inet then Any_Inet_Addr
         else Any_Inet6_Addr);
      ListeningAddress.Port := Package_Default_Port;
      Bind_Socket (ListeningSocket, ListeningAddress);
      Debug.Handle_Event(Socket_Bound, Image(ListeningAddress));
      Debug.Handle_Event(Loop_Start,"Listen");
      loop
         declare
            TalkingAddress : Sock_Addr_Type;
            Message        : Stream_Element_Array (1 .. Max_Message_Length);
            MessageLength  : Stream_Element_Offset;
         begin
            Receive_Socket
              (ListeningSocket, Message, MessageLength, TalkingAddress);
	    Debug.Handle_Event(Listen_Loop_Recvd);
            Request_Handler.New_Request
              (Sock    => ListeningSocket, Address => TalkingAddress,
               Message => Message, MessageLength => MessageLength);
         end;
      end loop;
   exception
      when E : Socket_Error =>
	Debug.Handle_Event(ERR_Exception, "Server", Ada.Exceptions.Exception_Message(E));
   end Server;

   task Client_Thread is
      entry Try_New_Server (Addr : Inet_Addr_Type; Port : Port_Type);
   end Client_Thread;

   procedure Connect_To_Server (Addr : Inet_Addr_Type; Port : Port_Type) is
   begin
      Debug.Handle_Event(Routine_Called, "Connect_To_Server", Image(Value=>Addr), Port'Image);
      Client_Thread.Try_New_Server (Addr, Port);
   end Connect_To_Server;

   task body Client_Thread is
      package Queue_Interface is new Ada.Containers
        .Synchronized_Queue_Interfaces
          (Element_Type => Sock_Addr_Type);
      package Address_Queues is new Ada.Containers
        .Unbounded_Synchronized_Queues
          (Queue_Interface);
      Queue : Address_Queues.Queue;

      task Server_Selector is
         entry Reconnect;
         pragma Unreferenced (Reconnect);
      end Server_Selector;

      task body Server_Selector is
         -- The string to send to ask for the number of connected children
         Query_String : constant Stream_Element_Array :=
                          (1 => Character'Pos ('?'));
         -- The string to send to ask to join the server (needs to be constucted using the port number provided in the package instantiation)
         Join_String  : Stream_Element_Array (1 .. Max_Message_Length);
         -- A memory_stream used to construct the previous variable (needs to be deallocated)
         Buf          : constant Memory_Stream.Stream_Access :=
                          new Memory_Stream.Memory_Buffer_Stream (Max_Message_Length);
         -- The length of the join request. Gets set when the join request is sent.
         Join_String_Length : Stream_Element_Offset;
         -- A Selector for listening to multiple sockets at once.
         Selector     : Selector_Type;

         Max_Retries : constant Integer := 5;
         type Connection_Retry_Count is range 1 .. Max_Retries;

         -- The number of times I have tried to connect to each of the sockets in each set.
         Connections_By_Retry_Count :
           array (Connection_Retry_Count) of Socket_Set_Type;

         use Ada.Containers;
      begin
         Debug.Handle_Event(Thread_Start, "Server_Selector");
         -- prepare the join request string
         String'Write (Buf, "j");
         Unsigned_8'Write
           (Buf,
            (if Package_Default_Network_Family = Family_Inet then 4 else 6));
         Port_Type'Write (Buf, Package_Default_Port);
         Memory_Stream.Read
           (Memory_Stream.Memory_Buffer_Stream (Buf.all), Join_String,
            Join_String_Length);
         Memory_Stream.Free (Buf);
         Debug.Handle_Event(Var_Is, "Join_String", Image(Join_String));
         Create_Selector (Selector);

         for I in Connections_By_Retry_Count'Range loop
            Sock.Empty (Connections_By_Retry_Count (I));
         end loop;

         Debug.Handle_Event(Loop_Start,"Server_Selector");
         loop
            Reconnect_Loop :
            loop
               Debug.Handle_Event(Condition_Check, "for new servers in the queue");
               while Queue.Current_Use > 0 loop
                  -- Add the new address to the set
                  declare
                     Server_Address   : Sock_Addr_Type;
                     Server_Socket    : Socket_Type;
                     Transmitted_Data : Stream_Element_Offset;
                  begin
                     -- Dequeue blocks until there is data available
                     Queue.Dequeue (Server_Address);
                     Create_Socket
                       (Server_Socket, Server_Address.Family, Socket_Datagram);
                     Send_Socket
                       (Server_Socket, Query_String, Transmitted_Data,
                        Server_Address);
                     Set (Connections_By_Retry_Count (1), Server_Socket);
                  end;
               end loop;
               Debug.Handle_Event(Selector_Prepare);
               declare
                  R_Set          : Socket_Set_Type;
                  W_Set_Dummy    : Socket_Set_Type;
                  Tmp_Copy       : Socket_Set_Type;
                  Socket_To_Read : Socket_Type;
                  Status         : Selector_Status;
               begin
                  Empty (W_Set_Dummy);
                  -- copy all sockets over to the read set, to be selected between.
                  for I in Connections_By_Retry_Count'Range loop
                     Copy (Connections_By_Retry_Count (I), Tmp_Copy);
                     loop
                        Get (Tmp_Copy, Socket_To_Read);
                        exit when Socket_To_Read = No_Socket;
                        Set (R_Set, Socket_To_Read);
                     end loop;
                  end loop;
                  -- check the selector: is there any incoming data?
                  Check_Selector (Selector, R_Set, W_Set_Dummy, Status, 0.3);
                  for I in Connections_By_Retry_Count'Range loop
                     Copy (Connections_By_Retry_Count (I), Tmp_Copy);
                     loop
                        Get (Tmp_Copy, Socket_To_Read);
                        exit when Socket_To_Read = No_Socket;
                        if not Is_Set (R_Set, Socket_To_Read) then
                           Clear
                             (Connections_By_Retry_Count (I), Socket_To_Read);
                           if I /= Connections_By_Retry_Count'Last then
                               Set (R_Set, Socket_To_Read);
                           else
                               Debug.Handle_Event(Selector_Timeout,
                                       Image (Socket_To_Read));
                           end if;
                        end if;
                     end loop;
                  end loop;
                  Debug.Handle_Event(Selector_Return, Status'Image);
                  case Status is
                     when Aborted =>
                        exit;
                     when Expired =>
                        null;
                     when Completed =>
                        -- parse the response(s).
                        while not Is_Empty (R_Set) loop
                           Get (R_Set, Socket_To_Read);
                           declare
                              Addr           : Sock_Addr_Type;
                              Message        :
                              Stream_Element_Array (1 .. Max_Message_Length);
                              Message_Length : Stream_Element_Offset;
                           begin
                              Receive_Socket
                                (Socket_To_Read, Message, Message_Length, Addr,
                                 Wait_For_A_Full_Reception);
                              Debug.Handle_Event(Connection_Try, Image(Addr));
                              exit Reconnect_Loop when Message_Length >= 2 and
                                Message (1) = Character'Pos ('o') and
                                Message (2) = Character'Pos ('k');
                              Debug.Handle_Event(Connection_Fail, Image(Addr));
                              if
                                (Message_Length >= 3 and
                                   Message (1) = Character'Pos ('e') and
                                     Message (2) = Character'Pos ('r') and
                                     Message (3) = Character'Pos ('r'))
                              then
                                 Debug.Handle_Event(Join_Request_Denied);
                                 for I in Connections_By_Retry_Count'Range loop
                                    if Is_Set
                                      (Connections_By_Retry_Count (I),
                                       Socket_To_Read)
                                    then
                                       Clear
                                         (Connections_By_Retry_Count (I),
                                          Socket_To_Read);
                                       Shutdown_Socket (Socket_To_Read);
                                       exit;
                                    end if;
                                 end loop;
                              else
                                 declare
                                    Number                               : Unsigned_16;
                                    Servers_Children                     : Child_Set;
                                    Buf                                  :
                                    constant Memory_Stream.Stream_Access :=
                                                                             new Memory_Stream.Memory_Buffer_Stream
                                                                               (Max_Message_Length);
                                    Transmitted_Data                     : Stream_Element_Offset;
                                 begin
                                    Memory_Stream.Write
                                      (Memory_Stream.Memory_Buffer_Stream
                                         (Buf.all),
                                       Message);
                                    Unsigned_16'Read (Buf, Number);
                                    Child_Set'Read (Buf, Servers_Children);
                                    for I in Servers_Children'Range loop
                                       if Servers_Children (I).Family =
                                         Family_Inet and
                                         Servers_Children (I).Addr =
                                         Any_Inet_Addr
                                       then
                                          if Addr.Family = Family_Inet then
                                             Servers_Children (I).Addr :=
                                               Addr.Addr;
                                          end if;
                                       end if;
                                       if Servers_Children (I).Family =
                                         Family_Inet6 and
                                         Servers_Children (I).Addr =
                                         Any_Inet6_Addr
                                       then
                                          if Addr.Family = Family_Inet6 then
                                             Servers_Children (I).Addr :=
                                               Addr.Addr;
                                          end if;
                                       end if;
                                    end loop;

                                    case Number is
                                       when 0 | 1 =>
                                          Send_Socket
                                            (Socket_To_Read,
                                             Join_String
                                               (1 .. Join_String_Length),
                                             Transmitted_Data, Addr);
                                       when 2 =>
                                          Connect_To_Server
                                            (Servers_Children (1).Addr,
                                             Servers_Children (1).Port);
                                          Connect_To_Server
                                            (Servers_Children (2).Addr,
                                             Servers_Children (2).Port);
                                          for I in Connections_By_Retry_Count'
                                          Range
                                          loop
                                             if Is_Set
                                               (Connections_By_Retry_Count
                                                  (I),
                                                Socket_To_Read)
                                             then
                                                Clear
                                                  (Connections_By_Retry_Count
                                                     (I),
                                                   Socket_To_Read);
                                                Close_Socket (Socket_To_Read);
                                                exit;
                                             end if;
                                          end loop;
                                       when others =>
                                          -- server allegedly has more than two kids, and we can't trust out parsing. We'll just ignore it.
                                          null;
                                    end case;
                                 end;
                              end if;
                           end;
                        end loop;
                  end case;
               end;
            end loop Reconnect_Loop;
            Debug.Handle_Event(Connection_Success);
            select
               accept Reconnect;
            or
               terminate;
            end select;
         end loop;
      exception
         when E : Socket_Error =>
             Debug.Handle_Event(ERR_Exception,  "Server_Selector2", Ada.Exceptions.Exception_Information(E));
      end Server_Selector;

   begin
      loop
         select
            accept Try_New_Server (Addr : Inet_Addr_Type; Port : Port_Type) do
               if Addr.Family = Family_Inet then
                  Queue.Enqueue
                    (Sock_Addr_Type'
                       (Family => Family_Inet, Addr => Addr, Port => Port));
               else
                  Queue.Enqueue
                    (Sock_Addr_Type'
                       (Family => Family_Inet6, Addr => Addr, Port => Port));
               end if;
            end Try_New_Server;
         or
            terminate;
         end select;
      end loop;
   exception
      when E : Socket_Error =>
          Debug.Handle_Event(ERR_Exception, "Client_Thread", Ada.Exceptions.Exception_Information(E));
   end Client_Thread;
end Network_Tree;
