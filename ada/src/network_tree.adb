with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;

with GNAT.Sockets; use GNAT.Sockets;

with Interfaces; use Interfaces;

with Memory_Stream;

with Text_IO;

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
            pragma Debug
               (Text_IO.Put_Line
                   (Text_IO.Standard_Error,
                     "Add_Child(" & Image (child) & ") called."));
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
            c := set;
            pragma Debug
               (Text_IO.Put_Line
                   (Text_IO.Standard_Error,
                    "Get_Children(" & n'Image & " (out), [" & Image (c (1)) &
                    ", " & Image (c (2)) & "] (out)) called."));
            locked := False;
        end Get_Children;

        entry Remove_Child (child : Sock_Addr_Type; status : out Child_Status)
           when not locked is
        begin
            locked := True;
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
            pragma Debug
               (Text_IO.Put_Line
                   (Text_IO.Standard_Error,
                    "Remove_Child(" & Image (child) & ", " & status'Image &
                    ") called"));
            locked := False;
        end Remove_Child;
    end Children;

    Message_Number : Unsigned_16 := 0;

    task Request_Handler is
        entry new_Request
           (sock          : Socket_Type; address : Sock_Addr_Type;
            message       : Stream_Element_Array;
            messageLength : Stream_Element_Offset);
    end Request_Handler;

    task body Request_Handler is
        socket : Socket_Type;
        msg    : Stream_Element_Array (1 .. MaxMessageLength);
        msgLen : Stream_Element_Offset;
        talker : Sock_Addr_Type;
        str    : constant Memory_Stream.Stream_Access :=
           new Memory_Stream.Memory_Buffer_Stream (MaxMessageLength);
    begin
        pragma Debug
           (Text_IO.Put_Line
               (Text_IO.Standard_Error, "Request_Handler starting..."));
        loop
            select
                accept new_Request
                   (sock          : Socket_Type; address : Sock_Addr_Type;
                    message       : Stream_Element_Array;
                    messageLength : Stream_Element_Offset)
                do
                    pragma Debug
                       (Text_IO.Put_Line
                           (Text_IO.Standard_Error,
                            "Request_Handler.new_Request(" & Image (sock) &
                            "," & Image (address) &
                            ", <Stream_Element_Array> ," &
                            messageLength'Image & ") called."));
                    socket := sock;
                    msg    := message;
                    msgLen := messageLength;
                    talker := address;
                end new_Request;
                if msgLen >= 1 then
                    declare
                        message_type : constant Unsigned_8 :=
                           Unsigned_8 (msg (1));
                    begin
                        pragma Debug
                           (Text_IO.Put_Line
                               (Text_IO.Standard_Error,
                                "message_type is " & message_type'Image &
                                " ('" & Character'Val (message_type) & "')"));
                        case message_type is
                            when Character'Pos ('?') =>
                                pragma Debug
                                   (Text_IO.Put_Line
                                       (Text_IO.Standard_Error,
                                        "message identified as query."));
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
                                            if cSet (1).Family /= Family_Inet
                                            then
                                                flags := flags + 1;
                                            end if;
                                            Unsigned_16'Write (str, flags);
                                            Unsigned_16'Write (str, 1);
                                            Child_Set'Write (str, cSet);
                                        when 2 =>
                                            if cSet (1).Family /= Family_Inet
                                            then
                                                flags := flags + 1;
                                            end if;
                                            if cSet (2).Family /= Family_Inet
                                            then
                                                flags := flags + 2;
                                            end if;
                                            Unsigned_16'Write (str, flags);
                                            Unsigned_16'Write (str, 2);
                                            Child_Set'Write (str, cSet);
                                    end case;
                                end;
                            when Character'Pos ('j') =>
                                pragma Debug
                                   (Text_IO.Put_Line
                                       (Text_IO.Standard_Error,
                                        "message identified as a join request."));
                                declare
                                    number : Child_Number;
                                    cSet   : Child_Set;
                                    buf :
                                       constant Memory_Stream.Stream_Access :=
                                       new Memory_Stream.Memory_Buffer_Stream
                                          (MaxMessageLength);
                                    Child_Address : Sock_Addr_Type := talker;
                                begin
                                    Children.Get_Children (number, cSet);
                                    Memory_Stream.Write
                                       (Memory_Stream.Memory_Buffer_Stream
                                           (buf.all),
                                        msg (msg'First + 1 .. msg'Last));
                                    Port_Type'Read (buf, Child_Address.Port);
                                    if number = 2 then
                                        pragma Debug
                                           (Text_IO.Put_Line
                                               (Text_IO.Standard_Error,
                                                "Join request denied"));
                                        String'Write (str, "err");
                                    else
                                        pragma Debug
                                           (Text_IO.Put_Line
                                               (Text_IO.Standard_Error,
                                                "Join request accepted"));
                                        Children.Add_Child
                                           (child => Child_Address);
                                        String'Write (str, "ok");
                                        Unsigned_16'Write
                                           (str, Message_Number);
                                    end if;
                                    Memory_Stream.Free (buf);
                                exception
                                    when E : Constraint_Error =>
                                        String'Write (str, "err");
                                        Memory_Stream.Free (buf);
                                        Text_IO.Put_Line
                                           (Text_IO.Standard_Error,
                                            "Error when trying to add child: " &
                                            Ada.Exceptions.Exception_Message
                                               (E));
                                end;
                            when others =>
                                pragma Debug
                                   (Text_IO.Put_Line
                                       (Text_IO.Standard_Error,
                                        "message is a genaric message to be passed on."));
                                Message_Number := Message_Number + 1;
                        end case;
                    end;
                    declare
                        outbound :
                           Stream_Element_Array (1 .. MaxMessageLength);
                    begin
                        pragma Debug
                           (Text_IO.Put_Line
                               (Text_IO.Standard_Error,
                                "Sending reply to sender of message."));
                        Memory_Stream.Read
                           (Memory_Stream.Memory_Buffer_Stream (str.all),
                            outbound, msgLen);
                        pragma Debug
                           (Text_IO.Put_Line
                               (Text_IO.Standard_Error,
                                "msgLen is " & msgLen'Image & "."));
                        if msgLen = 0 then
                            outbound (1) := 0;
                            msgLen       := 1;
                        end if;
                        Send_Socket
                           (socket, outbound (1 .. msgLen), msgLen, talker);
                    end;
                end if;
            or
                terminate;
            end select;
        end loop;
    exception
        when E : others =>
            Text_IO.Put_Line
               (Text_IO.Standard_Error,
                "Mesenger thread error:" &
                Ada.Exceptions.Exception_Message (E));
    end Request_Handler;

    task body Server is
        listeningSocket  : Socket_Type;
        listeningAddress : Sock_Addr_Type (Family);
    begin
        pragma Debug
           (Text_IO.Put_Line
               (Text_IO.Standard_Error, "Server thread starting..."));
        Create_Socket
           (listeningSocket, Family, Socket_Datagram,
            IP_Protocol_For_UDP_Level);
        pragma Debug
           (Text_IO.Put_Line
               (Text_IO.Standard_Error,
                "Socket created, with family=" & Family'Image & "."));
        listeningAddress.Addr :=
           (if Family = Family_Inet then Any_Inet_Addr else Any_Inet6_Addr);
        listeningAddress.Port := Port;
        Bind_Socket (listeningSocket, listeningAddress);
        pragma Debug
           (Text_IO.Put_Line
               (Text_IO.Standard_Error,
                "Socket bound to address " & Image (listeningAddress) &
                ". Starting listening loop."));
        loop
            declare
                talkingAddress : Sock_Addr_Type;
                message        : Stream_Element_Array (1 .. MaxMessageLength);
                messageLength  : Stream_Element_Offset;
            begin
                Receive_Socket
                   (listeningSocket, message, messageLength, talkingAddress);
                pragma Debug
                   (Text_IO.Put_Line
                       (Text_IO.Standard_Error,
                        "listening loop got something."));
                Request_Handler.new_Request
                   (sock    => listeningSocket, address => talkingAddress,
                    message => message, messageLength => messageLength);
            end;
        end loop;
    exception
        when E : Socket_Error =>
            Text_IO.Put_Line
               (Text_IO.Standard_Error,
                "Server thread error:" & Ada.Exceptions.Exception_Message (E));
    end Server;

    task Client_Thread is
        entry Try_New_Server (addr : Inet_Addr_Type; port : Port_Type);
    end Client_Thread;

    procedure Connect_To_Server (addr : Inet_Addr_Type; port : Port_Type) is
    begin
        pragma Debug
           (Text_IO.Put_Line
               (File => Text_IO.Standard_Error,
                Item =>
                   "Connect_To_Server(" & Image (Value => addr) & "," &
                   port'Image & ") called."));
        Client_Thread.Try_New_Server (addr, port);
    end Connect_To_Server;

    task body Client_Thread is
        package Queue_interface is new Ada.Containers
           .Synchronized_Queue_Interfaces
           (Element_Type => Sock_Addr_Type);
        package Address_Queues is new Ada.Containers
           .Unbounded_Synchronized_Queues
           (Queue_interface);
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
            Join_String : Stream_Element_Array (1 .. MaxMessageLength);
            -- A memory_stream used to construct the previous variable (needs to be deallocated)
            buf : constant Memory_Stream.Stream_Access :=
               new Memory_Stream.Memory_Buffer_Stream (MaxMessageLength);
            -- The length of the join request. Gets set when the join request is sent.
            Join_String_Length : Stream_Element_Offset;
            -- A Selector for listening to multiple sockets at once.
            Selector : Selector_Type;

            Max_Retries : constant Integer := 5;
            type Connection_Retry_Count is range 1 .. Max_Retries;

            -- The number of times I have tried to connect to each of the sockets in each set.
            Connections_By_Retry_Count :
               array (Connection_Retry_Count) of Socket_Set_Type;

            --------------------------------------------------------
            -- Save a Stream_Element_Array to a debuging log file --
            --------------------------------------------------------
            procedure Debug_Stream_Element_Array
               (arr : in Stream_Element_Array)
            is
                F : Stream_IO.File_Type;
                S : Stream_IO.Stream_Access;
            begin
                pragma Debug
                   (Text_IO.Put_Line
                       (Text_IO.Standard_Error,
                        "Debug_Stream_Element_Array( <Stream_Element_Array> ) called."));
                Stream_IO.Create (F, Stream_IO.Append_File, "/tmp/debug.log");
                S := Stream_IO.Stream (F);
                Stream_Element_Array'Write (S, arr);
            end Debug_Stream_Element_Array;

            use Ada.Containers;
        begin
            pragma Debug
               (Text_IO.Put_Line
                   (Text_IO.Standard_Error,
                    "Server_Selector thread starting..."));
            -- prepare the join request string
            String'Write (buf, "j");
            Port_Type'Write (buf, Port);
            Memory_Stream.Read
               (Memory_Stream.Memory_Buffer_Stream (buf.all), Join_String,
                Join_String_Length);
            Memory_Stream.Free (buf);
            -- save the join request string to a debugging log file (currently does not work as intended)
            pragma Debug (Debug_Stream_Element_Array (Join_String));

            Create_Selector (Selector);

            for I in Connections_By_Retry_Count'Range loop
                Sock.Empty (Connections_By_Retry_Count (I));
            end loop;

            pragma Debug
               (Text_IO.Put_Line
                   (Text_IO.Standard_Error,
                    "Starting server connection loop..."));
            loop
                Reconnect_Loop :
                loop
                    pragma Debug
                       (Text_IO.Put_Line
                           (Text_IO.Standard_Error,
                            "Server connection loop checking for new servers on the queue..."));
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
                               (Server_Socket, Server_Address.Family,
                                Socket_Datagram);
                            Send_Socket
                               (Server_Socket, Query_String, Transmitted_Data,
                                Server_Address);
                            Set (Connections_By_Retry_Count (1),
                                Server_Socket);
                        end;
                    end loop;
                    pragma Debug
                       (Text_IO.Put_Line
                           (Text_IO.Standard_Error,
                            "preparing selector to see if there are any answers from the servers"));
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
                        Check_Selector
                           (Selector, R_Set, W_Set_Dummy, Status, 0.3);
                        for I in Connections_By_Retry_Count'Range loop
                            Copy (Connections_By_Retry_Count (I), Tmp_Copy);
                            loop
                                Get (Tmp_Copy, Socket_To_Read);
                                exit when Socket_To_Read = No_Socket;
                                if not Is_Set (R_Set, Socket_To_Read) then
                                    Clear
                                       (Connections_By_Retry_Count (I),
                                        Socket_To_Read);
                                    if I /= Connections_By_Retry_Count'Last
                                    then
                                        Set (R_Set, Socket_To_Read);
                                    else
                                        pragma Debug
                                           (Text_IO.Put_Line
                                               (Text_IO.Standard_Error,
                                                "Connection timed out on socket " &
                                                Image (Socket_To_Read)));
                                    end if;
                                end if;
                            end loop;
                        end loop;
                        pragma Debug
                           (Text_IO.Put_Line
                               (Text_IO.Standard_Error,
                                "Selector returned status " & Status'Image));
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
                                        Addr    : Sock_Addr_Type;
                                        message :
                                           Stream_Element_Array
                                              (1 .. MaxMessageLength);
                                        message_length : Stream_Element_Offset;
                                    begin
                                        Receive_Socket
                                           (Socket_To_Read, message,
                                            message_length, Addr,
                                            Wait_For_A_Full_Reception);
                                        pragma Debug
                                           (Text_IO.Put
                                               (Text_IO.Standard_Error,
                                                "Connected to server at " &
                                                Image (Addr) & "?"));
                                        exit Reconnect_Loop when message_length >=
                                           2 and
                                           message (1) =
                                              Character'Pos ('o') and
                                           message (2) = Character'Pos ('k');
                                        pragma Debug
                                           (Text_IO.Put_Line
                                               (Text_IO.Standard_Error,
                                                "No."));
                                        if (message_length >= 3 and
                                            message (1) =
                                               Character'Pos ('e') and
                                            message (2) =
                                               Character'Pos ('r') and
                                            message (3) = Character'Pos ('r'))
                                        then
                                            pragma Debug
                                               (Text_IO.Put_Line
                                                   (Text_IO.Standard_Error,
                                                    "Our join request was denied."));
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
                                                    Shutdown_Socket
                                                       (Socket_To_Read);
                                                    exit;
                                                end if;
                                            end loop;
                                        else
                                            declare
                                                flags            : Unsigned_16;
                                                number           : Unsigned_16;
                                                Servers_Children : Child_Set;
                                                buf :
                                                   constant Memory_Stream
                                                      .Stream_Access :=
                                                   new Memory_Stream
                                                      .Memory_Buffer_Stream
                                                      (MaxMessageLength);
                                                Transmitted_Data :
                                                   Stream_Element_Offset;
                                            begin
                                                Memory_Stream.Write
                                                   (Memory_Stream
                                                       .Memory_Buffer_Stream
                                                       (buf.all),
                                                    message);
                                                Unsigned_16'Read (buf, flags);
                                                Unsigned_16'Read (buf, number);
                                                Child_Set'Read
                                                   (buf, Servers_Children);
                                                case number is
                                                    when 0 | 1 =>
                                                        Send_Socket
                                                           (Socket_To_Read,
                                                            Join_String
                                                               (1 ..
                                                                      Join_String_Length),
                                                            Transmitted_Data,
                                                            Addr);
                                                    when 2 =>
                                                        Connect_To_Server
                                                           (Servers_Children
                                                               (1)
                                                               .Addr,
                                                            Servers_Children
                                                               (1)
                                                               .Port);
                                                        Connect_To_Server
                                                           (Servers_Children
                                                               (2)
                                                               .Addr,
                                                            Servers_Children
                                                               (2)
                                                               .Port);
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
                                                                Close_Socket
                                                                   (Socket_To_Read);
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
                pragma Debug
                   (Text_IO.Put_Line (Text_IO.Standard_Error, "Yes."));
                select
                    accept Reconnect;
                or
                    terminate;
                end select;
            end loop;
        exception
            when E : Socket_Error =>
                Text_IO.Put_Line
                   (Text_IO.Standard_Error,
                    "Server connection thread error:" &
                    Ada.Exceptions.Exception_Message (E));
        end Server_Selector;

    begin
        loop
            select
                accept Try_New_Server (addr : Inet_Addr_Type; port : Port_Type)
                do
                    if addr.Family = Family_Inet then
                        Queue.Enqueue
                           (Sock_Addr_Type'
                               (Family => Family_Inet, Addr => addr,
                                Port   => port));
                    else
                        Queue.Enqueue
                           (Sock_Addr_Type'
                               (Family => Family_Inet6, Addr => addr,
                                Port   => port));
                    end if;
                end Try_New_Server;
            or
                terminate;
            end select;
        end loop;
    exception
        when E : Socket_Error =>
            Text_IO.Put_Line
               (Text_IO.Standard_Error,
                "Client thread error:" & Ada.Exceptions.Exception_Message (E));
    end Client_Thread;
end Network_Tree;
