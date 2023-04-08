with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Sockets;

with Network_Tree;

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
        case GNAT.Command_Line.Getopt ("h p: 6 -help -port=") is
            when 'h' =>
                Print_Help := True;
            when 'p' =>
                Port :=
                   GNAT.Sockets.Port_Type'Value (GNAT.Command_Line.Parameter);
            when '6' =>
                Family := GNAT.Sockets.Family_Inet6;
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
        null; -- TODO : print help mesage
    else
        -- Parse servers to try listening to, in the form of ip addresses with ports.
        -- accepted formats:
        -- ipv4, ipv6, [ipv4], [ipv6], ipv4:port, [ipv6]:port
        declare
            package my_tree is new Network_Tree
               (family => Family, port => Port);
            End_Of_Arguments : Boolean := False;
            Format_Error : exception;
        begin
            while not End_Of_Arguments loop
                declare
                    -- get next command line argument
                    Argument : constant String :=
                       GNAT.Command_Line.Get_Argument
                          (End_Of_Arguments => End_Of_Arguments);
                    use Ada.Strings.Fixed;
                begin
                    pragma Debug
                       (Put_Line
                           (Standard_Error, "Parsing '" & Argument & "'."));
                    if Argument'Length /= 0 then
                        if GNAT.Sockets.Is_IPv4_Address (Argument)
                           or else GNAT.Sockets.Is_IPv6_Address (Argument)
                        then
                            -- note that Is_IPv4 accepts some addresses that Inet_Addr does not.
                            -- In these cases, Inet_Addr raises a Socket_Error
                            my_tree.Connect_To_Server
                               (GNAT.Sockets.Inet_Addr (Argument), Port);
                        elsif Argument (1) = '[' then
                            declare
                        -- If there is not ']' in Argument, Close_Bracket will be 0
                        -- in which case, I will soon try to access
                        -- Argument (2 .. -1) which will raise a Constraint_Error.
                                Close_Bracket : constant Natural :=
                                   Index (Argument, "]");
                            begin
                                if GNAT.Sockets.Is_IPv6_Address
                                      (Name =>
                                          Argument (2 .. Close_Bracket - 1))
                                   or else GNAT.Sockets.Is_IPv4_Address
                                      (Name =>
                                          Argument (2 .. Close_Bracket - 1))
                                then
                                    if Close_Bracket = Argument'Last then
                                        -- looks like it's an [ipv6] or [ipv4]. If not, Socket_Error.
                                        my_tree.Connect_To_Server
                                           (GNAT.Sockets.Inet_Addr
                                               (Argument
                                                   (2 .. Close_Bracket - 1)),
                                            Port);
                                    elsif Argument (Close_Bracket + 1) = ':'
                                    then
                                        -- We know it's [ipv*]:*.
                                        -- We try to parse the port, and let Ada do it's thing.
                                        my_tree.Connect_To_Server
                                           (GNAT.Sockets.Inet_Addr
                                               (Argument
                                                   (2 .. Close_Bracket - 1)),
                                            GNAT.Sockets.Port_Type'Value
                                               (Argument
                                                   (Close_Bracket + 2 ..
                                                          Argument'Last)));
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
                            if Ada.Strings.Fixed.Count (Argument, ":") > 1
                            then -- too many colons
                                raise Format_Error
                                   with "'" &
                                   Argument
                                      (Index (Argument, ":") ..
                                             Argument'Last) &
                                   "' is not a Natural number (for IPv6, the expected format is along the lines of [::1]:1234)";
                            elsif Ada.Strings.Fixed.Count (Argument, ":") /= 1
                            then --not enough colons
                                raise Format_Error
                                   with "Argument '" & Argument &
                                   "' was not recognised as an IP address (with an optional port).";
                            end if;
                            declare
                                Colon : constant Natural :=
                                   Index (Argument, ":");
                            begin
                                if GNAT.Sockets.Is_IPv4_Address
                                      (Argument (1 .. Colon - 1))
                                then
                                    my_tree.Connect_To_Server
                                       (GNAT.Sockets.Inet_Addr
                                           (Argument (1 .. Colon - 1)),
                                        GNAT.Sockets.Port_Type'Value
                                           (Argument
                                               (Colon + 1 .. Argument'Last)));
                                end if;
                            end;
                        end if;
                    end if;
                exception
                    when E : GNAT.Sockets.Socket_Error =>
                        Put_Line
                           (Standard_Error,
                            "Error parsing IP address in '" & Argument & "':");
                        Put_Line
                           (Standard_Error,
                            Ada.Exceptions.Exception_Message (E));
                    when E : Constraint_Error =>
                        Put_Line
                           (Standard_Error,
                            "Error parsing argument '" & Argument &
                            "' (no closing bracket?):");
                        Put_Line
                           (Standard_Error,
                            Ada.Exceptions.Exception_Message (E));
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
