with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Escapes;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with GNAT.OS_Lib;
package body Debug is
    procedure Handle_Event
       (Event : Debug_Event; Param_1 : String := ""; Param_2 : String := "";
        Param_3 : String := ""; Param_4 : String := ""; Param_5 : String := "";
        Source_Location  : String := GNAT.Source_Info.Source_Location;
        Enclosing_Entity : String := GNAT.Source_Info.Enclosing_Entity)
    is
        Now       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
        Timestamp : constant String := Ada.Calendar.Formatting.Image (Now,True);
    begin
        pragma Debug
           (Put_Line
               (Standard_Error,
                Image(Get_Event_Severity(Event)) & HT &
                GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id)'Image & HT &
                Timestamp & HT &
                Source_Location & HT &
                Enclosing_Entity & HT &
                Event'Image & HT &
                (case Event is
                    when Routine_Called =>
                       Param_1 & "(" &
                       (if Param_2 /= "" then "<" & Param_2 & ">" else "") &
                       (if Param_3 /= "" then ", <" & Param_3 & ">" else "") &
                       (if Param_4 /= "" then ", <" & Param_4 & ">" else "") &
                       (if Param_5 /= "" then ", <" & Param_5 & ">" else "") &
                       ") has been called.",
    -- Network_Utility_Functions

                                                                                                                                                                                                       when Parse_Addr_Port =>
                       "Parsing Adress and Port from string '" & Param_1 &
                       "' (of length " & Param_2 & ")",
    -- Network_Tree

                                                                                                                                                                                                       when Join_Request_Denied =>
                       "Join Request Denied",
                    when Join_Request_Accepted => "Join Request Accepted",
                    when Message_Type_Selector =>
                       "Message_Type is " & Param_1 & " (" & Param_2 & ")",
                    when Message_Type_Query => "Message identified as Query",
                    when Message_Type_Join =>
                       "Message identified as Join request",
                    when Message_Type_Fwd =>
                       "Message identified as a message to be passed on",
                    when ERR_Wrong_Order =>
                       Escapes.Red_FG & "Messages arriving in wrong order! " &
                       "Expected message #" & Param_1 & ", but got message #" &
                       Param_2 & Escapes.Default_FG,
                    when Message_Contents =>
                       "Message recieved (w/ metadata stripped) is '" &
                       Param_1 & "'.",
                    when ERR_Message_Type_Unknown =>
                       Escapes.Yellow_BG &
                       "Unrecognised message type. Full message is '" &
                       Param_1 & "'",
                    when Sending_Reply =>
                       "Sending a reply to the message's sender.",
                    when Sending_Length =>
                       "Reply's Length is " & Param_1 & ".",
                    when ERR_Exception =>
                       Escapes.Red_BG & Escapes.Black_FG & "Error in " &
                       Param_1 & " Thread: " & Param_2 & Escapes.Default_FG &
                       Escapes.Default_BG,
                    when Thread_Start => "Thread Starting: " & Param_1,

                    when Socket_Created =>
                       "Socket Created, with family=" & Param_1 & ".",
                    when Socket_Bound => "Socket bound to address " & Param_1,
                    when Loop_Start => "Starting " & Param_1 & " loop",
                    when Loop_Iterate => Param_1 & " Loop iterating.",
                    when Listen_Loop_Recvd =>
                       "Listening loop received something",
                    when Condition_Check => "Checking " & Param_1,
                    when Selector_Prepare => "Preparing Selector",
                    when Selector_Timeout =>
                       "Socket " & Param_1 & " timed out.",
                    when Selector_Return =>
                       "Selector returned status " & Param_1,

                    when Connection_Try =>
                       "Trying to conenct to server at " & Param_1,
                    when Connection_Fail =>
                       "Connecting to server at " & Param_1 & " failed.",
                    when Connection_Success =>
                       Escapes.Green_FG & "Connecting to server succeeded." &
                       Escapes.Default_FG,

                    when Var_Is =>
                        Param_1 & " is '" & Param_2 & "'"

        -- This (v) is an Error!
        -- when others =>
        --      Escapes.Red_BG & Escapes.Black_FG & "Well, now isn't this interesting. I don't know what a '"&Event'Image&"' is." &Escapes.Default_BG & Escapes.Default_FG
        )));
    end Handle_Event;

    function Get_Event_Severity (event : Debug_Event) return Severity is
    begin
        return
           (case event is when Routine_Called => TRACE,
               when Parse_Addr_Port => DEBUG,
               when Join_Request_Denied | Join_Request_Accepted => INFO,
               when Message_Type_Selector | Message_Type_Query
                  | Message_Type_Join | Message_Type_Fwd => DEBUG,
               when ERR_Wrong_Order => ERROR, -- Note: Might just be a Warning
               when Message_Contents => NOTICE,
               when ERR_Message_Type_Unknown => WARNING,
               when Sending_Reply => INFO, when Sending_Length => DEBUG,
               when ERR_Exception => CRITICAL, when Thread_Start => TRACE,
               when Socket_Created => TRACE, when Socket_Bound => DEBUG,
               when Loop_Start => DEBUG, when Loop_Iterate => TRACE,
               when Listen_Loop_Recvd => DEBUG, when Condition_Check => DEBUG,
               when Selector_Prepare => DEBUG, when Selector_Timeout => TRACE,
               when Selector_Return => TRACE, when Connection_Try => INFO,
               when Connection_Fail => INFO, when Connection_Success => NOTICE,
               when Var_Is => Trace,
               when others => FATAL);
    end Get_Event_Severity;

    function Image (s : Severity) return String is
        function Pad (S : String) return String is
        begin
            return HT & S & HT;
        end Pad;
    begin
        return
           "[" &
           (case s is
                when TRACE => Escapes.White_FG,
                when DEBUG => Escapes.Cyan_FG,
                when INFO => Escapes.Green_FG,
                when NOTICE => Escapes.Blue_FG,
                when WARNING => Escapes.Yellow_FG,
                when ERROR => Escapes.Red_FG,
                when CRITICAL => Escapes.Red_FG & Escapes.Bold,
                when FATAL => Escapes.White_FG & Escapes.Red_BG) &
           Pad(s'Image) &
           Escapes.Default_FG & Escapes.Default_BG & Escapes.Reset &
           "]";
    end Image;
end Debug;
