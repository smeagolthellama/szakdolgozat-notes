with GNAT.Source_info;

package Debug is
        type Debug_Event is (
		Routine_Called,

		-- Network_Utility_Functions
		Parse_Addr_Port,

		-- Network_Children
		--Add_Child_Called,
		--Get_Children_Called,
		--Remove_Child_Called,
		-- Replaced with Routine_Called

		-- Network_Tree
		Join_Request_Denied,
		Join_Request_Accepted,

		Message_Type_Selector,
		Message_Type_Query,
		Message_Type_Join,
		Message_Type_Fwd,
		ERR_Message_Type_Unknown,

		ERR_Wrong_Order,

		Message_Contents,

		Sending_Reply,
		Sending_Length,

		Thread_Start,
		Loop_Start,
                Loop_Iterate,
                Condition_Check,

		ERR_Exception,

		Socket_Created,
		Socket_Bound,
		Listen_Loop_Recvd,

                Selector_Prepare,
                Selector_Timeout,
                Selector_Return,

                Connection_Try,
                Connection_Fail,
                Connection_Success,

                Var_Is
		);

        procedure Handle_Event(
		event   : Debug_Event;
		Param_1 : String := "";
		Param_2 : String := "";
		Param_3 : String := "";
		Param_4 : String := "";
		Param_5 : String := "";
                Source_Location   : String := GNAT.Source_info.Source_Location;
                Enclosing_Entity : String := GNAT.Source_info.Enclosing_Entity
		);

        type Severity is (
            TRACE,    -- Very detailed information, useful for debugging but normally not needed
            DEBUG,    -- Detailed information, useful for debugging
            INFO,     -- General information about normal operation
            NOTICE,   -- Notable events that are part of normal operation
            WARNING,  -- Issues that might cause problems but don't prevent operation
            ERROR,    -- Serious issues that prevent some functionality
            CRITICAL, -- Very severe issues that might lead to data loss or crashes
            FATAL     -- The most severe issues that will cause the program to terminate
        );
        function Image (s : Severity) return String;
        function Get_Event_Severity (event : Debug_Event) return Severity;
end Debug;
