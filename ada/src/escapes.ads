with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package Escapes is
        -- Foreground Colors
	Black_FG   : constant String := ESC & "[30m";
        Red_FG     : constant String := ESC & "[31m";
        Green_FG   : constant String := ESC & "[32m";
        Yellow_FG  : constant String := ESC & "[33m";
        Blue_FG    : constant String := ESC & "[34m";
        Magenta_FG : constant String := ESC & "[35m";
        Cyan_FG    : constant String := ESC & "[36m";
        White_FG   : constant String := ESC & "[37m";
        Default_FG : constant String := ESC & "[39m";

        -- Background Colors
	Black_BG   : constant String := ESC & "[40m";
        Red_BG     : constant String := ESC & "[41m";
        Green_BG   : constant String := ESC & "[42m";
        Yellow_BG  : constant String := ESC & "[43m";
        Blue_BG    : constant String := ESC & "[44m";
        Magenta_BG : constant String := ESC & "[45m";
        Cyan_BG    : constant String := ESC & "[46m";
        White_BG   : constant String := ESC & "[47m";
        Default_BG : constant String := ESC & "[49m";

        -- Text Styles
        Bold      : constant String := ESC & "[1m";
        Dim       : constant String := ESC & "[2m";
        Italic    : constant String := ESC & "[3m";
        Underline : constant String := ESC & "[4m";
        Blink     : constant String := ESC & "[5m";
        Revs      : constant String := ESC & "[7m";
        Hidden    : constant String := ESC & "[8m";
        Reset     : constant String := ESC & "[0m";

        -- Screen Control
        Clear_Screen : constant String := ESC & "[2J";
        Clear_Line   : constant String := ESC & "[2K";
end Escapes;
