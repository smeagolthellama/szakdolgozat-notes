package body Network_Utility_Functions is
   function Image (Arr : Stream_Element_Array) return String is
      String_Index  : Integer                  := 1;
      Return_String : String (1 .. Arr'Length) := (others => ' ');
   begin
      for I in Arr'Range loop
         Return_String (String_Index) := Character'Val (Arr (I));
         String_Index                 := String_Index + 1;
      end loop;
      return Return_String;
   end Image;
end Network_Utility_Functions;
