with Text_IO;

package body Network_Children is
   protected body Children is
      entry Add_Child (Child : Sock_Addr_Type) when not Locked is
      begin
         pragma Debug
           (Text_IO.Put_Line
              (Text_IO.Standard_Error,
               "Add_Child(" & Image (Child) & ") called."));
         Locked       := True;
         Number       := Number + 1;
         Set (Number) := Child;
         Locked       := False;
      end Add_Child;

      entry Get_Children (N : out Child_Number; C : out Child_Set)
        when not Locked is
      begin
         Locked := True;
         N      := Number;
         if Number < 2 then
            Set (2) := No_Sock_Addr;
         end if;
         if Number < 1 then
            Set (1) := No_Sock_Addr;
         end if;
         C := Set;
         pragma Debug
           (Text_IO.Put_Line
              (Text_IO.Standard_Error,
               "Get_Children(" & N'Image & " (out), [" & Image (C (1)) &
                 ", " & Image (C (2)) & "] (out)) called."));
         Locked := False;
      end Get_Children;

      entry Remove_Child (Child : Sock_Addr_Type; Status : out Child_Status)
        when not Locked is
      begin
         Locked := True;
         if Number = 0 then
            Status := Empty;
            return;
         end if;
         Status := NotExist;
         for I in Set'Range loop
            if Status = Success then
               if I /= Set'Last then
                  Set (I) := Set (I + 1);
               end if;
            else

               if Child = Set (I) then
                  Set (I) := No_Sock_Addr;
                  Status  := Success;
               end if;
            end if;

         end loop;
         pragma Debug
           (Text_IO.Put_Line
              (Text_IO.Standard_Error,
               "Remove_Child(" & Image (Child) & ", " & Status'Image &
                 ") called"));
         Locked := False;
      end Remove_Child;
   end Children;
end Network_Children;
