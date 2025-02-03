with Debug; use Debug;

package body Network_Children is
   protected body Children is
      entry Add_Child (Child : Sock_Addr_Type) when not Locked is
      begin
	 Debug.Handle_Event(Routine_Called,"Add_Child", Image(Child));
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
	 Debug.Handle_Event(Routine_Called,"Get_Children",N'Image,Image(C(1)),Image(C(2)));
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
	 Debug.Handle_Event(Routine_Called,"Remove_Child",Image(Child),Status'Image);
         Locked := False;
      end Remove_Child;
   end Children;
end Network_Children;
