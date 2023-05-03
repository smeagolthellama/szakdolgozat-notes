with GNAT.Sockets;use GNAT.Sockets;

package Network_Children is
    type Child_Number is range 0 .. 2;
    subtype Child_Index is Child_Number range 1 .. Child_Number'Last;

    type Child_Set is array (Child_Index) of Sock_Addr_Type;

    type Child_Status is (Success, NotExist, Empty);
    protected Children is

        entry Add_Child (Child : Sock_Addr_Type);
        entry Get_Children (N : out Child_Number; C : out Child_Set);
        entry Remove_Child (Child : Sock_Addr_Type; Status : out Child_Status);
        pragma Unreferenced (Remove_Child);
    private
        Number : Child_Number := 0;
        Set    : Child_Set    := (others => No_Sock_Addr);
        Locked : Boolean      := False;
    end Children;
end Network_Children;
