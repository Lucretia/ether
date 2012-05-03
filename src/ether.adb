--                              -*- Mode: Ada -*-
--  Filename        : ether.adb
--  Description     : Body root of the Ether SCGI library.
--  Author          : Luke A. Guest
--  Created On      : Thu May  3 15:32:35 2012
package body Ether is
   procedure Initialise (Bytes : in Positive; Upload_Dir : in String) is
   begin
      Max_Request_Bytes := Bytes;
      Dir               := US.To_Unbounded_String (Upload_Dir);
      Initialised       := True;
   end Initialise;
   
   function Is_Initialised return Boolean is
   begin
      return Initialised;
   end Is_Initialised;
   
   function Max_Request_Size return Positive is
   begin
      return Max_Request_Bytes;
   end Max_Request_Size;
   
   function Upload_Dir return String is
   begin
      return US.To_String (Dir);
   end Upload_Dir;
end Ether;
