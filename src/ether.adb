--                              -*- Mode: Ada -*-
--  Filename        : ether.adb
--  Description     : Body root of the Ether SCGI library.
--  Author          : Luke A. Guest
--  Created On      : Thu May  3 15:32:35 2012
with Ada.Directories;

package body Ether is
   procedure Initialise
     (Bytes      : in Positive;
      Upload_Dir : in String;
      Temp_Dir   : in String) is

      package Dirs renames Ada.Directories;
   begin
      Max_Request_Bytes := Bytes;

      if Dirs.Exists (Upload_Dir) then
         Uploads := US.To_Unbounded_String (Upload_Dir);

         if Dirs.Exists (Temp_Dir) then
            Temps       := US.To_Unbounded_String (Temp_Dir);
            Initialised := True;
         else
            raise Initialisation_Error with
              "[Ether] Temporary directory """ & Temp_Dir & """ does not exist!";
         end if;
      else
         raise Initialisation_Error with
           "[Ether] Upload directory """ & Upload_Dir & """ does not exist!";
      end if;
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
      return US.To_String (Uploads);
   end Upload_Dir;

   function Temp_Dir return String is
   begin
      return US.To_String (Temps);
   end Temp_Dir;
end Ether;
