--                              -*- Mode: Ada -*-
--  Filename        : ether.ads
--  Description     : Root of the Ether SCGI library.
--  Author          : Luke A. Guest
--  Created On      : Mon Mar 28 11:43:06 2011
with Ada.Strings.Unbounded;

package Ether is
   --  Initialise the library with the max umber of bytes a request can have as content-type
   --  and the library's upload directory, i.e. where to save files to.
   procedure Initialise
     (Bytes      : in Positive;
      Upload_Dir : in String;
      Temp_Dir   : in String);

   --  Has the library been initialisied?
   function Is_Initialised return Boolean;

   --  Return the max number of bytes a request can have as it's content-type.
   function Max_Request_Size return Positive;

   --  Get the directory to where the uploaded files are saved to.
   function Upload_Dir return String;

   --  Get the directory where temporary files go.
   function Temp_Dir return String;
private
   package US renames Ada.Strings.Unbounded;

   Initialised       : Boolean             := False;
   Max_Request_Bytes : Positive            := 2000;
   Uploads           : US.Unbounded_String := US.Null_Unbounded_String;
   Temps             : US.Unbounded_String := US.Null_Unbounded_String;
end Ether;
