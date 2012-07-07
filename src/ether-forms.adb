--                              -*- Mode: Ada -*-
--  Filename        : ether-forms.adb
--  Description     : Abstraction around the form data returned from the SCGI client (HTTP server).
--  Author          : Luke A. Guest
--  Created On      : Tue May  1 18:04:04 2012
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with AWS.URL;

package body Ether.Forms is
   package US renames Ada.Strings.Unbounded;
   
   use type US.Unbounded_String;
   
   --  Split up each component from the QUERY_STRING and insert it into a Form object.
   procedure Decode_Query (Data : in String) is
      type States is (Is_Key, Is_Value);
      
      --  Length : Natural := Natural'First;
      --  Total  : Natural := Natural'First + 1;
      State  : States  := Is_Key;
      
      Key    : US.Unbounded_String;
      Value  : US.Unbounded_String;
   begin
      if Data = "" then
	 return;
      end if;
      
      for Index in Data'Range loop
	 if Data (Index) = '&' or Data (Index) = '=' then
	    if State = Is_Key then
	       State := Is_Value;
	       
	       Put_Line ("Key   : " & AWS.URL.Decode (US.To_String (Key)));
	       
	       Key := US.Null_Unbounded_String;
	    elsif State = Is_Value then
	       State := Is_Key;
	       
	       Put_Line ("Value : " & AWS.URL.Decode (US.To_String (Value)));
	       
	       Value := US.Null_Unbounded_String;
	    end if;
	 else
	    --  Append to the correct variable.
	    if State = Is_Key then
	       Key := Key & Data (Index);
	    elsif State = Is_Value then
	       Value := Value & Data (Index);
	    end if;
	 end if;
      end loop;
      
      --  At the end of the string, we need to also determine which was completed - should
      --  always be the value!
      if State = Is_Key then
	 raise Form_Error
	   with "[Ether] Forms always end with a value, not a key.";
      elsif State = Is_Value then
	 Put_Line ("Value : " & US.To_String (Value));
	       
	 Value := US.Null_Unbounded_String;
      end if;
   end Decode_Query;
   
   procedure Decode_Content (Data : access String) is
   begin
      --  Find the boundary string from CONTENT_TYPE.
      --  Each part starts with the boundary string, so for each part, read this in, 1 line.
      --  Next line should be "Content-Disposition: ..."
      --    Should contain "form-data;" followed by form field data, name and possible filename.
      --    If there is a filename, the next line will be "Content-Type: ..."
      --    Then follows field data or the file data.
      
      --  When searching for boundary, there are 2 dashes followed by the boundary marker.
      null;
   end Decode_Content;
end Ether.Forms;
