--                              -*- Mode: Ada -*-
--  Filename        : ether-response.adb
--  Description     : Body for the response objects.
--  Author          : Luke A. Guest
--  Created On      : Sun Jul  4 19:22:48 2010

with Ada.Characters.Latin_1;
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Ether.Responses is
   package L1 renames Ada.Characters.Latin_1;
   package US renames Ada.Strings.Unbounded;
   
   use type US.Unbounded_String;

   CRLF : constant String := (L1.CR, L1.LF);

   procedure Send
     (Output    : in GNAT.Sockets.Stream_Access;
      Status    : in AWS.Messages.Status_Code;
      Mime_type : in String;
      Content   : in String;
      Char_Set  : in String := "UTF-8") is
      
      Actual_Char_Set : US.Unbounded_String := US.To_Unbounded_String("; charset=");
   begin
      if Char_Set = "" then
	 Actual_Char_Set := US.Null_Unbounded_String;
      else
	 Actual_Char_Set := Actual_Char_Set & Char_Set;
      end if;
      
      -- TODO: Check to make sure that there is no body for response codes:
      --       1xx, 204, 304, raise an exception if it does. See S.4.3 of
      --       RFC2616.

--      Object := Request'(Status => Status, Mime => Mime, Content => Content);
      String'Write
        (Output,
         "Status: " & AWS.Messages.Image(Status) & CRLF &
         "Content-Type: " & Mime_Type & US.To_String(Actual_Char_Set) & CRLF &
         CRLF &
         Content);

--        Put_Line("Status: " & Status_Map(Status) & " " & Status_Type'Image(Status) & CRLF &
--           "Content-Type: " & US.To_String(Mime_Map(Mime)) & CRLF &
--           CRLF &
--           Content);
   end Send;
end Ether.Responses;
