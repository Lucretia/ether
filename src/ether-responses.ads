--                              -*- Mode: Ada -*-
--  Filename        : ether-response.ads
--  Description     : Interface for the response objects.
--  Author          : Luke A. Guest
--  Created On      : Sun Jul  4 19:22:48 2010

--  with Ada.Strings.Unbounded;
with GNAT.Sockets;
--  with HTTP;
--  with MIME;
with AWS.Messages;

package Ether.Responses is
--   type Response is private;

--     Status        : constant String := "Status: ";
--     Status_OK     : constant String := "200 OK";
--     Status_400    : constant String := "400 Bad Request";
--     Status_404    : constant String := "404 Not Found";
--     Content_Type  : constant String := "Content-Type: ";
--     MIME_HTML     : constant String := "text/html";
--     MIME_Text     : constant String := "text/plain";

   procedure Send
     (--Object  : in out Response;
      Output    : in GNAT.Sockets.Stream_Access;
      Status    : in AWS.Messages.Status_Code; --  HTTP.Status_Codes;
      Mime_Type : in String; --  MIME.MIME_Type;
      Content   : in String); --  TODO: This should be a Unicode string.
private

   -- TODO: This will be expanded!
--     type Response is
--        record
--           Status  : Status_Type;
--           Mime    : Mime_Type;
--           Content : String;
--        end record;
end Ether.Responses;
