with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Interfaces.C; use Interfaces.C;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ether.Requests;
with Ether.Responses;
with AWS.Messages;
with AWS.MIME;

procedure Test is
   package L renames Ada.Characters.Latin_1;
--   use type MIME.Mime_Type;

   CRLF          : constant String := (L.CR, L.LF);

   Address       : Sock_Addr_Type;
   Server_Socket : Socket_Type;
   Socket        : Socket_Type;
   Channel       : GNAT.Sockets.Stream_Access;
   Request       : Ether.Requests.Request;

   User_Login    : String :=
     "<html>" & CRLF &
     "  <head>" & CRLF &
     "    <title>User Login</title>" & CRLF &
     "  </head>" & CRLF &
     "  <body>" & CRLF &
     "    <h1>User Login</h1>" & CRLF &
     "    <form method=""post"" accept-charset=""UTF-8"" enctype=""multipart/form-data"">" & CRLF &
     "      <p><label>Username: <input name=""username""></label></p>" & CRLF &
     "      <p><label>Password: <input name=""password""></label></p>" & CRLF &
     "      <p><label>Filename: <input type=""File"" name=""filename""></lable></p>" & CRLF &
     "      <p><button type=""submit"">Send</button></p>" & CRLF &
     "    </form>" & CRLF &
     "  </body>" & CRLF &
     "</html>" & CRLF;
begin
   -- for loop start
   Address.Addr := Addresses(Get_Host_By_Name(Host_Name), 1);
   Address.Port := 4000;

   Create_Socket(Server_Socket);

   Set_Socket_Option(Server_Socket, Socket_Level, (Reuse_Address, True));

   Bind_Socket(Server_Socket, Address);
   -- for loop end

   Listen_Socket(Server_Socket);

   loop
      Accept_Socket(Server_Socket, Socket, Address);

      Channel := Stream(Socket);

      delay 0.2;

      Ether.Requests.Receive(Object => Request, Input => Channel);

      --  Put_Line("Content Length       : " & Natural'Image(Ether.Requests.Length(Request)));
      --  Put_Line("Content_Type         : " & Ether.Requests.Value(Request, Ether.Requests.Content_Type));
      --  Put_Line("URL                  : " & Ether.Requests.Value(Request, Ether.Requests.HTTP_Host) & Ether.Requests.Value(Request, Ether.Requests.Request_URI));
      --  Put_Line("path_info            : " & Ether.Requests.Value(Request, Ether.Requests.Path_Info));
      --  Put_Line("HTTP_Cookie          : " & Ether.Requests.Value(Request, Ether.Requests.HTTP_Cookie));
      --  Put_Line("HTTP_Accept          : " & Ether.Requests.Value(Request, Ether.Requests.HTTP_Accept));
      --  Put_Line("HTTP_Accept_Language : " & Ether.Requests.Value(Request, Ether.Requests.HTTP_Accept_Language));
      --  Put_Line("HTTP_Accept_Encoding : " & Ether.Requests.Value(Request, Ether.Requests.HTTP_Accept_Encoding));
      --  Put_Line("HTTP_Accept_Charset  : " & Ether.Requests.Value(Request, Ether.Requests.HTTP_Accept_Charset));
      Put_Line("Content              : " & To_String(Ether.Requests.Content(Request)));

      --        Ether.Responses.Create
--          (Output  => Channel,
--           Status  => Ether.Responses.Ok,
--           Mime    => Ether.Responses.Text_Plain,
--           Content => "Ada Ether server running!" & CRLF &
--                      "Requested URL: " & Ether.Requests.Value(Request, Ether.Requests.HTTP_Host) & Ether.Requests.Value(Request, Ether.Requests.Request_URI));
      
      if Ether.Requests.Value(Request, Ether.Requests.Server_Name) = "newhome.co.uk" or
	Ether.Requests.Value(Request, Ether.Requests.Server_Name) = "newhome.com" or
	Ether.Requests.Value(Request, Ether.Requests.Server_Name) = "newhome.org" then
	 Put_Line("---- NEW HOME");
      else
	 Put_Line("---- OTHER");
      end if;
	 
      declare
         URI          : String := Ether.Requests.Value
           (Request,
            Ether.Requests.Request_URI);

         Content_Type : String := Ether.Requests.Value
              (Request,
               Ether.Requests.Content_Type);
      begin
         --  TODO: URI's should be grabbed from the database.
         if URI = "/" then
            if Content_Type = AWS.MIME.Application_Form_Data then
               Ether.Responses.Send
                 (Output    => Channel,
                  Status    => AWS.Messages.S200,
                  Mime_Type => AWS.MIME.Text_Plain,
                  Content   => "Hello");
            else
               Ether.Responses.Send
                 (Output    => Channel,
                  Status    => AWS.Messages.S200,
                  Mime_Type => AWS.MIME.Text_HTML,
                  Content   => User_Login);
            end if;
         else
            Ether.Responses.Send
              (Output    => Channel,
               Status    => AWS.Messages.S200,
               Mime_Type => AWS.MIME.Text_Plain,
               Content   => "हिन्दी समाचार");
         end if;
      exception
         --  TODO: Send actual error pages here.
         when Ether.Requests.Request_Error =>
            Ether.Responses.Send
              (Output    => Channel,
               Status    => AWS.Messages.S200,
               Mime_Type => AWS.MIME.Text_Plain,
               Content   => "Request Error");
         when others =>
            Ether.Responses.Send
              (Output    => Channel,
               Status    => AWS.Messages.S200,
               Mime_Type => AWS.MIME.Text_Plain,
               Content   => "Something else happened that shouldn't have");
      end;

      Ether.Requests.Clean(Request);

      Free(Channel);

      Close_Socket(Socket);

      Put_Line("Ok, finished request.");
      New_Line;
   end loop;

   Close_Socket(Server_Socket);
exception
   when others =>
      Close_Socket(Socket);
      Close_Socket(Server_Socket);
      Free(Channel);

      Put_Line("Some kind of error occurred!");

      raise;
end Test;
