--                              -*- Mode: Ada -*-
--  Filename        : ether-requests.adb
--  Description     : Body to the request objects.
--  Author          : Luke A. Guest
--  Created On      : Sun Jul  4 19:11:27 2010
with Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;
with AWS.MIME;

package body Ether.Requests is
   procedure Receive
     (Object : in out Request;
      Input  : in GNAT.Sockets.Stream_Access) is

      package L1 renames Ada.Characters.Latin_1;
      package AS renames Ada.Streams;

      use type AS.Stream_Element;
      use type AS.Stream_Element_Offset;
      use type US.Unbounded_String;

      -------------------------------------------------------------------------
      --  Read in the environment variables from the web server as a NetString.
      procedure Read_Environment
        (Object : in out Request;
         Input  : in GNAT.Sockets.Stream_Access) is

         Char   : AS.Stream_Element;
         Buffer : US.Unbounded_String        := US.Null_Unbounded_String;
         Colon  : constant AS.Stream_Element := Character'Pos(':');
         Comma  : constant AS.Stream_Element := Character'Pos(',');
         Nul    : constant AS.Stream_Element := Character'Pos(L1.Nul);
         Length : AS.Stream_Element_Offset   := AS.Stream_Element_Offset'First;
      begin
         Read_Header_Length : loop
            Char := AS.Stream_Element'Input(Input);

            --  Ada.Text_IO.Put_Line("[" & Character'Val(Char) & "]");

            exit when Char = Colon;

            US.Append(Buffer, Character'Val(Char));
         end loop Read_Header_Length;

         Length := AS.Stream_Element_Offset'Value(US.To_String(Buffer));

         --  Ada.Text_IO.Put_Line("Buffer: " & US.To_String(Buffer) & " - Length: " & Ada.Streams.Stream_Element_Offset'Image(Length));

         Buffer := US.Null_Unbounded_String;

         --  Read in the rest of the NetString.
         Read_Net_String : declare
            Actual_Read : AS.Stream_Element_Offset;
            Total_Read  : AS.Stream_Element_Offset := 1;
            Message     : AS.Stream_Element_Array(1 .. Length);
         begin
            --  Just in case it won't read the header in 1 chunk.
            while Total_Read < Length loop
               AS.Read(Input.all, Message(Total_Read .. Length), Actual_Read);

               Total_Read := Total_Read + Actual_Read;
            end loop;

            --  Make sure we end the netstring with a comma.
            Char := AS.Stream_Element'Input(Input);

            if Char /= Comma then
               raise Request_Error
                 with "[Ether] Missing comma from NetString in request!";
            end if;

            Decode_Net_String : declare
               Reading_Key : Boolean             := True;
               Key         : US.Unbounded_String := US.Null_Unbounded_String;
            begin
               for I in 1 .. Length loop
                  if Message(I) = Nul then
                     if Reading_Key then
                        Key         := Buffer;
                        Buffer      := US.Null_Unbounded_String;
                        Reading_Key := False;
                     else
                        --  TODO: When this works, get rid of this header from
                        --  the enumeration.
                        --  This is because the cookies are stored in a
                        --  separate map therefore this key is not required.
                        if Key = "HTTP_COOKIE" then
                           Put_Line(US.To_String(Key) & " : " & US.To_String(Buffer));

                           Cookie_Map.Insert
                             (Container => Object.Cookies,
                              Key       => US.To_String(Key),
                              New_Item  => US.To_String(Buffer));
                        else
                           Put_Line(US.To_String(Key) & " : " & US.To_String(Buffer));

                           Variable_Map.Insert
                             (Container => Object.Environment,
                              Key       => US.To_String(Key),
                              New_Item  => US.To_String(Buffer));
                        end if;

                        Key         := US.Null_Unbounded_String;
                        Buffer      := US.Null_Unbounded_String;
                        Reading_Key := True;
                     end if;
                  else
                     --  Ada.Text_IO.Put(Character'Val(Message(I)));

                     US.Append(Buffer, Character'Val(Message(I)));
                  end if;
               end loop;
            end Decode_Net_String;
         end Read_Net_String;
      end Read_Environment;


      -------------------------------------------------------------------------
      --  This is the data that follows the NetString, if there is any; usually
      --  form data.
      function Read_Content
        (Input  : GNAT.Sockets.Stream_Access;
         Length : Integer) return US.Unbounded_String is

         L           : AS.Stream_Element_Offset := AS.Stream_Element_Offset(Length);
         Actual_Read : AS.Stream_Element_Offset;
         Total_Read  : AS.Stream_Element_Offset := 1;
         Message     : AS.Stream_Element_Array(1 .. L);
         Result      : String(1 .. Length);
      begin
         --  Just in case it won't read the content in 1 chunk.
         while Total_Read < L loop
            AS.Read(Input.all, Message(Total_Read .. L), Actual_Read);

            Total_Read := Total_Read + Actual_Read;
         end loop;

         -- Convert the stream elements into characters that we can display.
         for Count in Message'Range loop
            Result(Integer(Count)) := Character'Val(Message(Count));
         end loop;

         return US.To_Unbounded_String(Result);
      end Read_Content;

      L : Integer := 0;
   begin
      Read_Environment(Object, Input);

      --  TODO: Read in any form data and store them in the request
      --  object as well. Does this mean we don't need the Read_Content?
      --  Read_Form(Object.Form, Input);

      --  TODO: Form data can contain Unicode, how can this be handled?
      --  i.e. हिन्दी is sent back as %26%232361%3B%26%232367%3B%26%232344%3B%26%232381%3B%26%232342%3B%26%232368%3B

      --  TODO: Check this outside of the request object, send back a bad request
      --  response if something is wrong?
      --
      --  TODO: Object.Content most likely won't be required in future!
      if Is_Valid(Object) then
         L := Content_Length(Object);

         if L = 0 then
            --  At this point, there could be data passed in QUERY_STRING.
            Object.Content := US.Null_Unbounded_String;
         else
            --  At this point, we know we have content after the headers. We
            --  just don't know what type it is.
            if Value(Object, Content_Type) = AWS.MIME.Application_Form_Data then
               --  URIs.Decode
               --    (Request => Value(Object, Request_URI),
               --     Path    =>,
               --     Params  => Form);
               null;
            elsif Value(Object, Content_Type) = AWS.MIME.Multipart_Form_Data then
               null;
            --  elsif Value(Object, Content_Type) = MIME.Get_Mime(MIME.Multipart_Digest) then
            --     null;
            --  elsif Value(Object, Content_Type) = MIME.Get_Mime(MIME.Multipart_Mixed) then
            else
               null;
            end if;

            Object.Content := Read_Content(Input, L);
         end if;
      else
         raise Request_Error
           with "[Ether] Non-SCGI request received.";
      end if;
   end Receive;


   procedure Clean(Object : in out Request) is
   begin
      Variable_Map.Clear(Object.Environment);

      Object.Content := US.Null_Unbounded_String;
   end Clean;


   function Is_Valid(Object : Request) return Boolean is
      SCGI_Value : String := Value(Object, SCGI);
   begin
      --  According to the spec, this should always be "1"
      if SCGI_Value = "" or SCGI_Value /= "1" then
         return False;
      end if;

      return True;
   end Is_Valid;


   function Value(Object : Request; Item : Header) return String is
   begin
      return Value(Object, Header'Image(Item));
   end Value;


   function Value(Object : Request; Item : String) return String is
   begin
      if Variable_Map.Contains(Object.Environment, Item) then
         return Variable_Map.Element(Object.Environment, Item);
      end if;

      return "";
   end Value;


   function Cookie(Object : Request; Item : String) return String is
   begin
      if Cookie_Map.Contains(Object.Cookies, Item) then
         return Cookie_Map.Element(Object.Cookies, Item);
      end if;

      return "";
   end Cookie;


   function Form(Object : Request; Item : String) return Unicode.CES.Byte_Sequence is --Unicode.UTF8_String is
   begin
      if Form_Map.Contains(Object.Form, Item) then
         return Form_Map.Element(Object.Form, Item);
      end if;

      return "";
   end Form;


   function Content(Object : Request) return
     Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Object.Content;
   end Content;


   function Content_Length(Object : Request) return Natural is
      L : String := Value(Object, Content_Length);
   begin
      --  If we get no length, there's a problem!
      if L = "" then
         raise Request_Error;
      end if;

      return Natural'Value(L);
   end Content_Length;


   function Hash(Key : Header) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(Header'Image(Key));
   end Hash;
end Ether.Requests;
