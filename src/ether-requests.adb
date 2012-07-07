--                              -*- Mode: Ada -*-
--  Filename        : ether-requests.adb
--  Description     : Body to the request objects.
--  Author          : Luke A. Guest
--  Created On      : Sun Jul  4 19:11:27 2010
with Ada.Characters.Latin_1;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with AWS.MIME;
with Ether.Forms;

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

      
      function Find_Boundary (Object : in Request) return String is
	 Data     : String          := Value (Object, Content_Type);
	 Index    : Positive        := Positive'First;
	 Boundary : constant String := "boundary=";
      begin
	 --  find "boundary="
	 while Index /= Data'Length loop
	    exit when Data (Index) = ';';
	    
	    Index := Index + 1;
	 end loop;
	 
	 --  Have not found the ; and therefore not a multipart!
	 if Index = Data'Length then
	    raise SCGI_Error
	      with "[Ether] Content has no multipart data within, boundary not present.";
	 end if;
	 
	 --  Skip past the ';'
	 
	 Index := Index + 1;
	 
	 if Data (Index) = ' ' then
	    -- Skip past the space.
	    Index := Index + 1;
	    
	    -- We need to make sure we find the boundary marker.
	    if Data'Length - Index < Boundary'Length then
	       raise SCGI_Error
		 with "[Ether] Content has no multipart data within, ""boundary="" not found.";
	    end if;
	    
	    --  The remainder of the string is our boundary.
	    if Data (Index .. Index + Boundary'Length - 1) = Boundary then
	       return Data (Index + Boundary'Length .. Data'Length);
	    end if;
	 end if;
	 
	 --  Should never get here!
	 return "";
      end Find_Boundary;
      
      -------------------------------------------------------------------------
      --  This is the data that follows the NetString, if there is any; usually
      --  form data.
      --  function Read_Content
      --    (Input  : GNAT.Sockets.Stream_Access;
      --     Length : Integer) return US.Unbounded_String is

      --     L           : AS.Stream_Element_Offset := AS.Stream_Element_Offset(Length);
      --     Actual_Read : AS.Stream_Element_Offset;
      --     Total_Read  : AS.Stream_Element_Offset := 1;
      --     Message     : AS.Stream_Element_Array(1 .. L);
      --     Result      : String(1 .. Length);
      --  begin
      --     --  Just in case it won't read the content in 1 chunk.
      --     while Total_Read < L loop
      --        AS.Read(Input.all, Message(Total_Read .. L), Actual_Read);

      --        Total_Read := Total_Read + Actual_Read;
      --     end loop;

      --     -- Convert the stream elements into characters that we can display.
      --     for Count in Message'Range loop
      --        Result(Integer(Count)) := Character'Val(Message(Count));
      --     end loop;

      --     return US.To_Unbounded_String(Result);
      --  end Read_Content;
      
      type Stream_Element_Array_Access is access AS.Stream_Element_Array;
      
      procedure Read_Content
        (Input   : in     GNAT.Sockets.Stream_Access;
	 Content : in out Stream_Element_Array_Access) is

         L           : AS.Stream_Element_Offset := Content.all'Length;
         Actual_Read : AS.Stream_Element_Offset;
         Total_Read  : AS.Stream_Element_Offset := 1;
      begin
         --  Just in case it won't read the content in 1 chunk.
         while Total_Read < L loop
            AS.Read(Input.all, Content.all (Total_Read .. L), Actual_Read);

            Total_Read := Total_Read + Actual_Read;
         end loop;
      end Read_Content;
      
      procedure Free is new Ada.Unchecked_Deallocation
	(Object => AS.Stream_Element_Array,
	 Name   => Stream_Element_Array_Access);
      
      function To_Stream_Element_Array (Data : in String) return AS.Stream_Element_Array is
	 R : AS.Stream_Element_Array (1 .. Data'Length);
      begin
	 for Index in Data'Range loop
	    R (AS.Stream_Element_Offset (Index)) := AS.Stream_Element (Character'Pos (Data (Index)));
	 end loop;
	 
	 return R;
      end To_Stream_Element_Array;
	 
      function To_String (Data : in AS.Stream_Element_Array) return String is
	 Current : Positive := Positive'First;
	 R       : String (Current .. Data'Length);
      begin
	 for Index in Data'Range loop
	    R (Current) := Character'Val (Data (Index));
	    
	    Current := Current + 1;
	 end loop;
	 
	 return R;
      end To_String;
      
      function Matches (Data : in AS.Stream_Element_Array; Match : in String) return Boolean is
	 Test : String := 
	   To_String (Data (Data'First .. Data'First + AS.Stream_Element_Offset (Match'Length) - 1));
      begin
	 if Test = Match then
	    return True;
	 end if;
	 
	 return False;
      end Matches;
      
      --  Find the index of the next CRLF pair in a string.
      function Find_CRLF (Data : in String) return Positive is
	 Index : Positive := Data'First;
      begin
	 while Index /= Data'Last - 1 loop
	    if Data (Index) = L1.CR and Data (Index + 1) = L1.LF then
	       return Index;
	    end if;
	    
	    Index := Index + 1;
	 end loop;
	 
	 return Positive'Last;
      end Find_CRLF;
      
      --  Find Key within Data, then return whatever is in quotes, i.e. <key>="<value>"
      procedure Find_Value (Data : in String; Key : in String; Value : out US.Unbounded_String; Last_Index : out Positive) is
	 Index : Positive := Data'First;
	 First : Positive := Positive'First;
	 Last  : Positive := Positive'First;
      begin
	 --  Obviously a fail.
	 if Data = "" then
	    Last_Index := Positive'Last;
	    Value      := US.Null_Unbounded_String;
	    
	    return;
	 end if;
	 
	 while Index /= Data'Last - Key'Length loop
	    if Data (Index .. Index + Key'Length - 1) = Key then
	       --  The first character after the '="' marker.
	       First := Index + Key'Length + 2;
	       
	       --  Then find where the terminating quote is.
	       for Count in First .. Data'Last loop
		  if Data (Count) = L1.Quotation then
		     Last := Count - 1;
		     
		     exit;
		  end if;
	       end loop;
	       
	       --  Skip past the "
	       Last_Index := Last + 2;
	       
	       Value := US.To_Unbounded_String (Data (First .. Last));
	       
	       return;
	    else
	       Index := Index + 1;
	    end if;
	 end loop;
	 
	 Last_Index := Positive'Last;
	 Value      := US.Null_Unbounded_String;
      end Find_Value;
      
      --  Matches a string within a stream up until a CRLF pair.
      procedure Is_In
	(Data     : in     AS.Stream_Element_Array;
	 Match    : in     String;
	 Includes :    out Boolean;
	 At_Index :    out Positive) is
	 
	 Test     : String :=
	   To_String (Data (Data'First .. Data'First + AS.Stream_Element_Offset (Match'Length)));
	 CRLF_Pos : Positive := Find_CRLF (Test);
	 Index    : Positive := Positive'First;
      begin
	 --  Should never get here, unless the stream is just wrong.
	 if CRLF_Pos = Positive'Last then
	    raise SCGI_Error
	      with "[Ether] No CRLF pair found in current stream.";
	 end if;

	 while AS.Stream_Element_Offset (Index) /= Data'Last - AS.Stream_Element_Offset (Match'Length) loop
	    if Test (Index .. Match'Length) = Match then
	       At_Index := Index;
	       Includes := True;
	       
	       exit;
	    end if;
	 end loop;
	 
	 At_Index := Positive'Last;
	 Includes := False;
      end Is_In;
      
      Content_Disposition : constant String := "Content-Disposition";
      Form_Data           : constant String := "form-data";
      Name                : constant String := "name";
      Filename            : constant String := "filename";
      
      L          : Integer  := 0;
      Index      : Positive := Positive'First;
      Chunk_Size : Positive := Positive'First;
   begin
      Read_Environment(Object, Input);
      
      --  We read in the form data, if there is any...
      if Form_Data_Method_Is (Object) = Get then
	 --  from a urlencoded string.
	 Ether.Forms.Decode_Query (Value (Object, Query_String));
      else
	 --  from multipart/form-data content body.
	 if Is_Valid(Object) then
	    L := Content_Length(Object);

	    if L /= 0 then
	       --  Only have a boundary if this is multipart.
	       if Get_Content_Type (Object) = AWS.MIME.Multipart_Form_Data then
		  declare
		     Content      : Stream_Element_Array_Access := new AS.Stream_Element_Array
		       (1 .. AS.Stream_Element_Offset (L));
		     Boundary     : US.Unbounded_String         := US.To_Unbounded_String
		       (Find_Boundary (Object));
		     Current_Char : Character;
		     Next_Char    : Character;
		  begin
		     Put_Line ("boundary = '" & US.To_String (Boundary) & "'");
		     
		     --  Temporarily read in the rest of the stream into memory.
		     Read_Content (Input, Content);
		     
		     while Index /= Content'Length - 1 loop
			Current_Char := Character'Val (Content (AS.Stream_Element_Offset (Index)));
			Next_Char    := Character'Val (Content (AS.Stream_Element_Offset (Index + 1)));
			
			if Current_Char = L1.CR and Next_Char = L1.LF then
			   Put_Line ("-- CRLF -- ");
			   
			   Index := Index + 2;
			elsif Current_Char = L1.Hyphen and Next_Char = L1.Hyphen then
			   if Index = Content'Length - 3 then
			      --  This is the end marker of the stream which is "--CRLF".
			      Put_Line ("## End of content ##");
			      
			      Index := Index + 2;
			   elsif Content'Length - Index >= US.Length (Boundary) then
			      --  Found the boundary marker.
			      if To_String (Content (AS.Stream_Element_Offset (Index + 2) .. AS.Stream_Element_Offset (Index + 1 + US.Length (Boundary)))) = Boundary then
				 Put_Line ("## Boundary ##");
				 
				 Index := Index + 2 + US.Length (Boundary);
			      else
				 raise SCGI_Error
				   with "[Ether] Invalid boundary marker found within content.";
			      end if;
			   end if;
			elsif Matches (Content (AS.Stream_Element_Offset (Index) .. Content'Last), Content_Disposition) then
			   declare
			      --  Get the position of the next line start, this will be the blank line before the form data.
			      Next_Line : Positive := Find_CRLF
				(To_String (Content (AS.Stream_Element_Offset (Index) .. Content'Last)));
			      
			      Name                 : US.Unbounded_String;
			      Filename             : US.Unbounded_String;
			      Index_After_Name     : Positive;
			      Index_After_Filename : Positive;
			   begin
			      Find_Value
				(Data       => To_String (Content (AS.Stream_Element_Offset (Index) + Content_Disposition'Length .. AS.Stream_Element_Offset (Index + Next_Line))),
				 Key        => "name",
				 Value      => Name,
				 Last_Index => Index_After_Name);
			      
			      Find_Value
				(Data       => To_String (Content (AS.Stream_Element_Offset (Index + Index_After_Name) .. AS.Stream_Element_Offset (Index + Next_Line))),
				 Key        => "filename",
				 Value      => Filename,
				 Last_Index => Index_After_Filename);
				  
			      Put ("Form field '" & US.To_String (Name) & "' = '");
			      
			      --  Normal field data.
			      --  Skip the next CRLF which indicates a blank line..
			      declare
				 Data_First : AS.Stream_Element_Offset := AS.Stream_Element_Offset (Index + Next_Line + 2);
				 Data_Last  : AS.Stream_Element_Offset := AS.Stream_Element_Offset (Index + Find_CRLF (To_String (Content (Data_First .. Content'Last))));
			      begin
				 Put_Line (To_String (Content (Data_First .. Data_Last)) & "'");
			      end;
			      
			      if Filename /= "" then
				 --  Contents of a file.
				 Put_Line ("Form field filename '" & US.To_String (Filename) & "' = '");
				 
				 Index := Index + Next_Line;
			      end if;
			      
			      --  Skip to the next line.
			      Index := Index + Positive (Next_Line) + 1;
			   end;
			      
			   --  Find_CRLF after Content_Disposition, find "name" and/or "filename" getting
			   --  their value's.
			   
			   --  declare
			   --     First : AS.Stream_Element_Offset : AS.Strea_Element_Offset (Index + Content_Disposition'Length);
			   --  begin
			   --     if Matches (Content (First .. First + 1), ": ") then
			   --  	--  Skip ": "
			   --  	First := First + 2;
				
			   --  	if Matches (Content (First .. First + AS.Strea_Element_Offset (Form_Data'Length)), Form_Data) then
			   --  	   --  Skip "form-data"
			   --  	   First := First + AS.Strea_Element_Offset (Form_Data'Length);
				   
			   --  	   if Matches (Content (First .. First + 1), "; ") then
			   --  	      -- Skip "; "
			   --  	      First := First + 2;
				      
			   --  	      if Matches (Content (First .. First + AS.Strea_Element_Offset (Name'Length)), Name) then
			   --  		 Put_Line
			   --  		   ("Form field name: " &
			   --  		      Content (AS.Strea_Element_Offset (Form_Data'Length) .. ));
			   --  	      else
			   --  		 raise SCGI_Error
			   --  		   with "[Ether] Missing ""name"" after Content-Disposition in content.";
			   --  	      end if;
			   --  	   else
			   --  	      raise SCGI_Error
			   --  		with "[Ether] Missing ""; "" after form-data in Content-Disposition in content.";
			   --  	   end if;
			   --  	else
			   --  	   raise SCGI_Error
			   --  	     with "[Ether] Missing ""Form-Data"" after Content-Disposition in content.";
			   --  	end if;
			   --     else
			   --  	 raise SCGI_Error
			   --  	   with "[Ether] Missing "": "" after Content-Disposition in content.";
			   --     end if;
			   --  end;
			else
			   Put (Current_Char);
			   
			   Index := Index + 1;
			end if;
		     end loop;
	       
		     Free (Content);
		  end;
	       else
		  declare
		     Content : String (1 .. L);
		  begin
		     String'Read (Input, Content);
		     
		     Ether.Forms.Decode_Query (Content);
		  end;
	       end if;
	       
	       --  --  At this point, we know we have content after the headers. We
	       --  --  just don't know what type it is.
	       --  if Value(Object, Content_Type) = AWS.MIME.Application_Form_Data then
	       --     --  URIs.Decode
	       --     --    (Request => Value(Object, Request_URI),
	       --     --     Path    =>,
	       --     --     Params  => Form);
	       --     null;
	       --  elsif Value(Object, Content_Type) = AWS.MIME.Multipart_Form_Data then
	       --     null;
	       --  --  elsif Value(Object, Content_Type) = MIME.Get_Mime(MIME.Multipart_Digest) then
	       --  --     null;
	       --  --  elsif Value(Object, Content_Type) = MIME.Get_Mime(MIME.Multipart_Mixed) then
	       --  else
	       --     null;
	       --  end if;

	       --  Object.Content := Read_Content(Input, L);
	    end if;
	 else
	    raise Request_Error
	      with "[Ether] Non-SCGI request received.";
	 end if;
      end if;
   end Receive;


   procedure Clean(Object : in out Request) is
   begin
      Variable_Map.Clear(Object.Environment);

      --  Object.Content := US.Null_Unbounded_String;
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


   --  function Content(Object : Request) return String is
   --  begin
   --     return Object.Content.all;
   --  end Content;


   function Content_Length(Object : Request) return Natural is
      L : String := Value(Object, Content_Length);
   begin
      --  If we get no length, there's a problem!
      if L = "" then
         raise Request_Error
	   with "[Ether] No content length defined.";
      end if;

      return Natural'Value(L);
   end Content_Length;


   function Get_Content_Type (Object : Request) return String is
      C : String := Value (Object, Content_Type);
   begin
      if C = AWS.MIME.Application_Form_Data then
	 return AWS.MIME.Application_Form_Data;
      elsif C (1 .. AWS.MIME.Multipart_Form_Data'Length) = AWS.MIME.Multipart_Form_Data then
	 return AWS.MIME.Multipart_Form_Data;
      else
	 raise SCGI_Error
	   with "[Ether] Unknown Content-Type found.";
      end if;
   end Get_Content_Type;
   
   function Form_Data_Method_Is (Object : in Request) return Form_Data_Method is
   begin
      if Content_Length(Object) = 0 and Value(Object, Request_Method) = "GET" then
	 return Get;
      end if;
      
      return Put;
   end Form_Data_Method_Is;
   
   
   function Hash(Key : Header) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash(Header'Image(Key));
   end Hash;
end Ether.Requests;
