--                              -*- Mode: Ada -*-
--  Filename        : ether-forms.ads
--  Description     : Abstraction around the form data returned from the SCGI client (HTTP server).
--  Author          : Luke A. Guest
--  Created On      : Tue May  1 13:10:29 2012
with Unicode.CES;

package Ether.Forms is
   --  Form data is transmitted to the SCGI server in 1 of 2 ways, GET and PUT. GET provides
   --  the form data in the URI
   --  Each item in a form will have some data associated with it. This data will either be
   --  Unicode.CES.Byte_Sequence if it is normal data or it will be data, i.e. a file that
   --  has been encoded, Base64?
   --
   --  We will handle the translation of text from whatever it comes in as, i.e. UTF-8 to
   --  the abstract type, Unicode.CES.Byte_Sequence, the application must then translate it to
   --  it's preferred encoding.
   --
   --  Some forms can send multiple items per field, this will be handled internally as a list.
   --  The data stored will look something like this:
   --
   --  +------------------------+
   --  | Field     | Data       |
   --  +------------------------+
   --  | forename  | Barry      |
   --  | surname   | Watkins    |
   --  | filename  | hello.txt  | - First in the filename list
   --  | filename  | hello2.txt | - Second in the filename list
   --  +------------------------+
   --
   --  Obviously, any file that is transmitted to the SCGI server will also include the file
   --  data.
   --
   --  Both urlencoded and multipart type forms will be supported by this package and will
   --  present the data in the same way.
   --
   --  The form data is essentially mapping of:
   --    field name :-> list of data.
   
   --  type Data_Type is
   --    (Octet,  --  This represents a file.
   --     String  --  This is a unicode string encoded to Byte_Sequence.
   --    );
   
   --  type Form is private;
   
   Form_Error : exception;
   
   procedure Decode_Query (Data : in String);
--  private
   --  type Form is 
   --     null record
      --  end record;
end Ether.Forms;
