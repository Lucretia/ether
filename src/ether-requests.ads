--                              -*- Mode: Ada -*-
--  Filename        : ether-requests.ads
--  Description     : Interface to the request objects.
--  Author          : Luke A. Guest
--  Created On      : Sun Jul  4 19:11:27 2010
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;
with GNAT.Sockets;
with Unicode.CES;

package Ether.Requests is
   type Request is private;

   --  These will be queried from the request object.
   type Header is (Auth_Type,
                   Content_Length,  --  Same as HTTP_Content_Length?
                   Content_Type,    --  Same as HTTP_Content_Type?
                   Document_Root,
                   Gateway_Interface,
                   HTTP_Accept,
                   HTTP_Accept_Language,
                   HTTP_Accept_Encoding,
                   HTTP_Accept_Charset,
                   HTTP_Connection,
                   HTTP_Cookie,
                   HTTP_Host,
                   HTTP_Keep_Alive,
                   HTTP_User_Agent,
                   HTTP_Referer,
                   Path_Info,
                   Path_Translated,
                   Query_String,
                   Remote_Addr,
                   Remote_Host,
                   Remote_Ident,
                   Remote_Port,
                   Remote_User,
                   Request_Method,
                   Request_URI,
                   Script_Name,
                   Server_Addr,
                   Server_Admin,
                   Server_Name,
                   Server_Port,
                   Server_Protocol,
                   Server_Signature,
                   Server_Software,
                   SCGI);
   
   type Form_Data_Method is (Get, Put);

   SCGI_Error    : exception;
   Request_Error : exception;

   --  Read in the request from some socket and store the data in the request
   --  object.
   --  Raises Request_Error.
   procedure Receive
     (Object : in out Request;
      Input  : in GNAT.Sockets.Stream_Access);

   --  Clean up the request object, i.e. at the end of it's use.
   procedure Clean(Object : in out Request);

   --  Check to see if this is a real SCGI request.
   --  Raises SCGI_Error.
   function Is_Valid(Object : Request) return Boolean;

   function Value(Object : Request; Item : Header) return String;
   function Value(Object : Request; Item : String) return String;

   function Cookie(Object : Request; Item : String) return String;
   function Form(Object : Request; Item : String) return Unicode.CES.Byte_Sequence;--Unicode.UTF8_String;

   --  function Content(Object : Request) return String;
   
   --  Get the length of the content.
   --  Raises Request_Error.
   function Content_Length (Object : Request) return Natural;
   
   function Get_Content_Type (Object : Request) return String;
   
   function Form_Data_Method_Is (Object : in Request) return Form_Data_Method;

private
   package US renames Ada.Strings.Unbounded;

   function Hash(Key : Header) return Ada.Containers.Hash_Type;

   package Variable_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=",
      "="             => Standard."=");

   package Cookie_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=",
      "="             => Standard."=");

   Package Form_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Unicode.CES.Byte_Sequence,--Unicode.UTF8_String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=",
      "="             => Standard."=");
   
   type Request is
      record
         Environment  : Variable_Map.Map       := Variable_Map.Empty_Map;
         Cookies      : Cookie_Map.Map         := Cookie_Map.Empty_Map;
         Form         : Form_Map.Map           := Form_Map.Empty_Map;
         --  Form        : URIs.Parameter_Map.Map := URIs.Parameter_Map.Empty_Map;
         --  Content     : access Content_String  := "";
      end record;
end Ether.Requests;
