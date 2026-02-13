-------------------------------------------------------------------------------
--  CT_HTTP - Implementation using curl (MVP version)
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Implements HTTP operations using curl subprocess calls.
--  Provides the HTTP foundation for CT_Registry and CT_Transparency.
--
--  NOTE: This is an MVP implementation. Future versions should use a native
--  Ada HTTP library when gnatcoll/AWS compatibility is resolved.
-------------------------------------------------------------------------------

with GNAT.OS_Lib;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with Cerro_URL;

package body CT_HTTP is

   use Ada.Strings.Fixed;
   use Ada.Text_IO;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Generate_Temp_File (Prefix : String) return String is
      use Ada.Directories;
      Temp_Dir : constant String := "/tmp";
      Counter  : Natural := 0;
   begin
      loop
         declare
            File_Name : constant String :=
               Temp_Dir & "/" & Prefix & Natural'Image (Counter) & ".tmp";
         begin
            if not Exists (File_Name) then
               return File_Name;
            end if;
            Counter := Counter + 1;
         end;
      end loop;
   end Generate_Temp_File;

   procedure Delete_File_Safe (Path : String) is
      use Ada.Directories;
   begin
      if Exists (Path) then
         Delete_File (Path);
      end if;
   exception
      when others => null;  --  Ignore deletion failures
   end Delete_File_Safe;

   function Read_File_Contents (Path : String) return String is
      use Ada.Streams.Stream_IO;
      File   : Ada.Streams.Stream_IO.File_Type;
      Buffer : String (1 .. Natural (Ada.Directories.Size (Path)));
   begin
      Open (File, In_File, Path);
      String'Read (Stream (File), Buffer);
      Close (File);
      return Buffer;
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         return "";
   end Read_File_Contents;

   ---------------------------------------------------------------------------
   --  Status Code Predicates
   ---------------------------------------------------------------------------

   function Is_Success (Code : Status_Code) return Boolean is
   begin
      return Code in 200 .. 299;
   end Is_Success;

   function Is_Redirect (Code : Status_Code) return Boolean is
   begin
      return Code in 300 .. 399;
   end Is_Redirect;

   function Is_Client_Error (Code : Status_Code) return Boolean is
   begin
      return Code in 400 .. 499;
   end Is_Client_Error;

   function Is_Server_Error (Code : Status_Code) return Boolean is
   begin
      return Code in 500 .. 599;
   end Is_Server_Error;

   ---------------------------------------------------------------------------
   --  Response Constructors
   ---------------------------------------------------------------------------

   function Empty_Response return HTTP_Response is
   begin
      return HTTP_Response'
        (Status_Code   => 0,
         Status_Reason => Null_Unbounded_String,
         Headers       => Header_Maps.Empty_Map,
         Content       => Null_Unbounded_String,
         Error_Message => Null_Unbounded_String,
         Success       => False);
   end Empty_Response;

   function Error_Response (Message : String) return HTTP_Response is
      Response : HTTP_Response := Empty_Response;
   begin
      Response.Error_Message := To_Unbounded_String (Message);
      Response.Success := False;
      return Response;
   end Error_Response;

   ---------------------------------------------------------------------------
   --  Header Utilities
   ---------------------------------------------------------------------------

   function Normalize_Header_Name (Name : String) return String is
      Result : String := Name;
   begin
      for I in Result'Range loop
         Result (I) := Ada.Characters.Handling.To_Lower (Result (I));
      end loop;
      return Result;
   end Normalize_Header_Name;

   function Get_Header
     (Response : HTTP_Response;
      Name     : String) return String
   is
      Normalized_Name : constant String := Normalize_Header_Name (Name);
      Cursor : Header_Maps.Cursor;
   begin
      Cursor := Response.Headers.First;
      while Header_Maps.Has_Element (Cursor) loop
         if Normalize_Header_Name (Header_Maps.Key (Cursor)) = Normalized_Name then
            return Header_Maps.Element (Cursor);
         end if;
         Header_Maps.Next (Cursor);
      end loop;
      return "";
   end Get_Header;

   function Has_Header
     (Response : HTTP_Response;
      Name     : String) return Boolean
   is
   begin
      return Get_Header (Response, Name) /= "";
   end Has_Header;

   ---------------------------------------------------------------------------
   --  curl Invocation
   ---------------------------------------------------------------------------

   function Execute_Curl
     (Method       : HTTP_Method;
      URL          : String;
      Config       : HTTP_Client_Config;
      Auth         : Auth_Credentials;
      Headers      : Header_Map;
      Request_Body : String := "";
      Content_Type : String := "";
      Output_File  : String := "";
      Input_File   : String := "") return HTTP_Response
   is
      use GNAT.OS_Lib;

      Response      : HTTP_Response;
      Header_File   : constant String := Generate_Temp_File ("ct-headers-");
      Body_File     : constant String := Generate_Temp_File ("ct-body-");
      Body_In_File  : constant String := Generate_Temp_File ("ct-bodyin-");

      Arg_List      : Argument_List (1 .. 50);
      Arg_Count     : Natural := 0;
      Success       : Boolean;
      Return_Code   : Integer;

      procedure Add_Arg (Arg : String) is
      begin
         Arg_Count := Arg_Count + 1;
         Arg_List (Arg_Count) := new String'(Arg);
      end Add_Arg;

   begin
      --  Base curl arguments
      Add_Arg ("-s");  --  Silent
      Add_Arg ("-S");  --  Show errors
      Add_Arg ("-D");  --  Dump headers to file
      Add_Arg (Header_File);

      --  Timeout
      Add_Arg ("--max-time");
      Add_Arg (Ada.Strings.Fixed.Trim (Positive'Image (Config.Timeout_Seconds), Ada.Strings.Both));

      --  TLS verification
      if not Config.Verify_TLS then
         Add_Arg ("-k");
      end if;

      --  Follow redirects
      if Config.Follow_Redirects then
         Add_Arg ("-L");
         Add_Arg ("--max-redirs");
         Add_Arg (Ada.Strings.Fixed.Trim (Positive'Image (Config.Max_Redirects), Ada.Strings.Both));
      end if;

      --  User agent
      Add_Arg ("-A");
      Add_Arg (To_String (Config.User_Agent));

      --  HTTP version
      case Config.Protocol_Version is
         when HTTP_Auto => null;  --  curl default behavior
         when HTTP_1_0 => Add_Arg ("--http1.0");
         when HTTP_1_1 => Add_Arg ("--http1.1");
         when HTTP_2 => Add_Arg ("--http2");
         when HTTP_2_Prior => Add_Arg ("--http2-prior-knowledge");
         when HTTP_3 => Add_Arg ("--http3");  --  Requires curl 7.66+ with HTTP/3 support
      end case;

      --  Encrypted Client Hello (curl 8.2+)
      if Config.Enable_ECH then
         Add_Arg ("--ech");
      end if;

      --  Alt-Svc for HTTP/3 upgrade
      if not Config.Enable_Alt_Svc then
         Add_Arg ("--no-alt-svc");
      end if;

      --  DANE/TLSA (DNS-based TLS authentication)
      if Config.DNS_Sec.Enable_DANE then
         if Config.DNS_Sec.Require_DANE then
            Add_Arg ("--tlsv1.2");  --  DANE requires TLS 1.2+
            Add_Arg ("--tlsauthtype");
            Add_Arg ("SRP");  --  Use DANE/TLSA records
         end if;
         --  Note: curl automatically uses TLSA records when available
         --  No explicit flag needed for opportunistic DANE
      end if;

      --  DNS-over-HTTPS
      if Length (Config.DNS_Sec.DoH_URL) > 0 then
         Add_Arg ("--doh-url");
         Add_Arg (To_String (Config.DNS_Sec.DoH_URL));
      end if;

      --  Oblivious DNS-over-HTTPS (ODoH / ODNS)
      --  Requires both target and proxy URLs (curl 7.73+)
      if Length (Config.DNS_Sec.ODoH_Target_URL) > 0 and then
         Length (Config.DNS_Sec.ODoH_Proxy_URL) > 0
      then
         --  Note: As of curl 8.x, ODoH support is experimental
         --  Use --doh-url with the target, then configure proxy separately
         --  This is a simplified implementation - full ODoH may need additional flags
         Add_Arg ("--doh-url");
         Add_Arg (To_String (Config.DNS_Sec.ODoH_Target_URL));
         --  TODO: Add ODoH proxy configuration when curl support stabilizes
         --  For now, this provides basic DoH to the target URL
      end if;

      --  Proxy configuration
      if Config.Proxy.Protocol /= No_Proxy then
         declare
            Proxy_URL : constant String :=
               To_String (Config.Proxy.Host) & ":" &
               Natural'Image (Config.Proxy.Port);
         begin
            case Config.Proxy.Protocol is
               when No_Proxy => null;
               when HTTP_Proxy =>
                  Add_Arg ("-x");
                  Add_Arg (Proxy_URL);
               when SOCKS4_Proxy =>
                  Add_Arg ("--socks4");
                  Add_Arg (Proxy_URL);
               when SOCKS4A_Proxy =>
                  Add_Arg ("--socks4a");
                  Add_Arg (Proxy_URL);
               when SOCKS5_Proxy =>
                  Add_Arg ("--socks5");
                  Add_Arg (Proxy_URL);
               when SOCKS5H_Proxy =>
                  Add_Arg ("--socks5-hostname");
                  Add_Arg (Proxy_URL);
            end case;

            --  Proxy authentication
            if Length (Config.Proxy.Username) > 0 then
               Add_Arg ("--proxy-user");
               Add_Arg (To_String (Config.Proxy.Username) & ":" &
                        To_String (Config.Proxy.Password));
            end if;
         end;
      end if;

      --  TCP options
      if Config.TCP_Keepalive then
         Add_Arg ("--keepalive-time");
         Add_Arg ("60");  --  60 seconds
      end if;

      if Config.TCP_Nodelay then
         Add_Arg ("--tcp-nodelay");
      end if;

      --  IP version
      if Config.IPv4_Only then
         Add_Arg ("-4");
      elsif Config.IPv6_Only then
         Add_Arg ("-6");
      end if;

      --  Method
      case Method is
         when GET  => Add_Arg ("-X"); Add_Arg ("GET");
         when POST => Add_Arg ("-X"); Add_Arg ("POST");
         when PUT  => Add_Arg ("-X"); Add_Arg ("PUT");
         when DELETE => Add_Arg ("-X"); Add_Arg ("DELETE");
         when HEAD => Add_Arg ("-I");
         when PATCH => Add_Arg ("-X"); Add_Arg ("PATCH");
      end case;

      --  Authentication
      case Auth.Scheme is
         when No_Auth => null;
         when Basic_Auth =>
            Add_Arg ("-u");
            Add_Arg (To_String (Auth.Username) & ":" & To_String (Auth.Password));
         when Bearer_Token =>
            Add_Arg ("-H");
            Add_Arg ("Authorization: Bearer " & To_String (Auth.Token));
      end case;

      --  Custom headers
      declare
         Cursor : Header_Maps.Cursor := Headers.First;
      begin
         while Header_Maps.Has_Element (Cursor) loop
            Add_Arg ("-H");
            Add_Arg (Header_Maps.Key (Cursor) & ": " & Header_Maps.Element (Cursor));
            Header_Maps.Next (Cursor);
         end loop;
      end;

      --  Body handling
      if Request_Body'Length > 0 then
         --  Write body to temp file
         declare
            F : File_Type;
         begin
            Create (F, Out_File, Body_In_File);
            Put (F, Request_Body);
            Close (F);
         end;

         Add_Arg ("--data-binary");
         Add_Arg ("@" & Body_In_File);

         if Content_Type'Length > 0 then
            Add_Arg ("-H");
            Add_Arg ("Content-Type: " & Content_Type);
         end if;
      elsif Input_File'Length > 0 then
         Add_Arg ("--data-binary");
         Add_Arg ("@" & Input_File);

         if Content_Type'Length > 0 then
            Add_Arg ("-H");
            Add_Arg ("Content-Type: " & Content_Type);
         end if;
      end if;

      --  Output handling
      if Output_File'Length > 0 then
         Add_Arg ("-o");
         Add_Arg (Output_File);
      else
         Add_Arg ("-o");
         Add_Arg (Body_File);
      end if;

      --  URL (must be last)
      Add_Arg (URL);

      --  Execute curl
      Spawn ("/usr/bin/curl", Arg_List (1 .. Arg_Count), Success);
      Return_Code := 0;  --  Success is set by Spawn, assume 0 if successful

      --  Parse response
      if Success and Return_Code = 0 then
         Response.Success := True;

         --  Parse headers file
         if Ada.Directories.Exists (Header_File) then
            declare
               F    : File_Type;
               Line : String (1 .. 8192);
               Last : Natural;
            begin
               Open (F, In_File, Header_File);

               --  First line is status line
               if not End_Of_File (F) then
                  Get_Line (F, Line, Last);
                  --  Parse "HTTP/1.1 200 OK"
                  declare
                     Status_Start : constant Natural := Index (Line (1 .. Last), " ");
                     Status_End   : Natural;
                  begin
                     if Status_Start > 0 then
                        Status_End := Index (Line (Status_Start + 1 .. Last), " ");
                        if Status_End = 0 then
                           Status_End := Last + 1;
                        end if;

                        Response.Status_Code := Status_Code'Value (
                           Line (Status_Start + 1 .. Status_End - 1));

                        if Status_End <= Last then
                           Response.Status_Reason := To_Unbounded_String (
                              Line (Status_End + 1 .. Last));
                        end if;
                     end if;
                  end;
               end if;

               --  Remaining lines are headers
               while not End_Of_File (F) loop
                  Get_Line (F, Line, Last);
                  if Last > 0 and Line (1) /= Character'Val (13) then
                     declare
                        Colon_Pos : constant Natural := Index (Line (1 .. Last), ":");
                     begin
                        if Colon_Pos > 0 then
                           declare
                              Key : constant String :=
                                 Trim (Line (1 .. Colon_Pos - 1), Ada.Strings.Both);
                              Value : constant String :=
                                 Trim (Line (Colon_Pos + 1 .. Last), Ada.Strings.Both);
                           begin
                              Response.Headers.Insert (Key, Value);
                           end;
                        end if;
                     end;
                  end if;
               end loop;

               Close (F);
            end;
         end if;

         --  Read body (if not output to file)
         if Output_File'Length = 0 and Ada.Directories.Exists (Body_File) then
            Response.Content := To_Unbounded_String (Read_File_Contents (Body_File));
         end if;
      else
         Response.Success := False;
         Response.Error_Message := To_Unbounded_String ("curl failed");
      end if;

      --  Cleanup temp files
      Delete_File_Safe (Header_File);
      Delete_File_Safe (Body_File);
      Delete_File_Safe (Body_In_File);

      --  Free argument memory
      for I in 1 .. Arg_Count loop
         Free (Arg_List (I));
      end loop;

      return Response;

   exception
      when E : others =>
         --  Cleanup on error
         Delete_File_Safe (Header_File);
         Delete_File_Safe (Body_File);
         Delete_File_Safe (Body_In_File);
         for I in 1 .. Arg_Count loop
            Free (Arg_List (I));
         end loop;

         return Error_Response ("HTTP request failed: " &
            Ada.Exceptions.Exception_Message (E));
   end Execute_Curl;

   ---------------------------------------------------------------------------
   --  HTTP Request Functions
   ---------------------------------------------------------------------------

   function Get
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (GET, URL, Config, Auth, Headers);
   end Get;

   function Post
     (URL          : String;
      Data         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (POST, URL, Config, Auth, Headers, Data, Content_Type);
   end Post;

   function Put
     (URL          : String;
      Data         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (PUT, URL, Config, Auth, Headers, Data, Content_Type);
   end Put;

   function Delete
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (DELETE, URL, Config, Auth, Headers);
   end Delete;

   function Head
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (HEAD, URL, Config, Auth, Headers);
   end Head;

   function Patch
     (URL          : String;
      Data         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (PATCH, URL, Config, Auth, Headers, Data, Content_Type);
   end Patch;

   ---------------------------------------------------------------------------
   --  Streaming/Large Body Support
   ---------------------------------------------------------------------------

   function Download_To_File
     (URL         : String;
      Output_Path : String;
      Config      : HTTP_Client_Config := Default_Config;
      Auth        : Auth_Credentials := No_Credentials;
      Headers     : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (GET, URL, Config, Auth, Headers,
                          Output_File => Output_Path);
   end Download_To_File;

   function Upload_From_File
     (URL          : String;
      Input_Path   : String;
      Content_Type : String := "application/octet-stream";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   is
   begin
      return Execute_Curl (PUT, URL, Config, Auth, Headers,
                          Content_Type => Content_Type,
                          Input_File => Input_Path);
   end Upload_From_File;

   ---------------------------------------------------------------------------
   --  Authentication Helpers
   ---------------------------------------------------------------------------

   function Make_Basic_Auth
     (Username : String;
      Password : String) return Auth_Credentials
   is
   begin
      return (Scheme   => Basic_Auth,
              Username => To_Unbounded_String (Username),
              Password => To_Unbounded_String (Password),
              Token    => Null_Unbounded_String);
   end Make_Basic_Auth;

   function Make_Bearer_Auth (Token : String) return Auth_Credentials is
   begin
      return (Scheme   => Bearer_Token,
              Username => Null_Unbounded_String,
              Password => Null_Unbounded_String,
              Token    => To_Unbounded_String (Token));
   end Make_Bearer_Auth;

   function Parse_WWW_Authenticate (Header_Value : String) return WWW_Auth_Challenge is
      Result : WWW_Auth_Challenge;
   begin
      --  TODO: Implement proper parsing of Bearer realm="..." service="..." scope="..."
      --  For now, return empty challenge
      return Result;
   end Parse_WWW_Authenticate;

   ---------------------------------------------------------------------------
   --  URL Utilities
   ---------------------------------------------------------------------------

   function URL_Encode (S : String) return String is
   begin
      --  Delegate to Cerro_URL for RFC 3986 compliant percent encoding
      --  (Inspired by formally verified Proven.SafeHTTP module)
      return Cerro_URL.URL_Encode (S);
   end URL_Encode;

   function Build_URL
     (Base_URL : String;
      Path     : String;
      Query    : Header_Map := Header_Maps.Empty_Map) return String
   is
      Result : Unbounded_String := To_Unbounded_String (Base_URL);
   begin
      if Path'Length > 0 then
         if Path (Path'First) /= '/' and
            Element (Result, Length (Result)) /= '/' then
            Append (Result, '/');
         end if;
         Append (Result, Path);
      end if;

      if not Query.Is_Empty then
         Append (Result, '?');
         declare
            Cursor : Header_Maps.Cursor := Query.First;
            First  : Boolean := True;
         begin
            while Header_Maps.Has_Element (Cursor) loop
               if not First then
                  Append (Result, '&');
               end if;
               Append (Result, URL_Encode (Header_Maps.Key (Cursor)));
               Append (Result, '=');
               Append (Result, URL_Encode (Header_Maps.Element (Cursor)));
               First := False;
               Header_Maps.Next (Cursor);
            end loop;
         end;
      end if;

      return To_String (Result);
   end Build_URL;

   function Join_Path (Base : String; Path : String) return String is
   begin
      if Base'Length = 0 then
         return Path;
      elsif Path'Length = 0 then
         return Base;
      elsif Base (Base'Last) = '/' and Path (Path'First) = '/' then
         return Base & Path (Path'First + 1 .. Path'Last);
      elsif Base (Base'Last) /= '/' and Path (Path'First) /= '/' then
         return Base & '/' & Path;
      else
         return Base & Path;
      end if;
   end Join_Path;

end CT_HTTP;
