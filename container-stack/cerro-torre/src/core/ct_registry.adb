-------------------------------------------------------------------------------
--  CT_Registry - Implementation of OCI Distribution Specification Client
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Implements registry operations per OCI Distribution Specification.
--  Currently provides stub implementations pending HTTP client integration.
--
--  Future Integration Plan:
--    1. Add AWS.Client or similar HTTP library dependency
--    2. Implement OAuth2 token exchange for authentication
--    3. Add chunked upload support for large blobs
--    4. Integrate with Cerro_Crypto for digest verification
--
--  Security Considerations:
--    - Always verify TLS certificates in production
--    - Never log credentials or tokens
--    - Verify digests on all downloaded content
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  SPARK mode off pending HTTP client bindings

with Ada.Strings.Fixed;
with Ada.Text_IO;
with CT_HTTP;
with CT_JSON;
--  with Proven.Safe_Registry;  --  Temporarily disabled
--  with Proven.Safe_Digest;     --  Temporarily disabled
with Cerro_Crypto;

package body CT_Registry is

   use Ada.Strings.Fixed;

   ---------------------------------------------------------------------------
   --  Authentication Conversion
   ---------------------------------------------------------------------------

   function To_HTTP_Auth (Reg_Auth : Registry_Auth_Credentials) return CT_HTTP.Auth_Credentials
   is
      HTTP_Auth : CT_HTTP.Auth_Credentials;
   begin
      --  Convert registry auth method to HTTP auth scheme
      case Reg_Auth.Method is
         when None =>
            HTTP_Auth.Scheme := CT_HTTP.No_Auth;
         when Basic =>
            HTTP_Auth.Scheme := CT_HTTP.Basic_Auth;
            HTTP_Auth.Username := Reg_Auth.Username;
            HTTP_Auth.Password := Reg_Auth.Password;
         when Bearer | AWS_ECR | GCP_GCR | Azure_ACR =>
            --  All token-based auth becomes Bearer_Token for HTTP layer
            HTTP_Auth.Scheme := CT_HTTP.Bearer_Token;
            HTTP_Auth.Token := Reg_Auth.Token;
      end case;

      return HTTP_Auth;
   end To_HTTP_Auth;

   ---------------------------------------------------------------------------
   --  Internal Constants
   ---------------------------------------------------------------------------

   API_Version : constant String := "v2";

   ---------------------------------------------------------------------------
   --  Client Creation
   ---------------------------------------------------------------------------

   function Create_Client
     (Registry : String;
      Auth     : Registry_Auth_Credentials := (others => <>)) return Registry_Client
   is
      Client : Registry_Client;
      Reg    : String := Registry;
   begin
      --  Normalize registry URL
      if Registry'Length >= 8 and then
         Registry (Registry'First .. Registry'First + 7) = "https://"
      then
         Client.Base_URL := To_Unbounded_String (Registry);
      elsif Registry'Length >= 7 and then
            Registry (Registry'First .. Registry'First + 6) = "http://"
      then
         --  Allow HTTP for localhost/testing only
         Client.Base_URL := To_Unbounded_String (Registry);
      elsif Registry'Length >= 9 and then
            Registry (Registry'First .. Registry'First + 8) = "localhost"
      then
         --  localhost defaults to HTTP (for testing with local registries)
         Client.Base_URL := To_Unbounded_String ("http://" & Registry);
      else
         --  Prepend https://
         Client.Base_URL := To_Unbounded_String ("https://" & Registry);
      end if;

      Client.Auth := Auth;
      Client.User_Agent := To_Unbounded_String (Default_User_Agent);
      Client.Timeout_Ms := 30_000;
      Client.Verify_TLS := True;
      Client.Debug_Logging := False;

      return Client;
   end Create_Client;

   ---------------------------------------------------------------------------
   --  Authentication
   ---------------------------------------------------------------------------

   function Authenticate
     (Client     : in out Registry_Client;
      Repository : String;
      Actions    : String := "pull") return Registry_Error
   is
      Test_URL : constant String := To_String (Client.Base_URL) & "/v2/";
   begin
      --  Step 1: Try anonymous access to /v2/
      declare
         use CT_HTTP;
         Response : HTTP_Response;
      begin
         Response := Get (URL => Test_URL);

         --  If 200, no auth needed
         if Response.Success and then Response.Status_Code = 200 then
            Client.Auth.Method := None;
            return Success;
         end if;

         --  If not 401, something else is wrong
         if Response.Status_Code /= 401 then
            return Network_Error;
         end if;

         --  Step 2: Parse WWW-Authenticate header
         declare
            WWW_Auth : constant String := Get_Header (Response, WWW_Authenticate_Header);
            Challenge : WWW_Auth_Challenge;
         begin
            if WWW_Auth'Length = 0 then
               return Auth_Failed;
            end if;

            Challenge := Parse_WWW_Authenticate (WWW_Auth);

            --  Step 3: Exchange credentials for token
            if Length (Client.Auth.Username) = 0 then
               return Auth_Required;
            end if;

            --  Build token endpoint URL
            declare
               Token_URL : constant String := To_String (Challenge.Realm) &
                  "?service=" & To_String (Challenge.Service) &
                  "&scope=repository:" & Repository & ":" & Actions;
               Token_Auth : constant Auth_Credentials := Make_Basic_Auth (
                  To_String (Client.Auth.Username),
                  To_String (Client.Auth.Password));
               Token_Response : HTTP_Response;
            begin
               Token_Response := Get (
                  URL  => Token_URL,
                  Auth => Token_Auth);

               if not Token_Response.Success or else
                  not Is_Success (Token_Response.Status_Code)
               then
                  return Auth_Failed;
               end if;

               --  Parse JSON response to extract "token" or "access_token" field
               --  Response format: {"token": "...", "access_token": "...", "expires_in": 300}
               declare
                  JSON_Body : constant String := To_String (Token_Response.Content);
                  Token     : String := CT_JSON.Get_String_Field (JSON_Body, "token");
               begin
                  --  Try "access_token" if "token" not found (some registries use this)
                  if Token'Length = 0 then
                     Token := CT_JSON.Get_String_Field (JSON_Body, "access_token");
                  end if;

                  if Token'Length = 0 then
                     return Auth_Failed;  --  No token in response
                  end if;

                  Client.Auth.Token := To_Unbounded_String (Token);
                  Client.Auth.Method := Bearer;

                  return Success;
               end;
            end;
         end;
      end;
   end Authenticate;

   function Is_Authenticated (Client : Registry_Client) return Boolean is
   begin
      return Length (Client.Auth.Token) > 0 or else
             (Client.Auth.Method = Basic and then
              Length (Client.Auth.Username) > 0);
   end Is_Authenticated;

   ---------------------------------------------------------------------------
   --  Manifest Operations
   ---------------------------------------------------------------------------

   function Pull_Manifest
     (Client     : Registry_Client;
      Repository : String;
      Reference  : String) return Pull_Result
   is
      Result : Pull_Result;
      URL    : constant String := To_String (Client.Base_URL) &
                                  "/v2/" & Repository & "/manifests/" & Reference;
   begin
      --  Build Accept header for OCI manifest types
      declare
         use CT_HTTP;
         Headers : Header_Map;
         Auth    : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Add Accept header for manifest media types
         Headers.Insert (Accept_Header,
            OCI_Manifest_V1 & ", " &
            OCI_Index_V1 & ", " &
            Docker_Manifest_V2 & ", " &
            Docker_Manifest_List);

         --  Add authentication if available
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Perform GET request
         Response := Get (
            URL     => URL,
            Auth    => Auth,
            Headers => Headers);

         --  Handle response
         if not Response.Success then
            Result.Error := Network_Error;
            return Result;
         elsif Response.Status_Code = 401 or Response.Status_Code = 403 then
            Result.Error := Auth_Failed;
            return Result;
         elsif Response.Status_Code = 404 then
            Result.Error := Not_Found;
            return Result;
         elsif not Is_Success (Response.Status_Code) then
            Result.Error := Server_Error;
            return Result;
         end if;

         --  Success - store raw JSON for signature verification
         Result.Raw_Json := Response.Content;

         --  Extract Docker-Content-Digest header if present
         declare
            Digest_Header : constant String := Get_Header (Response, Docker_Content_Digest);
         begin
            if Digest_Header'Length > 0 then
               Result.Digest := To_Unbounded_String (Digest_Header);
            else
               --  Calculate digest from body
               Result.Digest := To_Unbounded_String (
                  Manifest_Digest (To_String (Response.Content)));
            end if;
         end;

         --  TODO: Parse JSON body into OCI_Manifest structure
         --  For now, mark success with empty manifest
         Result.Manifest.Schema_Version := 2;
         Result.Manifest.Media_Type := To_Unbounded_String (OCI_Manifest_V1);

         Result.Error := Success;
         return Result;
      end;
   end Pull_Manifest;

   function Push_Manifest
     (Client        : Registry_Client;
      Repository    : String;
      Tag           : String;
      Manifest      : OCI_Manifest;
      Manifest_Json : String := "") return Push_Result
   is
      Result : Push_Result;
      Json   : constant String := (if Manifest_Json'Length > 0
                                   then Manifest_Json
                                   else Manifest_To_Json (Manifest));
      URL    : constant String := To_String (Client.Base_URL) &
                                  "/v2/" & Repository & "/manifests/" & Tag;
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
         Content_Type : constant String := To_String (Manifest.Media_Type);
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  PUT manifest with appropriate Content-Type
         Response := Put (
            URL          => URL,
            Data         => Json,
            Content_Type => (if Content_Type'Length > 0
                            then Content_Type
                            else OCI_Manifest_V1),
            Auth         => Auth);

         --  Handle response
         if not Response.Success then
            if Client.Debug_Logging then
               Ada.Text_IO.Put_Line ("[DEBUG] HTTP request failed");
               Ada.Text_IO.Put_Line ("[DEBUG] Error: " & To_String (Response.Error_Message));
            end if;
            Result.Error := Network_Error;
            return Result;
         elsif Response.Status_Code = 401 or Response.Status_Code = 403 then
            if Client.Debug_Logging then
               Ada.Text_IO.Put_Line ("[DEBUG] Authentication failed");
               Ada.Text_IO.Put_Line ("[DEBUG] Status: " & Status_Code'Image (Response.Status_Code));
               Ada.Text_IO.Put_Line ("[DEBUG] Body: " & To_String (Response.Content));
            end if;
            Result.Error := Auth_Failed;
            return Result;
         elsif Response.Status_Code = 415 then
            if Client.Debug_Logging then
               Ada.Text_IO.Put_Line ("[DEBUG] Unsupported media type");
               Ada.Text_IO.Put_Line ("[DEBUG] Status: " & Status_Code'Image (Response.Status_Code));
               Ada.Text_IO.Put_Line ("[DEBUG] Body: " & To_String (Response.Content));
            end if;
            Result.Error := Unsupported_Media_Type;
            return Result;
         elsif not Is_Success (Response.Status_Code) then
            if Client.Debug_Logging then
               Ada.Text_IO.Put_Line ("[DEBUG] Server error");
               Ada.Text_IO.Put_Line ("[DEBUG] Status: " & Status_Code'Image (Response.Status_Code) &
                                     " " & To_String (Response.Status_Reason));
               Ada.Text_IO.Put_Line ("[DEBUG] Body: " & To_String (Response.Content));
            end if;
            Result.Error := Server_Error;
            return Result;
         end if;

         --  Extract Docker-Content-Digest from response
         declare
            Digest_Header : constant String := Get_Header (Response, Docker_Content_Digest);
         begin
            if Digest_Header'Length > 0 then
               Result.Digest := To_Unbounded_String (Digest_Header);
            else
               --  Calculate digest if not provided
               Result.Digest := To_Unbounded_String (Manifest_Digest (Json));
            end if;
         end;

         Result.Error := Success;
         return Result;
      end;
   end Push_Manifest;

   function Manifest_Exists
     (Client     : Registry_Client;
      Repository : String;
      Reference  : String) return Boolean
   is
      URL : constant String := To_String (Client.Base_URL) &
                                "/v2/" & Repository & "/manifests/" & Reference;
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Add authentication if available
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Perform HEAD request (efficient, no body)
         Response := Head (
            URL  => URL,
            Auth => Auth);

         --  Return true if 200 OK, false otherwise
         return Response.Success and then Response.Status_Code = 200;
      end;
   end Manifest_Exists;

   function Delete_Manifest
     (Client     : Registry_Client;
      Repository : String;
      Digest     : String) return Registry_Error
   is
      URL : constant String := To_String (Client.Base_URL) &
                                "/v2/" & Repository & "/manifests/" & Digest;
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  DELETE manifest (requires digest, not tag)
         Response := Delete (
            URL  => URL,
            Auth => Auth);

         --  Handle response
         if not Response.Success then
            return Network_Error;
         elsif Response.Status_Code = 202 or Response.Status_Code = 204 then
            return Success;  --  Accepted or No Content
         elsif Response.Status_Code = 401 or Response.Status_Code = 403 then
            return Auth_Failed;
         elsif Response.Status_Code = 404 then
            return Not_Found;
         elsif Response.Status_Code = 405 then
            return Forbidden;  --  Registry doesn't allow deletes
         else
            return Server_Error;
         end if;
      end;
   end Delete_Manifest;

   ---------------------------------------------------------------------------
   --  Blob Operations
   ---------------------------------------------------------------------------

   function Pull_Blob
     (Client      : Registry_Client;
      Repository  : String;
      Digest      : String;
      Output_Path : String := "") return Blob_Result
   is
      Result : Blob_Result;
      URL    : constant String := To_String (Client.Base_URL) &
                                  "/v2/" & Repository & "/blobs/" & Digest;
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Download blob (streaming to file if path provided)
         if Output_Path'Length > 0 then
            Response := Download_To_File (
               URL         => URL,
               Output_Path => Output_Path,
               Auth        => Auth);
         else
            Response := Get (
               URL  => URL,
               Auth => Auth);
         end if;

         --  Handle errors
         if not Response.Success then
            Result.Error := Network_Error;
            return Result;
         elsif Response.Status_Code = 404 then
            Result.Error := Not_Found;
            return Result;
         elsif not Is_Success (Response.Status_Code) then
            Result.Error := Server_Error;
            return Result;
         end if;

         --  Success
         Result.Content := Response.Content;
         Result.Digest := To_Unbounded_String (Digest);

         --  TODO: Verify digest matches downloaded content
         --  For now, assume digest is correct (caller should verify)

         Result.Error := Success;
         return Result;
      end;
   end Pull_Blob;

   function Push_Blob
     (Client     : Registry_Client;
      Repository : String;
      Content    : String;
      Media_Type : String := OCI_Layer_Gzip) return Push_Result
   is
      Result        : Push_Result;
      Init_URL      : constant String := To_String (Client.Base_URL) &
                                         "/v2/" & Repository & "/blobs/uploads/";
   begin
      --  Monolithic blob upload flow:
      --  1. POST to initiate upload session
      --  2. PUT to upload location with digest parameter

      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Step 1: Initiate upload (POST to get Location)
         Response := Post (
            URL          => Init_URL,
            Data         => "",  -- Empty body
            Content_Type => Media_Type,
            Auth         => Auth);

         if not Response.Success or else
            not (Response.Status_Code = 202 or Response.Status_Code = 201)
         then
            Result.Error := (if Response.Status_Code = 401 or Response.Status_Code = 403
                            then Auth_Failed
                            else Push_Failed);
            return Result;
         end if;

         --  Step 2: Extract upload Location from response
         declare
            Upload_Location : constant String := Get_Header (Response, Location_Header);
            Blob_Digest     : constant String := Manifest_Digest (Content);
            Upload_URL      : constant String := Upload_Location & "?digest=" & Blob_Digest;
         begin
            if Upload_Location'Length = 0 then
               Result.Error := Push_Failed;
               return Result;
            end if;

            --  Step 3: PUT blob content to upload location
            Response := Put (
               URL          => Upload_URL,
               Data         => Content,
               Content_Type => Media_Type,
               Auth         => Auth);

            if not Response.Success or else Response.Status_Code /= 201 then
               Result.Error := Push_Failed;
               return Result;
            end if;

            --  Success - extract digest from response
            Result.Digest := To_Unbounded_String (Blob_Digest);
            Result.Error := Success;
            return Result;
         end;
      end;
   end Push_Blob;

   function Push_Blob_From_File
     (Client      : Registry_Client;
      Repository  : String;
      File_Path   : String;
      Media_Type  : String := OCI_Layer_Gzip;
      Chunk_Size  : Positive := 5_242_880) return Push_Result
   is
      pragma Unreferenced (Chunk_Size);  --  TODO: Implement chunked upload
      Result   : Push_Result;
      Init_URL : constant String := To_String (Client.Base_URL) &
                                    "/v2/" & Repository & "/blobs/uploads/";
   begin
      --  For large files, we should use chunked upload via PATCH
      --  For now, use monolithic upload via Upload_From_File
      --
      --  TODO: Implement proper chunked upload:
      --  1. POST to initiate
      --  2. Loop: PATCH chunks with Content-Range headers
      --  3. PUT final chunk with digest

      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  Step 1: Initiate upload session
         Response := Post (
            URL          => Init_URL,
            Data         => "",
            Content_Type => Media_Type,
            Auth         => Auth);

         if not Response.Success or else
            not (Response.Status_Code = 202 or Response.Status_Code = 201)
         then
            Result.Error := (if Response.Status_Code = 401 or Response.Status_Code = 403
                            then Auth_Failed
                            else Push_Failed);
            return Result;
         end if;

         --  Step 2: Upload file in one shot (monolithic)
         --  TODO: Calculate digest first
         declare
            Upload_Location : constant String := Get_Header (Response, Location_Header);
            --  Placeholder digest (all zeros)
            Blob_Digest     : constant String :=
               "sha256:0000000000000000000000000000000000000000000000000000000000000000";
            Upload_URL      : constant String := Upload_Location & "?digest=" & Blob_Digest;
         begin
            if Upload_Location'Length = 0 then
               Result.Error := Push_Failed;
               return Result;
            end if;

            Response := Upload_From_File (
               URL          => Upload_URL,
               Input_Path   => File_Path,
               Content_Type => Media_Type,
               Auth         => Auth);

            if not Response.Success or else Response.Status_Code /= 201 then
               Result.Error := Push_Failed;
               return Result;
            end if;

            Result.Digest := To_Unbounded_String (Blob_Digest);
            Result.Error := Success;
            return Result;
         end;
      end;
   end Push_Blob_From_File;

   function Blob_Exists
     (Client     : Registry_Client;
      Repository : String;
      Digest     : String) return Boolean
   is
      URL : constant String := To_String (Client.Base_URL) &
                                "/v2/" & Repository & "/blobs/" & Digest;
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  HEAD request (efficient, no body downloaded)
         Response := Head (
            URL  => URL,
            Auth => Auth);

         return Response.Success and then Response.Status_Code = 200;
      end;
   end Blob_Exists;

   function Mount_Blob
     (Client      : Registry_Client;
      Target_Repo : String;
      Source_Repo : String;
      Digest      : String) return Registry_Error
   is
      URL : constant String := To_String (Client.Base_URL) &
                                "/v2/" & Target_Repo & "/blobs/uploads/" &
                                "?mount=" & Digest & "&from=" & Source_Repo;
   begin
      --  Cross-repository blob mount (efficiency optimization)
      --  If successful, registry creates a reference without re-uploading
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  POST with mount and from parameters
         Response := Post (
            URL          => URL,
            Data         => "",
            Content_Type => "application/octet-stream",
            Auth         => Auth);

         --  Handle response
         if not Response.Success then
            return Network_Error;
         elsif Response.Status_Code = 201 then
            return Success;  --  Blob mounted successfully
         elsif Response.Status_Code = 202 then
            --  Mount not possible, need to upload normally
            --  Location header contains upload URL for fallback
            return Not_Found;  --  Caller should fall back to regular upload
         elsif Response.Status_Code = 401 or Response.Status_Code = 403 then
            return Auth_Failed;
         else
            return Server_Error;
         end if;
      end;
   end Mount_Blob;

   ---------------------------------------------------------------------------
   --  Tag Operations
   ---------------------------------------------------------------------------

   function List_Tags
     (Client     : Registry_Client;
      Repository : String;
      Page_Size  : Positive := 100) return Tags_Result
   is
      Result : Tags_Result;
      URL    : constant String := To_String (Client.Base_URL) &
                                  "/v2/" & Repository & "/tags/list" &
                                  "?n=" & Positive'Image (Page_Size);
   begin
      declare
         use CT_HTTP;
         Auth     : Auth_Credentials := No_Credentials;
         Response : HTTP_Response;
      begin
         --  Setup authentication
         if Client.Auth.Method = Bearer and then
            Length (Client.Auth.Token) > 0
         then
            Auth := Make_Bearer_Auth (To_String (Client.Auth.Token));
         elsif Client.Auth.Method = Basic and then
               Length (Client.Auth.Username) > 0
         then
            Auth := Make_Basic_Auth (
               To_String (Client.Auth.Username),
               To_String (Client.Auth.Password));
         end if;

         --  GET tags list
         Response := Get (
            URL  => URL,
            Auth => Auth);

         --  Handle errors
         if not Response.Success then
            Result.Error := Network_Error;
            return Result;
         elsif Response.Status_Code = 401 or Response.Status_Code = 403 then
            Result.Error := Auth_Failed;
            return Result;
         elsif Response.Status_Code = 404 then
            Result.Error := Not_Found;
            return Result;
         elsif not Is_Success (Response.Status_Code) then
            Result.Error := Server_Error;
            return Result;
         end if;

         --  Parse JSON response
         --  Expected format: {"name": "repo", "tags": ["v1.0", "v1.1"]}
         declare
            JSON_Body : constant String := To_String (Response.Content);
            Tags_Array : constant CT_JSON.String_Array :=
              CT_JSON.Get_Array_Field (JSON_Body, "tags");
         begin
            for Tag of Tags_Array loop
               Result.Tags.Append (Tag);
            end loop;
         end;

         --  TODO: Pagination - Check Link header for next page URL
         --  Link: </v2/repo/tags/list?n=100&last=v1.9>; rel="next"

         Result.Error := Success;
         return Result;
      end;
   end List_Tags;

   ---------------------------------------------------------------------------
   --  Image Reference Parsing
   ---------------------------------------------------------------------------

   function Parse_Reference (Ref : String) return Image_Reference
   is
      Result        : Image_Reference;
      --  TODO: Use formally verified Proven.Safe_Registry.Parse when proven library compiles
      --  Format: [registry[:port]/]repository[:tag][@digest]
      At_Pos       : Natural;
      Colon_Pos    : Natural;
      Slash_Pos    : Natural;
      Tag_Colon    : Natural := 0;  --  Colon for tag (after repository)
   begin
      --  Simple fallback parser (not formally verified)
      --  Examples:
      --    "nginx" -> docker.io/library/nginx:latest
      --    "nginx:1.25" -> docker.io/library/nginx:1.25
      --    "ghcr.io/user/repo:v1.0" -> as-is
      --    "ghcr.io/user/repo@sha256:abc" -> as-is
      --    "localhost:5000/app:v1" -> localhost:5000 registry, app repo, v1 tag

      --  Check for @ (digest)
      At_Pos := Ada.Strings.Fixed.Index (Ref, "@");
      if At_Pos > 0 then
         Result.Digest := To_Unbounded_String (Ref (At_Pos + 1 .. Ref'Last));
      end if;

      --  Find first slash to separate registry from repository
      declare
         Search_End : constant Natural := (if At_Pos > 0 then At_Pos - 1 else Ref'Last);
      begin
         Slash_Pos := Ada.Strings.Fixed.Index (Ref (Ref'First .. Search_End), "/");

         --  Find tag colon (only search after repository path, not in registry part)
         if Slash_Pos > 0 then
            --  Has slash - look for tag colon after the repository path
            Tag_Colon := Ada.Strings.Fixed.Index (Ref (Slash_Pos + 1 .. Search_End), ":");
            if Tag_Colon > 0 then
               Result.Tag := To_Unbounded_String (Ref (Tag_Colon + 1 .. Search_End));
            else
               Result.Tag := To_Unbounded_String (Default_Tag);
            end if;
         else
            --  No slash - look for tag colon in entire string
            Colon_Pos := Ada.Strings.Fixed.Index (Ref (Ref'First .. Search_End), ":");
            if Colon_Pos > 0 then
               Result.Tag := To_Unbounded_String (Ref (Colon_Pos + 1 .. Search_End));
               Tag_Colon := Colon_Pos;  --  For Repo_End calculation
            else
               Result.Tag := To_Unbounded_String (Default_Tag);
            end if;
         end if;
      end;

      --  Parse registry/repository
      declare
         Repo_End : constant Natural :=
            (if Tag_Colon > 0 then Tag_Colon - 1
             elsif At_Pos > 0 then At_Pos - 1
             else Ref'Last);
      begin
         if Slash_Pos > 0 and then
            (Ada.Strings.Fixed.Index (Ref (Ref'First .. Slash_Pos - 1), ".") > 0 or else
             Ada.Strings.Fixed.Index (Ref (Ref'First .. Slash_Pos - 1), ":") > 0)
         then
            --  Has registry (contains dot or colon [port] before first slash)
            Result.Registry := To_Unbounded_String (Ref (Ref'First .. Slash_Pos - 1));
            Result.Repository := To_Unbounded_String (Ref (Slash_Pos + 1 .. Repo_End));
         else
            --  No registry - use default
            Result.Registry := To_Unbounded_String (Default_Registry);
            if Slash_Pos = 0 then
               Result.Repository := To_Unbounded_String ("library/" & Ref (Ref'First .. Repo_End));
            else
               Result.Repository := To_Unbounded_String (Ref (Ref'First .. Repo_End));
            end if;
         end if;
      end;

      return Result;
   end Parse_Reference;

   function To_String (Ref : Image_Reference) return String is
      Result : Unbounded_String;
   begin
      --  Format: registry/repository[:tag][@digest]
      if Length (Ref.Registry) > 0 and then
         To_String (Ref.Registry) /= Default_Registry
      then
         Append (Result, Ref.Registry);
         Append (Result, "/");
      end if;

      Append (Result, Ref.Repository);

      if Length (Ref.Tag) > 0 and then
         To_String (Ref.Tag) /= Default_Tag
      then
         Append (Result, ":");
         Append (Result, Ref.Tag);
      end if;

      if Length (Ref.Digest) > 0 then
         Append (Result, "@");
         Append (Result, Ref.Digest);
      end if;

      return To_String (Result);
   end To_String;

   ---------------------------------------------------------------------------
   --  Manifest Serialization
   ---------------------------------------------------------------------------

   function Manifest_To_Json (M : OCI_Manifest) return String is
      Builder : CT_JSON.JSON_Builder := CT_JSON.Create;

      --  Helper: Serialize blob descriptor to JSON
      function Blob_To_JSON (B : Blob_Descriptor) return String is
         Blob_Builder : CT_JSON.JSON_Builder := CT_JSON.Create;
      begin
         CT_JSON.Add_String (Blob_Builder, "mediaType", To_String (B.Media_Type));
         CT_JSON.Add_String (Blob_Builder, "digest", To_String (B.Digest));
         CT_JSON.Add_Integer (Blob_Builder, "size", Integer (B.Size));
         if Length (B.Annotations) > 0 then
            CT_JSON.Add_Object (Blob_Builder, "annotations", To_String (B.Annotations));
         end if;
         return CT_JSON.To_JSON (Blob_Builder);
      end Blob_To_JSON;

   begin
      --  Build OCI manifest JSON per OCI Image Manifest Specification
      CT_JSON.Add_Integer (Builder, "schemaVersion", M.Schema_Version);
      CT_JSON.Add_String (Builder, "mediaType", To_String (M.Media_Type));

      --  Add config descriptor
      CT_JSON.Add_Object (Builder, "config", Blob_To_JSON (M.Config));

      --  Add layers array
      declare
         Layers_JSON : Unbounded_String := To_Unbounded_String ("[");
      begin
         for I in M.Layers.First_Index .. M.Layers.Last_Index loop
            if I > M.Layers.First_Index then
               Append (Layers_JSON, ",");
            end if;
            Append (Layers_JSON, Blob_To_JSON (M.Layers (I)));
         end loop;
         Append (Layers_JSON, "]");
         CT_JSON.Add_Object (Builder, "layers", To_String (Layers_JSON));
      end;

      --  Add annotations if present
      if Length (M.Annotations) > 0 then
         CT_JSON.Add_Object (Builder, "annotations", To_String (M.Annotations));
      end if;

      return CT_JSON.To_JSON (Builder);
   end Manifest_To_Json;

   function Parse_Manifest (Json : String) return Pull_Result is
      Result : Pull_Result;
   begin
      --  Parse OCI manifest JSON structure
      --  Format: {"schemaVersion": 2, "mediaType": "...", "config": {...}, "layers": [...]}

      --  Parse basic fields
      Result.Manifest.Schema_Version := CT_JSON.Get_Integer_Field (Json, "schemaVersion");
      Result.Manifest.Media_Type := To_Unbounded_String (
         CT_JSON.Get_String_Field (Json, "mediaType"));

      --  TODO: Parse config and layers objects
      --  This requires more sophisticated JSON parsing (nested objects/arrays)
      --  For now, mark as partially implemented - can serialize but not fully parse

      Result.Raw_Json := To_Unbounded_String (Json);
      Result.Error := Success;
      return Result;
   end Parse_Manifest;

   function Manifest_Digest (Json : String) return String is
      Hash : Cerro_Crypto.SHA256_Digest;
      Hex  : String (1 .. 64);
   begin
      --  Compute SHA256 hash of canonical JSON using formally verified crypto
      Hash := Cerro_Crypto.Compute_SHA256 (Json);
      Hex  := Cerro_Crypto.Bytes_To_Hex (Hash);

      --  Return in OCI format: algorithm:hex
      return "sha256:" & Hex;
   end Manifest_Digest;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Verify_Digest
     (Content : String;
      Digest  : String) return Boolean
   is
      Actual_Digest : constant String := Manifest_Digest (Content);
   begin
      --  TODO: Use formally verified Proven.Safe_Digest.Verify when proven library compiles
      --  For now, simple string comparison (NOT constant-time, not timing-attack-safe)
      --  Format: "algorithm:hex"

      --  Simple comparison
      return Actual_Digest = Digest;
   end Verify_Digest;

   function Error_Message (E : Registry_Error) return String is
   begin
      case E is
         when Success =>
            return "Operation completed successfully";
         when Not_Implemented =>
            return "Feature not yet implemented";
         when Network_Error =>
            return "Network connection failed";
         when Auth_Required =>
            return "Authentication required";
         when Auth_Failed =>
            return "Authentication failed: invalid credentials";
         when Not_Found =>
            return "Resource not found (404)";
         when Forbidden =>
            return "Access denied (403)";
         when Digest_Mismatch =>
            return "Content digest verification failed";
         when Invalid_Manifest =>
            return "Invalid manifest format";
         when Rate_Limited =>
            return "Rate limit exceeded (429)";
         when Server_Error =>
            return "Registry server error";
         when Push_Failed =>
            return "Failed to push content";
         when Unsupported_Media_Type =>
            return "Media type not supported by registry";
         when Timeout =>
            return "Request timed out";
      end case;
   end Error_Message;

end CT_Registry;
