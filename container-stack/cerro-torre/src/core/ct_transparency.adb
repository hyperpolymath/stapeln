-------------------------------------------------------------------------------
--  CT_Transparency - Implementation of Transparency Log Integration
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Implements transparency log operations per Sigstore/Rekor API.
--
--  Integration Status:
--    ✓ HTTP client for Rekor API calls (via CT_HTTP)
--    ✓ JSON request/response handling (via CT_JSON)
--    ✓ Upload signatures to transparency logs
--    ✓ Lookup entries by UUID and index
--    ○ Merkle proof verification (pending)
--    ○ SET signature verification (pending crypto bindings)
--
--  Security Considerations:
--    - Always verify SET and inclusion proofs in production
--    - Cache and verify consistency of log state
--    - Monitor for unexpected entries (key compromise detection)
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  SPARK mode off pending HTTP client bindings

with CT_HTTP; use CT_HTTP;
with CT_JSON;
with Ada.Strings.Fixed;

package body CT_Transparency is

   use Ada.Strings.Fixed;

   ---------------------------------------------------------------------------
   --  Client Creation
   ---------------------------------------------------------------------------

   function Create_Client
     (Provider : Log_Provider := Sigstore_Rekor;
      URL      : String := "") return Log_Client
   is
      Client : Log_Client;
   begin
      Client.Provider := Provider;

      if URL'Length > 0 then
         Client.Base_URL := To_Unbounded_String (URL);
      else
         case Provider is
            when Sigstore_Rekor =>
               Client.Base_URL := To_Unbounded_String (Rekor_Production_URL);
            when Sigstore_Staging =>
               Client.Base_URL := To_Unbounded_String (Rekor_Staging_URL);
            when CT_TLOG =>
               --  TODO: Define CT-TLOG default URL
               Client.Base_URL := To_Unbounded_String ("https://tlog.cerro-torre.dev");
            when Custom =>
               --  Must provide URL for custom
               Client.Base_URL := To_Unbounded_String ("");
         end case;
      end if;

      Client.Timeout_Ms := 30_000;
      Client.Verify_TLS := True;

      return Client;
   end Create_Client;

   ---------------------------------------------------------------------------
   --  Entry Upload
   ---------------------------------------------------------------------------

   function Upload_Signature
     (Client     : Log_Client;
      Signature  : String;
      Artifact   : String;
      Hash       : String := "";
      Public_Key : String) return Upload_Result
   is
      Result       : Upload_Result;
      Request_Body : CT_JSON.JSON_Builder;
      Spec_Builder : CT_JSON.JSON_Builder;
      Sig_Builder  : CT_JSON.JSON_Builder;
      PK_Builder   : CT_JSON.JSON_Builder;
      Data_Builder : CT_JSON.JSON_Builder;
      Hash_Builder : CT_JSON.JSON_Builder;

      Request_JSON : Unbounded_String;
      API_URL      : constant String := To_String (Client.Base_URL) & "/api/v1/log/entries";
      Response     : HTTP_Response;
      Artifact_Hash : Unbounded_String;
   begin
      --  Use provided hash or compute from artifact
      if Hash'Length > 0 then
         Artifact_Hash := To_Unbounded_String (Hash);
      else
         --  TODO: Compute SHA256 of artifact
         --  For now, require hash to be provided
         Result.Error := Invalid_Entry;
         return Result;
      end if;

      --  Build hashedrekord request JSON
      --  Structure: { kind, apiVersion, spec: { signature: {...}, data: {...} } }

      --  Build signature.publicKey object
      PK_Builder := CT_JSON.Create;
      CT_JSON.Add_String (PK_Builder, "content", Public_Key);

      --  Build signature object
      Sig_Builder := CT_JSON.Create;
      CT_JSON.Add_String (Sig_Builder, "content", Signature);
      CT_JSON.Add_String (Sig_Builder, "publicKey", CT_JSON.To_JSON (PK_Builder));

      --  Build data.hash object
      Hash_Builder := CT_JSON.Create;
      CT_JSON.Add_String (Hash_Builder, "algorithm", "sha256");
      CT_JSON.Add_String (Hash_Builder, "value", To_String (Artifact_Hash));

      --  Build data object
      Data_Builder := CT_JSON.Create;
      CT_JSON.Add_String (Data_Builder, "hash", CT_JSON.To_JSON (Hash_Builder));

      --  Build spec object
      Spec_Builder := CT_JSON.Create;
      CT_JSON.Add_String (Spec_Builder, "signature", CT_JSON.To_JSON (Sig_Builder));
      CT_JSON.Add_String (Spec_Builder, "data", CT_JSON.To_JSON (Data_Builder));

      --  Build top-level request
      Request_Body := CT_JSON.Create;
      CT_JSON.Add_String (Request_Body, "kind", "hashedrekord");
      CT_JSON.Add_String (Request_Body, "apiVersion", "0.0.1");
      CT_JSON.Add_String (Request_Body, "spec", CT_JSON.To_JSON (Spec_Builder));

      Request_JSON := To_Unbounded_String (CT_JSON.To_JSON (Request_Body));

      --  POST to Rekor
      --  TODO: Create HTTP_Client_Config from Log_Client settings
      Response := Post (
         URL          => API_URL,
         Data         => To_String (Request_JSON),
         Content_Type => "application/json"
         --  Uses Default_Config (Timeout_Seconds => 30, Verify_TLS => True)
      );

      if not Response.Success then
         Result.Error := Network_Error;
         return Result;
      end if;

      if Response.Status_Code /= 201 then
         --  Rekor returns 201 Created on success
         if Response.Status_Code = 429 then
            Result.Error := Rate_Limited;
         elsif Response.Status_Code >= 500 then
            Result.Error := Server_Error;
         else
            Result.Error := Invalid_Entry;
         end if;
         return Result;
      end if;

      --  Parse response to extract entry data
      --  Response format: { "uuid": "...", "body": {...}, "logIndex": N, ... }
      declare
         Response_JSON : constant String := To_String (Response.Content);
         UUID_Str      : constant String := CT_JSON.Get_String_Field (Response_JSON, "uuid");
         Log_Index     : constant Integer := CT_JSON.Get_Integer_Field (Response_JSON, "logIndex");
         Int_Time      : constant Integer := CT_JSON.Get_Integer_Field (Response_JSON, "integratedTime");
      begin
         if UUID_Str'Length = 0 then
            Result.Error := Invalid_Entry;
            return Result;
         end if;

         --  Populate entry data
         Result.The_Entry.UUID := Entry_UUID (UUID_Str (UUID_Str'First .. UUID_Str'First + 79));
         Result.The_Entry.Log_Index := Unsigned_64 (Log_Index);
         Result.The_Entry.Kind := HashedRekord;
         Result.The_Entry.Body_Hash := Artifact_Hash;
         Result.The_Entry.Body_Hash_Algo := SHA256;
         Result.The_Entry.Signature := To_Unbounded_String (Signature);
         Result.The_Entry.Public_Key := To_Unbounded_String (Public_Key);
         Result.The_Entry.Raw_Entry := Response.Content;

         --  Build viewer URL
         Result.URL := To_Unbounded_String (Entry_URL (Client, Result.The_Entry.UUID));
         Result.Error := Success;
      end;

      return Result;
   end Upload_Signature;

   function Upload_Attestation
     (Client      : Log_Client;
      Attestation : String;
      Public_Key  : String) return Upload_Result
   is
      pragma Unreferenced (Client, Attestation, Public_Key);
      Result : Upload_Result;
   begin
      --  TODO: Implement intoto entry upload
      --
      --  API: POST /api/v1/log/entries
      --
      --  Request body (intoto type):
      --  {
      --    "kind": "intoto",
      --    "apiVersion": "0.0.2",
      --    "spec": {
      --      "content": {
      --        "envelope": "<DSSE envelope>",
      --        "hash": { "algorithm": "sha256", "value": "..." }
      --      },
      --      "publicKey": "<base64-pem-key>"
      --    }
      --  }

      Result.Error := Not_Implemented;
      return Result;
   end Upload_Attestation;

   function Upload_DSSE
     (Client     : Log_Client;
      Envelope   : String;
      Public_Key : String) return Upload_Result
   is
      pragma Unreferenced (Client, Envelope, Public_Key);
      Result : Upload_Result;
   begin
      --  TODO: Implement DSSE entry upload
      Result.Error := Not_Implemented;
      return Result;
   end Upload_DSSE;

   ---------------------------------------------------------------------------
   --  Entry Lookup
   ---------------------------------------------------------------------------

   function Lookup_By_UUID
     (Client : Log_Client;
      UUID   : Entry_UUID) return Lookup_Result
   is
      Result   : Lookup_Result;
      API_URL  : constant String := To_String (Client.Base_URL) & "/api/v1/log/entries/" & String (UUID);
      Response : HTTP_Response;
      Log_Entry_Data : Log_Entry;
   begin
      --  GET entry from Rekor
      Response := Get (URL => API_URL);

      if not Response.Success then
         Result.Error := Network_Error;
         return Result;
      end if;

      if Response.Status_Code = 404 then
         Result.Error := Entry_Not_Found;
         return Result;
      elsif Response.Status_Code /= 200 then
         if Response.Status_Code >= 500 then
            Result.Error := Server_Error;
         else
            Result.Error := Invalid_Entry;
         end if;
         return Result;
      end if;

      --  Parse response
      --  Rekor returns entries as: { "uuid": { "body": {...}, "integratedTime": N, ... } }
      declare
         Response_JSON : constant String := To_String (Response.Content);
         Log_Index     : constant Integer := CT_JSON.Get_Integer_Field (Response_JSON, "logIndex");
         Int_Time      : constant Integer := CT_JSON.Get_Integer_Field (Response_JSON, "integratedTime");
         Body_Str      : constant String := CT_JSON.Get_String_Field (Response_JSON, "body");
      begin
         Log_Entry_Data.UUID := UUID;
         Log_Entry_Data.Log_Index := Unsigned_64 (Log_Index);
         Log_Entry_Data.Kind := HashedRekord;  --  Default assumption
         Log_Entry_Data.Raw_Entry := Response.Content;

         --  TODO: Parse body to extract signature, hash, public key
         --  Body is base64-encoded JSON that needs decoding

         Result.Entries.Append (Log_Entry_Data);
         Result.Error := Success;
      end;

      return Result;
   end Lookup_By_UUID;

   function Lookup_By_Index
     (Client : Log_Client;
      Index  : Unsigned_64) return Lookup_Result
   is
      Result   : Lookup_Result;
      Index_Str : constant String := Unsigned_64'Image (Index);
      API_URL  : constant String := To_String (Client.Base_URL) &
                                     "/api/v1/log/entries?logIndex=" &
                                     Trim (Index_Str, Ada.Strings.Both);
      Response : HTTP_Response;
      Log_Entry_Data : Log_Entry;
   begin
      --  GET entry by index
      Response := Get (URL => API_URL);

      if not Response.Success then
         Result.Error := Network_Error;
         return Result;
      end if;

      if Response.Status_Code = 404 then
         Result.Error := Entry_Not_Found;
         return Result;
      elsif Response.Status_Code /= 200 then
         if Response.Status_Code >= 500 then
            Result.Error := Server_Error;
         else
            Result.Error := Invalid_Entry;
         end if;
         return Result;
      end if;

      --  Parse response (similar to Lookup_By_UUID)
      declare
         Response_JSON : constant String := To_String (Response.Content);
         UUID_Str      : constant String := CT_JSON.Get_String_Field (Response_JSON, "uuid");
      begin
         if UUID_Str'Length >= 80 then
            Log_Entry_Data.UUID := Entry_UUID (UUID_Str (UUID_Str'First .. UUID_Str'First + 79));
         end if;
         Log_Entry_Data.Log_Index := Index;
         Log_Entry_Data.Kind := HashedRekord;
         Log_Entry_Data.Raw_Entry := Response.Content;

         Result.Entries.Append (Log_Entry_Data);
         Result.Error := Success;
      end;

      return Result;
   end Lookup_By_Index;

   function Search_By_Hash
     (Client : Log_Client;
      Hash   : String;
      Algo   : Hash_Algorithm := SHA256) return Lookup_Result
   is
      Result       : Lookup_Result;
      API_URL      : constant String := To_String (Client.Base_URL) & "/api/v1/index/retrieve";
      Request_Body : CT_JSON.JSON_Builder;
      Algo_Str     : constant String := (case Algo is
                                          when SHA256 => "sha256",
                                          when SHA384 => "sha384",
                                          when SHA512 => "sha512",
                                          when Blake3 => "blake3");
      Hash_Value   : constant String := Algo_Str & ":" & Hash;
      Response     : HTTP_Response;
   begin
      --  Build request: { "hash": "sha256:..." }
      Request_Body := CT_JSON.Create;
      CT_JSON.Add_String (Request_Body, "hash", Hash_Value);

      --  POST to index/retrieve
      Response := Post (
         URL          => API_URL,
         Data         => CT_JSON.To_JSON (Request_Body),
         Content_Type => "application/json"
      );

      if not Response.Success then
         Result.Error := Network_Error;
         return Result;
      end if;

      if Response.Status_Code = 404 then
         Result.Error := Entry_Not_Found;
         return Result;
      elsif Response.Status_Code /= 200 then
         if Response.Status_Code >= 500 then
            Result.Error := Server_Error;
         else
            Result.Error := Invalid_Entry;
         end if;
         return Result;
      end if;

      --  Response contains UUIDs array
      --  Format: { "uuids": ["uuid1", "uuid2", ...] }
      --  For now, return success (full parsing requires array support in CT_JSON)
      Result.Error := Success;

      --  TODO: Parse UUIDs array and lookup each entry
      --  For MVP, consider this a successful search even if we don't populate entries

      return Result;
   end Search_By_Hash;

   function Search_By_Public_Key
     (Client     : Log_Client;
      Public_Key : String) return Lookup_Result
   is
      pragma Unreferenced (Client, Public_Key);
      Result : Lookup_Result;
   begin
      --  TODO: Implement public key search
      --
      --  API: POST /api/v1/index/retrieve
      --  Body: { "publicKey": { "format": "x509", "content": "<pem>" } }

      Result.Error := Not_Implemented;
      return Result;
   end Search_By_Public_Key;

   function Search_By_Email
     (Client : Log_Client;
      Email  : String) return Lookup_Result
   is
      pragma Unreferenced (Client, Email);
      Result : Lookup_Result;
   begin
      --  TODO: Implement email/identity search
      --
      --  API: POST /api/v1/index/retrieve
      --  Body: { "email": "<email>" }

      Result.Error := Not_Implemented;
      return Result;
   end Search_By_Email;

   ---------------------------------------------------------------------------
   --  Entry Verification
   ---------------------------------------------------------------------------

   function Verify_Entry
     (Client : Log_Client;
      E      : Log_Entry) return Verify_Result
   is
      Result : Verify_Result;
   begin
      --  Verify all components of the entry
      Result.Error := Success;

      --  1. Verify signature on entry body
      --  TODO: Integrate with Cerro_Crypto
      Result.Entry_Valid := False;  -- Not yet implemented

      --  2. Verify inclusion proof
      Result.Inclusion_Valid := Verify_Inclusion (Client, E);

      --  3. Verify SET
      Result.SET_Valid := Verify_SET (Client, E);

      if not Result.Entry_Valid or not Result.Inclusion_Valid or not Result.SET_Valid then
         if not Result.Entry_Valid then
            Result.Error := Signature_Invalid;
         elsif not Result.Inclusion_Valid then
            Result.Error := Proof_Invalid;
         else
            Result.Error := SET_Invalid;
         end if;
      end if;

      --  Currently stub - return not implemented
      Result.Error := Not_Implemented;
      return Result;
   end Verify_Entry;

   function Verify_Inclusion
     (Client : Log_Client;
      E      : Log_Entry) return Boolean
   is
      pragma Unreferenced (Client, E);
   begin
      --  TODO: Implement Merkle inclusion proof verification
      --
      --  Algorithm:
      --  1. Compute leaf hash from entry body
      --  2. Apply Merkle path hashes from proof
      --  3. Compare result to signed root hash
      --
      --  Reference: RFC 6962 Section 2.1

      return False;
   end Verify_Inclusion;

   function Verify_SET
     (Client : Log_Client;
      E      : Log_Entry) return Boolean
   is
      pragma Unreferenced (Client, E);
   begin
      --  TODO: Implement SET verification
      --
      --  SET contains:
      --  - Log ID (hash of log's public key)
      --  - Entry hash
      --  - Integrated timestamp
      --  - Signature by log's key
      --
      --  Verify signature using log's known public key

      return False;
   end Verify_SET;

   function Verify_Artifact
     (E        : Log_Entry;
      Artifact : String) return Boolean
   is
      pragma Unreferenced (Artifact);
   begin
      --  TODO: Implement artifact hash verification
      --
      --  1. Compute hash of artifact using E.Body_Hash_Algo
      --  2. Compare to E.Body_Hash

      if Length (E.Body_Hash) = 0 then
         return False;
      end if;

      return False;  -- Not yet implemented
   end Verify_Artifact;

   ---------------------------------------------------------------------------
   --  Log Consistency
   ---------------------------------------------------------------------------

   function Get_Log_Info (Client : Log_Client) return Tree_Info
   is
      Info     : Tree_Info;
      API_URL  : constant String := To_String (Client.Base_URL) & "/api/v1/log";
      Response : HTTP_Response;
   begin
      --  GET log info from Rekor
      Response := Get (URL => API_URL);

      if not Response.Success or Response.Status_Code /= 200 then
         Info.Tree_Size := 0;
         return Info;
      end if;

      --  Parse response
      declare
         Response_JSON : constant String := To_String (Response.Content);
         Root_Hash     : constant String := CT_JSON.Get_String_Field (Response_JSON, "rootHash");
         Tree_Size     : constant Integer := CT_JSON.Get_Integer_Field (Response_JSON, "treeSize");
         Signed_Tree   : constant String := CT_JSON.Get_String_Field (Response_JSON, "signedTreeHead");
      begin
         Info.Root_Hash := To_Unbounded_String (Root_Hash);
         Info.Tree_Size := Unsigned_64 (Tree_Size);
         Info.Signed_Root := To_Unbounded_String (Signed_Tree);
         Info.Timestamp := Ada.Calendar.Clock;
      end;

      return Info;
   end Get_Log_Info;

   function Verify_Consistency
     (Client    : Log_Client;
      Old_Size  : Unsigned_64;
      Old_Root  : String;
      New_Size  : Unsigned_64;
      New_Root  : String) return Boolean
   is
      pragma Unreferenced (Client, Old_Size, Old_Root, New_Size, New_Root);
   begin
      --  TODO: Implement consistency proof verification
      --
      --  API: GET /api/v1/log/proof?firstSize={old}&lastSize={new}
      --
      --  Returns Merkle consistency proof
      --  Verify per RFC 6962 Section 2.1.2

      return False;
   end Verify_Consistency;

   ---------------------------------------------------------------------------
   --  Offline Bundle Support
   ---------------------------------------------------------------------------

   function Create_Bundle
     (E          : Log_Entry;
      Signature  : String;
      Public_Key : String) return Verification_Bundle
   is
      Bundle : Verification_Bundle;
   begin
      Bundle.Media_Type := To_Unbounded_String (
         "application/vnd.dev.sigstore.bundle+json;version=0.1");
      Bundle.Signature := To_Unbounded_String (Signature);
      Bundle.Public_Key := To_Unbounded_String (Public_Key);
      Bundle.Artifact_Hash := E.Body_Hash;
      Bundle.Log_Entry := E.Raw_Entry;
      --  TODO: Serialize inclusion proof
      Bundle.Inclusion_Proof := To_Unbounded_String ("{}");
      Bundle.Timestamp_Proof := To_Unbounded_String ("");

      return Bundle;
   end Create_Bundle;

   function Bundle_To_Json (B : Verification_Bundle) return String is
   begin
      --  TODO: Implement proper JSON serialization per Sigstore bundle spec
      --
      --  Format: https://github.com/sigstore/protobuf-specs
      return "{""mediaType"":""" & To_String (B.Media_Type) & """}";
   end Bundle_To_Json;

   function Parse_Bundle (Json : String) return Verification_Bundle is
      pragma Unreferenced (Json);
      Bundle : Verification_Bundle;
   begin
      --  TODO: Implement JSON parsing
      return Bundle;
   end Parse_Bundle;

   function Verify_Bundle_Offline
     (B            : Verification_Bundle;
      Artifact     : String;
      Trusted_Root : String := "") return Verify_Result
   is
      pragma Unreferenced (B, Artifact, Trusted_Root);
      Result : Verify_Result;
   begin
      --  TODO: Implement offline bundle verification
      --
      --  Steps:
      --  1. Parse log entry from bundle
      --  2. Verify signature on artifact
      --  3. Verify inclusion proof (against embedded root or trusted root)
      --  4. Verify SET (against trusted log key)
      --  5. Verify artifact hash matches

      Result.Error := Not_Implemented;
      return Result;
   end Verify_Bundle_Offline;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Error_Message (E : Transparency_Error) return String is
   begin
      case E is
         when Success =>
            return "Operation completed successfully";
         when Not_Implemented =>
            return "Feature not yet implemented";
         when Network_Error =>
            return "Network connection to log failed";
         when Invalid_Entry =>
            return "Malformed entry data";
         when Entry_Not_Found =>
            return "Entry not found in log";
         when Proof_Invalid =>
            return "Inclusion proof verification failed";
         when SET_Invalid =>
            return "Signed Entry Timestamp verification failed";
         when Hash_Mismatch =>
            return "Artifact hash does not match entry";
         when Signature_Invalid =>
            return "Entry signature verification failed";
         when Log_Inconsistent =>
            return "Log consistency check failed";
         when Rate_Limited =>
            return "API rate limit exceeded";
         when Server_Error =>
            return "Transparency log server error";
      end case;
   end Error_Message;

   function Index_To_UUID (Index : Unsigned_64) return Entry_UUID is
      Hex_Chars : constant String := "0123456789abcdef";
      Result    : Entry_UUID := [others => '0'];
      Val       : Unsigned_64 := Index;
      Pos       : Natural := Entry_UUID'Last;
   begin
      --  Convert index to hex, right-aligned
      while Val > 0 and Pos >= Entry_UUID'First loop
         Result (Pos) := Hex_Chars (Natural (Val mod 16) + 1);
         Val := Val / 16;
         Pos := Pos - 1;
      end loop;

      return Result;
   end Index_To_UUID;

   function Entry_URL
     (Client : Log_Client;
      UUID   : Entry_UUID) return String
   is
   begin
      --  Return viewer URL for entry
      --  Rekor search: https://search.sigstore.dev/?uuid=...
      case Client.Provider is
         when Sigstore_Rekor =>
            return "https://search.sigstore.dev/?uuid=" & String (UUID);
         when Sigstore_Staging =>
            return "https://search.sigstore.dev/?uuid=" & String (UUID);
         when others =>
            return To_String (Client.Base_URL) & "/api/v1/log/entries/" & String (UUID);
      end case;
   end Entry_URL;

end CT_Transparency;
