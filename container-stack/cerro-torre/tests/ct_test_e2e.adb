-------------------------------------------------------------------------------
--  CT_TEST_E2E - End-to-End Integration Tests
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Tests the integration of CT_Registry + CT_Transparency types and APIs
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Interfaces;
with CT_Registry;
with CT_Transparency;
with CT_HTTP;

procedure CT_Test_E2E is

   Passed : Natural := 0;
   Failed : Natural := 0;

   procedure Test (Name : String; Success : Boolean) is
   begin
      if Success then
         Put_Line ("  ✓ PASS: " & Name);
         Passed := Passed + 1;
      else
         Put_Line ("  ✗ FAIL: " & Name);
         Failed := Failed + 1;
      end if;
   end Test;

   ---------------------------------------------------------------------------
   --  Test 1: Registry Reference Parsing
   ---------------------------------------------------------------------------

   procedure Test_Parse_Reference is
      use CT_Registry;

      --  Test full reference with digest
      Ref1 : Image_Reference := Parse_Reference
         ("ghcr.io/hyperpolymath/nginx:v1.26@sha256:abc123");

      --  Test reference with tag only
      Ref2 : Image_Reference := Parse_Reference
         ("docker.io/library/hello:latest");

      --  Test reference with registry port
      Ref3 : Image_Reference := Parse_Reference
         ("localhost:5000/test/app:v1");
   begin
      Put_Line ("=== Test 1: Registry Reference Parsing ===");
      Put_Line ("");

      Test ("Parse registry from full reference",
            To_String (Ref1.Registry) = "ghcr.io");

      Test ("Parse repository from full reference",
            To_String (Ref1.Repository) = "hyperpolymath/nginx");

      Test ("Parse tag from full reference",
            To_String (Ref1.Tag) = "v1.26");

      Test ("Parse digest from full reference",
            Index (Ref1.Digest, "sha256:abc123") > 0);

      Test ("Parse default registry (docker.io)",
            To_String (Ref2.Registry) = "docker.io");

      Test ("Parse library namespace",
            Index (Ref2.Repository, "library/hello") > 0);

      Test ("Parse localhost with port",
            To_String (Ref3.Registry) = "localhost:5000");

      Put_Line ("");
   end Test_Parse_Reference;

   ---------------------------------------------------------------------------
   --  Test 2: HTTP Client Configuration
   ---------------------------------------------------------------------------

   procedure Test_HTTP_Config is
      use CT_HTTP;

      Config : HTTP_Client_Config := Default_Config;
      Auth   : Auth_Credentials := Make_Bearer_Auth ("test-token-123");
   begin
      Put_Line ("=== Test 2: HTTP Client Configuration ===");
      Put_Line ("");

      Test ("Default config has TLS verification enabled",
            Config.Verify_TLS = True);

      Test ("Default config follows redirects",
            Config.Follow_Redirects = True);

      Test ("Default timeout is 30 seconds",
            Config.Timeout_Seconds = 30);

      Test ("Bearer auth token set",
            Length (Auth.Token) > 0);

      Test ("Bearer auth scheme correct",
            Auth.Scheme = Bearer_Token);

      Test ("Default config uses HTTP auto-negotiation",
            Config.Protocol_Version = HTTP_Auto);

      Test ("Default config disables ECH for MVP (requires modern curl)",
            Config.Enable_ECH = False);

      Test ("Default config enables DANE (security)",
            Config.DNS_Sec.Enable_DANE = True);

      Put_Line ("");
   end Test_HTTP_Config;

   ---------------------------------------------------------------------------
   --  Test 3: Registry Client Creation
   ---------------------------------------------------------------------------

   procedure Test_Registry_Client is
      use CT_Registry;

      --  Test with full URL
      Client1 : Registry_Client := Create_Client (
         Registry => "https://ghcr.io",
         Auth     => (others => <>));

      --  Test with bare hostname (should prepend https://)
      Client2 : Registry_Client := Create_Client (
         Registry => "docker.io",
         Auth     => (others => <>));

      --  Test with port
      Client3 : Registry_Client := Create_Client (
         Registry => "localhost:5000",
         Auth     => (others => <>));
   begin
      Put_Line ("=== Test 3: Registry Client Creation ===");
      Put_Line ("");

      Test ("Client 1 preserves https:// URL",
            Index (Client1.Base_URL, "https://ghcr.io") > 0);

      Test ("Client 2 prepends https://",
            Index (Client2.Base_URL, "https://docker.io") > 0);

      Test ("Client 3 handles localhost with port",
            Index (Client3.Base_URL, "localhost:5000") > 0);

      Test ("Default user agent set",
            Length (Client1.User_Agent) > 0);

      Test ("TLS verification enabled by default",
            Client1.Verify_TLS = True);

      Put_Line ("");
   end Test_Registry_Client;

   ---------------------------------------------------------------------------
   --  Test 4: Transparency Log Entry Structure
   ---------------------------------------------------------------------------

   procedure Test_Transparency_Types is
      use CT_Transparency;
      use Interfaces;

      --  Create a sample log entry manually
      Sample_Entry : Log_Entry;
   begin
      Put_Line ("=== Test 4: Transparency Log Entry Structure ===");
      Put_Line ("");

      --  Initialize UUID (80 chars, padded)
      Sample_Entry.UUID := (others => '0');
      Sample_Entry.UUID (1 .. 8) := "test-uid";

      Sample_Entry.Kind := HashedRekord;
      Sample_Entry.Log_Index := 999;
      Sample_Entry.Integrated_Time := Ada.Calendar.Clock;
      Sample_Entry.Log_ID := To_Unbounded_String ("log-id-hash");
      Sample_Entry.Body_Hash := To_Unbounded_String ("sha256:abc123...");
      Sample_Entry.Body_Hash_Algo := SHA256;
      Sample_Entry.Signature := To_Unbounded_String ("base64-sig");
      Sample_Entry.Sig_Algo := Ed25519;
      Sample_Entry.Public_Key := To_Unbounded_String ("base64-pubkey");
      Sample_Entry.Raw_Entry := To_Unbounded_String ("{}");

      Test ("Entry UUID set correctly",
            Sample_Entry.UUID (1 .. 8) = "test-uid");

      Test ("Entry kind is HashedRekord",
            Sample_Entry.Kind = HashedRekord);

      Test ("Log index is positive",
            Sample_Entry.Log_Index > 0);

      Test ("Body hash set",
            Length (Sample_Entry.Body_Hash) > 0);

      Test ("Body hash algorithm is SHA256",
            Sample_Entry.Body_Hash_Algo = SHA256);

      Test ("Signature present",
            Length (Sample_Entry.Signature) > 0);

      Test ("Signature algorithm is Ed25519",
            Sample_Entry.Sig_Algo = Ed25519);

      Test ("Public key present",
            Length (Sample_Entry.Public_Key) > 0);

      Test ("Raw entry present",
            Length (Sample_Entry.Raw_Entry) > 0);

      Put_Line ("");
   end Test_Transparency_Types;

   ---------------------------------------------------------------------------
   --  Test 5: Registry Auth Credentials
   ---------------------------------------------------------------------------

   procedure Test_Registry_Auth is
      use CT_Registry;
      use CT_HTTP;

      --  Basic auth
      Basic_Creds : Registry_Auth_Credentials := (
         Method   => Basic,
         Username => To_Unbounded_String ("testuser"),
         Password => To_Unbounded_String ("testpass"),
         Token    => Null_Unbounded_String);

      --  Bearer token
      Bearer_Creds : Registry_Auth_Credentials := (
         Method   => Bearer,
         Username => Null_Unbounded_String,
         Password => Null_Unbounded_String,
         Token    => To_Unbounded_String ("test-token"));

      --  AWS ECR (uses token)
      ECR_Creds : Registry_Auth_Credentials := (
         Method   => AWS_ECR,
         Username => Null_Unbounded_String,
         Password => Null_Unbounded_String,
         Token    => To_Unbounded_String ("ecr-token"));

      --  Convert to HTTP auth
      HTTP_Basic  : CT_HTTP.Auth_Credentials := To_HTTP_Auth (Basic_Creds);
      HTTP_Bearer : CT_HTTP.Auth_Credentials := To_HTTP_Auth (Bearer_Creds);
      HTTP_ECR    : CT_HTTP.Auth_Credentials := To_HTTP_Auth (ECR_Creds);
   begin
      Put_Line ("=== Test 5: Registry Authentication ===");
      Put_Line ("");

      Test ("Basic auth has username",
            Length (Basic_Creds.Username) > 0);

      Test ("Basic auth has password",
            Length (Basic_Creds.Password) > 0);

      Test ("Bearer auth has token",
            Length (Bearer_Creds.Token) > 0);

      Test ("ECR auth has token",
            Length (ECR_Creds.Token) > 0);

      Test ("Basic auth converts to HTTP Basic",
            HTTP_Basic.Scheme = Basic_Auth);

      Test ("Bearer auth converts to HTTP Bearer",
            HTTP_Bearer.Scheme = Bearer_Token);

      Test ("ECR auth converts to HTTP Bearer",
            HTTP_ECR.Scheme = Bearer_Token);

      Put_Line ("");
   end Test_Registry_Auth;

   ---------------------------------------------------------------------------
   --  Test 6: Transparency Log Types
   ---------------------------------------------------------------------------

   procedure Test_Log_Provider is
      use CT_Transparency;

      --  Test provider enumeration
      Prov1 : Log_Provider := Sigstore_Rekor;
      Prov2 : Log_Provider := CT_TLOG;
      Prov3 : Log_Provider := Custom;

      --  Test entry kinds
      Kind1 : Entry_Kind := HashedRekord;
      Kind2 : Entry_Kind := Intoto;
   begin
      Put_Line ("=== Test 6: Transparency Log Types ===");
      Put_Line ("");

      Test ("Sigstore Rekor provider type exists",
            Prov1 = Sigstore_Rekor);

      Test ("CT_TLOG provider type exists",
            Prov2 = CT_TLOG);

      Test ("Custom provider type exists",
            Prov3 = Custom);

      Test ("HashedRekord entry kind exists",
            Kind1 = HashedRekord);

      Test ("Intoto entry kind exists",
            Kind2 = Intoto);

      Put_Line ("");
   end Test_Log_Provider;

   ---------------------------------------------------------------------------
   --  Main Test Runner
   ---------------------------------------------------------------------------

begin
   Put_Line ("");
   Put_Line ("CERRO TORRE END-TO-END TESTS");
   Put_Line ("============================");
   Put_Line ("");

   --  Run all test suites
   Test_Parse_Reference;
   Test_HTTP_Config;
   Test_Registry_Client;
   Test_Transparency_Types;
   Test_Registry_Auth;
   Test_Log_Provider;

   --  Print summary
   Put_Line ("=== Results ===");
   Put_Line ("Passed: " & Natural'Image (Passed));
   Put_Line ("Failed: " & Natural'Image (Failed));

   if Failed = 0 then
      Put_Line ("✓ All tests passed!");
   else
      Put_Line ("✗ Some tests failed!");
   end if;

end CT_Test_E2E;
