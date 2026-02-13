-------------------------------------------------------------------------------
--  CT_HTTP - HTTP Client Wrapper for Cerro Torre
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Provides a thin wrapper around AWS.Client for HTTP operations needed by
--  CT_Registry (OCI Distribution Spec) and CT_Transparency (Rekor/tlog).
--
--  Features:
--    - GET/POST/PUT/DELETE/HEAD requests
--    - Bearer token and Basic authentication headers
--    - TLS certificate verification control
--    - Configurable timeouts
--    - Response handling (status, headers, body)
--
--  Security Considerations:
--    - TLS verification enabled by default (disable only for testing)
--    - Credentials should never be logged
--    - All sensitive data in headers must be protected
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package CT_HTTP
   with SPARK_Mode => Off  --  External library bindings
is
   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  HTTP methods supported
   type HTTP_Method is (GET, POST, PUT, DELETE, HEAD, PATCH);

   --  Authentication schemes
   type Auth_Scheme is
     (No_Auth,       --  No authentication
      Basic_Auth,    --  HTTP Basic Authentication (username:password)
      Bearer_Token); --  OAuth2 Bearer Token

   --  Authentication credentials
   type Auth_Credentials is record
      Scheme   : Auth_Scheme := No_Auth;
      Username : Unbounded_String;  --  For Basic_Auth
      Password : Unbounded_String;  --  For Basic_Auth
      Token    : Unbounded_String;  --  For Bearer_Token
   end record;

   --  No credentials constant for convenience
   No_Credentials : constant Auth_Credentials :=
     (Scheme   => No_Auth,
      Username => Null_Unbounded_String,
      Password => Null_Unbounded_String,
      Token    => Null_Unbounded_String);

   --  Header map type
   package Header_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   subtype Header_Map is Header_Maps.Map;

   ---------------------------------------------------------------------------
   --  Protocol Support (via curl)
   ---------------------------------------------------------------------------

   --  HTTP protocol version (curl --http1.0, --http1.1, --http2, --http3)
   --  Default: HTTP_Auto (let curl negotiate, RECOMMENDED)
   type HTTP_Version is
     (HTTP_Auto,      --  Let curl negotiate (default, RECOMMENDED)
      HTTP_1_0,       --  DEPRECATED: Force HTTP/1.0 (insecure, legacy only)
      HTTP_1_1,       --  Force HTTP/1.1 (compatible fallback)
      HTTP_2,         --  Force HTTP/2 (requires HTTPS, falls back to 1.1)
      HTTP_2_Prior,   --  HTTP/2 with prior knowledge (no upgrade)
      HTTP_3);        --  Force HTTP/3 over QUIC (UDP, modern)

   --  Proxy protocol
   type Proxy_Protocol is
     (No_Proxy,
      HTTP_Proxy,     --  HTTP/HTTPS proxy
      SOCKS4_Proxy,   --  SOCKS4 proxy
      SOCKS4A_Proxy,  --  SOCKS4A (DNS via proxy)
      SOCKS5_Proxy,   --  SOCKS5 proxy
      SOCKS5H_Proxy); --  SOCKS5 with hostname resolution

   --  Proxy configuration
   type Proxy_Config is record
      Protocol : Proxy_Protocol := No_Proxy;
      Host     : Unbounded_String;
      Port     : Natural := 0;
      Username : Unbounded_String;  --  Optional auth
      Password : Unbounded_String;
   end record;

   No_Proxy_Config : constant Proxy_Config :=
     (Protocol => No_Proxy,
      Host     => Null_Unbounded_String,
      Port     => 0,
      Username => Null_Unbounded_String,
      Password => Null_Unbounded_String);

   --  DNS/TLS security options
   type DNS_Security is record
      --  DANE (DNS-based Authentication of Named Entities) / TLSA
      --  Verifies TLS certificates against DNSSEC-signed DNS records
      Enable_DANE       : Boolean := True;   --  Use TLSA records if available (curl 7.52+)
      Require_DANE      : Boolean := False;  --  FAIL if TLSA record exists but doesn't match

      --  DNS-over-HTTPS (DoH) - encrypted DNS queries
      --  NOTE: Requires curl 7.62+ and system DoH resolver configuration
      DoH_URL           : Unbounded_String;  --  e.g., "https://cloudflare-dns.com/dns-query"

      --  Oblivious DNS-over-HTTPS (ODoH / ODNS) - privacy-enhanced DoH
      --  RFC 9230: Adds proxy layer so resolver doesn't see client IP
      --  NOTE: Requires curl 7.73+ and ODoH-enabled resolver
      ODoH_Target_URL   : Unbounded_String;  --  e.g., "https://odoh.cloudflare-dns.com/dns-query"
      ODoH_Proxy_URL    : Unbounded_String;  --  e.g., "https://odoh1.surfdomeinen.nl/proxy"

      --  EDNS (Extension Mechanisms for DNS)
      --  NOTE: Automatic via system resolver - no curl configuration needed
      --  Configure in /etc/resolv.conf with "options edns0"
   end record;

   No_DNS_Security : constant DNS_Security :=
     (Enable_DANE     => False,
      Require_DANE    => False,
      DoH_URL         => Null_Unbounded_String,
      ODoH_Target_URL => Null_Unbounded_String,
      ODoH_Proxy_URL  => Null_Unbounded_String);

   Default_DNS_Security : constant DNS_Security :=
     (Enable_DANE     => True,   --  Opportunistic DANE
      Require_DANE    => False,  --  Don't fail on DANE absence
      DoH_URL         => Null_Unbounded_String,
      ODoH_Target_URL => Null_Unbounded_String,  --  Optional ODNS
      ODoH_Proxy_URL  => Null_Unbounded_String);

   --  Client configuration
   type HTTP_Client_Config is record
      User_Agent        : Unbounded_String;
      Timeout_Seconds   : Positive := 30;
      Verify_TLS        : Boolean := True;  --  DEPRECATED if False: INSECURE!
      Follow_Redirects  : Boolean := True;
      Max_Redirects     : Positive := 5;

      --  Protocol options
      Protocol_Version  : HTTP_Version := HTTP_Auto;  --  Let curl negotiate
      Enable_ECH        : Boolean := True;   --  Encrypted Client Hello (curl 8.2+)
      Enable_Alt_Svc    : Boolean := True;   --  Alt-Svc for HTTP/3 upgrade

      --  DNS/TLS security (DANE/TLSA, DoH, EDNS)
      DNS_Sec           : DNS_Security := Default_DNS_Security;  --  Renamed field

      --  Proxy support
      Proxy             : Proxy_Config := No_Proxy_Config;

      --  Connection options
      TCP_Keepalive     : Boolean := True;
      TCP_Nodelay       : Boolean := True;   --  Disable Nagle's algorithm
      IPv4_Only         : Boolean := False;  --  Force IPv4
      IPv6_Only         : Boolean := False;  --  Force IPv6

      --  Debug options
      Debug_Logging     : Boolean := False;  --  Enable HTTP request/response logging
   end record;
   --  SECURITY: TLS verification must be enabled in production
   --  Dynamic predicate removed - checked at runtime in HTTP operations instead

   --  Default configuration (secure by default)
   Default_Config : constant HTTP_Client_Config :=
     (User_Agent        => To_Unbounded_String ("cerro-torre/0.2"),
      Timeout_Seconds   => 30,
      Verify_TLS        => True,  --  ALWAYS True for security
      Follow_Redirects  => True,
      Max_Redirects     => 5,
      Protocol_Version  => HTTP_Auto,  --  Let curl negotiate best version
      Enable_ECH        => False,      --  Disabled for MVP (requires modern curl)
      Enable_Alt_Svc    => True,       --  Allow HTTP/3 upgrade
      DNS_Sec           => Default_DNS_Security,  --  DANE enabled opportunistically
      Proxy             => No_Proxy_Config,
      TCP_Keepalive     => True,
      TCP_Nodelay       => True,
      IPv4_Only         => False,
      IPv6_Only         => False,
      Debug_Logging     => False);     --  Disabled by default

   ---------------------------------------------------------------------------
   --  Security Notes
   ---------------------------------------------------------------------------

   --  EDNS (Extension Mechanisms for DNS):
   --    Automatic via system resolver. Configure in /etc/resolv.conf:
   --      options edns0 trust-ad
   --
   --  DANE/TLSA (DNS-based Certificate Authentication):
   --    Requires DNSSEC-enabled resolver and TLSA records in DNS.
   --    curl verifies TLS certificates against TLSA records automatically
   --    when Enable_DANE is True.
   --
   --  DNS-over-HTTPS (DoH):
   --    Encrypts DNS queries. Set DoH_URL to a trusted resolver:
   --      https://cloudflare-dns.com/dns-query  (Cloudflare)
   --      https://dns.google/dns-query          (Google)
   --      https://dns.quad9.net/dns-query       (Quad9)
   --
   --  Oblivious DNS-over-HTTPS (ODoH / ODNS):
   --    Privacy-enhanced DoH per RFC 9230. Requires TWO URLs:
   --      1. ODoH_Target_URL - The actual DNS resolver
   --      2. ODoH_Proxy_URL  - Proxy that hides your IP from resolver
   --    Example configuration:
   --      ODoH_Target_URL: https://odoh.cloudflare-dns.com/dns-query
   --      ODoH_Proxy_URL:  https://odoh1.surfdomeinen.nl/proxy
   --    Benefits:
   --      - Resolver doesn't see client IP (proxy hides it)
   --      - Proxy doesn't see DNS queries (encrypted for target)
   --      - Stronger privacy than regular DoH
   --    Note: curl 7.73+ required, both URLs must be set
   --
   --  Encrypted Client Hello (ECH):
   --    Hides SNI in TLS handshake (privacy). Requires curl 8.2+
   --    and server support. Enabled by default.
   --
   --  DEPRECATED/INSECURE OPTIONS:
   --    - Verify_TLS => False     (MitM vulnerable!)
   --    - HTTP_1_0                (obsolete, limited features)
   --    - Cleartext HTTP          (use HTTPS!)
   --
   --  RECOMMENDED SECURE CONFIG:
   --    - Use Default_Config (secure by default)
   --    - Set DoH_URL for DNS privacy
   --    - Require_DANE => True for high-security environments

   ---------------------------------------------------------------------------
   --  HTTP Response
   ---------------------------------------------------------------------------

   --  HTTP status code categories (0 = no response)
   subtype Status_Code is Natural range 0 .. 599;

   function Is_Success (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Success'Result = (Code in 200 .. 299);

   function Is_Redirect (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Redirect'Result = (Code in 300 .. 399);

   function Is_Client_Error (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Client_Error'Result = (Code in 400 .. 499);

   function Is_Server_Error (Code : Status_Code) return Boolean
   with Global => null,
        Post   => Is_Server_Error'Result = (Code in 500 .. 599);

   --  HTTP response structure
   type HTTP_Response is record
      Status_Code   : CT_HTTP.Status_Code := 0;
      Status_Reason : Unbounded_String;
      Headers       : Header_Map;
      Content       : Unbounded_String;  --  Response body content (renamed from Body)
      Error_Message : Unbounded_String;  --  Set if request failed
      Success       : Boolean := False;  --  True if request completed
   end record;

   --  Create empty/error response
   function Empty_Response return HTTP_Response
   with Global => null;

   function Error_Response (Message : String) return HTTP_Response
   with Global => null;

   ---------------------------------------------------------------------------
   --  Header Utilities
   ---------------------------------------------------------------------------

   --  Get header value (case-insensitive lookup)
   function Get_Header
     (Response : HTTP_Response;
      Name     : String) return String
   with Global => null;

   --  Check if header exists
   function Has_Header
     (Response : HTTP_Response;
      Name     : String) return Boolean
   with Global => null;

   --  Common header names
   Content_Type_Header       : constant String := "Content-Type";
   Content_Length_Header     : constant String := "Content-Length";
   Authorization_Header      : constant String := "Authorization";
   Accept_Header             : constant String := "Accept";
   User_Agent_Header         : constant String := "User-Agent";
   Location_Header           : constant String := "Location";
   WWW_Authenticate_Header   : constant String := "WWW-Authenticate";
   Docker_Content_Digest     : constant String := "Docker-Content-Digest";

   ---------------------------------------------------------------------------
   --  HTTP Request Functions
   ---------------------------------------------------------------------------

   --  Perform GET request
   function Get
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform POST request with body
   function Post
     (URL          : String;
      Data         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform PUT request with body
   function Put
     (URL          : String;
      Data         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform DELETE request
   function Delete
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform HEAD request (response body will be empty)
   function Head
     (URL     : String;
      Config  : HTTP_Client_Config := Default_Config;
      Auth    : Auth_Credentials := No_Credentials;
      Headers : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   --  Perform PATCH request with body
   function Patch
     (URL          : String;
      Data         : String;
      Content_Type : String := "application/json";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and URL'Length <= 8192;

   ---------------------------------------------------------------------------
   --  Streaming/Large Body Support
   ---------------------------------------------------------------------------

   --  Download to file (for large blobs)
   function Download_To_File
     (URL         : String;
      Output_Path : String;
      Config      : HTTP_Client_Config := Default_Config;
      Auth        : Auth_Credentials := No_Credentials;
      Headers     : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and Output_Path'Length > 0;

   --  Upload from file (for large blobs)
   function Upload_From_File
     (URL          : String;
      Input_Path   : String;
      Content_Type : String := "application/octet-stream";
      Config       : HTTP_Client_Config := Default_Config;
      Auth         : Auth_Credentials := No_Credentials;
      Headers      : Header_Map := Header_Maps.Empty_Map) return HTTP_Response
   with Global => null,
        Pre    => URL'Length > 0 and Input_Path'Length > 0;

   ---------------------------------------------------------------------------
   --  Authentication Helpers
   ---------------------------------------------------------------------------

   --  Create Basic auth credentials
   function Make_Basic_Auth
     (Username : String;
      Password : String) return Auth_Credentials
   with Global => null;

   --  Create Bearer token credentials
   function Make_Bearer_Auth (Token : String) return Auth_Credentials
   with Global => null;

   --  Parse WWW-Authenticate header (for Docker Registry v2 auth flow)
   --  Returns realm, service, and scope from the header
   type WWW_Auth_Challenge is record
      Realm   : Unbounded_String;
      Service : Unbounded_String;
      Scope   : Unbounded_String;
   end record;

   function Parse_WWW_Authenticate (Header_Value : String) return WWW_Auth_Challenge
   with Global => null;

   ---------------------------------------------------------------------------
   --  URL Utilities
   ---------------------------------------------------------------------------

   --  URL encode a string (for query parameters)
   function URL_Encode (S : String) return String
   with Global => null;

   --  Build URL with query parameters
   function Build_URL
     (Base_URL : String;
      Path     : String;
      Query    : Header_Map := Header_Maps.Empty_Map) return String
   with Global => null;

   --  Join URL path segments
   function Join_Path (Base : String; Path : String) return String
   with Global => null;

end CT_HTTP;
