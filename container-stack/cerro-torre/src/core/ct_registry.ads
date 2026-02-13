-------------------------------------------------------------------------------
--  CT_Registry - OCI Distribution Specification Client
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Implements the OCI Distribution Specification for interacting with
--  container registries. Supports:
--    - Token-based authentication (bearer tokens)
--    - Manifest push/pull
--    - Blob push/pull (content-addressable storage)
--    - Tag listing and management
--
--  References:
--    - OCI Distribution Spec: https://github.com/opencontainers/distribution-spec
--    - Docker Registry HTTP API V2
--
--  Security Considerations:
--    - All connections should use TLS (HTTPS)
--    - Credentials are handled through environment variables or secure storage
--    - Digest verification on all blob downloads
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Interfaces;
with CT_HTTP;

package CT_Registry
   with SPARK_Mode => On
is
   use Interfaces;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Registry reference (e.g., "ghcr.io/hyperpolymath/hello:v1.0")
   type Image_Reference is record
      Registry   : Unbounded_String;  --  e.g., "ghcr.io"
      Repository : Unbounded_String;  --  e.g., "hyperpolymath/hello"
      Tag        : Unbounded_String;  --  e.g., "v1.0" or ""
      Digest     : Unbounded_String;  --  e.g., "sha256:abc..." or ""
   end record;

   --  Registry-specific authentication method (high-level)
   type Registry_Auth_Method is
     (None,            --  No authentication
      Basic,           --  HTTP Basic Auth
      Bearer,          --  OAuth2 Bearer Token
      AWS_ECR,         --  AWS ECR token exchange
      GCP_GCR,         --  GCP Artifact Registry
      Azure_ACR);      --  Azure Container Registry

   --  Registry authentication credentials (extends CT_HTTP.Auth_Credentials concept)
   type Registry_Auth_Credentials is record
      Method   : Registry_Auth_Method := None;
      Username : Unbounded_String;
      Password : Unbounded_String;  --  Or token for Bearer
      Token    : Unbounded_String;  --  Exchanged bearer token
   end record;

   --  Convert registry auth to HTTP auth for making requests
   function To_HTTP_Auth (Reg_Auth : Registry_Auth_Credentials) return CT_HTTP.Auth_Credentials
   with Global => null;

   --  Media types for manifests
   OCI_Manifest_V1        : constant String := "application/vnd.oci.image.manifest.v1+json";
   OCI_Index_V1           : constant String := "application/vnd.oci.image.index.v1+json";
   Docker_Manifest_V2     : constant String := "application/vnd.docker.distribution.manifest.v2+json";
   Docker_Manifest_List   : constant String := "application/vnd.docker.distribution.manifest.list.v2+json";

   --  Media types for blobs
   OCI_Layer_Gzip         : constant String := "application/vnd.oci.image.layer.v1.tar+gzip";
   OCI_Layer_Zstd         : constant String := "application/vnd.oci.image.layer.v1.tar+zstd";
   OCI_Config_V1          : constant String := "application/vnd.oci.image.config.v1+json";

   --  Signature media types (Sigstore/Cosign)
   Cosign_Signature       : constant String := "application/vnd.dev.cosign.simplesigning.v1+json";
   Cosign_Attestation     : constant String := "application/vnd.dsse.envelope.v1+json";

   --  Blob descriptor (content-addressable)
   type Blob_Descriptor is record
      Media_Type : Unbounded_String;
      Digest     : Unbounded_String;  --  e.g., "sha256:abc123..."
      Size       : Unsigned_64 := 0;
      Annotations : Unbounded_String;  --  JSON object string
   end record;

   package Blob_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Blob_Descriptor);

   subtype Blob_List is Blob_Vectors.Vector;

   --  OCI Image Manifest
   type OCI_Manifest is record
      Schema_Version : Positive := 2;
      Media_Type     : Unbounded_String;
      Config         : Blob_Descriptor;
      Layers         : Blob_List;
      Annotations    : Unbounded_String;  --  JSON object string
   end record;

   --  Tag list response
   package Tag_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);

   subtype Tag_List is Tag_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  Operation Results
   ---------------------------------------------------------------------------

   type Registry_Error is
     (Success,
      Not_Implemented,        --  Feature not yet implemented
      Network_Error,          --  Connection failed
      Auth_Required,          --  Authentication needed
      Auth_Failed,            --  Invalid credentials
      Not_Found,              --  Manifest/blob not found (404)
      Forbidden,              --  Access denied (403)
      Digest_Mismatch,        --  Downloaded content hash mismatch
      Invalid_Manifest,       --  Malformed manifest JSON
      Rate_Limited,           --  Registry rate limit (429)
      Server_Error,           --  Registry server error (5xx)
      Push_Failed,            --  Failed to upload
      Unsupported_Media_Type, --  Registry doesn't support media type
      Timeout);               --  Request timeout

   type Pull_Result is record
      Error    : Registry_Error := Not_Implemented;
      Manifest : OCI_Manifest;
      Raw_Json : Unbounded_String;  --  Original JSON for signature verification
      Digest   : Unbounded_String;  --  Manifest digest
   end record;

   type Push_Result is record
      Error  : Registry_Error := Not_Implemented;
      Digest : Unbounded_String;  --  Resulting digest
      URL    : Unbounded_String;  --  Full URL of pushed content
   end record;

   type Blob_Result is record
      Error    : Registry_Error := Not_Implemented;
      Content  : Unbounded_String;  --  For small blobs; empty for streamed
      Size     : Unsigned_64 := 0;
      Digest   : Unbounded_String;
   end record;

   type Tags_Result is record
      Error : Registry_Error := Not_Implemented;
      Tags  : Tag_List;
   end record;

   ---------------------------------------------------------------------------
   --  Registry Client
   ---------------------------------------------------------------------------

   type Registry_Client is record
      Base_URL      : Unbounded_String;  --  e.g., "https://ghcr.io"
      Auth          : Registry_Auth_Credentials;
      User_Agent    : Unbounded_String;  --  Client identifier
      Timeout_Ms    : Positive := 30_000;  --  Request timeout
      Verify_TLS    : Boolean := True;    --  Verify TLS certificates
      Debug_Logging : Boolean := False;   --  Enable HTTP debug logging
   end record;

   --  Default client configuration
   Default_User_Agent : constant String := "cerro-torre/0.2";

   function Create_Client
     (Registry : String;
      Auth     : Registry_Auth_Credentials := (others => <>)) return Registry_Client
   with Global => null,
        Pre    => Registry'Length > 0 and Registry'Length <= 256;

   ---------------------------------------------------------------------------
   --  Authentication
   ---------------------------------------------------------------------------

   --  Exchange credentials for bearer token (Docker Registry v2 auth)
   function Authenticate
     (Client     : in out Registry_Client;
      Repository : String;
      Actions    : String := "pull") return Registry_Error
   with SPARK_Mode => Off,  --  SPARK doesn't allow functions with in out parameters
        Global => null,
        Pre    => Repository'Length > 0;
   --  Actions: "pull", "push", or "pull,push"

   --  Check if client has valid authentication
   function Is_Authenticated (Client : Registry_Client) return Boolean
   with Global => null;

   ---------------------------------------------------------------------------
   --  Manifest Operations
   ---------------------------------------------------------------------------

   --  Pull (GET) manifest by tag or digest
   function Pull_Manifest
     (Client     : Registry_Client;
      Repository : String;
      Reference  : String) return Pull_Result
   with Global => null,
        Pre    => Repository'Length > 0 and Reference'Length > 0;
   --  Reference: tag (e.g., "v1.0") or digest (e.g., "sha256:abc...")

   --  Push (PUT) manifest
   function Push_Manifest
     (Client       : Registry_Client;
      Repository   : String;
      Tag          : String;
      Manifest     : OCI_Manifest;
      Manifest_Json : String := "") return Push_Result
   with Global => null,
        Pre    => Repository'Length > 0 and Tag'Length > 0;
   --  If Manifest_Json is empty, serializes Manifest to JSON

   --  Check if manifest exists (HEAD request)
   function Manifest_Exists
     (Client     : Registry_Client;
      Repository : String;
      Reference  : String) return Boolean
   with Global => null;

   --  Delete manifest
   function Delete_Manifest
     (Client     : Registry_Client;
      Repository : String;
      Digest     : String) return Registry_Error
   with Global => null,
        Pre    => Digest'Length > 7;  --  "sha256:" minimum

   ---------------------------------------------------------------------------
   --  Blob Operations
   ---------------------------------------------------------------------------

   --  Pull (GET) blob by digest
   function Pull_Blob
     (Client      : Registry_Client;
      Repository  : String;
      Digest      : String;
      Output_Path : String := "") return Blob_Result
   with Global => null,
        Pre    => Repository'Length > 0 and Digest'Length > 7;
   --  If Output_Path is provided, streams to file instead of memory

   --  Push blob with single POST (monolithic upload)
   function Push_Blob
     (Client     : Registry_Client;
      Repository : String;
      Content    : String;
      Media_Type : String := OCI_Layer_Gzip) return Push_Result
   with Global => null,
        Pre    => Repository'Length > 0;

   --  Push blob from file (chunked upload for large files)
   function Push_Blob_From_File
     (Client      : Registry_Client;
      Repository  : String;
      File_Path   : String;
      Media_Type  : String := OCI_Layer_Gzip;
      Chunk_Size  : Positive := 5_242_880) return Push_Result  --  5MB chunks
   with Global => null,
        Pre    => Repository'Length > 0 and File_Path'Length > 0;

   --  Check if blob exists (HEAD request)
   function Blob_Exists
     (Client     : Registry_Client;
      Repository : String;
      Digest     : String) return Boolean
   with Global => null;

   --  Mount blob from another repository (cross-repo mount)
   function Mount_Blob
     (Client          : Registry_Client;
      Target_Repo     : String;
      Source_Repo     : String;
      Digest          : String) return Registry_Error
   with Global => null,
        Pre    => Target_Repo'Length > 0 and Source_Repo'Length > 0;

   ---------------------------------------------------------------------------
   --  Tag Operations
   ---------------------------------------------------------------------------

   --  List all tags for a repository
   function List_Tags
     (Client     : Registry_Client;
      Repository : String;
      Page_Size  : Positive := 100) return Tags_Result
   with Global => null,
        Pre    => Repository'Length > 0;

   ---------------------------------------------------------------------------
   --  Image Reference Parsing
   ---------------------------------------------------------------------------

   --  Parse image reference string (e.g., "ghcr.io/user/repo:tag@sha256:...")
   function Parse_Reference (Ref : String) return Image_Reference
   with Global => null,
        Pre    => Ref'Length > 0 and Ref'Length <= 512;

   --  Convert image reference to string
   function To_String (Ref : Image_Reference) return String
   with Global => null;

   --  Get default registry for unqualified images
   Default_Registry : constant String := "docker.io";
   Default_Tag      : constant String := "latest";

   ---------------------------------------------------------------------------
   --  Manifest Serialization
   ---------------------------------------------------------------------------

   --  Serialize OCI manifest to JSON (canonical format for signing)
   function Manifest_To_Json (M : OCI_Manifest) return String
   with Global => null;

   --  Parse OCI manifest from JSON
   function Parse_Manifest (Json : String) return Pull_Result
   with Global => null;

   --  Calculate manifest digest (sha256)
   function Manifest_Digest (Json : String) return String
   with Global => null;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Verify blob digest matches content
   function Verify_Digest
     (Content : String;
      Digest  : String) return Boolean
   with Global => null;

   --  Get error message for registry error
   function Error_Message (E : Registry_Error) return String
   with Global => null;

end CT_Registry;
