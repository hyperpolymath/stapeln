-------------------------------------------------------------------------------
--  CT_Transparency - Transparency Log Integration
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  Implements transparency log integration for Cerro Torre signatures.
--  Supports Sigstore Rekor and compatible append-only logs.
--
--  Transparency logs provide:
--    - Public, append-only record of all signing operations
--    - Immutable audit trail for signature verification
--    - Proof of existence at specific timestamps
--    - Detection of key compromise via log monitoring
--
--  Integration Points:
--    - Sigstore Rekor (https://rekor.sigstore.dev)
--    - CT-TLOG (Cerro Torre native transparency log)
--
--  References:
--    - Certificate Transparency RFC 6962
--    - Sigstore Rekor API: https://github.com/sigstore/rekor
--    - Trillian Merkle Tree: https://github.com/google/trillian
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Interfaces;

package CT_Transparency
   with SPARK_Mode => On
is
   use Interfaces;

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Transparency log providers
   type Log_Provider is
     (Sigstore_Rekor,    --  Sigstore public instance
      Sigstore_Staging,  --  Sigstore staging instance
      CT_TLOG,           --  Cerro Torre native log
      Custom);           --  Self-hosted Rekor-compatible log

   --  Entry types supported by Rekor
   type Entry_Kind is
     (HashedRekord,      --  Signature + hash (default for CT)
      Intoto,            --  In-toto attestation
      Dsse,              --  DSSE envelope
      Rfc3161,           --  RFC3161 timestamp
      Rekord,            --  Legacy: signature + artifact
      Alpine,            --  Alpine apk package
      Helm,              --  Helm chart provenance
      Jar,               --  Java JAR signature
      Rpm,               --  RPM package signature
      Cose,              --  COSE Sign1 message
      Tuf);              --  TUF metadata

   --  Log entry identifier
   type Entry_UUID is new String (1 .. 80);  --  64 hex chars typical

   --  Signature algorithm used in entry
   type Sig_Algorithm is
     (Ed25519,
      ECDSA_P256,
      ECDSA_P384,
      RSA_2048,
      RSA_4096,
      ML_DSA_87);        --  Post-quantum (future)

   --  Hash algorithm for artifact
   type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3);

   --  Signed Entry Timestamp (SET) - proof of log inclusion
   type SET_Proof is record
      Log_ID         : Unbounded_String;  --  Log identity (public key hash)
      Log_Index      : Unsigned_64;       --  Sequential entry number
      Integrated_Time : Ada.Calendar.Time;
      Signed_Entry   : Unbounded_String;  --  Base64-encoded SET
   end record;

   --  Inclusion proof (Merkle path)
   type Merkle_Proof is record
      Root_Hash   : Unbounded_String;  --  Tree root at time of proof
      Tree_Size   : Unsigned_64;       --  Tree size when proof generated
      Leaf_Index  : Unsigned_64;       --  Index of entry in tree
      Hashes      : Unbounded_String;  --  JSON array of path hashes
   end record;

   --  Transparency log entry
   type Log_Entry is record
      UUID            : Entry_UUID;
      Kind            : Entry_Kind;
      Log_Index       : Unsigned_64;
      Integrated_Time : Ada.Calendar.Time;
      Log_ID          : Unbounded_String;

      --  Entry content
      Body_Hash       : Unbounded_String;   --  Hash of artifact
      Body_Hash_Algo  : Hash_Algorithm;
      Signature       : Unbounded_String;   --  Base64-encoded signature
      Sig_Algo        : Sig_Algorithm;
      Public_Key      : Unbounded_String;   --  Public key used

      --  Proofs
      SET             : SET_Proof;
      Inclusion       : Merkle_Proof;

      --  Raw entry for verification
      Raw_Entry       : Unbounded_String;   --  JSON of complete entry
   end record;

   package Entry_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Log_Entry);

   subtype Entry_List is Entry_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  Operation Results
   ---------------------------------------------------------------------------

   type Transparency_Error is
     (Success,
      Not_Implemented,      --  Feature not yet implemented
      Network_Error,        --  Connection to log failed
      Invalid_Entry,        --  Malformed entry data
      Entry_Not_Found,      --  UUID not found in log
      Proof_Invalid,        --  Inclusion proof verification failed
      SET_Invalid,          --  Signed Entry Timestamp invalid
      Hash_Mismatch,        --  Artifact hash doesn't match entry
      Signature_Invalid,    --  Entry signature verification failed
      Log_Inconsistent,     --  Log consistency check failed
      Rate_Limited,         --  API rate limit exceeded
      Server_Error);        --  Log server error

   type Upload_Result is record
      Error       : Transparency_Error := Not_Implemented;
      The_Entry   : Log_Entry;  --  Renamed from "Entry" (reserved word)
      URL         : Unbounded_String;  --  URL to view entry
   end record;

   type Lookup_Result is record
      Error   : Transparency_Error := Not_Implemented;
      Entries : Entry_List;
   end record;

   type Verify_Result is record
      Error           : Transparency_Error := Not_Implemented;
      Entry_Valid     : Boolean := False;
      Inclusion_Valid : Boolean := False;
      SET_Valid       : Boolean := False;
   end record;

   ---------------------------------------------------------------------------
   --  Log Client Configuration
   ---------------------------------------------------------------------------

   type Log_Client is record
      Provider     : Log_Provider := Sigstore_Rekor;
      Base_URL     : Unbounded_String;
      Public_Key   : Unbounded_String;  --  Log's signing key
      Timeout_Ms   : Positive := 30_000;
      Verify_TLS   : Boolean := True;
   end record;

   --  Well-known Rekor instances
   Rekor_Production_URL : constant String := "https://rekor.sigstore.dev";
   Rekor_Staging_URL    : constant String := "https://rekor.staging.sigstore.dev";

   function Create_Client
     (Provider : Log_Provider := Sigstore_Rekor;
      URL      : String := "") return Log_Client
   with Global => null;
   --  If URL is empty, uses default URL for provider

   ---------------------------------------------------------------------------
   --  Entry Upload
   ---------------------------------------------------------------------------

   --  Upload a hashedrekord entry (signature + artifact hash)
   function Upload_Signature
     (Client     : Log_Client;
      Signature  : String;       --  Base64-encoded signature
      Artifact   : String;       --  Artifact content or path
      Hash       : String := ""; --  Pre-computed hash (if empty, computed)
      Public_Key : String)       --  PEM-encoded public key
      return Upload_Result
   with Global => null,
        Pre    => Signature'Length > 0 and Public_Key'Length > 0;

   --  Upload an in-toto attestation
   function Upload_Attestation
     (Client      : Log_Client;
      Attestation : String;      --  JSON attestation
      Public_Key  : String)      --  PEM-encoded public key
      return Upload_Result
   with Global => null,
        Pre    => Attestation'Length > 0 and Public_Key'Length > 0;

   --  Upload a DSSE envelope
   function Upload_DSSE
     (Client   : Log_Client;
      Envelope : String;         --  DSSE envelope JSON
      Public_Key : String)
      return Upload_Result
   with Global => null,
        Pre    => Envelope'Length > 0 and Public_Key'Length > 0;

   ---------------------------------------------------------------------------
   --  Entry Lookup
   ---------------------------------------------------------------------------

   --  Lookup entry by UUID
   function Lookup_By_UUID
     (Client : Log_Client;
      UUID   : Entry_UUID) return Lookup_Result
   with Global => null;

   --  Lookup entry by log index
   function Lookup_By_Index
     (Client : Log_Client;
      Index  : Unsigned_64) return Lookup_Result
   with Global => null;

   --  Search entries by artifact hash
   function Search_By_Hash
     (Client : Log_Client;
      Hash   : String;
      Algo   : Hash_Algorithm := SHA256) return Lookup_Result
   with Global => null,
        Pre    => Hash'Length > 0;

   --  Search entries by public key
   function Search_By_Public_Key
     (Client     : Log_Client;
      Public_Key : String) return Lookup_Result
   with Global => null,
        Pre    => Public_Key'Length > 0;

   --  Search entries by email (identity)
   function Search_By_Email
     (Client : Log_Client;
      Email  : String) return Lookup_Result
   with Global => null,
        Pre    => Email'Length > 0;

   ---------------------------------------------------------------------------
   --  Entry Verification
   ---------------------------------------------------------------------------

   --  Verify a log entry (signature, SET, inclusion proof)
   function Verify_Entry
     (Client : Log_Client;
      E      : Log_Entry) return Verify_Result
   with Global => null;

   --  Verify inclusion proof against current tree root
   function Verify_Inclusion
     (Client : Log_Client;
      E      : Log_Entry) return Boolean
   with Global => null;

   --  Verify SET signature
   function Verify_SET
     (Client : Log_Client;
      E      : Log_Entry) return Boolean
   with Global => null;

   --  Verify artifact hash matches entry
   function Verify_Artifact
     (E        : Log_Entry;
      Artifact : String) return Boolean
   with Global => null;

   ---------------------------------------------------------------------------
   --  Log Consistency
   ---------------------------------------------------------------------------

   --  Get current log tree info
   type Tree_Info is record
      Root_Hash   : Unbounded_String;
      Tree_Size   : Unsigned_64;
      Timestamp   : Ada.Calendar.Time;
      Signed_Root : Unbounded_String;  --  Signed tree head
   end record;

   function Get_Log_Info (Client : Log_Client) return Tree_Info
   with Global => null;

   --  Verify log consistency between two tree sizes
   function Verify_Consistency
     (Client    : Log_Client;
      Old_Size  : Unsigned_64;
      Old_Root  : String;
      New_Size  : Unsigned_64;
      New_Root  : String) return Boolean
   with Global => null,
        Pre    => Old_Size <= New_Size;

   ---------------------------------------------------------------------------
   --  Offline Bundle Support
   ---------------------------------------------------------------------------

   --  A verification bundle for offline verification (Sigstore bundle format)
   type Verification_Bundle is record
      Media_Type      : Unbounded_String;  --  "application/vnd.dev.sigstore.bundle+json"
      Signature       : Unbounded_String;
      Public_Key      : Unbounded_String;
      Artifact_Hash   : Unbounded_String;
      Log_Entry       : Unbounded_String;  --  JSON of entry
      Inclusion_Proof : Unbounded_String;  --  JSON of proof
      Timestamp_Proof : Unbounded_String;  --  Optional RFC3161
   end record;

   --  Create offline bundle from entry
   function Create_Bundle
     (E          : Log_Entry;
      Signature  : String;
      Public_Key : String) return Verification_Bundle
   with Global => null;

   --  Serialize bundle to JSON
   function Bundle_To_Json (B : Verification_Bundle) return String
   with Global => null;

   --  Parse bundle from JSON
   function Parse_Bundle (Json : String) return Verification_Bundle
   with Global => null;

   --  Verify bundle offline (no network required)
   function Verify_Bundle_Offline
     (B        : Verification_Bundle;
      Artifact : String;
      Trusted_Root : String := "") return Verify_Result
   with Global => null;
   --  Trusted_Root: If provided, verify SET against this root key

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Get error message
   function Error_Message (E : Transparency_Error) return String
   with Global => null;

   --  Format entry UUID from log index
   function Index_To_UUID (Index : Unsigned_64) return Entry_UUID
   with Global => null;

   --  Get entry viewer URL
   function Entry_URL
     (Client : Log_Client;
      UUID   : Entry_UUID) return String
   with Global => null;

end CT_Transparency;
