--  Cerro Torre Provenance - SPARK-verified provenance chain
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides provenance chain verification with formal proofs.
--  It implements in-toto attestation validation and ensures complete
--  provenance from source to binary.

with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Cerro_Crypto;
with Cerro_Manifest;

package Cerro_Provenance
   with SPARK_Mode => On
is
   use Ada.Strings.Unbounded;
   use Cerro_Crypto;

   ---------------------
   -- Type Definitions --
   ---------------------

   --  Attestation statement types (in-toto compatible)
   type Statement_Type is
      (Source_Attestation,      --  Source code provenance
       Build_Attestation,       --  Build environment and process
       Package_Attestation,     --  Package creation
       Signature_Attestation,   --  Cryptographic signing
       Review_Attestation);     --  Human review

   --  Subject of an attestation (what is being attested)
   type Subject is record
      Name   : Unbounded_String;  --  Package name or identifier
      Digest : SHA256_Digest;     --  Content hash
   end record;

   package Subject_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Subject);

   subtype Subject_List is Subject_Vectors.Vector;

   --  Predicate (what is being claimed)
   type Predicate_Type is
      (SLSA_Provenance,    --  SLSA provenance predicate
       SPDX_SBOM,          --  SPDX SBOM predicate
       CT_Native);         --  Cerro Torre native predicate

   --  Builder identity
   type Builder_Identity is record
      ID          : Unbounded_String;  --  Builder identifier
      Version     : Unbounded_String;  --  Builder version
      Verified    : Boolean;           --  Is builder identity verified?
   end record;

   --  Build invocation record
   type Build_Invocation is record
      Config_Source : Unbounded_String;  --  Manifest or recipe location
      Config_Digest : SHA256_Digest;     --  Hash of build config
      Parameters    : Unbounded_String;  --  Build parameters (JSON)
      Environment   : Unbounded_String;  --  Environment variables (JSON)
   end record;

   --  Material (input to build)
   type Material is record
      URI    : Unbounded_String;
      Digest : SHA256_Digest;
   end record;

   package Material_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Material);

   subtype Material_List is Material_Vectors.Vector;

   ------------------------
   -- Attestation Record --
   ------------------------

   type Attestation is record
      Stmt_Type        : Statement_Type;
      Pred_Type        : Predicate_Type;
      Subjects         : Subject_List;
      Builder          : Builder_Identity;
      Invocation       : Build_Invocation;
      Materials        : Material_List;
      Build_Start_Time : Ada.Calendar.Time;
      Build_End_Time   : Ada.Calendar.Time;
      Signature        : Ed25519_Signature;
      Signer_Key       : Ed25519_Public_Key;
   end record;

   ----------------------
   -- Provenance Chain --
   ----------------------

   package Attestation_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Attestation);

   subtype Attestation_Chain is Attestation_Vectors.Vector;

   --  Complete provenance record for a package
   type Package_Provenance is record
      Package_Name    : Unbounded_String;
      Package_Version : Cerro_Manifest.Version;
      Package_Digest  : SHA256_Digest;
      Chain           : Attestation_Chain;
      Is_Complete     : Boolean;  --  All links verified
      Is_Trusted      : Boolean;  --  All signers trusted
   end record;

   --------------------------
   -- Verification Results --
   --------------------------

   type Verification_Status is
      (Verified,                 --  Chain complete and all signatures valid
       Missing_Attestation,      --  Gap in provenance chain
       Invalid_Signature,        --  Cryptographic verification failed
       Untrusted_Signer,         --  Signer not in trust store
       Hash_Mismatch,            --  Content doesn't match claimed hash
       Expired_Attestation,      --  Attestation outside validity period
       Parse_Error);             --  Malformed attestation

   type Verification_Result is record
      Status          : Verification_Status;
      Failed_At       : Natural;  --  Index in chain where failure occurred
      Error_Message   : Unbounded_String;
   end record;

   ---------------------------
   -- Verification Functions --
   ---------------------------

   --  Verify a single attestation's signature
   function Verify_Attestation_Signature (A : Attestation) return Boolean
      with Global => null;

   --  Verify that attestation chain is complete (no gaps)
   function Is_Chain_Complete (Chain : Attestation_Chain) return Boolean
      with Global => null;

   --  Verify that all materials in an attestation have valid hashes
   function Verify_Materials (A : Attestation) return Boolean
      with Global => null;

   --  Full verification of a provenance chain
   function Verify_Chain
      (Chain       : Attestation_Chain;
       Trust_Store : String)  --  Path to trusted keys
      return Verification_Result
      with Global => null,
           Pre    => Trust_Store'Length > 0;

   --  Verify a package against its claimed provenance
   function Verify_Package
      (Package_Path : String;
       Provenance   : Package_Provenance)
      return Verification_Result
      with Pre => Package_Path'Length > 0;

   --------------------------
   -- Attestation Creation --
   --------------------------

   --  Create a source attestation
   function Create_Source_Attestation
      (Source_URI    : String;
       Source_Digest : SHA256_Digest;
       Upstream_Sig  : String)
      return Attestation
      with Global => null,
           Pre    => Source_URI'Length > 0;

   --  Create a build attestation
   function Create_Build_Attestation
      (Manifest      : Cerro_Manifest.Manifest;
       Output_Digest : SHA256_Digest;
       Builder       : Builder_Identity;
       Materials     : Material_List)
      return Attestation
      with Global => null;

   -------------------------
   -- Serialization (JSON) --
   -------------------------

   --  Serialize attestation to in-toto JSON format
   function To_JSON (A : Attestation) return String
      with Global => null;

   --  Parse attestation from JSON
   type Parse_Result (Success : Boolean := False) is record
      case Success is
         when True =>
            Value : Attestation;
         when False =>
            Error_Msg : Unbounded_String;
      end case;
   end record;

   function From_JSON (JSON : String) return Parse_Result
      with Global => null,
           Pre    => JSON'Length > 0 and JSON'Length <= 10_000_000;

end Cerro_Provenance;
