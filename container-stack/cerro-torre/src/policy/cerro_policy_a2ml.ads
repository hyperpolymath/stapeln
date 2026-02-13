-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Cerro Torre A2ML Policy Integration
--
-- This package integrates A2ML (Attested Markup Language) for policy definitions
-- and trust store manifests, providing attestable, verifiable policy enforcement.

pragma Ada_2022;
pragma Assertion_Policy (Check);

with Ada.Containers.Vectors;
with Ada.Strings.Bounded;

package Cerro.Policy.A2ML with
   SPARK_Mode,
   Elaborate_Body
is

   -- Maximum string sizes for bounded strings
   Max_ID_Length       : constant := 128;
   Max_String_Length   : constant := 1024;
   Max_Path_Length     : constant := 4096;

   package ID_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_ID_Length);
   subtype ID_String is ID_Strings.Bounded_String;

   package Short_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_String_Length);
   subtype Short_String is Short_Strings.Bounded_String;

   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max_Path_Length);
   subtype Path_String is Path_Strings.Bounded_String;

   -------------------------
   -- Trust Store Types --
   -------------------------

   type Key_Algorithm is (Ed25519, ML_DSA_65, RSA_4096, ECDSA_P256);

   type Signing_Key is record
      ID           : ID_String;
      Algorithm    : Key_Algorithm;
      Public_Key   : Short_String;  -- Base64-encoded public key
      Fingerprint  : Short_String;  -- SHA256 fingerprint
      Purpose      : Short_String;  -- Human-readable purpose
      Owner        : Short_String;  -- Key owner contact
      Revoked      : Boolean;
      Revocation_Reason : Short_String;  -- Empty if not revoked
   end record;

   package Key_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Signing_Key);

   type Trust_Store is record
      ID       : ID_String;
      Version  : ID_String;
      Keys     : Key_Vectors.Vector;
      Verified : Boolean;  -- True if attestation verified
   end record;

   ----------------------
   -- Policy Types    --
   ----------------------

   type Policy_Requirement_Type is
     (Attestation_Required,      -- Rekor transparency log required
      SBOM_Required,             -- SBOM must be present
      Min_Signatures,            -- Minimum signature count
      Registry_Allowlist,        -- Registry must be in allowlist
      Build_Attestation);        -- SLSA provenance required

   type Policy_Requirement is record
      Req_Type    : Policy_Requirement_Type;
      Enabled     : Boolean;
      Min_Sigs    : Positive;  -- For Min_Signatures
      SBOM_Format : Short_String;  -- For SBOM_Required (e.g., "spdx")
   end record;

   package Requirement_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Policy_Requirement);

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Short_String);

   type Policy is record
      ID                 : ID_String;
      Version            : ID_String;
      Description        : Short_String;
      Requirements       : Requirement_Vectors.Vector;
      Allowed_Registries : String_Vectors.Vector;
      Blocked_Registries : String_Vectors.Vector;
      Trusted_Key_IDs    : String_Vectors.Vector;
      Audit_Log_Path     : Path_String;
      Verified           : Boolean;  -- True if policy signature verified
   end record;

   -------------------------
   -- Loading Functions  --
   -------------------------

   function Load_Trust_Store (Path : String) return Trust_Store
     with Pre  => Path'Length > 0,
          Post => (if Load_Trust_Store'Result.Verified
                   then not Load_Trust_Store'Result.Keys.Is_Empty);
   -- Load trust store from A2ML file
   -- Verifies attestation (Rekor entry if present)
   -- Returns Trust_Store with Verified=True if attestation valid

   function Load_Policy (Path : String) return Policy
     with Pre  => Path'Length > 0,
          Post => not Load_Policy'Result.Requirements.Is_Empty;
   -- Load policy from A2ML file
   -- Verifies policy signature if present
   -- Returns Policy with Verified=True if signature valid

   ------------------------
   -- Query Functions   --
   ------------------------

   function Find_Key (Store : Trust_Store; Key_ID : String) return Signing_Key
     with Pre => not Store.Keys.Is_Empty;
   -- Find key by ID in trust store
   -- Raises Constraint_Error if not found

   function Is_Key_Valid (Key : Signing_Key) return Boolean
     with Post => (if Is_Key_Valid'Result then not Key.Revoked);
   -- Check if key is valid (not revoked)

   function Allows_Registry (Pol : Policy; Registry_URL : String) return Boolean;
   -- Check if policy allows the registry
   -- Returns True if registry is in allowlist or allowlist is empty
   -- Returns False if registry is in blocklist

   -------------------------
   -- Verification       --
   -------------------------

   function Verify_Trust_Store (Store : Trust_Store) return Boolean
     with Post => (if Verify_Trust_Store'Result then Store.Verified);
   -- Verify trust store attestation (Rekor entry)
   -- For MVP: stub implementation (always returns True)
   -- TODO: Implement actual Rekor verification

   function Verify_Policy (Pol : Policy) return Boolean
     with Post => (if Verify_Policy'Result then Pol.Verified);
   -- Verify policy signature
   -- For MVP: stub implementation (always returns True)
   -- TODO: Implement actual signature verification

   -------------------------
   -- Exceptions         --
   -------------------------

   Parse_Error      : exception;
   Verification_Error : exception;
   Key_Not_Found    : exception;

end Cerro.Policy.A2ML;
