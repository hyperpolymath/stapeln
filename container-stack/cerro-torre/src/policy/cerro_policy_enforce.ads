-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Cerro Torre Policy Enforcement Engine
--
-- Enforces A2ML-defined policies against bundle verification

pragma Ada_2022;
pragma Assertion_Policy (Check);

with Cerro.Policy.A2ML;

package Cerro.Policy.Enforce with
   SPARK_Mode,
   Elaborate_Body
is

   use Cerro.Policy.A2ML;

   -------------------------
   -- Verification Types --
   -------------------------

   type Verification_Result is (Pass, Fail, Inconclusive);

   type Verification_Details is record
      Result              : Verification_Result;
      Reason              : Short_String;  -- Human-readable reason
      Signatures_Found    : Natural;
      Signatures_Required : Natural;
      SBOM_Present        : Boolean;
      Rekor_Verified      : Boolean;
      Registry_Allowed    : Boolean;
   end record;

   -------------------------
   -- Bundle Handle      --
   -------------------------

   -- Opaque bundle reference (defined elsewhere)
   -- For MVP: We'll use a placeholder type
   type Bundle_Handle is record
      Path : Path_String;
      Valid : Boolean;
   end record;

   function Is_Valid (Bundle : Bundle_Handle) return Boolean is (Bundle.Valid);

   -------------------------
   -- Policy Handle      --
   -------------------------

   type Policy_Handle is record
      Pol : Policy;
      Valid : Boolean;
   end record;

   function Is_Valid (Handle : Policy_Handle) return Boolean is (Handle.Valid);

   -------------------------
   -- Enforcement        --
   -------------------------

   function Enforce_Policy
     (Bundle : Bundle_Handle;
      Pol    : Policy_Handle) return Verification_Details
   with Pre  => Is_Valid (Bundle) and Is_Valid (Pol),
        Post => (if Enforce_Policy'Result.Result = Pass
                 then Enforce_Policy'Result.Signatures_Found >=
                      Enforce_Policy'Result.Signatures_Required);
   -- Enforce policy requirements against bundle
   -- Returns detailed verification results

   function Check_Signatures
     (Bundle : Bundle_Handle;
      Pol    : Policy_Handle;
      Store  : Trust_Store) return Verification_Details
   with Pre  => Is_Valid (Bundle) and Is_Valid (Pol);
   -- Check bundle signatures against trust store and policy

   function Check_SBOM
     (Bundle : Bundle_Handle;
      Pol    : Policy_Handle) return Verification_Details
   with Pre  => Is_Valid (Bundle) and Is_Valid (Pol);
   -- Check SBOM presence and format

   function Check_Registry
     (Bundle : Bundle_Handle;
      Pol    : Policy_Handle) return Verification_Details
   with Pre  => Is_Valid (Bundle) and Is_Valid (Pol);
   -- Check bundle origin registry against policy allowlist

   -------------------------
   -- Logging            --
   -------------------------

   procedure Log_Enforcement_Decision
     (Bundle  : Bundle_Handle;
      Pol     : Policy_Handle;
      Details : Verification_Details)
   with Pre => Is_Valid (Bundle) and Is_Valid (Pol);
   -- Log enforcement decision to audit log specified in policy

   -------------------------
   -- Exceptions         --
   -------------------------

   Policy_Violation : exception;
   Enforcement_Error : exception;

end Cerro.Policy.Enforce;
