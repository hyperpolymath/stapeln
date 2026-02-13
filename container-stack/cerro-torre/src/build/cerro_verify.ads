--  Cerro_Verify - Bundle verification
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Verifies .ctp bundle integrity and signatures.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Cerro_Verify is

   --  Verification result codes (matches exit codes in spec)
   type Verify_Code is
     (OK,                    --  0: Verification succeeded
      Hash_Mismatch,         --  1: Content hash doesn't match
      Signature_Invalid,     --  2: Ed25519 signature invalid
      Key_Not_Trusted,       --  3: Signing key not in policy
      Policy_Rejection,      --  4: Bundle rejected by policy rules
      Missing_Attestation,   --  5: Required attestation missing
      Malformed_Bundle,      --  10: Invalid tar/structure
      IO_Error);             --  11: File system error

   --  Exit code mapping
   function To_Exit_Code (Code : Verify_Code) return Integer;

   --  Verification result with details
   type Verify_Result is record
      Code          : Verify_Code;
      Package_Name  : Unbounded_String;
      Package_Ver   : Unbounded_String;
      Manifest_Hash : Unbounded_String;
      Details       : Unbounded_String;
   end record;

   --  Verification options
   type Verify_Options is record
      Bundle_Path : Unbounded_String;
      Policy_Path : Unbounded_String;
      Offline     : Boolean := False;
      Verbose     : Boolean := False;
   end record;

   --  Main verification entry point
   function Verify_Bundle (Opts : Verify_Options) return Verify_Result;

end Cerro_Verify;
