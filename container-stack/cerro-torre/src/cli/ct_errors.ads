--  Cerro Torre CLI - Exit codes and error types
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Command_Line;

package CT_Errors is

   --  Exit codes per spec/cli-ergonomics.adoc
   --  Each exit code maps to a specific failure type for scripting

   Exit_Success          : constant Ada.Command_Line.Exit_Status := 0;
   Exit_Hash_Mismatch    : constant Ada.Command_Line.Exit_Status := 1;
   Exit_Signature_Invalid : constant Ada.Command_Line.Exit_Status := 2;
   Exit_Key_Not_Trusted  : constant Ada.Command_Line.Exit_Status := 3;
   Exit_Policy_Rejection : constant Ada.Command_Line.Exit_Status := 4;
   Exit_Missing_Attestation : constant Ada.Command_Line.Exit_Status := 5;
   Exit_Malformed_Bundle : constant Ada.Command_Line.Exit_Status := 10;
   Exit_IO_Error         : constant Ada.Command_Line.Exit_Status := 11;
   Exit_Network_Error    : constant Ada.Command_Line.Exit_Status := 12;

   --  General failure (usage error, not yet implemented, etc.)
   Exit_General_Failure  : constant Ada.Command_Line.Exit_Status := 1;

   --  Error categories for message formatting
   type Error_Category is
     (Cat_Hash_Mismatch,
      Cat_Signature_Invalid,
      Cat_Key_Not_Trusted,
      Cat_Policy_Rejection,
      Cat_Missing_Attestation,
      Cat_Malformed_Bundle,
      Cat_IO_Error,
      Cat_Network_Error);

   --  Get exit code for an error category
   function Exit_Code_For (Category : Error_Category)
     return Ada.Command_Line.Exit_Status;

   --  Get human-readable category name
   function Category_Name (Category : Error_Category) return String;

end CT_Errors;
