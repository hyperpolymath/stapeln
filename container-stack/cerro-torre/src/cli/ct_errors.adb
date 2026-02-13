--  Cerro Torre CLI - Exit codes and error types
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

package body CT_Errors is

   function Exit_Code_For (Category : Error_Category)
     return Ada.Command_Line.Exit_Status
   is
   begin
      case Category is
         when Cat_Hash_Mismatch     => return Exit_Hash_Mismatch;
         when Cat_Signature_Invalid => return Exit_Signature_Invalid;
         when Cat_Key_Not_Trusted   => return Exit_Key_Not_Trusted;
         when Cat_Policy_Rejection  => return Exit_Policy_Rejection;
         when Cat_Missing_Attestation => return Exit_Missing_Attestation;
         when Cat_Malformed_Bundle  => return Exit_Malformed_Bundle;
         when Cat_IO_Error          => return Exit_IO_Error;
         when Cat_Network_Error     => return Exit_Network_Error;
      end case;
   end Exit_Code_For;

   function Category_Name (Category : Error_Category) return String is
   begin
      case Category is
         when Cat_Hash_Mismatch     => return "hash mismatch";
         when Cat_Signature_Invalid => return "signature invalid";
         when Cat_Key_Not_Trusted   => return "key not trusted";
         when Cat_Policy_Rejection  => return "policy rejection";
         when Cat_Missing_Attestation => return "missing attestation";
         when Cat_Malformed_Bundle  => return "malformed bundle";
         when Cat_IO_Error          => return "I/O error";
         when Cat_Network_Error     => return "network error";
      end case;
   end Category_Name;

end CT_Errors;
