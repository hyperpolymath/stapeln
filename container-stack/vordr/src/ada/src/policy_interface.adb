-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper
-- C-compatible interface implementation for Rust FFI

pragma SPARK_Mode (Off);  -- FFI code cannot be proven

with Container_Policy;
with OCI_Parser;
with Ada.Strings.Fixed;

package body Policy_Interface is

   use Interfaces.C;
   use Interfaces.C.Strings;

   --  Static error messages (must remain in scope)
   Msg_Valid             : aliased constant String := "Configuration is valid and secure" & ASCII.NUL;
   Msg_Invalid_Caps      : aliased constant String := "SYS_ADMIN capability requires privileged mode" & ASCII.NUL;
   Msg_Invalid_User_NS   : aliased constant String := "Root UID (0) requires user namespace to be enabled" & ASCII.NUL;
   Msg_Invalid_Network   : aliased constant String := "NET_ADMIN capability requires Restricted or Admin network mode" & ASCII.NUL;
   Msg_Invalid_Priv_Esc  : aliased constant String := "Potential privilege escalation: set no_new_privileges or enable user namespace" & ASCII.NUL;
   Msg_Parse_Error       : aliased constant String := "Failed to parse container configuration" & ASCII.NUL;
   Msg_Internal_Error    : aliased constant String := "Internal error in security validation" & ASCII.NUL;
   Msg_Unknown           : aliased constant String := "Unknown error code" & ASCII.NUL;

   --  Version string
   Version_String : aliased constant String := "0.1.0" & ASCII.NUL;

   ------------------------
   -- Verify_Json_Config --
   ------------------------

   function Verify_Json_Config (
      Json_Str : Interfaces.C.Strings.chars_ptr
   ) return int
   is
      Json_Ada    : String (1 .. OCI_Parser.Max_Json_Length);
      Json_Length : Natural;
      Parse_Result : OCI_Parser.Parse_Result;
      Valid_Result : Container_Policy.Validation_Result;
   begin
      --  Handle null pointer
      if Json_Str = Null_Ptr then
         return int (Container_Policy.To_Exit_Code (Container_Policy.Parse_Error));
      end if;

      --  Convert C string to Ada string
      declare
         C_String : constant String := Value (Json_Str);
      begin
         Json_Length := C_String'Length;

         if Json_Length > OCI_Parser.Max_Json_Length then
            return int (Container_Policy.To_Exit_Code (Container_Policy.Parse_Error));
         end if;

         if Json_Length > 0 then
            Json_Ada (1 .. Json_Length) := C_String;
         end if;
      end;

      --  Parse the JSON
      Parse_Result := OCI_Parser.Parse_Oci_Config (Json_Ada, Json_Length);

      if Parse_Result.Status /= OCI_Parser.Parse_OK then
         return int (Container_Policy.To_Exit_Code (Container_Policy.Parse_Error));
      end if;

      --  Validate the configuration
      Valid_Result := Container_Policy.Validate_Configuration (Parse_Result.Config);

      return int (Container_Policy.To_Exit_Code (Valid_Result));

   exception
      when others =>
         return int (Container_Policy.To_Exit_Code (Container_Policy.Internal_Error));
   end Verify_Json_Config;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message (
      Code : int
   ) return Interfaces.C.Strings.chars_ptr
   is
      Result : Container_Policy.Validation_Result;
   begin
      Result := Container_Policy.From_Exit_Code (Integer (Code));

      case Result is
         when Container_Policy.Valid =>
            return New_String (Msg_Valid);
         when Container_Policy.Invalid_Capabilities =>
            return New_String (Msg_Invalid_Caps);
         when Container_Policy.Invalid_User_Namespace =>
            return New_String (Msg_Invalid_User_NS);
         when Container_Policy.Invalid_Network_Mode =>
            return New_String (Msg_Invalid_Network);
         when Container_Policy.Invalid_Privilege_Escape =>
            return New_String (Msg_Invalid_Priv_Esc);
         when Container_Policy.Parse_Error =>
            return New_String (Msg_Parse_Error);
         when Container_Policy.Internal_Error =>
            return New_String (Msg_Internal_Error);
      end case;

   exception
      when others =>
         return New_String (Msg_Unknown);
   end Get_Error_Message;

   ---------------------
   -- Sanitise_Config --
   ---------------------

   function Sanitise_Config (
      Json_Str      : Interfaces.C.Strings.chars_ptr;
      Output_Buffer : Interfaces.C.Strings.chars_ptr;
      Buffer_Size   : int
   ) return int
   is
      Json_Ada     : String (1 .. OCI_Parser.Max_Json_Length);
      Json_Length  : Natural;
      Parse_Result : OCI_Parser.Parse_Result;
   begin
      --  Handle null pointers
      if Json_Str = Null_Ptr or Output_Buffer = Null_Ptr then
         return -int (Container_Policy.To_Exit_Code (Container_Policy.Parse_Error));
      end if;

      if Buffer_Size <= 0 then
         return -int (Container_Policy.To_Exit_Code (Container_Policy.Parse_Error));
      end if;

      --  Convert C string to Ada string
      declare
         C_String : constant String := Value (Json_Str);
      begin
         Json_Length := C_String'Length;

         if Json_Length > OCI_Parser.Max_Json_Length then
            return -int (Container_Policy.To_Exit_Code (Container_Policy.Parse_Error));
         end if;

         if Json_Length > 0 then
            Json_Ada (1 .. Json_Length) := C_String;
         end if;
      end;

      --  Parse and sanitise
      Parse_Result := OCI_Parser.Parse_Oci_Config (Json_Ada, Json_Length);

      if Parse_Result.Status /= OCI_Parser.Parse_OK then
         return -int (Container_Policy.To_Exit_Code (Container_Policy.Parse_Error));
      end if;

      --  Security defaults are already applied by Parse_Oci_Config
      --  For now, return the original JSON length
      --  (Full implementation would serialize the sanitised config)

      return int (Json_Length);

   exception
      when others =>
         return -int (Container_Policy.To_Exit_Code (Container_Policy.Internal_Error));
   end Sanitise_Config;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version return Interfaces.C.Strings.chars_ptr is
   begin
      return New_String (Version_String);
   end Get_Version;

   ---------------------
   -- Gatekeeper_Init --
   ---------------------

   function Gatekeeper_Init return int is
   begin
      --  Currently no initialization required
      --  This is a hook for future initialization needs
      return 0;
   exception
      when others =>
         return -1;
   end Gatekeeper_Init;

end Policy_Interface;
