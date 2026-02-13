-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper
-- C-compatible interface for Rust FFI

pragma SPARK_Mode (Off);  -- FFI code cannot be proven

with Interfaces.C;
with Interfaces.C.Strings;

package Policy_Interface is

   use Interfaces.C;

   --  Error message buffer size
   Error_Buffer_Size : constant := 256;

   --  Validate JSON configuration string
   --  Parameters:
   --    Json_Str: Pointer to null-terminated JSON string
   --  Returns:
   --    0 = Valid
   --    1 = Invalid_Capabilities
   --    2 = Invalid_User_Namespace
   --    3 = Invalid_Network_Mode
   --    4 = Invalid_Privilege_Escape
   --    5 = Parse_Error
   --   -1 = Internal_Error
   function Verify_Json_Config (
      Json_Str : Interfaces.C.Strings.chars_ptr
   ) return int
     with Export, Convention => C, External_Name => "verify_json_config";

   --  Get human-readable error message for result code
   --  Parameters:
   --    Code: Validation result code
   --  Returns:
   --    Pointer to static error message string (do not free)
   function Get_Error_Message (
      Code : int
   ) return Interfaces.C.Strings.chars_ptr
     with Export, Convention => C, External_Name => "get_error_message";

   --  Validate and sanitise JSON configuration
   --  Returns sanitised JSON with security defaults applied
   --  Parameters:
   --    Json_Str: Pointer to null-terminated JSON string
   --    Output_Buffer: Buffer to write sanitised JSON
   --    Buffer_Size: Size of output buffer
   --  Returns:
   --    >= 0: Length of sanitised JSON written
   --    <  0: Error code (negated)
   function Sanitise_Config (
      Json_Str      : Interfaces.C.Strings.chars_ptr;
      Output_Buffer : Interfaces.C.Strings.chars_ptr;
      Buffer_Size   : int
   ) return int
     with Export, Convention => C, External_Name => "sanitise_config";

   --  Get the library version
   --  Returns:
   --    Pointer to version string (do not free)
   function Get_Version return Interfaces.C.Strings.chars_ptr
     with Export, Convention => C, External_Name => "gatekeeper_version";

   --  Initialise the gatekeeper (call once at startup)
   --  Returns:
   --    0 on success, non-zero on failure
   function Gatekeeper_Init return int
     with Export, Convention => C, External_Name => "gatekeeper_init";

end Policy_Interface;
