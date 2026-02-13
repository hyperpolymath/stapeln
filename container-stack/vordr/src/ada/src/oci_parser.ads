-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Vörðr Gatekeeper
-- OCI configuration JSON parser (simplified, formally verified)

pragma SPARK_Mode (On);

with Container_Policy;

package OCI_Parser is

   --  Maximum input JSON length we'll handle
   Max_Json_Length : constant := 65536;

   --  Bounded string type for JSON input
   subtype Json_String is String (1 .. Max_Json_Length);
   subtype Json_Length is Natural range 0 .. Max_Json_Length;

   --  Parse result status
   type Parse_Status is (
      Parse_OK,
      Parse_Too_Long,
      Parse_Invalid_Json,
      Parse_Missing_Field,
      Parse_Invalid_Value
   );

   --  Parsing result with configuration
   type Parse_Result is record
      Status : Parse_Status;
      Config : Container_Policy.Container_Config;
   end record;

   --  Parse OCI runtime configuration JSON into Container_Config
   --  This is a simplified parser that extracts only security-relevant fields
   function Parse_Oci_Config (
      Json   : String;
      Length : Natural
   ) return Parse_Result
     with Pre => Length <= Json'Length and Length <= Max_Json_Length;

   --  Check if a character is whitespace
   function Is_Whitespace (C : Character) return Boolean is
     (C = ' ' or C = ASCII.HT or C = ASCII.LF or C = ASCII.CR);

   --  Check if a character is a digit
   function Is_Digit (C : Character) return Boolean is
     (C >= '0' and C <= '9');

private

   --  Internal parsing state
   type Parser_State is record
      Position : Natural;
      Length   : Natural;
   end record;

   --  Skip whitespace in JSON
   procedure Skip_Whitespace (
      Json  : String;
      State : in out Parser_State
   )
     with Pre => State.Position <= Json'Last and State.Length <= Json'Length;

   --  Check for and consume a specific string
   function Match_String (
      Json    : String;
      State   : in out Parser_State;
      Pattern : String
   ) return Boolean
     with Pre => State.Position <= Json'Last and State.Length <= Json'Length;

   --  Parse a boolean value
   function Parse_Boolean (
      Json  : String;
      State : in out Parser_State
   ) return Boolean
     with Pre => State.Position <= Json'Last and State.Length <= Json'Length;

   --  Parse a natural number
   function Parse_Natural (
      Json  : String;
      State : in Out Parser_State
   ) return Natural
     with Pre => State.Position <= Json'Last and State.Length <= Json'Length;

   --  Parse a string value (returns True if successful)
   function Parse_String_Value (
      Json   : String;
      State  : in Out Parser_State;
      Result : out String;
      Length : out Natural
   ) return Boolean
     with Pre => State.Position <= Json'Last and State.Length <= Json'Length;

   --  Find a field by name in a JSON object
   function Find_Field (
      Json  : String;
      State : in Out Parser_State;
      Name  : String
   ) return Boolean
     with Pre => State.Position <= Json'Last and State.Length <= Json'Length;

end OCI_Parser;
