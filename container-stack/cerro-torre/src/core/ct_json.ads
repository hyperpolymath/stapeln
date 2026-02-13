-------------------------------------------------------------------------------
--  CT_JSON - Minimal JSON parser for OCI registry operations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Provides lightweight JSON parsing/serialization tailored to:
--  - OAuth2 token responses
--  - OCI manifest structures
--  - Tag list responses
--
--  Not a general-purpose JSON library - handles specific registry formats only.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package CT_JSON
   with SPARK_Mode => Off  --  String parsing requires dynamic memory
is

   ---------------------------------------------------------------------------
   --  String Vectors for JSON arrays
   ---------------------------------------------------------------------------

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String,
      "="          => "=");

   subtype String_Array is String_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  JSON Value Extraction
   ---------------------------------------------------------------------------

   --  Extract string value from JSON object
   --  Example: Get_String_Field ('{"token":"abc123"}', "token") -> "abc123"
   function Get_String_Field
     (JSON  : String;
      Field : String) return String;

   --  Extract integer value from JSON object
   --  Example: Get_Integer_Field ('{"count":42}', "count") -> 42
   function Get_Integer_Field
     (JSON  : String;
      Field : String) return Integer;

   --  Extract boolean value from JSON object
   --  Example: Get_Boolean_Field ('{"valid":true}', "valid") -> True
   function Get_Boolean_Field
     (JSON  : String;
      Field : String) return Boolean;

   --  Extract string array from JSON object
   --  Example: Get_Array_Field ('{"tags":["v1","v2"]}', "tags") -> ["v1","v2"]
   function Get_Array_Field
     (JSON  : String;
      Field : String) return String_Array;

   --  Check if field exists in JSON
   function Has_Field
     (JSON  : String;
      Field : String) return Boolean;

   ---------------------------------------------------------------------------
   --  JSON Construction (for serialization)
   ---------------------------------------------------------------------------

   --  Build JSON object from key-value pairs
   type JSON_Builder is tagged private;

   --  Create new JSON builder
   function Create return JSON_Builder;

   --  Add string field
   procedure Add_String
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : String);

   --  Add integer field
   procedure Add_Integer
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : Integer);

   --  Add boolean field
   procedure Add_Boolean
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : Boolean);

   --  Add null field
   procedure Add_Null
     (Builder : in out JSON_Builder;
      Key     : String);

   --  Add array field
   procedure Add_Array
     (Builder : in out JSON_Builder;
      Key     : String;
      Values  : String_Array);

   --  Add nested object field (pre-built JSON)
   procedure Add_Object
     (Builder : in out JSON_Builder;
      Key     : String;
      Value   : String);

   --  Serialize to JSON string
   function To_JSON (Builder : JSON_Builder) return String;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Escape special characters for JSON string
   function Escape_JSON_String (S : String) return String;

   --  Unescape JSON string (handle \", \\, etc.)
   function Unescape_JSON_String (S : String) return String;

private

   type JSON_Builder is tagged record
      Content : Unbounded_String;
      Empty   : Boolean := True;
   end record;

end CT_JSON;
