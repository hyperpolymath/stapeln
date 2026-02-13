--  Cerro Torre Manifest - SPARK-verified manifest parsing
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides manifest parsing with formal verification.
--  The parser is proven to terminate on all inputs and to correctly
--  validate manifest structure.

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Calendar;

package Cerro_Manifest
   with SPARK_Mode => On
is
   use Ada.Strings.Unbounded;

   ---------------------
   -- Type Definitions --
   ---------------------

   --  Package name (validated identifier)
   subtype Package_Name_Length is Positive range 1 .. 64;
   subtype Package_Name is String
      with Dynamic_Predicate =>
         Package_Name'Length in Package_Name_Length
         and then (for all C of Package_Name =>
            C in 'a' .. 'z' | '0' .. '9' | '-');

   --  Version components
   type Version is record
      Epoch    : Natural := 0;
      Upstream : Unbounded_String;
      Revision : Positive := 1;
   end record;

   --  Hash specification
   type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3, Shake256);

   type Hash_Value is record
      Algorithm : Hash_Algorithm;
      Digest    : Unbounded_String;  --  Hex-encoded
   end record;

   --  SPDX license expression (simplified)
   subtype License_Expression is Unbounded_String;

   --  Dependency reference
   type Version_Constraint_Kind is (Any, Exact, Minimum, Maximum, Range_Constraint);

   type Version_Constraint (Kind : Version_Constraint_Kind := Any) is record
      case Kind is
         when Any =>
            null;
         when Exact | Minimum | Maximum =>
            Bound : Unbounded_String;
         when Range_Constraint =>
            Lower_Bound : Unbounded_String;
            Upper_Bound : Unbounded_String;
      end case;
   end record;

   type Dependency_Reference is record
      Name       : Unbounded_String;
      Constraint : Version_Constraint;
   end record;

   --  Dependency list
   package Dependency_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Dependency_Reference);

   subtype Dependency_List is Dependency_Vectors.Vector;

   --  String list (for patches, flags, etc.)
   package String_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Unbounded_String);

   subtype String_List is String_Vectors.Vector;

   ----------------------
   -- Manifest Sections --
   ----------------------

   --  [metadata] section
   type Metadata_Section is record
      Name        : Unbounded_String;
      Version     : Cerro_Manifest.Version;
      Summary     : Unbounded_String;
      Description : Unbounded_String;
      License     : License_Expression;
      Homepage    : Unbounded_String;
      Maintainer  : Unbounded_String;
   end record;

   --  [provenance] section
   type Provenance_Section is record
      Upstream_URL       : Unbounded_String;
      Upstream_Hash      : Hash_Value;
      Upstream_Signature : Unbounded_String;  --  Optional
      Upstream_Keyring   : Unbounded_String;  --  Optional
      Imported_From      : Unbounded_String;  --  e.g., "debian:nginx/1.26.0-1"
      Import_Date        : Ada.Calendar.Time;
      Patches            : String_List;
   end record;

   --  [dependencies] section
   type Dependencies_Section is record
      Runtime   : Dependency_List;
      Build     : Dependency_List;
      Check     : Dependency_List;
      Optional  : Dependency_List;
      Conflicts : Dependency_List;
      Provides  : Dependency_List;
      Replaces  : Dependency_List;
   end record;

   --  Build system identifier
   type Build_System is
      (Autoconf, CMake, Meson, Cargo, Alire, Dune, Mix, Make, Custom);

   --  [build] section
   type Build_Section is record
      System          : Build_System;
      Configure_Flags : String_List;
      Build_Flags     : String_List;
      Install_Flags   : String_List;
      --  Environment and phases handled separately
   end record;

   --  [outputs] section
   type Outputs_Section is record
      Primary : Unbounded_String;
      Split   : String_List;
   end record;

   --  Attestation types
   type Attestation_Type is
      (Source_Signature, Reproducible_Build, SBOM_Complete,
       Security_Audit, Fuzz_Tested);

   package Attestation_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Attestation_Type);

   subtype Attestation_List is Attestation_Vectors.Vector;

   --  [attestations] section
   type Attestations_Section is record
      Required    : Attestation_List;
      Recommended : Attestation_List;
   end record;

   -----------------------
   -- Complete Manifest --
   -----------------------

   type Manifest is record
      Metadata     : Metadata_Section;
      Provenance   : Provenance_Section;
      Dependencies : Dependencies_Section;
      Build        : Build_Section;
      Outputs      : Outputs_Section;
      Attestations : Attestations_Section;
   end record;

   ------------------
   -- Parse Result --
   ------------------

   type Parse_Error_Kind is
      (None,
       File_Not_Found,
       Invalid_Encoding,
       Syntax_Error,
       Missing_Required_Field,
       Invalid_Value,
       Duplicate_Section,
       Unknown_Section);

   type Parse_Result (Success : Boolean := False) is record
      case Success is
         when True =>
            Value : Manifest;
         when False =>
            Error      : Parse_Error_Kind;
            Error_Line : Natural;
            Error_Msg  : Unbounded_String;
      end case;
   end record;

   -----------------------
   -- Parsing Functions --
   -----------------------

   --  Parse a manifest from string content
   function Parse_String (Content : String) return Parse_Result
      with Global => null,
           Pre    => Content'Length <= 1_000_000;  --  1MB max manifest

   --  Parse a manifest from file
   function Parse_File (Path : String) return Parse_Result
      with Pre => Path'Length > 0 and Path'Length <= 4096;

   -------------------------
   -- Validation Functions --
   -------------------------

   --  Validate manifest completeness (all required fields present)
   function Is_Complete (M : Manifest) return Boolean
      with Global => null;

   --  Validate hash format
   function Is_Valid_Hash (H : Hash_Value) return Boolean
      with Global => null;

   --  Validate version string
   function Is_Valid_Version (V : Version) return Boolean
      with Global => null;

   -----------------------
   -- Serialization --
   -----------------------

   --  Serialize manifest to string
   function To_String (M : Manifest) return String
      with Global => null;

end Cerro_Manifest;
