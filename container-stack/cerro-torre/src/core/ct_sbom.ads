-------------------------------------------------------------------------------
--  CT_SBOM - Software Bill of Materials Generation
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  This package provides SBOM (Software Bill of Materials) generation
--  from Cerro Torre manifests. Supports:
--    - CycloneDX 1.5 format (JSON) - Primary format
--    - SPDX 2.3 format (JSON)
--    - Component enumeration from manifest
--    - Dependency relationship mapping
--    - Hash/checksum inclusion
--    - License information extraction
--
--  References:
--    - CycloneDX 1.5: https://cyclonedx.org/specification/overview/
--    - SPDX 2.3: https://spdx.github.io/spdx-spec/v2.3/
--    - Package URL (purl): https://github.com/package-url/purl-spec
--    - CPE: https://nvd.nist.gov/products/cpe
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Cerro_Manifest;

package CT_SBOM
   with SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  SBOM Format Types
   ---------------------------------------------------------------------------

   --  Supported SBOM output formats
   type SBOM_Format is
     (CycloneDX_1_5,    --  CycloneDX 1.5 JSON (primary format)
      SPDX_2_3);        --  SPDX 2.3 JSON

   --  Component type classification (CycloneDX compatible)
   type Component_Type is
     (Application,       --  Standalone application
      Framework,         --  Software framework
      Library,           --  Software library
      Container,         --  Container image
      Operating_System,  --  Operating system
      Device,            --  Hardware device
      Firmware,          --  Device firmware
      File,              --  Individual file
      Platform,          --  Computing platform
      Machine_Learning,  --  ML model
      Data);             --  Data asset

   --  Component scope (where the component is used)
   type Component_Scope is
     (Required,          --  Required for runtime
      Optional,          --  Optional/plugin
      Excluded);         --  Excluded from final build

   --  Hash algorithm identifiers (aligned with Cerro_Manifest)
   type SBOM_Hash_Algorithm is
     (SHA_256,
      SHA_384,
      SHA_512,
      BLAKE3,
      SHA3_256,
      SHA3_512);

   ---------------------------------------------------------------------------
   --  Hash Value
   ---------------------------------------------------------------------------

   type SBOM_Hash is record
      Algorithm : SBOM_Hash_Algorithm;
      Content   : Unbounded_String;  --  Hex-encoded hash value
   end record;

   package Hash_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => SBOM_Hash);

   subtype Hash_List is Hash_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  External Reference Types
   ---------------------------------------------------------------------------

   --  External reference type (for linking to external resources)
   type External_Reference_Type is
     (VCS,                --  Version control system
      Issue_Tracker,      --  Issue/bug tracker
      Website,            --  Project website
      Advisories,         --  Security advisories
      BOM,                --  Bill of Materials reference
      Mailing_List,       --  Mailing list
      Social,             --  Social media
      Chat,               --  Chat/messaging
      Documentation,      --  Documentation
      Support,            --  Support portal
      Distribution,       --  Distribution channel
      License,            --  License information
      Build_Meta,         --  Build metadata
      Build_System,       --  Build system
      Release_Notes,      --  Release notes
      Other);             --  Other reference

   type External_Reference is record
      Ref_Type : External_Reference_Type;
      URL      : Unbounded_String;
      Comment  : Unbounded_String;  --  Optional description
   end record;

   package External_Ref_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => External_Reference);

   subtype External_Ref_List is External_Ref_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  License Information
   ---------------------------------------------------------------------------

   type License_Info is record
      SPDX_ID    : Unbounded_String;  --  SPDX license identifier
      Name       : Unbounded_String;  --  Full license name
      URL        : Unbounded_String;  --  URL to license text
      Expression : Unbounded_String;  --  Full SPDX expression (e.g., "MIT OR Apache-2.0")
   end record;

   package License_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => License_Info);

   subtype License_List is License_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  Package Identifiers (purl, CPE)
   ---------------------------------------------------------------------------

   --  Package URL (purl) - standard package identifier
   --  Format: pkg:type/namespace/name@version?qualifiers#subpath
   type Package_URL is record
      Pkg_Type    : Unbounded_String;  --  e.g., "cerro", "deb", "rpm", "cargo"
      Namespace   : Unbounded_String;  --  e.g., organization or maintainer
      Name        : Unbounded_String;  --  Package name
      Version     : Unbounded_String;  --  Package version
      Qualifiers  : Unbounded_String;  --  Optional qualifiers (arch, os, etc.)
      Subpath     : Unbounded_String;  --  Optional subpath
   end record;

   --  CPE (Common Platform Enumeration) - vulnerability identification
   --  Format: cpe:2.3:part:vendor:product:version:update:edition:language:sw_edition:target_sw:target_hw:other
   type CPE_Identifier is record
      Part        : Unbounded_String;  --  a=application, o=os, h=hardware
      Vendor      : Unbounded_String;  --  Vendor/maintainer
      Product     : Unbounded_String;  --  Product name
      Version     : Unbounded_String;  --  Version string
      Update      : Unbounded_String;  --  Update/patch level
      Edition     : Unbounded_String;  --  Edition
      Language    : Unbounded_String;  --  Language
   end record;

   ---------------------------------------------------------------------------
   --  Component Definition
   ---------------------------------------------------------------------------

   type SBOM_Component is record
      --  Identity
      BOM_Ref           : Unbounded_String;  --  Unique reference within this SBOM
      Comp_Type         : Component_Type;
      Name              : Unbounded_String;
      Version           : Unbounded_String;
      Description       : Unbounded_String;

      --  Identifiers
      PURL              : Package_URL;
      CPE               : CPE_Identifier;

      --  Provenance
      Author            : Unbounded_String;  --  Original author
      Publisher         : Unbounded_String;  --  Package publisher/maintainer
      Supplier          : Unbounded_String;  --  Supplier organization

      --  Hashes
      Hashes            : Hash_List;

      --  Licensing
      Licenses          : License_List;
      Copyright         : Unbounded_String;

      --  Scope and purpose
      Scope             : Component_Scope;

      --  External references
      External_Refs     : External_Ref_List;
   end record;

   package Component_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => SBOM_Component);

   subtype Component_List is Component_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  Dependency Relationship Types
   ---------------------------------------------------------------------------

   --  Relationship types (CycloneDX/SPDX compatible)
   type Dependency_Relationship is
     (Depends_On,            --  Runtime dependency
      Dev_Dependency,        --  Development/build dependency
      Optional_Dependency,   --  Optional/suggested dependency
      Provided_Dependency,   --  Virtual package provided
      Build_Dependency,      --  Build-time only
      Test_Dependency,       --  Test/check dependency
      Ancestor_Of,           --  Previous version of
      Descendant_Of,         --  Derived from
      Variant_Of,            --  Alternative variant
      Generates,             --  Produces/generates
      Contains,              --  Contains/bundles
      Contained_By);         --  Is contained within

   type Dependency_Edge is record
      Ref        : Unbounded_String;  --  BOM reference of source component
      Depends_On : Unbounded_String;  --  BOM reference of target dependency
      Relation   : Dependency_Relationship;
   end record;

   package Dependency_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Dependency_Edge);

   subtype Dependency_Graph is Dependency_Vectors.Vector;

   ---------------------------------------------------------------------------
   --  SBOM Metadata
   ---------------------------------------------------------------------------

   --  Tool that generated the SBOM
   type SBOM_Tool is record
      Vendor  : Unbounded_String;
      Name    : Unbounded_String;
      Version : Unbounded_String;
   end record;

   --  SBOM document metadata
   type SBOM_Metadata is record
      --  Document identification
      Serial_Number   : Unbounded_String;  --  Unique SBOM identifier (UUID)
      Version         : Positive := 1;      --  SBOM version number
      Timestamp       : Ada.Calendar.Time;

      --  Document creator
      Tool            : SBOM_Tool;
      Authors         : Unbounded_String;  --  Comma-separated author list

      --  Primary component (main package being described)
      Component       : SBOM_Component;

      --  Supplier of the SBOM
      Supplier        : Unbounded_String;
   end record;

   ---------------------------------------------------------------------------
   --  Complete SBOM Document
   ---------------------------------------------------------------------------

   type SBOM_Document is record
      Format       : SBOM_Format := CycloneDX_1_5;
      Metadata     : SBOM_Metadata;
      Components   : Component_List;
      Dependencies : Dependency_Graph;
   end record;

   ---------------------------------------------------------------------------
   --  Generation Options
   ---------------------------------------------------------------------------

   type SBOM_Options is record
      Format              : SBOM_Format := CycloneDX_1_5;
      Include_Hashes      : Boolean := True;
      Include_Licenses    : Boolean := True;
      Include_External    : Boolean := True;
      Include_Purl        : Boolean := True;
      Include_Cpe         : Boolean := True;
      Include_Build_Deps  : Boolean := True;
      Include_Test_Deps   : Boolean := True;
      Include_Optional    : Boolean := False;
      Pretty_Print        : Boolean := True;
      Serial_Number       : Unbounded_String;  --  Custom UUID, or auto-generate
   end record;

   Default_Options : constant SBOM_Options :=
     (Format              => CycloneDX_1_5,
      Include_Hashes      => True,
      Include_Licenses    => True,
      Include_External    => True,
      Include_Purl        => True,
      Include_Cpe         => True,
      Include_Build_Deps  => True,
      Include_Test_Deps   => True,
      Include_Optional    => False,
      Pretty_Print        => True,
      Serial_Number       => Null_Unbounded_String);

   ---------------------------------------------------------------------------
   --  Generation Result
   ---------------------------------------------------------------------------

   type Generation_Error is
     (None,
      Invalid_Manifest,      --  Manifest missing required fields
      Invalid_License,       --  Unrecognized license expression
      Serialization_Error,   --  JSON serialization failed
      IO_Error);             --  File I/O error

   type Generation_Result (Success : Boolean := False) is record
      case Success is
         when True =>
            Document : SBOM_Document;
            JSON     : Unbounded_String;
         when False =>
            Error     : Generation_Error;
            Error_Msg : Unbounded_String;
      end case;
   end record;

   ---------------------------------------------------------------------------
   --  SBOM Generation Functions
   ---------------------------------------------------------------------------

   --  Generate SBOM from Cerro Torre manifest
   function Generate
     (Manifest : Cerro_Manifest.Manifest;
      Options  : SBOM_Options := Default_Options) return Generation_Result
   with Global => null;

   --  Generate SBOM from manifest file
   function Generate_From_File
     (Manifest_Path : String;
      Options       : SBOM_Options := Default_Options) return Generation_Result
   with Pre => Manifest_Path'Length > 0 and Manifest_Path'Length <= 4096;

   --  Generate and write SBOM to file
   function Generate_To_File
     (Manifest    : Cerro_Manifest.Manifest;
      Output_Path : String;
      Options     : SBOM_Options := Default_Options) return Generation_Error
   with Pre => Output_Path'Length > 0 and Output_Path'Length <= 4096;

   ---------------------------------------------------------------------------
   --  SBOM Serialization
   ---------------------------------------------------------------------------

   --  Serialize SBOM document to JSON string
   function To_JSON
     (Doc          : SBOM_Document;
      Pretty_Print : Boolean := True) return String
   with Global => null;

   --  Generate CycloneDX 1.5 JSON
   function To_CycloneDX_JSON
     (Doc          : SBOM_Document;
      Pretty_Print : Boolean := True) return String
   with Global => null;

   --  Generate SPDX 2.3 JSON
   function To_SPDX_JSON
     (Doc          : SBOM_Document;
      Pretty_Print : Boolean := True) return String
   with Global => null;

   ---------------------------------------------------------------------------
   --  Identifier Generation
   ---------------------------------------------------------------------------

   --  Generate Package URL from manifest
   function Generate_PURL
     (Manifest : Cerro_Manifest.Manifest) return Package_URL
   with Global => null;

   --  Generate Package URL from component info
   function Generate_PURL
     (Pkg_Type  : String;
      Namespace : String;
      Name      : String;
      Version   : String) return Package_URL
   with Global => null,
        Pre    => Name'Length > 0;

   --  Format Package URL as string
   function PURL_To_String (P : Package_URL) return String
   with Global => null;

   --  Generate CPE identifier from manifest
   function Generate_CPE
     (Manifest : Cerro_Manifest.Manifest) return CPE_Identifier
   with Global => null;

   --  Generate CPE identifier from component info
   function Generate_CPE
     (Vendor  : String;
      Product : String;
      Version : String) return CPE_Identifier
   with Global => null,
        Pre    => Product'Length > 0;

   --  Format CPE as CPE 2.3 string
   function CPE_To_String (C : CPE_Identifier) return String
   with Global => null;

   ---------------------------------------------------------------------------
   --  Component Conversion
   ---------------------------------------------------------------------------

   --  Convert manifest metadata to SBOM component
   function Manifest_To_Component
     (Manifest : Cerro_Manifest.Manifest) return SBOM_Component
   with Global => null;

   --  Convert dependency reference to SBOM component
   function Dependency_To_Component
     (Dep : Cerro_Manifest.Dependency_Reference) return SBOM_Component
   with Global => null;

   ---------------------------------------------------------------------------
   --  Hash Conversion
   ---------------------------------------------------------------------------

   --  Convert Cerro hash algorithm to SBOM algorithm
   function Convert_Hash_Algorithm
     (Algo : Cerro_Manifest.Hash_Algorithm) return SBOM_Hash_Algorithm
   with Global => null;

   --  Convert Cerro hash value to SBOM hash
   function Convert_Hash
     (Hash : Cerro_Manifest.Hash_Value) return SBOM_Hash
   with Global => null;

   ---------------------------------------------------------------------------
   --  License Processing
   ---------------------------------------------------------------------------

   --  Parse SPDX license expression
   function Parse_License_Expression
     (Expression : String) return License_List
   with Global => null;

   --  Validate SPDX license identifier
   function Is_Valid_SPDX_ID (ID : String) return Boolean
   with Global => null;

   ---------------------------------------------------------------------------
   --  UUID Generation
   ---------------------------------------------------------------------------

   --  Generate a new UUID v4 for SBOM serial number
   function Generate_UUID return String
   with Global => null;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   --  Get SBOM format name string
   function Format_Name (F : SBOM_Format) return String
   with Global => null;

   --  Get SBOM format version string
   function Format_Version (F : SBOM_Format) return String
   with Global => null;

   --  Validate SBOM document completeness
   function Is_Valid (Doc : SBOM_Document) return Boolean
   with Global => null;

   --  Get error message
   function Error_Message (E : Generation_Error) return String
   with Global => null;

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  CycloneDX schema information
   CycloneDX_Spec_Version : constant String := "1.5";
   CycloneDX_Schema_URL   : constant String :=
     "http://cyclonedx.org/schema/bom-1.5.schema.json";

   --  SPDX schema information
   SPDX_Spec_Version : constant String := "SPDX-2.3";
   SPDX_Schema_URL   : constant String :=
     "https://raw.githubusercontent.com/spdx/spdx-spec/v2.3/schemas/spdx-schema.json";

   --  Cerro Torre tool information
   CT_Vendor  : constant String := "Cerro Torre Project";
   CT_Name    : constant String := "cerro-torre";
   CT_Version : constant String := "0.2.0";

   --  Default purl type for Cerro packages
   CT_PURL_Type : constant String := "cerro";

end CT_SBOM;
