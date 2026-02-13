-------------------------------------------------------------------------------
--  Cerro_Import_Fedora - Fedora SRPM Importer
--
--  Imports Fedora source RPMs and converts them to Cerro Torre manifest
--  format with full provenance tracking.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Cerro_Manifest;        use Cerro_Manifest;

package Cerro_Import_Fedora is

   ---------------------------------------------------------------------------
   --  Import Status
   ---------------------------------------------------------------------------

   type Import_Status is
      (Success,
       Package_Not_Found,
       Download_Failed,
       Parse_Error,
       Hash_Mismatch,
       Unsupported_Format);

   type Import_Result is record
      Status   : Import_Status := Package_Not_Found;
      Manifest : Cerro_Manifest.Manifest;
      Errors   : Unbounded_String;
   end record;

   ---------------------------------------------------------------------------
   --  Import Operations
   ---------------------------------------------------------------------------

   function Import_Package
      (Package_Name : String;
       Version      : String := "") return Import_Result;
   --  Import a Fedora source package.
   --  @param Package_Name Fedora package name
   --  @param Version Specific version or empty for latest
   --  @return Import result with generated manifest

   function Import_From_Srpm (Srpm_Path : String) return Import_Result;
   --  Import from a local .src.rpm file.
   --  @param Srpm_Path Path to the source RPM
   --  @return Import result with generated manifest

   function Import_From_Koji
      (Package_Name : String;
       Build_Id     : Natural := 0) return Import_Result;
   --  Import from Fedora Koji build system.
   --  @param Package_Name Package name
   --  @param Build_Id Specific build ID or 0 for latest
   --  @return Import result

   ---------------------------------------------------------------------------
   --  Spec Parsing
   ---------------------------------------------------------------------------

   type Spec_Info is record
      Name          : Unbounded_String;
      Version       : Unbounded_String;
      Release       : Unbounded_String;
      Source0       : Unbounded_String;
      Source0_Hash  : Unbounded_String;
      Build_Requires : Unbounded_String;
      Patches       : Unbounded_String;  -- Newline-separated
   end record;

   function Parse_Spec (Content : String) return Spec_Info;
   --  Parse an RPM .spec file content.
   --  @param Content The spec file content
   --  @return Parsed spec information

end Cerro_Import_Fedora;
