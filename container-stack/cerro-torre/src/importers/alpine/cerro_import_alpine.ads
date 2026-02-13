-------------------------------------------------------------------------------
--  Cerro_Import_Alpine - Alpine APKBUILD Importer
--
--  Imports Alpine Linux packages and converts them to Cerro Torre manifest
--  format with full provenance tracking.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Cerro_Manifest;        use Cerro_Manifest;

package Cerro_Import_Alpine is

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
   --  Import an Alpine package.
   --  @param Package_Name Alpine package name
   --  @param Version Specific version or empty for latest
   --  @return Import result with generated manifest

   function Import_From_Apkbuild (Path : String) return Import_Result;
   --  Import from a local APKBUILD file.
   --  @param Path Path to the APKBUILD file
   --  @return Import result with generated manifest

   function Import_From_Aports
      (Package_Name : String;
       Branch       : String := "master") return Import_Result;
   --  Import from Alpine aports git repository.
   --  @param Package_Name Package name
   --  @param Branch Git branch (master, 3.19-stable, etc.)
   --  @return Import result

   ---------------------------------------------------------------------------
   --  APKBUILD Parsing
   ---------------------------------------------------------------------------

   type Apkbuild_Info is record
      Pkgname     : Unbounded_String;
      Pkgver      : Unbounded_String;
      Pkgrel      : Unbounded_String;
      Source      : Unbounded_String;
      Sha512sums  : Unbounded_String;
      Makedepends : Unbounded_String;
      Depends     : Unbounded_String;
   end record;

   function Parse_Apkbuild (Content : String) return Apkbuild_Info;
   --  Parse an APKBUILD file content.
   --  @param Content The APKBUILD content
   --  @return Parsed information

end Cerro_Import_Alpine;
