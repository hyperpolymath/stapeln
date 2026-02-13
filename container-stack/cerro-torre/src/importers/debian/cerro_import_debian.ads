-------------------------------------------------------------------------------
--  Cerro_Import_Debian - Debian Source Package Importer
--
--  Imports Debian source packages (.dsc) and converts them to Cerro Torre
--  manifest format with full provenance tracking.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Cerro_Manifest;        use Cerro_Manifest;

package Cerro_Import_Debian is

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
   --  Import a Debian source package.
   --  @param Package_Name Debian package name (e.g., "nginx")
   --  @param Version Specific version or empty for latest
   --  @return Import result with generated manifest

   function Import_From_Dsc (Dsc_Path : String) return Import_Result;
   --  Import from a local .dsc file.
   --  @param Dsc_Path Path to the .dsc file
   --  @return Import result with generated manifest

   function Import_From_Apt_Source
      (Package_Name : String;
       Release      : String := "stable") return Import_Result;
   --  Import from Debian APT repositories.
   --  @param Package_Name Package name
   --  @param Release Debian release (stable, testing, unstable, etc.)
   --  @return Import result

   ---------------------------------------------------------------------------
   --  DSC Parsing
   ---------------------------------------------------------------------------

   type Dsc_Info is record
      Source         : Unbounded_String;
      Version        : Unbounded_String;
      Maintainer     : Unbounded_String;
      Build_Depends  : Unbounded_String;
      Orig_Tarball   : Unbounded_String;
      Orig_Hash      : Unbounded_String;
      Debian_Tarball : Unbounded_String;
      Debian_Hash    : Unbounded_String;
   end record;

   function Parse_Dsc (Content : String) return Dsc_Info;
   --  Parse a .dsc file content.
   --  @param Content The .dsc file content
   --  @return Parsed DSC information

   ---------------------------------------------------------------------------
   --  Repository Configuration
   ---------------------------------------------------------------------------

   type Mirror_Config is record
      URL      : Unbounded_String;
      Release  : Unbounded_String;
      Sections : Unbounded_String;  -- "main contrib non-free"
   end record;

   Default_Mirror : constant Mirror_Config :=
      (URL      => To_Unbounded_String ("https://deb.debian.org/debian"),
       Release  => To_Unbounded_String ("stable"),
       Sections => To_Unbounded_String ("main"));

   procedure Set_Mirror (Config : Mirror_Config);
   --  Configure the Debian mirror to use.
   --  @param Config Mirror configuration

end Cerro_Import_Debian;
