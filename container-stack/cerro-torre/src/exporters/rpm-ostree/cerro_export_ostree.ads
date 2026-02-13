-------------------------------------------------------------------------------
--  Cerro_Export_OSTree - rpm-ostree / OSTree Exporter
--
--  Exports Cerro Torre packages to OSTree repositories for use with
--  immutable operating systems like Fedora CoreOS, Silverblue, etc.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Cerro_Manifest;        use Cerro_Manifest;

package Cerro_Export_OSTree is

   ---------------------------------------------------------------------------
   --  Export Status
   ---------------------------------------------------------------------------

   type Export_Status is
      (Success,
       Invalid_Manifest,
       Repo_Init_Failed,
       Commit_Failed,
       Ref_Update_Failed);

   type Export_Result is record
      Status     : Export_Status := Invalid_Manifest;
      Commit_Id  : Unbounded_String;  -- OSTree commit checksum
      Ref        : Unbounded_String;  -- e.g., "cerro/x86_64/hello"
      Size_Bytes : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Repository Configuration
   ---------------------------------------------------------------------------

   type OSTree_Mode is (Archive_Z2, Bare, Bare_User);
   --  Archive_Z2: Compressed, suitable for HTTP serving
   --  Bare: Uncompressed, for local builds
   --  Bare_User: Unprivileged operation

   type Repo_Config is record
      Path        : Unbounded_String;
      Mode        : OSTree_Mode := Archive_Z2;
      Ref_Prefix  : Unbounded_String;  -- e.g., "cerro/x86_64"
      GPG_Sign    : Boolean := True;
      GPG_Key_Id  : Unbounded_String;
   end record;

   ---------------------------------------------------------------------------
   --  Export Operations
   ---------------------------------------------------------------------------

   function Export_Package
      (M      : Manifest;
       Config : Repo_Config) return Export_Result;
   --  Export a package to an OSTree repository.
   --  @param M The package manifest
   --  @param Config Repository configuration
   --  @return Export result with commit ID

   function Init_Repository (Config : Repo_Config) return Boolean;
   --  Initialize a new OSTree repository.
   --  @param Config Repository configuration
   --  @return True if initialization succeeded

   function Commit_Tree
      (Repo_Path : String;
       Tree_Path : String;
       Subject   : String;
       Ref       : String) return Export_Result;
   --  Commit a directory tree to OSTree.
   --  @param Repo_Path Path to OSTree repository
   --  @param Tree_Path Path to content to commit
   --  @param Subject Commit message
   --  @param Ref Branch reference
   --  @return Export result with commit ID

   ---------------------------------------------------------------------------
   --  rpm-ostree Integration
   ---------------------------------------------------------------------------

   function Create_Treefile
      (M : Manifest) return String;
   --  Generate an rpm-ostree treefile from manifest.
   --  @param M The manifest
   --  @return YAML treefile content

   function Compose_Tree
      (Treefile  : String;
       Repo_Path : String) return Export_Result;
   --  Run rpm-ostree compose tree.
   --  @param Treefile Path to treefile
   --  @param Repo_Path Path to OSTree repository
   --  @return Compose result

   ---------------------------------------------------------------------------
   --  Delta Generation
   ---------------------------------------------------------------------------

   function Generate_Static_Delta
      (Repo_Path : String;
       From_Rev  : String;
       To_Rev    : String) return Boolean;
   --  Generate a static delta between two commits.
   --  @param Repo_Path Path to OSTree repository
   --  @param From_Rev Source revision (or empty for full)
   --  @param To_Rev Target revision
   --  @return True if delta generated

   ---------------------------------------------------------------------------
   --  Summary Updates
   ---------------------------------------------------------------------------

   function Update_Summary
      (Repo_Path : String;
       GPG_Key   : String := "") return Boolean;
   --  Update repository summary file.
   --  @param Repo_Path Path to OSTree repository
   --  @param GPG_Key Key ID for signing (empty = no signing)
   --  @return True if summary updated

end Cerro_Export_OSTree;
