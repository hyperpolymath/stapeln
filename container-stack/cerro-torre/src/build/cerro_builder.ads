-------------------------------------------------------------------------------
--  Cerro_Builder - Package Build Orchestration
--
--  This package handles the build process for Cerro Torre packages,
--  coordinating source retrieval, patch application, compilation,
--  and artifact generation.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Cerro_Manifest;        use Cerro_Manifest;

package Cerro_Builder is

   ---------------------------------------------------------------------------
   --  Build Status
   ---------------------------------------------------------------------------

   type Build_Status is
      (Success,
       Source_Fetch_Failed,
       Hash_Mismatch,
       Patch_Failed,
       Configure_Failed,
       Compile_Failed,
       Install_Failed,
       Package_Failed);

   type Build_Result is record
      Status       : Build_Status := Source_Fetch_Failed;
      Output_Path  : Unbounded_String;
      Build_Log    : Unbounded_String;
      Duration_Sec : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Build Configuration
   ---------------------------------------------------------------------------

   type Build_Options is record
      Work_Dir       : Unbounded_String;  -- Build workspace
      Output_Dir     : Unbounded_String;  -- Where to put artifacts
      Parallel_Jobs  : Positive := 1;     -- -j for make
      Verbose        : Boolean := False;
      Keep_Build_Dir : Boolean := False;  -- Don't clean up after build
      Reproducible   : Boolean := True;   -- Attempt reproducible build
   end record;

   Default_Options : constant Build_Options := (others => <>);

   ---------------------------------------------------------------------------
   --  Build Operations
   ---------------------------------------------------------------------------

   function Build_Package
      (M       : Manifest;
       Options : Build_Options := Default_Options) return Build_Result;
   --  Build a package from its manifest.
   --  @param M The package manifest
   --  @param Options Build configuration
   --  @return Build result with status and artifacts

   function Fetch_Source (M : Manifest; Dest : String) return Build_Status;
   --  Download and verify upstream source.
   --  @param M The manifest with source URL and hash
   --  @param Dest Destination directory
   --  @return Status (Success or Source_Fetch_Failed/Hash_Mismatch)

   function Apply_Patches
      (M          : Manifest;
       Source_Dir : String) return Build_Status;
   --  Apply all patches from the manifest in order.
   --  @param M The manifest with patch list
   --  @param Source_Dir Directory containing unpacked source
   --  @return Status (Success or Patch_Failed)

   function Run_Build
      (M          : Manifest;
       Source_Dir : String;
       Options    : Build_Options) return Build_Status;
   --  Execute the build commands from the manifest.
   --  @param M The manifest with build instructions
   --  @param Source_Dir Directory with patched source
   --  @param Options Build options (jobs, etc.)
   --  @return Status (Success or Configure/Compile/Install_Failed)

   ---------------------------------------------------------------------------
   --  Reproducibility Support
   ---------------------------------------------------------------------------

   type Reproducibility_Check is record
      Is_Reproducible : Boolean := False;
      First_Hash      : Unbounded_String;
      Second_Hash     : Unbounded_String;
      Differing_Files : Natural := 0;
   end record;

   function Check_Reproducibility
      (M       : Manifest;
       Options : Build_Options) return Reproducibility_Check;
   --  Build package twice and compare results.
   --  @param M The manifest
   --  @param Options Build options
   --  @return Reproducibility check results

end Cerro_Builder;
