-------------------------------------------------------------------------------
--  Cerro_Export_OCI - OCI Container Image Exporter
--
--  Exports Cerro Torre packages as OCI-compliant container images
--  that can be used with Podman, Docker, and other OCI runtimes.
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Cerro_Manifest;        use Cerro_Manifest;

package Cerro_Export_OCI is

   ---------------------------------------------------------------------------
   --  Export Status
   ---------------------------------------------------------------------------

   type Export_Status is
      (Success,
       Invalid_Manifest,
       Build_Failed,
       Registry_Error,
       Push_Failed);

   type Export_Result is record
      Status     : Export_Status := Invalid_Manifest;
      Image_Ref  : Unbounded_String;  -- e.g., "cerro-torre/hello:2.10-3"
      Digest     : Unbounded_String;  -- SHA256 of manifest
      Size_Bytes : Natural := 0;
      Layers     : Natural := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Export Configuration
   ---------------------------------------------------------------------------

   type OCI_Config is record
      Base_Image   : Unbounded_String;  -- Base image or "scratch"
      Registry     : Unbounded_String;  -- Target registry URL
      Repository   : Unbounded_String;  -- Repository name prefix
      Entrypoint   : Unbounded_String;  -- Container entrypoint
      Cmd          : Unbounded_String;  -- Default command
      User         : Unbounded_String;  -- Run as user (default: nonroot)
      Labels       : Unbounded_String;  -- OCI labels (key=value,...)
      Annotations  : Unbounded_String;  -- OCI annotations
   end record;

   Default_Config : constant OCI_Config :=
      (Base_Image  => To_Unbounded_String ("scratch"),
       User        => To_Unbounded_String ("65534:65534"),
       others      => <>);

   ---------------------------------------------------------------------------
   --  Export Operations
   ---------------------------------------------------------------------------

   function Export_Package
      (M      : Manifest;
       Config : OCI_Config := Default_Config) return Export_Result;
   --  Export a package as an OCI image.
   --  @param M The package manifest
   --  @param Config Export configuration
   --  @return Export result with image reference

   function Export_To_Tarball
      (M           : Manifest;
       Output_Path : String;
       Config      : OCI_Config := Default_Config) return Export_Result;
   --  Export package as OCI image tarball.
   --  @param M The package manifest
   --  @param Output_Path Path for the output .tar file
   --  @param Config Export configuration
   --  @return Export result

   function Push_To_Registry
      (Image_Path : String;
       Registry   : String;
       Tag        : String) return Export_Status;
   --  Push an OCI image tarball to a registry.
   --  @param Image_Path Path to OCI tarball
   --  @param Registry Registry URL
   --  @param Tag Image tag
   --  @return Push status

   ---------------------------------------------------------------------------
   --  Image Building
   ---------------------------------------------------------------------------

   function Create_Rootfs
      (M        : Manifest;
       Work_Dir : String) return Boolean;
   --  Create a root filesystem from package files.
   --  @param M The manifest with file list
   --  @param Work_Dir Working directory for rootfs
   --  @return True if successful

   function Create_Config_Json
      (M      : Manifest;
       Config : OCI_Config) return String;
   --  Generate OCI image config.json.
   --  @param M The manifest
   --  @param Config Export configuration
   --  @return JSON string for config

   ---------------------------------------------------------------------------
   --  Provenance Attestations
   ---------------------------------------------------------------------------

   function Attach_Provenance
      (Image_Ref : String;
       M         : Manifest) return Boolean;
   --  Attach SLSA provenance attestation to image.
   --  Uses Sigstore/cosign format for attestation.
   --  @param Image_Ref Image reference in registry
   --  @param M The manifest with provenance info
   --  @return True if attestation attached

   function Attach_SBOM
      (Image_Ref : String;
       M         : Manifest) return Boolean;
   --  Attach Software Bill of Materials to image.
   --  @param Image_Ref Image reference in registry
   --  @param M The manifest
   --  @return True if SBOM attached

end Cerro_Export_OCI;
