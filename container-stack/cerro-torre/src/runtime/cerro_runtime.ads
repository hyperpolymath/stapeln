--  Cerro_Runtime - Container runtime execution
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Manages container runtime detection and execution for ct run.
--  Supports: svalinn, podman, nerdctl, docker (in preference order)

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Cerro_Runtime is

   --  Available container runtimes (FOSS-first preference order)
   type Runtime_Kind is
     (Runtime_Svalinn,   --  Svalinn (recommended, formally verified)
      Runtime_Podman,    --  Podman (FOSS, rootless)
      Runtime_Nerdctl,   --  containerd/nerdctl (FOSS)
      Runtime_Docker);   --  Docker (fallback)

   --  Runtime detection result
   type Runtime_Info is record
      Kind      : Runtime_Kind;
      Available : Boolean;
      Path      : Unbounded_String;  --  Full path to executable
      Version   : Unbounded_String;  --  Version string
      Rootless  : Boolean;           --  Running in rootless mode
   end record;

   --  Execution result
   type Run_Result is record
      Success       : Boolean;
      Exit_Code     : Integer;
      Container_ID  : Unbounded_String;
      Error_Message : Unbounded_String;
   end record;

   --  Run options
   type Run_Options is record
      Runtime      : Runtime_Kind := Runtime_Svalinn;
      Image_Path   : Unbounded_String;  --  Path to OCI tarball or image ref
      Detach       : Boolean := False;
      Ports        : Unbounded_String;  --  Port mappings "-p 8080:80"
      Volumes      : Unbounded_String;  --  Volume mounts "-v /host:/container"
      Environment  : Unbounded_String;  --  Environment vars "-e FOO=bar"
      Extra_Args   : Unbounded_String;  --  Additional runtime arguments
   end record;

   --  Detect all available runtimes
   procedure Detect_Runtimes;

   --  Check if a specific runtime is available
   function Is_Available (Kind : Runtime_Kind) return Boolean;

   --  Get info for a specific runtime
   function Get_Runtime_Info (Kind : Runtime_Kind) return Runtime_Info;

   --  Get the best available runtime (FOSS-first preference)
   function Get_Preferred_Runtime return Runtime_Kind;

   --  Parse runtime name from string (e.g., "podman" -> Runtime_Podman)
   function Parse_Runtime_Name (Name : String) return Runtime_Kind;

   --  Get runtime command name
   function Runtime_Command (Kind : Runtime_Kind) return String;

   --  Load an OCI tarball into the runtime
   function Load_Image
     (Kind       : Runtime_Kind;
      Tarball    : String;
      Image_Name : String := "") return Run_Result;

   --  Run a container
   function Run_Container (Opts : Run_Options) return Run_Result;

   --  Check if Svalinn API is reachable
   function Check_Svalinn_Endpoint (Endpoint : String) return Boolean;

end Cerro_Runtime;
