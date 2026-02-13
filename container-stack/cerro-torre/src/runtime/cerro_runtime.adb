--  Cerro_Runtime - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later

with Ada.Environment_Variables;
with Ada.Directories;
with GNAT.OS_Lib;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Cerro_Runtime is

   package Env renames Ada.Environment_Variables;
   package Dir renames Ada.Directories;
   package OS renames GNAT.OS_Lib;

   --  Runtime registry
   type Runtime_Registry is array (Runtime_Kind) of Runtime_Info;
   Runtimes : Runtime_Registry := [others => (
      Kind      => Runtime_Svalinn,
      Available => False,
      Path      => Null_Unbounded_String,
      Version   => Null_Unbounded_String,
      Rootless  => False
   )];

   Detection_Done : Boolean := False;

   ---------------------------------------------------------------------------
   --  Find_Executable - Search PATH for an executable
   ---------------------------------------------------------------------------

   function Find_Executable (Name : String) return String is
      Path_Var : constant String := Env.Value ("PATH", "");
      Start    : Natural := Path_Var'First;
      Colon    : Natural;
   begin
      if Path_Var'Length = 0 then
         return "";
      end if;

      --  Search each PATH component
      loop
         --  Find next colon or end
         Colon := Ada.Strings.Fixed.Index (Path_Var, ":", Start);
         if Colon = 0 then
            Colon := Path_Var'Last + 1;
         end if;

         --  Check this directory
         declare
            Dir_Path  : constant String := Path_Var (Start .. Colon - 1);
            Full_Path : constant String := Dir_Path & "/" & Name;
         begin
            if Dir.Exists (Full_Path) then
               return Full_Path;
            end if;
         end;

         exit when Colon > Path_Var'Last;
         Start := Colon + 1;
      end loop;

      return "";
   end Find_Executable;

   ---------------------------------------------------------------------------
   --  Get_Command_Output - Run a command and capture output
   ---------------------------------------------------------------------------

   function Get_Command_Output
     (Program : String;
      Args    : OS.Argument_List) return String
   is
      use type OS.File_Descriptor;

      Temp_File : constant String := "/tmp/ct_runtime_" &
         OS.Pid_To_Integer (OS.Current_Process_Id)'Image & ".tmp";
      Result_FD : OS.File_Descriptor;
      Success   : Boolean;
      Return_Code : Integer;
      Output    : Unbounded_String := Null_Unbounded_String;
   begin
      --  Create output file
      Result_FD := OS.Create_Output_Text_File (Temp_File);
      if Result_FD = OS.Invalid_FD then
         return "";
      end if;
      OS.Close (Result_FD);

      --  Spawn process with output redirect
      OS.Spawn
        (Program_Name => Program,
         Args         => Args,
         Output_File  => Temp_File,
         Success      => Success,
         Return_Code  => Return_Code,
         Err_To_Out   => True);

      if not Success or Return_Code /= 0 then
         OS.Delete_File (Temp_File, Success);
         return "";
      end if;

      --  Read output file
      declare
         File : Ada.Text_IO.File_Type;
         Line : String (1 .. 1024);
         Last : Natural;
      begin
         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Temp_File);
         while not Ada.Text_IO.End_Of_File (File) loop
            Ada.Text_IO.Get_Line (File, Line, Last);
            if Length (Output) > 0 then
               Append (Output, ASCII.LF);
            end if;
            Append (Output, Line (1 .. Last));
         end loop;
         Ada.Text_IO.Close (File);
      exception
         when others =>
            if Ada.Text_IO.Is_Open (File) then
               Ada.Text_IO.Close (File);
            end if;
      end;

      OS.Delete_File (Temp_File, Success);
      return To_String (Output);
   end Get_Command_Output;

   ---------------------------------------------------------------------------
   --  Check_Runtime - Check if a runtime is available
   ---------------------------------------------------------------------------

   procedure Check_Runtime (Kind : Runtime_Kind) is
      Command : constant String := Runtime_Command (Kind);
      Path    : constant String := Find_Executable (Command);
      Version_Args : OS.Argument_List (1 .. 1);
   begin
      Runtimes (Kind).Kind := Kind;

      if Path'Length = 0 then
         Runtimes (Kind).Available := False;
         return;
      end if;

      Runtimes (Kind).Path := To_Unbounded_String (Path);

      --  Get version
      Version_Args (1) := new String'("--version");
      declare
         Version : constant String := Get_Command_Output (Path, Version_Args);
      begin
         if Version'Length > 0 then
            Runtimes (Kind).Available := True;
            --  Extract first line only
            for I in Version'Range loop
               if Version (I) = ASCII.LF then
                  Runtimes (Kind).Version := To_Unbounded_String (Version (Version'First .. I - 1));
                  exit;
               end if;
            end loop;
            if Length (Runtimes (Kind).Version) = 0 then
               Runtimes (Kind).Version := To_Unbounded_String (Version);
            end if;
         end if;
      end;
      OS.Free (Version_Args (1));

      --  Check rootless mode for podman/docker
      case Kind is
         when Runtime_Podman | Runtime_Nerdctl =>
            --  These are typically rootless by default
            Runtimes (Kind).Rootless := True;

         when Runtime_Docker =>
            --  Check if running rootless Docker
            declare
               Context_Args : OS.Argument_List (1 .. 2);
            begin
               Context_Args (1) := new String'("context");
               Context_Args (2) := new String'("show");
               declare
                  Context : constant String := Get_Command_Output (Path, Context_Args);
               begin
                  Runtimes (Kind).Rootless :=
                     Ada.Strings.Fixed.Index (Context, "rootless") > 0;
               end;
               OS.Free (Context_Args (1));
               OS.Free (Context_Args (2));
            end;

         when Runtime_Svalinn =>
            --  Svalinn is rootless by design
            Runtimes (Kind).Rootless := True;
      end case;
   end Check_Runtime;

   ---------------------------------------------------------------------------
   --  Detect_Runtimes - Detect all available runtimes
   ---------------------------------------------------------------------------

   procedure Detect_Runtimes is
   begin
      if Detection_Done then
         return;
      end if;

      for Kind in Runtime_Kind'Range loop
         Check_Runtime (Kind);
      end loop;

      Detection_Done := True;
   end Detect_Runtimes;

   ---------------------------------------------------------------------------
   --  Is_Available
   ---------------------------------------------------------------------------

   function Is_Available (Kind : Runtime_Kind) return Boolean is
   begin
      if not Detection_Done then
         Detect_Runtimes;
      end if;
      return Runtimes (Kind).Available;
   end Is_Available;

   ---------------------------------------------------------------------------
   --  Get_Runtime_Info
   ---------------------------------------------------------------------------

   function Get_Runtime_Info (Kind : Runtime_Kind) return Runtime_Info is
   begin
      if not Detection_Done then
         Detect_Runtimes;
      end if;
      return Runtimes (Kind);
   end Get_Runtime_Info;

   ---------------------------------------------------------------------------
   --  Get_Preferred_Runtime - FOSS-first preference
   ---------------------------------------------------------------------------

   function Get_Preferred_Runtime return Runtime_Kind is
   begin
      if not Detection_Done then
         Detect_Runtimes;
      end if;

      --  FOSS-first: svalinn > podman > nerdctl > docker
      if Runtimes (Runtime_Svalinn).Available then
         return Runtime_Svalinn;
      elsif Runtimes (Runtime_Podman).Available then
         return Runtime_Podman;
      elsif Runtimes (Runtime_Nerdctl).Available then
         return Runtime_Nerdctl;
      elsif Runtimes (Runtime_Docker).Available then
         return Runtime_Docker;
      else
         --  Default to svalinn even if not available (will fail at run time)
         return Runtime_Svalinn;
      end if;
   end Get_Preferred_Runtime;

   ---------------------------------------------------------------------------
   --  Parse_Runtime_Name
   ---------------------------------------------------------------------------

   function Parse_Runtime_Name (Name : String) return Runtime_Kind is
      Lower_Name : String := Name;
   begin
      --  Convert to lowercase
      for C of Lower_Name loop
         if C in 'A' .. 'Z' then
            C := Character'Val (Character'Pos (C) + 32);
         end if;
      end loop;

      if Lower_Name = "svalinn" then
         return Runtime_Svalinn;
      elsif Lower_Name = "podman" then
         return Runtime_Podman;
      elsif Lower_Name = "nerdctl" or Lower_Name = "containerd" then
         return Runtime_Nerdctl;
      elsif Lower_Name = "docker" then
         return Runtime_Docker;
      else
         --  Default to svalinn
         return Runtime_Svalinn;
      end if;
   end Parse_Runtime_Name;

   ---------------------------------------------------------------------------
   --  Runtime_Command
   ---------------------------------------------------------------------------

   function Runtime_Command (Kind : Runtime_Kind) return String is
   begin
      case Kind is
         when Runtime_Svalinn => return "svalinn";
         when Runtime_Podman  => return "podman";
         when Runtime_Nerdctl => return "nerdctl";
         when Runtime_Docker  => return "docker";
      end case;
   end Runtime_Command;

   ---------------------------------------------------------------------------
   --  Load_Image - Load an OCI tarball into the runtime
   ---------------------------------------------------------------------------

   function Load_Image
     (Kind       : Runtime_Kind;
      Tarball    : String;
      Image_Name : String := "") return Run_Result
   is
      pragma Unreferenced (Image_Name);

      Result : Run_Result := (
         Success       => False,
         Exit_Code     => 1,
         Container_ID  => Null_Unbounded_String,
         Error_Message => Null_Unbounded_String
      );

      Info : constant Runtime_Info := Get_Runtime_Info (Kind);
   begin
      if not Info.Available then
         Result.Error_Message := To_Unbounded_String (
            Runtime_Command (Kind) & " is not available");
         return Result;
      end if;

      --  For Svalinn, use the HTTP API instead
      if Kind = Runtime_Svalinn then
         Result.Error_Message := To_Unbounded_String (
            "Svalinn uses HTTP API, not load command");
         return Result;
      end if;

      --  Use podman/nerdctl/docker load
      declare
         Load_Args : OS.Argument_List (1 .. 3);
         Success   : Boolean;
         Path      : constant String := To_String (Info.Path);
      begin
         Load_Args (1) := new String'("load");
         Load_Args (2) := new String'("-i");
         Load_Args (3) := new String'(Tarball);

         OS.Spawn (Path, Load_Args, Success);

         OS.Free (Load_Args (1));
         OS.Free (Load_Args (2));
         OS.Free (Load_Args (3));

         Result.Success := Success;
         Result.Exit_Code := (if Success then 0 else 1);

         if not Result.Success then
            Result.Error_Message := To_Unbounded_String ("Load failed");
         end if;
      end;

      return Result;
   end Load_Image;

   ---------------------------------------------------------------------------
   --  Run_Container
   ---------------------------------------------------------------------------

   function Run_Container (Opts : Run_Options) return Run_Result is
      Result : Run_Result := (
         Success       => False,
         Exit_Code     => 1,
         Container_ID  => Null_Unbounded_String,
         Error_Message => Null_Unbounded_String
      );

      Info : constant Runtime_Info := Get_Runtime_Info (Opts.Runtime);
   begin
      if not Info.Available then
         Result.Error_Message := To_Unbounded_String (
            Runtime_Command (Opts.Runtime) & " is not available");
         return Result;
      end if;

      --  Build argument list
      --  run [-d] [ports] [volumes] [env] <image> [extra_args]
      declare
         Max_Args  : constant := 50;
         Args      : OS.Argument_List (1 .. Max_Args);
         Arg_Count : Natural := 0;
         Path      : constant String := To_String (Info.Path);
         Success   : Boolean;

         procedure Add_Arg (S : String) is
         begin
            Arg_Count := Arg_Count + 1;
            Args (Arg_Count) := new String'(S);
         end Add_Arg;

      begin
         Add_Arg ("run");

         if Opts.Detach then
            Add_Arg ("-d");
         end if;

         --  Add ports (simplified - assumes single port mapping in string)
         if Length (Opts.Ports) > 0 then
            Add_Arg ("-p");
            Add_Arg (To_String (Opts.Ports));
         end if;

         --  Add volumes
         if Length (Opts.Volumes) > 0 then
            Add_Arg ("-v");
            Add_Arg (To_String (Opts.Volumes));
         end if;

         --  Add environment
         if Length (Opts.Environment) > 0 then
            Add_Arg ("-e");
            Add_Arg (To_String (Opts.Environment));
         end if;

         --  Add image
         Add_Arg (To_String (Opts.Image_Path));

         --  Extra args (simplified - assumes single extra arg)
         if Length (Opts.Extra_Args) > 0 then
            Add_Arg (To_String (Opts.Extra_Args));
         end if;

         --  Execute
         OS.Spawn (Path, Args (1 .. Arg_Count), Success);

         --  Free arguments
         for I in 1 .. Arg_Count loop
            OS.Free (Args (I));
         end loop;

         Result.Success := Success;
         Result.Exit_Code := (if Success then 0 else 1);

         if not Result.Success then
            Result.Error_Message := To_Unbounded_String ("Run failed");
         end if;
      end;

      return Result;
   end Run_Container;

   ---------------------------------------------------------------------------
   --  Check_Svalinn_Endpoint
   ---------------------------------------------------------------------------

   function Check_Svalinn_Endpoint (Endpoint : String) return Boolean is
      pragma Unreferenced (Endpoint);
   begin
      --  TODO: HTTP health check to Svalinn API
      --  For now, check if svalinn binary exists
      return Is_Available (Runtime_Svalinn);
   end Check_Svalinn_Endpoint;

end Cerro_Runtime;
