-------------------------------------------------------------------------------
--  Cerro_Export_OCI - Implementation
-------------------------------------------------------------------------------

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.SHA256;

package body Cerro_Export_OCI is

   function Export_Package
      (M      : Manifest;
       Config : OCI_Config := Default_Config) return Export_Result
   is
      pragma Unreferenced (M, Config);
   begin
      --  TODO: Implement OCI image building
      return (Status => Invalid_Manifest, others => <>);
   end Export_Package;

   function Export_To_Tarball
      (M           : Manifest;
       Output_Path : String;
       Config      : OCI_Config := Default_Config) return Export_Result
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use GNAT.OS_Lib;

      Work_Dir     : constant String := "/tmp/cerro-oci-" & To_String (M.Metadata.Name);
      Rootfs_Dir   : constant String := Work_Dir & "/rootfs";
      Layer_Dir    : constant String := Work_Dir & "/layer";
      Layer_Tar    : constant String := Layer_Dir & "/layer.tar";
      Config_Path  : constant String := Work_Dir & "/config.json";
      Manifest_Path : constant String := Work_Dir & "/manifest.json";
      Result       : Export_Result;
      Success_Flag : Boolean;
      Layer_Digest : Unbounded_String;
      Config_Digest : Unbounded_String;

      function Execute_Command (Cmd : String; Args : Argument_List) return Boolean is
         Exe_Path : String_Access := Locate_Exec_On_Path (Cmd);
         Exit_Status : Integer;
      begin
         if Exe_Path = null then
            return False;
         end if;

         Exit_Status := Spawn (Exe_Path.all, Args);
         Free (Exe_Path);
         return Exit_Status = 0;
      end Execute_Command;

      function Calculate_SHA256 (File_Path : String) return String is
         File : File_Type;
         Buffer : String (1 .. 4096);
         Last : Natural;
         Ctx : GNAT.SHA256.Context := GNAT.SHA256.Initial_Context;
      begin
         Open (File, In_File, File_Path);

         loop
            exit when End_Of_File (File);
            Get_Line (File, Buffer, Last);
            GNAT.SHA256.Update (Ctx, Buffer (1 .. Last));

            --  Handle newline if not at EOF
            if not End_Of_File (File) then
               GNAT.SHA256.Update (Ctx, "" & Character'Val (10));
            end if;
         end loop;

         Close (File);
         return GNAT.SHA256.Digest (Ctx);

      exception
         when others =>
            if Is_Open (File) then
               Close (File);
            end if;
            return "";
      end Calculate_SHA256;

      procedure Write_JSON_File (Path : String; Content : String) is
         F : File_Type;
      begin
         Create (F, Out_File, Path);
         Put (F, Content);
         Close (F);
      end Write_JSON_File;

   begin
      --  Clean up any existing work directory
      if Exists (Work_Dir) then
         Delete_Tree (Work_Dir);
      end if;

      --  Create work directories
      Create_Path (Layer_Dir);

      --  Step 1: Create rootfs
      Success_Flag := Create_Rootfs (M, Work_Dir);
      if not Success_Flag then
         Result.Status := Build_Failed;
         return Result;
      end if;

      --  Step 2: Create layer tarball from rootfs
      declare
         Tar_Args : Argument_List :=
            (new String'("-czf"),
             new String'(Layer_Tar),
             new String'("-C"),
             new String'(Rootfs_Dir),
             new String'("."));
      begin
         Success_Flag := Execute_Command ("tar", Tar_Args);

         for Arg of Tar_Args loop
            Free (Arg);
         end loop;

         if not Success_Flag then
            Result.Status := Build_Failed;
            return Result;
         end if;
      end;

      --  Step 3: Calculate layer digest
      declare
         Layer_Hash : constant String := Calculate_SHA256 (Layer_Tar);
      begin
         if Layer_Hash = "" then
            Result.Status := Build_Failed;
            return Result;
         end if;
         Layer_Digest := To_Unbounded_String ("sha256:" & Layer_Hash);
      end;

      --  Step 4: Generate config.json with layer reference
      declare
         Config_JSON : constant String := Create_Config_Json (M, Config);
         LF : constant Character := Character'Val (10);
         Enhanced_Config : Unbounded_String;
      begin
         --  Parse and enhance config to include diff_ids
         Enhanced_Config := To_Unbounded_String (Config_JSON);

         --  Replace empty diff_ids array with actual layer digest
         declare
            Search_Str : constant String := """diff_ids"": []";
            Replace_Str : constant String :=
               """diff_ids"": [""" & To_String (Layer_Digest) & """]";
            Pos : Natural := Index (To_String (Enhanced_Config), Search_Str);
         begin
            if Pos > 0 then
               Replace_Slice (Enhanced_Config, Pos, Pos + Search_Str'Length - 1, Replace_Str);
            end if;
         end;

         Write_JSON_File (Config_Path, To_String (Enhanced_Config));
      end;

      --  Step 5: Calculate config digest
      declare
         Config_Hash : constant String := Calculate_SHA256 (Config_Path);
      begin
         if Config_Hash = "" then
            Result.Status := Build_Failed;
            return Result;
         end if;
         Config_Digest := To_Unbounded_String ("sha256:" & Config_Hash);
      end;

      --  Step 6: Create manifest.json (Docker load format)
      declare
         LF : constant Character := Character'Val (10);
         Manifest : Unbounded_String;
         Image_Name : constant String := To_String (M.Metadata.Name);
         Image_Version : constant String := To_String (M.Metadata.Version);
         Config_File : constant String := To_String (Config_Digest) (8 .. Length (Config_Digest)) & ".json";
      begin
         Append (Manifest, "[" & LF);
         Append (Manifest, "  {" & LF);
         Append (Manifest, "    ""Config"": """ & Config_File & """," & LF);
         Append (Manifest, "    ""RepoTags"": [""cerro-torre/" & Image_Name & ":" & Image_Version & """]," & LF);
         Append (Manifest, "    ""Layers"": [""layer/layer.tar""]" & LF);
         Append (Manifest, "  }" & LF);
         Append (Manifest, "]" & LF);

         Write_JSON_File (Manifest_Path, To_String (Manifest));

         --  Also copy config.json to the digest-named file
         declare
            Config_Dest : constant String := Work_Dir & "/" & Config_File;
            Copy_Args : Argument_List :=
               (new String'(Config_Path),
                new String'(Config_Dest));
         begin
            Success_Flag := Execute_Command ("cp", Copy_Args);

            for Arg of Copy_Args loop
               Free (Arg);
            end loop;

            if not Success_Flag then
               Result.Status := Build_Failed;
               return Result;
            end if;
         end;
      end;

      --  Step 7: Create final tarball
      declare
         Config_File : constant String := To_String (Config_Digest) (8 .. Length (Config_Digest)) & ".json";
         Tar_Args : Argument_List :=
            (new String'("-cf"),
             new String'(Output_Path),
             new String'("-C"),
             new String'(Work_Dir),
             new String'("manifest.json"),
             new String'(Config_File),
             new String'("layer"));
      begin
         Success_Flag := Execute_Command ("tar", Tar_Args);

         for Arg of Tar_Args loop
            Free (Arg);
         end loop;

         if not Success_Flag then
            Result.Status := Build_Failed;
            return Result;
         end if;
      end;

      --  Clean up work directory
      if Exists (Work_Dir) then
         Delete_Tree (Work_Dir);
      end if;

      --  Populate result
      Result.Status := Success;
      Result.Image_Ref := To_Unbounded_String (
         "cerro-torre/" & To_String (M.Metadata.Name) & ":" & To_String (M.Metadata.Version));
      Result.Digest := Config_Digest;
      Result.Layers := 1;

      --  Calculate output file size
      if Exists (Output_Path) then
         Result.Size_Bytes := Natural (Size (Output_Path));
      end if;

      return Result;

   exception
      when others =>
         --  Clean up on error
         if Exists (Work_Dir) then
            Delete_Tree (Work_Dir);
         end if;
         Result.Status := Build_Failed;
         return Result;
   end Export_To_Tarball;

   function Push_To_Registry
      (Image_Path : String;
       Registry   : String;
       Tag        : String) return Export_Status
   is
      pragma Unreferenced (Image_Path, Registry, Tag);
   begin
      --  TODO: Implement registry push (use skopeo or similar)
      return Registry_Error;
   end Push_To_Registry;

   function Create_Rootfs
      (M        : Manifest;
       Work_Dir : String) return Boolean
   is
      use Ada.Directories;
      use Ada.Text_IO;

      Rootfs_Dir : constant String := Work_Dir & "/rootfs";
   begin
      --  Create base rootfs directory
      if not Exists (Work_Dir) then
         Create_Directory (Work_Dir);
      end if;

      if not Exists (Rootfs_Dir) then
         Create_Directory (Rootfs_Dir);
      end if;

      --  Create standard FHS directories
      declare
         type Dir_List is array (Positive range <>) of access constant String;
         Standard_Dirs : constant Dir_List :=
            (new String'("bin"),
             new String'("etc"),
             new String'("lib"),
             new String'("lib64"),
             new String'("usr"),
             new String'("usr/bin"),
             new String'("usr/lib"),
             new String'("usr/local"),
             new String'("var"),
             new String'("tmp"));
      begin
         for Dir of Standard_Dirs loop
            declare
               Full_Path : constant String := Rootfs_Dir & "/" & Dir.all;
            begin
               if not Exists (Full_Path) then
                  Create_Directory (Full_Path);
               end if;
            end;
         end loop;
      end;

      --  TODO: Copy actual package files into rootfs
      --  This would require parsing M.Build.Outputs or similar
      --  to know what files to include

      --  Create a marker file to indicate rootfs is initialized
      declare
         Marker_File : File_Type;
         Marker_Path : constant String := Rootfs_Dir & "/.cerro-built";
      begin
         Create (Marker_File, Out_File, Marker_Path);
         Put_Line (Marker_File, "# Cerro Torre rootfs");
         Put_Line (Marker_File, "# Package: " & To_String (M.Metadata.Name));
         Put_Line (Marker_File, "# Version: " & To_String (M.Metadata.Version));
         Close (Marker_File);
      end;

      return True;

   exception
      when others =>
         return False;
   end Create_Rootfs;

   function Create_Config_Json
      (M      : Manifest;
       Config : OCI_Config) return String
   is
      LF : constant Character := Character'Val (10);
      Result : Unbounded_String;

      procedure Append_Line (S : String) is
      begin
         Append (Result, S & LF);
      end Append_Line;

      procedure Append_JSON_String_Field (Name, Value : String; Add_Comma : Boolean := True) is
      begin
         Append (Result, "    """ & Name & """: """ & Value & """");
         if Add_Comma then
            Append (Result, ",");
         end if;
         Append (Result, LF);
      end Append_JSON_String_Field;

      procedure Append_JSON_Array_Field (Name : String; Values : String; Add_Comma : Boolean := True) is
      begin
         Append (Result, "    """ & Name & """: [" & Values & "]");
         if Add_Comma then
            Append (Result, ",");
         end if;
         Append (Result, LF);
      end Append_JSON_Array_Field;

   begin
      Append_Line ("{");
      Append_JSON_String_Field ("created", "2026-01-22T00:00:00Z");
      Append_JSON_String_Field ("author", To_String (M.Metadata.Maintainer));
      Append_JSON_String_Field ("architecture", "amd64");
      Append_JSON_String_Field ("os", "linux");

      --  config section
      Append_Line ("  ""config"": {");

      if Length (Config.User) > 0 then
         Append_JSON_String_Field ("User", To_String (Config.User));
      end if;

      if Length (Config.Entrypoint) > 0 then
         Append_JSON_Array_Field ("Entrypoint", """" & To_String (Config.Entrypoint) & """");
      end if;

      if Length (Config.Cmd) > 0 then
         Append_JSON_Array_Field ("Cmd", """" & To_String (Config.Cmd) & """");
      end if;

      --  Default PATH
      Append_JSON_Array_Field ("Env", """PATH=/usr/local/bin:/usr/bin:/bin""");

      --  Labels
      Append_Line ("    ""Labels"": {}");
      Append_Line ("  },");

      --  rootfs section
      Append_Line ("  ""rootfs"": {");
      Append_JSON_String_Field ("type", "layers");
      Append_Line ("    ""diff_ids"": []");
      Append_Line ("  }");

      Append (Result, "}");

      return To_String (Result);
   end Create_Config_Json;

   function Attach_Provenance
      (Image_Ref : String;
       M         : Manifest) return Boolean
   is
      pragma Unreferenced (Image_Ref, M);
   begin
      --  TODO: Implement SLSA provenance attachment
      return False;
   end Attach_Provenance;

   function Attach_SBOM
      (Image_Ref : String;
       M         : Manifest) return Boolean
   is
      pragma Unreferenced (Image_Ref, M);
   begin
      --  TODO: Implement SBOM generation and attachment
      return False;
   end Attach_SBOM;

end Cerro_Export_OCI;
