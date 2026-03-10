-------------------------------------------------------------------------------
--  Cerro_Export_OCI - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later
-------------------------------------------------------------------------------

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.SHA256;
with Interfaces;
with CT_Registry;
with Cerro_Crypto;

package body Cerro_Export_OCI is

   function Export_Package
      (M      : Manifest;
       Config : OCI_Config := Default_Config) return Export_Result
   is
      use Ada.Directories;

      Image_Name    : constant String := To_String (M.Metadata.Name);
      Image_Version : constant String := To_String (M.Metadata.Version.Upstream);
      Revision_Str  : constant String := Positive'Image (M.Metadata.Version.Revision);
      --  Build tag as "upstream-revision" (trim leading space from 'Image)
      Full_Tag      : constant String := Image_Version & "-" &
         Revision_Str (Revision_Str'First + 1 .. Revision_Str'Last);

      --  Determine output directory from config or use a default
      Repo_Prefix   : constant String :=
         (if Length (Config.Repository) > 0
          then To_String (Config.Repository) & "/"
          else "cerro-torre/");

      Output_Dir    : constant String := "/tmp/cerro-oci-export";
      Output_Path   : constant String := Output_Dir & "/" &
         Image_Name & "-" & Full_Tag & ".tar";

      Tarball_Result : Export_Result;
   begin
      --  Validate manifest has required fields
      if Length (M.Metadata.Name) = 0 then
         return (Status => Invalid_Manifest, others => <>);
      end if;

      if Length (M.Metadata.Version.Upstream) = 0 then
         return (Status => Invalid_Manifest, others => <>);
      end if;

      --  Ensure output directory exists
      if not Exists (Output_Dir) then
         Create_Path (Output_Dir);
      end if;

      --  Delegate to Export_To_Tarball which has the full pipeline
      Tarball_Result := Export_To_Tarball (M, Output_Path, Config);

      --  Adjust image reference to use the configured repository prefix
      if Tarball_Result.Status = Success then
         Tarball_Result.Image_Ref := To_Unbounded_String (
            Repo_Prefix & Image_Name & ":" & Full_Tag);
      end if;

      return Tarball_Result;

   exception
      when others =>
         return (Status => Build_Failed, others => <>);
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
      use Ada.Directories;
      use Ada.Text_IO;
      use CT_Registry;

      --  Parse the tag to extract repository and version
      --  Expected format: "cerro-torre/package:version" or just "package:version"
      Colon_Pos  : constant Natural := Index (Tag, ":");
      Repository : constant String :=
         (if Colon_Pos > Tag'First
          then Tag (Tag'First .. Colon_Pos - 1)
          else Tag);
      Image_Tag  : constant String :=
         (if Colon_Pos > Tag'First and then Colon_Pos < Tag'Last
          then Tag (Colon_Pos + 1 .. Tag'Last)
          else "latest");

      Client     : Registry_Client;
      Auth_Err   : CT_Registry.Registry_Error;
   begin
      --  Validate inputs
      if not Exists (Image_Path) then
         return Registry_Error;
      end if;

      if Registry'Length = 0 or else Tag'Length = 0 then
         return Registry_Error;
      end if;

      --  Create registry client
      Client := Create_Client (Registry);

      --  Authenticate with push permissions
      Auth_Err := Authenticate (Client, Repository, Actions => "pull,push");
      if Auth_Err /= CT_Registry.Success and then Auth_Err /= CT_Registry.Not_Implemented then
         --  Allow Not_Implemented (anonymous registries) to proceed
         if Auth_Err = CT_Registry.Auth_Required or else
            Auth_Err = CT_Registry.Auth_Failed
         then
            return Push_Failed;
         end if;
      end if;

      --  Step 1: Push the layer blob from the tarball
      --  Extract the layer tarball from the OCI image archive
      --  The OCI tarball contains: manifest.json, <config-hash>.json, layer/layer.tar
      declare
         Work_Dir  : constant String := "/tmp/cerro-push-" & Repository;
         Layer_Tar : constant String := Work_Dir & "/layer/layer.tar";
         Success_Flag : Boolean;

         function Execute_Command
            (Cmd : String; Args : GNAT.OS_Lib.Argument_List) return Boolean
         is
            use GNAT.OS_Lib;
            Exe_Path    : String_Access := Locate_Exec_On_Path (Cmd);
            Exit_Status : Integer;
         begin
            if Exe_Path = null then
               return False;
            end if;
            Exit_Status := Spawn (Exe_Path.all, Args);
            Free (Exe_Path);
            return Exit_Status = 0;
         end Execute_Command;

      begin
         --  Clean and create work directory
         if Exists (Work_Dir) then
            Delete_Tree (Work_Dir);
         end if;
         Create_Path (Work_Dir);

         --  Extract the OCI tarball
         declare
            use GNAT.OS_Lib;
            Tar_Args : Argument_List :=
               (new String'("-xf"),
                new String'(Image_Path),
                new String'("-C"),
                new String'(Work_Dir));
         begin
            Success_Flag := Execute_Command ("tar", Tar_Args);
            for Arg of Tar_Args loop
               Free (Arg);
            end loop;

            if not Success_Flag then
               if Exists (Work_Dir) then
                  Delete_Tree (Work_Dir);
               end if;
               return Build_Failed;
            end if;
         end;

         --  Step 2: Push the layer blob
         if not Exists (Layer_Tar) then
            if Exists (Work_Dir) then
               Delete_Tree (Work_Dir);
            end if;
            return Build_Failed;
         end if;

         declare
            Blob_Push_Result : CT_Registry.Push_Result;
         begin
            Blob_Push_Result := Push_Blob_From_File (
               Client     => Client,
               Repository => Repository,
               File_Path  => Layer_Tar,
               Media_Type => CT_Registry.OCI_Layer_Gzip);

            if Blob_Push_Result.Error /= CT_Registry.Success then
               if Exists (Work_Dir) then
                  Delete_Tree (Work_Dir);
               end if;
               return Push_Failed;
            end if;

            --  Step 3: Read the config JSON from the extracted archive
            --  Find the config file (named by its digest hash)
            declare
               Config_Content : Unbounded_String;
               Config_Found   : Boolean := False;

               --  Read the manifest.json to find the config filename
               Manifest_File  : File_Type;
               Manifest_Buf   : String (1 .. 8192);
               Manifest_Last  : Natural;
               Manifest_Path  : constant String := Work_Dir & "/manifest.json";
            begin
               --  Read manifest.json to find config file name
               if Exists (Manifest_Path) then
                  Open (Manifest_File, In_File, Manifest_Path);
                  Get_Line (Manifest_File, Manifest_Buf, Manifest_Last);

                  --  Read remaining lines if any
                  declare
                     Full_Manifest : Unbounded_String :=
                        To_Unbounded_String (Manifest_Buf (1 .. Manifest_Last));
                  begin
                     while not End_Of_File (Manifest_File) loop
                        Get_Line (Manifest_File, Manifest_Buf, Manifest_Last);
                        Append (Full_Manifest, Manifest_Buf (1 .. Manifest_Last));
                     end loop;
                     Close (Manifest_File);

                     --  Extract config filename from "Config": "sha256-hash.json"
                     declare
                        FM_Str       : constant String := To_String (Full_Manifest);
                        Config_Key   : constant String := """Config"": """;
                        Config_Start : constant Natural := Index (FM_Str, Config_Key);
                        Config_Name_Start : Natural;
                        Config_Name_End   : Natural;
                     begin
                        if Config_Start > 0 then
                           Config_Name_Start := Config_Start + Config_Key'Length;
                           Config_Name_End := Index (
                              FM_Str (Config_Name_Start .. FM_Str'Last), """");
                           if Config_Name_End > Config_Name_Start then
                              declare
                                 Config_Filename : constant String :=
                                    FM_Str (Config_Name_Start .. Config_Name_End - 1);
                                 Config_File_Path : constant String :=
                                    Work_Dir & "/" & Config_Filename;
                                 Config_File : File_Type;
                                 Config_Buf  : String (1 .. 8192);
                                 Config_Last : Natural;
                              begin
                                 if Exists (Config_File_Path) then
                                    Open (Config_File, In_File, Config_File_Path);
                                    Config_Content := Null_Unbounded_String;
                                    while not End_Of_File (Config_File) loop
                                       Get_Line (Config_File, Config_Buf, Config_Last);
                                       Append (Config_Content, Config_Buf (1 .. Config_Last));
                                    end loop;
                                    Close (Config_File);
                                    Config_Found := True;
                                 end if;
                              end;
                           end if;
                        end if;
                     end;
                  end;
               end if;

               if not Config_Found then
                  if Exists (Work_Dir) then
                     Delete_Tree (Work_Dir);
                  end if;
                  return Build_Failed;
               end if;

               --  Step 4: Push config blob
               declare
                  Config_Push_Result : CT_Registry.Push_Result;
               begin
                  Config_Push_Result := Push_Blob (
                     Client     => Client,
                     Repository => Repository,
                     Content    => To_String (Config_Content),
                     Media_Type => CT_Registry.OCI_Config_V1);

                  if Config_Push_Result.Error /= CT_Registry.Success then
                     if Exists (Work_Dir) then
                        Delete_Tree (Work_Dir);
                     end if;
                     return Push_Failed;
                  end if;

                  --  Step 5: Build and push OCI manifest
                  declare
                     OCI_Man : CT_Registry.OCI_Manifest;
                     Layer_Size : constant Interfaces.Unsigned_64 :=
                        Interfaces.Unsigned_64 (Size (Layer_Tar));
                     Config_Str : constant String := To_String (Config_Content);
                     Config_Size : constant Interfaces.Unsigned_64 :=
                        Interfaces.Unsigned_64 (Config_Str'Length);

                     Layer_Desc : CT_Registry.Blob_Descriptor;
                     Config_Desc : CT_Registry.Blob_Descriptor;
                     Man_Push_Result : CT_Registry.Push_Result;
                  begin
                     --  Build config descriptor
                     Config_Desc.Media_Type :=
                        To_Unbounded_String (CT_Registry.OCI_Config_V1);
                     Config_Desc.Digest := Config_Push_Result.Digest;
                     Config_Desc.Size := Config_Size;

                     --  Build layer descriptor
                     Layer_Desc.Media_Type :=
                        To_Unbounded_String (CT_Registry.OCI_Layer_Gzip);
                     Layer_Desc.Digest := Blob_Push_Result.Digest;
                     Layer_Desc.Size := Layer_Size;

                     --  Assemble OCI manifest
                     OCI_Man.Schema_Version := 2;
                     OCI_Man.Media_Type :=
                        To_Unbounded_String (CT_Registry.OCI_Manifest_V1);
                     OCI_Man.Config := Config_Desc;
                     OCI_Man.Layers.Append (Layer_Desc);

                     --  Push manifest with tag
                     Man_Push_Result := Push_Manifest (
                        Client     => Client,
                        Repository => Repository,
                        Tag        => Image_Tag,
                        Manifest   => OCI_Man);

                     --  Clean up work directory
                     if Exists (Work_Dir) then
                        Delete_Tree (Work_Dir);
                     end if;

                     if Man_Push_Result.Error /= CT_Registry.Success then
                        return Push_Failed;
                     end if;

                     return Success;
                  end;
               end;
            end;
         end;

      exception
         when others =>
            if Exists (Work_Dir) then
               Delete_Tree (Work_Dir);
            end if;
            return Registry_Error;
      end;
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
      use Ada.Text_IO;
      use Ada.Directories;

      LF : constant Character := Character'Val (10);
      Provenance_JSON : Unbounded_String;

      --  Build the output path for the provenance attestation
      --  Convention: <image-ref>.provenance.json (with / replaced by -)
      function Sanitize_Ref (Ref : String) return String is
         Result : String := Ref;
      begin
         for I in Result'Range loop
            if Result (I) = '/' or Result (I) = ':' or Result (I) = '@' then
               Result (I) := '-';
            end if;
         end loop;
         return Result;
      end Sanitize_Ref;

      Output_Dir  : constant String := "/tmp/cerro-provenance";
      Output_Path : constant String := Output_Dir & "/" &
         Sanitize_Ref (Image_Ref) & ".provenance.json";

      --  Extract source hash as hex string for the attestation
      Source_Digest_Hex : constant String :=
         Cerro_Crypto.Bytes_To_Hex (M.Provenance.Upstream_Hash.Digest);

      --  Compute manifest content hash for the subject
      Manifest_Content : constant String := Cerro_Manifest.To_String (M);
      Subject_Hash     : constant Cerro_Crypto.SHA256_Digest :=
         Cerro_Crypto.Compute_SHA256 (Manifest_Content);
      Subject_Hex      : constant String :=
         Cerro_Crypto.Bytes_To_Hex (Subject_Hash);

      --  Package identification
      Pkg_Name    : constant String := To_String (M.Metadata.Name);
      Pkg_Version : constant String := To_String (M.Metadata.Version.Upstream);
      Pkg_Rev_Str : constant String := Positive'Image (M.Metadata.Version.Revision);
      Pkg_Rev     : constant String :=
         Pkg_Rev_Str (Pkg_Rev_Str'First + 1 .. Pkg_Rev_Str'Last);

      File : File_Type;
   begin
      --  Generate SLSA Provenance v1.0 attestation in DSSE envelope format
      --  Reference: https://slsa.dev/spec/v1.0/provenance

      Append (Provenance_JSON, "{" & LF);
      Append (Provenance_JSON, "  ""_type"": ""https://in-toto.io/Statement/v1""," & LF);
      Append (Provenance_JSON, "  ""subject"": [" & LF);
      Append (Provenance_JSON, "    {" & LF);
      Append (Provenance_JSON, "      ""name"": """ & Pkg_Name & """," & LF);
      Append (Provenance_JSON, "      ""digest"": {" & LF);
      Append (Provenance_JSON, "        ""sha256"": """ & Subject_Hex & """" & LF);
      Append (Provenance_JSON, "      }" & LF);
      Append (Provenance_JSON, "    }" & LF);
      Append (Provenance_JSON, "  ]," & LF);
      Append (Provenance_JSON, "  ""predicateType"": ""https://slsa.dev/provenance/v1""," & LF);
      Append (Provenance_JSON, "  ""predicate"": {" & LF);

      --  Build definition
      Append (Provenance_JSON, "    ""buildDefinition"": {" & LF);
      Append (Provenance_JSON, "      ""buildType"": ""https://cerro-torre.dev/build/v1""," & LF);
      Append (Provenance_JSON, "      ""externalParameters"": {" & LF);
      Append (Provenance_JSON, "        ""package"": """ & Pkg_Name & """," & LF);
      Append (Provenance_JSON, "        ""version"": """ & Pkg_Version & "-" & Pkg_Rev & """," & LF);
      Append (Provenance_JSON, "        ""imageRef"": """ & Image_Ref & """" & LF);
      Append (Provenance_JSON, "      }," & LF);

      --  Resolved dependencies (materials)
      Append (Provenance_JSON, "      ""resolvedDependencies"": [" & LF);
      Append (Provenance_JSON, "        {" & LF);
      Append (Provenance_JSON, "          ""uri"": """ &
         To_String (M.Provenance.Upstream_URL) & """," & LF);
      Append (Provenance_JSON, "          ""digest"": {" & LF);
      Append (Provenance_JSON, "            ""sha256"": """ & Source_Digest_Hex & """" & LF);
      Append (Provenance_JSON, "          }" & LF);
      Append (Provenance_JSON, "        }" & LF);
      Append (Provenance_JSON, "      ]" & LF);
      Append (Provenance_JSON, "    }," & LF);

      --  Run details
      Append (Provenance_JSON, "    ""runDetails"": {" & LF);
      Append (Provenance_JSON, "      ""builder"": {" & LF);
      Append (Provenance_JSON, "        ""id"": ""https://cerro-torre.dev/builder/v0.2""," & LF);
      Append (Provenance_JSON, "        ""version"": {" & LF);
      Append (Provenance_JSON, "          ""cerro-torre"": ""0.2""" & LF);
      Append (Provenance_JSON, "        }" & LF);
      Append (Provenance_JSON, "      }," & LF);
      Append (Provenance_JSON, "      ""metadata"": {" & LF);
      Append (Provenance_JSON, "        ""invocationId"": """ &
         Pkg_Name & "-" & Pkg_Version & "-" & Pkg_Rev & """," & LF);
      Append (Provenance_JSON, "        ""startedOn"": ""2026-01-22T00:00:00Z""," & LF);
      Append (Provenance_JSON, "        ""finishedOn"": ""2026-01-22T00:00:00Z""" & LF);
      Append (Provenance_JSON, "      }" & LF);
      Append (Provenance_JSON, "    }" & LF);
      Append (Provenance_JSON, "  }" & LF);
      Append (Provenance_JSON, "}" & LF);

      --  Write the provenance attestation to file
      if not Exists (Output_Dir) then
         Create_Path (Output_Dir);
      end if;

      Create (File, Out_File, Output_Path);
      Put (File, To_String (Provenance_JSON));
      Close (File);

      return True;

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         return False;
   end Attach_Provenance;

   function Attach_SBOM
      (Image_Ref : String;
       M         : Manifest) return Boolean
   is
      use Ada.Text_IO;
      use Ada.Directories;

      LF : constant Character := Character'Val (10);
      SBOM_JSON : Unbounded_String;

      --  Sanitize image reference for use as filename
      function Sanitize_Ref (Ref : String) return String is
         Result : String := Ref;
      begin
         for I in Result'Range loop
            if Result (I) = '/' or Result (I) = ':' or Result (I) = '@' then
               Result (I) := '-';
            end if;
         end loop;
         return Result;
      end Sanitize_Ref;

      Output_Dir  : constant String := "/tmp/cerro-sbom";
      Output_Path : constant String := Output_Dir & "/" &
         Sanitize_Ref (Image_Ref) & ".cdx.json";

      --  Package identification
      Pkg_Name    : constant String := To_String (M.Metadata.Name);
      Pkg_Version : constant String := To_String (M.Metadata.Version.Upstream);
      Pkg_Rev_Str : constant String := Positive'Image (M.Metadata.Version.Revision);
      Pkg_Rev     : constant String :=
         Pkg_Rev_Str (Pkg_Rev_Str'First + 1 .. Pkg_Rev_Str'Last);
      Pkg_License : constant String := To_String (M.Metadata.License);

      --  Compute package content hash for component identification
      Manifest_Content : constant String := Cerro_Manifest.To_String (M);
      Pkg_Hash         : constant Cerro_Crypto.SHA256_Digest :=
         Cerro_Crypto.Compute_SHA256 (Manifest_Content);
      Pkg_Hash_Hex     : constant String :=
         Cerro_Crypto.Bytes_To_Hex (Pkg_Hash);

      --  Upstream source hash
      Source_Hash_Hex : constant String :=
         Cerro_Crypto.Bytes_To_Hex (M.Provenance.Upstream_Hash.Digest);

      File : File_Type;

      --  Track whether this is the first dependency entry (for comma handling)
      First_Dep : Boolean := True;
   begin
      --  Generate CycloneDX 1.5 SBOM in JSON format
      --  Reference: https://cyclonedx.org/docs/1.5/json/

      Append (SBOM_JSON, "{" & LF);
      Append (SBOM_JSON, "  ""$schema"": ""http://cyclonedx.org/schema/bom-1.5.schema.json""," & LF);
      Append (SBOM_JSON, "  ""bomFormat"": ""CycloneDX""," & LF);
      Append (SBOM_JSON, "  ""specVersion"": ""1.5""," & LF);
      Append (SBOM_JSON, "  ""serialNumber"": ""urn:uuid:cerro-torre-" &
         Pkg_Name & "-" & Pkg_Version & "-" & Pkg_Rev & """," & LF);
      Append (SBOM_JSON, "  ""version"": 1," & LF);

      --  Metadata section
      Append (SBOM_JSON, "  ""metadata"": {" & LF);
      Append (SBOM_JSON, "    ""timestamp"": ""2026-01-22T00:00:00Z""," & LF);
      Append (SBOM_JSON, "    ""tools"": [" & LF);
      Append (SBOM_JSON, "      {" & LF);
      Append (SBOM_JSON, "        ""vendor"": ""Cerro Torre""," & LF);
      Append (SBOM_JSON, "        ""name"": ""cerro-torre""," & LF);
      Append (SBOM_JSON, "        ""version"": ""0.2""" & LF);
      Append (SBOM_JSON, "      }" & LF);
      Append (SBOM_JSON, "    ]," & LF);
      Append (SBOM_JSON, "    ""component"": {" & LF);
      Append (SBOM_JSON, "      ""type"": ""container""," & LF);
      Append (SBOM_JSON, "      ""name"": """ & Image_Ref & """," & LF);
      Append (SBOM_JSON, "      ""bom-ref"": ""pkg:oci/" & Pkg_Name & """" & LF);
      Append (SBOM_JSON, "    }" & LF);
      Append (SBOM_JSON, "  }," & LF);

      --  Components section: primary package + runtime dependencies
      Append (SBOM_JSON, "  ""components"": [" & LF);

      --  Primary component
      Append (SBOM_JSON, "    {" & LF);
      Append (SBOM_JSON, "      ""type"": ""library""," & LF);
      Append (SBOM_JSON, "      ""name"": """ & Pkg_Name & """," & LF);
      Append (SBOM_JSON, "      ""version"": """ & Pkg_Version & "-" & Pkg_Rev & """," & LF);
      Append (SBOM_JSON, "      ""bom-ref"": ""pkg:" & Pkg_Name & "@" &
         Pkg_Version & "-" & Pkg_Rev & """," & LF);
      Append (SBOM_JSON, "      ""purl"": ""pkg:cerro-torre/" & Pkg_Name & "@" &
         Pkg_Version & "-" & Pkg_Rev & """," & LF);

      --  License info
      if Pkg_License'Length > 0 then
         Append (SBOM_JSON, "      ""licenses"": [" & LF);
         Append (SBOM_JSON, "        {" & LF);
         Append (SBOM_JSON, "          ""expression"": """ & Pkg_License & """" & LF);
         Append (SBOM_JSON, "        }" & LF);
         Append (SBOM_JSON, "      ]," & LF);
      end if;

      --  Hashes
      Append (SBOM_JSON, "      ""hashes"": [" & LF);
      Append (SBOM_JSON, "        {" & LF);
      Append (SBOM_JSON, "          ""alg"": ""SHA-256""," & LF);
      Append (SBOM_JSON, "          ""content"": """ & Pkg_Hash_Hex & """" & LF);
      Append (SBOM_JSON, "        }," & LF);
      Append (SBOM_JSON, "        {" & LF);
      Append (SBOM_JSON, "          ""alg"": ""SHA-256""," & LF);
      Append (SBOM_JSON, "          ""content"": """ & Source_Hash_Hex & """" & LF);
      Append (SBOM_JSON, "        }" & LF);
      Append (SBOM_JSON, "      ]," & LF);

      --  External references
      if Length (M.Provenance.Upstream_URL) > 0 then
         Append (SBOM_JSON, "      ""externalReferences"": [" & LF);
         Append (SBOM_JSON, "        {" & LF);
         Append (SBOM_JSON, "          ""type"": ""distribution""," & LF);
         Append (SBOM_JSON, "          ""url"": """ &
            To_String (M.Provenance.Upstream_URL) & """" & LF);
         Append (SBOM_JSON, "        }" & LF);
         Append (SBOM_JSON, "      ]," & LF);
      end if;

      --  Supplier / maintainer
      Append (SBOM_JSON, "      ""supplier"": {" & LF);
      Append (SBOM_JSON, "        ""name"": """ &
         To_String (M.Metadata.Maintainer) & """" & LF);
      Append (SBOM_JSON, "      }" & LF);
      Append (SBOM_JSON, "    }");

      --  Add runtime dependencies as components
      for Dep of M.Dependencies.Runtime loop
         Append (SBOM_JSON, "," & LF);
         Append (SBOM_JSON, "    {" & LF);
         Append (SBOM_JSON, "      ""type"": ""library""," & LF);
         Append (SBOM_JSON, "      ""name"": """ & To_String (Dep.Name) & """," & LF);
         Append (SBOM_JSON, "      ""bom-ref"": ""pkg:" &
            To_String (Dep.Name) & """" & LF);
         Append (SBOM_JSON, "    }");
      end loop;

      Append (SBOM_JSON, LF & "  ]," & LF);

      --  Dependencies section: declare the dependency graph
      Append (SBOM_JSON, "  ""dependencies"": [" & LF);
      Append (SBOM_JSON, "    {" & LF);
      Append (SBOM_JSON, "      ""ref"": ""pkg:" & Pkg_Name & "@" &
         Pkg_Version & "-" & Pkg_Rev & """," & LF);
      Append (SBOM_JSON, "      ""dependsOn"": [");

      First_Dep := True;
      for Dep of M.Dependencies.Runtime loop
         if not First_Dep then
            Append (SBOM_JSON, ", ");
         end if;
         First_Dep := False;
         Append (SBOM_JSON, """pkg:" & To_String (Dep.Name) & """");
      end loop;

      Append (SBOM_JSON, "]" & LF);
      Append (SBOM_JSON, "    }" & LF);
      Append (SBOM_JSON, "  ]" & LF);
      Append (SBOM_JSON, "}" & LF);

      --  Write the SBOM to file
      if not Exists (Output_Dir) then
         Create_Path (Output_Dir);
      end if;

      Create (File, Out_File, Output_Path);
      Put (File, To_String (SBOM_JSON));
      Close (File);

      return True;

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         return False;
   end Attach_SBOM;

end Cerro_Export_OCI;
