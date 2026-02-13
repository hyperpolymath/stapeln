-------------------------------------------------------------------------------
--  Cerro_Import_Debian - Implementation
-------------------------------------------------------------------------------

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Calendar;
with CT_HTTP;
with GNAT.OS_Lib;

package body Cerro_Import_Debian is

   Current_Mirror : Mirror_Config := Default_Mirror;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Trim_Both (S : String) return String is
   begin
      return Trim (S, Ada.Strings.Both);
   end Trim_Both;

   function Extract_Field (Content : String; Field_Name : String) return String is
      Search_Pattern : constant String := Field_Name & ":";
      Start_Pos      : Natural;
      End_Pos        : Natural;
      Line_Start     : Natural;
   begin
      --  Find field in content (case-insensitive)
      Start_Pos := Index (To_Lower (Content), To_Lower (Search_Pattern));

      if Start_Pos = 0 then
         return "";
      end if;

      --  Skip field name and colon
      Line_Start := Start_Pos + Search_Pattern'Length;

      --  Find end of line
      End_Pos := Index (Content (Line_Start .. Content'Last), "" & Character'Val (10));

      if End_Pos = 0 then
         End_Pos := Content'Last + 1;
      end if;

      --  Extract and trim value
      return Trim_Both (Content (Line_Start .. End_Pos - 1));
   end Extract_Field;

   function Extract_File_Hash
      (Content : String;
       Filename_Pattern : String) return String
   is
      Line_Start : Natural := Content'First;
      Line_End   : Natural;
      LF         : constant Character := Character'Val (10);
   begin
      --  Search line by line for the filename
      while Line_Start <= Content'Last loop
         Line_End := Index (Content (Line_Start .. Content'Last), "" & LF);

         if Line_End = 0 then
            Line_End := Content'Last + 1;
         end if;

         declare
            Line : constant String := Content (Line_Start .. Line_End - 1);
         begin
            --  Check if line contains the filename pattern
            if Index (Line, Filename_Pattern) > 0 then
               --  Extract hash (first field on the line)
               declare
                  Hash_End : constant Natural := Index (Line, " ");
               begin
                  if Hash_End > 0 then
                     return Trim_Both (Line (Line'First .. Hash_End - 1));
                  end if;
               end;
            end if;
         end;

         exit when Line_End > Content'Last;
         Line_Start := Line_End + 1;
      end loop;

      return "";
   end Extract_File_Hash;

   ---------------------------------------------------------------------------
   --  Parse_Debian_Version - Convert Debian version to Manifest version
   ---------------------------------------------------------------------------

   function Parse_Debian_Version (Debian_Ver : String) return Version is
      V : Version;
      Epoch_Pos : Natural;
      Revision_Pos : Natural;
   begin
      --  Debian version format: [epoch:]upstream[-revision]
      --  Example: "2:1.26.0-1" -> epoch=2, upstream="1.26.0", revision=1

      Epoch_Pos := Index (Debian_Ver, ":");
      if Epoch_Pos > 0 then
         --  Has epoch
         V.Epoch := Natural'Value (Debian_Ver (Debian_Ver'First .. Epoch_Pos - 1));
         declare
            Rest : constant String := Debian_Ver (Epoch_Pos + 1 .. Debian_Ver'Last);
         begin
            Revision_Pos := Index (Rest, "-", Ada.Strings.Backward);
            if Revision_Pos > 0 then
               V.Upstream := To_Unbounded_String (Rest (Rest'First .. Revision_Pos - 1));
               V.Revision := Positive'Value (Rest (Revision_Pos + 1 .. Rest'Last));
            else
               V.Upstream := To_Unbounded_String (Rest);
               V.Revision := 1;
            end if;
         end;
      else
         --  No epoch
         V.Epoch := 0;
         Revision_Pos := Index (Debian_Ver, "-", Ada.Strings.Backward);
         if Revision_Pos > 0 then
            V.Upstream := To_Unbounded_String (
               Debian_Ver (Debian_Ver'First .. Revision_Pos - 1));
            V.Revision := Positive'Value (
               Debian_Ver (Revision_Pos + 1 .. Debian_Ver'Last));
         else
            V.Upstream := To_Unbounded_String (Debian_Ver);
            V.Revision := 1;
         end if;
      end if;

      return V;
   exception
      when others =>
         --  Fallback: treat whole string as upstream version
         return (Epoch => 0, Upstream => To_Unbounded_String (Debian_Ver), Revision => 1);
   end Parse_Debian_Version;

   ---------------------------------------------------------------------------
   --  Parse_Debian_Dependencies - Convert Build-Depends to Dependency_List
   ---------------------------------------------------------------------------

   function Parse_Debian_Dependencies (Build_Depends : String) return Dependency_List is
      Result : Dependency_List := Dependency_Vectors.Empty_Vector;
      Dep_Start : Natural := Build_Depends'First;
      Dep_End : Natural;
   begin
      --  Build-Depends format: "pkg1, pkg2 (>= 1.0), pkg3 | pkg4"
      --  Simplified parser: split by comma, extract package name

      if Build_Depends'Length = 0 then
         return Result;
      end if;

      loop
         --  Find next comma
         Dep_End := Index (Build_Depends (Dep_Start .. Build_Depends'Last), ",");
         if Dep_End = 0 then
            Dep_End := Build_Depends'Last + 1;
         end if;

         declare
            Dep_Str : constant String := Trim_Both (
               Build_Depends (Dep_Start .. Dep_End - 1));
            Paren_Pos : constant Natural := Index (Dep_Str, "(");
            Pipe_Pos : constant Natural := Index (Dep_Str, "|");
            Pkg_Name : Unbounded_String;
         begin
            --  Extract package name (before parenthesis or pipe)
            if Paren_Pos > 0 then
               Pkg_Name := To_Unbounded_String (Trim_Both (Dep_Str (Dep_Str'First .. Paren_Pos - 1)));
            elsif Pipe_Pos > 0 then
               Pkg_Name := To_Unbounded_String (Trim_Both (Dep_Str (Dep_Str'First .. Pipe_Pos - 1)));
            else
               Pkg_Name := To_Unbounded_String (Dep_Str);
            end if;

            --  Add to result if non-empty
            if Length (Pkg_Name) > 0 then
               Result.Append ((
                  Name       => Pkg_Name,
                  Constraint => (Kind => Any)));
            end if;
         end;

         exit when Dep_End > Build_Depends'Last;
         Dep_Start := Dep_End + 1;
      end loop;

      return Result;
   end Parse_Debian_Dependencies;

   ---------------------------------------------------------------------------
   --  Parse_Dsc - Parse Debian .dsc file
   ---------------------------------------------------------------------------

   function Parse_Dsc (Content : String) return Dsc_Info is
      Info : Dsc_Info;
   begin
      --  Extract basic fields
      Info.Source := To_Unbounded_String (Extract_Field (Content, "Source"));
      Info.Version := To_Unbounded_String (Extract_Field (Content, "Version"));
      Info.Maintainer := To_Unbounded_String (Extract_Field (Content, "Maintainer"));
      Info.Build_Depends := To_Unbounded_String (Extract_Field (Content, "Build-Depends"));

      --  Look for Checksums-Sha256 section
      declare
         Checksums_Pos : constant Natural :=
            Index (Content, "Checksums-Sha256:");
      begin
         if Checksums_Pos > 0 then
            declare
               Checksums_Section : constant String :=
                  Content (Checksums_Pos .. Content'Last);
            begin
               --  Find .orig.tar and .debian.tar hashes
               Info.Orig_Hash := To_Unbounded_String (
                  Extract_File_Hash (Checksums_Section, ".orig.tar"));
               Info.Debian_Hash := To_Unbounded_String (
                  Extract_File_Hash (Checksums_Section, ".debian.tar"));

               --  Extract filenames from Files section for tarball names
               declare
                  Files_Pos : constant Natural := Index (Content, "Files:");
               begin
                  if Files_Pos > 0 then
                     declare
                        Files_Section : constant String :=
                           Content (Files_Pos .. Content'Last);
                        Line_Start : Natural := Files_Pos;
                        Line_End   : Natural;
                        LF         : constant Character := Character'Val (10);
                     begin
                        --  Parse Files section for tarball names
                        while Line_Start <= Content'Last loop
                           Line_End := Index (Content (Line_Start .. Content'Last), "" & LF);
                           if Line_End = 0 then
                              Line_End := Content'Last + 1;
                           end if;

                           declare
                              Line : constant String := Content (Line_Start .. Line_End - 1);
                           begin
                              if Index (Line, ".orig.tar") > 0 then
                                 --  Extract filename (last field)
                                 declare
                                    Filename_Start : Natural := 0;
                                 begin
                                    for I in reverse Line'Range loop
                                       if Line (I) = ' ' or Line (I) = Character'Val (9) then
                                          Filename_Start := I + 1;
                                          exit;
                                       end if;
                                    end loop;
                                    if Filename_Start > 0 then
                                       Info.Orig_Tarball := To_Unbounded_String (
                                          Trim_Both (Line (Filename_Start .. Line'Last)));
                                    end if;
                                 end;
                              elsif Index (Line, ".debian.tar") > 0 then
                                 --  Extract filename (last field)
                                 declare
                                    Filename_Start : Natural := 0;
                                 begin
                                    for I in reverse Line'Range loop
                                       if Line (I) = ' ' or Line (I) = Character'Val (9) then
                                          Filename_Start := I + 1;
                                          exit;
                                       end if;
                                    end loop;
                                    if Filename_Start > 0 then
                                       Info.Debian_Tarball := To_Unbounded_String (
                                          Trim_Both (Line (Filename_Start .. Line'Last)));
                                    end if;
                                 end;
                              end if;
                           end;

                           exit when Line_End > Content'Last;
                           Line_Start := Line_End + 1;
                        end loop;
                     end;
                  end if;
               end;
            end;
         end if;
      end;

      return Info;
   end Parse_Dsc;

   ---------------------------------------------------------------------------
   --  Import_From_Dsc - Import from local .dsc file
   ---------------------------------------------------------------------------

   function Import_From_Dsc (Dsc_Path : String) return Import_Result is
      use Ada.Text_IO;
      use Ada.Directories;

      Result : Import_Result;
   begin
      --  Check if file exists
      if not Exists (Dsc_Path) then
         Result.Status := Package_Not_Found;
         Result.Errors := To_Unbounded_String ("File not found: " & Dsc_Path);
         return Result;
      end if;

      --  Read file content
      declare
         File    : File_Type;
         Content : Unbounded_String;
      begin
         Open (File, In_File, Dsc_Path);

         while not End_Of_File (File) loop
            declare
               Line : constant String := Get_Line (File);
            begin
               if Length (Content) > 0 then
                  Append (Content, Character'Val (10));  --  LF
               end if;
               Append (Content, Line);
            end;
         end loop;

         Close (File);

         --  Parse the .dsc content
         declare
            Dsc : constant Dsc_Info := Parse_Dsc (To_String (Content));
         begin
            --  Validate that we got required fields
            if Length (Dsc.Source) = 0 then
               Result.Status := Parse_Error;
               Result.Errors := To_Unbounded_String (
                  "Missing 'Source' field in .dsc file");
               return Result;
            end if;

            if Length (Dsc.Version) = 0 then
               Result.Status := Parse_Error;
               Result.Errors := To_Unbounded_String (
                  "Missing 'Version' field in .dsc file");
               return Result;
            end if;

            --  Convert Dsc_Info to Cerro_Manifest.Manifest
            declare
               M : Manifest;
            begin
               --  [metadata] section
               M.Metadata.Name := Dsc.Source;
               M.Metadata.Version := Parse_Debian_Version (To_String (Dsc.Version));
               M.Metadata.Summary := To_Unbounded_String (
                  "Debian package: " & To_String (Dsc.Source));
               M.Metadata.Description := M.Metadata.Summary;
               M.Metadata.License := To_Unbounded_String ("UNKNOWN");
               M.Metadata.Homepage := Null_Unbounded_String;
               M.Metadata.Maintainer := Dsc.Maintainer;

               --  [provenance] section
               M.Provenance.Upstream_URL := To_Unbounded_String (
                  To_String (Current_Mirror.URL) & "/pool/main/" &
                  To_String (Dsc.Orig_Tarball));
               M.Provenance.Upstream_Hash := (
                  Algorithm => SHA256,
                  Digest    => Dsc.Orig_Hash);
               M.Provenance.Upstream_Signature := Null_Unbounded_String;
               M.Provenance.Upstream_Keyring := Null_Unbounded_String;
               M.Provenance.Imported_From := To_Unbounded_String (
                  "debian:" & To_String (Dsc.Source) & "/" & To_String (Dsc.Version));
               M.Provenance.Import_Date := Ada.Calendar.Clock;
               M.Provenance.Patches := String_Vectors.Empty_Vector;

               --  [dependencies] section
               M.Dependencies.Runtime := Dependency_Vectors.Empty_Vector;
               M.Dependencies.Build := Parse_Debian_Dependencies (
                  To_String (Dsc.Build_Depends));
               M.Dependencies.Check := Dependency_Vectors.Empty_Vector;
               M.Dependencies.Optional := Dependency_Vectors.Empty_Vector;
               M.Dependencies.Conflicts := Dependency_Vectors.Empty_Vector;
               M.Dependencies.Provides := Dependency_Vectors.Empty_Vector;
               M.Dependencies.Replaces := Dependency_Vectors.Empty_Vector;

               --  [build] section - assume autoconf for most Debian packages
               M.Build.System := Autoconf;
               M.Build.Configure_Flags := String_Vectors.Empty_Vector;
               M.Build.Build_Flags := String_Vectors.Empty_Vector;
               M.Build.Install_Flags := String_Vectors.Empty_Vector;

               --  [outputs] section
               M.Outputs.Primary := Dsc.Source;
               M.Outputs.Split := String_Vectors.Empty_Vector;

               --  [attestations] section - minimal for imports
               M.Attestations.Required := Attestation_Vectors.Empty_Vector;
               M.Attestations.Recommended := Attestation_Vectors.Empty_Vector;

               Result.Status := Success;
               Result.Manifest := M;
               return Result;
            end;
         end;

      exception
         when E : others =>
            if Is_Open (File) then
               Close (File);
            end if;
            Result.Status := Parse_Error;
            Result.Errors := To_Unbounded_String (
               "Error reading .dsc file: " & Ada.Exceptions.Exception_Message (E));
            return Result;
      end;
   end Import_From_Dsc;

   ---------------------------------------------------------------------------
   --  Import_Package - Import by package name
   ---------------------------------------------------------------------------

   function Import_Package
      (Package_Name : String;
       Version      : String := "") return Import_Result
   is
      use Ada.Text_IO;
      use Ada.Directories;
      use CT_HTTP;

      Result : Import_Result;
      Temp_Dir : constant String := "/tmp/cerro-import-" & Package_Name;
      Dsc_File : constant String := Temp_Dir & "/" & Package_Name & ".dsc";
   begin
      --  Create temp directory
      if Exists (Temp_Dir) then
         Delete_Tree (Temp_Dir);
      end if;
      Create_Directory (Temp_Dir);

      --  Build URL to package's .dsc file
      --  Debian mirror structure: /pool/main/[first-letter]/[package]/[package]_[version].dsc
      declare
         First_Letter : constant Character := Package_Name (Package_Name'First);
         Pool_Path : constant String := "/pool/main/" & First_Letter & "/" & Package_Name & "/";
         Mirror_URL : constant String := To_String (Current_Mirror.URL) & Pool_Path;

         --  If version not specified, we need to query the Sources file
         --  For now, fallback to Import_From_Apt_Source
      begin
         if Version'Length = 0 then
            --  No version specified, use apt source method
            Result := Import_From_Apt_Source (Package_Name, To_String (Current_Mirror.Release));
            if Exists (Temp_Dir) then
               Delete_Tree (Temp_Dir);
            end if;
            return Result;
         end if;

         --  Download .dsc file
         declare
            Dsc_URL : constant String := Mirror_URL & Package_Name & "_" & Version & ".dsc";
            Response : constant HTTP_Response := Download_To_File (
               URL => Dsc_URL,
               Output_Path => Dsc_File);
         begin
            if not Response.Success or not Is_Success (Response.Status_Code) then
               Result.Status := Download_Failed;
               Result.Errors := To_Unbounded_String (
                  "Failed to download .dsc: " & To_String (Response.Error_Message));
               if Exists (Temp_Dir) then
                  Delete_Tree (Temp_Dir);
               end if;
               return Result;
            end if;

            --  Import from downloaded .dsc file
            Result := Import_From_Dsc (Dsc_File);

            --  Cleanup
            if Exists (Temp_Dir) then
               Delete_Tree (Temp_Dir);
            end if;

            return Result;
         end;
      end;

   exception
      when E : others =>
         if Exists (Temp_Dir) then
            Delete_Tree (Temp_Dir);
         end if;
         Result.Status := Parse_Error;
         Result.Errors := To_Unbounded_String (
            "Import failed: " & Ada.Exceptions.Exception_Message (E));
         return Result;
   end Import_Package;

   ---------------------------------------------------------------------------
   --  Import_From_Apt_Source - Import from APT repository
   ---------------------------------------------------------------------------

   function Import_From_Apt_Source
      (Package_Name : String;
       Release      : String := "stable") return Import_Result
   is
      use Ada.Text_IO;
      use Ada.Directories;
      use CT_HTTP;

      Result : Import_Result;
      Temp_Dir : constant String := "/tmp/cerro-apt-" & Package_Name;
      Sources_File : constant String := Temp_Dir & "/Sources";
      Dsc_File : constant String := Temp_Dir & "/" & Package_Name & ".dsc";
   begin
      --  Create temp directory
      if Exists (Temp_Dir) then
         Delete_Tree (Temp_Dir);
      end if;
      Create_Directory (Temp_Dir);

      --  Download Sources file from APT repository
      --  URL format: http://deb.debian.org/debian/dists/stable/main/source/Sources.gz
      declare
         Sources_URL : constant String :=
            To_String (Current_Mirror.URL) & "/dists/" & Release & "/main/source/Sources.gz";
         Sources_GZ : constant String := Sources_File & ".gz";
         Response : HTTP_Response;
      begin
         --  Download Sources.gz
         Response := Download_To_File (
            URL => Sources_URL,
            Output_Path => Sources_GZ);

         if not Response.Success or not Is_Success (Response.Status_Code) then
            Result.Status := Download_Failed;
            Result.Errors := To_Unbounded_String (
               "Failed to download Sources.gz: " & To_String (Response.Error_Message));
            if Exists (Temp_Dir) then
               Delete_Tree (Temp_Dir);
            end if;
            return Result;
         end if;

         --  Decompress Sources.gz
         declare
            use GNAT.OS_Lib;
            Gunzip : String_Access := Locate_Exec_On_Path ("gunzip");
            Args : Argument_List := (new String'("-f"), new String'(Sources_GZ));
            Exit_Status : Integer;
         begin
            if Gunzip = null then
               Result.Status := Parse_Error;
               Result.Errors := To_Unbounded_String ("gunzip not found in PATH");
               if Exists (Temp_Dir) then
                  Delete_Tree (Temp_Dir);
               end if;
               return Result;
            end if;

            Exit_Status := Spawn (Gunzip.all, Args);
            Free (Args (1));
            Free (Args (2));
            Free (Gunzip);

            if Exit_Status /= 0 then
               Result.Status := Parse_Error;
               Result.Errors := To_Unbounded_String ("Failed to decompress Sources.gz");
               if Exists (Temp_Dir) then
                  Delete_Tree (Temp_Dir);
               end if;
               return Result;
            end if;
         end;

         --  Parse Sources file to find package's .dsc URL
         declare
            File : File_Type;
            In_Package : Boolean := False;
            Dsc_URL : Unbounded_String;
         begin
            Open (File, In_File, Sources_File);

            while not End_Of_File (File) loop
               declare
                  Line : constant String := Get_Line (File);
               begin
                  --  Check for Package: line
                  if Index (Line, "Package: ") = 1 then
                     declare
                        Pkg : constant String := Trim_Both (Line (10 .. Line'Last));
                     begin
                        In_Package := (Pkg = Package_Name);
                     end;
                  elsif In_Package and Index (Line, "Directory: ") = 1 then
                     --  Found Directory line for our package
                     declare
                        Dir : constant String := Trim_Both (Line (12 .. Line'Last));
                     begin
                        --  Directory format: "pool/main/n/nginx"
                        --  Next, look for .dsc file in the Files section
                        while not End_Of_File (File) loop
                           declare
                              File_Line : constant String := Get_Line (File);
                           begin
                              if Index (File_Line, ".dsc") > 0 then
                                 --  Extract .dsc filename (last field)
                                 declare
                                    Filename_Start : Natural := 0;
                                 begin
                                    for I in reverse File_Line'Range loop
                                       if File_Line (I) = ' ' or File_Line (I) = Character'Val (9) then
                                          Filename_Start := I + 1;
                                          exit;
                                       end if;
                                    end loop;

                                    if Filename_Start > 0 then
                                       declare
                                          Dsc_Filename : constant String :=
                                             Trim_Both (File_Line (Filename_Start .. File_Line'Last));
                                       begin
                                          Dsc_URL := To_Unbounded_String (
                                             To_String (Current_Mirror.URL) & "/" & Dir & "/" & Dsc_Filename);
                                          exit;
                                       end;
                                    end if;
                                 end;
                              elsif Index (File_Line, "Package: ") = 1 then
                                 --  Next package, stop searching
                                 exit;
                              end if;
                           end;
                        end loop;
                        exit;
                     end;
                  end if;
               end;
            end loop;

            Close (File);

            --  Check if we found the .dsc URL
            if Length (Dsc_URL) = 0 then
               Result.Status := Package_Not_Found;
               Result.Errors := To_Unbounded_String (
                  "Package not found in APT sources: " & Package_Name);
               if Exists (Temp_Dir) then
                  Delete_Tree (Temp_Dir);
               end if;
               return Result;
            end if;

            --  Download .dsc file
            Response := Download_To_File (
               URL => To_String (Dsc_URL),
               Output_Path => Dsc_File);

            if not Response.Success or not Is_Success (Response.Status_Code) then
               Result.Status := Download_Failed;
               Result.Errors := To_Unbounded_String (
                  "Failed to download .dsc: " & To_String (Response.Error_Message));
               if Exists (Temp_Dir) then
                  Delete_Tree (Temp_Dir);
               end if;
               return Result;
            end if;

            --  Import from downloaded .dsc file
            Result := Import_From_Dsc (Dsc_File);

            --  Cleanup
            if Exists (Temp_Dir) then
               Delete_Tree (Temp_Dir);
            end if;

            return Result;
         end;
      end;

   exception
      when E : others =>
         if Exists (Temp_Dir) then
            Delete_Tree (Temp_Dir);
         end if;
         Result.Status := Parse_Error;
         Result.Errors := To_Unbounded_String (
            "Import from APT source failed: " & Ada.Exceptions.Exception_Message (E));
         return Result;
   end Import_From_Apt_Source;

   ---------------------------------------------------------------------------
   --  Set_Mirror - Configure mirror
   ---------------------------------------------------------------------------

   procedure Set_Mirror (Config : Mirror_Config) is
   begin
      Current_Mirror := Config;
   end Set_Mirror;

end Cerro_Import_Debian;
