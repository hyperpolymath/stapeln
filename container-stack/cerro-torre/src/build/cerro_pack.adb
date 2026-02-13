--  Cerro_Pack - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later

with Ada.Text_IO;
with Ada.Directories;
with Ada.Calendar.Formatting;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with GNAT.OS_Lib;
with Cerro_Manifest;
with Cerro_Crypto;
with Cerro_Crypto_OpenSSL;
with Cerro_Trust_Store;
with Cerro_Tar;

package body Cerro_Pack is

   package TIO renames Ada.Text_IO;
   package Dir renames Ada.Directories;

   ---------------------------------------------------------------------------
   --  Hash_File - Compute SHA-256 hash of file contents
   ---------------------------------------------------------------------------

   function Hash_File (Path : String) return String is
      File    : TIO.File_Type;
      Buffer  : String (1 .. 8192);
      Last    : Natural;
      Content : Unbounded_String := Null_Unbounded_String;
   begin
      if not Dir.Exists (Path) then
         return "";
      end if;

      --  Read file contents
      begin
         TIO.Open (File, TIO.In_File, Path);
         while not TIO.End_Of_File (File) loop
            TIO.Get_Line (File, Buffer, Last);
            Append (Content, Buffer (1 .. Last));
            Append (Content, ASCII.LF);
         end loop;
         TIO.Close (File);
      exception
         when others =>
            if TIO.Is_Open (File) then
               TIO.Close (File);
            end if;
            return "";
      end;

      --  Compute hash
      declare
         Hash : constant Cerro_Crypto.SHA256_Digest :=
            Cerro_Crypto.Compute_SHA256 (To_String (Content));
      begin
         return Cerro_Crypto.Bytes_To_Hex (Hash);
      end;
   end Hash_File;

   ---------------------------------------------------------------------------
   --  Write_Summary_Json - Generate summary.json with all content hashes
   ---------------------------------------------------------------------------

   procedure Write_Summary_Json
      (Path          : String;
       Manifest      : Cerro_Manifest.Manifest;
       Manifest_Hash : String;
       Signature_Hex : String := "";
       Key_Id        : String := "";
       Fingerprint   : String := "";
       Timestamp     : String := "")
   is
      File : TIO.File_Type;
      Has_Signature : constant Boolean := (Signature_Hex'Length > 0);
   begin
      TIO.Create (File, TIO.Out_File, Path);

      TIO.Put_Line (File, "{");
      TIO.Put_Line (File, "  ""ctp_version"": ""1.0"",");
      TIO.Put_Line (File, "  ""package"": {");
      TIO.Put_Line (File, "    ""name"": """ & To_String (Manifest.Metadata.Name) & """,");
      TIO.Put_Line (File, "    ""version"": """ & To_String (Manifest.Metadata.Version.Upstream) & """");
      TIO.Put_Line (File, "  },");
      TIO.Put_Line (File, "  ""content"": {");
      TIO.Put_Line (File, "    ""manifest_sha256"": """ & Manifest_Hash & """");
      TIO.Put_Line (File, "  },");

      if Has_Signature then
         TIO.Put_Line (File, "  ""attestations"": [");
         TIO.Put_Line (File, "    {");
         TIO.Put_Line (File, "      ""type"": ""signature"",");
         TIO.Put_Line (File, "      ""suite"": ""CT-SIG-01"",");
         TIO.Put_Line (File, "      ""key_id"": """ & Key_Id & """,");
         TIO.Put_Line (File, "      ""fingerprint"": """ & Fingerprint & """,");
         TIO.Put_Line (File, "      ""signature"": """ & Signature_Hex & """,");
         TIO.Put_Line (File, "      ""timestamp"": """ & Timestamp & """");
         TIO.Put_Line (File, "    }");
         TIO.Put_Line (File, "  ]");
      else
         TIO.Put_Line (File, "  ""attestations"": []");
      end if;

      TIO.Put_Line (File, "}");

      TIO.Close (File);
   exception
      when others =>
         if TIO.Is_Open (File) then
            TIO.Close (File);
         end if;
   end Write_Summary_Json;

   ---------------------------------------------------------------------------
   --  Add_Directory_To_Archive - Recursively add directory contents
   ---------------------------------------------------------------------------

   procedure Add_Directory_To_Archive
     (Writer      : in out Cerro_Tar.Tar_Writer;
      Dir_Path    : String;
      Prefix      : String;
      File_Count  : in out Natural)
   is
      use Dir;

      Search   : Search_Type;
      Dir_Ent  : Directory_Entry_Type;
   begin
      Start_Search (Search, Dir_Path, "*", (others => True));

      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Ent);

         declare
            Name : constant String := Simple_Name (Dir_Ent);
            Full_Path : constant String := Full_Name (Dir_Ent);
         begin
            --  Skip . and ..
            if Name /= "." and Name /= ".." then
               case Kind (Dir_Ent) is
                  when Ordinary_File =>
                     declare
                        Archive_Name : constant String := Prefix & "/" & Name;
                     begin
                        Cerro_Tar.Add_File_From_Disk (Writer, Archive_Name, Full_Path);
                        File_Count := File_Count + 1;
                     end;

                  when Directory =>
                     --  Recurse into subdirectory
                     Add_Directory_To_Archive
                       (Writer, Full_Path, Prefix & "/" & Name, File_Count);

                  when Special_File =>
                     null;  --  Skip special files
               end case;
            end if;
         end;
      end loop;

      End_Search (Search);
   exception
      when others =>
         null;  --  Directory access error - continue
   end Add_Directory_To_Archive;

   ---------------------------------------------------------------------------
   --  Create_Tar_Bundle - Create tar archive with manifest and summary
   ---------------------------------------------------------------------------

   procedure Create_Tar_Bundle
      (Output_Path   : String;
       Manifest_Path : String;
       Summary_Path  : String;
       Source_Dir    : String;
       Verbose       : Boolean;
       Success       : out Boolean;
       Error_Msg     : out Unbounded_String)
   is
      Archive_Path : constant String := Output_Path & ".ctp";
      Writer       : Cerro_Tar.Tar_Writer;
      Source_Count : Natural := 0;
   begin
      --  Create real POSIX ustar tar archive
      begin
         --  Delete existing archive if present
         if Dir.Exists (Archive_Path) then
            Dir.Delete_File (Archive_Path);
         end if;

         --  Create new tar archive
         Cerro_Tar.Create (Writer, Archive_Path);

         --  Add manifest.ctp (use base name in archive)
         Cerro_Tar.Add_File_From_Disk (Writer, "manifest.ctp", Manifest_Path);

         --  Add summary.json
         Cerro_Tar.Add_File_From_Disk (Writer, "summary.json", Summary_Path);

         --  Add source files if directory specified
         if Source_Dir'Length > 0 and then Dir.Exists (Source_Dir) then
            if Verbose then
               TIO.Put_Line ("Including sources from: " & Source_Dir);
            end if;
            Add_Directory_To_Archive (Writer, Source_Dir, "src", Source_Count);
            if Verbose then
               TIO.Put_Line ("Added" & Natural'Image (Source_Count) & " source files");
            end if;
         end if;

         --  Finalize archive (writes end-of-archive markers)
         Cerro_Tar.Close (Writer);

         Success := True;
         Error_Msg := To_Unbounded_String ("Bundle created: " & Archive_Path);
      exception
         when others =>
            if Cerro_Tar.Is_Open (Writer) then
               Cerro_Tar.Close (Writer);
            end if;
            Success := False;
            Error_Msg := To_Unbounded_String ("Failed to create tar archive");
      end;
   end Create_Tar_Bundle;

   ---------------------------------------------------------------------------
   --  Create_Bundle - Main entry point
   ---------------------------------------------------------------------------

   function Create_Bundle (Opts : Pack_Options) return Pack_Result is
      Manifest_Path_Str : constant String := To_String (Opts.Manifest_Path);
      Output_Path_Str   : constant String := To_String (Opts.Output_Path);
   begin
      --  Check manifest exists
      if not Dir.Exists (Manifest_Path_Str) then
         return (Success => False,
                 Bundle_Path => Null_Unbounded_String,
                 Error_Msg => To_Unbounded_String ("Manifest not found: " & Manifest_Path_Str));
      end if;

      --  Parse manifest
      declare
         Parse_Result : constant Cerro_Manifest.Parse_Result :=
            Cerro_Manifest.Parse_File (Manifest_Path_Str);
      begin
         if not Parse_Result.Success then
            return (Success => False,
                    Bundle_Path => Null_Unbounded_String,
                    Error_Msg => To_Unbounded_String ("Failed to parse manifest: " &
                                                       To_String (Parse_Result.Error_Msg)));
         end if;

         if Opts.Verbose then
            TIO.Put_Line ("Parsed manifest: " & To_String (Parse_Result.Value.Metadata.Name));
            TIO.Put_Line ("Version: " & To_String (Parse_Result.Value.Metadata.Version.Upstream));
         end if;

         --  Compute manifest hash
         declare
            Manifest_Hash : constant String := Hash_File (Manifest_Path_Str);
            Summary_Path  : constant String := "/tmp/ct-summary-" &
                            To_String (Parse_Result.Value.Metadata.Name) & ".json";
         begin
            if Opts.Verbose then
               TIO.Put_Line ("Manifest SHA256: " & Manifest_Hash);
            end if;

            --  Sign bundle if requested
            declare
               Signature_Hex : Unbounded_String := Null_Unbounded_String;
               Key_Id_Str    : Unbounded_String := Null_Unbounded_String;
               Fingerprint   : Unbounded_String := Null_Unbounded_String;
               Timestamp_Str : Unbounded_String := Null_Unbounded_String;
            begin
               if Opts.Sign then
                  if Length (Opts.Key_Id) = 0 then
                     return (Success => False,
                             Bundle_Path => Null_Unbounded_String,
                             Error_Msg => To_Unbounded_String ("--key <id> required when using --sign"));
                  end if;

                  declare
                     use Cerro_Crypto_OpenSSL;
                     use Cerro_Trust_Store;
                     use Ada.Calendar.Formatting;

                     Key_Id_S      : constant String := To_String (Opts.Key_Id);
                     Home          : constant String := Ada.Environment_Variables.Value ("HOME", "/tmp");
                     Priv_Key_Path : constant String := Home & "/.config/cerro-torre/keys/" &
                                                        Key_Id_S & ".priv";
                     Priv_File     : TIO.File_Type;
                     Priv_Hex      : String (1 .. 128);
                     Last          : Natural;
                     Private_Key   : Ed25519_Private_Key;
                     Signature     : Cerro_Crypto.Ed25519_Signature;
                     Success       : Boolean;
                     Info          : Key_Info;
                     Result        : Store_Result;
                  begin
                     --  Load private key
                     if not Dir.Exists (Priv_Key_Path) then
                        return (Success => False,
                                Bundle_Path => Null_Unbounded_String,
                                Error_Msg => To_Unbounded_String ("Private key not found: " &
                                                                  Priv_Key_Path));
                     end if;

                     TIO.Open (Priv_File, TIO.In_File, Priv_Key_Path);
                     TIO.Get_Line (Priv_File, Priv_Hex, Last);
                     TIO.Close (Priv_File);

                     --  Convert hex to binary
                     Hex_To_Private_Key (Priv_Hex, Private_Key, Success);
                     if not Success then
                        return (Success => False,
                                Bundle_Path => Null_Unbounded_String,
                                Error_Msg => To_Unbounded_String ("Invalid private key format"));
                     end if;

                     --  Sign manifest hash (shell-based MVP)
                     --  TODO: Replace with direct OpenSSL bindings once Spawn is fixed
                     declare
                        use GNAT.OS_Lib;
                        Script_Path : constant String := "/tmp/ct-sign-" &
                                                        Ada.Strings.Fixed.Trim (Key_Id_S, Ada.Strings.Both) &
                                                        ".sh";
                        Sig_Path    : constant String := "/tmp/ct-sig-" &
                                                        Ada.Strings.Fixed.Trim (Key_Id_S, Ada.Strings.Both) &
                                                        ".hex";
                        Script_File : TIO.File_Type;
                        Sig_File    : TIO.File_Type;
                        Sig_Hex     : String (1 .. 128);
                        Args        : Argument_List_Access;
                        Sig_Last    : Natural;
                     begin
                        --  Create signing script
                        TIO.Create (Script_File, TIO.Out_File, Script_Path);
                        TIO.Put_Line (Script_File, "#!/bin/bash");
                        TIO.Put_Line (Script_File, "set -e");
                        TIO.Put_Line (Script_File, "TEMP=$(mktemp -d)");
                        TIO.Put_Line (Script_File, "cd ""$TEMP""");
                        --  Write hex private key (first 64 hex chars = 32-byte seed)
                        TIO.Put_Line (Script_File, "echo -n '" & Priv_Hex (1 .. 64) & "' > seed.hex");
                        --  Write message to sign
                        TIO.Put_Line (Script_File, "echo -n '" & Manifest_Hash & "' > msg.txt");
                        --  Convert seed to DER format (add DER header + 32-byte seed)
                        TIO.Put_Line (Script_File,
                           "echo -ne '\x30\x2e\x02\x01\x00\x30\x05\x06\x03\x2b\x65\x70\x04\x22\x04\x20' " &
                           "> key.der");
                        TIO.Put_Line (Script_File,
                           "python3 -c ""import sys; " &
                           "sys.stdout.buffer.write(bytes.fromhex('$(cat seed.hex)'))"" >> key.der");
                        --  Convert DER to PEM
                        TIO.Put_Line (Script_File,
                           "openssl pkey -inform DER -in key.der -out key.pem 2>/dev/null");
                        --  Sign message and write to final destination
                        TIO.Put_Line (Script_File,
                           "openssl pkeyutl -sign -inkey key.pem -in msg.txt 2>/dev/null | " &
                           "hexdump -v -e '/1 ""%02x""' > """ & Sig_Path & """");
                        TIO.Put_Line (Script_File, "cd / && rm -rf ""$TEMP""");
                        TIO.Close (Script_File);

                        --  Make executable and run
                        Args := new Argument_List'(
                           new String'("+x"),
                           new String'(Script_Path)
                        );
                        Spawn ("/usr/bin/chmod", Args.all, Success);
                        Free (Args (1));
                        Free (Args (2));
                        Free (Args);

                        Args := new Argument_List'(1 => new String'(Script_Path));
                        Spawn ("/bin/bash", Args.all, Success);
                        Free (Args (1));
                        Free (Args);

                        --  Clean up script (commented for debugging)
                        --  begin
                        --     Dir.Delete_File (Script_Path);
                        --  exception
                        --     when others => null;
                        --  end;

                        --  Read signature
                        if not Dir.Exists (Sig_Path) then
                           return (Success => False,
                                   Bundle_Path => Null_Unbounded_String,
                                   Error_Msg => To_Unbounded_String ("Signature generation failed"));
                        end if;

                        TIO.Open (Sig_File, TIO.In_File, Sig_Path);
                        TIO.Get_Line (Sig_File, Sig_Hex, Sig_Last);
                        TIO.Close (Sig_File);
                        Dir.Delete_File (Sig_Path);

                        --  Convert to binary
                        Hex_To_Signature (Sig_Hex, Signature, Success);
                        if not Success then
                           return (Success => False,
                                   Bundle_Path => Null_Unbounded_String,
                                   Error_Msg => To_Unbounded_String ("Invalid signature format"));
                        end if;
                     end;

                     --  Get key info from trust store
                     Result := Get_Key (Key_Id_S, Info);
                     if Result /= OK then
                        return (Success => False,
                                Bundle_Path => Null_Unbounded_String,
                                Error_Msg => To_Unbounded_String ("Key not found in trust store: " &
                                                                  Key_Id_S));
                     end if;

                     --  Prepare attestation data
                     Signature_Hex := To_Unbounded_String (Signature_To_Hex (Signature));
                     Key_Id_Str    := To_Unbounded_String (Key_Id_S);
                     Fingerprint   := To_Unbounded_String (Info.Fingerprint (1 .. Info.Finger_Len));
                     Timestamp_Str := To_Unbounded_String (Image (Ada.Calendar.Clock));

                     if Opts.Verbose then
                        TIO.Put_Line ("✓ Signed with key: " & Key_Id_S);
                        TIO.Put_Line ("  Fingerprint: " & To_String (Fingerprint));
                     end if;
                  end;
               end if;

               --  Write summary.json (with or without signature)
               Write_Summary_Json (
                  Path          => Summary_Path,
                  Manifest      => Parse_Result.Value,
                  Manifest_Hash => Manifest_Hash,
                  Signature_Hex => To_String (Signature_Hex),
                  Key_Id        => To_String (Key_Id_Str),
                  Fingerprint   => To_String (Fingerprint),
                  Timestamp     => To_String (Timestamp_Str)
               );

               if Opts.Verbose then
                  TIO.Put_Line ("Created summary: " & Summary_Path);
               end if;
            end;

            --  Create bundle
            declare
               Tar_Success : Boolean;
               Tar_Error   : Unbounded_String;
               Source_Dir  : constant String := To_String (Opts.Source_Dir);
            begin
               Create_Tar_Bundle
                 (Output_Path   => Output_Path_Str,
                  Manifest_Path => Manifest_Path_Str,
                  Summary_Path  => Summary_Path,
                  Source_Dir    => Source_Dir,
                  Verbose       => Opts.Verbose,
                  Success       => Tar_Success,
                  Error_Msg     => Tar_Error);

               if Tar_Success then
                  return (Success => True,
                          Bundle_Path => To_Unbounded_String (Output_Path_Str & ".ctp"),
                          Error_Msg => Null_Unbounded_String);
               else
                  return (Success => False,
                          Bundle_Path => Null_Unbounded_String,
                          Error_Msg => Tar_Error);
               end if;
            end;
         end;
      end;
   end Create_Bundle;

end Cerro_Pack;
