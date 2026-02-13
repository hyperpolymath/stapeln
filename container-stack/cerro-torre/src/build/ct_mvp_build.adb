--  SPDX-License-Identifier: PMPL-1.0 OR PMPL-1.0-or-later
--  MVP build pipeline (Ada orchestration with external tools)

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;
with GNAT.OS_Lib;

package body CT_MVP_Build is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;

   type Manifest_Data is record
      Name          : Unbounded_String;
      Version       : Unbounded_String;
      Upstream      : Unbounded_String;
      Upstream_Hash : Unbounded_String;
   end record;

   procedure Run_Command (Command : String) is
      Status : Integer := 0;
      Args   : GNAT.OS_Lib.Argument_List (1 .. 2);
   begin
      Args (1) := new String'("-c");
      Args (2) := new String'(Command);
      GNAT.OS_Lib.Spawn ("/bin/sh", Args, Status);
      GNAT.OS_Lib.Free (Args);
      if Status /= 0 then
         raise Program_Error with "Command failed: " & Command;
      end if;
   end Run_Command;

   function Read_First_Token (Path : String) return String is
      File  : File_Type;
      Line  : Unbounded_String := To_Unbounded_String ("");
   begin
      Open (File, In_File, Path);
      Line := To_Unbounded_String (Get_Line (File));
      Close (File);
      declare
         Parts : constant String := Trim (To_String (Line), Both);
         Space : constant Natural := Index (Parts, " ");
      begin
         if Space = 0 then
            return Parts;
         else
            return Parts (Parts'First .. Space - 1);
         end if;
      end;
   end Read_First_Token;

   function Read_File (Path : String) return String is
      File   : File_Type;
      Buffer : Unbounded_String := To_Unbounded_String ("");
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Append (Buffer, Line);
         end;
      end loop;
      Close (File);
      return To_String (Buffer);
   end Read_File;

   function Parse_Manifest (Path : String) return Manifest_Data is
      File    : File_Type;
      Current : Unbounded_String := To_Unbounded_String ("");
      Result  : Manifest_Data;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line : String := Trim (Get_Line (File), Both);
         begin
            if Line'Length = 0 or else Line (Line'First) = '#' then
               null;
            elsif Line (Line'First) = '[' and then Line (Line'Last) = ']' then
               Current := To_Unbounded_String (Line (Line'First + 1 .. Line'Last - 1));
            elsif Index (Line, "=") > 0 then
               declare
                  Eq    : constant Natural := Index (Line, "=");
                  Key   : constant String := Trim (Line (Line'First .. Eq - 1), Both);
                  Value : String := Trim (Line (Eq + 1 .. Line'Last), Both);
               begin
                  if Value'Length >= 2 and then Value (Value'First) = '"' and then Value (Value'Last) = '"' then
                     Value := Value (Value'First + 1 .. Value'Last - 1);
                  end if;
                  if To_String (Current) = "metadata" then
                     if Key = "name" then
                        Result.Name := To_Unbounded_String (Value);
                     elsif Key = "version" then
                        Result.Version := To_Unbounded_String (Value);
                     end if;
                  elsif To_String (Current) = "provenance" then
                     if Key = "upstream" then
                        Result.Upstream := To_Unbounded_String (Value);
                     elsif Key = "upstream_hash" then
                        Result.Upstream_Hash := To_Unbounded_String (Value);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
      Close (File);
      return Result;
   end Parse_Manifest;

   procedure Write_File (Path : String; Content : String) is
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      Put (File, Content);
      Close (File);
   end Write_File;

   procedure Run (Manifest_Path : String; Out_Dir : String; Attach : Boolean) is
      Data        : constant Manifest_Data := Parse_Manifest (Manifest_Path);
      Name        : constant String := To_String (Data.Name);
      Version     : constant String := To_String (Data.Version);
      Upstream    : constant String := To_String (Data.Upstream);
      Upstream_Hash : constant String := To_String (Data.Upstream_Hash);
      Rootfs_Dir  : constant String := Out_Dir & "/rootfs";
      Oci_Dir     : constant String := Out_Dir & "/oci";
      Blobs_Dir   : constant String := Oci_Dir & "/blobs/sha256";
      Keys_Dir    : constant String := Out_Dir & "/keys";
      Temp_Dir    : constant String := Out_Dir & "/tmp";
      Archive_Path : constant String := Temp_Dir & "/source.tar.gz";
      Layer_Path  : constant String := Out_Dir & "/layer.tar";
      Config_Path : constant String := Out_Dir & "/config.json";
      Manifest_Path_Local : constant String := Out_Dir & "/manifest.json";
      Index_Path  : constant String := Oci_Dir & "/index.json";
      Layout_Path : constant String := Oci_Dir & "/oci-layout";
      Log_Entry_Path : constant String := Out_Dir & "/log-entry.json";
      PAE_Path    : constant String := Out_Dir & "/pae.bin";
      Sig_Path    : constant String := Out_Dir & "/sig.bin";
      Bundle_Path : constant String := Out_Dir & "/bundle.json";
      Trust_Path  : constant String := Out_Dir & "/trust-store.json";
      Summary_Path : constant String := Out_Dir & "/summary.json";
   begin
      if Name = "" or else Version = "" then
         raise Program_Error with "Manifest metadata.name and metadata.version required";
      end if;
      if Upstream = "" or else Upstream_Hash'Length < 8 or else Upstream_Hash (Upstream_Hash'First .. Upstream_Hash'First + 6) /= "sha256:" then
         raise Program_Error with "Manifest provenance.upstream and sha256 upstream_hash required";
      end if;

      Ada.Directories.Create_Path (Out_Dir);
      Ada.Directories.Create_Path (Temp_Dir);
      Ada.Directories.Create_Path (Rootfs_Dir);
      Ada.Directories.Create_Path (Blobs_Dir);
      Ada.Directories.Create_Path (Keys_Dir);

      Run_Command ("curl -fsSL -o " & Archive_Path & " " & Upstream);
      Run_Command ("sha256sum " & Archive_Path & " > " & Temp_Dir & "/source.sha256");
      if Read_First_Token (Temp_Dir & "/source.sha256") /= Upstream_Hash (Upstream_Hash'First + 7 .. Upstream_Hash'Last) then
         raise Program_Error with "Upstream hash mismatch";
      end if;

      Run_Command ("tar -xf " & Archive_Path & " -C " & Rootfs_Dir);
      Run_Command ("tar -cf " & Layer_Path & " -C " & Rootfs_Dir & " .");
      Run_Command ("sha256sum " & Layer_Path & " > " & Temp_Dir & "/layer.sha256");
      Run_Command ("stat -c %s " & Layer_Path & " > " & Temp_Dir & "/layer.size");

      declare
         Layer_Digest : constant String := Read_First_Token (Temp_Dir & "/layer.sha256");
         Layer_Size   : constant String := Read_File (Temp_Dir & "/layer.size");
         Config_Json  : constant String :=
           "{""created"":""2025-01-01T00:00:00Z"",""architecture"":""amd64"",""os"":""linux""," &
           """config"":{""Labels"":{""org.opencontainers.image.ref.name"":""" & Name & ":" & Version & """}}," &
           """rootfs"":{""type"":""layers"",""diff_ids"":[""sha256:" & Layer_Digest & """]}," &
           """history"":[{""created"":""2025-01-01T00:00:00Z"",""created_by"":""ct_build""}]}" ;
      begin
         Write_File (Config_Path, Config_Json);
         Run_Command ("sha256sum " & Config_Path & " > " & Temp_Dir & "/config.sha256");
         Run_Command ("stat -c %s " & Config_Path & " > " & Temp_Dir & "/config.size");
         declare
            Config_Digest : constant String := Read_First_Token (Temp_Dir & "/config.sha256");
            Config_Size   : constant String := Read_File (Temp_Dir & "/config.size");
            Manifest_Json : constant String :=
              "{""schemaVersion"":2,""mediaType"":""application/vnd.oci.image.manifest.v1+json""," &
              """config"":{""mediaType"":""application/vnd.oci.image.config.v1+json"",""digest"":""sha256:" &
              Config_Digest & """,""size"":" & Trim (Config_Size, Both) & "}," &
              """layers"":[{""mediaType"":""application/vnd.oci.image.layer.v1.tar"",""digest"":""sha256:" &
              Layer_Digest & """,""size"":" & Trim (Layer_Size, Both) & "}]}";
         begin
            Write_File (Manifest_Path_Local, Manifest_Json);
            Run_Command ("sha256sum " & Manifest_Path_Local & " > " & Temp_Dir & "/manifest.sha256");
            declare
               Manifest_Digest : constant String := Read_First_Token (Temp_Dir & "/manifest.sha256");
               Index_Json : constant String :=
                 "{""schemaVersion"":2,""manifests"":[{""mediaType"":""application/vnd.oci.image.manifest.v1+json"",""digest"":""sha256:" &
                 Manifest_Digest & """,""size"":" & Trim (Integer'Image (Manifest_Json'Length), Both) &
                 ",""annotations"":{""org.opencontainers.image.ref.name"":""" & Name & ":" & Version & """}}]}";
            begin
               Run_Command ("cp " & Config_Path & " " & Blobs_Dir & "/" & Config_Digest);
               Run_Command ("cp " & Layer_Path & " " & Blobs_Dir & "/" & Layer_Digest);
               Run_Command ("cp " & Manifest_Path_Local & " " & Blobs_Dir & "/" & Manifest_Digest);
               Write_File (Index_Path, Index_Json);
               Write_File (Layout_Path, "{""imageLayoutVersion"":""1.0.0""}");
            end;
         end;
      end;

      --  Key generation
      Run_Command ("openssl genpkey -algorithm ED25519 -out " & Keys_Dir & "/signer.key");
      Run_Command ("openssl pkey -in " & Keys_Dir & "/signer.key -pubout -out " & Keys_Dir & "/signer.pub");
      Run_Command ("openssl genpkey -algorithm ED25519 -out " & Keys_Dir & "/log.key");
      Run_Command ("openssl pkey -in " & Keys_Dir & "/log.key -pubout -out " & Keys_Dir & "/log.pub");

      Run_Command ("openssl pkey -pubin -in " & Keys_Dir & "/signer.pub -outform DER > " & Temp_Dir & "/signer.der");
      Run_Command ("openssl pkey -pubin -in " & Keys_Dir & "/log.pub -outform DER > " & Temp_Dir & "/log.der");
      Run_Command ("sha256sum " & Temp_Dir & "/signer.der > " & Temp_Dir & "/signer.id");
      Run_Command ("sha256sum " & Temp_Dir & "/log.der > " & Temp_Dir & "/log.id");
      Run_Command ("base64 -w0 " & Temp_Dir & "/signer.der > " & Temp_Dir & "/signer.b64");
      Run_Command ("base64 -w0 " & Temp_Dir & "/log.der > " & Temp_Dir & "/log.b64");

      declare
         Signer_Id : constant String := "sha256:" & Read_First_Token (Temp_Dir & "/signer.id");
         Log_Id    : constant String := "verified-container-log-mvp";
         Signer_Key : constant String := Trim (Read_File (Temp_Dir & "/signer.b64"), Both);
         Log_Key    : constant String := Trim (Read_File (Temp_Dir & "/log.b64"), Both);
         Trust_Json : constant String :=
           "{""$schema"":""https://verified-container.org/schema/trust-store-v1.json"",""version"":1," &
           """id"":""cerro-torre-mvp"",""updated"":""2025-01-01T00:00:00Z""," &
           """keys"":{""builders"":[{""id"":""" & Signer_Id & """,""algorithm"":""ed25519"",""publicKey"":""" &
           Signer_Key & """,""validFrom"":""2024-01-01T00:00:00Z""}]}," &
           """thresholds"":{},""logs"":{""" & Log_Id & """:{""operator"":""Cerro Torre MVP Log"",""publicKey"":""" &
           Log_Key & """,""url"":""https://logs.mvp.local"",""algorithm"":""ed25519""}}}";
      begin
         Write_File (Trust_Path, Trust_Json);
      end;

      --  Attestations (SLSA + SPDX) + bundle
      declare
         Image_Digest : constant String := Read_First_Token (Temp_Dir & "/manifest.sha256");
         Subject_Digest : constant String := Image_Digest;
         Payload_Type : constant String := "application/vnd.in-toto+json";
      begin
         --  SLSA statement
         Write_File (Temp_Dir & "/slsa.json",
           "{""_type"":""https://in-toto.io/Statement/v1"",""subject"":[{""name"":""" & Name & ":" & Version &
           """,""digest"":{""sha256"":""" & Subject_Digest & """}}]," &
           """predicateType"":""https://slsa.dev/provenance/v1"",""predicate"":{""buildType"":""https://cerro-torre.org/build/mvp"",""builder"":{""id"":""cerro-torre-mvp""},""invocation"":{}}}");
         Run_Command ("base64 -w0 " & Temp_Dir & "/slsa.json > " & Temp_Dir & "/slsa.b64");
         Write_File (PAE_Path,
           "DSSEv1 " & Trim (Integer'Image (Payload_Type'Length), Both) & " " & Payload_Type & " " &
           Trim (Integer'Image (Read_File (Temp_Dir & "/slsa.b64")'Length), Both) & " " &
           Trim (Read_File (Temp_Dir & "/slsa.b64"), Both));
         Run_Command ("openssl pkeyutl -sign -inkey " & Keys_Dir & "/signer.key -rawin -in " & PAE_Path & " -out " & Sig_Path);
         Run_Command ("base64 -w0 " & Sig_Path & " > " & Temp_Dir & "/slsa.sig");

         Write_File (Temp_Dir & "/slsa.dsse",
           "{""payloadType"":""" & Payload_Type & """,""payload"":""" & Trim (Read_File (Temp_Dir & "/slsa.b64"), Both) &
           """,""signatures"":[{""keyid"":""" & Signer_Id &
           """,""sig"":""" & Trim (Read_File (Temp_Dir & "/slsa.sig"), Both) & """}]}");

         --  SPDX statement
         Write_File (Temp_Dir & "/sbom.json",
           "{""_type"":""https://in-toto.io/Statement/v1"",""subject"":[{""name"":""" & Name & ":" & Version &
           """,""digest"":{""sha256"":""" & Subject_Digest & """}}]," &
           """predicateType"":""https://spdx.dev/Document"",""predicate"":{""spdxVersion"":""SPDX-2.3"",""name"":""mvp-sbom""}}");
         Run_Command ("base64 -w0 " & Temp_Dir & "/sbom.json > " & Temp_Dir & "/sbom.b64");
         Write_File (PAE_Path,
           "DSSEv1 " & Trim (Integer'Image (Payload_Type'Length), Both) & " " & Payload_Type & " " &
           Trim (Integer'Image (Read_File (Temp_Dir & "/sbom.b64")'Length), Both) & " " &
           Trim (Read_File (Temp_Dir & "/sbom.b64"), Both));
         Run_Command ("openssl pkeyutl -sign -inkey " & Keys_Dir & "/signer.key -rawin -in " & PAE_Path & " -out " & Sig_Path);
         Run_Command ("base64 -w0 " & Sig_Path & " > " & Temp_Dir & "/sbom.sig");

         Write_File (Temp_Dir & "/sbom.dsse",
           "{""payloadType"":""" & Payload_Type & """,""payload"":""" & Trim (Read_File (Temp_Dir & "/sbom.b64"), Both) &
           """,""signatures"":[{""keyid"":""" & Signer_Id &
           """,""sig"":""" & Trim (Read_File (Temp_Dir & "/sbom.sig"), Both) & """}]}");

         Run_Command ("sha256sum " & Temp_Dir & "/slsa.dsse > " & Temp_Dir & "/slsa.dsse.sha256");
         Run_Command ("sha256sum " & Temp_Dir & "/sbom.dsse > " & Temp_Dir & "/sbom.dsse.sha256");
         Run_Command ("date -u +%Y-%m-%dT%H:%M:%SZ > " & Temp_Dir & "/time.txt");

         declare
            Timestamp : constant String := Trim (Read_File (Temp_Dir & "/time.txt"), Both);
            Slsa_Digest : constant String := "sha256:" & Read_First_Token (Temp_Dir & "/slsa.dsse.sha256");
            Sbom_Digest : constant String := "sha256:" & Read_First_Token (Temp_Dir & "/sbom.dsse.sha256");
         begin
            Write_File (Log_Entry_Path,
              "{""version"":1,""timestamp"":""" & Timestamp & """,""entryType"":""attestation"",""body"":{""attestationDigest"":""" &
              Slsa_Digest & """,""subjectDigest"":""sha256:" & Subject_Digest & """,""predicateType"":""https://slsa.dev/provenance/v1""}}");
            Run_Command ("openssl pkeyutl -sign -inkey " & Keys_Dir & "/log.key -rawin -in " & Log_Entry_Path & " -out " & Sig_Path);
            Run_Command ("base64 -w0 " & Sig_Path & " > " & Temp_Dir & "/log.sig");

            Write_File (Bundle_Path,
              "{""mediaType"":""application/vnd.verified-container.bundle+json"",""version"":""0.1.0"",""attestations"":[" &
              Read_File (Temp_Dir & "/slsa.dsse") & "," & Read_File (Temp_Dir & "/sbom.dsse") & "]," &
              """" & "logEntries" & """:[" &
              "{""logId"":""verified-container-log-mvp"",""logIndex"":0,""integratedTime"":""" & Timestamp &
              """,""inclusionProof"":{""logIndex"":0,""rootHash"":""" & Read_First_Token (Temp_Dir & "/slsa.dsse.sha256") &
              """,""treeSize"":1,""hashes"":[]},""signedEntryTimestamp"":""" &
              Trim (Read_File (Temp_Dir & "/log.sig"), Both) & """}" &
              "]}");
         end;
      end;

      if Attach then
         Run_Command ("tools/mvp/publish_bundle.sh " & Name & ":" & Version & " " & Bundle_Path & " application/vnd.verified-container.bundle+json");
      end if;

      Write_File (Summary_Path,
        "{""name"":""" & Name & """,""version"":""" & Version & """,""imageDigest"":""sha256:" &
        Read_First_Token (Temp_Dir & "/manifest.sha256") & """,""bundle"":""" & Bundle_Path &
        """,""trustStore"":""" & Trust_Path & """}");
   end Run;

end CT_MVP_Build;
