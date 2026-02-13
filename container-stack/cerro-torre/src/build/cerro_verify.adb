--  Cerro_Verify - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later

with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Text_IO;
with Cerro_Crypto;

package body Cerro_Verify is

   Debug : constant Boolean := False;  --  Set True for debug output

   package SIO renames Ada.Streams.Stream_IO;
   package Dir renames Ada.Directories;

   --  TAR block size
   Block_Size : constant := 512;

   ---------------------------------------------------------------------------
   --  To_Exit_Code - Map verification code to process exit code
   ---------------------------------------------------------------------------

   function To_Exit_Code (Code : Verify_Code) return Integer is
   begin
      case Code is
         when OK                  => return 0;
         when Hash_Mismatch       => return 1;
         when Signature_Invalid   => return 2;
         when Key_Not_Trusted     => return 3;
         when Policy_Rejection    => return 4;
         when Missing_Attestation => return 5;
         when Malformed_Bundle    => return 10;
         when IO_Error            => return 11;
      end case;
   end To_Exit_Code;

   ---------------------------------------------------------------------------
   --  Extract_File_From_Tar - Extract a named file from tar archive
   ---------------------------------------------------------------------------

   function Extract_File_From_Tar
     (Archive_Path : String;
      File_Name    : String) return Unbounded_String
   is
      use Ada.Streams;

      File    : SIO.File_Type;
      Header  : Stream_Element_Array (1 .. Block_Size);
      Last    : Stream_Element_Offset;
      Content : Unbounded_String := Null_Unbounded_String;
   begin
      if not Dir.Exists (Archive_Path) then
         return Null_Unbounded_String;
      end if;

      SIO.Open (File, SIO.In_File, Archive_Path);

      --  Process tar entries
      while not SIO.End_Of_File (File) loop
         --  Read header block
         SIO.Read (File, Header, Last);
         if Last < Block_Size then
            exit;  --  Incomplete header
         end if;

         --  Check for end-of-archive (two zero blocks)
         declare
            All_Zero : Boolean := True;
         begin
            for I in Header'Range loop
               if Header (I) /= 0 then
                  All_Zero := False;
                  exit;
               end if;
            end loop;
            if All_Zero then
               exit;  --  End of archive
            end if;
         end;

         --  Extract name (first 100 bytes, NUL-terminated)
         declare
            Name : String (1 .. 100);
            Name_Len : Natural := 0;
         begin
            for I in 1 .. 100 loop
               Name (I) := Character'Val (Integer (Header (Stream_Element_Offset (I))));
               if Name (I) = ASCII.NUL then
                  Name_Len := I - 1;
                  exit;
               end if;
               Name_Len := I;
            end loop;

            --  Extract size (bytes 125-136 in 1-based, which is 124-135 in tar spec)
            declare
               Size_Str : String (1 .. 12);
               File_Size : Natural := 0;
            begin
               for I in 1 .. 12 loop
                  Size_Str (I) := Character'Val (Integer (Header (124 + Stream_Element_Offset (I))));
               end loop;

               --  Parse octal size
               for C of Size_Str loop
                  exit when C = ' ' or C = ASCII.NUL;
                  if C in '0' .. '7' then
                     File_Size := File_Size * 8 + (Character'Pos (C) - Character'Pos ('0'));
                  end if;
               end loop;

               if Debug then
                  Ada.Text_IO.Put_Line ("TAR: Found entry '" & Name (1 .. Name_Len) &
                                        "' size=" & Natural'Image (File_Size));
               end if;

               --  Check if this is the file we want
               if Name_Len > 0 and then Name (1 .. Name_Len) = File_Name then
                  --  Read file content
                  declare
                     Data_Blocks : constant Natural := (File_Size + Block_Size - 1) / Block_Size;
                     Data        : Stream_Element_Array (1 .. Stream_Element_Offset (Data_Blocks * Block_Size));
                     Data_Last   : Stream_Element_Offset;
                  begin
                     SIO.Read (File, Data, Data_Last);
                     --  Convert to string (only up to File_Size bytes)
                     for I in 1 .. File_Size loop
                        Append (Content, Character'Val (Integer (Data (Stream_Element_Offset (I)))));
                     end loop;
                  end;
                  SIO.Close (File);
                  return Content;
               else
                  --  Skip this file's content
                  declare
                     Data_Blocks : constant Natural := (File_Size + Block_Size - 1) / Block_Size;
                     Skip_Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Data_Blocks * Block_Size));
                     Skip_Last   : Stream_Element_Offset;
                  begin
                     if Data_Blocks > 0 then
                        SIO.Read (File, Skip_Buffer, Skip_Last);
                     end if;
                  end;
               end if;
            end;
         end;
      end loop;

      SIO.Close (File);
      return Null_Unbounded_String;  --  File not found

   exception
      when others =>
         if SIO.Is_Open (File) then
            SIO.Close (File);
         end if;
         return Null_Unbounded_String;
   end Extract_File_From_Tar;

   ---------------------------------------------------------------------------
   --  Parse_Summary_Json - Extract manifest hash from summary.json
   ---------------------------------------------------------------------------

   function Parse_Manifest_Hash (Summary_Json : String) return String is
      Key     : constant String := """manifest_sha256"": """;
      Key_Pos : Natural := 0;
   begin
      --  Simple JSON parsing - find the key
      for I in Summary_Json'First .. Summary_Json'Last - Key'Length + 1 loop
         if Summary_Json (I .. I + Key'Length - 1) = Key then
            Key_Pos := I + Key'Length;
            exit;
         end if;
      end loop;

      if Key_Pos = 0 then
         return "";
      end if;

      --  Extract 64-char hex hash
      declare
         Hash_End : constant Natural := Key_Pos + 63;
      begin
         if Hash_End <= Summary_Json'Last then
            return Summary_Json (Key_Pos .. Hash_End);
         else
            return "";
         end if;
      end;
   end Parse_Manifest_Hash;

   ---------------------------------------------------------------------------
   --  Parse_Package_Name - Extract package name from summary.json
   ---------------------------------------------------------------------------

   function Parse_Package_Name (Summary_Json : String) return String is
      Key     : constant String := """name"": """;
      Key_Pos : Natural := 0;
   begin
      for I in Summary_Json'First .. Summary_Json'Last - Key'Length + 1 loop
         if Summary_Json (I .. I + Key'Length - 1) = Key then
            Key_Pos := I + Key'Length;
            exit;
         end if;
      end loop;

      if Key_Pos = 0 then
         return "";
      end if;

      --  Find closing quote
      for I in Key_Pos .. Summary_Json'Last loop
         if Summary_Json (I) = '"' then
            return Summary_Json (Key_Pos .. I - 1);
         end if;
      end loop;

      return "";
   end Parse_Package_Name;

   ---------------------------------------------------------------------------
   --  Parse_Package_Version - Extract version from summary.json
   ---------------------------------------------------------------------------

   function Parse_Package_Version (Summary_Json : String) return String is
      Key     : constant String := """version"": """;
      Key_Pos : Natural := 0;
   begin
      for I in Summary_Json'First .. Summary_Json'Last - Key'Length + 1 loop
         if Summary_Json (I .. I + Key'Length - 1) = Key then
            Key_Pos := I + Key'Length;
            exit;
         end if;
      end loop;

      if Key_Pos = 0 then
         return "";
      end if;

      --  Find closing quote
      for I in Key_Pos .. Summary_Json'Last loop
         if Summary_Json (I) = '"' then
            return Summary_Json (Key_Pos .. I - 1);
         end if;
      end loop;

      return "";
   end Parse_Package_Version;

   ---------------------------------------------------------------------------
   --  Verify_Bundle - Main verification entry point
   ---------------------------------------------------------------------------

   function Verify_Bundle (Opts : Verify_Options) return Verify_Result is
      Bundle_Path_Str : constant String := To_String (Opts.Bundle_Path);
      Result : Verify_Result := (Code => OK, others => Null_Unbounded_String);
   begin
      --  Check bundle exists
      if not Dir.Exists (Bundle_Path_Str) then
         Result.Code := IO_Error;
         Result.Details := To_Unbounded_String ("Bundle not found: " & Bundle_Path_Str);
         return Result;
      end if;

      --  Extract summary.json
      declare
         Summary_Content : constant Unbounded_String :=
            Extract_File_From_Tar (Bundle_Path_Str, "summary.json");
         Summary_Str : constant String := To_String (Summary_Content);
      begin
         if Length (Summary_Content) = 0 then
            Result.Code := Malformed_Bundle;
            Result.Details := To_Unbounded_String ("summary.json not found in bundle");
            return Result;
         end if;

         --  Parse expected hash
         declare
            Expected_Hash : constant String := Parse_Manifest_Hash (Summary_Str);
         begin
            if Expected_Hash'Length /= 64 then
               Result.Code := Malformed_Bundle;
               Result.Details := To_Unbounded_String ("Invalid manifest_sha256 in summary.json");
               return Result;
            end if;

            --  Extract package info
            Result.Package_Name := To_Unbounded_String (Parse_Package_Name (Summary_Str));
            Result.Package_Ver := To_Unbounded_String (Parse_Package_Version (Summary_Str));

            --  Extract manifest.ctp
            declare
               Manifest_Content : constant Unbounded_String :=
                  Extract_File_From_Tar (Bundle_Path_Str, "manifest.ctp");
               Manifest_Str : constant String := To_String (Manifest_Content);
            begin
               if Length (Manifest_Content) = 0 then
                  Result.Code := Malformed_Bundle;
                  Result.Details := To_Unbounded_String ("manifest.ctp not found in bundle");
                  return Result;
               end if;

               --  Compute actual hash
               declare
                  Actual_Hash : constant Cerro_Crypto.SHA256_Digest :=
                     Cerro_Crypto.Compute_SHA256 (Manifest_Str);
                  Actual_Hex : constant String := Cerro_Crypto.Bytes_To_Hex (Actual_Hash);
               begin
                  Result.Manifest_Hash := To_Unbounded_String (Actual_Hex);

                  --  Compare hashes
                  if Actual_Hex /= Expected_Hash then
                     Result.Code := Hash_Mismatch;
                     Result.Details := To_Unbounded_String (
                        "Expected: " & Expected_Hash & ASCII.LF &
                        "Actual:   " & Actual_Hex);
                     return Result;
                  end if;
               end;
            end;
         end;
      end;

      --  All checks passed
      Result.Code := OK;
      Result.Details := To_Unbounded_String ("Bundle verification successful");
      return Result;
   end Verify_Bundle;

end Cerro_Verify;
