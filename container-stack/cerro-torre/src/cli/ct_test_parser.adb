--  ct_test_parser - Test program for CTP manifest parser
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Usage: ct_test_parser <manifest.ctp>

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Cerro_Manifest;

procedure CT_Test_Parser is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Cerro_Manifest;
begin
   if Argument_Count < 1 then
      Put_Line ("Usage: ct_test_parser <manifest.ctp>");
      Set_Exit_Status (1);
      return;
   end if;

   declare
      Path   : constant String := Argument (1);
      Result : constant Parse_Result := Parse_File (Path);
   begin
      if Result.Success then
         Put_Line ("✓ Parse successful!");
         Put_Line ("");

         --  Display parsed manifest
         Put_Line ("=== Metadata ===");
         Put_Line ("Name:       " & To_String (Result.Value.Metadata.Name));
         Put_Line ("Version:    " & To_String (Result.Value.Metadata.Version.Upstream));
         Put_Line ("Revision:   " & Positive'Image (Result.Value.Metadata.Version.Revision));
         Put_Line ("Summary:    " & To_String (Result.Value.Metadata.Summary));
         Put_Line ("License:    " & To_String (Result.Value.Metadata.License));
         Put_Line ("Maintainer: " & To_String (Result.Value.Metadata.Maintainer));
         Put_Line ("");

         Put_Line ("=== Provenance ===");
         Put_Line ("Upstream:   " & To_String (Result.Value.Provenance.Upstream_URL));
         if Is_Valid_Hash (Result.Value.Provenance.Upstream_Hash) then
            Put_Line ("Hash algo:  " & Hash_Algorithm'Image (Result.Value.Provenance.Upstream_Hash.Algorithm));
            Put_Line ("Hash value: " & To_String (Result.Value.Provenance.Upstream_Hash.Digest));
         else
            Put_Line ("Hash:       (not set)");
         end if;
         Put_Line ("Imported:   " & To_String (Result.Value.Provenance.Imported_From));
         Put_Line ("");

         Put_Line ("=== Build ===");
         Put_Line ("System:     " & Build_System'Image (Result.Value.Build.System));
         declare
            Flags : constant String_List := Result.Value.Build.Configure_Flags;
         begin
            Put_Line ("Config flags: " & Natural'Image (Natural (Flags.Length)));
            for F of Flags loop
               Put_Line ("  - " & To_String (F));
            end loop;
         end;
         Put_Line ("");

         Put_Line ("=== Outputs ===");
         Put_Line ("Primary:    " & To_String (Result.Value.Outputs.Primary));
         Put_Line ("");

         Put_Line ("=== Dependencies ===");
         Put_Line ("Runtime:    " & Natural'Image (Natural (Result.Value.Dependencies.Runtime.Length)));
         for D of Result.Value.Dependencies.Runtime loop
            Put_Line ("  - " & To_String (D.Name));
         end loop;
         Put_Line ("Build:      " & Natural'Image (Natural (Result.Value.Dependencies.Build.Length)));
         for D of Result.Value.Dependencies.Build loop
            Put_Line ("  - " & To_String (D.Name));
         end loop;
         Put_Line ("");

         Put_Line ("=== Attestations ===");
         Put_Line ("Required:   " & Natural'Image (Natural (Result.Value.Attestations.Required.Length)));
         for A of Result.Value.Attestations.Required loop
            Put_Line ("  - " & Attestation_Type'Image (A));
         end loop;
         Put_Line ("");

         --  Validation
         if Is_Complete (Result.Value) then
            Put_Line ("✓ Manifest is complete");
         else
            Put_Line ("⚠ Manifest is missing required fields");
         end if;

         if Is_Valid_Version (Result.Value.Metadata.Version) then
            Put_Line ("✓ Version is valid");
         else
            Put_Line ("⚠ Version is invalid");
         end if;

      else
         Put_Line ("✗ Parse failed!");
         Put_Line ("Error: " & Parse_Error_Kind'Image (Result.Error));
         Put_Line ("Line:  " & Natural'Image (Result.Error_Line));
         Put_Line ("Msg:   " & To_String (Result.Error_Msg));
         Set_Exit_Status (1);
      end if;
   end;
end CT_Test_Parser;
