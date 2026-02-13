--  Cerro_Pack - Package bundling operations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Creates .ctp bundles from manifests and source files.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Cerro_Pack is

   --  Result of pack operation
   type Pack_Result is record
      Success     : Boolean;
      Bundle_Path : Unbounded_String;  --  Output file path if success
      Error_Msg   : Unbounded_String;  --  Error message if failure
   end record;

   --  Pack options
   type Pack_Options is record
      Manifest_Path : Unbounded_String;  --  Path to manifest.ctp
      Output_Path   : Unbounded_String;  --  Output .ctp bundle path
      Source_Dir    : Unbounded_String;  --  Directory containing sources
      Sign          : Boolean := False;   --  Whether to sign bundle
      Key_Id        : Unbounded_String;  --  Signing key ID (required if Sign=True)
      Verbose       : Boolean := False;   --  Verbose output
   end record;

   --  Create a .ctp bundle from manifest and sources
   function Create_Bundle (Opts : Pack_Options) return Pack_Result;

   --  Compute SHA-256 hash of a file
   function Hash_File (Path : String) return String;

end Cerro_Pack;
