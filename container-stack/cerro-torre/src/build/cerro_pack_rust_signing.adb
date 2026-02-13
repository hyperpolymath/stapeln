--  Cerro_Pack - Signing using Rust cerro-sign binary
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Replacement for shell script signing

with Ada.Text_IO;
with Ada.Directories;
with GNAT.OS_Lib;
with Ada.Strings.Fixed;

package body Cerro_Pack_Rust_Signing is

   package TIO renames Ada.Text_IO;
   package Dir renames Ada.Directories;

   ---------------------------------------------------------------------------
   --  Sign_Manifest_Hash - Sign a manifest hash using Rust cerro-sign
   ---------------------------------------------------------------------------

   function Sign_Manifest_Hash
      (Manifest_Hash : String;
       Private_Key_Path : String;
       Key_Id : String) return Sign_Result
   is
      use GNAT.OS_Lib;

      Temp_Msg    : constant String := "/tmp/ct-msg-" &
                                      Ada.Strings.Fixed.Trim (Key_Id, Ada.Strings.Both) & ".txt";
      Temp_Sig    : constant String := "/tmp/ct-sig-" &
                                      Ada.Strings.Fixed.Trim (Key_Id, Ada.Strings.Both) & ".hex";
      Cerro_Sign  : constant String := "cerro-sign";  --  Assume in PATH or bin/
      Args        : Argument_List :=
         (new String'("sign"),
          new String'("--key"),
          new String'(Private_Key_Path),
          new String'("--message"),
          new String'(Manifest_Hash),
          new String'("--output"),
          new String'(Temp_Sig));
      Exit_Status : Integer;
      Sig_File    : TIO.File_Type;
      Sig_Hex     : String (1 .. 128);
      Sig_Last    : Natural;
      Success     : Boolean;
   begin
      --  Execute cerro-sign
      Spawn (Cerro_Sign, Args, Exit_Status, True);

      --  Free arguments
      for I in Args'Range loop
         Free (Args (I));
      end loop;

      if Exit_Status /= 0 then
         return (Success => False,
                 Signature => (others => 0));
      end if;

      --  Read signature
      if not Dir.Exists (Temp_Sig) then
         return (Success => False,
                 Signature => (others => 0));
      end if;

      TIO.Open (Sig_File, TIO.In_File, Temp_Sig);
      TIO.Get_Line (Sig_File, Sig_Hex, Sig_Last);
      TIO.Close (Sig_File);

      --  Clean up temp file
      Dir.Delete_File (Temp_Sig);

      --  Convert hex to binary signature
      declare
         Signature : Cerro_Crypto.Ed25519_Signature;
      begin
         Hex_To_Signature (Sig_Hex (1 .. Sig_Last), Signature, Success);

         if not Success then
            return (Success => False,
                    Signature => (others => 0));
         end if;

         return (Success => True,
                 Signature => Signature);
      end;
   end Sign_Manifest_Hash;

end Cerro_Pack_Rust_Signing;
