--  Cerro_CLI - Keygen using Rust cerro-sign binary
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Replacement for shell script keygen

with Ada.Text_IO;
with Ada.Directories;
with Ada.Environment_Variables;
with GNAT.OS_Lib;
with Cerro_Trust_Store;
with Cerro_Crypto_OpenSSL;

package body Cerro_CLI_Keygen_Rust is

   package TIO renames Ada.Text_IO;
   package Dir renames Ada.Directories;

   procedure Run_Keygen is
      use Cerro_Crypto_OpenSSL;
      use Cerro_Trust_Store;

      Key_Id      : Unbounded_String := To_Unbounded_String ("ct-key-default");
      Output_Dir  : Unbounded_String;
      Private_Key : Ed25519_Private_Key;
      Public_Key  : Ed25519_Public_Key;
      Success     : Boolean;
   begin
      --  Parse arguments (simplified for demonstration)
      --  In production, integrate with existing argument parsing

      --  Set up output directory
      declare
         Home_Dir : constant String := Ada.Environment_Variables.Value ("HOME");
         Keys_Dir : constant String := Home_Dir & "/.config/cerro-torre/keys/";
      begin
         if not Dir.Exists (Keys_Dir) then
            Dir.Create_Path (Keys_Dir);
         end if;
         Output_Dir := To_Unbounded_String (Keys_Dir);
      end;

      --  Call Rust cerro-sign binary for keygen
      declare
         use GNAT.OS_Lib;
         Cerro_Sign  : constant String := "cerro-sign";  --  Assume in PATH or bin/
         Priv_Path   : constant String := To_String (Output_Dir) & To_String (Key_Id) & ".priv";
         Pub_Path    : constant String := To_String (Output_Dir) & To_String (Key_Id) & ".pub";
         Args        : Argument_List :=
            (new String'("keygen"),
             new String'("--priv-key"),
             new String'(Priv_Path),
             new String'("--pub-key"),
             new String'(Pub_Path));
         Exit_Status : Integer;
      begin
         --  Execute cerro-sign keygen
         Spawn (Cerro_Sign, Args, Exit_Status, True);

         --  Free arguments
         for I in Args'Range loop
            Free (Args (I));
         end loop;

         if Exit_Status /= 0 then
            TIO.Put_Line ("✗ Keygen failed (cerro-sign exit code: " &
                         Integer'Image (Exit_Status) & ")");
            return;
         end if;

         --  Read generated public key for trust store import
         declare
            Pub_File : TIO.File_Type;
            Pub_Hex  : String (1 .. 64);
            Last     : Natural;
         begin
            TIO.Open (Pub_File, TIO.In_File, Pub_Path);
            TIO.Get_Line (Pub_File, Pub_Hex, Last);
            TIO.Close (Pub_File);

            --  Convert hex to binary
            Hex_To_Public_Key (Pub_Hex (1 .. Last), Public_Key, Success);
            if not Success then
               TIO.Put_Line ("✗ Invalid public key format");
               return;
            end if;
         end;
      end;

      --  Import to trust store
      declare
         Result : Store_Status;
         Fingerprint : Cerro_Crypto.SHA256_Digest;
      begin
         --  Compute fingerprint of public key
         Fingerprint := Cerro_Crypto.Compute_SHA256 (
            Public_Key_To_String (Public_Key));

         --  Import public key
         Result := Import_Key (
            Key_Id_Val    => To_String (Key_Id),
            Public_Key    => Public_Key,
            Fingerprint   => Cerro_Crypto.Bytes_To_Hex (Fingerprint),
            Trust_Level   => Ultimate,
            Key_Type_Val  => Ed25519,
            Suite         => CT_SIG_01);

         case Result is
            when OK =>
               TIO.Put_Line ("✓ Private key saved: " &
                           To_String (Output_Dir) & To_String (Key_Id) & ".priv");
               TIO.Put_Line ("✓ Public key imported to trust store");
               TIO.Put_Line ("✓ Trust level set to 'ultimate'");
            when others =>
               TIO.Put_Line ("✗ Failed to import public key");
         end case;
      end;
   end Run_Keygen;

end Cerro_CLI_Keygen_Rust;
