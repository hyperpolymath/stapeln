--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Calendar;
with Interfaces;
with GNAT.OS_Lib;
with CT_Errors;
with CT_Registry;
with CT_Transparency;
with Cerro_Pack;
with Cerro_Verify;
with Cerro_Explain;
with Cerro_Trust_Store;
with Cerro_Runtime;
with Cerro_Crypto_OpenSSL;

package body Cerro_CLI is

   use Ada.Text_IO;
   use Ada.Command_Line;
   use type Cerro_Verify.Verify_Code;
   use type Interfaces.Unsigned_64;

   ----------
   -- Pack --
   ----------

   procedure Run_Pack is
      Opts        : Cerro_Pack.Pack_Options;
      Output_Set  : Boolean := False;
      Verbose     : Boolean := False;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct pack <manifest.ctp> -o <output.ctp>");
         Put_Line ("");
         Put_Line ("Create a .ctp bundle from a manifest file.");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct pack ./hello.ctp -o hello-bundle.ctp");
         Put_Line ("  ct pack manifests/nginx.ctp -o nginx-bundle.ctp -v");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -o, --output <file>    Output path for .ctp bundle (required)");
         Put_Line ("  -s, --sources <dir>    Include source files from directory");
         Put_Line ("  --sign                 Sign bundle with Ed25519 key");
         Put_Line ("  --key <id>             Key ID to sign with (required if --sign)");
         Put_Line ("  -v, --verbose          Show detailed progress");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Parse arguments
      Opts.Manifest_Path := To_Unbounded_String (Argument (2));

      declare
         I : Positive := 3;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "-o" or Arg = "--output" then
                  if I < Argument_Count then
                     I := I + 1;
                     Opts.Output_Path := To_Unbounded_String (Argument (I));
                     Output_Set := True;
                  end if;
               elsif Arg = "-s" or Arg = "--sources" then
                  if I < Argument_Count then
                     I := I + 1;
                     Opts.Source_Dir := To_Unbounded_String (Argument (I));
                  end if;
               elsif Arg = "--sign" then
                  Opts.Sign := True;
               elsif Arg = "--key" then
                  if I < Argument_Count then
                     I := I + 1;
                     Opts.Key_Id := To_Unbounded_String (Argument (I));
                  end if;
               elsif Arg = "-v" or Arg = "--verbose" then
                  Opts.Verbose := True;
                  Verbose := True;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate required options
      if not Output_Set then
         Put_Line ("Error: Output path required (-o <file>)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Run pack
      if Verbose then
         Put_Line ("Packing manifest: " & To_String (Opts.Manifest_Path));
         Put_Line ("Output: " & To_String (Opts.Output_Path));
         Put_Line ("");
      end if;

      declare
         Result : constant Cerro_Pack.Pack_Result := Cerro_Pack.Create_Bundle (Opts);
      begin
         if Result.Success then
            Put_Line ("✓ Bundle created: " & To_String (Result.Bundle_Path));
            Set_Exit_Status (0);
         else
            Put_Line ("✗ Pack failed: " & To_String (Result.Error_Msg));
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
         end if;
      end;
   end Run_Pack;

   ------------
   -- Verify --
   ------------

   procedure Run_Verify is
      Opts    : Cerro_Verify.Verify_Options;
      Verbose : Boolean := False;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct verify <bundle.ctp> [--policy <file>]");
         Put_Line ("");
         Put_Line ("Verify a .ctp bundle with specific exit codes.");
         Put_Line ("");
         Put_Line ("Exit codes:");
         Put_Line ("  0   Verification succeeded");
         Put_Line ("  1   Hash mismatch (content tampered)");
         Put_Line ("  2   Signature invalid");
         Put_Line ("  3   Key not trusted by policy");
         Put_Line ("  4   Policy rejection (registry/base not allowed)");
         Put_Line ("  5   Missing required attestation");
         Put_Line ("  10  Malformed bundle");
         Put_Line ("  11  I/O error");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --policy <file>   Trust policy file");
         Put_Line ("  --offline         Skip transparency log checks");
         Put_Line ("  --verbose         Show detailed verification steps");
         Put_Line ("  --json            Output machine-readable JSON");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Parse arguments
      Opts.Bundle_Path := To_Unbounded_String (Argument (2));

      declare
         I : Positive := 3;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "--policy" then
                  if I < Argument_Count then
                     I := I + 1;
                     Opts.Policy_Path := To_Unbounded_String (Argument (I));
                  end if;
               elsif Arg = "--offline" then
                  Opts.Offline := True;
               elsif Arg = "-v" or Arg = "--verbose" then
                  Opts.Verbose := True;
                  Verbose := True;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Run verification
      declare
         Result : constant Cerro_Verify.Verify_Result :=
            Cerro_Verify.Verify_Bundle (Opts);
         Exit_Code : constant Integer := Cerro_Verify.To_Exit_Code (Result.Code);
      begin
         if Verbose then
            Put_Line ("Bundle: " & To_String (Opts.Bundle_Path));
            Put_Line ("");
         end if;

         case Result.Code is
            when Cerro_Verify.OK =>
               Put_Line ("✓ Verification successful");
               if Length (Result.Package_Name) > 0 then
                  Put_Line ("  Package: " & To_String (Result.Package_Name) &
                            " " & To_String (Result.Package_Ver));
               end if;
               if Verbose then
                  Put_Line ("  Manifest hash: " & To_String (Result.Manifest_Hash));
               end if;

            when Cerro_Verify.Hash_Mismatch =>
               Put_Line ("✗ Hash mismatch - content may have been tampered");
               Put_Line ("  " & To_String (Result.Details));

            when Cerro_Verify.Signature_Invalid =>
               Put_Line ("✗ Signature verification failed");

            when Cerro_Verify.Key_Not_Trusted =>
               Put_Line ("✗ Signing key not trusted by policy");

            when Cerro_Verify.Policy_Rejection =>
               Put_Line ("✗ Bundle rejected by policy");
               Put_Line ("  " & To_String (Result.Details));

            when Cerro_Verify.Missing_Attestation =>
               Put_Line ("✗ Missing required attestation");

            when Cerro_Verify.Malformed_Bundle =>
               Put_Line ("✗ Malformed bundle");
               Put_Line ("  " & To_String (Result.Details));

            when Cerro_Verify.IO_Error =>
               Put_Line ("✗ I/O error");
               Put_Line ("  " & To_String (Result.Details));
         end case;

         Set_Exit_Status (Ada.Command_Line.Exit_Status (Exit_Code));
      end;
   end Run_Verify;

   -------------
   -- Explain --
   -------------

   procedure Run_Explain is
   begin
      Cerro_Explain.Run_Explain;
   end Run_Explain;

   ------------
   -- Keygen --
   ------------

   procedure Run_Keygen is
      use Cerro_Crypto_OpenSSL;
      use Cerro_Trust_Store;
      use Ada.Directories;

      Key_Id      : Unbounded_String := To_Unbounded_String ("ct-key-default");
      Suite       : Unbounded_String := To_Unbounded_String ("CT-SIG-01");
      Output_Dir  : Unbounded_String := Null_Unbounded_String;
      Private_Key : Ed25519_Private_Key;
      Public_Key  : Ed25519_Public_Key;
      Success     : Boolean;
   begin
      --  Show help if no arguments
      if Argument_Count < 1 then
         Put_Line ("Usage: ct keygen [--id <name>] [--suite <suite-id>]");
         Put_Line ("");
         Put_Line ("Generate a new signing keypair.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --id <name>        Key identifier (default: auto-generated)");
         Put_Line ("  --suite <suite>    Crypto suite (default: CT-SIG-01)");
         Put_Line ("  --output <dir>     Output directory (default: ~/.config/cerro-torre/keys/)");
         Put_Line ("");
         Put_Line ("Suites:");
         Put_Line ("  CT-SIG-01   Ed25519 (classical, default)");
         Put_Line ("  CT-SIG-02   Ed25519 + ML-DSA-87 (hybrid, v0.2 - not yet)");
         Put_Line ("  CT-SIG-03   ML-DSA-87 (post-quantum only, v0.2 - not yet)");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct keygen");
         Put_Line ("  ct keygen --id my-signing-key");
         Put_Line ("  ct keygen --id prod-2026 --output /secure/keys/");
         Set_Exit_Status (0);
         return;
      end if;

      --  Parse arguments
      declare
         I : Positive := 2;  --  Skip "keygen" command
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "--id" and then I < Argument_Count then
                  Key_Id := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif Arg = "--suite" and then I < Argument_Count then
                  Suite := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif Arg = "--output" and then I < Argument_Count then
                  Output_Dir := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               else
                  Put_Line ("Unknown argument: " & Arg);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;
            end;
         end loop;
      end;

      --  Only CT-SIG-01 (Ed25519) supported in MVP
      if To_String (Suite) /= "CT-SIG-01" then
         Put_Line ("✗ Only CT-SIG-01 (Ed25519) is supported in current version");
         Put_Line ("  Post-quantum suites (CT-SIG-02, CT-SIG-03) coming in v0.2");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Determine output directory
      if Length (Output_Dir) = 0 then
         declare
            Home : constant String := Ada.Environment_Variables.Value ("HOME", "/tmp");
         begin
            Output_Dir := To_Unbounded_String (Home & "/.config/cerro-torre/keys/");
         end;
      end if;

      --  Create output directory if needed
      begin
         if not Exists (To_String (Output_Dir)) then
            Create_Path (To_String (Output_Dir));
         end if;
      exception
         when others =>
            Put_Line ("✗ Failed to create directory: " & To_String (Output_Dir));
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
            return;
      end;

      --  Generate Ed25519 keypair
      Put_Line ("Generating Ed25519 keypair...");

      --  MVP: Use shell script wrapper until Spawn issue is debugged
      --  TODO: Replace with direct OpenSSL bindings once GNAT.OS_Lib.Spawn is fixed
      declare
         use GNAT.OS_Lib;
         Script_Path : constant String := To_String (Output_Dir) & ".keygen.sh";
         Script_File : Ada.Text_IO.File_Type;
         Priv_Path   : constant String := To_String (Output_Dir) & To_String (Key_Id) & ".priv";
         Pub_Path    : constant String := To_String (Output_Dir) & To_String (Key_Id) & ".pub";
         Args        : Argument_List_Access;
      begin
         --  Create temporary shell script
         Ada.Text_IO.Create (Script_File, Ada.Text_IO.Out_File, Script_Path);
         Ada.Text_IO.Put_Line (Script_File, "#!/bin/bash");
         Ada.Text_IO.Put_Line (Script_File, "set -e");
         Ada.Text_IO.Put_Line (Script_File, "TEMP_DIR=$(mktemp -d)");
         Ada.Text_IO.Put_Line (Script_File, "cd ""$TEMP_DIR""");
         Ada.Text_IO.Put_Line (Script_File, "openssl genpkey -algorithm ED25519 -out private.pem 2>/dev/null");
         Ada.Text_IO.Put_Line (Script_File, "openssl pkey -in private.pem -pubout -out public.pem 2>/dev/null");
         Ada.Text_IO.Put_Line (Script_File,
            "openssl pkey -in private.pem -outform DER -out private.der 2>/dev/null");
         Ada.Text_IO.Put_Line (Script_File,
            "openssl pkey -pubin -in public.pem -outform DER -out public.der 2>/dev/null");
         Ada.Text_IO.Put_Line (Script_File,
            "tail -c 32 private.der | hexdump -v -e '/1 ""%02x""' > seed.hex");
         Ada.Text_IO.Put_Line (Script_File,
            "tail -c 32 public.der | hexdump -v -e '/1 ""%02x""' > pub.hex");
         Ada.Text_IO.Put_Line (Script_File,
            "cat seed.hex pub.hex > """ & Priv_Path & """");
         Ada.Text_IO.Put_Line (Script_File,
            "cp pub.hex """ & Pub_Path & """");
         Ada.Text_IO.Put_Line (Script_File, "cd /");
         Ada.Text_IO.Put_Line (Script_File, "rm -rf ""$TEMP_DIR""");
         Ada.Text_IO.Close (Script_File);

         --  Make executable
         Args := new Argument_List'(
            new String'("+x"),
            new String'(Script_Path)
         );
         Spawn ("/usr/bin/chmod", Args.all, Success);
         Free (Args (1));
         Free (Args (2));
         Free (Args);

         --  Execute script
         Args := new Argument_List'(1 => new String'(Script_Path));
         Spawn ("/bin/bash", Args.all, Success);
         Free (Args (1));
         Free (Args);

         --  Delete script
         begin
            Ada.Directories.Delete_File (Script_Path);
         exception
            when others => null;
         end;

         if not Success or else not Ada.Directories.Exists (Priv_Path) then
            Put_Line ("✗ Keypair generation failed");
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
            return;
         end if;

         --  Read generated keys
         declare
            Priv_File : Ada.Text_IO.File_Type;
            Pub_File  : Ada.Text_IO.File_Type;
            Priv_Hex  : String (1 .. 128);
            Pub_Hex   : String (1 .. 64);
            Last      : Natural;
         begin
            Ada.Text_IO.Open (Priv_File, Ada.Text_IO.In_File, Priv_Path);
            Ada.Text_IO.Get_Line (Priv_File, Priv_Hex, Last);
            Ada.Text_IO.Close (Priv_File);

            Ada.Text_IO.Open (Pub_File, Ada.Text_IO.In_File, Pub_Path);
            Ada.Text_IO.Get_Line (Pub_File, Pub_Hex, Last);
            Ada.Text_IO.Close (Pub_File);

            --  Convert hex to binary
            Hex_To_Private_Key (Priv_Hex, Private_Key, Success);
            if not Success then
               Put_Line ("✗ Invalid private key format");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            Hex_To_Public_Key (Pub_Hex, Public_Key, Success);
            if not Success then
               Put_Line ("✗ Invalid public key format");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            --  Delete .pub file, keep .priv
            Ada.Directories.Delete_File (Pub_Path);
         end;
      exception
         when others =>
            Put_Line ("✗ Keypair generation failed");
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
            return;
      end;

      --  Private key already saved by shell script
      Put_Line ("✓ Private key saved: " & To_String (Output_Dir) & To_String (Key_Id) & ".priv");
      Put_Line ("  ⚠️  KEEP THIS FILE SECURE - it can sign bundles as you!");

      --  Import public key to trust store
      declare
         Pub_Hex    : constant String := Public_Key_To_Hex (Public_Key);
         Result     : Store_Result;
      begin
         --  Initialize trust store if needed
         if not Is_Initialized then
            Initialize;
         end if;

         --  Import public key
         Result := Import_Key_Hex (Pub_Hex, To_String (Key_Id));

         if Result = OK then
            Put_Line ("✓ Public key imported to trust store");

            --  Set trust to "ultimate" (it's our own key)
            Result := Set_Trust (To_String (Key_Id), Ultimate);
            if Result = OK then
               Put_Line ("✓ Trust level set to 'ultimate'");
            end if;

            --  Compute and show fingerprint
            declare
               Fingerprint : constant String := Compute_Fingerprint (Pub_Hex);
            begin
               Put_Line ("");
               Put_Line ("Key details:");
               Put_Line ("  ID:          " & To_String (Key_Id));
               Put_Line ("  Suite:       " & To_String (Suite));
               Put_Line ("  Fingerprint: " & Fingerprint);
               Put_Line ("  Public key:  " & Pub_Hex);
               Put_Line ("");
               Put_Line ("To sign a bundle:");
               Put_Line ("  ct pack manifest.ctp -o output.ctp --sign --key " & To_String (Key_Id));
            end;

            Set_Exit_Status (0);
         else
            case Result is
               when Already_Exists =>
                  Put_Line ("✗ Key ID already exists in trust store");
               when IO_Error =>
                  Put_Line ("✗ Failed to write to trust store");
               when Invalid_Key =>
                  Put_Line ("✗ Invalid key format");
               when Invalid_Format =>
                  Put_Line ("✗ Invalid key format");
               when others =>
                  Put_Line ("✗ Failed to import public key");
            end case;
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
         end if;
      end;
   end Run_Keygen;

   ---------
   -- Key --
   ---------

   procedure Print_Key_Info (Info : Cerro_Trust_Store.Key_Info) is
      Trust_Str : constant String :=
         (case Info.Trust is
            when Cerro_Trust_Store.Untrusted => "untrusted",
            when Cerro_Trust_Store.Marginal  => "marginal",
            when Cerro_Trust_Store.Full      => "full",
            when Cerro_Trust_Store.Ultimate  => "ultimate");
   begin
      Put_Line ("  " & Info.Key_Id (1 .. Info.Key_Id_Len));
      Put_Line ("    Fingerprint: " & Info.Fingerprint (1 .. 16) & "...");
      Put_Line ("    Trust: " & Trust_Str);
   end Print_Key_Info;

   procedure Run_Key is
      use Cerro_Trust_Store;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct key <subcommand> [args]");
         Put_Line ("");
         Put_Line ("Key management subcommands:");
         Put_Line ("  list                   List all keys in trust store");
         Put_Line ("  import <file>          Import a public key");
         Put_Line ("  import-hex <hex> <id>  Import key from hex string");
         Put_Line ("  export <id> -o <file>  Export public key");
         Put_Line ("  delete <id>            Remove a key");
         Put_Line ("  trust <id> <level>     Set trust level (untrusted/marginal/full/ultimate)");
         Put_Line ("  default [id]           Show or set default signing key");
         Put_Line ("  info <id>              Show key details");
         Put_Line ("");
         Put_Line ("Trust store: " & Get_Store_Path);
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct key list");
         Put_Line ("  ct key import upstream-nginx.pub");
         Put_Line ("  ct key trust nginx-upstream full");
         Put_Line ("  ct key default my-signing-key");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  Initialize trust store
      Initialize;

      declare
         Subcommand : constant String := Argument (2);
      begin
         --  LIST
         if Subcommand = "list" then
            declare
               Count : constant Natural := Key_Count;
            begin
               if Count = 0 then
                  Put_Line ("No keys in trust store.");
                  Put_Line ("Import keys with: ct key import <file.pub>");
               else
                  Put_Line ("Keys in trust store (" & Natural'Image (Count) & "):");
                  Put_Line ("");
                  For_Each_Key (Print_Key_Info'Access);
               end if;
               Set_Exit_Status (0);
            end;

         --  IMPORT
         elsif Subcommand = "import" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key import <file.pub> [--id <name>]");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Path   : constant String := Argument (3);
               Key_Id : Unbounded_String := Null_Unbounded_String;
               Result : Store_Result;
            begin
               --  Check for --id flag
               if Argument_Count >= 5 and then Argument (4) = "--id" then
                  Key_Id := To_Unbounded_String (Argument (5));
               end if;

               if Length (Key_Id) > 0 then
                  Result := Import_Key (Path, To_String (Key_Id));
               else
                  Result := Import_Key (Path);
               end if;

               case Result is
                  when OK =>
                     Put_Line ("Key imported successfully.");
                     Put_Line ("Set trust level with: ct key trust <id> full");
                     Set_Exit_Status (0);
                  when Already_Exists =>
                     Put_Line ("Error: Key already exists in trust store.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Invalid_Key =>
                     Put_Line ("Error: Invalid key format (expected 64 hex chars).");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Invalid_Format =>
                     Put_Line ("Error: Invalid key ID format.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Not_Found =>
                     Put_Line ("Error: File not found: " & Path);
                     Set_Exit_Status (CT_Errors.Exit_IO_Error);
                  when IO_Error =>
                     Put_Line ("Error: Could not read file.");
                     Set_Exit_Status (CT_Errors.Exit_IO_Error);
               end case;
            end;

         --  IMPORT-HEX
         elsif Subcommand = "import-hex" then
            if Argument_Count < 4 then
               Put_Line ("Usage: ct key import-hex <hex-pubkey> <key-id>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Hex_Key : constant String := Argument (3);
               Key_Id  : constant String := Argument (4);
               Result  : Store_Result;
            begin
               Result := Import_Key_Hex (Hex_Key, Key_Id);
               case Result is
                  when OK =>
                     Put_Line ("Key '" & Key_Id & "' imported successfully.");
                     Set_Exit_Status (0);
                  when Already_Exists =>
                     Put_Line ("Error: Key '" & Key_Id & "' already exists.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when Invalid_Key =>
                     Put_Line ("Error: Invalid hex key (expected 64 hex chars).");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  when others =>
                     Put_Line ("Error: Could not import key.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end case;
            end;

         --  EXPORT
         elsif Subcommand = "export" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key export <key-id> -o <file>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id : constant String := Argument (3);
               Output : Unbounded_String := Null_Unbounded_String;
               Result : Store_Result;
               Info   : Key_Info;
            begin
               --  Check for -o flag
               if Argument_Count >= 5 and then Argument (4) = "-o" then
                  Output := To_Unbounded_String (Argument (5));
               end if;

               if Length (Output) > 0 then
                  Result := Export_Key (Key_Id, To_String (Output));
                  if Result = OK then
                     Put_Line ("Exported to: " & To_String (Output));
                     Set_Exit_Status (0);
                  else
                     Put_Line ("Error: Key not found: " & Key_Id);
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  end if;
               else
                  --  Print to stdout
                  Result := Get_Key (Key_Id, Info);
                  if Result = OK then
                     Put_Line (Info.Public_Key (1 .. Info.Pubkey_Len));
                     Set_Exit_Status (0);
                  else
                     Put_Line ("Error: Key not found: " & Key_Id);
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  end if;
               end if;
            end;

         --  DELETE
         elsif Subcommand = "delete" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key delete <key-id>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id : constant String := Argument (3);
               Result : Store_Result;
            begin
               Result := Delete_Key (Key_Id);
               if Result = OK then
                  Put_Line ("Key '" & Key_Id & "' deleted.");
                  Set_Exit_Status (0);
               else
                  Put_Line ("Error: Key not found: " & Key_Id);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end;

         --  TRUST
         elsif Subcommand = "trust" then
            if Argument_Count < 4 then
               Put_Line ("Usage: ct key trust <key-id> <level>");
               Put_Line ("Levels: untrusted, marginal, full, ultimate");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id    : constant String := Argument (3);
               Level_Str : constant String := Argument (4);
               Level     : Trust_Level;
               Result    : Store_Result;
            begin
               if Level_Str = "untrusted" then
                  Level := Untrusted;
               elsif Level_Str = "marginal" then
                  Level := Marginal;
               elsif Level_Str = "full" then
                  Level := Full;
               elsif Level_Str = "ultimate" then
                  Level := Ultimate;
               else
                  Put_Line ("Error: Invalid trust level: " & Level_Str);
                  Put_Line ("Valid levels: untrusted, marginal, full, ultimate");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;

               Result := Set_Trust (Key_Id, Level);
               if Result = OK then
                  Put_Line ("Trust level for '" & Key_Id & "' set to " & Level_Str);
                  Set_Exit_Status (0);
               else
                  Put_Line ("Error: Key not found: " & Key_Id);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end;

         --  DEFAULT
         elsif Subcommand = "default" then
            if Argument_Count < 3 then
               --  Show current default
               declare
                  Default : constant String := Get_Default_Key;
               begin
                  if Default'Length > 0 then
                     Put_Line ("Default signing key: " & Default);
                  else
                     Put_Line ("No default signing key set.");
                     Put_Line ("Set with: ct key default <key-id>");
                  end if;
                  Set_Exit_Status (0);
               end;
            else
               --  Set default
               declare
                  Key_Id : constant String := Argument (3);
                  Result : Store_Result;
               begin
                  Result := Set_Default_Key (Key_Id);
                  if Result = OK then
                     Put_Line ("Default signing key set to: " & Key_Id);
                     Set_Exit_Status (0);
                  else
                     Put_Line ("Error: Key not found: " & Key_Id);
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  end if;
               end;
            end if;

         --  INFO
         elsif Subcommand = "info" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct key info <key-id>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Key_Id : constant String := Argument (3);
               Info   : Key_Info;
               Result : Store_Result;
            begin
               Result := Get_Key (Key_Id, Info);
               if Result = OK then
                  Put_Line ("Key: " & Info.Key_Id (1 .. Info.Key_Id_Len));
                  Put_Line ("Fingerprint: " & Info.Fingerprint (1 .. Info.Finger_Len));
                  Put_Line ("Public Key: " & Info.Public_Key (1 .. Info.Pubkey_Len));
                  Put_Line ("Trust Level: " &
                     (case Info.Trust is
                        when Untrusted => "untrusted",
                        when Marginal  => "marginal",
                        when Full      => "full",
                        when Ultimate  => "ultimate"));
                  if Info.Created_Len > 0 then
                     Put_Line ("Created: " & Info.Created (1 .. Info.Created_Len));
                  end if;
                  Set_Exit_Status (0);
               else
                  Put_Line ("Error: Key not found: " & Key_Id);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end;

         else
            Put_Line ("Unknown subcommand: " & Subcommand);
            Put_Line ("Run 'ct key' for usage.");
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
         end if;
      end;
   end Run_Key;

   ----------
   -- Sign --
   ----------

   procedure Run_Sign is
      use Cerro_Crypto_OpenSSL;
      use Cerro_Trust_Store;
      use Ada.Directories;

      Key_Id_Str  : Unbounded_String := Null_Unbounded_String;
      Input_Path  : Unbounded_String := Null_Unbounded_String;
      Output_Path : Unbounded_String := Null_Unbounded_String;
      Verbose     : Boolean := False;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct sign <file> --key <key-id> [-o <sig-file>]");
         Put_Line ("");
         Put_Line ("Sign a file with an Ed25519 private key.");
         Put_Line ("Produces a detached .sig file with the hex-encoded signature.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --key <key-id>       Signing key ID (required, or use default)");
         Put_Line ("  -o, --output <file>  Output signature path (default: <file>.sig)");
         Put_Line ("  -v, --verbose        Show detailed progress");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct sign bundle.ctp --key my-key");
         Put_Line ("  ct sign manifest.ctp --key prod-2026 -o manifest.sig");
         Put_Line ("  ct sign data.bin    (uses default signing key)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  First positional argument is the file to sign
      Input_Path := To_Unbounded_String (Argument (2));

      --  Parse remaining arguments
      declare
         I : Positive := 3;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if (Arg = "--key" or Arg = "-k")
                  and then I < Argument_Count
               then
                  Key_Id_Str := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif (Arg = "-o" or Arg = "--output")
                  and then I < Argument_Count
               then
                  Output_Path := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif Arg = "-v" or Arg = "--verbose" then
                  Verbose := True;
                  I := I + 1;
               else
                  Put_Line ("Unknown argument: " & Arg);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;
            end;
         end loop;
      end;

      --  Validate input file exists
      if not Ada.Directories.Exists (To_String (Input_Path)) then
         Put_Line ("Error: File not found: " & To_String (Input_Path));
         Set_Exit_Status (CT_Errors.Exit_IO_Error);
         return;
      end if;

      --  Use default key if none specified
      if Length (Key_Id_Str) = 0 then
         Initialize;
         declare
            Default : constant String := Get_Default_Key;
         begin
            if Default'Length = 0 then
               Put_Line ("Error: No --key specified and no default key set.");
               Put_Line ("  Generate a key: ct keygen --id <name>");
               Put_Line ("  Set default:    ct key default <key-id>");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;
            Key_Id_Str := To_Unbounded_String (Default);
         end;
      end if;

      --  Default output path: <input>.sig
      if Length (Output_Path) = 0 then
         Output_Path := Input_Path & ".sig";
      end if;

      if Verbose then
         Put_Line ("Signing: " & To_String (Input_Path));
         Put_Line ("Key:     " & To_String (Key_Id_Str));
         Put_Line ("Output:  " & To_String (Output_Path));
         Put_Line ("");
      end if;

      --  Read private key from key store directory
      declare
         Home      : constant String :=
            Ada.Environment_Variables.Value ("HOME", "/tmp");
         Key_Dir   : constant String :=
            Home & "/.config/cerro-torre/keys/";
         Priv_Path : constant String :=
            Key_Dir & To_String (Key_Id_Str) & ".priv";
      begin
         if not Ada.Directories.Exists (Priv_Path) then
            Put_Line ("Error: Private key not found: " & Priv_Path);
            Put_Line ("  Generate with: ct keygen --id " &
                      To_String (Key_Id_Str));
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
            return;
         end if;

         --  Read hex-encoded private key
         declare
            Priv_File : Ada.Text_IO.File_Type;
            Priv_Hex  : String (1 .. 128);
            Last      : Natural;
            Priv_Key  : Ed25519_Private_Key;
            Sig       : Ed25519_Signature;
            OK        : Boolean;
         begin
            Ada.Text_IO.Open (Priv_File, Ada.Text_IO.In_File, Priv_Path);
            Ada.Text_IO.Get_Line (Priv_File, Priv_Hex, Last);
            Ada.Text_IO.Close (Priv_File);

            if Last /= 128 then
               Put_Line ("Error: Invalid private key format " &
                  "(expected 128 hex chars).");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            Hex_To_Private_Key (Priv_Hex, Priv_Key, OK);
            if not OK then
               Put_Line ("Error: Invalid private key hex encoding.");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            --  Read file content to sign
            declare
               In_File    : Ada.Text_IO.File_Type;
               Content    : Unbounded_String := Null_Unbounded_String;
               Line_Buf   : String (1 .. 4096);
               Line_Last  : Natural;
               First_Line : Boolean := True;
            begin
               Ada.Text_IO.Open (In_File, Ada.Text_IO.In_File,
                                 To_String (Input_Path));
               while not Ada.Text_IO.End_Of_File (In_File) loop
                  Ada.Text_IO.Get_Line (In_File, Line_Buf, Line_Last);
                  if First_Line then
                     First_Line := False;
                  else
                     Append (Content, ASCII.LF);
                  end if;
                  Append (Content, Line_Buf (1 .. Line_Last));
               end loop;
               Ada.Text_IO.Close (In_File);

               --  Sign the content
               if Verbose then
                  Put_Line ("Signing" &
                     Natural'Image (Length (Content)) & " bytes...");
               end if;

               Sign_Ed25519 (To_String (Content), Priv_Key, Sig, OK);

               if not OK then
                  Put_Line ("Error: Signing failed (OpenSSL error).");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;

               --  Write signature as hex to output file
               declare
                  Sig_Hex  : constant String := Signature_To_Hex (Sig);
                  Out_File : Ada.Text_IO.File_Type;
               begin
                  Ada.Text_IO.Create (Out_File, Ada.Text_IO.Out_File,
                                      To_String (Output_Path));
                  Ada.Text_IO.Put_Line (Out_File, Sig_Hex);
                  Ada.Text_IO.Close (Out_File);
               end;

               Put_Line ("Signature created: " &
                  To_String (Output_Path));
               if Verbose then
                  Put_Line ("  Signature: " &
                     Signature_To_Hex (Sig) (1 .. 32) & "...");
               end if;
               Set_Exit_Status (0);
            end;
         exception
            when E : others =>
               Put_Line ("Error reading key: " &
                  Ada.Exceptions.Exception_Message (E));
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
         end;
      end;
   end Run_Sign;

   ----------------
   -- Verify_Sig --
   ----------------

   procedure Run_Verify_Sig is
      use Cerro_Crypto_OpenSSL;
      use Cerro_Trust_Store;
      use Ada.Directories;

      Input_Path : Unbounded_String := Null_Unbounded_String;
      Sig_Path   : Unbounded_String := Null_Unbounded_String;
      Key_Id_Str : Unbounded_String := Null_Unbounded_String;
      Verbose    : Boolean := False;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct verify-sig <file> --sig <sig-file> " &
                   "--key <key-id>");
         Put_Line ("");
         Put_Line ("Verify a detached Ed25519 signature against a file.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --sig <file>     Signature file (default: <file>.sig)");
         Put_Line ("  --key <key-id>   Public key ID in trust store");
         Put_Line ("  -v, --verbose    Show detailed progress");
         Put_Line ("");
         Put_Line ("Exit codes:");
         Put_Line ("  0   Signature valid");
         Put_Line ("  2   Signature invalid");
         Put_Line ("  3   Key not found in trust store");
         Put_Line ("  11  I/O error");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct verify-sig bundle.ctp --key upstream-nginx");
         Put_Line ("  ct verify-sig data.bin --sig data.sig --key my-key");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      --  First positional argument is the file to verify
      Input_Path := To_Unbounded_String (Argument (2));

      --  Parse remaining arguments
      declare
         I : Positive := 3;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "--sig"
                  and then I < Argument_Count
               then
                  Sig_Path := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif (Arg = "--key" or Arg = "-k")
                  and then I < Argument_Count
               then
                  Key_Id_Str := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif Arg = "-v" or Arg = "--verbose" then
                  Verbose := True;
                  I := I + 1;
               else
                  Put_Line ("Unknown argument: " & Arg);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;
            end;
         end loop;
      end;

      --  Validate input file
      if not Ada.Directories.Exists (To_String (Input_Path)) then
         Put_Line ("Error: File not found: " & To_String (Input_Path));
         Set_Exit_Status (CT_Errors.Exit_IO_Error);
         return;
      end if;

      --  Default signature path: <input>.sig
      if Length (Sig_Path) = 0 then
         Sig_Path := Input_Path & ".sig";
      end if;

      if not Ada.Directories.Exists (To_String (Sig_Path)) then
         Put_Line ("Error: Signature file not found: " &
                   To_String (Sig_Path));
         Set_Exit_Status (CT_Errors.Exit_IO_Error);
         return;
      end if;

      --  Key ID is required for verification
      if Length (Key_Id_Str) = 0 then
         Put_Line ("Error: --key is required for verification.");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      if Verbose then
         Put_Line ("Verifying: " & To_String (Input_Path));
         Put_Line ("Signature: " & To_String (Sig_Path));
         Put_Line ("Key:       " & To_String (Key_Id_Str));
         Put_Line ("");
      end if;

      --  Look up public key from trust store
      Initialize;
      declare
         Info   : Key_Info;
         Result : Store_Result;
      begin
         Result := Get_Key (To_String (Key_Id_Str), Info);
         if Result /= OK then
            Put_Line ("Error: Key not found in trust store: " &
                      To_String (Key_Id_Str));
            Put_Line ("  Import with: ct key import <file.pub> --id " &
                      To_String (Key_Id_Str));
            Set_Exit_Status (CT_Errors.Exit_Key_Not_Trusted);
            return;
         end if;

         --  Parse public key hex from trust store
         declare
            Pub_Hex : constant String :=
               Info.Public_Key (1 .. Info.Pubkey_Len);
            Pub_Key : Ed25519_Public_Key;
            OK      : Boolean;
         begin
            if Pub_Hex'Length /= 64 then
               Put_Line ("Error: Invalid public key length " &
                  "in trust store.");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            Hex_To_Public_Key (Pub_Hex, Pub_Key, OK);
            if not OK then
               Put_Line ("Error: Invalid public key hex " &
                  "in trust store.");
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            --  Read signature hex from .sig file
            declare
               Sig_File : Ada.Text_IO.File_Type;
               Sig_Hex  : String (1 .. 128);
               Sig_Last : Natural;
               Sig      : Ed25519_Signature;
            begin
               Ada.Text_IO.Open (Sig_File, Ada.Text_IO.In_File,
                                 To_String (Sig_Path));
               Ada.Text_IO.Get_Line (Sig_File, Sig_Hex, Sig_Last);
               Ada.Text_IO.Close (Sig_File);

               if Sig_Last /= 128 then
                  Put_Line ("Error: Invalid signature format " &
                     "(expected 128 hex chars, got" &
                     Natural'Image (Sig_Last) & ").");
                  Set_Exit_Status (CT_Errors.Exit_Signature_Invalid);
                  return;
               end if;

               Hex_To_Signature (Sig_Hex, Sig, OK);
               if not OK then
                  Put_Line ("Error: Invalid signature hex.");
                  Set_Exit_Status (CT_Errors.Exit_Signature_Invalid);
                  return;
               end if;

               --  Read file content to verify
               declare
                  In_File    : Ada.Text_IO.File_Type;
                  Content    : Unbounded_String := Null_Unbounded_String;
                  Line_Buf   : String (1 .. 4096);
                  Line_Last  : Natural;
                  First_Line : Boolean := True;
                  Valid      : Boolean;
                  Verify_OK  : Boolean;
               begin
                  Ada.Text_IO.Open (In_File, Ada.Text_IO.In_File,
                                    To_String (Input_Path));
                  while not Ada.Text_IO.End_Of_File (In_File) loop
                     Ada.Text_IO.Get_Line (In_File, Line_Buf,
                                           Line_Last);
                     if First_Line then
                        First_Line := False;
                     else
                        Append (Content, ASCII.LF);
                     end if;
                     Append (Content,
                             Line_Buf (1 .. Line_Last));
                  end loop;
                  Ada.Text_IO.Close (In_File);

                  if Verbose then
                     Put_Line ("Verifying" &
                        Natural'Image (Length (Content)) &
                        " bytes...");
                  end if;

                  Verify_Ed25519 (To_String (Content), Sig,
                                  Pub_Key, Valid, Verify_OK);

                  if not Verify_OK then
                     Put_Line ("Error: Verification process failed " &
                        "(OpenSSL error).");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                     return;
                  end if;

                  if Valid then
                     Put_Line ("Signature valid.");
                     if Verbose then
                        Put_Line ("  Signer: " &
                           To_String (Key_Id_Str));
                        Put_Line ("  Fingerprint: " &
                           Info.Fingerprint (1 .. Info.Finger_Len));
                     end if;
                     Set_Exit_Status (CT_Errors.Exit_Success);
                  else
                     Put_Line ("Signature INVALID.");
                     Set_Exit_Status (CT_Errors.Exit_Signature_Invalid);
                  end if;
               end;
            exception
               when E : others =>
                  Put_Line ("Error reading signature: " &
                     Ada.Exceptions.Exception_Message (E));
                  Set_Exit_Status (CT_Errors.Exit_IO_Error);
            end;
         end;
      end;
   end Run_Verify_Sig;

   -----------
   -- Fetch --
   -----------

   procedure Run_Fetch is
      use CT_Registry;
      use Ada.Directories;

      Reference_Str : Unbounded_String := Null_Unbounded_String;
      Output_Path   : Unbounded_String := Null_Unbounded_String;
      Verbose       : Boolean := False;
      Verify_Only   : Boolean := False;

   begin
      --  Parse arguments
      if Argument_Count < 2 then
         Put_Line ("Usage: ct fetch <reference> -o <output.ctp> [options]");
         Put_Line ("");
         Put_Line ("Pull a .ctp bundle from an OCI registry.");
         Put_Line ("");
         Put_Line ("Arguments:");
         Put_Line ("  <reference>    OCI reference (registry/repo:tag or @sha256:...)");
         Put_Line ("  -o <file>      Output path for .ctp bundle (required)");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -v, --verbose     Show detailed progress");
         Put_Line ("  --verify-only     Check if bundle exists without downloading");
         Put_Line ("");
         Put_Line ("Authentication:");
         Put_Line ("  Reads from ~/.docker/config.json or CT_REGISTRY_AUTH env var");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct fetch ghcr.io/hyperpolymath/nginx:v1.0 -o nginx.ctp");
         Put_Line ("  ct fetch docker.io/library/alpine:latest -o alpine.ctp");
         Put_Line ("  ct fetch myregistry.io/app:v2@sha256:abc... -o app.ctp");
         Put_Line ("  ct fetch ghcr.io/org/image:tag --verify-only");
         Put_Line ("");
         Put_Line ("Exit codes:");
         Put_Line ("  0   Fetch succeeded");
         Put_Line ("  1   Not found or authentication failed");
         Put_Line ("  2   Network error or registry unavailable");
         Put_Line ("  3   Digest verification failed");
         Put_Line ("  10  Invalid arguments");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      Reference_Str := To_Unbounded_String (Argument (2));

      --  Parse options
      declare
         I : Positive := 3;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if (Arg = "-o" or Arg = "--output") and I < Argument_Count then
                  I := I + 1;
                  Output_Path := To_Unbounded_String (Argument (I));
               elsif Arg = "-v" or Arg = "--verbose" then
                  Verbose := True;
               elsif Arg = "--verify-only" then
                  Verify_Only := True;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate arguments
      if not Verify_Only and Length (Output_Path) = 0 then
         Put_Line ("Error: Output path required (-o <file>)");
         Set_Exit_Status (10);
         return;
      end if;

      if Verbose then
         Put_Line ("Reference: " & To_String (Reference_Str));
         if not Verify_Only then
            Put_Line ("Output: " & To_String (Output_Path));
         end if;
         Put_Line ("");
      end if;

      --  Parse reference
      declare
         Ref : constant Image_Reference := Parse_Reference (To_String (Reference_Str));
      begin
         if Length (Ref.Registry) = 0 or Length (Ref.Repository) = 0 then
            Put_Line ("Error: Invalid registry reference");
            Put_Line ("  Expected format: registry/repository:tag or @digest");
            Set_Exit_Status (10);
            return;
         end if;

         if Verbose then
            Put_Line ("Parsed reference:");
            Put_Line ("  Registry: " & To_String (Ref.Registry));
            Put_Line ("  Repository: " & To_String (Ref.Repository));

            if Length (Ref.Tag) > 0 then
               Put_Line ("  Tag: " & To_String (Ref.Tag));
            end if;

            if Length (Ref.Digest) > 0 then
               Put_Line ("  Digest: " & To_String (Ref.Digest));
            end if;

            Put_Line ("");
         end if;

         --  Step 1: Load credentials
         declare
            Auth : CT_Registry.Registry_Auth_Credentials := (others => <>);
            Username_Env : constant String := Ada.Environment_Variables.Value ("CT_REGISTRY_USER", "");
            Password_Env : constant String := Ada.Environment_Variables.Value ("CT_REGISTRY_PASS", "");
         begin
            if Username_Env'Length > 0 then
               Auth.Method := CT_Registry.Basic;
               Auth.Username := To_Unbounded_String (Username_Env);
               Auth.Password := To_Unbounded_String (Password_Env);

               if Verbose then
                  Put_Line ("Using credentials from environment");
               end if;
            else
               if Verbose then
                  Put_Line ("No credentials configured (attempting anonymous pull)");
               end if;
            end if;

            --  Step 2: Create registry client
            declare
               Client : Registry_Client := Create_Client (
                  Registry => To_String (Ref.Registry),
                  Auth     => Auth);
            begin
               if Verbose then
                  Put_Line ("Connecting to registry: " & To_String (Client.Base_URL));
               end if;

               --  Step 3: Authenticate if credentials provided
               if Auth.Method /= CT_Registry.None then
                  declare
                     Auth_Result : constant Registry_Error := Authenticate (
                        Client     => Client,
                        Repository => To_String (Ref.Repository),
                        Actions    => "pull");
                  begin
                     if Auth_Result /= CT_Registry.Success and Auth_Result /= CT_Registry.Not_Implemented then
                        Put_Line ("✗ Authentication failed: " & CT_Registry.Error_Message (Auth_Result));
                        Set_Exit_Status (1);
                        return;
                     end if;

                     if Verbose and Auth_Result = CT_Registry.Success then
                        Put_Line ("✓ Authenticated");
                     end if;
                  end;
               end if;

               --  Step 4: Resolve reference (tag to digest if needed)
               declare
                  Reference_To_Pull : constant String :=
                     (if Length (Ref.Digest) > 0
                      then To_String (Ref.Digest)
                      else To_String (Ref.Tag));
               begin
                  --  Step 5: Check if manifest exists (for --verify-only)
                  if Verify_Only then
                     if Manifest_Exists (Client, To_String (Ref.Repository), Reference_To_Pull) then
                        Put_Line ("✓ Manifest exists: " & To_String (Reference_Str));
                        Set_Exit_Status (0);
                     else
                        Put_Line ("✗ Manifest not found: " & To_String (Reference_Str));
                        Set_Exit_Status (1);
                     end if;
                     return;
                  end if;

                  --  Step 6: Pull manifest
                  declare
                     Pull_Res : Pull_Result;
                  begin
                     if Verbose then
                        Put_Line ("Pulling manifest...");
                     end if;

                     Pull_Res := Pull_Manifest (
                        Client     => Client,
                        Repository => To_String (Ref.Repository),
                        Reference  => Reference_To_Pull);

                     if Pull_Res.Error /= CT_Registry.Success then
                        Put_Line ("✗ Pull failed: " & Error_Message (Pull_Res.Error));

                        case Pull_Res.Error is
                           when Not_Found =>
                              Set_Exit_Status (1);
                           when Auth_Failed | Auth_Required | Forbidden =>
                              Set_Exit_Status (1);
                           when Network_Error | Timeout | Server_Error =>
                              Set_Exit_Status (2);
                           when Digest_Mismatch =>
                              Set_Exit_Status (3);
                           when others =>
                              Set_Exit_Status (CT_Errors.Exit_General_Failure);
                        end case;
                        return;
                     end if;

                     if Verbose then
                        Put_Line ("✓ Manifest retrieved");
                        Put_Line ("  Digest: " & To_String (Pull_Res.Digest));
                        Put_Line ("  Layers: " & Natural'Image (Natural (Pull_Res.Manifest.Layers.Length)));
                     end if;

                     --  Step 7: Save manifest to file (simple .ctp format for MVP)
                     --  For now, save raw JSON manifest. Future: full OCI layout with blobs
                     declare
                        use Ada.Text_IO;
                        Output_File : File_Type;
                        Output_Str  : constant String := To_String (Output_Path);
                     begin
                        Create (Output_File, Out_File, Output_Str);
                        Put_Line (Output_File, "# Cerro Torre Bundle v0.2");
                        Put_Line (Output_File, "# Manifest digest: " & To_String (Pull_Res.Digest));
                        Put_Line (Output_File, "# Registry: " & To_String (Ref.Registry));
                        Put_Line (Output_File, "# Repository: " & To_String (Ref.Repository));
                        Put_Line (Output_File, "# Reference: " & Reference_To_Pull);
                        Put_Line (Output_File, "");
                        Put_Line (Output_File, "# Manifest JSON:");
                        Put_Line (Output_File, To_String (Pull_Res.Raw_Json));
                        Close (Output_File);

                        Put_Line ("✓ Fetched to " & Output_Str);
                        Put_Line ("  Manifest digest: " & To_String (Pull_Res.Digest));

                        if Verbose then
                           declare
                              use Ada.Directories;
                              Size_Bytes : constant File_Size := Size (Output_Str);
                           begin
                              Put_Line ("  Size: " & File_Size'Image (Size_Bytes) & " bytes");
                           end;
                           Put_Line ("");
                           Put_Line ("Note: This is a manifest-only bundle (MVP).");
                           Put_Line ("Full implementation will include:");
                           Put_Line ("  - All container layers (blobs)");
                           Put_Line ("  - OCI layout directory structure");
                           Put_Line ("  - Attestations and signatures");
                        end if;

                        Set_Exit_Status (0);
                     exception
                        when Error : others =>
                           Put_Line ("✗ Failed to write bundle: " & Ada.Exceptions.Exception_Message (Error));
                           Set_Exit_Status (CT_Errors.Exit_General_Failure);
                     end;
                  end;
               end;
            end;
         end;
      end;
   end Run_Fetch;

   ----------
   -- Push --
   ----------

   procedure Run_Push is
      use CT_Registry;
      use Ada.Directories;

      Bundle_Path : Unbounded_String := Null_Unbounded_String;
      Destination : Unbounded_String := Null_Unbounded_String;
      Verbose     : Boolean := False;
      Force       : Boolean := False;

   begin
      --  Parse arguments
      if Argument_Count < 3 then
         Put_Line ("Usage: ct push <bundle.ctp> <destination> [options]");
         Put_Line ("");
         Put_Line ("Publish a .ctp bundle to an OCI registry.");
         Put_Line ("");
         Put_Line ("Arguments:");
         Put_Line ("  <bundle.ctp>     Path to .ctp bundle file");
         Put_Line ("  <destination>    Registry reference (registry/repo:tag)");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -v, --verbose    Show detailed progress");
         Put_Line ("  -f, --force      Overwrite existing tag");
         Put_Line ("");
         Put_Line ("Authentication:");
         Put_Line ("  Reads from ~/.docker/config.json or CT_REGISTRY_AUTH env var");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct push nginx.ctp ghcr.io/hyperpolymath/nginx:v1.0");
         Put_Line ("  ct push hello.ctp docker.io/myuser/hello:latest");
         Put_Line ("  ct push app.ctp myregistry.io/apps/myapp:v2.1 -v");
         Put_Line ("");
         Put_Line ("Exit codes:");
         Put_Line ("  0   Push succeeded");
         Put_Line ("  1   Authentication failed");
         Put_Line ("  2   Network error or registry unavailable");
         Put_Line ("  3   Bundle not found or malformed");
         Put_Line ("  10  Invalid arguments");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      Bundle_Path := To_Unbounded_String (Argument (2));
      Destination := To_Unbounded_String (Argument (3));

      --  Parse optional flags
      declare
         I : Positive := 4;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Arg = "-v" or Arg = "--verbose" then
                  Verbose := True;
               elsif Arg = "-f" or Arg = "--force" then
                  Force := True;
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate bundle exists
      declare
         Bundle_Str : constant String := To_String (Bundle_Path);
      begin
         if not Ada.Directories.Exists (Bundle_Str) then
            Put_Line ("Error: Bundle not found: " & Bundle_Str);
            Set_Exit_Status (3);
            return;
         end if;

         if Verbose then
            Put_Line ("Bundle: " & Bundle_Str);
            Put_Line ("Destination: " & To_String (Destination));
            Put_Line ("");
         end if;
      end;

      --  Parse destination reference
      declare
         Ref : constant Image_Reference := Parse_Reference (To_String (Destination));
      begin
         if Length (Ref.Registry) = 0 or Length (Ref.Repository) = 0 then
            Put_Line ("Error: Invalid destination reference");
            Put_Line ("  Expected format: registry/repository:tag");
            Set_Exit_Status (10);
            return;
         end if;

         if Verbose then
            Put_Line ("Parsed destination:");
            Put_Line ("  Registry: " & To_String (Ref.Registry));
            Put_Line ("  Repository: " & To_String (Ref.Repository));
            Put_Line ("  Tag: " & To_String (Ref.Tag));
            Put_Line ("");
         end if;

         --  Step 1: Load credentials
         --  TODO: Read from ~/.docker/config.json or CT_REGISTRY_AUTH
         declare
            Auth : CT_Registry.Registry_Auth_Credentials := (others => <>);
            Username_Env : constant String := Ada.Environment_Variables.Value ("CT_REGISTRY_USER", "");
            Password_Env : constant String := Ada.Environment_Variables.Value ("CT_REGISTRY_PASS", "");
         begin
            if Username_Env'Length > 0 then
               Auth.Method := CT_Registry.Basic;
               Auth.Username := To_Unbounded_String (Username_Env);
               Auth.Password := To_Unbounded_String (Password_Env);

               if Verbose then
                  Put_Line ("Using credentials from environment");
               end if;
            else
               if Verbose then
                  Put_Line ("No credentials configured (attempting anonymous push)");
               end if;
            end if;

            --  Step 2: Create registry client
            declare
               Client : Registry_Client := Create_Client (
                  Registry => To_String (Ref.Registry),
                  Auth     => Auth);
            begin
               if Verbose then
                  Put_Line ("Connecting to registry: " & To_String (Client.Base_URL));
               end if;

               --  Step 3: Authenticate if credentials provided
               if Auth.Method /= CT_Registry.None then
                  declare
                     Auth_Result : constant Registry_Error := Authenticate (
                        Client     => Client,
                        Repository => To_String (Ref.Repository),
                        Actions    => "push");
                  begin
                     if Auth_Result /= CT_Registry.Success and Auth_Result /= CT_Registry.Not_Implemented then
                        Put_Line ("✗ Authentication failed: " & CT_Registry.Error_Message (Auth_Result));
                        Set_Exit_Status (1);
                        return;
                     end if;

                     if Verbose and Auth_Result = CT_Registry.Success then
                        Put_Line ("✓ Authenticated");
                     end if;
                  end;
               end if;

               --  Step 4: Check if tag already exists (unless --force)
               if not Force then
                  if Manifest_Exists (Client, To_String (Ref.Repository), To_String (Ref.Tag)) then
                     Put_Line ("Error: Tag already exists: " & To_String (Ref.Tag));
                     Put_Line ("  Use --force to overwrite");
                     Set_Exit_Status (1);
                     return;
                  end if;
               end if;

               --  Step 5: Read manifest from bundle
               --  For MVP, .ctp is just a text file with manifest JSON
               --  Full implementation will extract from tarball
               declare
                  use Ada.Text_IO;
                  Bundle_File : File_Type;
                  Manifest_JSON : Unbounded_String := Null_Unbounded_String;
                  In_Manifest : Boolean := False;
               begin
                  if Verbose then
                     Put_Line ("Reading bundle manifest...");
                  end if;

                  Open (Bundle_File, In_File, To_String (Bundle_Path));

                  --  Parse bundle file: skip header comments, extract JSON
                  while not End_Of_File (Bundle_File) loop
                     declare
                        Line : constant String := Get_Line (Bundle_File);
                     begin
                        --  Start of manifest JSON section
                        if Line = "# Manifest JSON:" then
                           In_Manifest := True;
                        elsif In_Manifest and then Line'Length > 0 and then Line (Line'First) /= '#' then
                           --  Accumulate JSON lines
                           Append (Manifest_JSON, Line);
                           Append (Manifest_JSON, ASCII.LF);
                        end if;
                     end;
                  end loop;

                  Close (Bundle_File);

                  if Length (Manifest_JSON) = 0 then
                     Put_Line ("✗ Error: No manifest found in bundle");
                     Set_Exit_Status (3);
                     return;
                  end if;

                  if Verbose then
                     Put_Line ("✓ Manifest extracted (" &
                               Natural'Image (Length (Manifest_JSON)) & " bytes)");
                  end if;

                  --  Step 6: Push manifest to registry
                  declare
                     Manifest : OCI_Manifest;  --  Minimal record (JSON passed separately)
                     Push_Res : Push_Result;
                  begin
                     --  For MVP: use raw JSON (full parsing not yet implemented)
                     Manifest.Schema_Version := 2;
                     Manifest.Media_Type := To_Unbounded_String (OCI_Manifest_V1);

                     if Verbose then
                        Put_Line ("Pushing manifest to registry...");
                     end if;

                     Push_Res := Push_Manifest (
                        Client        => Client,
                        Repository    => To_String (Ref.Repository),
                        Tag           => To_String (Ref.Tag),
                        Manifest      => Manifest,
                        Manifest_Json => To_String (Manifest_JSON));

                     if Push_Res.Error /= CT_Registry.Success then
                        Put_Line ("✗ Push failed: " & Error_Message (Push_Res.Error));

                        case Push_Res.Error is
                           when Auth_Failed | Auth_Required | Forbidden =>
                              Set_Exit_Status (1);
                           when Network_Error | Timeout | Server_Error =>
                              Set_Exit_Status (2);
                           when others =>
                              Set_Exit_Status (CT_Errors.Exit_General_Failure);
                        end case;
                        return;
                     else
                        Put_Line ("✓ Pushed to " & To_String (Destination));
                        Put_Line ("  Digest: " & To_String (Push_Res.Digest));

                        if Verbose then
                           Put_Line ("  URL: " & To_String (Push_Res.URL));
                           Put_Line ("");
                           Put_Line ("Note: This is an MVP implementation.");
                           Put_Line ("  - Blobs not yet pushed (manifest-only)");
                           Put_Line ("  - Full OCI layer support pending");
                           Put_Line ("  - See IMPLEMENTATION-STATUS.md for roadmap");
                        end if;

                        Set_Exit_Status (0);
                     end if;
                  end;

               exception
                  when Error : others =>
                     Put_Line ("✗ Failed to read bundle: " &
                               Ada.Exceptions.Exception_Message (Error));
                     Set_Exit_Status (3);
                     return;
               end;
            end;
         end;
      end;
   end Run_Push;

   ------------
   -- Import --
   ------------

   procedure Run_Import is
   begin
      Put_Line ("Usage: ct import <archive> [--verify]");
      Put_Line ("");
      Put_Line ("Import from offline archive.");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --verify            Verify each bundle after import");
      Put_Line ("  --policy <file>     Policy for verification");
      Put_Line ("  --keys-only         Only import keys, not bundles");
      Put_Line ("  --output-dir <dir>  Where to place imported bundles");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Import;

   ---------
   -- Log --
   ---------

   procedure Run_Log is
      use CT_Transparency;
      use Ada.Directories;

   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct log <subcommand> [args]");
         Put_Line ("");
         Put_Line ("Transparency log operations:");
         Put_Line ("  submit <bundle.ctp>       Upload attestations to transparency logs");
         Put_Line ("  verify <bundle.ctp>       Verify log inclusion proofs");
         Put_Line ("  search --hash <digest>    Find entries by artifact hash");
         Put_Line ("  search --pubkey <file>    Find entries by public key");
         Put_Line ("  info                      Show log tree status");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct log submit nginx.ctp");
         Put_Line ("  ct log verify nginx.ctp");
         Put_Line ("  ct log search --hash sha256:abc123...");
         Put_Line ("  ct log info");
         Put_Line ("");
         Put_Line ("Transparency logs:");
         Put_Line ("  Sigstore Rekor (default)  Public log at rekor.sigstore.dev");
         Put_Line ("  CT-TLOG (future)          Cerro Torre native log");
         Put_Line ("");
         Put_Line ("Exit codes:");
         Put_Line ("  0   Operation succeeded");
         Put_Line ("  1   Entry not found or verification failed");
         Put_Line ("  2   Network error");
         Put_Line ("  10  Invalid arguments");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Subcommand : constant String := Argument (2);
      begin
         --  SUBMIT
         if Subcommand = "submit" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct log submit <bundle.ctp> [--logs <count>]");
               Put_Line ("");
               Put_Line ("Upload bundle attestations to transparency logs.");
               Put_Line ("");
               Put_Line ("Options:");
               Put_Line ("  --logs <N>    Submit to N logs (default: 2, quorum)");
               Put_Line ("  --verbose     Show detailed progress");
               Put_Line ("");
               Put_Line ("Federated logs (default: submit to both):");
               Put_Line ("  - Sigstore Rekor (rekor.sigstore.dev)");
               Put_Line ("  - CT-TLOG (future: tlog.cerro-torre.dev)");
               Set_Exit_Status (10);
               return;
            end if;

            declare
               Bundle_Path : constant String := Argument (3);
               Verbose     : Boolean := False;
               Min_Logs    : Positive := 2;
            begin
               --  Parse options
               for I in 4 .. Argument_Count loop
                  declare
                     Arg : constant String := Argument (I);
                  begin
                     if Arg = "-v" or Arg = "--verbose" then
                        Verbose := True;
                     elsif Arg = "--logs" and I < Argument_Count then
                        Min_Logs := Positive'Value (Argument (I + 1));
                     end if;
                  end;
               end loop;

               if not Ada.Directories.Exists (Bundle_Path) then
                  Put_Line ("Error: Bundle not found: " & Bundle_Path);
                  Set_Exit_Status (10);
                  return;
               end if;

               if Verbose then
                  Put_Line ("Submitting attestations from: " & Bundle_Path);
                  Put_Line ("  Minimum logs: " & Natural'Image (Min_Logs));
                  Put_Line ("");
               end if;

               --  TODO: Extract attestations from bundle
               --  TODO: Submit to each log
               declare
                  Client : constant Log_Client := Create_Client (Sigstore_Rekor);
                  Upload_Res : Upload_Result;
               begin
                  --  Placeholder: Upload dummy entry
                  Upload_Res := Upload_Signature (
                     Client     => Client,
                     Signature  => "placeholder",
                     Artifact   => Bundle_Path,
                     Public_Key => "placeholder");

                  if Upload_Res.Error = Not_Implemented then
                     Put_Line ("");
                     Put_Line ("Log submission prepared but not yet implemented.");
                     Put_Line ("");
                     Put_Line ("Implementation roadmap:");
                     Put_Line ("  1. HTTP client integration for Rekor API");
                     Put_Line ("  2. Extract signatures from .ctp attestations");
                     Put_Line ("  3. Submit hashedrekord entries to Rekor");
                     Put_Line ("  4. Store log proofs in bundle");
                     Put_Line ("  5. Support federated logs (2+ log quorum)");
                     Put_Line ("");
                     Put_Line ("When implemented, this will:");
                     Put_Line ("  ✓ Submit attestations to Sigstore Rekor");
                     Put_Line ("  ✓ Submit to CT-TLOG (when available)");
                     Put_Line ("  ✓ Require majority agreement (quorum)");
                     Put_Line ("  ✓ Store inclusion proofs in bundle");
                     Put_Line ("  ✓ Generate offline verification bundles");
                     Put_Line ("");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  elsif Upload_Res.Error /= CT_Transparency.Success then
                     Put_Line ("✗ Upload failed: " & CT_Transparency.Error_Message (Upload_Res.Error));
                     Set_Exit_Status (case Upload_Res.Error is
                        when Network_Error => 2,
                        when others => 1);
                  else
                     Put_Line ("✓ Submitted to transparency log");
                     Put_Line ("  UUID: " & String (Upload_Res.The_Entry.UUID));
                     Put_Line ("  View: " & To_String (Upload_Res.URL));
                     Set_Exit_Status (0);
                  end if;
               end;
            end;

         --  VERIFY
         elsif Subcommand = "verify" then
            if Argument_Count < 3 then
               Put_Line ("Usage: ct log verify <bundle.ctp>");
               Put_Line ("");
               Put_Line ("Verify transparency log inclusion proofs.");
               Set_Exit_Status (10);
               return;
            end if;

            declare
               Bundle_Path : constant String := Argument (3);
            begin
               if not Ada.Directories.Exists (Bundle_Path) then
                  Put_Line ("Error: Bundle not found: " & Bundle_Path);
                  Set_Exit_Status (10);
                  return;
               end if;

               Put_Line ("Verifying log proofs in: " & Bundle_Path);
               Put_Line ("");

               --  TODO: Extract log proofs from bundle
               --  TODO: Verify each proof
               declare
                  Client : constant Log_Client := Create_Client (Sigstore_Rekor);
                  Dummy_Entry : Log_Entry;
                  Verify_Res : Verify_Result;
               begin
                  Dummy_Entry.UUID := (others => '0');

                  Verify_Res := Verify_Entry (Client, Dummy_Entry);

                  if Verify_Res.Error = Not_Implemented then
                     Put_Line ("Log verification prepared but not yet implemented.");
                     Put_Line ("");
                     Put_Line ("When implemented, this will:");
                     Put_Line ("  ✓ Verify Merkle inclusion proofs");
                     Put_Line ("  ✓ Verify Signed Entry Timestamps (SET)");
                     Put_Line ("  ✓ Check artifact hash matches");
                     Put_Line ("  ✓ Verify quorum (2+ logs agree)");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  elsif Verify_Res.Error /= CT_Transparency.Success then
                     Put_Line ("✗ Verification failed: " & CT_Transparency.Error_Message (Verify_Res.Error));
                     Set_Exit_Status (1);
                  else
                     Put_Line ("✓ All log proofs verified");
                     Set_Exit_Status (0);
                  end if;
               end;
            end;

         --  SEARCH
         elsif Subcommand = "search" then
            if Argument_Count < 4 then
               Put_Line ("Usage: ct log search --hash <digest> | --pubkey <file>");
               Put_Line ("");
               Put_Line ("Search transparency log for entries.");
               Put_Line ("");
               Put_Line ("Options:");
               Put_Line ("  --hash <digest>      Search by artifact SHA256 hash");
               Put_Line ("  --pubkey <file>      Search by public key");
               Put_Line ("  --email <address>    Search by identity email");
               Set_Exit_Status (10);
               return;
            end if;

            declare
               Search_Type : constant String := Argument (3);
               Search_Val  : constant String := Argument (4);
            begin
               declare
                  Client : constant Log_Client := Create_Client (Sigstore_Rekor);
                  Search_Res : Lookup_Result;
               begin
                  if Search_Type = "--hash" then
                     Search_Res := Search_By_Hash (Client, Search_Val);
                  elsif Search_Type = "--pubkey" then
                     Search_Res := Search_By_Public_Key (Client, Search_Val);
                  elsif Search_Type = "--email" then
                     Search_Res := Search_By_Email (Client, Search_Val);
                  else
                     Put_Line ("Error: Unknown search type: " & Search_Type);
                     Set_Exit_Status (10);
                     return;
                  end if;

                  if Search_Res.Error = CT_Transparency.Not_Implemented then
                     Put_Line ("Log search prepared but not yet implemented.");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  elsif Search_Res.Error /= CT_Transparency.Success then
                     Put_Line ("✗ Search failed: " & CT_Transparency.Error_Message (Search_Res.Error));
                     Set_Exit_Status (2);
                  else
                     Put_Line ("Found " & Natural'Image (Natural (Search_Res.Entries.Length)) & " entries:");
                     for Log_Entry of Search_Res.Entries loop
                        Put_Line ("  UUID: " & String (Log_Entry.UUID));
                        Put_Line ("    Index: " & Interfaces.Unsigned_64'Image (Log_Entry.Log_Index));
                     end loop;
                     Set_Exit_Status (0);
                  end if;
               end;
            end;

         --  INFO
         elsif Subcommand = "info" then
            declare
               Client : constant Log_Client := Create_Client (Sigstore_Rekor);
               Info   : constant Tree_Info := Get_Log_Info (Client);
            begin
               Put_Line ("Transparency Log: " & To_String (Client.Base_URL));
               Put_Line ("  Tree size: " & Interfaces.Unsigned_64'Image (Info.Tree_Size));
               if Length (Info.Root_Hash) > 0 then
                  Put_Line ("  Root hash: " & To_String (Info.Root_Hash));
               end if;
               Set_Exit_Status (0);
            end;

         else
            Put_Line ("Unknown subcommand: " & Subcommand);
            Put_Line ("Run 'ct log' for usage.");
            Set_Exit_Status (10);
         end if;
      end;
   end Run_Log;

   ------------
   -- Export --
   ------------

   procedure Run_Export is
   begin
      Put_Line ("Usage: ct export <bundles...> -o <archive>");
      Put_Line ("");
      Put_Line ("Export bundles for offline transfer.");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  -o, --output <file>    Output archive path");
      Put_Line ("  --manifest <file>      File listing bundles to export");
      Put_Line ("  --include-keys         Include public keys for verification");
      Put_Line ("  --format <fmt>         Archive format: tar, tar.gz, tar.zst");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Export;

   ---------
   -- Run --
   ---------

   procedure Run_Run is
      package Dir renames Ada.Directories;

      Bundle_Path   : Unbounded_String := Null_Unbounded_String;
      Runtime_Name  : Unbounded_String := Null_Unbounded_String;
      Skip_Verify   : Boolean := False;
      Detach        : Boolean := False;
      Extra_Args    : Unbounded_String := Null_Unbounded_String;
      Ports         : Unbounded_String := Null_Unbounded_String;
      Volumes       : Unbounded_String := Null_Unbounded_String;
      Found_Separator : Boolean := False;  --  Track if we found "--"

      procedure Show_Help is
      begin
         Put_Line ("Usage: ct run <bundle.ctp> [--runtime=<name>] [-- <args>]");
         Put_Line ("");
         Put_Line ("Run a verified bundle via configured runtime.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -h, --help         Show this help");
         Put_Line ("  --runtime=<name>   Runtime to use (default: auto-detect)");
         Put_Line ("  --no-verify        Skip verification before run");
         Put_Line ("  -d, --detach       Run container in background");
         Put_Line ("  -p <ports>         Port mapping (e.g., 8080:80)");
         Put_Line ("  -v <vol>           Volume mount (e.g., /host:/container)");
         Put_Line ("  -- <args>          Pass remaining args to container");
         Put_Line ("");
         Put_Line ("Runtimes (FOSS-first preference):");
         Put_Line ("  svalinn            Svalinn (recommended, formally verified)");
         Put_Line ("  podman             Podman (rootless)");
         Put_Line ("  nerdctl            containerd/nerdctl");
         Put_Line ("  docker             Docker (fallback)");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct run nginx.ctp");
         Put_Line ("  ct run nginx.ctp --runtime=podman");
         Put_Line ("  ct run nginx.ctp -p 8080:80 -d");
         Put_Line ("  ct run myapp.ctp -- --config /etc/app.conf");
      end Show_Help;

   begin
      --  Show help if no arguments or --help/-h specified
      if Argument_Count < 2 or else
         Argument (2) = "--help" or else
         Argument (2) = "-h"
      then
         Show_Help;
         Set_Exit_Status (CT_Errors.Exit_Success);
         return;
      end if;

      --  Parse arguments
      declare
         I : Positive := 2;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if Found_Separator then
                  --  Everything after "--" goes to container
                  if Length (Extra_Args) > 0 then
                     Append (Extra_Args, " ");
                  end if;
                  Append (Extra_Args, Arg);

               elsif Arg = "--" then
                  Found_Separator := True;

               elsif Arg'Length > 10 and then
                     Arg (Arg'First .. Arg'First + 9) = "--runtime="
               then
                  Runtime_Name := To_Unbounded_String (Arg (Arg'First + 10 .. Arg'Last));

               elsif Arg = "--no-verify" then
                  Skip_Verify := True;

               elsif Arg = "-d" or Arg = "--detach" then
                  Detach := True;

               elsif Arg = "-p" and I < Argument_Count then
                  I := I + 1;
                  Ports := To_Unbounded_String (Argument (I));

               elsif Arg = "-v" and I < Argument_Count then
                  I := I + 1;
                  Volumes := To_Unbounded_String (Argument (I));

               elsif Arg (Arg'First) /= '-' and Length (Bundle_Path) = 0 then
                  Bundle_Path := To_Unbounded_String (Arg);
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate bundle path
      if Length (Bundle_Path) = 0 then
         Put_Line ("Error: No bundle path specified");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Str : constant String := To_String (Bundle_Path);
      begin
         if not Dir.Exists (Bundle_Str) then
            Put_Line ("Error: Bundle not found: " & Bundle_Str);
            Set_Exit_Status (CT_Errors.Exit_IO_Error);
            return;
         end if;

         Put_Line ("Running bundle: " & Bundle_Str);

         --  Step 1: Verify bundle (unless --no-verify)
         if not Skip_Verify then
            Put_Line ("  Verifying bundle...");
            declare
               Verify_Opts : Cerro_Verify.Verify_Options := (
                  Bundle_Path => Bundle_Path,
                  Policy_Path => Null_Unbounded_String,
                  Offline     => False,
                  Verbose     => False
               );
               Verify_Result : constant Cerro_Verify.Verify_Result :=
                  Cerro_Verify.Verify_Bundle (Verify_Opts);
            begin
               if not (Verify_Result.Code = Cerro_Verify.OK) then
                  Put_Line ("  Verification failed: " & To_String (Verify_Result.Details));
                  Set_Exit_Status (Ada.Command_Line.Exit_Status (
                     Cerro_Verify.To_Exit_Code (Verify_Result.Code)));
                  return;
               end if;
               Put_Line ("  ✓ Bundle verified: " &
                  To_String (Verify_Result.Package_Name) & " " &
                  To_String (Verify_Result.Package_Ver));
            end;
         else
            Put_Line ("  Skipping verification (--no-verify)");
         end if;

         --  Step 2: Detect runtime
         Cerro_Runtime.Detect_Runtimes;

         declare
            Selected_Runtime : Cerro_Runtime.Runtime_Kind;
         begin
            if Length (Runtime_Name) > 0 then
               Selected_Runtime := Cerro_Runtime.Parse_Runtime_Name (To_String (Runtime_Name));
               if not Cerro_Runtime.Is_Available (Selected_Runtime) then
                  Put_Line ("  Warning: " & To_String (Runtime_Name) &
                     " not available, falling back to preferred runtime");
                  Selected_Runtime := Cerro_Runtime.Get_Preferred_Runtime;
               end if;
            else
               Selected_Runtime := Cerro_Runtime.Get_Preferred_Runtime;
            end if;

            declare
               Runtime_Info : constant Cerro_Runtime.Runtime_Info :=
                  Cerro_Runtime.Get_Runtime_Info (Selected_Runtime);
            begin
               if not Runtime_Info.Available then
                  Put_Line ("Error: No container runtime available");
                  Put_Line ("  Please install one of: svalinn, podman, nerdctl, docker");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;

               Put_Line ("  Using runtime: " &
                  Cerro_Runtime.Runtime_Command (Selected_Runtime) &
                  " (" & To_String (Runtime_Info.Version) & ")");

               --  Step 3: Load and run
               --  For now, we'll use the bundle path directly as image reference
               --  In the future, we'll unpack to OCI and load properly
               Put_Line ("  Loading image...");

               declare
                  Load_Result : constant Cerro_Runtime.Run_Result :=
                     Cerro_Runtime.Load_Image (
                        Kind    => Selected_Runtime,
                        Tarball => Bundle_Str);
               begin
                  if not Load_Result.Success then
                     --  If load fails, try running directly (some runtimes support this)
                     Put_Line ("  Note: Direct load not supported, using image reference");
                  end if;
               end;

               Put_Line ("  Starting container...");

               declare
                  Run_Opts : Cerro_Runtime.Run_Options := (
                     Runtime     => Selected_Runtime,
                     Image_Path  => Bundle_Path,  --  Will need to map to loaded image
                     Detach      => Detach,
                     Ports       => Ports,
                     Volumes     => Volumes,
                     Environment => Null_Unbounded_String,
                     Extra_Args  => Extra_Args
                  );
                  Run_Result : constant Cerro_Runtime.Run_Result :=
                     Cerro_Runtime.Run_Container (Run_Opts);
               begin
                  if Run_Result.Success then
                     Put_Line ("  ✓ Container started");
                     if Length (Run_Result.Container_ID) > 0 then
                        Put_Line ("  Container ID: " & To_String (Run_Result.Container_ID));
                     end if;
                     Set_Exit_Status (CT_Errors.Exit_Success);
                  else
                     Put_Line ("  Error: " & To_String (Run_Result.Error_Message));
                     Set_Exit_Status (Ada.Command_Line.Exit_Status (Run_Result.Exit_Code));
                  end if;
               end;
            end;
         end;
      end;
   end Run_Run;

   ------------
   -- Unpack --
   ------------

   procedure Run_Unpack is
      package Dir renames Ada.Directories;

      type Output_Format is (Format_OCI, Format_Docker);

      Bundle_Path          : Unbounded_String := Null_Unbounded_String;
      Output_Path          : Unbounded_String := Null_Unbounded_String;
      Format               : Output_Format := Format_OCI;
      Include_Attestations : Boolean := False;

      procedure Show_Help is
      begin
         Put_Line ("Usage: ct unpack <bundle.ctp> -o <dir> [--format=oci|docker]");
         Put_Line ("");
         Put_Line ("Extract bundle to OCI layout on disk.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -h, --help           Show this help");
         Put_Line ("  -o, --output <dir>   Output directory (required)");
         Put_Line ("  --format=oci         OCI image layout (default)");
         Put_Line ("  --format=docker      Docker save format");
         Put_Line ("  --include-attestations  Copy attestations alongside");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct unpack nginx.ctp -o ./nginx-oci/");
         Put_Line ("  ct unpack nginx.ctp -o nginx.tar --format=docker");
         Put_Line ("");
         Put_Line ("Use with:");
         Put_Line ("  podman load < nginx.tar");
         Put_Line ("  nerdctl load < nginx.tar");
      end Show_Help;

   begin
      --  Show help if no arguments or --help/-h specified
      if Argument_Count < 2 or else
         Argument (2) = "--help" or else
         Argument (2) = "-h"
      then
         Show_Help;
         Set_Exit_Status (CT_Errors.Exit_Success);
         return;
      end if;

      --  Parse arguments
      declare
         I : Positive := 2;
      begin
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if (Arg = "-o" or Arg = "--output") and I < Argument_Count then
                  I := I + 1;
                  Output_Path := To_Unbounded_String (Argument (I));

               elsif Arg'Length > 9 and then
                     Arg (Arg'First .. Arg'First + 8) = "--format="
               then
                  declare
                     Format_Str : constant String := Arg (Arg'First + 9 .. Arg'Last);
                  begin
                     if Format_Str = "oci" then
                        Format := Format_OCI;
                     elsif Format_Str = "docker" then
                        Format := Format_Docker;
                     else
                        Put_Line ("Error: Unknown format: " & Format_Str);
                        Put_Line ("  Supported formats: oci, docker");
                        Set_Exit_Status (CT_Errors.Exit_General_Failure);
                        return;
                     end if;
                  end;

               elsif Arg = "--include-attestations" then
                  Include_Attestations := True;

               elsif Arg (Arg'First) /= '-' and Length (Bundle_Path) = 0 then
                  Bundle_Path := To_Unbounded_String (Arg);
               end if;
            end;
            I := I + 1;
         end loop;
      end;

      --  Validate arguments
      if Length (Bundle_Path) = 0 then
         Put_Line ("Error: No bundle path specified");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      if Length (Output_Path) = 0 then
         Put_Line ("Error: Output path required (-o <dir>)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Str : constant String := To_String (Bundle_Path);
         Output_Str : constant String := To_String (Output_Path);
      begin
         if not Dir.Exists (Bundle_Str) then
            Put_Line ("Error: Bundle not found: " & Bundle_Str);
            Set_Exit_Status (CT_Errors.Exit_IO_Error);
            return;
         end if;

         Put_Line ("Unpacking bundle: " & Bundle_Str);
         Put_Line ("  Output: " & Output_Str);
         Put_Line ("  Format: " & (if Format = Format_OCI then "OCI" else "Docker"));

         --  Step 1: Verify bundle first
         Put_Line ("  Verifying bundle...");
         declare
            Verify_Opts : Cerro_Verify.Verify_Options := (
               Bundle_Path => Bundle_Path,
               Policy_Path => Null_Unbounded_String,
               Offline     => False,
               Verbose     => False
            );
            Verify_Result : constant Cerro_Verify.Verify_Result :=
               Cerro_Verify.Verify_Bundle (Verify_Opts);
         begin
            if not (Verify_Result.Code = Cerro_Verify.OK) then
               Put_Line ("  Verification failed: " & To_String (Verify_Result.Details));
               Set_Exit_Status (Ada.Command_Line.Exit_Status (
                  Cerro_Verify.To_Exit_Code (Verify_Result.Code)));
               return;
            end if;
            Put_Line ("  ✓ Bundle verified");
         end;

         --  Step 2: Create output directory (for OCI layout)
         if Format = Format_OCI then
            if not Dir.Exists (Output_Str) then
               Dir.Create_Path (Output_Str);
            end if;
         end if;

         --  Step 3: Extract bundle contents
         Put_Line ("  Extracting contents...");

         --  For now, use system tar command to extract
         --  Future: use Cerro_Tar for pure Ada extraction
         declare
            use GNAT.OS_Lib;
            Args    : Argument_List (1 .. 4);
            Success : Boolean;
         begin
            if Format = Format_OCI then
               --  Extract to directory
               Args (1) := new String'("-xf");
               Args (2) := new String'(Bundle_Str);
               Args (3) := new String'("-C");
               Args (4) := new String'(Output_Str);

               Spawn ("/usr/bin/tar", Args, Success);

               Free (Args (1));
               Free (Args (2));
               Free (Args (3));
               Free (Args (4));

               if Success then
                  Put_Line ("  ✓ Extracted to " & Output_Str);

                  --  Create OCI layout marker files if needed
                  declare
                     Layout_File : Ada.Text_IO.File_Type;
                     Index_Path  : constant String := Output_Str & "/oci-layout";
                  begin
                     if not Dir.Exists (Index_Path) then
                        Ada.Text_IO.Create (Layout_File, Ada.Text_IO.Out_File, Index_Path);
                        Ada.Text_IO.Put_Line (Layout_File, "{""imageLayoutVersion"": ""1.0.0""}");
                        Ada.Text_IO.Close (Layout_File);
                        Put_Line ("  Created oci-layout marker");
                     end if;
                  exception
                     when others =>
                        null;  --  Non-fatal if we can't create marker
                  end;

                  if Include_Attestations then
                     Put_Line ("  Attestations included in output");
                  end if;

                  Set_Exit_Status (CT_Errors.Exit_Success);
               else
                  Put_Line ("  Error: Extraction failed");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;

            else
               --  For Docker format, the .ctp is already a tar, we just verify and copy
               --  In production, we'd reformat to Docker-save compatible format
               Put_Line ("  Note: Docker format export requires conversion");
               Put_Line ("  For now, using direct tar copy");

               declare
                  Args2 : Argument_List (1 .. 2);
               begin
                  --  Just copy the bundle for now
                  Args2 (1) := new String'(Bundle_Str);
                  Args2 (2) := new String'(Output_Str);

                  Spawn ("/usr/bin/cp", Args2, Success);

                  Free (Args2 (1));
                  Free (Args2 (2));
               end;

               if Success then
                  Put_Line ("  ✓ Created " & Output_Str);
                  Put_Line ("");
                  Put_Line ("Load with:");
                  Put_Line ("  podman load < " & Output_Str);
                  Put_Line ("  docker load < " & Output_Str);
                  Set_Exit_Status (CT_Errors.Exit_Success);
               else
                  Put_Line ("  Error: Copy failed");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
               end if;
            end if;
         end;
      end;
   end Run_Unpack;

   ------------
   -- Doctor --
   ------------

   procedure Run_Doctor is
   begin
      Put_Line ("ct doctor - Check distribution pipeline health");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --quick   Just check essentials");
      Put_Line ("  --fix     Attempt to fix issues");
      Put_Line ("");
      Put_Line ("Checks performed:");
      Put_Line ("");
      Put_Line ("  Crypto backend:");
      Put_Line ("    [ ] libsodium available");
      Put_Line ("    [ ] liboqs available (for post-quantum)");
      Put_Line ("");
      Put_Line ("  Configuration:");
      Put_Line ("    [ ] Config file valid (~/.config/cerro/config.toml)");
      Put_Line ("    [ ] Policy file valid (~/.config/cerro/policy.json)");
      Put_Line ("    [ ] Default key configured");
      Put_Line ("");
      Put_Line ("  Keys:");
      Put_Line ("    [ ] Keys directory accessible");
      Put_Line ("    [ ] No expired keys");
      Put_Line ("    [ ] Private key decryptable");
      Put_Line ("");
      Put_Line ("  Registry access:");
      Put_Line ("    [ ] Can reach configured registries");
      Put_Line ("    [ ] Authentication valid");
      Put_Line ("");
      Put_Line ("  System:");
      Put_Line ("    [ ] Clock within tolerance");
      Put_Line ("    [ ] Content store healthy");
      Put_Line ("    [ ] Sufficient disk space");
      Put_Line ("");
      Put_Line ("(v0.2 - Not yet implemented)");
      Set_Exit_Status (CT_Errors.Exit_General_Failure);
   end Run_Doctor;

   ------------
   -- Resign --
   ------------

   procedure Run_Resign is
      use Cerro_Crypto_OpenSSL;
      use Cerro_Trust_Store;
      use Ada.Directories;

      Key_Id_Str  : Unbounded_String := Null_Unbounded_String;
      Output_Path : Unbounded_String := Null_Unbounded_String;
      Verbose     : Boolean := False;
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct re-sign <bundle.ctp> -k <key-id> [options]");
         Put_Line ("");
         Put_Line ("Re-sign a bundle with a new key (preserves content).");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  -k, --key <key-id>   New signing key (required)");
         Put_Line ("  -o, --output <file>  Output path (default: <bundle>.sig)");
         Put_Line ("  -v, --verbose        Show detailed progress");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct re-sign nginx.ctp -k new-key-2026");
         Put_Line ("  ct re-sign nginx.ctp -k backup-key -o nginx.sig");
         Put_Line ("");
         Put_Line ("Use cases:");
         Put_Line ("  - Key rotation (old key expiring)");
         Put_Line ("  - Multi-party signing (threshold policies)");
         Put_Line ("  - Countersigning (adding endorsements)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Bundle_Path : constant String := Argument (2);
         I           : Positive := 3;
      begin
         --  Parse arguments
         while I <= Argument_Count loop
            declare
               Arg : constant String := Argument (I);
            begin
               if (Arg = "-k" or Arg = "--key")
                  and then I < Argument_Count
               then
                  Key_Id_Str := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif (Arg = "-o" or Arg = "--output")
                  and then I < Argument_Count
               then
                  Output_Path := To_Unbounded_String (Argument (I + 1));
                  I := I + 2;
               elsif Arg = "-v" or Arg = "--verbose" then
                  Verbose := True;
                  I := I + 1;
               else
                  Put_Line ("Unknown argument: " & Arg);
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;
            end;
         end loop;

         --  Validate bundle exists
         if not Ada.Directories.Exists (Bundle_Path) then
            Put_Line ("Error: Bundle not found: " & Bundle_Path);
            Set_Exit_Status (CT_Errors.Exit_IO_Error);
            return;
         end if;

         --  Key is required
         if Length (Key_Id_Str) = 0 then
            Put_Line ("Error: -k/--key is required for re-signing.");
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
            return;
         end if;

         --  Default output: <bundle>.sig
         if Length (Output_Path) = 0 then
            Output_Path := To_Unbounded_String (Bundle_Path & ".sig");
         end if;

         if Verbose then
            Put_Line ("Re-signing: " & Bundle_Path);
            Put_Line ("New key:    " & To_String (Key_Id_Str));
            Put_Line ("Output:     " & To_String (Output_Path));
            Put_Line ("");
         end if;

         --  Read the private key
         declare
            Home      : constant String :=
               Ada.Environment_Variables.Value ("HOME", "/tmp");
            Key_Dir   : constant String :=
               Home & "/.config/cerro-torre/keys/";
            Priv_Path : constant String :=
               Key_Dir & To_String (Key_Id_Str) & ".priv";
         begin
            if not Ada.Directories.Exists (Priv_Path) then
               Put_Line ("Error: Private key not found: " & Priv_Path);
               Put_Line ("  Generate with: ct keygen --id " &
                         To_String (Key_Id_Str));
               Set_Exit_Status (CT_Errors.Exit_General_Failure);
               return;
            end if;

            declare
               Priv_File : Ada.Text_IO.File_Type;
               Priv_Hex  : String (1 .. 128);
               Last      : Natural;
               Priv_Key  : Ed25519_Private_Key;
               Sig       : Ed25519_Signature;
               OK        : Boolean;
            begin
               Ada.Text_IO.Open (Priv_File, Ada.Text_IO.In_File,
                                 Priv_Path);
               Ada.Text_IO.Get_Line (Priv_File, Priv_Hex, Last);
               Ada.Text_IO.Close (Priv_File);

               if Last /= 128 then
                  Put_Line ("Error: Invalid key format.");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;

               Hex_To_Private_Key (Priv_Hex, Priv_Key, OK);
               if not OK then
                  Put_Line ("Error: Invalid key hex encoding.");
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
                  return;
               end if;

               --  Read bundle content
               declare
                  In_File    : Ada.Text_IO.File_Type;
                  Content    : Unbounded_String := Null_Unbounded_String;
                  Line_Buf   : String (1 .. 4096);
                  Line_Last  : Natural;
                  First_Line : Boolean := True;
               begin
                  Ada.Text_IO.Open (In_File, Ada.Text_IO.In_File,
                                    Bundle_Path);
                  while not Ada.Text_IO.End_Of_File (In_File) loop
                     Ada.Text_IO.Get_Line (In_File, Line_Buf,
                                           Line_Last);
                     if First_Line then
                        First_Line := False;
                     else
                        Append (Content, ASCII.LF);
                     end if;
                     Append (Content, Line_Buf (1 .. Line_Last));
                  end loop;
                  Ada.Text_IO.Close (In_File);

                  --  Sign with new key
                  Sign_Ed25519 (To_String (Content), Priv_Key, Sig, OK);

                  if not OK then
                     Put_Line ("Error: Re-signing failed (OpenSSL).");
                     Set_Exit_Status (CT_Errors.Exit_General_Failure);
                     return;
                  end if;

                  --  Write new signature
                  declare
                     Sig_Hex  : constant String :=
                        Signature_To_Hex (Sig);
                     Out_File : Ada.Text_IO.File_Type;
                  begin
                     Ada.Text_IO.Create (Out_File,
                        Ada.Text_IO.Out_File,
                        To_String (Output_Path));
                     Ada.Text_IO.Put_Line (Out_File, Sig_Hex);
                     Ada.Text_IO.Close (Out_File);
                  end;

                  Put_Line ("Re-signed: " &
                     To_String (Output_Path));
                  if Verbose then
                     Put_Line ("  New signature: " &
                        Signature_To_Hex (Sig) (1 .. 32) & "...");
                  end if;
                  Set_Exit_Status (0);
               end;
            exception
               when E : others =>
                  Put_Line ("Error: " &
                     Ada.Exceptions.Exception_Message (E));
                  Set_Exit_Status (CT_Errors.Exit_General_Failure);
            end;
         end;
      end;
   end Run_Resign;

   ----------
   -- Diff --
   ----------

   procedure Run_Diff is
   begin
      if Argument_Count < 3 then
         Put_Line ("Usage: ct diff <old.ctp> <new.ctp> [options]");
         Put_Line ("");
         Put_Line ("Human-readable diff between bundles.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --layers     Show only layer changes");
         Put_Line ("  --config     Show only config/env changes");
         Put_Line ("  --signers    Show only signature changes");
         Put_Line ("  --json       Output machine-readable JSON");
         Put_Line ("");
         Put_Line ("Output shows:");
         Put_Line ("  - Changed layers (added/removed/modified)");
         Put_Line ("  - Config differences (ENV, labels, entrypoint)");
         Put_Line ("  - Signature changes (new signers, removed)");
         Put_Line ("  - Attestation differences (SBOM, provenance)");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct diff nginx-1.25.ctp nginx-1.26.ctp");
         Put_Line ("  ct diff old.ctp new.ctp --layers");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Old_Bundle : constant String := Argument (2);
         New_Bundle : constant String := Argument (3);
      begin
         Put_Line ("Comparing bundles:");
         Put_Line ("  Old: " & Old_Bundle);
         Put_Line ("  New: " & New_Bundle);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Put_Line ("");
         Put_Line ("Sample output:");
         Put_Line ("");
         Put_Line ("  Layers:");
         Put_Line ("    ~ sha256:abc... -> sha256:def...  (base changed)");
         Put_Line ("    + sha256:123...                   (new layer)");
         Put_Line ("");
         Put_Line ("  Config:");
         Put_Line ("    ~ ENV[""VERSION""] = ""1.25"" -> ""1.26""");
         Put_Line ("");
         Put_Line ("  Signatures:");
         Put_Line ("    = Both signed by: cerro-official-2025");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Diff;

   -----------
   -- Index --
   -----------

   procedure Run_Index is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct index <directory> [options]");
         Put_Line ("");
         Put_Line ("Build searchable index of bundles.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --update    Update existing index");
         Put_Line ("  --output    Index file path (default: ./ct-index.json)");
         Put_Line ("");
         Put_Line ("Indexed fields:");
         Put_Line ("  - name, version, description");
         Put_Line ("  - source image digest");
         Put_Line ("  - signer key IDs and fingerprints");
         Put_Line ("  - SBOM presence, licenses");
         Put_Line ("  - build provenance (builder, date)");
         Put_Line ("  - base image lineage");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Dir_Path : constant String := Argument (2);
      begin
         Put_Line ("Indexing directory: " & Dir_Path);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Index;

   ------------
   -- Search --
   ------------

   procedure Run_Search is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct search <query> [options]");
         Put_Line ("");
         Put_Line ("Search bundles by metadata.");
         Put_Line ("");
         Put_Line ("Options:");
         Put_Line ("  --signer <pattern>   Filter by signer key ID");
         Put_Line ("  --has-sbom           Only bundles with SBOM");
         Put_Line ("  --has-provenance     Only bundles with provenance");
         Put_Line ("  --digest <sha256>    By source image digest");
         Put_Line ("  --after <date>       Created after date");
         Put_Line ("  --before <date>      Created before date");
         Put_Line ("  --index <file>       Index file to search");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct search nginx");
         Put_Line ("  ct search --signer cerro-official-*");
         Put_Line ("  ct search --has-sbom --after 2025-01-01");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Query : constant String := Argument (2);
      begin
         Put_Line ("Searching for: " & Query);
         Put_Line ("");
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Search;

   ------------
   -- Policy --
   ------------

   procedure Run_Policy is
   begin
      if Argument_Count < 2 then
         Put_Line ("Usage: ct policy <subcommand> [args]");
         Put_Line ("");
         Put_Line ("Policy management subcommands:");
         Put_Line ("  init                   Create starter policy interactively");
         Put_Line ("  show                   Display current policy");
         Put_Line ("  add-signer <key-id>    Trust a signer");
         Put_Line ("  add-registry <pat>     Allow a registry pattern");
         Put_Line ("  deny <key-id> [date]   Add to deny-list");
         Put_Line ("  pin <bundle> <digest>  Pin bundle to specific digest");
         Put_Line ("");
         Put_Line ("Examples:");
         Put_Line ("  ct policy init");
         Put_Line ("  ct policy add-signer cerro-official-2025");
         Put_Line ("  ct policy add-registry 'docker.io/library/*'");
         Put_Line ("  ct policy deny compromised-key --after 2025-06-01");
         Put_Line ("  ct policy pin nginx.ctp sha256:abc123...");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;

      declare
         Subcommand : constant String := Argument (2);
      begin
         Put_Line ("Policy subcommand: " & Subcommand);
         Put_Line ("(v0.2 - Not yet implemented)");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
      end;
   end Run_Policy;

   ----------
   -- Help --
   ----------

   procedure Run_Help is
   begin
      Cerro_Explain.Run_Help;
   end Run_Help;

   ---------
   -- Man --
   ---------

   procedure Run_Man is
   begin
      Cerro_Explain.Run_Man;
   end Run_Man;

   -------------
   -- Version --
   -------------

   procedure Run_Version is
   begin
      Cerro_Explain.Run_Version;
   end Run_Version;

end Cerro_CLI;
