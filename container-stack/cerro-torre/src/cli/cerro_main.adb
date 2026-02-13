--  Cerro Torre - Ship containers safely
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  "Ship containers safely" - the distribution complement to
--  Svalinn's "run containers nicely".

with Ada.Text_IO;
with Ada.Command_Line;
with Cerro_CLI;

procedure Cerro_Main is
   use Ada.Text_IO;
   use Ada.Command_Line;

   Version : constant String := "0.1.0-dev";
begin
   if Argument_Count = 0 then
      Put_Line ("ct - Ship containers safely");
      Put_Line ("");
      Put_Line ("Usage: ct <command> [options]");
      Put_Line ("");
      Put_Line ("Core commands:");
      Put_Line ("  pack <image> -o <file>    Pack OCI image into .ctp bundle");
      Put_Line ("  verify <bundle>           Verify a .ctp bundle");
      Put_Line ("  explain <bundle>          Show verification chain");
      Put_Line ("");
      Put_Line ("Key management:");
      Put_Line ("  keygen                    Generate signing keypair");
      Put_Line ("  key <subcommand>          Key management (list, import, export)");
      Put_Line ("");
      Put_Line ("Signing:");
      Put_Line ("  sign <file> --key <id>    Sign a file with Ed25519");
      Put_Line ("  verify-sig <file> --key   Verify a detached signature");
      Put_Line ("");
      Put_Line ("Distribution:");
      Put_Line ("  fetch <ref> -o <file>     Fetch bundle from registry");
      Put_Line ("  push <bundle> <dest>      Push bundle to registry");
      Put_Line ("  export <bundles> -o <ar>  Export for offline transfer");
      Put_Line ("  import <archive>          Import from offline archive");
      Put_Line ("");
      Put_Line ("Runtime:");
      Put_Line ("  run <bundle>              Run via Svalinn/podman/docker");
      Put_Line ("  unpack <bundle> -o <dir>  Extract to OCI layout");
      Put_Line ("");
      Put_Line ("Diagnostics:");
      Put_Line ("  doctor                    Check pipeline health");
      Put_Line ("  diff <old> <new>          Compare bundles");
      Put_Line ("");
      Put_Line ("Maintenance:");
      Put_Line ("  re-sign <bundle> -k <key> Re-sign with new key");
      Put_Line ("  policy <subcommand>       Policy management");
      Put_Line ("  index <dir>               Build searchable index");
      Put_Line ("  search <query>            Search bundles");
      Put_Line ("");
      Put_Line ("Options:");
      Put_Line ("  --help, -h                Show this help");
      Put_Line ("  --version, -v             Show version");
      Put_Line ("");
      Put_Line ("Examples:");
      Put_Line ("  ct pack docker.io/library/nginx:1.26 -o nginx.ctp");
      Put_Line ("  ct verify nginx.ctp --policy strict.json");
      Put_Line ("  ct run nginx.ctp --runtime=svalinn");
      Put_Line ("  ct diff old.ctp new.ctp");
      Put_Line ("");
      Set_Exit_Status (Failure);
      return;
   end if;

   declare
      Command : constant String := Argument (1);
   begin
      --  Help system (high arity - multiple entry points)
      if Command = "--help" or Command = "-h" or Command = "help" then
         Cerro_CLI.Run_Help;

      elsif Command = "--version" or Command = "-v" or Command = "version" then
         Cerro_CLI.Run_Version;

      elsif Command = "man" then
         Cerro_CLI.Run_Man;

      --  Core commands (MVP v0.1)
      elsif Command = "pack" then
         Cerro_CLI.Run_Pack;

      elsif Command = "verify" then
         Cerro_CLI.Run_Verify;

      elsif Command = "explain" then
         Cerro_CLI.Run_Explain;

      elsif Command = "keygen" then
         Cerro_CLI.Run_Keygen;

      elsif Command = "key" then
         Cerro_CLI.Run_Key;

      elsif Command = "sign" then
         Cerro_CLI.Run_Sign;

      elsif Command = "verify-sig" then
         Cerro_CLI.Run_Verify_Sig;

      --  Distribution commands
      elsif Command = "fetch" then
         Cerro_CLI.Run_Fetch;

      elsif Command = "push" then
         Cerro_CLI.Run_Push;

      elsif Command = "export" then
         Cerro_CLI.Run_Export;

      elsif Command = "import" then
         Cerro_CLI.Run_Import;

      --  Runtime integration
      elsif Command = "run" then
         Cerro_CLI.Run_Run;

      elsif Command = "unpack" then
         Cerro_CLI.Run_Unpack;

      --  Diagnostics
      elsif Command = "doctor" then
         Cerro_CLI.Run_Doctor;

      elsif Command = "diff" then
         Cerro_CLI.Run_Diff;

      --  Maintenance
      elsif Command = "re-sign" then
         Cerro_CLI.Run_Resign;

      elsif Command = "policy" then
         Cerro_CLI.Run_Policy;

      elsif Command = "index" then
         Cerro_CLI.Run_Index;

      elsif Command = "search" then
         Cerro_CLI.Run_Search;

      else
         Put_Line ("Unknown command: " & Command);
         Put_Line ("");
         Put_Line ("Run 'ct --help' for usage.");
         Set_Exit_Status (Failure);
      end if;
   end;
end Cerro_Main;
