--  Cerro Torre CLI - Command implementations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--  Palimpsest-Covenant: 1.0
--
--  "Ship containers safely" - the distribution complement to Svalinn's
--  "run containers nicely".

package Cerro_CLI is

   --  Pack an OCI image into a verifiable .ctp bundle
   --  Usage: ct pack <image-ref> -o <output.ctp>
   --  Example: ct pack docker.io/library/nginx:1.26 -o nginx.ctp
   procedure Run_Pack;

   --  Verify a .ctp bundle with specific exit codes
   --  Usage: ct verify <bundle.ctp> [--policy <file>]
   --  Exit codes: 0=valid, 1=hash, 2=sig, 3=key, 4=policy, 10=malformed
   procedure Run_Verify;

   --  Print human-readable verification chain
   --  Usage: ct explain <bundle.ctp> [--signers|--layers]
   procedure Run_Explain;

   --  Generate a signing keypair
   --  Usage: ct keygen [--id <name>] [--suite <suite-id>]
   procedure Run_Keygen;

   --  Key management subcommands
   --  Usage: ct key <list|import|export|default> [args]
   procedure Run_Key;

   --  Sign a file or message with Ed25519 private key
   --  Usage: ct sign <file> --key <key-id> [-o <sig-file>]
   --  Produces a detached .sig file containing the Ed25519 signature.
   procedure Run_Sign;

   --  Verify a detached Ed25519 signature
   --  Usage: ct verify-sig <file> --sig <sig-file> --key <key-id>
   --  Exit codes: 0=valid, 2=invalid signature, 3=key not found
   procedure Run_Verify_Sig;

   --  ======== v0.2 Commands (stubs) ========

   --  Fetch bundle from registry or create from OCI image
   --  Usage: ct fetch <ref> -o <output.ctp> [--create]
   procedure Run_Fetch;

   --  Push bundle to registry/mirror
   --  Usage: ct push <bundle.ctp> <destination>
   procedure Run_Push;

   --  Export bundles for offline transfer
   --  Usage: ct export <bundles...> -o <archive>
   procedure Run_Export;

   --  Import from offline archive
   --  Usage: ct import <archive> [--verify]
   procedure Run_Import;

   --  Transparency log operations
   --  Usage: ct log <submit|verify|search> [args]
   procedure Run_Log;

   --  ======== Runtime Integration (v0.2) ========

   --  Run bundle via configured runtime (Svalinn, podman, etc.)
   --  Usage: ct run <bundle.ctp> [--runtime=<name>] [-- <args>]
   procedure Run_Run;

   --  Unpack bundle to OCI layout on disk
   --  Usage: ct unpack <bundle.ctp> -o <dir> [--format=oci|docker]
   procedure Run_Unpack;

   --  ======== Diagnostics (v0.2) ========

   --  Check distribution pipeline health
   --  Usage: ct doctor [--quick]
   procedure Run_Doctor;

   --  ======== Key Rotation (v0.2) ========

   --  Re-sign bundle with new key (preserves content)
   --  Usage: ct re-sign <bundle.ctp> -k <key-id> [--add-signature]
   procedure Run_Resign;

   --  ======== Bundle Comparison (v0.2) ========

   --  Human-readable diff between bundles
   --  Usage: ct diff <old.ctp> <new.ctp> [--layers|--signers]
   procedure Run_Diff;

   --  ======== Index & Search (v0.2) ========

   --  Build searchable index of bundles
   --  Usage: ct index <dir> [--update]
   procedure Run_Index;

   --  Search bundles by metadata
   --  Usage: ct search <query> [--signer|--has-sbom|--digest|--after]
   procedure Run_Search;

   --  ======== Policy Helpers (v0.2) ========

   --  Policy management subcommands
   --  Usage: ct policy <init|add-signer|add-registry|deny>
   procedure Run_Policy;

   --  ======== Help System (v0.1) ========

   --  Show help (multiple entry points for high arity)
   --  Usage: ct help [command], ct -h, ct --help
   procedure Run_Help;

   --  Man-page style documentation
   --  Usage: ct man <topic>
   procedure Run_Man;

   --  Version and crypto suite info
   --  Usage: ct version
   procedure Run_Version;

end Cerro_CLI;
