--  Cerro_Explain - Implementation
--  SPDX-License-Identifier: PMPL-1.0-or-later

with Ada.Text_IO;
with Ada.Command_Line;
with CT_Errors;

package body Cerro_Explain is

   use Ada.Text_IO;
   use Ada.Command_Line;

   Version_String : constant String := "0.1.0-alpha";
   Build_Date     : constant String := "2026-01-18";

   ---------------------------------------------------------------------------
   --  Internal: Output helpers
   ---------------------------------------------------------------------------

   procedure Put_Json_String (Key, Value : String) is
   begin
      Put ("  """ & Key & """: """ & Value & """");
   end Put_Json_String;

   function Get_Output_Format return Output_Format is
   begin
      for I in 2 .. Argument_Count loop
         if Argument (I) = "--json" then
            return Fmt_Json;
         elsif Argument (I) = "--man" then
            return Fmt_Man;
         elsif Argument (I) = "--md" or Argument (I) = "--markdown" then
            return Fmt_Markdown;
         end if;
      end loop;
      return Fmt_Text;
   end Get_Output_Format;

   ---------------------------------------------------------------------------
   --  Show_Version - ct version
   ---------------------------------------------------------------------------

   procedure Show_Version_Text is
   begin
      Put_Line ("Cerro Torre " & Version_String);
      Put_Line ("Build: " & Build_Date);
      Put_Line ("License: PMPL-1.0-or-later");
      Put_Line ("");
      Put_Line ("Crypto suites:");
      Put_Line ("  CT-SIG-01  Ed25519 (RFC 8032)           [available]");
      Put_Line ("  CT-SIG-02  Ed25519 + ML-DSA-87 (hybrid) [v0.2]");
      Put_Line ("  CT-SIG-03  ML-DSA-87 (post-quantum)     [v0.2]");
      Put_Line ("  CT-SIG-04  Ed448 (optional)             [v0.2]");
      Put_Line ("");
      Put_Line ("Hash algorithms:");
      Put_Line ("  SHA-256    FIPS 180-4                   [available]");
      Put_Line ("  SHA-512    FIPS 180-4                   [available]");
   end Show_Version_Text;

   procedure Show_Version_Json is
   begin
      Put_Line ("{");
      Put_Json_String ("version", Version_String);
      Put_Line (",");
      Put_Json_String ("build_date", Build_Date);
      Put_Line (",");
      Put_Json_String ("license", "PMPL-1.0-or-later");
      Put_Line (",");
      Put_Line ("  ""crypto_suites"": {");
      Put_Line ("    ""CT-SIG-01"": {""name"": ""Ed25519"", ""status"": ""available""},");
      Put_Line ("    ""CT-SIG-02"": {""name"": ""Ed25519+ML-DSA-87"", ""status"": ""v0.2""},");
      Put_Line ("    ""CT-SIG-03"": {""name"": ""ML-DSA-87"", ""status"": ""v0.2""},");
      Put_Line ("    ""CT-SIG-04"": {""name"": ""Ed448"", ""status"": ""v0.2""}");
      Put_Line ("  },");
      Put_Line ("  ""hash_algorithms"": {");
      Put_Line ("    ""SHA-256"": ""available"",");
      Put_Line ("    ""SHA-512"": ""available""");
      Put_Line ("  }");
      Put_Line ("}");
   end Show_Version_Json;

   procedure Run_Version is
      Fmt : constant Output_Format := Get_Output_Format;
   begin
      case Fmt is
         when Fmt_Json => Show_Version_Json;
         when others   => Show_Version_Text;
      end case;
      Set_Exit_Status (0);
   end Run_Version;

   ---------------------------------------------------------------------------
   --  Show_Exit_Codes - Explain exit code semantics
   ---------------------------------------------------------------------------

   procedure Show_Exit_Codes (Format : Output_Format := Fmt_Text) is
   begin
      case Format is
         when Fmt_Json =>
            Put_Line ("{");
            Put_Line ("  ""exit_codes"": {");
            Put_Line ("    ""0"": {""name"": ""OK"", ""meaning"": ""Verification succeeded""},");
            Put_Line ("    ""1"": {""name"": ""HASH_MISMATCH"", ""meaning"": ""Content tampered""},");
            Put_Line ("    ""2"": {""name"": ""SIGNATURE_INVALID"", ""meaning"": ""Signature verification failed""},");
            Put_Line ("    ""3"": {""name"": ""KEY_NOT_TRUSTED"", ""meaning"": ""Key not in trust store""},");
            Put_Line ("    ""4"": {""name"": ""POLICY_REJECTION"", ""meaning"": ""Policy denies this bundle""},");
            Put_Line ("    ""5"": {""name"": ""MISSING_ATTESTATION"", ""meaning"": ""Required attestation absent""},");
            Put_Line ("    ""10"": {""name"": ""MALFORMED_BUNDLE"", ""meaning"": ""Bundle structure invalid""},");
            Put_Line ("    ""11"": {""name"": ""IO_ERROR"", ""meaning"": ""File system or network error""}");
            Put_Line ("  }");
            Put_Line ("}");

         when Fmt_Man =>
            Put_Line (".TH CT-EXIT-CODES 7 """ & Build_Date & """ ""Cerro Torre"" ""Exit Codes""");
            Put_Line (".SH NAME");
            Put_Line ("ct-exit-codes \- Cerro Torre verification exit codes");
            Put_Line (".SH DESCRIPTION");
            Put_Line ("Exit codes from ct verify are designed for CI/CD integration.");
            Put_Line (".SH EXIT CODES");
            Put_Line (".TP");
            Put_Line (".B 0");
            Put_Line ("Verification succeeded. Bundle is valid and trusted.");
            Put_Line (".TP");
            Put_Line (".B 1");
            Put_Line ("Hash mismatch. Content may have been tampered with.");
            Put_Line (".TP");
            Put_Line (".B 2");
            Put_Line ("Signature invalid. Cryptographic verification failed.");
            Put_Line (".TP");
            Put_Line (".B 3");
            Put_Line ("Key not trusted. Signing key not in trust store.");
            Put_Line (".TP");
            Put_Line (".B 4");
            Put_Line ("Policy rejection. Bundle denied by local policy.");
            Put_Line (".TP");
            Put_Line (".B 5");
            Put_Line ("Missing attestation. Required attestation not present.");
            Put_Line (".TP");
            Put_Line (".B 10");
            Put_Line ("Malformed bundle. Bundle structure is invalid.");
            Put_Line (".TP");
            Put_Line (".B 11");
            Put_Line ("I/O error. File system or network problem.");

         when others =>
            Put_Line ("EXIT CODES");
            Put_Line ("==========");
            Put_Line ("");
            Put_Line ("Cerro Torre uses specific exit codes for CI/CD integration:");
            Put_Line ("");
            Put_Line ("  Code  Name                 Meaning");
            Put_Line ("  ----  ----                 -------");
            Put_Line ("  0     OK                   Verification succeeded");
            Put_Line ("  1     HASH_MISMATCH        Content tampered (hash doesn't match)");
            Put_Line ("  2     SIGNATURE_INVALID    Signature verification failed");
            Put_Line ("  3     KEY_NOT_TRUSTED      Signing key not in trust store");
            Put_Line ("  4     POLICY_REJECTION     Bundle denied by policy");
            Put_Line ("  5     MISSING_ATTESTATION  Required attestation absent");
            Put_Line ("  10    MALFORMED_BUNDLE     Bundle structure invalid");
            Put_Line ("  11    IO_ERROR             File system or network error");
            Put_Line ("");
            Put_Line ("Shell integration:");
            Put_Line ("  ct verify bundle.ctp && echo 'OK' || echo ""Failed: $?""");
            Put_Line ("");
            Put_Line ("CI/CD example (GitHub Actions):");
            Put_Line ("  - run: ct verify ${{ inputs.bundle }}");
            Put_Line ("    continue-on-error: true");
            Put_Line ("    id: verify");
            Put_Line ("  - if: steps.verify.outcome == 'failure'");
            Put_Line ("    run: |");
            Put_Line ("      case ${{ steps.verify.outputs.exit_code }} in");
            Put_Line ("        1) echo 'Content tampered!' ;;");
            Put_Line ("        3) echo 'Key not trusted' ;;");
            Put_Line ("      esac");
      end case;
   end Show_Exit_Codes;

   ---------------------------------------------------------------------------
   --  Show_All_Commands - List all available commands
   ---------------------------------------------------------------------------

   procedure Show_All_Commands (Format : Output_Format := Fmt_Text) is
   begin
      case Format is
         when Fmt_Json =>
            Put_Line ("{");
            Put_Line ("  ""commands"": {");
            Put_Line ("    ""core"": [");
            Put_Line ("      {""name"": ""pack"", ""description"": ""Create .ctp bundle from manifest""},");
            Put_Line ("      {""name"": ""verify"", ""description"": ""Verify bundle integrity and trust""},");
            Put_Line ("      {""name"": ""explain"", ""description"": ""Show human-readable verification chain""}");
            Put_Line ("    ],");
            Put_Line ("    ""keys"": [");
            Put_Line ("      {""name"": ""keygen"", ""description"": ""Generate signing keypair""},");
            Put_Line ("      {""name"": ""key"", ""description"": ""Key management (list/import/export)""}");
            Put_Line ("    ],");
            Put_Line ("    ""distribution"": [");
            Put_Line ("      {""name"": ""fetch"", ""description"": ""Pull bundle from registry""},");
            Put_Line ("      {""name"": ""push"", ""description"": ""Publish bundle to registry""},");
            Put_Line ("      {""name"": ""import"", ""description"": ""Import from offline archive""},");
            Put_Line ("      {""name"": ""export"", ""description"": ""Export for offline transfer""}");
            Put_Line ("    ],");
            Put_Line ("    ""runtime"": [");
            Put_Line ("      {""name"": ""run"", ""description"": ""Run verified bundle via runtime""},");
            Put_Line ("      {""name"": ""unpack"", ""description"": ""Extract to OCI layout""}");
            Put_Line ("    ],");
            Put_Line ("    ""policy"": [");
            Put_Line ("      {""name"": ""policy"", ""description"": ""Trust policy management""}");
            Put_Line ("    ],");
            Put_Line ("    ""utility"": [");
            Put_Line ("      {""name"": ""diff"", ""description"": ""Compare two bundles""},");
            Put_Line ("      {""name"": ""index"", ""description"": ""Build searchable index""},");
            Put_Line ("      {""name"": ""search"", ""description"": ""Search bundles by metadata""},");
            Put_Line ("      {""name"": ""doctor"", ""description"": ""Check system health""},");
            Put_Line ("      {""name"": ""re-sign"", ""description"": ""Re-sign bundle with new key""}");
            Put_Line ("    ],");
            Put_Line ("    ""help"": [");
            Put_Line ("      {""name"": ""help"", ""description"": ""Show help (alias: -h, --help)""},");
            Put_Line ("      {""name"": ""version"", ""description"": ""Show version information""},");
            Put_Line ("      {""name"": ""man"", ""description"": ""Man-page style documentation""}");
            Put_Line ("    ]");
            Put_Line ("  }");
            Put_Line ("}");

         when others =>
            Put_Line ("CERRO TORRE COMMANDS");
            Put_Line ("====================");
            Put_Line ("");
            Put_Line ("Core (v0.1):");
            Put_Line ("  pack        Create .ctp bundle from manifest");
            Put_Line ("  verify      Verify bundle integrity and trust");
            Put_Line ("  explain     Show human-readable verification chain");
            Put_Line ("");
            Put_Line ("Key Management:");
            Put_Line ("  keygen      Generate signing keypair");
            Put_Line ("  key         Key management (list/import/export/delete)");
            Put_Line ("");
            Put_Line ("Distribution (v0.2):");
            Put_Line ("  fetch       Pull bundle from registry");
            Put_Line ("  push        Publish bundle to registry");
            Put_Line ("  import      Import from offline archive");
            Put_Line ("  export      Export for offline transfer");
            Put_Line ("");
            Put_Line ("Runtime Integration (v0.2):");
            Put_Line ("  run         Run verified bundle via Svalinn/Podman/Docker");
            Put_Line ("  unpack      Extract to OCI layout on disk");
            Put_Line ("");
            Put_Line ("Policy:");
            Put_Line ("  policy      Trust policy management");
            Put_Line ("");
            Put_Line ("Utilities:");
            Put_Line ("  diff        Human-readable diff between bundles");
            Put_Line ("  index       Build searchable index of bundles");
            Put_Line ("  search      Search bundles by metadata");
            Put_Line ("  doctor      Check distribution pipeline health");
            Put_Line ("  re-sign     Re-sign bundle with new key");
            Put_Line ("");
            Put_Line ("Help:");
            Put_Line ("  help        Show help (alias: ct -h, ct --help)");
            Put_Line ("  version     Show version and crypto suite info");
            Put_Line ("  man         Man-page style documentation");
            Put_Line ("");
            Put_Line ("Use 'ct help <command>' for detailed help on any command.");
            Put_Line ("Use 'ct explain <topic>' for conceptual explanations.");
      end case;
   end Show_All_Commands;

   ---------------------------------------------------------------------------
   --  List_Topics - Show available topics in a category
   ---------------------------------------------------------------------------

   procedure List_Topics (Category : Topic_Category; Format : Output_Format := Fmt_Text) is
   begin
      case Format is
         when Fmt_Json =>
            Put_Line ("{");
            case Category is
               when Cat_Concept =>
                  Put_Line ("  ""topics"": [");
                  Put_Line ("    ""provenance"", ""attestation"", ""trust-chain"",");
                  Put_Line ("    ""threshold-signing"", ""transparency-log"",");
                  Put_Line ("    ""crypto-suites"", ""post-quantum""");
                  Put_Line ("  ]");
               when Cat_Exit_Code =>
                  Put_Line ("  ""topics"": [""exit-codes""]");
               when Cat_Format =>
                  Put_Line ("  ""topics"": [""manifest"", ""summary-json"", ""policy-file""]");
               when others =>
                  Put_Line ("  ""topics"": []");
            end case;
            Put_Line ("}");

         when others =>
            Put_Line ("Available topics:");
            Put_Line ("");
            case Category is
               when Cat_Concept =>
                  Put_Line ("  provenance         Package origin and chain of custody");
                  Put_Line ("  attestation        Third-party claims about packages");
                  Put_Line ("  trust-chain        How trust is established");
                  Put_Line ("  threshold-signing  Multi-party signature requirements");
                  Put_Line ("  transparency-log   Public append-only audit log");
                  Put_Line ("  crypto-suites      Supported cryptographic algorithms");
                  Put_Line ("  post-quantum       Quantum-resistant cryptography");
               when Cat_Exit_Code =>
                  Put_Line ("  exit-codes         Verification exit code meanings");
               when Cat_Format =>
                  Put_Line ("  manifest           .ctp manifest file format");
                  Put_Line ("  summary-json       summary.json schema");
                  Put_Line ("  policy-file        Trust policy configuration");
               when others =>
                  Put_Line ("  (no topics in this category)");
            end case;
      end case;
   end List_Topics;

   ---------------------------------------------------------------------------
   --  Show_Topic - Display documentation for a specific topic
   ---------------------------------------------------------------------------

   procedure Show_Topic (Name : String; Format : Output_Format := Fmt_Text) is
   begin
      if Name = "provenance" then
         case Format is
            when Fmt_Json =>
               Put_Line ("{");
               Put_Json_String ("topic", "provenance");
               Put_Line (",");
               Put_Json_String ("summary", "Package origin and chain of custody");
               Put_Line (",");
               Put_Line ("  ""content"": ""Provenance tracks where a package came from and how it was built.""");
               Put_Line ("}");
            when others =>
               Put_Line ("PROVENANCE");
               Put_Line ("==========");
               Put_Line ("");
               Put_Line ("Provenance is the record of a package's origin and transformation history.");
               Put_Line ("");
               Put_Line ("A CTP bundle's provenance includes:");
               Put_Line ("  - upstream     Original source location (tarball, git repo)");
               Put_Line ("  - upstream_hash   Cryptographic hash of original");
               Put_Line ("  - imported_from   Distribution it was imported from");
               Put_Line ("  - import_date     When the import occurred");
               Put_Line ("  - build_info      How it was compiled/assembled");
               Put_Line ("");
               Put_Line ("Example manifest provenance section:");
               Put_Line ("  [provenance]");
               Put_Line ("  upstream = ""https://ftp.gnu.org/gnu/hello/hello-2.10.tar.gz""");
               Put_Line ("  upstream_hash = ""sha256:31e066137a962676...""");
               Put_Line ("  imported_from = ""debian:hello/2.10-3""");
               Put_Line ("  import_date = 2025-01-15T14:30:00Z");
         end case;

      elsif Name = "exit-codes" then
         Show_Exit_Codes (Format);

      elsif Name = "crypto-suites" then
         case Format is
            when Fmt_Json =>
               Put_Line ("{");
               Put_Json_String ("topic", "crypto-suites");
               Put_Line (",");
               Put_Line ("  ""suites"": {");
               Put_Line ("    ""CT-SIG-01"": {");
               Put_Line ("      ""algorithms"": [""Ed25519""],");
               Put_Line ("      ""security"": ""128-bit classical"",");
               Put_Line ("      ""status"": ""default""");
               Put_Line ("    },");
               Put_Line ("    ""CT-SIG-02"": {");
               Put_Line ("      ""algorithms"": [""Ed25519"", ""ML-DSA-87""],");
               Put_Line ("      ""security"": ""128-bit classical + 192-bit post-quantum"",");
               Put_Line ("      ""status"": ""recommended for governance""");
               Put_Line ("    },");
               Put_Line ("    ""CT-SIG-03"": {");
               Put_Line ("      ""algorithms"": [""ML-DSA-87""],");
               Put_Line ("      ""security"": ""192-bit post-quantum"",");
               Put_Line ("      ""status"": ""available""");
               Put_Line ("    },");
               Put_Line ("    ""CT-SIG-04"": {");
               Put_Line ("      ""algorithms"": [""Ed448""],");
               Put_Line ("      ""security"": ""224-bit classical"",");
               Put_Line ("      ""status"": ""optional""");
               Put_Line ("    }");
               Put_Line ("  }");
               Put_Line ("}");
            when others =>
               Put_Line ("CRYPTOGRAPHIC SUITES");
               Put_Line ("====================");
               Put_Line ("");
               Put_Line ("Cerro Torre supports multiple cryptographic suites for different security needs.");
               Put_Line ("");
               Put_Line ("CT-SIG-01: Ed25519 (Default)");
               Put_Line ("  - Algorithm: Ed25519 (RFC 8032)");
               Put_Line ("  - Security: 128-bit classical");
               Put_Line ("  - Use: General purpose, fast verification");
               Put_Line ("  - Status: Available now");
               Put_Line ("");
               Put_Line ("CT-SIG-02: Ed25519 + ML-DSA-87 (Hybrid)");
               Put_Line ("  - Algorithms: Ed25519 + ML-DSA-87 (FIPS 204)");
               Put_Line ("  - Security: 128-bit classical + 192-bit post-quantum");
               Put_Line ("  - Use: Governance keys, long-term archives");
               Put_Line ("  - Status: v0.2");
               Put_Line ("");
               Put_Line ("CT-SIG-03: ML-DSA-87 (Post-Quantum Only)");
               Put_Line ("  - Algorithm: ML-DSA-87 (FIPS 204)");
               Put_Line ("  - Security: 192-bit post-quantum");
               Put_Line ("  - Use: Post-quantum only environments");
               Put_Line ("  - Status: v0.2");
               Put_Line ("");
               Put_Line ("CT-SIG-04: Ed448 (Stronger Classical)");
               Put_Line ("  - Algorithm: Ed448 (RFC 8032)");
               Put_Line ("  - Security: 224-bit classical");
               Put_Line ("  - Use: Higher security classical environments");
               Put_Line ("  - Status: v0.2");
         end case;

      elsif Name = "manifest" then
         Put_Line ("MANIFEST FORMAT");
         Put_Line ("===============");
         Put_Line ("");
         Put_Line ("The .ctp manifest file uses a TOML-like format with these sections:");
         Put_Line ("");
         Put_Line ("[metadata]");
         Put_Line ("  name, version, summary, description, license, maintainer");
         Put_Line ("");
         Put_Line ("[provenance]");
         Put_Line ("  upstream, upstream_hash, imported_from, import_date");
         Put_Line ("");
         Put_Line ("[dependencies]");
         Put_Line ("  runtime = [""dep1"", ""dep2""]");
         Put_Line ("  build = [""gcc"", ""make""]");
         Put_Line ("");
         Put_Line ("[build]");
         Put_Line ("  system = ""autoconf""");
         Put_Line ("  configure_flags = [""--prefix=/usr""]");
         Put_Line ("");
         Put_Line ("[attestations]");
         Put_Line ("  require = [""source-signature""]");
         Put_Line ("  recommend = [""reproducible-build""]");

      else
         Put_Line ("Unknown topic: " & Name);
         Put_Line ("");
         Put_Line ("Use 'ct explain --list' to see available topics.");
         Set_Exit_Status (CT_Errors.Exit_General_Failure);
         return;
      end if;
      Set_Exit_Status (0);
   end Show_Topic;

   ---------------------------------------------------------------------------
   --  Run_Help - ct help [command]
   ---------------------------------------------------------------------------

   procedure Run_Help is
      Fmt : constant Output_Format := Get_Output_Format;
   begin
      if Argument_Count < 2 or else Argument (2) = "--json" then
         Show_All_Commands (Fmt);
         Set_Exit_Status (0);
         return;
      end if;

      declare
         Topic : constant String := Argument (2);
      begin
         if Topic = "pack" then
            Put_Line ("ct pack - Create a .ctp bundle from a manifest");
            Put_Line ("");
            Put_Line ("USAGE:");
            Put_Line ("  ct pack <manifest.ctp> -o <output.ctp> [options]");
            Put_Line ("");
            Put_Line ("OPTIONS:");
            Put_Line ("  -o, --output <file>    Output path for .ctp bundle (required)");
            Put_Line ("  -s, --sources <dir>    Include source files from directory");
            Put_Line ("  -v, --verbose          Show detailed progress");
            Put_Line ("  --no-sign              Create unsigned bundle");
            Put_Line ("");
            Put_Line ("EXAMPLES:");
            Put_Line ("  ct pack manifests/hello.ctp -o hello.ctp");
            Put_Line ("  ct pack nginx.ctp -o nginx.ctp -s ./nginx-src/ -v");
            Put_Line ("");
            Put_Line ("EXIT CODES:");
            Put_Line ("  0   Success");
            Put_Line ("  1   General failure");

         elsif Topic = "verify" then
            Put_Line ("ct verify - Verify bundle integrity and trust");
            Put_Line ("");
            Put_Line ("USAGE:");
            Put_Line ("  ct verify <bundle.ctp> [options]");
            Put_Line ("");
            Put_Line ("OPTIONS:");
            Put_Line ("  --policy <file>   Trust policy file");
            Put_Line ("  --offline         Skip transparency log checks");
            Put_Line ("  -v, --verbose     Show detailed verification steps");
            Put_Line ("  --json            Output machine-readable JSON");
            Put_Line ("");
            Put_Line ("EXIT CODES:");
            Put_Line ("  0   OK - Verification succeeded");
            Put_Line ("  1   HASH_MISMATCH - Content tampered");
            Put_Line ("  2   SIGNATURE_INVALID - Bad signature");
            Put_Line ("  3   KEY_NOT_TRUSTED - Key not in trust store");
            Put_Line ("  4   POLICY_REJECTION - Denied by policy");
            Put_Line ("  5   MISSING_ATTESTATION - Required attestation absent");
            Put_Line ("  10  MALFORMED_BUNDLE - Invalid bundle structure");
            Put_Line ("  11  IO_ERROR - File system or network error");
            Put_Line ("");
            Put_Line ("EXAMPLES:");
            Put_Line ("  ct verify nginx.ctp");
            Put_Line ("  ct verify nginx.ctp --policy strict.json");
            Put_Line ("  ct verify nginx.ctp --json | jq .status");

         elsif Topic = "explain" then
            Put_Line ("ct explain - Show human-readable verification chain");
            Put_Line ("");
            Put_Line ("USAGE:");
            Put_Line ("  ct explain <bundle.ctp> [options]");
            Put_Line ("  ct explain <topic>");
            Put_Line ("  ct explain --list");
            Put_Line ("");
            Put_Line ("OPTIONS (bundle mode):");
            Put_Line ("  --signers    Show only signer information");
            Put_Line ("  --layers     Show only layer digests");
            Put_Line ("  --json       Machine-readable output");
            Put_Line ("");
            Put_Line ("TOPICS:");
            Put_Line ("  provenance      Package origin and chain of custody");
            Put_Line ("  exit-codes      Verification exit code meanings");
            Put_Line ("  crypto-suites   Supported cryptographic algorithms");
            Put_Line ("  manifest        .ctp manifest file format");
            Put_Line ("");
            Put_Line ("EXAMPLES:");
            Put_Line ("  ct explain nginx.ctp");
            Put_Line ("  ct explain provenance");
            Put_Line ("  ct explain exit-codes --json");

         elsif Topic = "exit-codes" then
            Show_Exit_Codes (Fmt);

         else
            Put_Line ("Unknown command: " & Topic);
            Put_Line ("");
            Put_Line ("Use 'ct help' to see all available commands.");
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
            return;
         end if;
         Set_Exit_Status (0);
      end;
   end Run_Help;

   ---------------------------------------------------------------------------
   --  Run_Explain - ct explain <topic>
   ---------------------------------------------------------------------------

   procedure Run_Explain is
      Fmt : constant Output_Format := Get_Output_Format;
   begin
      if Argument_Count < 2 then
         Put_Line ("ct explain - Documentation and explanation system");
         Put_Line ("");
         Put_Line ("USAGE:");
         Put_Line ("  ct explain <bundle.ctp>    Show verification chain for bundle");
         Put_Line ("  ct explain <topic>         Explain a concept");
         Put_Line ("  ct explain --list          List available topics");
         Put_Line ("");
         Put_Line ("OUTPUT FORMATS:");
         Put_Line ("  (default)      Human-readable terminal output");
         Put_Line ("  --json         Machine-readable JSON for headless use");
         Put_Line ("  --man          Man-page style (roff) output");
         Put_Line ("  --md           Markdown output");
         Put_Line ("");
         Put_Line ("TOPICS:");
         Put_Line ("  provenance     Package origin and chain of custody");
         Put_Line ("  exit-codes     Verification exit code meanings");
         Put_Line ("  crypto-suites  Supported cryptographic algorithms");
         Put_Line ("  manifest       .ctp manifest file format");
         Put_Line ("");
         Put_Line ("EXAMPLES:");
         Put_Line ("  ct explain provenance");
         Put_Line ("  ct explain exit-codes --json");
         Put_Line ("  ct explain crypto-suites --man > ct-crypto.7");
         Set_Exit_Status (0);
         return;
      end if;

      declare
         Topic : constant String := Argument (2);
      begin
         if Topic = "--list" then
            List_Topics (Cat_Concept, Fmt);
         elsif Topic (Topic'Last - 3 .. Topic'Last) = ".ctp" then
            --  Bundle explanation (stub for now)
            Put_Line ("Explaining bundle: " & Topic);
            Put_Line ("");
            Put_Line ("(Bundle explanation not yet implemented)");
            Put_Line ("");
            Put_Line ("Will show:");
            Put_Line ("  - Package info (name, version, suite)");
            Put_Line ("  - Provenance (source, fetch time)");
            Put_Line ("  - Content (manifest digest, layers)");
            Put_Line ("  - Signatures (key id, fingerprint, time)");
            Put_Line ("  - Trust chain status");
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
         else
            Show_Topic (Topic, Fmt);
         end if;
      end;
   end Run_Explain;

   ---------------------------------------------------------------------------
   --  Run_Man - ct man <topic>
   ---------------------------------------------------------------------------

   procedure Run_Man is
   begin
      if Argument_Count < 2 then
         Put_Line ("ct man - Man-page style documentation");
         Put_Line ("");
         Put_Line ("USAGE:");
         Put_Line ("  ct man <topic>");
         Put_Line ("  ct man ct                  Main manual page");
         Put_Line ("  ct man ct-verify           Verify command");
         Put_Line ("  ct man ct-exit-codes       Exit code reference");
         Put_Line ("");
         Put_Line ("Generate installable man pages:");
         Put_Line ("  ct man ct > /usr/share/man/man1/ct.1");
         Put_Line ("  ct man ct-verify > /usr/share/man/man1/ct-verify.1");
         Put_Line ("  ct man ct-exit-codes > /usr/share/man/man7/ct-exit-codes.7");
         Set_Exit_Status (0);
         return;
      end if;

      declare
         Topic : constant String := Argument (2);
      begin
         if Topic = "ct" or Topic = "cerro-torre" then
            Put_Line (".TH CT 1 """ & Build_Date & """ ""Cerro Torre " & Version_String & """ ""User Commands""");
            Put_Line (".SH NAME");
            Put_Line ("ct \- Cerro Torre provenance-verified container management");
            Put_Line (".SH SYNOPSIS");
            Put_Line (".B ct");
            Put_Line (".IR command");
            Put_Line ("[options] [arguments]");
            Put_Line (".SH DESCRIPTION");
            Put_Line ("Cerro Torre (ct) provides provenance-verified container distribution.");
            Put_Line ("It wraps OCI images with cryptographic signatures, attestations,");
            Put_Line ("and trust policies for secure supply chain management.");
            Put_Line (".SH COMMANDS");
            Put_Line (".TP");
            Put_Line (".B pack");
            Put_Line ("Create a .ctp bundle from a manifest.");
            Put_Line (".TP");
            Put_Line (".B verify");
            Put_Line ("Verify bundle integrity and trust chain.");
            Put_Line (".TP");
            Put_Line (".B explain");
            Put_Line ("Show human-readable verification details.");
            Put_Line (".SH SEE ALSO");
            Put_Line (".BR ct-verify (1),");
            Put_Line (".BR ct-exit-codes (7)");
            Set_Exit_Status (0);

         elsif Topic = "ct-exit-codes" then
            Show_Exit_Codes (Fmt_Man);

         else
            Put_Line ("Unknown man page: " & Topic);
            Set_Exit_Status (CT_Errors.Exit_General_Failure);
         end if;
      end;
   end Run_Man;

end Cerro_Explain;
