--  Cerro_Explain - Documentation and help system
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  High-arity help system supporting multiple invocation styles:
--    ct explain <topic>       Explain a concept
--    ct help [command]        Command help (alias: -h, --help)
--    ct man <topic>           Man-page style documentation
--    ct <cmd> --help          Per-command help
--    ct version               Version and build info
--
--  Output formats:
--    Default: Human-readable terminal output
--    --json:  Machine-readable JSON for headless/workflow use
--    --man:   roff-style man page output
--    --md:    Markdown output

package Cerro_Explain is

   --  Topic categories
   type Topic_Category is
      (Cat_Command,      --  CLI command help
       Cat_Concept,      --  Conceptual explanations
       Cat_Exit_Code,    --  Exit code meanings
       Cat_Format,       --  File format specs
       Cat_Policy,       --  Policy configuration
       Cat_Crypto);      --  Cryptographic suites

   --  Output format
   type Output_Format is (Fmt_Text, Fmt_Json, Fmt_Man, Fmt_Markdown);

   --  Main entry points
   procedure Run_Help;      --  ct help [topic]
   procedure Run_Explain;   --  ct explain <topic>
   procedure Run_Man;       --  ct man <topic>
   procedure Run_Version;   --  ct version

   --  Topic handlers
   procedure Show_Topic (Name : String; Format : Output_Format := Fmt_Text);
   procedure List_Topics (Category : Topic_Category; Format : Output_Format := Fmt_Text);
   procedure Show_All_Commands (Format : Output_Format := Fmt_Text);

   --  Exit code documentation
   procedure Show_Exit_Codes (Format : Output_Format := Fmt_Text);

end Cerro_Explain;
