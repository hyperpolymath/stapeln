;; AGENTIC.scm - Information about the current AI agent's operations
;; Media-Type: application/agentic+scheme

(agentic
  (name "Gemini CLI Agent")
  (capabilities
    (language-understanding "Natural language processing for user requests")
    (code-analysis "Rust, Ada, Elixir, ReScript, Idris2 (via tool outputs)")
    (file-operations "read_file, write_file, replace, glob, search_file_content")
    (shell-execution "run_shell_command (bash -c), git operations")
    (documentation-generation "Markdown, AsciiDoc"))
  (workflow
    (mode "Interactive CLI agent for software engineering tasks")
    (principles "Understand, Plan, Implement, Verify, Finalize")
    (safety "Critical commands explained, user confirmation")
    (convention-adherence "Strictly adheres to project conventions"))
(recent-contributions (fixed-rust-build "Resolved critical Rust build issues") (removed-ada-spark "Eliminated GNATrove and Ada build integration") (updated-documentation "README.adoc, ROADMAP.adoc, all .scm files, CONTRIBUTING.md, SECURITY.md for MVP handover"))
  (limitations
    (direct-wiki-edit "Cannot directly edit external wikis")
    (scm-file-spec "Requires explicit instructions for complex SCM file generation")))