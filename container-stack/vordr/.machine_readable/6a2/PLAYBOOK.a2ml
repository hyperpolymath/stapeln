;; PLAYBOOK.scm - Common workflows and operational procedures for Vordr
;; Media-Type: application/playbook+scheme

(playbook
  (development-workflow
    (bug-fixing "Identify issue -> Reproduce -> Analyze logs/errors -> Plan fix -> Implement -> Test -> Verify -> Commit -> Push")
    (feature-implementation "Understand requirements -> Design -> Plan -> Implement -> Test -> Verify -> Commit -> Push")
    (build-issue-resolution
      (steps
        "Attempt `cargo build` to get error details."
        "Analyze error messages (e.g., unresolved imports, duplicate definitions, syntax errors)."
        "Identify affected files and components."
        "Read relevant source code (`read_file`) to understand context."
        "Formulate targeted fix using `replace` or `write_file`."
        "Iterate build/fix until successful."
        "Run `cargo fix` for automated linting/cleanup."
        "Commit changes.")))
  (documentation-updates
    (process "Review affected `.adoc` files -> Update content to reflect changes -> Generate auxiliary docs (e.g., WIKI_UPDATE.md) -> Commit."))
  (language-focus
    (rust "Primary CLI and eBPF development, focus on performance and safety.")
    (elixir "Orchestration, fault tolerance, state management.")
    (idris2 "Core for formal verification, ensuring correctness."))
  (scm-handling-guidance
    (current-agent-limitation "Current agent cannot independently generate/update complex SCM files without explicit spec or tooling.")
    (recommended-action "Provide specific instructions or a generation script for SCM files.")))