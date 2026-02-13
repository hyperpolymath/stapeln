;; SPDX-License-Identifier: PMPL-1.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for stapeln

(define agentic-config
  `((version . "1.0.0")
    (claude-code
      ((model . "claude-opus-4-6")
       (tools . ("read" "edit" "bash" "grep" "glob"))
       (permissions . "read-all")))
    (patterns
      ((code-review . "thorough")
       (refactoring . "conservative")
       (testing . "comprehensive")))
    (constraints
      ((languages . ("rescript" "elixir" "zig" "idris2"))
       (banned . ("typescript" "go" "python" "makefile"))))))
