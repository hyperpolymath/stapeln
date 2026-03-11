;; SPDX-License-Identifier: PMPL-1.0-or-later
;; echidnabot.scm — Formal verification and fuzzing directives for stapeln
;;
;; Languages present: ReScript, Elixir, Idris2, Zig
(bot-directive
  (bot "echidnabot")
  (scope "formal verification, fuzzing, and dangerous-pattern detection")
  (allow ("analysis" "fuzzing" "proof checks" "pattern scanning"))
  (deny ("write to core modules" "write to bindings"))

  ;; Idris2 ABI layer — CRITICAL
  (idris2-rules
    (ban "believe_me"       severity: "critical" reason: "undermines formal verification")
    (ban "assert_total"     severity: "critical" reason: "bypasses totality checker")
    (ban "assert_smaller"   severity: "critical" reason: "bypasses termination checker")
    (ban "unsafePerformIO"  severity: "critical" reason: "breaks referential transparency")
    (warn "postulate"       severity: "medium"   reason: "unproved assumption; document justification"))

  ;; Zig FFI layer
  (zig-rules
    (warn "@ptrCast"        severity: "medium" reason: "potential type confusion")
    (warn "@intToPtr"       severity: "high"   reason: "raw pointer construction")
    (ban  "unreachable"     severity: "high"   reason: "use @panic with message instead"))

  ;; Elixir backend
  (elixir-rules
    (ban  "Code.eval_string"  severity: "critical" reason: "arbitrary code execution")
    (ban  "Code.eval_quoted"  severity: "critical" reason: "arbitrary code execution")
    (warn "System.cmd"        severity: "medium"   reason: "shell injection risk; validate inputs")
    (warn ":erlang.binary_to_term" severity: "high" reason: "deserialization of untrusted data"))

  ;; ReScript frontend
  (rescript-rules
    (warn "Obj.magic"       severity: "high"   reason: "unsafe type cast")
    (warn "%raw"            severity: "medium" reason: "raw JS escape hatch; review carefully")
    (warn "Js.Unsafe"       severity: "high"   reason: "bypasses type safety"))

  (notes "May open findings; code changes require explicit approval"))
