(*
  ct-shadow: minimal ATS2 shadow verifier for canonical CTP bytes
  Non-authoritative: intended for CI only.

  Default checks:
    - no CR
    - ends with exactly one LF
    - no TAB
    - no trailing whitespace
    - best-effort key ordering for lines matching: ^(spaces)(key):
  Optional:
    - --check-digest with --summary (uses sha256sum)
    - --check-idempotence with --canon-cmd (runs external canonicalizer)
*)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"
staload "libats/libc/SATS/stdio.sats"
staload "libats/libc/SATS/stdlib.sats"
staload "libats/libc/SATS/string.sats"

typedef str = string

fun
die(msg: str): void = let
  val () = fprintln!(stderr_ref, "ct-shadow: FAIL: ", msg)
in
  exit(1)
end

fun
warn(msg: str): void = fprintln!(stderr_ref, "ct-shadow: WARN: ", msg)

fun
ok(msg: str): void = fprintln!(stdout_ref, "ct-shadow: OK: ", msg)

fun
read_file(path: str): str = let
  val fp = fopen_exn(path, "rb")
  val bs = fileref_get_file_string(fp)
  val () = fclose_exn(fp)
in
  bs
end

fun
has_char(s: str, c: char): bool = let
  val n = string_length(s)
  fun loop(i:int): bool =
    if i >= n then false
    else if string_get_at(s, i) = c then true else loop(i+1)
in
  loop(0)
end

fun
ends_with_single_lf(s: str): bool = let
  val n = string_length(s)
in
  if n < 1 then false
  else if string_get_at(s, n-1) <> '\n' then false
  else if n >= 2 then
    (* allow ...\n but ensure not ...\n\n at end? spec says exactly one final LF *)
    if string_get_at(s, n-2) = '\n' then false else true
  else true
end

fun
check_no_trailing_ws(s: str): void = let
  val n = string_length(s)
  fun loop(i:int, last_non:int, last_char: char): void =
    if i >= n then ()
    else let
      val ch = string_get_at(s, i)
    in
      if ch = '\n' then (
        (* last_char was the char before LF (unless empty line) *)
        if last_char = ' ' then die("trailing whitespace before LF")
        else loop(i+1, i, '\n')
      ) else (
        loop(i+1, i, ch)
      )
    end
in
  if n = 0 then die("empty file")
  else loop(0, ~1, '\n')
end

fun
count_leading_spaces(line: str): int = let
  val n = string_length(line)
  fun loop(i:int): int =
    if i >= n then i
    else if string_get_at(line, i) = ' ' then loop(i+1) else i
in
  loop(0)
end

fun
is_key_char(ch: char): bool =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
  (ch >= '0' && ch <= '9') || ch = '_' || ch = '-' || ch = '.' || ch = '/'

fun
parse_key_colon(line: str): Option_vt@(int, str) = let
  val n = string_length(line)
  val ind = count_leading_spaces(line)
  fun scan_key(i:int): int =
    if i >= n then i else if is_key_char(string_get_at(line,i)) then scan_key(i+1) else i
  val j = scan_key(ind)
in
  if j = ind then None_vt()
  else if j < n && string_get_at(line, j) = ':' then
    let
      val key = string_make_substring(line, ind, j-ind)
    in
      Some_vt@(ind, key)
    end
  else None_vt()
end

fun
split_lines_keep_lf(s: str): List0_vt(str) = let
  val n = string_length(s)
  fun loop(i:int, start:int, acc: List0_vt(str)): List0_vt(str) =
    if i >= n then
      if start < n then list_vt_cons(string_make_substring(s, start, n-start), acc) else acc
    else if string_get_at(s, i) = '\n' then
      let
        val ln = string_make_substring(s, start, (i+1)-start)  (* includes LF *)
      in
        loop(i+1, i+1, list_vt_cons(ln, acc))
      end
    else loop(i+1, start, acc)
  val acc = loop(0, 0, list_vt_nil())
in
  list_vt_reverse(acc)
end

(*
  Best-effort key ordering by indentation level.
  Track last key seen at each indent (spaces count).
*)
typedef frame = @{ind=int, last=str}

fun
lex_leq(a: str, b: str): bool = (strcmp(a,b) <= 0)

fun
check_key_ordering(s: str): void = let
  val lines = split_lines_keep_lf(s)

  fun pop_to(ind: int, st: List0_vt(frame)): List0_vt(frame) =
    case+ st of
    | list_vt_nil() => st
    | list_vt_cons(f, rest) =>
        if f.ind > ind then pop_to(ind, rest) else st

  fun set_last(ind:int, key: str, st: List0_vt(frame)): List0_vt(frame) =
    case+ st of
    | list_vt_nil() => list_vt_cons(@{ind=ind, last=key}, list_vt_nil())
    | list_vt_cons(f, rest) =>
        if f.ind = ind then list_vt_cons(@{ind=ind, last=key}, rest)
        else if f.ind < ind then list_vt_cons(@{ind=ind, last=key}, st) (* push *)
        else set_last(ind, key, rest) (* shouldn't happen if popped, but safe *)

  fun get_last(ind:int, st: List0_vt(frame)): Option_vt(str) =
    case+ st of
    | list_vt_nil() => None_vt()
    | list_vt_cons(f, rest) =>
        if f.ind = ind then Some_vt(f.last) else get_last(ind, rest)

  fun loop(ls: List0_vt(str), st: List0_vt(frame)): void =
    case+ ls of
    | list_vt_nil() => ()
    | list_vt_cons(line, rest) =>
        let
          (* strip trailing LF for analysis; keep indentation *)
          val ln =
            if string_length(line) > 0 && string_get_at(line, string_length(line)-1) = '\n'
            then string_make_substring(line, 0, string_length(line)-1)
            else line
        in
          case+ parse_key_colon(ln) of
          | None_vt() => loop(rest, st)
          | Some_vt@(ind, key) =>
              let
                val st1 = pop_to(ind, st)
              in
                case+ get_last(ind, st1) of
                | None_vt() =>
                    loop(rest, set_last(ind, key, st1))
                | Some_vt(prev) =>
                    if lex_leq(prev, key) then
                      loop(rest, set_last(ind, key, st1))
                    else
                      die("key ordering violated at indent=" + int2string(ind) + ": " + prev + " > " + key)
              end
        end

in
  loop(lines, list_vt_nil())
end

(*
  Minimal JSON digest extraction:
  Looks for "digest_algorithm":"sha256" and "digest_value":"<hex>"
  This is intentionally dumb but stable for CI.
*)
fun
extract_json_string_field(js: str, field: str): Option_vt(str) = let
  val needle = "\"" + field + "\""
  val p = strstr(js, needle)
in
  if iseqz(p) then None_vt()
  else let
    val p2 = strstr(p, ":")
  in
    if iseqz(p2) then None_vt()
    else let
      val p3 = strstr(p2, "\"")
    in
      if iseqz(p3) then None_vt()
      else let
        val p4 = strstr(ptr_succ<char>(p3), "\"")
      in
        if iseqz(p4) then None_vt()
        else let
          val len = g0string_length(p3) - g0string_length(p4)
          (* p3 points at first quote; substring starts at p3+1 of length len-1 *)
          val start = ptr_succ<char>(p3)
          val out = string_make_substring_g0string(start, len-1)
        in
          Some_vt(out)
        end
      end
    end
  end
end

fun
sha256sum_file(path: str): str = let
  val cmd = "sha256sum " + path + " 2>/dev/null"
  val fp = popen_exn(cmd, "r")
  val out = fileref_get_file_string(fp)
  val () = pclose_exn(fp)
  (* sha256sum output: "<hex>  <filename>\n" *)
  val sp = strstr(out, " ")
in
  if iseqz(sp) then out else string_make_substring(out, 0, g0string_length(out) - g0string_length(sp))
end

fun
run_idempotence_check(canon_cmd: str, path: str, original: str): void = let
  (* Run: <canon_cmd> <path> and capture stdout, compare bytes *)
  val cmd = canon_cmd + " " + path + " 2>/dev/null"
  val fp = popen_exn(cmd, "r")
  val out = fileref_get_file_string(fp)
  val () = pclose_exn(fp)
in
  if strcmp(out, original) = 0 then ()
  else die("idempotence failed: external canonicalizer output differs")
end

(* --- CLI --- *)

fun
main0(argc:int, argv: &(@[str]) >> _): void = let
  var path: str = ""
  var summary_path: str = ""
  var check_digest: bool = false
  var check_idem: bool = false
  var canon_cmd: str = ""

  fun arg(i:int): str = argv.[i]

  fun parse(i:int): void =
    if i >= argc then ()
    else if arg(i) = "--summary" then (summary_path := arg(i+1); parse(i+2))
    else if arg(i) = "--check-digest" then (check_digest := true; parse(i+1))
    else if arg(i) = "--check-idempotence" then (check_idem := true; parse(i+1))
    else if arg(i) = "--canon-cmd" then (canon_cmd := arg(i+1); parse(i+2))
    else if string_head(arg(i)) = '-' then die("unknown option: " + arg(i))
    else (path := arg(i); parse(i+1))

  val () = parse(1)
  val () = if path = "" then die("usage: ct-shadow [--summary file.json --check-digest] [--canon-cmd \"ct canonicalize\" --check-idempotence] canonical.ctp") else ()

  val content = read_file(path)

  val () = if has_char(content, '\r') then die("CR detected: canonical bytes must be LF-only") else ()
  val () = if has_char(content, '\t') then die("TAB detected: canonical bytes must not contain TAB") else ()
  val () = if ends_with_single_lf(content) then () else die("file must end with exactly one LF (and not two)")
  val () = check_no_trailing_ws(content)
  val () = check_key_ordering(content)

  val () =
    if check_digest then
      if summary_path = "" then die("--check-digest requires --summary <file.json>")
      else let
        val js = read_file(summary_path)
      in
        case+ extract_json_string_field(js, "digest_algorithm") of
        | None_vt() => die("summary.json missing digest_algorithm")
        | Some_vt(alg) =>
            if alg <> "sha256" then die("unsupported digest_algorithm (expected sha256): " + alg) else ()
        ;
        case+ extract_json_string_field(js, "digest_value") of
        | None_vt() => die("summary.json missing digest_value")
        | Some_vt(expected) =>
            let
              val got = sha256sum_file(path)
            in
              if strcmp(got, expected) = 0 then ()
              else die("digest mismatch: expected=" + expected + " got=" + got)
            end
      end
    else ()

  val () =
    if check_idem then
      if canon_cmd = "" then die("--check-idempotence requires --canon-cmd \"<command>\"")
      else run_idempotence_check(canon_cmd, path, content)
    else ()

  val () = ok("all requested checks passed")
in
  exit(0)
end
