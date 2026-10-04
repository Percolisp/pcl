---
name: project_io_tests_and_open_errors
description: Perl t/io test adoption plan (user wants adopt+curate) + known open/IO error-handling gaps to remember
metadata: 
  node_type: memory
  type: project
  originSessionId: 23a45cf5-6f24-4989-a4b9-9b1d7fcf198c
---

**Perl's authoritative file-IO tests live in the 5.40.3 source tree:**
`/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t/io/` — scalar.t
(in-memory fh, 128 tests), open.t (560 lines), read.t, tell.t, print.t, say.t,
binmode.t, argv.t, … PCL's `perl-tests/` had almost NO IO coverage (only
print.t/readline.t, both from t/op). **Use 5.40.3, not the 5.26.3 build tree
that also exists** (user directive — match the rest of the suite).

**DECISION (session 244): user chose "Adopt + curate into perl-tests/".** NEXT
SESSION: bring t/io/ files in (scalar.t, open.t, read.t, tell.t, print.t),
registry-skip the not-supported parts, fix what's fixable. Watch the
fully-passing guard — do NOT leave uncurated PARTIAL files in the sweep (I
imported io_print.t to mine its bug, then removed it; the bug → Pl/t regression).
Naming: `io_print.t` etc. (perl-tests/print.t is already the t/op one). NB the
`open(foo,">-")` test writes a junk file `perl-tests/t/-` — clean it up.

**OPEN / IO ERROR-HANDLING GAPS to remember (found via t/io/print.t, session 244):**
- `open(FH, ">-")` (and `<-`) — dup STDOUT/STDIN — NOT supported (creates a real
  file named "-"). t/io/print.t test 6.
- **`print`/`printf`/`read` to an UNOPENED/closed/nonexistent filehandle does NOT
  set `$! = EBADF`** — PCL silently writes nowhere (or to stdout) instead of
  failing. t/io/print.t test 19. This is the "open errors" the user flagged: IO
  ops on a bad handle should fail-with-errno, currently they don't.
- `printf "%n"` — NOT supported (security-removed-ish; emits "Redundant argument").
- `open` itself returns false + sets `$!` for ENOENT (that path works via
  %pcl-save-errno); the gap is error detection on USING a bad handle, and the
  exotic open modes (`>-`, `+<` on in-memory, pipes `|-`/`-|`).

**IN-MEMORY FH (shipped 244 commit 88effd4; REWORKED session 245):** `open my
$fh,MODE,\$s` now position-aware. `cl/pcl-runtime.lisp`: `p-string-stream-mixin`
(target box + write offset), `p-string-output-stream` (`>`/`>>`),
`p-string-io-stream` (`+<`/`+>`/`<`, adds read side). DONE: `seek`/`tell`
(stream-file-position; SEEK_END via buffer len; negative offset → seek returns
false not fault), offset-overwrite + NUL zero-fill on forward seek, `+<`/`+>`
read+write, %psos-buf rebuilds the scalar if reassigned mid-write. Also gave
`p-tie-proxy` a non-descending print-object (self-referential tie box was
exhausting the control stack). **t/io/scalar.t ADOPTED → perl-tests/scalar.t:
CRASH@39 → PARTIAL 120/128, 66 pass.** Remaining = not-supported (B introspection,
`pack 'P'`, utf8 byte semantics, read-only enforcement, tie+IO), refaliasing.

**PAREN-PRINT FIX (session 245):** `print($fh "x")`/`printf(STDERR ...)`/
`say({EXPR} ...)` — filehandle inside the parens — silently dropped the write
(parsed FH as first list elem → "Missing case"). Fixed in `Pl/PExpr.pm`
`handle_subcalls` via new `_extract_paren_filehandle`. Regression tests in
`Pl/t/fileio-02.t` (tests 14-15). **Fcntl shim added: `lib/Fcntl.pm`** (SEEK_*/
O_*/LOCK_*/S_I* constants + S_IS* mode helpers) — was undefined-function before.

STILL NOT YET (from the gap list above): `>-`/`<-` dup STDOUT/STDIN; print/read
to a bad/unopened handle setting `$!`=EBADF. See [[project_difftest_fuzzer]].
