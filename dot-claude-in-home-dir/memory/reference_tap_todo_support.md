---
name: reference-tap-todo-support
description: How the PCL test harness honors Test::More $TODO (reads main::$TODO at runtime; do NOT map $TODO in codegen)
metadata: 
  node_type: memory
  type: reference
  originSessionId: b7ca5a5a-8885-46cd-ae5e-2f5977896135
---

PCL's TAP harness honors Test::More `$TODO` (session 230). A test run under
`local $TODO = "reason"` (or `local $::TODO`) is a known-broken-in-Perl test: a
failure is *expected* and must NOT count as a real failure (matches `prove`).

**Mechanism (no codegen change, no variable hijack):**
- `cl/pcl-test.lisp` `%current-todo` reads the dynamic value of the symbol named
  literally `"$TODO"` in package `MAIN` via `(find-symbol "$TODO" (find-package :main))`.
  perl-tests run in `main`, and BOTH bare `local $TODO` and `local $::TODO` resolve to
  `MAIN::|$TODO|` (the test's `our`/`local` defvars + dynamically binds it). Out of the
  TODO extent the symbol holds its defvar'd undef box → `test-undef-p` rejects it.
- `test-ok` checks `%current-todo` **before** the skip-registry: a failing TODO emits
  `not ok N - desc # TODO reason` and does NOT incf `*test-failures*` or write the
  faillog; an unexpected pass emits `ok N … # TODO` and counts as a normal pass.
- `sweep-perl-tests.pl` TAP parser: `not ok … # TODO` → skip (non-fail), `ok … # TODO`
  → pass. `# skip` still takes precedence.

**Rejected alternative — do NOT do this:** mapping `$TODO` → a pcl special via
`%SPECIAL_VARS` (`Pl/ExprToCL.pm`). It would also rewrite lexical `my $TODO` in real
code (the lookup can't tell `my` from `our` at the reference site), breaking general
programs. `$TODO` is a test convention, not a Perl special var — keep it in the harness.

Caveat: only works for `$TODO` in `main`. A test file that switches packages and uses a
bare `local $TODO` there would not be seen — none in the current suite do. 18 perl-tests
files use `$TODO`. See [[feedback-no-simplify-tests]] and `docs/session-log.md` §230.
