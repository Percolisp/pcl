---
name: project_perl_suite_survey
description: "Running Perl's own t/ test dirs (base/cmd/comp/re/io/uni/...) through PCL is a top bug finder; survey + runner exist."
metadata: 
  node_type: memory
  type: project
  originSessionId: fbb309c9-0152-4b66-a4f9-16528986a343
---

**Bug-finding method (continue this):** run Perl's distribution core test files
through PCL and diff TAP vs real perl. PCL's `perl-tests/` corpus is ~all
`t/op/`; the *other* dirs (`t/base`, `t/cmd`, `t/comp`, `t/re`, `t/io`, `t/uni`,
`t/mro`, `t/class`) exercise untouched ground and surface many bugs.

- Source tree: `/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t`.
- Runner: `tools/run-perl-suite.pl base/rs.t` (one file) or `--dir comp` (all
  self-contained files in a subdir). Prints `P:perl_ok/notok C:pcl_ok/notok
  STATUS [crash-sig]`.
- **Results doc: `docs/perl-test-suite-survey.md`** — per-file status +
  categorisation. UPDATE it when a row changes so we don't re-triage.

**Surveyed 2026-06-23 (base/cmd/comp).** **2026-06-24 fixed a batch** (commits
e40d9d2, 6e64eb2, b20eda9, a24caed, 559d3be) — all from this survey:
- `comp/term.t` ✅ — `eval "{ 'a','b' }"` literal-key anon hash (PPI bug #5 logged).
- `cmd/mod.t` — `do{}while/until` now POST-test (`p-do-while`/`p-do-until`).
- `comp/opsubs.t` 0→32 — main-pkg global (`$::TODO`) used only in a sub now
  forward-declared (was unbound-abort killing the file).
- `base/lex.t` 1→18 — `$#[0]` = elem 0 of array `@#` (NOT a PPI bug); `@#`
  punctuation name escaped forward-decl → registered via
  `environment->register_punct_global` (see [[feedback_ast_vs_string_matching]] —
  user: collect via data structures, NEVER regex generated CL text).
- `comp/package_block.t` 2→3 — `eval "__PACKAGE__"` inside `package Foo{}` now
  resolves (eval inherits caller's PERL pkg via `eval_pkg`); `package NAME VER`
  sets `$VERSION`.

**Still OPEN fix-targets:** `$/` record sep (`base/rs.t`); `package_block.t`
test 2 (`$VERSION` read BEFORE its `package` stmt = compile-phase ordering);
`base/term.t`/`cmd/mod.t` last fails are fixture deps (relative `harness`/`TEST`
files only in perl's t/ CWD — NOT PCL bugs).

**t/re (2026-06-24): GATED behind Perl's `test.pl` harness + `re_tests` data file
via relative paths.** Runner gives 0/0 = harness miss, not regex bugs. The
unlock = get `test.pl` (2069 lines) to transpile+load (gates much of t/). 3 parse
errors blocked it; **2 FIXED** (conditional `local …=… if COND` for
scalar/array/elem via `_split_local_init_modifier`+`_conditional_local_init`;
loop-modifier `EXPR foreach LIST` in tail-if via `_process_tail_stmt`). **1 TODO:
`system { PROG } LIST` indirect block form** — system/exec are BUILTINS
(`Config.pm`), so they skip the generic-funcall paren handler where I first
(wrongly) hooked it; fix belongs in the builtin list-op arg path (lower `{PROG}`
→ first ordinary arg ≈ `system(PROG,LIST)`). Then reassess test.pl runtime +
CWD/`re_tests`/`charset_tools.pl`/`loc_tools.pl` fixtures. Details in survey doc.

**NOT yet surveyed:** `t/io` (44, [[project_io_tests_and_open_errors]]),
`t/uni` (30), `t/mro` (73), `t/class` (10), rest of `t/op`.

Companion finder: [[project_difftest_fuzzer]] (op axes) + difftest-eval (string
eval). This session's fuzzer/probe fixes: filetest-as-list-op-arg + bare
filetest `$_`; sprintf `%g` integer trailing zeros; general list flattening of
list-valued elements (ranges/arrays) in `()`; `..` range-vs-flipflop in map
blocks and ternary branches.
