---
name: project_cpan_test_suite_strategy
description: Highest-yield bug-finding now = run real pure-Perl core-module .t suites through PCL (no install)
metadata: 
  node_type: memory
  type: project
  originSessionId: 8c98161e-0ee5-4621-b936-c68179b6b9ca
---

Bug-finding has moved past single-expression fuzzing (PCL nails isolated exprs;
`tools/difftest-ops.pl` + ~150 hand probes came up empty except documented
divergences). Remaining bugs are **compositional** (feature interactions) and
**long-tail**. Highest-yield engine now (s262): **run a real pure-Perl module's
own `.t` suite through PCL** — a module's test file is the best compositional
fuzzer ever written, with `is`/`ok` as the oracle.

**No CPAN install needed** — perl core modules ship in the perl SOURCE tree:
`/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/{cpan,dist,ext}/<Mod>/t/*.t`.
Pure-Perl targets on disk: Text-ParseWords, Getopt-Long, Time-Local, Tie-RefHash,
Params-Check, Data-Dumper (dist/, 26 files), JSON-PP (cpan/).
Run via: `./runpcl <path>/cpan/<Mod>/t/<file>.t 2>&1 | tr -d '\0'`. Baseline first:
`perl -I<src>/lib <file>.t`. (`use <Mod>` resolves to the real module on @INC and
PCL transpiles it like user code.)

**Text::ParseWords (first target) found 2 real bugs:**
1. Undef regex captures vanished in `my (...)=(...)` — FIXED s262 (`880ab6e`):
   capture undef is now `*p-undef*` not raw nil (raw nil is dropped by
   `%p-flatten-list` as a hole). See [[project_difftest_fuzzer]].
2. cl-ppcre `:extended-mode` not restored after `(?-x:…)` — FIXED s262: PCL-side
   `/x` normaliser (`%pcl-normalize-extended` in pcl-runtime.lisp) engaged only
   when the pattern has an `x` mode-modifier (plain `/x` stays on cl-ppcre, no
   regression); scanner builds memoized (`*pcl-scanner-cache*`, hand-rolled per
   user). Text::ParseWords 6→26/27 (last = `old_shellwords`, separate niche).
   Writeup `docs/clppcre-extended-mode-modifier-bug.md`; tests
   `Pl/t/regex-extended-mode-01.t`. Pre-existing residual: ws inside `\x{ }` /x.

**Test-file convention (s262, user):** misc-fixes-02.t is too big — put NEW tests
in a NEW `Pl/t/*.t` file (e.g. regex-extended-mode-01.t), not in misc-fixes-02.t.

Two more s262 fixes (both committed): `7%-3` PPI `%-`/`%+` mis-tokenization
(`7a00928`, + `docs/ppi-bug-modulo-magic.md`); runtime warning strings used CL
`"\n"` = a bare `n` (`2f38c97`) — note **PCL treats every program as
`use warnings`** (user-confirmed design: PCL always warns).

Other engines (lower yield, noted): program-level/compositional fuzzer (extend
difftest from random-expr → random nested-program); mine un-adopted perl `t/`
(magic.t, tie.t, overload.t).
