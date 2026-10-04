---
name: project-s304-state
description: "s304 detail — E2.1 near-complete, E3 eval-mode shipped, Moo-on-v2 fixed (p-raw-params + strict premerge); ledger of the 5 latent bugs found"
metadata: 
  node_type: memory
  type: project
  originSessionId: a1578bfb-39cf-4dd3-9220-142eb0e4c044
  modified: 2026-07-20T18:36:29.424Z
---

# s304 (2026-07-20) detail ledger

## E2.1 (complete except inline_lambda; 5 byte-parity commits 7b63c8b→)
s///+tr/// leaves (gen_substitution_form/gen_transliteration_form),
Symbol/Magic compounds (gen_symbol_form), \(LIST) family (range-mix still
declines: multiline let + gensym), -bareword/SUPER:: heads, eval funcall
branch — **%FUNCALL_FORM_DECLINES deleted**. Remaining E2 = task #78:
inline_lambda re-host (parse_block_to_cl_string → structured block lowering;
subsumes #65) + 7 empty-shape trailing-space quirks + \(RANGE,…) at E2.final
+ never-firing safety nets (s304 census: zero corpus hits). Full catalog in
plan §E2.1 + s304 session log.

## E3 eval-mode (7981a56, gen v2-45)
Parser2 eval_mode/eval_pkg; _assemble_eval_mode = head/body split + v1's
exact p-eval-thunk shape; pl2cl server/--eval-pkg v2-first with per-eval v1
retry. Retry gates: top-level `package`; trailing my/our decl (value-losing
let); lone bareword ARRAY subscript (out-of-frame constant). fallback_parser
carries eval_mode (CMM lvalue-probe `&sub = 1` dies correctly).
Latent bugs fixed: (1) _blank_string_innards — forward-decl TEXT scan matched
$names inside string literals (embedded eval sources → phantom defvars
proclaimed eval lexicals special, broke closures; eval.t #39; kills #66's
sprintf %x phantom; 61 corpus files pure defvar removals); (2)
p-eval-lex-lookup INSTALLS the autovivified global (cross-eval persistence;
ir-spec §9.1 stop 3); (3) eval state cells tagged `__state__e<md5:8>_N`
(collision with enclosing file's cells whose __init was set; state.t 148/149).

## Task #80: Moo on v2 (gen v2-46) — two GENERAL v2 bugs
1. **Calling convention**: signature fast path's bare `&optional` lambda
   list misbound aggregate args — `f(@args)`/`f(@_)` pass containers RAW
   (uniform convention: callee flattens); vector landed whole in param 1.
   Moo chain: _Utils::_name_coderef → set_subname(@_) → undef →
   _install_coderef installed nothing → `use Moo` = empty class. Fix:
   runtime macro **p-raw-params** (raw unboxed binding, flatten contract,
   no-alloc all-scalar fast path via %p-args-need-flatten); Parser2 emits it.
2. **use strict invisible to ahead-of-stream lowering**: PExpr bareword-
   after-binary-op gate (PExpr.pm ~3590) needs strict_subs; v2 lowered subs
   before the pragma statement fallback → `$module =~ _module_name_rx`
   (glob-installed constant sub) became the STRING → _load_module croaked
   on every extends/with. Fix: **_premerge_strict_pragma** (pattern of
   _premerge_include_prototypes). Annotator stale-stamp (s276 family) healed
   by the same pre-seed.
- Debug hook: **PCL_V1_FILES=<substr,substr>** in Parser2::parse forces
  matching files through v1 (per-module pipeline bisection).
- Guards: Pl/t/moo-01.t (13-tag end-to-end battery); 5 convention shapes in
  transpile-test-04b.t; parser2-01/02 pins → p-raw-params.
- Moo differential battery 17/18 = perl. Residue = task #81: trigger fires
  once with empty value at construction when attr absent (v2 only); plus
  latent `CORE::prototype($code)` self-call in lib/Sub/Util.pm (CORE::
  resolution should hit the builtin).

## CPAN suites on v2 (s304 re-run vs s276b baselines)
Try-Tiny 5/3/3 (=); Role-Tiny 10/7/6 (was 4/6/13, better); Scalar-List-Utils
8/22/8 (was 7/22/9); Sub-Uplevel 2/2/6 (first recording). Suites on disk:
~/.cpanm/work/*/Try-Tiny-0.32, ~/.cpan/build/{Role-Tiny,Sub-Uplevel,
Scalar-List-Utils}-*; survey driver recreated per s304 log (PASS/PARTIAL/FAIL
via runpcl+TAP count).

## Suites post-E3 (all = baseline)
Gate 116/4286 ALL PASS; perl-tests sweep 18386 pass / 66 fully; run-perl-suite
42 OK / 52 XDIFF / 31 NOTAP / 7 TIMEOUT / 301 DIFF (identical OK sets).
