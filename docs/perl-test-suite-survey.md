# Perl core test-suite survey (`t/base`, `t/cmd`, `t/comp`)

**Purpose.** PCL's `perl-tests/` corpus (111 files) is almost entirely `t/op/`.
Perl's own distribution ships many *other* test directories that exercise areas
`t/op/` never touches — and running them through PCL is one of the best bug
finders we have (the framing: *"this is how security people find holes."*).
This file records a survey of the **self-contained** files (no
`require './test.pl'`, no `chdir`) in `t/base`, `t/cmd`, `t/comp`, so we **don't
re-investigate the same files repeatedly**. Update it when a row changes.

> Source tree: `/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t`.
> Re-run a row with: `tools/run-perl-suite.pl <rel>` (see "How to re-run").

**Legend:** ✅ PASS (PCL matches perl ok/notok counts) · 🐞 real fixable bug ·
🚧 known-limitation (documented in `not-supported.md`) · 🟡 partial feature ·
P=perl `ok/notok`, C=PCL `ok/notok` at survey time (2026-06-23).

---

## t/base — basic language (mostly self-contained raw-TAP)

| file | P | C | status | notes |
|------|---|---|--------|-------|
| `cond.t`      | 4/0   | 4/0   | ✅ | conditional operators |
| `if.t`        | 2/0   | 2/0   | ✅ | |
| `num.t`       | 56/0  | 56/0  | ✅ | numeric stringification |
| `pat.t`       | 2/0   | 2/0   | ✅ | |
| `while.t`     | 4/0   | 4/0   | ✅ | |
| `translate.t` | 257/0 | 257/0 | ✅ | `tr///` |
| `term.t`      | 7/0   | 6/0   | ✅* | NOT a PCL bug: test 7 opens a relative `harness` file that only exists in perl's `t/` CWD; the runner runs sbcl from the PCL root, so `open` fails and `die`s. Fixture dependency. |
| `lex.t`       | 120/0 | 18/1  | 🟡 | **CRASH FIXED 2026-06-24**: `$#[0]` (removed `$#` magic + subscript) is element 0 of array `@#` (NOT a PPI bug — verified `@{"#"}=(10,20,30); $#[0]==10`). `@#`'s name isn't a word char, so it escaped forward-declaration → unbound abort killing 119 tests. Codegen registers `@#`/`%#` via `environment->register_punct_global` so a defvar is emitted (undef/empty). Now runs to ~test 18, then a separate `undef-fn:pl-ok` (later `ok()` call) + heredoc gaps cap it. |
| `rs.t`        | 41/0  | 6/35  | 🟡 | `$/` record separator (incl. paragraph mode `$/=""`, fixed-record `$/=\N`, and `*FH` glob passed to a sub). Real I/O feature gap. |

## t/cmd — control flow

| file | P | C | status | notes |
|------|---|---|--------|-------|
| `elsif.t`  | 4/0  | 4/0  | ✅ | |
| `switch.t` | 18/0 | 18/0 | ✅ | (despite the name, no `given/when`) |
| `for.t`    | 16/0 | 11/0 | 🚧 | aborts on `Internals::stack_refcounted` — `Internals::*` is not-supported (`not-supported.md`). |
| `mod.t`    | 15/0 | 14/1 | ✅* | **FIXED 2026-06-24**: `do BLOCK while/until COND` is now a POST-test loop (body runs at least once) via new `p-do-while`/`p-do-until` macros. Remaining 1 fail = test 8 opens a relative `TEST` fixture file that only exists in perl's `t/` CWD (not a PCL bug). |
| `subval.t` | 36/0 | 12/0 | 🟡 | aborts `unbound:$level`. `$level` is a file-lexical `my` mutated by recursive subs; the abort is a closure/recursion capture interaction (cf. fold.t). |

## t/comp — compilation / parsing

| file | P | C | status | notes |
|------|---|---|--------|-------|
| `cmdopt.t`       | 44/0   | 44/0   | ✅ | constant-folding of comparisons |
| `multiline.t`    | 6/0    | 6/0    | ✅ | |
| `term.t`         | 23/0   | 23/0   | ✅ | **FIXED 2026-06-24**: `eval "{ LITERAL , ... }"` anon-hash (string/number key + comma) — PPI mis-tokenized as a bare block (only `=>` triggered its Constructor). New `_bare_block_is_anon_hash` reroute. PPI bug #5 logged. |
| `colon.t`        | 25/0   | 14/11  | 🚧 | almost all fails are `eval "<invalid perl>"` returning undef = **error-detection of invalid Perl** (principle 9 / `not-supported.md`). PCL doesn't reject invalid Perl. |
| `bproto.t`       | 16/0   | 10/6   | 🚧 | `prototype()` introspection — returns undef in PCL (`not-supported.md`). |
| `uproto.t`       | 32/0   | 13/21  | 🚧 | same: prototype introspection + invalid-Perl rejection. |
| `redef.t`        | 20/0   | 1/19   | 🚧 | matches the `"Subroutine X redefined"` / prototype-mismatch **warning text** — error-message wording is not-supported (`not-supported.md`). |
| `our.t`          | 7/0    | 0/7    | 🚧 | built on a `tie`+`AUTOLOAD` `TieAll` harness — tied ARRAY/HASH unwired (`not-supported.md`, tie roadmap). Not actually an `our` bug. |
| `decl.t`         | 9/0    | 3/0    | 🚧 | aborts on `write` (format/`write` not-supported). |
| `form_scope.t`   | 14/0   | 0/0    | 🚧 | `format`/`write` — not-supported. |
| `opsubs.t`       | 36/0   | 32/4   | 🟡 | **CRASH FIXED 2026-06-24**: a main-package global (`$::TODO` → `main::$TODO`) referenced ONLY inside a sub got no forward defvar (the file-scope scan runs at sub_depth==0, and `main` was skipped in the cross-package scan) → unbound-var abort that killed all 36 tests. Now `main`-qualified non-runtime globals are forward-declared. Remaining 4 = `*CORE::GLOBAL::readpipe` override + parse-error **text** for `qx/s/tr/y('unqualified')` (not-supported, principle 9). |
| `fold.t`         | 35/0   | 11/9   | 🟡 | aborts `unbound:$test` — a file-lexical `my $test` interacts with the surrounding `eval q{...}` string-eval blocks (string-eval lexical capture is only partial — `eval-lexical-capture.md`). |
| `package_block.t`| 7/0    | 3/4    | 🟡 | **PARTIAL FIX 2026-06-24**: (a) `eval "__PACKAGE__"` inside a package block now resolves to that package (string-eval now inherits the caller's *Perl* package via `eval_pkg`, not just CL `*package*`); (b) `package NAME VERSION [{}]` now sets `$NAME::VERSION`. Remaining: test 2 = `$VERSION` read BEFORE the `package` stmt (perl sets it at COMPILE time; PCL emits in source order — needs cross-section BEGIN-phase emit); tests 4/7 = "Missing right curly"/`goto` warning **text** detection (not-supported, principle 9); test 6 = `goto LABEL` across package blocks. |
| `utf.t`          | 4216/0 | 12/4204| 🟡 | Unicode source / wide-char identifiers; massive divergence, likely one early cascade. Unicode-semantics bucket (`not-supported.md`). |

---

## What the survey teaches (recurring buckets)

Most non-✅ rows fall into already-documented buckets — **do not re-triage these**:

- **Error/warning *text* detection** (`redef.t`, parts of `colon.t`) — PCL
  doesn't reproduce Perl's exact diagnostic wording. `not-supported.md`.
- **Invalid-Perl rejection** (`colon.t`, `uproto.t`) — `eval "<bad perl>"` is
  expected to return undef; PCL accepts valid Perl only (principle 9).
- **`prototype()` introspection** (`bproto.t`, `uproto.t`) — returns undef.
- **`format`/`write`** (`decl.t`, `form_scope.t`) — not-supported.
- **`Internals::*`** (`cmd/for.t`) — not-supported.
- **`tie` ARRAY/HASH** (`our.t`) — unwired; tie roadmap.
- **Unicode semantics** (`utf.t`) — not-supported bucket.

### Genuinely real / fixable (fix targets — NOT yet done)

1. **Crash-aborts that kill a whole file.** When PCL hits an unbound var
   (`@#`, `$TODO`) or an undefined sub (`Internals::*`) at top level, the SBCL
   process aborts and every remaining test in the file is lost. Perl would
   either warn-and-continue (non-strict undef) or `die` into an `eval`. Making
   these *catchable Perl-level errors* (or undef for non-strict undeclared
   globals) would recover dozens of otherwise-passing tests per file.
   *Note:* whether an undeclared var is an error at all **depends on `strict`** —
   under `use strict 'vars'` it is a compile error; without it, it is a package
   global defaulting to undef. PCL targets valid Perl, so the non-strict path
   (undef, no crash) is the one to get right.
2. **`$/` record separator** (`rs.t`) — paragraph mode, fixed-record mode.
3. **`$#[0]` / `$#`-lexing edge** (`lex.t`) — niche, but the crash-abort is the
   real damage; treating an unknown `@#` as empty avoids it.
4. **`package NAME { }` block scoping** (`package_block.t`) — 5 fails.
5. **`comp/term.t` (3) / `cmd/mod.t` (2) / `base/term.t` (1)** — small crash/fail
   counts, individually investigable.

### t/re — gated behind Perl's `test.pl` harness (2026-06-24)

Almost all of `t/re` either `do './re/regexp.t'` (which reads the `re_tests`
data file) or `require './test.pl'` — i.e. they need Perl's test harness +
fixtures at a specific CWD, so the runner's 0/0 is a harness miss, NOT regex
bugs.  The real unlock is getting `test.pl` (2069 lines) to transpile+load,
which gates a huge swath of Perl's suite.  It nearly transpiles — **3 parse
errors** blocked it; **2 fixed 2026-06-24**, 1 left:

1. ✅ `local $h{k}=V if COND` / `local $x=V if COND` — conditional `local`
   (scalar/array/hash-elem).  The `if`/`unless` modifier leaked into the RHS
   parse.  Fixed via `_split_local_init_modifier` + `_conditional_local_init`
   (value = `COND ? RHS : current`; the glob form already had `p-local-glob-if`).
2. ✅ `EXPR foreach LIST` in TAIL position of an `if` block — the if-return
   transform tried to wrap the loop in `(setf ret_var …)`.  Fixed in
   `_process_tail_stmt`: emit the loop, set ret_var "".
3. ✅ `system { PROG } LIST` / `system({ PROG } argv…)` — the indirect block
   form (2026-06-25).  Both shapes are now lowered in `handle_subcalls` to the
   ordinary list form `system(PROG, LIST)` (argv[0]-override nuance dropped,
   acceptable).  Two parse paths: the **bare** `WORD { Block } LIST` branch and
   the **paren** `WORD ( Block|Constructor LIST )` branch — note PPI tokenises
   `{ PROG }` inside parens as a `PPI::Structure::Constructor` (anon-hash), not a
   `Block`, so the paren branch accepts either when it opens with `{`.  Regression
   guard: `Pl/t/system-block-01.t`.  **`test.pl` now transpiles with 0 parse
   errors** (`./pl2cl < t/test.pl`).

#### test.pl now LOADS and works as a harness (2026-06-25)

`require './test.pl'` transpiles+loads end-to-end and provides a working harness
(`plan`/`ok`/`is`/`like`/`done_testing`/… all defined; verified a 3-test probe
matches perl exactly).  Run a t/re file with **CWD = perl's `t/` dir** so the
`require './test.pl'` resolves:

```bash
T=/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t
./pl2cl "$T/re/pos.t" > /tmp/x.lisp
( cd "$T" && sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load /tmp/x.lisp )
```

One **compile-time** ERROR remains inside test.pl: `pl-watchdog` has a forward
`goto WATCHDOG_VIA_ALARM` that is NOT wrapped in a `(tagbody …)` →
"attempt to GO to nonexistent tag". This is **intra-sub-goto Blocker A**
(`docs/intra-sub-goto-plan.md`: 2-pass wrap result discarded). SBCL still defines
the function and loads the rest, and `watchdog()` is only called on timeout — so
**the harness is fully usable despite it**. Fixing it = the parked goto work.

#### t/re landscape (first pass, 2026-06-25) — bugs found via the loaded harness

- 🐞 **FIXED**: `pos.t` — failed `/g` match must reset `pos()` to undef; the
  undef must survive list flattening (was raw CL nil → dropped from `@_`). Fix:
  `p-pos` returns `*p-undef*`. Guard `Pl/t/pos-01.t`. (Also exposed that a raw
  CL nil scalar anywhere in a flattened Perl list silently vanishes — `p-pos`
  was the instance; watch for other builtins returning bare nil for undef.)
- ✅ already pass: `reg_eval.t` 8/8, `rt122747.t` 3/3, `pat_special_cc.t` 9/9,
  `qrstack.t` 1/1.
- 🚧 **`/n` no-capture modifier** → **documented not-supported** (`not-supported.md`).
  `reg_nocapture.t` (16/25), parts of `rxcode.t`. Looked like a clean pattern
  pre-pass but isn't: named captures `(?<a>…)` stay capturing under `/n`, the
  scoped `(?n:)`/`(?-n:)` overrides need a modifier-scope stack, and `qr/(x)/n`
  must stringify as `(?^n:(x))` (original preserved) while *matching* skips the
  capture — i.e. a match-time-only rewrite. Sizeable + rare → deferred.
- 🟡 partial (regex-semantics divergences, triage individually): `reg_pmod.t`
  59/88, `reg_posixcc.t` 1544/2560, `script_run.t` 71/185, `rxcode.t` 25/42,
  `qr.t` 3/4, `reg_60508.t` 0/1.
- 🚧 not-supported buckets: `qr-72922.t` crash = `weaken`/`Internals::SvREFCNT`
  refcount introspection; `subst.t` crash = `*Config::{NAME}` glob-slot on a
  stash snapshot (`p-glob-slot` got a hash-table, wants a typeglob) — both the
  stash/typeglob + Internals not-supported areas. The whole-file crash is the
  damage; a catch/no-op would recover the non-Internals tests.
- ⏭️ 0/0 = **legitimate `1..0 # Skip`, NOT bugs** (verified): `opt.t`/
  `recompile.t`/`anyof.t` need the `re` module (re::optimization/debug introspection);
  `reg_fold.t` needs `File::Spec`; `reg_nc_tie.t` needs `Tie::Hash::NamedCapture`.
  PCL behaves like miniperl (no dynamic loading) and skips, matching perl's own
  miniperl path. Don't chase these — they require XS/`re`-introspection modules.

NEXT t/re: triage the partials (`reg_pmod` 59/88, `reg_posixcc` 1544/2560,
`script_run` 71/185, `rxcode` 25/42) for isolable regex-semantics bugs. (`/n` is
now documented not-supported, see above.) The `re_tests`-driven `regexp*.t`
family still needs the `re_tests` fixture wired (separate effort).

## t/opbasic — minimal-dependency op tests (surveyed 2026-06-27)

`t/opbasic` holds op tests written to avoid the full harness. All use
`test.pl`, so run with CWD = perl's `t/`.

| file | P | C | status |
|------|---|---|--------|
| `arith.t` | 183 | 183/0 | ✅ |
| `cmp.t` | 12078 | 12078/0 | ✅ |
| `qq.t` | 30 | 30/0 | ✅ |
| `concat.t` | 254 | **247/7** | 🐞→🟡 (2026-06-27): two real bugs fixed; remaining 7 are not-supported buckets |
| `magic_phase.t` | 7 | 0/7 | 🚧 `${^GLOBAL_PHASE}` phase tracking + CHECK/INIT phasers + DESTROY-via-GC — not-supported bucket |

**`concat.t` 31→247 (2026-06-27).** Two genuinely fixable bugs:
- 🐞→✅ **`CORE::<declarator>` in expression context crashed.**
  `ref(CORE::state $y = …)` parsed `CORE::state` as a function call → undefined
  `pl-state`/`p-UNDEFINED` abort that killed ~215 remaining tests. Fix:
  `extract_declarations` (`Pl/PExpr.pm`) now accepts an optional `CORE::` prefix
  on `my`/`our`/`state`/`local` (PCL has no overridable builtins, so
  `CORE::<declarator>` == the bare declarator). Guard:
  `Pl/t/transpile-test-03.t`.
- 🐞→✅ **Compound-assign operators double-evaluated their place.**
  `(($a .= $a) .= $a) .= $a` gave 128 a's instead of 8: every compound-assign
  macro (`p-.=`, `p-*=`, `p-incf`, `p-str-x=`, …) textually expanded `place`
  2+ times, so a nested lvalue chain (each `.=` returns its LHS as an lvalue)
  re-ran the inner assignment exponentially. Fixed at the shared point with a new
  `%store-back-form` helper (`cl/pcl-runtime.lisp`) that binds a box place to a
  temp once; all compound-assign macros now route through it. Guard:
  `Pl/t/transpile-test-03.t`.

Remaining 7 `concat.t` fails are not-supported / niche: `use bytes` (23–24),
overloaded `.` fired by string interpolation (34), deref-assign through a `qr//`
ref (236), `@_` element aliasing / PVLV (237), typeglob string concat-alias
(239), tie arg-evaluation order (253).

## t/op extras (not in `perl-tests/`) — survey continued 2026-06-28

**Runner fix (2026-06-28):** `tools/run-perl-suite.pl` now runs sbcl with
CWD = perl's `t/` dir (it previously didn't `cd`, so every `test.pl`-dependent
file reported a *false* `crash:simple-error`). Re-run any earlier op row.

Batch-surveyed ~25 `t/op` files via the fixed runner. Most divergences are
already-documented not-supported buckets — **do not re-triage**:
- warning-*emission/text* detection (`numify.t` all 12 fails = "Argument isn't
  numeric"; `protowarn.t`; `redef.t`), invalid-Perl rejection (`my.t` 51-57 =
  `my $x if 0`; `mydef.t` = `my $_`), `@_`/element aliasing + SV-identity
  (`repeat.t` 46-47, `delete.t` 26/54/56), tie (`repeat.t` 43), prototype
  introspection (`gv.t`), `use B`/XS (`svflags.t`), `@INC` coderef hooks
  (`do.t` test at line 238 → `do '/eval_do'` via a sub in `@INC`).

| file | P | C | status |
|------|---|---|--------|
| `context.t` | 8 | 8/0 | ✅ |
| `defined.t` | 5 | 5/0 | ✅ |
| `dor.t` | 34 | 32/2 | 🚧 2 fails = "Search pattern not terminated" error-text |
| `numify.t` | 32 | 4/12 | 🚧 all 12 = "isn't numeric" warning-emission |
| `my.t` | 59 | 52/7 | 🚧 7 = `my $x if 0` invalid-Perl rejection |
| `mydef.t` | 2 | 0/2 | 🚧 `my $_` invalid-Perl rejection |
| `repeat.t` | 50 | 47/3 | 🚧 tie / @_ alias / lvalue-x aliasing |
| `delete.t` | 56 | 53/3 | 🚧 SV-identity + element-free (GC) |
| `each_array.t` | 65 | 64/1 | 🟡 test 24 = cross-`require` `$$` proto context (see below) |
| `do.t` | 71 | 54+crash | 🚧 `@INC` coderef hook returning a filehandle |
| `svflags.t` | 16 | crash | 🚧 `use B` (XS) |
| `ref.t` | 257 | 172/65 | 🟡 65 = deref-error-text + glob FORMAT/IO slots + REGEXP-ref edge + DESTROY (all not-supported) |
| `localref.t` | 64 | 63/1 | 🟡 was 30/34; FIXED `local` on symbolic deref (below); last fail = DESTROY-during-restore (GC, not-supp) |
| `substr.t` | 400 | 375/24 | 🟡 was TRANSPILE-FAIL (0/0); unblocked by `use utf8` source-decode (below) |
| `tiehash.t` | 27 | 13/8 | 🐞 `unbound:$count__lex__1` = closure-across-package bug (below); rest = TIEHASH unwired |
| `each.t` | 65 | 4/crash | 🚧 `require Hash::Util` (XS, not-supp) aborts whole file at test 4 |
| `pos.t` | 33 | 15/15 | 🚧 defelem aliasing + DESTROY + byte/UTF8 + `pos refuses @/%` error-detect (all not-supp) |
| `vec.t` | 78 | 73/5 | 🟡 5 = vec-lvalue error-text (RT131083) + DESTROY (all not-supp) |
| `universal.t` | 142 | 2→59/53 | 🐞→🟡 (2026-06-30): five general fixes below took it from a 2-test crash to 59; rest = `keys %UNIVERSAL::` stash-walk + isa-on-exotic-ref undef-vs-"" + `fresh_perl_is` (runperl) |
| `glob.t` | 18 | 6→16/1 | 🐞→🟡 (2026-06-30): was a `local %Pkg::` crash (6); fixes below recover 16. test 6 (`@x=glob` then `@x=glob` same pattern) = glob iterator keyed by pattern-string not call-site (returns empty 2nd time) — pre-existing limitation; 13/16-18 = File::Glob `~`/CORE::GLOBAL override (not-supp) |
| `print.t`(op) | 3 | 3/0 | ✅ |

**🐞→✅ Four general method-dispatch / parse fixes (2026-06-30, via `op/universal.t`).**
1. **Paren method-call invocant in LIST context wrapped in `(vector …)`.**
   `(EXPR)->method` where the whole call is an argument to another sub (LIST
   context) generated `(p-method-call (vector OBJ) "method" …)` → "Can't call
   method on unblessed reference".  Fix: `gen_methodcall` (`Pl/ExprToCL.pm`) now
   routes a paren-scalar-base invocant through `_gen_scalar_deref_base` (SCALAR
   ctx), reusing the same helper the `$x->[i]`/`$x->{k}` arrow-deref bases use.
   This is the high-leverage one — affects any `(…)->m` inside a list.
2. **`main::Foo` didn't resolve as a package.** `"main::Alice"->new` failed —
   `%pcl-find-package` (`cl/pcl-runtime.lisp`) treated `main::Foo` literally; it
   now retries with the root-stash `main::` prefix stripped (`Foo`).
3. **`isa("Foo")` on an object blessed into `"main::Foo"` returned false.**
   `p-isa` compares class names; now normalises a leading `main::` on both
   sides (`%pcl-normalize-pkg`).
4. **`is Qualified::name(ARGS)` mis-parsed as indirect object.**
   `is UNIVERSAL::isa($x,$y)` became `UNIVERSAL::isa->is(…)`.  The general
   indirect-object pre-pass (`Pl/PExpr.pm`) now skips a qualified-name (`Foo::bar`)
   invocant that is immediately followed by parens — that's a function call, not
   a class.  (`new Foo::Bar(…)` stays handled by the dedicated `new` pre-pass.)

   Guards: `Pl/t/transpile-test-03.t` (3 tests).  Remaining universal.t fails =
   `keys %UNIVERSAL::` stash-walk, isa-on-exotic-refs (LVALUE/GLOB) undef-vs-"",
   and `fresh_perl_is` (test.pl `runperl` spawns a real `./perl`) — all
   not-supported / fixture buckets.

**🐞→✅ `my(LIST)` as a list-operator funcall argument (2026-06-30).**
`f my($y), LIST` mis-parsed as `f($y)` with LIST orphaned — after `extract_declarations`
strips the `my`, the leftover `($y)` Structure::List was mistaken for the call's
own argument parens.  Now decl-lists are tagged `_pcl_decl_list` and the
funcall-paren detector in `handle_subcalls` skips them, so the bare list-operator
pass grabs `f($y, LIST)`.  This also fixed `tie my($x), "Class"` (was a `p-tie`-gets-1-arg
crash; bare `tie my $x` already worked) → unblocked the universal.t tie block
(55→59) and catch.t no longer crashes.  Guard: `Pl/t/transpile-test-03.t`.

**🐞→✅ Two glob fixes + whole-stash-local crash (2026-06-30, via `op/glob.t`).**
- **`local %Pkg::` (whole-stash localize) crashed the compile.** `_transform_pkg_var`
  renders `%File::Glob::` as a `(p-stash …)` form, which `_process_local_declaration`
  then used as a `let`-binding place → invalid CL → SBCL compile abort killing the
  file.  Now whole-stash locals (`(p-stash …)` vars) are dropped (stash
  localization is not supported) and the body runs unshadowed.
- **`glob "dir/*"` did not match files with extensions.** `p-glob--expand` built
  a CL wild pathname and called `(directory …)`; CL splits `aaa.txt` into
  name=`aaa`/type=`txt`, and a glob `*` parses to name=`:wild`/type=NIL — which
  matches only *extensionless* names.  So `glob("*")` returned 0 against any dir
  of dotted files.  Rewrote `p-glob--expand` to enumerate the (fixed) directory
  via `:name :wild :type :wild` and filter leaf names with a glob→regex
  (`%p-glob-component-regex`), honouring Perl's leading-dot rule and sorting
  results.  Wildcards in the *directory* portion fall back to the old pathname
  path.
- **Bare `glob` (no arg) now defaults to `$_`** — added `glob => [1,-2]` to
  `known_no_of_params` (`Pl/PExpr/Config.pm`); it was absent, so bare `glob`
  parsed as the bareword string `"glob"`.
  Guards: `Pl/t/glob-01.t` (+2).

**🐞→✅ Lower/mixed-case bareword filehandle after print/say (2026-06-30).**
`open(foo,…); print foo LIST` parse-errored — the print/say filehandle detector
(`Pl/PExpr.pm`) only recognised ALL-CAPS barewords; now also accepts any-case
barewords already registered as a filehandle via `open`.  `gen_filehandle`
(`Pl/ExprToCL.pm`) quotes any bareword identifier (not just all-caps) in fh
position.  `io/print.t` 22→23, `io/say.t` 8→13 (full).  Guard:
`Pl/t/fileio-02.t` test 31.

**🐞 OPEN — closure capturing a `my` across a PACKAGE boundary (found 2026-06-28,
`tiehash.t`).** A `my $x` whose closure lives in a *different* package fails two
ways: `my $x; package Foo { sub f { $x } }` recognises the capture (renames to
`$x__lex__N`) but the top-level `package {…}` block emits via **section-switch**
(`_open_section` in `_process_package_statement`) — each section is a separate
top-level CL chunk — which closes the enclosing closure-`let` early, so code
after the block references the var out of scope (`unbound`). The *statement*
form `my $x; package Foo; sub f { $x }` is worse: the capture isn't recognised
at all and `$x` resolves to the package var `Foo::$x` (also unbound). Root cause
is an architectural conflict: a closure-`let` must span the package block as one
lexical form, but top-level package blocks are section boundaries. Real fix is
substantial + risky (touches the bucket/section model + closure capture);
deferred — see `docs/closure-lexical-scoping.md`. Min repro: `{ my $count=0;
package Foo { sub bump { ++$count } } Foo::bump(); print $count }`.

**🐞→✅ `use utf8` now decodes the SOURCE as UTF-8 (2026-06-28).** PCL read the
source as raw bytes and ignored `use utf8`, so `use utf8; length("café")` gave
`5` (bytes) not `4` (chars), and any UTF-8 *identifier* (`my $café`, `*ワルド`)
parse-errored (PPI saw byte fragments). Fix: `_maybe_decode_utf8` in
`Pl/Parser.pm` runs `utf8::decode` on the raw source when `use utf8` is present
(and the string is still bytes) — before `_preprocess_source`/PPI. The pl2cl
output is already written UTF-8, so the wide chars round-trip into the generated
CL. Without the pragma, high bytes stay Latin-1 (byte semantics, matches Perl).
Unblocked `op/substr.t` (0→375/400; remaining = `*MAIN::` vs `*main::` glob-pkg
upcasing + test.pl watchdog fork). Guard: `Pl/t/utf8-source-01.t`.

**🐞→✅ `local` on a deref / symbolic ref (`local ${$x}`, `$$x`, `@{$x}`, `%$x`, …).**
These forms (`Cast($/@/%)` + `Block`/`Symbol`) were **silently dropped** — the
`_process_local_declaration` fall-through only recognised `Symbol`/`List`, so
`@vars` was empty and the localize+restore never emitted (the assignment leaked
out of scope). Fix: a new branch in `Pl/Parser.pm` emits
`(p-local-deref-{scalar,array,hash} REF …body)`; the macros (`cl/pcl-runtime.lisp`)
resolve REF **at run time** — a *symbolic* (string) ref resolves the package
variable and save/restores it, a *hard* reference is the fatal Perl error
`"Can't localize through a reference"` (so `eval { local $$hardref }` sets `$@`
matching the test regex). Scalar restore mutates the box in place, so it must
clear the nv/sv caches on both clear and restore (else the box reads back its
stale cached value). Guard: `Pl/t/local-symref-01.t`. `op/localref.t` 30→63/64.

**🐞→✅ scalar `($)` prototype now imposes SCALAR context (e51b6fa).** A user
sub with an old-style `$` prototype slot evaluates that argument in scalar
context: `takes(@a)` yields the count, `one(keys %h)` the count, `is2(each
@h,0)` the index — instead of flattening. Fix in `Pl/ExprToCL.pm` gen_funcall
arg loop: `$` slot sets SCALAR_CTX on the node before gen_node (handles
wantarray-sensitive `each`) and wraps non-obviously-scalar args (aggregates,
`keys`/`values`) in `(p-scalar …)` (skips number/string/`$`-symbol literals to
keep codegen clean). Guards: `transpile-test-03.t`; `bop-01.t` 1,3 exercise it.
**Only fires for same-compilation-unit prototypes** — cross-`require`
prototypes (perl's `test.pl` `is ($$@)`) aren't known at transpile time, so
`each_array.t` test 24 still fails. Making `require`d prototypes visible at
compile time is the remaining (larger) gap.

**Crucial guard (else it breaks `test.pl`):** the scalar imposition only fires
when the call supplies **≥ min_params** syntactic args. A call with fewer args
than mandatory slots means an array is *flattening* to fill them — Perl does
NOT scalarize it. The canonical case is `sub like ($$@) { like_yn(0, @_) }`
where `like_yn ($$$@)` has 3 mandatory slots but the call passes 2 args; `@_`
must spread, not collapse to its count. Without the guard, `@_` became
`(p-scalar @_)` = 3 and every `test.pl`-based file crashed in `like()`. (Found
because a *stale `~/.pcl-cache` entry* for the transpiled `test.pl` masked the
fix until cleared — when codegen changes affect a `require`d module, clear
`~/.pcl-cache`.)

## t/op extras (not in `perl-tests/`) — surveyed 2026-06-27

`op/lex_assign.t` (353 perl ok) reached **348** after two fixes; remaining 5 are
not-supported buckets (DESTROY-via-GC reassignment, `select`/`utime` arg-count
error-message detection, `getpriority` — a niche unimplemented POSIX builtin).
Two genuinely fixable bugs found here:
- 🐞→✅ **`$^T` (BASETIME) was unbound** → `localtime $^T` / `gmtime $^T` aborted.
  Added the special var: `|$^T|` defvar (program start, Unix seconds) in
  `cl/pcl-runtime.lisp` + `:export`, and `'$^T'` in `%SPECIAL_VARS`
  (`Pl/ExprToCL.pm`). Used by the `-M`/`-A`/`-C` file-test operators.
- 🐞→✅ **`\u\L` / `\l\U` case-escape composition was inverted.** `"\u\L$a"`
  (and `"\L\u$a"`) gave `lc(ucfirst($a))` = "ab" instead of `ucfirst(lc($a))`
  = "Ab": the one-shot `\u`/`\l` leaked *inside* the `\L`/`\U` span and wrapped
  the first element, so the span's `lc`/`uc` then overrode it. Fix
  (`Pl/PExpr/StringInterpolation.pm`): a `\u`/`\l` that opens before a span, or
  at the very start of one, is stashed as the span's `outer_char` and applied to
  the span's OUTPUT at close. Guard: `Pl/t/case-invert-01.t`. (Known remaining
  gap: a `\u` *mid*-span, `"\Lfoo\uBar"`, still lowercases the protected char —
  needs per-character span handling; rare.)

## t/io — I/O (started 2026-06-25; run with CWD = perl's `t/`)

All 44 files use `test.pl` (+ `chdir 't'`, a no-op when already in `t/`), so the
loaded harness runs them. First probe (P = perl ok/notok, C = PCL):

| file | P | C | status |
|------|---|---|--------|
| `defout.t` | 22 | **21/1** | 🐞→✅ FIXED (this session): see below; last fail `$-` is format-dependent |
| `print.t` | 24 | 21/1 | 🟡 close (3 diffs) |
| `say.t` | 13 | 8/1 | 🟡 |
| `tell.t` | 36 | **35/1** | 🐞→🟡 (2026-07-01): cross-require proto path fix (below) read `<TST>` in scalar ctx again; only test 29 (coercible glob) left. (2026-06-26): old-style symbolic filehandles. (1) `open($s,...)` where `$s` holds a NAME ("TST") now opens the *named* glob (`*TST`) instead of autovivifying a lexical into `$s` — both `<TST>`/`eof(TST)` and `<$TST>` reach it. (2) `%p-fh-arg` mis-cased the FH name recovered from a funcall-wrapped bareword (`eof(TST)`→`(pl-TST)`): "TST" vs readline's "tst" — fixed with a final `%pcl-invert-case` so it matches the direct bareword symbol. (3) argument-less `eof` now tests `*p-last-read-handle*` (last FH read) not STDIN. Remaining 4: per-handle `$.` line-number magic + `tell FH` setting the current handle (15/19/21), coercible-glob (29). |
| `read.t` | 2 | **2/2** | 🐞→✅ FIXED (2026-06-25b): `read()` was fundamentally broken (ignored its BUF, returned the read STRING not the count); now fills BUF in place, returns the count, NUL-pads at a positive OFFSET, EBADF on unopened handle |
| `errno.t` | 16 | n/a | 🚧 uses `runperl` (spawns a real `./perl` subprocess) — fixture dependency, not a PCL semantics gap |
| `paragraph_mode.t` | 80 | 16 | 🐞 `$/=""` paragraph mode (same `$/` gap as `base/rs.t` — convergent) |
| `binmode.t` | 9 | **9/9** | 🐞→✅ FIXED (2026-06-25b): test 9 (last fail) was the `$!` dualvar-through-`@_` bug — now fixed (see below) |
| `scalar.t` | 128 | **93/35** | 🐞→🟡 (2026-07-01): +4 from the symbolic-fh + cross-require-proto fixes below. (2026-06-25e): UNBLOCKED, was `skip_all` because `is_miniperl()` reported true. Fix: mark the runtime DynaLoader XS boot stubs (`pl-boot_DynaLoader` etc.) as `:defined` in `*p-declared-subs*` so `defined &DynaLoader::boot_DynaLoader` is true — PCL is a full perl, not miniperl. In-memory `open(\$scalar)` read/write/append/tell all work. Remaining 39 = not-supported buckets: tie/magic backing scalars, fd-dup (`+<&`/`>&=`), wide-char (>0xff) open-should-fail + errno/warning detection, scalar SV-identity (ref-to-ref, numeric/overload write), live external-modification of the backing SV. |
| `iprefix.t` | 2 | 0/2 | 🟡 |
| `fs.t` | 61 | **53/61** | 🐞→🟡 (2026-06-26): was 0 (crash on `umask`). Added builtins `umask`/`link`/`symlink`/`readlink`/`chown`/`utime` (Config.pm + RUNTIME_NAMES + sb-posix impls); rewrote `p-stat`/`p-lstat` to use real `sb-posix:stat` fields (Unix-epoch atime/mtime, real inode/mode/nlink — was CL `file-write-date`, off by 2208988800; `stat-blksize`/`stat-blocks` absent in SBCL so derived); implemented `p-truncate` (path + ftruncate(fd), was a warn-stub) as a macro quoting the FH arg; `chmod`/`chown` now fchmod/fchown a filehandle in the LIST; Config `d_fchmod`/`d_fchown` = define. Remaining 4: `futimes` (no sb-posix:futimes → error-text, principle 9), `*FH{IO}` glob-IO-slot truncate (51/52, needs glob↔*p-filehandles* wiring), `truncate BAREWORD` parser stringifies the bareword to a filename (53). |
| `argv.t` | 53 | runperl | 🚧 unblocked the `File::Spec->devnull` crash (added `devnull`/`tmpdir` to the shim) but the tests spawn child perl via `runperl` — fixture dependency. |

**Three t/io fixes (2026-07-01, commit `87d6d3f`).**
1. **Symbolic in-memory filehandle.** `$TST = "TST"; open($TST, "<", \$data)`
   short-circuited into the in-memory branch and autovivified a lexical handle
   *into the box*, so `eof(TST)` (bareword) and `eof($TST)` (scalar holding the
   name) saw different handles. Perl opens the *named* glob (*TST) and leaves
   $TST holding the string. Unified the symbolic-handle detection into
   `%p-install-fh` (was duplicated inline only in the *file* branch of
   `%p-open-impl`) so the in-memory path gets it too.
2. **Closed-stream `eof()` crash.** After `close TRY`, `*p-last-read-handle*`
   still pointed at the closed stream, so a later no-arg `eof()` did `peek-char`
   on it → `sb-int:closed-stream-error` abort (killed `io/argv.t` at test ~9).
   `%p-eof-impl` now guards with `open-stream-p`; a closed stream reads as EOF
   (Perl's eof-on-closed is true). `argv.t` crash moved to test 27 (`close ARGV
   or die`, diamond-ARGV, separate feature). Most of argv.t is `runperl` anyway.
3. **Cross-require prototype path (HIGH-LEVERAGE).** `_extract_file_prototypes`
   only searched cwd + the source file's own dir, so Perl's ubiquitous
   `chdir 't'; require './test.pl'` idiom missed the harness prototypes when the
   source is a *subdir* file (`t/io/scalar.t`) and `test.pl` lives in the
   grandparent `t/`. Without `is($$@)`'s prototype, `is(<$fh>, ...)` read the
   handle in LIST context (all lines) → wrong value / EOF position. Now walks up
   to 8 ancestor dirs for the (leading-`./`-stripped) relative path. This helps
   **every** subdir harness file across the survey (t/op, t/io, t/re, …) where a
   context-sensitive arg (readline/keys/each) is passed to a `$`-proto assertion.
   Guard: `Pl/t/fileio-02.t` test 32.

**`defout.t` fixed → 21/22** (commit this session). The hooks were a parse error
+ two crashes, all from `format`/`write` (not-supported) but mis-handled:
- `format NAME = … .` is now stripped in `_preprocess_source` (PPI swallowed the
  next statement → unknown-`.`-operator PARSE ERROR). Generic: any file with a
  stray format block now parses cleanly.
- `write()` → no-op `p-write` stub (was undefined-function crash).
- `close()` (no args) → `p-close` `&optional` success no-op (was an arg-count
  macroexpand crash).
- `select()` → returns `"main::STDOUT"` not raw nil (raw nil drops from a
  flattened list — **same class as the `pos()` fix**; the recurring lesson:
  builtins must return `*p-undef*`/a real value for undef, never bare CL nil).
- `$~`/`$^` format-var defaults set to `STDOUT`/`STDOUT_TOP`.
- Guard `Pl/t/format-skip-01.t`. Remaining fail (test 7 `$-`) is real format
  dependence: perl runs `write()` which sets lines-left; PCL can't.

**`binmode.t` fixed → 8/9** (commit this session): aborted at test 2 on
`find PerlIO::Layer 'perlio'` (indirect method call on a core package PCL didn't
ship → uncaught "Can't locate object method"). Shipped `lib/PerlIO/Layer.pm`
(picked up by the existing method-call auto-require, `p-method-call`→`p-require`
on an unknown class); `find` reports the standard core layer names as known.
Also `binmode` on an unopened handle now fails with errno EBADF. Guard
`Pl/t/binmode-01.t`. Last fail (test 9) is a **`$!` dualvar bug**: `$!` survives
a plain copy and string-eval but loses its numeric side when passed through
`@_` (verified: `sub f{my($g)=@_; $g==9}` fed `$!` fails) — fix it as part of
errno.t.

**`$!`/dualvar-through-aggregate FIXED (2026-06-25b).** The `binmode.t` test-9
lead was a general dualvar bug, not errno-specific: a dualvar box ($! errno or
`Scalar::Util::dualvar`) carries an independent numeric value alongside a string
value, but *every* place that unboxes a box to a scalar *value* for storage in an
array/hash/`@_` dropped the numeric half (unboxed to the string), so `$!+0`
downstream became 0. Root cause was four convergent chokepoints in
`cl/pcl-runtime.lisp` that all snapshot `(p-box-value box)` / `(unbox box)`:
`p-aref-unbox-elem` (reading `$_[0]`/`$a[i]`), `%p-flatten-list` (list-assign
RHS), `%p-array-store-scalar` (`@a=(…)`/`push`), `%p-make-hash-entry`
(`%h=(k=>…)`). Fix: a `%p-dualvar-box-p` predicate (nv-ok + string primary value
whose own numification ≠ the nv → a *genuine* dualvar, excluding plain numified
strings like `"5"`) plus a `%p-dualvar-copy` helper; each chokepoint now
preserves both halves. Also wired `Scalar::Util::dualvar` to actually build a
dualvar (`builtin::dualvar` → runtime `p-dualvar`; the shim previously returned
only the string). Guards: `Pl/t/errno-01.t` tests 8–9.

**`read()` FIXED (2026-06-25b).** `read(FH,BUF,LEN[,OFFSET])` was fundamentally
broken: `%p-read-impl` ignored BUF entirely and returned the read *string*
instead of the count, so `my $n = read($fh,$buf,5)` set `$n="hello"` and left
`$buf` empty. Rewrote it to fill BUF in place (via `box-set`), return the
char count (0 at EOF, undef + EBADF on an unopened handle), and honour a
positive/negative OFFSET (NUL-padding the gap). `io/read.t` 0/2 → 2/2. Guard:
`Pl/t/fileio-02.t` test 16.

NEXT t/io (high-leverage order): ~~`$/` record/paragraph modes~~ DONE
(`paragraph_mode.t` 80/80, `base/rs.t` improved) → ~~`scalar.t` in-memory
filehandles~~ DONE (89/128, unblocked via is_miniperl; rest are not-supported) →
`print.t`/`say.t`/`tell.t` partial diffs. Many files also use fork/pipe/socket
(`open.t`, `pipe.t`, `socket.t`, …) — lower priority, system-dependent.
(`errno.t` itself spawns `./perl` via `runperl`, so it is a fixture dependency,
not a PCL gap.)

**is_miniperl() fixed (2026-06-25e):** `defined &DynaLoader::boot_DynaLoader` now
true (runtime stubs marked `:defined`), so test.pl's `is_miniperl()` reports
false. This un-`skip_all`s every t/ file gated on it (54 files reference
miniperl; `scalar.t` is the big concrete win). Note `scalar_ungetc.t` is still
blocked separately by `use IO` (XS module).

### Not yet surveyed (next sessions)

`t/io/` remainder (34 of 44 — fork/pipe/socket-heavy), `t/uni/` (30),
`t/mro/` (73), `t/class/` (10), and the rest of `t/op/` not in `perl-tests/`.
Import the highest-signal ones the same way.

## How to re-run

```bash
T=/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t
perl tools/run-perl-suite.pl base/rs.t            # one file: perl-vs-PCL TAP + crash sig
perl tools/run-perl-suite.pl --dir comp           # all self-contained files in a dir
perl tools/run-perl-suite.pl --all                # every default dir, files NOT in the
                                                  # sweep corpus (task #25's companion sweep)
```

Since s302 the runner is the **companion sweep** for everything `perl-tests/`
doesn't cover: parallel (`--jobs`, default 8), runs against a fresh saved SBCL
core built per invocation (prove-core pattern, ~1.2s/file → ~0.003s), and
`--all`/`--dir` scans exclude files already copied into the sweep corpus
(matched by basename + head content — several `t/` files share a basename with
a corpus file from another dir).  `--include-copied` overrides; `--tsv FILE`
writes a per-file snapshot (`docs/perl-suite-run.tsv` = the s302 run).

**s302 `--all` snapshot (95 runnable / 528 scanned): 12 OK, 82 DIFF, 1 NOTAP.**
Scan coverage: op 82-in-corpus + 138 need-harness (only 1 extra runnable —
op is well covered by the sweep); mro 45 runnable (4 OK, rest = the known
C3-only/`next::method` gap, `docs/mro-plan.md`); re 21 runnable (crashes:
regex-engine slack + `\p{}` uniprops gap, `docs/unicode-property-regex-plan.md`);
comp 16, cmd 5, base 3, opbasic 2, run 2.  **Fully harness-dependent (need the
`require './test.pl'` fixture route before they can run at all): class 10/10,
uni 30/30, io 43/44, run 24/26, re 59/80, op 138/221.**  Notable movement vs
the 2026-06-23 rows: `base/rs.t` 6/35 → 26/15.
