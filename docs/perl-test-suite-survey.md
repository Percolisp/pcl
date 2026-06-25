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
- 🐞 **`/n` no-capture modifier** (real, fixable): `reg_nocapture.t` (16/25),
  parts of `rxcode.t`. `/n` makes `(...)` non-capturing; cl-ppcre has no `/n`, so
  rewrite the pattern's unescaped capturing `(` → `(?:` when `/n` is set (a
  generic pattern pre-pass, same shape as `%pcl-strip-gpos`). Best next t/re win.
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

NEXT t/re: implement `/n` (the only clean fixable win left in this batch, but
note it's bigger than it looks — scoped `(?n:)`/`(?-n:)`, named-capture
exemption, and stringification must preserve the original `(...)`), then triage
the partials (`reg_pmod`/`reg_posixcc`/`script_run`/`rxcode`). The
`re_tests`-driven `regexp*.t` family still needs the `re_tests` fixture wired
(separate effort).

### Not yet surveyed (next sessions)

`t/re/` (80, harness-gated — see above), `t/io/` (44 — partly tracked in `project_io_tests_and_open_errors`),
`t/uni/` (30), `t/mro/` (73), `t/class/` (10), and the rest of `t/op/` not in
`perl-tests/`. Import the highest-signal ones the same way.

## How to re-run

```bash
T=/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t
perl tools/run-perl-suite.pl base/rs.t            # one file: perl-vs-PCL TAP + crash sig
perl tools/run-perl-suite.pl --dir comp           # all self-contained files in a dir
```
