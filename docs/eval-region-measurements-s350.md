# s350 measurements — #230/F6 located, and the #240 step-2 instrumentation

Session 350.  Two items from the s349 queue (`docs/fable-answers-s348.md` §4.1),
both **measurement, no behaviour change**:

- **#230 / F6** — the audited locating sweep, then the s346 §2 decision tree.
- **#240 step 2 §2d** — the `p-eval-lex-lookup` miss-path listing (piggybacked
  on the same sweep, as ruled) and the `*package*`-consumer survey.

Both instruments are env-gated and inert when unset; the run that produced the
numbers below is gate-clean (§0).

---

## 0. The run

```bash
rm -rf ~/.pcl-cache/*
PCL_V2_AUDIT_LOG=…/f6.tsv PCL_EVAL_LEX_MISS_LOG=…/miss-sweep.tsv \
  perl sweep-perl-tests.pl --jobs 8
```

Cold cache, 108 files.  **GATE: clean** — `0 new, 0 fixed`, TOTAL passing
`baseline 18498, current 18498 (+0)`, 64 fully-passing files, min MemAvailable
5.4 GB.  The 2 UNSTABLE / 4 unverified rows are the standing postfixderef.t /
ref.t / tr.t crash-file noise.  So the instrumentation costs nothing and the
measurements below are taken on a green tree.

---

## 1. #230 / F6 — LOCATED: `perl-tests/tr.t` RT #132608, a string eval

The whole-sweep v1-route audit is **18 events** (13 `TODO` = v2 gaps, 5 `DIE` =
v2 correctly reporting a perl-level error, which v1 then raises identically),
and F6 is exactly one of the 13:

```
TODO  parse_code  $c = $s =~ tr/\x00-\xff/ABCDEFGHIJKLMNOABCDEFGHIJKLMNO…
      Parser2 TODO: oversized top-level run form (73769 chars > 64000)
```

`parse_code`, not `parse_file` — **the source is a string eval**, which the
s346c narrowing predicted (zero transpile-time events across all 111 files).
The eval string is built at run time in `perl-tests/tr.t:474-498`:

```perl
    # RT #132608
    $s = "\x{9000}\x{9001}\x{9002}";
    $e =    "\$c = \$s =~ tr/\\x00-\\xff/"
          . ("ABCDEFGHIJKLMNO" x (0xa000 / 15))     # 40960 chars
          . "/c; 1; ";
    eval $e or die $@;
    is $s, "IJK", "RT #132608 len=0xa000";
    is $c, 3,     "RT #132608 len=0xa000 count";
```

(and a second, larger copy at `0x12000 / 15` immediately after).  It is a
regression test for a perl bug about replacement lists longer than a short —
i.e. the input is *deliberately* torture-scale.

Reproduced in isolation (scratchpad `f6probe.pl`): both evals produce
`IJK/3` and `LMN/3` under PCL, identical to perl, via the v1 fallback.

### 1a. The decision-tree verdict: §2.3 applies — no pre-flip fix

The s346 §2 tree branches on what the sweep found:

- §2.2 (top-level-`my`-swallows-the-remainder → extend `_oversized_top_decls`)
  does **not** apply: the form is not a `my`-`let` body, it is a single `tr///`
  statement whose *own* lowering is 73769 chars.
- §2.3 **does** apply verbatim: *"If the source is an eval string or a
  `fresh_perl` child: NO pre-flip fix required.  The event becomes a ruled
  refusal under the §1.4 amendment — perl-shaped text at step 2, and the
  affected baseline row edited with its cause."*

So F6 needs no engineering.  What it needs is the number below, which the
ruling asked for implicitly ("one honest loud row … is an acceptable flip
outcome") and which nobody had taken.

### 1b. The cost at the flip, measured — 2 tr.t rows

The refusal is invisible today because the v1 retry compiles it.  After the
E4.1 flip there is no retry, so `eval $e` fails, `$@` is set, and tr.t's
`eval $e or die $@` aborts the file.  Measured by disabling the retry for
exactly this message (temporary local hook, reverted; the sweep was run
`--jobs 1 --no-gate` on tr.t alone):

| tr.t | passing | failing | ran / planned |
|---|---|---|---|
| today (v1 retry) | 241 | 3 | 244 / 317 |
| retry disabled (post-flip shape) | 239 | 3 | 242 / 317 |

**Exactly 2 passing rows**, and the file's abort point does not move — tr.t
already stops at test 244, which is the `len=0xa000` pair itself.  (That is
also why the sweep logs only ONE F6 event for two evals: the second one, at
`0x12000`, is never reached inside tr.t.  Run standalone it gates too.)

So the flip's F6 bill is: `RT #132608 len=0xa000` and `… count` move from
passing to a loud `$@`, in a file that is already PARTIAL and stops there
anyway.  Two rows to edit into `docs/fail-baseline.tsv` with their cause at
the step-2 commit, plus the `docs/not-supported.md` entry §5a.3 requires.

`$RUN_FORM_MAX` is not raised — unchanged, per the ruling.

### 1c. The other 17 events — the whole sweep's fallback inventory, named

§5a.3 (as amended s347) requires **zero UNEXPLAINED eval-mode fallbacks** at
the flip.  The same log answers that for the sweep half, so here is every
event with its owner.  Nothing is unexplained.

| n | why | subject | owner / what the flip does |
|---|---|---|---|
| 6 | `TODO` `parse_file` PPI parse failed, `/tmp/pcl_fp_*.pl` | fresh_perl children | **F4 / #228** — NUL bytes in lex.t's children, diagnosed s342d; 5 of 6 already blessed |
| 5 | `TODO` `parse_code` eval-mode trailing my/our declaration | `my $$x`, `my @$x`, `my %$x`, `my $$$x`, `my($a,$b),$x,my($c,$d)` | **F2 residual** — see below |
| 1 | `TODO` `parse_code` eval-mode multi-segment (top-level package statement) | `{ package Eval1; { package Eval2; … } }` | the **multi-switch ruled refusal** (s347 DECIDED); rephrase perl-shaped at step 2 |
| 1 | `TODO` `parse_code` oversized top-level run form | tr.t RT #132608 | **F6**, §1a–1b above |
| 5 | `DIE` `parse_code` | `chop($x) = 1`, `chomp($x) = 1`, the two 2-arg forms, one `sub f :lvalue` | **not a v2 gap** — v2 correctly reporting a perl-level error; v1 raises it identically (measured s342), so the flip changes nothing |

**The F2 residual is free, and four of its five rows get BETTER at the flip.**
The four `my $$x`-family strings live in `perl-tests/eval.t:239-246`, which
asserts `isnt($@, '', "my $_ is forbidden")` — and perl does forbid them
(`Can't declare scalar dereference in "my"`).  PCL accepts them today via the
v1 retry (principle 9: PCL does not validate invalid Perl), so **eval.t rows
46-49 are blessed FAILURES right now**.  After the flip the refusal makes the
eval die, `$@` is non-empty, and those four rows start passing.  The fifth,
`my($a,$b),$x,my($c,$d)` (my.t:154, RT #126844), is followed by an
unconditional `pass()`, so its outcome is unchanged either way.  Net flip cost
of the F2 residual: **zero rows, +4 likely**.

---

## 2. #240 step 2 §2d.1 — the miss-path listing

`p-eval-lex-lookup`'s alist-miss path (the branch that resolves a free name
against a package symbol, and the one the region binding would redirect) was
instrumented to log `name / *package* / *pcl-current-package* / bound-or-
autoviv / alist size`.  Whole sweep: **2720 events, 29 distinct names.**

Resolution package: `MAIN` 2719, plus one each in `_117941`, `NoSub`, `db`.
Outcome: **2703 `bound`** (an existing package global was found) / **19
`autoviv`** (a fresh container was installed).

### 2a. The expectation held: no punctuation/magic name arrives

The ruling's stated expectation was that `_eval_scope_free` skips
`%EVAL_RUNTIME_VARS` and Magic tokens, so no special name should reach the
lookup — with the listing, not the expectation, as the evidence.  **Confirmed:
zero punctuation variables** (`$@ $_ $! $0 @ARGV @_ $/ $\ …`) appear.  The
complete name list is ordinary user identifiers plus five groups worth naming:

| group | names | events | note for step 2 |
|---|---|---|---|
| sort pair | `$a`, `$b` | 1350 | perl scopes these **per package** too, so the binding is perl-correct; but the eval head's `(defvar $a …)` is emitted into the CALLER's package, so under a region binding the head and the lookup would name different symbols — see 2b |
| `$c`, `$d` | | 1240 | plain lexical-ish globals from the same mass-eval loops (bop.t family) |
| package-scoped specials | `@ISA` (2), `$AUTOLOAD` (1), `$VERSION` (1) | 4 | package-scoped in perl; the binding moves them TOWARD perl |
| PCL-internal mangled | `$x__state__e44768cee_0`, `…_0__init` | 2 | state-variable mangling; these are file-level PCL globals, and a region binding would move them to X — flagged, see 2b |
| non-scalar sigils | `*a *x *y`, `&SUB &t108 &fred4 &constant1 &constant2 &rt132760`, `@…`, `%…` | 27 | `*` and `&` have **no arm** in the autoviv `cond`, so they fall to the scalar-box arm.  Pre-existing, unchanged by step 2; a rule-12-adjacent note, not a blocker |

### 2b. What the listing does NOT say — and the second instrument

The log cannot record "a region package is in effect", because **nothing
establishes one at lookup time today — that is the bug**.  So the other half
was instrumented on the emitter side: every #226 collapse event, with its
region package and the free names that would reach the lookup under it
(`PCL_EVAL_REGION_LOG`, `_assemble_eval_mode`).

**Sweep half — 5 region events, every one with an EMPTY free-name set:**

```
T121::Z   0 free names     × 5
```

All five are `perl-tests/signatures.t:317-321`,
`eval("package T121::Z; ::t121(…)")`.  The bodies call a **fully qualified**
`::t121`, so even the body-side effect of the binding is nil there.

**So on the sweep, region × free-name = the empty set.**  The 2720 miss events
above happen in evals with NO region package, which step 2 does not touch: the
binding is established only when the emitter passes a region package.  Every
one of the five names flagged in 2a therefore has **zero live exposure** to
step 2 on this corpus — they are listed so the decision is stated, not because
anything is at risk today.  (Second run for this instrument only; TOTAL
passing identical at 18498.)

**Board half — this is the number that changes the picture.**  Same
instruments, the 14-dist board (`tools/cpan-scoreboard.pl --jobs 6 --timeout
120`, the s343 command verbatim):

- **108 region events**, and **86 of them have a NON-EMPTY free-name set.**
- **239 miss events**, in four groups: `&_capture_tee` × 184 (Capture::Tiny),
  `$method` × 50 (Class::Method::Modifiers), `$VERSION` × 4
  (ExtUtils::MakeMaker::_version), `$a` × 1 (MAIN).

**The s348 measurement was of the module SOURCES, not of the running suites**
("20 collapse events, every one with an EMPTY free-variable list" — Role-Tiny
+ Try-Tiny transpiled).  Under the board's *tests* the same idiom runs 108
times and 86 of those carry free names, so the sentence "the 20 live collapse
events have empty free-var sets" is true of what it measured and **not true of
the corpus step 2 must survive**.  Recording that here so step 2 sizes itself
against the right number.

The 86 are all one shape — Class::Method::Modifiers' modifier installer:

```perl
$cache->{wrapped} = eval "package $into; +sub $attrs { \$code->(\$method, \@_); };";   # l.89
eval $generated;                                                                       # l.148
```

with free sets `$code $method` (27), `$wrapped` (20), `$before $method
$wrapped` (18), `$after $before $method $wrapped` (12), `$after $method
$wrapped` (9).  Region packages are the test classes (`Child`,
`Child::After`, `MyRole`, …) plus Role::Tiny's generated
`Foo__WITH__R::…__A00n` names.

**Every one of these resolves through the capture alist, not the miss path —
except `$method`, 50 times.**  That is a *false-positive free name*, not a
capture gap: in the `$generated` string `$method` is declared by the string's
own `for my $method (@$after)`, so `_eval_scope_free` counts a name the body
immediately shadows.  It resolves to `$Class::Method::Modifiers::method`
today and would resolve to `$Child::method` under the binding; both are dead
values.  Worth one line in step 2's commit, not a fix.

So the honest summary of step 2's exposure: **the alist keeps winning for
every real capture** (the acceptance row `my $x = 5; eval 'package Cap; sub f
{ $x }'` is this exact shape, 86 times over), and the only names the binding
actually redirects are the ones that miss — 50 phantom `$method`s on the
board, 0 on the sweep.  The BODY-side effect, by contrast, applies to all 108.

Board drift vs `docs/cpan-board14-s343.tsv`: 3 files, all **gains**
(`role-basic-composition` 8/0 → 10/3, `role-basic-exceptions` 2/0 → 4/0,
`140-lvalue` 4/3 → 6/3, `reduce` 21/11 → 23/9) — the #208 drift plus the s346b
"read ROWS not labels" case.  Nothing regressed; the instrumentation is inert.

### 2c. Stop-rule status

The §2d stop-rule fires on "a special/magic name reaching the lookup".  No
punctuation/magic variable does.  The two shapes that deserve an explicit
decision in step 2 rather than a silent inheritance are in the table above
(`$a`/`$b` head-vs-lookup package, and the `$x__state__…` mangled globals);
both are named here so step 2 states its answer instead of discovering it.

---

## 3. #240 step 2 §2d.2 — the `*package*` consumer survey

`grep -n '\*package\*' cl/*.lisp` — 24 hits in `cl/pcl-runtime.lisp`, 1
comment-only hit elsewhere.  Dropping comments, the docstring, and the two
instrumentation lines added this session leaves **13 real consumers**.  Each
row says what changes when a region body runs with `*package*` bound to X.

| # | site | what it uses `*package*` for | under the binding |
|---|---|---|---|
| 1 | `p-sub` macro, `target-pkg` (~512) | package for an **unqualified** sub name (qualified names use `symbol-package`) | region subs are already emitted qualified by #226, so no change; an unqualified one would land in X — **toward perl** |
| 2 | `p-use`, `caller-pkg` (~11864) | import target when the codegen passes no explicit `into` | `use` inside the region imports into X — **toward perl** (this is the same bug #226 fixed for `*pcl-current-package*`) |
| 3 | `%p-resolve-sub-symbol` (~12349) | package for an unqualified symbolic sub name `&{"foo"}` | resolves `X::foo` — **toward perl** |
| 4 | `p-funcall-ref`'s inline pkg split (~12381) | die-message text only | message names X — **toward perl** |
| 5 | lazy-coderef AUTOLOAD fallback (~12566) | package to look `AUTOLOAD` up in | `X::AUTOLOAD` — **toward perl** |
| 6 | `p-get-coderef`, unqualified (~12605) | `\&{"name"}` resolution | `X::name` — **toward perl** |
| 7 | `%p-symref-box` get (~12712) | unqualified `${$n}` read | reads `$X::n` — **this is the D2 probe's fix** |
| 8 | `(setf %p-symref-box)` (~12731) | unqualified `${$n}` write, creating the package if needed (`make-package :use '(:cl :pcl)`) | writes `$X::n`; also the find-or-create convention step 2 must reuse |
| 9 | `%p-symref-array` (~12755) | `@{"name"}` | `@X::name` — **toward perl** |
| 10 | `%p-symref-hash` (~12780) | `%{"name"}` | `%X::name` — **toward perl** |
| 11 | `p-bless` empty/undef class (~14209) | class defaults to the current package | blesses into X — **toward perl** |
| 12 | `p-resolve-invocant` (~14302) | is there a sub named `Foo` in the current package, for `Foo->bar` | looks in X.  The only row where the binding can take something AWAY: a sub `Foo` defined in the CALLER stops being found from inside the region — which is what perl does |
| 13 | `p-eval` itself (~8239) | rebinds `(*package* *package*)` around the read/eval loop | unaffected: a nested eval re-establishes its own package from `*pcl-current-package*`, which the region's `p-set-current-package` has already switched to X |

**Every site implements "unqualified → current package".** Inside a region
perl says the current package IS X, so the binding moves all thirteen toward
perl; none of them moves away.  Row 12 is the only one that *removes* a
resolution, and removing it is the correction.

### 3a. Two facts step 2 needs, found while surveying

1. **The eval preamble is the CALLER's package, not `:pcl`.**  `pl2cl`'s
   `build_eval_preamble` replaces `(in-package :pcl)` with
   `(p-defpackage :Caller)(in-package :Caller)`, so `*package*` during the
   thunk is the caller — confirmed live by the miss log (`MAIN` × 2719).
   Nothing else establishes X: `p-set-current-package` sets only
   `*pcl-current-package*`, and it is emitted INSIDE the body, i.e. after the
   thunk's arguments have already been resolved.  The s349 §2c reading is
   exact.

2. **A region with no free names emits no thunk at all.**
   `_assemble_eval_mode` wraps the body in `(pcl:p-eval-thunk …)` only when
   the free-name set is non-empty; otherwise the body forms are emitted bare.
   Since every measured region event has an empty free set, step 2 cannot put
   the binding on `p-eval-thunk`'s argument list alone — it must also cover
   the no-free-names shape (emit the thunk unconditionally when a region
   package is present, or emit an explicit binding form around the body).
   This is the difference between "the binding fixes the lookup" and "the
   binding also fixes the BODY", and the body is where all 20 live events are.

---

## 4. Where this leaves the queue

**#230 is CLOSED by measurement, with no code change.**  Both halves:

- **F3** was already routed through #78 (s345 ruling) and #78 shipped; the #26
  gate stays as an unreached backstop for step 3's reachability pass to retire
  (s346 §3, unchanged).
- **F6** is `perl-tests/tr.t` RT #132608, an eval string, so the s346 §2.3
  branch applies: **ruled refusal, no pre-flip fix**, cost measured at 2 tr.t
  rows (§1b), text to be rephrased perl-shaped and the two baseline rows
  edited with their cause **in the E4.1 step-2 commit** — not earlier, because
  a `Parser2 TODO:` rephrase before then converts a silent retry into a
  user-visible die (s347 DECIDED).

**#240 step 2 keeps its ruling and its stop-rule; the stop-rule did NOT fire**
(no special/magic name reaches the lookup, §2a), so it proceeds as ruled.  Two
things it must now assume that the s349 ruling could not:

1. **The live region corpus is 108 events, 86 with free names** (§2b), not 20
   with none.  The `p-eval-thunk` argument list is genuinely exercised, and the
   acceptance battery's "the capture the parser gate must never refuse" row is
   the board's dominant shape rather than a hypothetical.
2. **A region with no free names emits no thunk at all** (§3a.2), so the
   binding cannot live only on `p-eval-thunk`'s parameter path — the 22
   empty-free-set regions (and all 5 on the sweep) would get no binding, and
   the body is where the effect matters.

Gate at this commit: `tools/prove-core` → **131 files / 4648 tests, Result:
PASS**.  Sweep gate clean (§0).  Board: 3 files drifted, all gains (§2b).

## 5. The instruments (delete with the step-2 commit)

| env var | where | what it logs |
|---|---|---|
| `PCL_EVAL_LEX_MISS_LOG` | `%p-eval-lex-miss-audit`, `cl/pcl-runtime.lisp` | one line per alist miss: name, `*package*`, `*pcl-current-package*`, bound/autoviv, alist size |
| `PCL_EVAL_REGION_LOG` | `_assemble_eval_mode`, `Pl/Parser2.pm` | one line per #226 collapse: region package, free-name count, free names |

Both are append-only, one short line per event (well under `PIPE_BUF`, so
`--jobs 8` interleaves cleanly), and no-ops when the variable is unset.
`getenv` is re-read per event on purpose — a cached value would freeze into a
saved core.

---

## 6. The one open design fork — PROBED, and it is not a fork

§3a.2 left step 2 with a question the s349 ruling could not have posed: the
binding has to cover regions with no free names, those emit no thunk, so their
bodies would have to be **wrapped** — and wrapping is precisely what
`_cap_inlining_if_huge` refuses to do to `eval-when` / `p-sub` / `defvar` /
`p-defpackage`, because a wrapper strips top-level-ness and breaks
compile-time visibility.  An eval region emits all four.  If that bit, step 2
would need two mechanisms (wrap when free names exist, head-position package
switch when they don't) and the fork would be a design call, not an
implementation detail.

**Measured instead of argued.**  `PCL_EVAL_REGION_WRAP=1` forces the wrap on
every #226 region regardless of free names.  The emission really does change:

```
WRAP=0:  <body forms at top level>
WRAP=1:  (pcl:p-eval-thunk (list )
          (lambda ()
            <body forms>))
```

Run over the three region-heavy dists — Role-Tiny (23 files),
Class-Method-Modifiers (14), Try-Tiny (11), 48 files, cold cache:

**Every row identical to the unwrapped run.**  `diff` of the two per-file TSVs
is empty.

Direct probe of the three things the wrap could plausibly break, all matching
perl exactly under both settings:

| probe | WRAP=0 | WRAP=1 | perl |
|---|---|---|---|
| `eval 'package Rw1; use Role::Tiny; sub greet {…} 1'` → `Rw1::greet()` | `hi from Rw1` | same | same |
| `Rw1->can('greet')` (the sub really installed in the stash) | yes | same | same |
| `eval 'package Rw2; our $V = 7; sub v { $Rw2::V } 1'` → `Rw2::v()` / `$Rw2::V` | 7 / 7 | same | same |

**So step 2 needs ONE mechanism, not two**: emit the thunk whenever a region
package is present (empty names list when there are none) and hang the
`*package*` binding on `p-eval-thunk`, exactly as s349 §2c sketched.  The
empty-free-set case is a parameter-list detail, not a second design.

The reason the wrap is harmless here is worth recording so it is not
re-derived: `p-eval` READs and EVALs the eval text **form by form**, and the
region's definitions are hoisted into the body by `_interleaved_defs` — they
are already inside one `eval`'d unit whose compile-time phase has run by the
time the body executes.  The `_cap_inlining_if_huge` prohibition is about
FILE-mode top-level forms, where `eval-when (:compile-toplevel)` is the only
thing making a sub visible to a later `BEGIN`.  Eval mode has no compile-file
phase to lose.

Instrument to delete with step 2, alongside the other two:
`PCL_EVAL_REGION_WRAP` (`_assemble_eval_mode`, `Pl/Parser2.pm`).
