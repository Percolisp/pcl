# DECIDED.md — one-grep index of settled questions

*Grep THIS FILE before probing, designing, or writing anything up as an open
question.  One line per decision: keywords, the ruling, the pointer to the
authoritative text.  This file is an INDEX — never put the full argument
here.  When a session settles a question (user ruling, Fable review, design
doc), add a line in the same commit.  When a line turns out wrong, fix the
authoritative doc first, then the line.*

*Origin: s316v wasted real time re-deriving four already-settled facts
(review doc §7).  The rule now: read failing test → grep DECIDED.md → grep
not-supported.md → only then probe.*

## Semantics / language policy

- **DESTROY**: not fired in ANY shape (scope exit, undef, reassign, sub
  return) → `not-supported.md` §DESTROY.  Re-open as scoped R2 feature =
  OPEN user decision (`fable-answers-s316v.md` §6c).
- **Error-message text / invalid-input detection**: out of scope — PCL
  transpiles valid Perl, exact fatal text not a goal (user 2026-07-28) →
  `not-supported.md` §Error message text + CLAUDE.md 9.  Blanket
  skip-registry category approved (`fable-answers-s316v.md` §6d).
- **Ref identity / refaddr / `CODE(0x…)` / `==` on refs**: monotonic
  never-reused ids, stable across GC — deliberately NOT raw pointers →
  `ir-spec.md` §2.5.
- **Host-visible values, how far to chase perl's bytes**: match perl's
  SHAPE and invariants, not its bytes; where perl's bytes carry a defect,
  do better; exact bytes only where a program can branch on them (e.g.
  seeded rand → drand48) → `ir-spec.md` (rule), `fable-answers-s316v.md` §6h.
- **`eval $str`**: HARD REQUIREMENT, must always work, never gated (user
  2026-07-07).
- **String-eval scope capture has NO third route** (s383, #295): the eval'd
  text's enclosing scope reaches eval sites INSIDE the text — and named subs
  the eval defines, called later — via `%p-eval-env%`, a lexical bound at
  body entry to the entry value of `*p-eval-lex-alist*` and APPENDED to each
  eval-mode site alist (route 1, the pad chain lexicalized).  Never an
  ambient/dynamic hand-off: that leaks the eval's lexicals into subs it
  merely calls (measured: the s382h "publish around the body" fix moved
  zero rows and was reverted) → `ir-spec.md` §9.1 "pad-chain continuation";
  guard `Pl/t/transpile-test-10.t`.
- **Speed vs readable generated CL**: performance wins (user 2026-07-02) →
  CLAUDE.md §2.
- **Bareword class names** (`tie $x, Tie::StdHash`): per-builtin
  class-name-ARGUMENT-position rule reusing the `bless` bareword mechanism
  (ExprToCL `$is_bareword`), NEVER a global bareword→string rule — three
  failed global attempts recorded in task #142 → `fable-answers-s316v.md` §1.
- **PExpr operand-boundary region** (`$end_pars` maze, PExpr.pm ~2600-3700):
  do NOT add guards there; the fix is Option B's `_reduce_term` →
  `pexpr-term-parsing-review.md`.  #147 and the #138 state-init residual
  WAIT for it (`fable-answers-s316v.md` §6e, §4).
- **Flip-flop constant operands** (`"x" .. 3` vs `$.`): classify per-operand
  at the existing codegen `p-flipflop*` selection point → task #141,
  `fable-answers-s316v.md` §2.
- **quotemeta / `\w`-sensitive builtins / no per-scalar UTF8 flag**: default
  is perl's BYTE semantics; Unicode rules only under `use utf8` /
  `unicode_strings`, selected at transpile time per scope → task #146,
  `fable-answers-s316v.md` §6b.
- **pack/unpack `U` character-vs-byte modes**: implement the real mode model
  (default character mode; `U0`/`C0` switches); anything not implemented
  DIES loudly, never a silent wrong value → task #148,
  `fable-answers-s316v.md` §6j.
- **`printf %n`**: OPEN — user policy call (recommendation: bless as
  not-supported) → task #143, `fable-answers-s316v.md` §6a.
- **Aggregate state model** (tie/readonly/referent-kind — #154-residue,
  #155, #159, #163): side table REJECTED (mutation-path tax); boxed
  aggregates = E5-era Fable design, do NOT start; referent-kind tag on the
  box APPROVED post-R1 (fixes #163 ref identity/printed type + #154's two
  shapes) → `fable-answers-s318.md` §1.  **The tag half is SUPERSEDED
  (s333 measurement, CONFIRMED s335)**: no slot was added — `is-ref` on the
  wrapper already discriminates, and a sticky tag would be *wrong* where it
  differs from the flag (perl re-decides SCALAR↔REF from the referent's
  current content) → `fable-answers-s334.md` §s333-1, DECIDED §s333 below.
- **tie on ARRAY/HASH**: interim = loud stderr WARNING + not-supported
  subsection (NOT a die — would CRASH avhv.t-class files); real support
  waits for boxed aggregates → `fable-answers-s318.md` §1, task #155.
  SHIPPED s320: `%p-warn-aggregate-tie` in `pcl-runtime.lisp`, one line per
  (kind, class) per process so a tie in a loop stays one line.  **s339: folded
  into the shared `%p-announce-unsupported`** (ruled `fable-answers-s337.md`
  §5b) — the class rides in the OPERAND, which is what keeps the per-class
  dedup: `PCL: tie: a HASH (class Foo) is not implemented — …`.
- **Read-only aggregates** (`Internals::SvREADONLY(@a,1)`): storage-swap to
  a simple vector, post-R1; never a weak-hash probe on the push path; do
  not bless as not-supported → `fable-answers-s318.md` §2, task #159.
  **SHIPPED s337**: `Internals::pl-SvREADONLY` is now a MACRO (the call site
  is the only place that has the variable's storage cell — no `Pl/` change),
  the flag IS the storage (simple vector = fixed size = perl's read-only AV,
  whose ELEMENTS stay writable), and the runtime checks only supply perl's
  message.  Rows: push.t/unshift.t/splice.t fully pass, sort.t's in-place
  row passes, both skip-registry entries deleted, the splice.t + sort.t
  hand-edited `ok(1,"SKIP")` stubs restored from the t/op originals.  Guard
  `Pl/t/readonly-array-01.t`.  Three announced divergences (shrinking
  `$#ro=N`, a ref taken BEFORE the swap, `SvREADONLY(@$ref,1)`) and the
  still-unimplemented SCALAR/HASH forms are in `not-supported.md`
  §`Internals::*`.
- **`do SUBNAME(LIST)`**: NO fix — pre-5.20 perl called the sub and PCL
  keeps that semantics; modern perl's rejection is principle-9 material
  (register do.t t63/t65, nothing near `$end_pars`) →
  `fable-answers-s318.md` §4, task #158.  Measured s320: the family is
  **four** rows, t63-t66 (one `foreach my $mode` over `subname("arg")`,
  `subname()`, `$subref("arg")`, `$subref()`, each asserting
  `$@ =~ /\Asyntax error/`), and do.t's **2 PCL-only extra TAP numbers are
  those same rows' `fail()` guard firing** — not a separate defect.  op/do.t
  still reads DIFF (65/8) and gets NO expected.tsv row: t67 (delete-helem
  result copied), t68 (RT 124248 DESTROY-by-GC) and t70 (`$@` false on
  `do DIR`) are unexplained, and the registration bar is all-or-nothing.
- **chr() above U+10FFFF**: blessed — SBCL `char-code-limit` = #x110000,
  U+FFFD is the answer; op/chr.t → XDIFF → `fable-answers-s318.md` §11,
  `not-supported.md` §Unicode, task #173.  SHIPPED s320 (verified XDIFF:
  t10-13 `use bytes` + t40-42 above-Unicode = all 7 rows).  `ord` still
  round-trips the number — only the character/encoded form is lost.
- **#149 application**: per-ROW; side-effect/value/behaviour assertions on
  VALID Perl never register; interleave, never a campaign →
  `fable-answers-s318.md` §3.
- **:crlf layer model** (#139) and **source-echo comments** (#132):
  user-held design decisions — do not start.
- **`**` float divergence**: parked → memory `project_power_op_float_divergence`.
- **mro**: C3-only, no `mro::` API / `next::method` yet →
  `not-supported.md` §mro, provider planned in `mro-plan.md`.
- **@_ aliasing, Unicode limits, and every other blessed gap**: grep
  `not-supported.md` FIRST — the answer is often already there.

## Coding rules (the ones people trip over)

- **Missing case DIES, never defaults**: a runtime `cond`/dispatch over a
  closed set of legal values ends in an explicit error naming the value,
  not a `(t …)` default — silent-wrong is the worst failure mode in the
  tree (vec-64, #138 silent drop) → CLAUDE.md 12, audit task #152.
- **Fix at the right LAYER**: module behavior → `lib/` shim; mechanisms →
  parser; core semantics → runtime → CLAUDE.md 9a.
- **Reuse, don't duplicate**: find the sibling mechanism; same logic in two
  places = hard stop → CLAUDE.md 11.
- **A tree-walking predicate is asked about CANDIDATES, never about every
  node**: `_ref_shadowed`-class predicates (climb parents + scan siblings)
  cost O(file) per call, so a per-token call is quadratic — filter first
  (`eq $canon`, "the fixer matched the name"), then ask.  s316b's one
  `$skip->($t)` per `PPI::Token` cost pack.t 5.8 s → 74 s of transpile for
  BYTE-IDENTICAL output → task #184, session-log s334, guard
  `Pl/t/parser2-02.t` (counts the calls, not the seconds).  **Corollary
  (s335): when the fix is "normalise into the sibling's discipline", grep
  for the OTHER siblings before closing** — the same predicate had two more
  per-token callers in the W10 spanning-rename loops (86.6 s → 0.98 s on a
  two-package 200-statement file) → `fable-answers-s334.md` §5.
- **#213's TIME was the printer, not the recursion**: `_close` asked
  `_ends_in_comment` (pure-Perl char scan) about the WHOLE accumulated
  subtree text at every nesting level — 93% of a nested file's transpile.
  Fixed byte-identically s335 (a terminal comment needs a `;` after the
  last newline — `rindex`/`index` guard).  What remains of #213 is bytes +
  recursion depth: clamp-indent (a) approved cosmetic, `let*` runs (b) is
  the real fix at E5, depth-keyed defvar flattening (c) REJECTED →
  `fable-answers-s334.md` §1.
- **A "suspect X" task carries the cheap discriminating measurement** (or
  names it and why it was skipped) — #184's suspicion was carried 13
  sessions when "time the two phases" was 3 minutes → CLAUDE.md lookup-order
  block, `fable-answers-s334.md` §2.
- **Never simplify a failing test**; skip only via the registry →
  CLAUDE.md 5, `test-skip-registry.md`.
- **Checked-in transpiled artifacts** (`cl/pcl-pack.lisp`, `cl/pcl-mro.lisp`):
  regenerate after emission changes (`tools/rebuild-pack`; pl2cl lib/mro.pm)
  → CLAUDE.md §Quick Reference.
- **Emission change ⇒ bump `*pcl-cache-generation*`** or stale caches run
  old codegen → CLAUDE.md §Pipelines.

## Test / triage infrastructure

- **`docs/perl-suite-run.tsv` columns**: `P`=PERL, `C`=PCL (C for CL);
  `NOTAP` means PERL produced no TAP (row not comparable — says nothing
  bad about PCL) → legend at top of the tsv; source
  `tools/run-perl-suite.pl`.
- **`.suitelog/*.fails.tsv` description column is PERL's test line**, values
  interpolated by perl — never read it as PCL's output.
- **A run that comes back all/mostly NOTAP means the FIXTURE is broken, not
  the files**: the perl side runs with CWD = the real `t/`, so `require
  './test.pl'` must find PERL's 2000-line harness; with PCL's ~400-line stub
  there instead, real perl cannot even compile `plan tests => N` and emits
  zero TAP.  This is what made io/defout.t / op/localref.t / uni/bless.t look
  NOTAP in s316v (a `cp` had followed a symlink onto the real file).  The
  runner now dies on that instead of producing the misleading run
  → `tools/run-perl-suite.pl` fixture-sanity block, task #151.
- **XDIFF registration bar**: a file gets an `perl-suite-expected.tsv` row
  ONLY when every failing test in it is explained by a blessed
  `not-supported.md` section — partially-explained files stay UNEXPLAINED
  → `perl-suite-expected.tsv` header, `fable-answers-s316v.md` §6f.
  **And the claim is MACHINE-CHECKED per row since s336 (task #185)**: the
  reason excuses only the rows blessed in `docs/perl-suite-expected-rows.tsv`
  (generated by `run-perl-suite.pl --bless-rows`, keyed by PERL's test
  DESCRIPTION per #177).  An unregistered diverging row keeps the file DIFF
  and is named; a row that stops diverging makes it STALE.  Escape hatch
  `*rows-unstable*` (one entry, hand-placed, needs a measurement that the row
  set is nondeterministic) — used by exactly one file,
  `mro/package_aliases_utf8.t`.
- **Copied-file corpus split**: t/ originals are authoritative; the runner
  no longer hides a t/ file behind a `perl-tests/` copy of the same name;
  drifted copies re-sync post-R1 → task #150, `fable-answers-s316v.md` §6i.
  **PART 2 DONE s337**: `perl-tests/{chop,dor,not,quotemeta}.t` restored
  byte-identical to `t/op/` (they had hand-lowered plans, commented-out
  assertions, and a hand-written `dor.t` substitute).  Each newly-visible
  failure got a CAUSE: not.t 21–24 → registry `:read-only` (interned
  booleans), dor.t 26/28 → registry `:principle9` (assert rejection of
  invalid Perl), quotemeta.t 30/31 → honest baseline fails owned by #146,
  chop.t → PARTIAL 96/148 because **`utf8::encode` is a no-op**, so the
  file's own `next` guard skips 48 rows (not skip-registrable — they never
  run).  A hand-lowered plan had been masking exactly that shortfall.
- **v1 is the emission ORACLE until E4.1** (`PCL_V1=1 ./pl2cl`) — copy its
  shape when correct, don't invent one → `v2-opus5-execution-plan.md` §2.
- **New Pl/t tests** → smallest `transpile-test-NN.t`, cap ~50/file, never
  -01 (user 2026-07-28).
- **Failed attempts are recorded IN the task** (what was tried, what killed
  it) — a task that says what NOT to retry outranks one that only states
  the goal.
- **The TAP layer must not build its own regex scanner.**  `pl-like`/
  `pl-unlike` in `cl/pcl-test.lisp` called `ppcre:create-scanner` directly,
  so `like`/`unlike` judged patterns by DIFFERENT rules than `=~` did —
  missing the cl-ppcre extended-mode workaround, and (s321) choking on the
  `/xx` option, with the `handler-case` turning the error into a quiet test
  FAILURE.  Route through `%pcl-create-scanner` → task #179, CLAUDE.md 11.
- **The emitted CL contains NO host-implementation symbols — a host
  primitive reaches generated code only wrapped in a `p-*` macro.**
  Checkable: `./pl2cl FILE | grep -ac 'sb-[a-z]*:'` is 0 over the corpus
  (measured s382g).  The runtime uses SBCL internals freely; the IR does
  not, so a translator reimplements the `p-*` vocabulary and never an
  implementation detail.  Direction D is the worked example of holding the
  line: a symbol-macro-over-global-cell would naturally have put
  `sb-ext:symbol-global-value` at every global declaration and every
  `local`, and instead lives inside `p-defcell`/`p-local-cell` → two macro
  definitions for a port instead of thousands of sites.  Any future change
  needing a host primitive in emitted code takes the same shape →
  `ir-spec.md` §1, tasks #289/#290, s382g.
- **A regex PATTERN interpolates through the one scanner, and each reference
  lowers by being COMPILED AS CODE.**  `Pl::InterpScan` (perl's own
  `S_intuit_more`/`Perl_regcurly`) decides where a reference starts and
  whether `[…]`/`{…}` after it is a subscript or regex syntax; the
  ExprToCL consumer copies the text between events verbatim (regex escapes
  must reach cl-ppcre unprocessed) and lowers each event by re-parsing its
  own source text through the ordinary expression pipeline — never a second
  lowering table.  The interpolation GATE is the same scan, because a
  narrower gate silently un-does the consumer (`/$1/`, `/$#a/`, `/\Q$^O\E/`
  stayed literal).  The s/// REPLACEMENT keeps the legacy predicate on
  purpose (dq text, and `$1$2` is better served by the runtime's backref
  substitution than a per-match lambda) → `interp-scan.md`, task #237,
  guard `Pl/t/regex-interp-01.t`, s382f.
- **A qr is an OBJECT, and its `(?^flags:…)` wrapper is load-bearing.**  A
  pattern that is exactly one interpolated qr IS that qr (outer modifiers
  ignored); as PART of a bigger pattern the wrapper embeds verbatim; `/xx`
  prints two x's; and the stringification comes from the SOURCE text, not
  from any cl-ppcre rewrite.  Therefore a variable holding a qr is never
  frozen to text by the raw-slot verdict (`write-object`, `VarAnnotator`) →
  `ir-spec.md` §10 regex rows, task #181, s322.
- **NEVER join two TAP streams by test NUMBER** — pair them by DESCRIPTION
  (`tools/lib/PclTapAlign.pm`, unit-tested in `tools/t/tap-align.t`).  A
  file where PCL emits extra or missing rows mid-run then mis-attributes
  every LATER row, in both directions: op/do.t's number join accused two
  rows that PASS and credited two that fail.  Re-sync only on evidence (an
  exact desc match ahead, confirmed by the next row); value-interpolated
  and unnamed descriptions must fall back to positional pairing →
  task #177, s321.
- **A dead run must not look like a run nobody asked for**: every requested
  file gets a row (KILLED / NOT-RUN when the run died), the exit code is
  nonzero, and a journal records rows as they arrive → task #157,
  `tools/run-perl-suite.pl` header.
- **Fixture artifacts get a FIXTURE status + registry in the runner** (not
  `perl-suite-expected.tsv` — a harness artifact is not a language gap):
  #151 stub-cp, #167 splitpath-skip, #172 shadow-symlink getcwd →
  `fable-answers-s318.md` §10.  SHIPPED s320: `docs/perl-suite-fixture.tsv`
  (`file<TAB>rows<TAB>cause`), matched per TEST NUMBER and all-or-nothing —
  one unregistered failing row keeps the file DIFF and names it, a
  fully-passing registered file is STALE.  #151/#167 were FIXED, not
  registered; the file's RESOLVED list says why (its bar rule 3).
- **The suite tsv snapshot GATES R1**: one full regeneration (per-dir
  FOREGROUND chunks, `--jobs 2-4`) as the LAST pre-R1 act, after the
  FIXTURE/XDIFF hygiene lands; partial regeneration forbidden →
  `fable-answers-s318.md` §6.
- **pack.t sweep hole (#176)**: DONE s322 — the sweep **retries a TIMEOUT
  once at 3× the timeout** (`sweep-perl-tests.pl`, `--no-retry` disables).
  Ruling was `fable-answers-s321.md` §1; its step 2 (bless ~89 rows post-R1)
  turned out moot — **pack.t's 58 baseline rows were there all along** and the
  run matches them exactly.  The "0 baseline rows" claim came from grepping a
  NUL-containing tsv (grep goes binary-silent — use `grep -a`/perl).  The
  90→166 s slowdown is a separate Target-A task (#184).
- **A TIMEOUT is a measurement failure, not a result**: a timing-out file's
  baseline rows come back "unverified", so a regression inside it is invisible
  while the headline still reads "0 new" → task #176, s322.
- **Mis-attributed-evidence registrations (#177 aftermath)**: a tainted
  registration must be RE-VERIFIED before R1; reason-text corrections gate
  R1, status changes only if re-verification fails.  In `renumbered` files,
  per-row claims must quote the DESCRIPTION, not bare tNN →
  `fable-answers-s321.md` §2.  Measured radius s321: backlog empty.
- **XDIFF rows column (machine-checked like FIXTURE)**: APPROVED, POST-R1
  S3 item — backfill from the S2 snapshot, key by description, enforce
  all-or-nothing → `fable-answers-s321.md` §3.
- **op/list.t + op/pack.t are QUARANTINED** (op/list.t OOMs a 10 GB
  cgroup in 53 s, transpiler innocent); they appear as NOT-RUN rows WITH
  the #160 reason, never silently absent; diagnosis post-R1, user
  re-authorization first, suspect SBCL compile time →
  `fable-answers-s318.md` §8, task #160.  SHIPPED s320 as `%QUARANTINE` in
  `run-perl-suite.pl` — a SEPARATE mechanism from `%HEAVY` (which only runs
  a file solo).  Quarantined ≠ skipped: NOT-RUN is an UNEXPLAINED status,
  so these still fail the run and still show as a hole in the tsv.
- **Forked workers must never run the parent's END blocks or signal
  handlers** ($$ == $MAIN_PID guard): one signalled worker otherwise
  `rm -rf`s the SHARED tmpdir and kills its siblings → task #157, guard in
  `run-perl-suite.pl` + `sweep-perl-tests.pl`.
- **`File::Spec::Functions` DELEGATES to `File::Spec`** — it must never carry a
  second implementation.  The two copies drifted apart in BOTH directions and
  the `splitpath($p,1)` divergence silently changed test control flow
  (op/chdir.t's `skip("Already in t/")` never fired, so PCL RAN two tests perl
  skips) → task #167, `lib/File/Spec/Functions.pm` header.
  Corollary when probing a shim: **probe the CALL FORM the caller uses** — the
  method form was correct while the imported function form was broken.
- **A phase block (`BEGIN`/`END`/`CHECK`/`INIT`/`UNITCHECK`) is NOT a runtime
  statement**: it must be skipped when picking a block's TAIL statement, or a
  trailing `BEGIN {}` silently demotes the real last statement to void context
  → `docs/wantarray-context.md` §"a phase block is not a runtime statement",
  task #164 (open; sequence after the CLAUDE.md §8 VOID_CTX regression).
- **`system()`/backticks in an END block EAT the exit status** — `$?` at the
  end of the last END block IS the process exit code, so cleanup ENDs need
  `local $?`.  This silently zeroed run-perl-suite's exit code for every run
  it ever made → task #157.
- **A `use` inside an expression block (`do{}`/`eval{}`/anon-sub) HOISTS, and
  under v2 the hoist must not take v1's DEFERRAL path** — `_pending_hoisted_defs`
  is flushed by `_process_children`, which v2 never calls, so the `use` was
  silently dropped and only surfaced as an undefined function at run time →
  task #187, `Pl/Parser.pm` `parse_block_as_function` teardown
  (`_v2_owner`), guard `Pl/t/transpile-test-09.t`.
- **A `package` inside a do/eval block is a RUNTIME switch only** — it opens no
  CL section, so a hoisted `use` would import into the enclosing package.
  `p-use` therefore takes `:into "Pkg"`, emitted whenever the block's package
  differs from the section's → task #187.
- **An unmatched capture group is perl UNDEF, never raw CL nil** — raw nil means
  "the empty list" to `%p-flatten-list`, so a list ASSIGNMENT (`p-list-=`)
  silently shifted every later capture up a slot, while the ARRAY target
  (`p-array-=`) was correct.  `my ($d,$f) = $p =~ m{^(.*/)?(.*)}` put the
  filename in `$d` → task #188, both capture-collection sites in
  `cl/pcl-runtime.lisp`.
- **`_` is perl's stat-cache filehandle, and it is a BOUND VARIABLE in :pcl** —
  `-e $f and -f _` lowers the bareword to the bare symbol `_`, so `:pcl`
  exports a defvar holding a marker; every filetest resolves its operand
  through ONE funnel (`%p--path` → `%p-stat-arg`) that maintains the cache.
  PCL caches the PATH and re-stats where perl caches the stat BUFFER — a
  deliberate divergence, same answer outside a race → task #186.
- **`lib/File/Basename.pm` is a SHIM ONLY because `$_[N]` writes do not reach a
  plain `my` lexical** (`_strip_trailing_sep($dirname)` did nothing, so
  `dirname("/a/b/c")` answered "/a/b/").  Delete the shim when task #189 lands;
  do NOT "fix" it by blanket-boxing every lexical passed to a sub.
- **A sub Perl already knows about beats INDIRECT-OBJECT syntax** — `divide
  $text => 4` is `divide($text, 4)`, never `$text->divide(4)`.  PExpr's
  indirect pre-pass asks `_is_known_callable($name, 1)`, the same compile-time
  question `_bareword_subscript_autoquotes` asks for bareword array subscripts.
  The question must be PACKAGE-QUALIFIED there (`Widget::show` is not visible
  as a bare `show` from main — probed, the unqualified version broke it), and
  the prototype table cannot answer it because it is keyed by bare name →
  task #190; residue #191.
- **`utf8::unicode_to_native` / `native_to_unicode` are defined ONCE**, in
  `cl/pcl-runtime.lisp`.  `cl/pcl-test.lisp` used to define the same two names
  again for charset_tools.pl; since the TAP layer loads AFTER the runtime its
  copy silently won, so the function behaved differently depending on whether
  Test::More was loaded — and SBCL printed "redefining …" on stderr for 17
  files of the perl-tests sweep.  pl2cl/runtime stderr must stay clean: test
  harnesses merge it into the generated CL.
- **An UNQUALIFIED dynamic glob name resolves in the package IN EFFECT, not
  main** — `*{"_IS_\U$_"} = …` inside `package File::Path` installs
  `File::Path::_IS_MSWIN32`.  Both `p-glob-assign-dynamic` and
  `p-dynamic-typeglob` hardcoded "main" → task #192.  Follow-on #193: those
  same constants are then USED as barewords, and under `use strict` an
  undeclared bareword is a compile error, so by principle 9 anything that
  compiles is a CALL — PCL still emits some of them as strings (always true).

## s323 Fable rulings (2026-08-02, `fable-answers-s323.md`)

- **#189 `writes_args`: shape APPROVED, POST-R1** — detect in the callee body
  (lvalue set PLUS `foreach` over `@_`, lvalue substr/vec/pos, and ESCAPES:
  `\$_[N]`, `\@_`, `&callee;`, `goto &sub`, `@_` passed on — anything not
  provably read-only sets the flag), carry on `sub_info`, consume as a
  VarAnnotator `arg-to-writer` boxing reason at same-file call sites first.
  The "Cannot modify non-boxed value" warning STAYS as the backstop for
  undetected writers.  NO blanket boxing of call args, ever.  Delete
  `lib/File/Basename.pm` when it lands → `fable-answers-s323.md` §1.
- **#193 strict-subs bareword = a CALL, CONFIRMED (principle 9)** — fix at
  the string-FALLTHROUGH point, gated on strict-subs; pre-R1 attempt
  authorized, time-boxed, corpus-diff with every changed row examined;
  **stop-rule: if the trace lands in the #142 `$end_pars` region, STOP**
  (that fix is #153 `_reduce_term`) → `fable-answers-s323.md` §2.
- **#191 indirect-method-in-brackets: DEFERRED** — loud failure, rare
  syntax; re-raise on a real CPAN cause line → `fable-answers-s323.md` §3.
- **Suite-snapshot staleness rule**: a crash-fixing commit MARKS the snapshot
  stale (one session-log line, write-side); regeneration is required at
  QUOTE points (review round, release number, the R1 call), not per commit;
  next regeneration adds a `# taken-at: <commit>` header
  → `fable-answers-s323.md` §4.
- **USER (2026-08-02): R1's CPAN half gates on the FOUR-DIST baseline only**
  (`docs/cpan-scoreboard.tsv`, no regressions); the widened board is the
  post-R1 worklist, `IO`/`IO::Handle` shim first (23 of 48 FAILs)
  → `fable-answers-s323.md` §5.
- **USER (2026-08-02): fetching/unpacking CPAN dists for measurement is
  blanket-OK'd**; system-level installs still ask → `fable-answers-s323.md` §5.
- **USER (2026-08-02): the full `tools/run-perl-suite.pl --all` run (~15 min)
  runs every 3rd–5th change, NOT per change** — per change it is
  `tools/prove-core` + a targeted single-file suite run (positional
  t-relative paths); full run always once before committing a batch or
  quoting numbers.  Same shape as the s323 sweep cadence
  → `fable-answers-s323.md` §6, memory `feedback_sweep_cadence`.

## s324 (Fable, 2026-08-02): #193 + the two bugs under it

- **#193 DONE: under strict-subs an undeclared bareword before a BINARY
  operator is a CALL** — the strict gate the end-of-expression branch already
  had, added to its second copy (the followed-by-binary-operator branch in
  `handle_subcalls`; the #142 `$end_pars` branch untouched).  `=>` and `->`
  keep the string reading even under strict (fat-comma autoquote; class-name
  invocant — probed).  No-strict behavior unchanged.  Guard:
  `transpile-test-09.t` (#193 row + inverse row).
- **readdir/opendir semantics**: opendir treats its path as a DIRECTORY even
  without a trailing slash (merge-pathnames used to list the PARENT); entries
  are the child's OWN last component (a subdir used to come back as `""`);
  `.`/`..` are included; symlinks unresolved; **readdir is
  wantarray-sensitive** (`%WANTARRAY_SENSITIVE`, list context drains to a
  vector) → `cl/pcl-runtime.lisp` `%p-opendir-impl`/`%p-readdir-impl`,
  session-log s324.
- **A sole-ternary parenthesized lvalue is a SCALAR assignment** —
  `($c ? $a : $b) = V` gives V scalar context and the expression's value is
  the assigned value (never the list-assignment COUNT), so while-loop
  implicit defined() applies; emitted as `(box-set (p-if …) V)` via
  `_sole_ternary_lvalue_id` in BOTH '=' emitters, with box-set arms in both
  auto-defined matchers → defins.t t10, session-log s324.

## R1 (2026-08-02)

- **R1 IS CALLED (user, 2026-08-02, session s325) at `dc6ce64`, tag `R1`.**
  R1 = correctness-by-gate.  The checklist that grounds it: Pl/t gate
  `Result: PASS` 125 files / 4479 tests; full sweep 18461/926 with
  sweep-diff 0 new / 0 fixed vs the 689-row baseline; four-dist CPAN board
  zero regressions (one improvement, Try-Tiny `00-report-prereqs.t`
  FAIL→PASS via #193, baseline advanced); suite snapshot current and
  stamped (`# taken-at:` in `docs/perl-suite-run.tsv`; the 7 TIMEOUT rows
  re-verified at `--timeout 300`, task #195); artifacts regenerated at gen
  v2-95 (bodies byte-identical) → numbers in `docs/session-log.md` s325.
  **The post-R1 backlog is now ACTIVE in its ruled order**
  (`fable-answers-s323.md` §7): IO/`IO::Handle` shim → #189 `writes_args`
  (+ delete `lib/File/Basename.pm`) → #163 → #176 step 2 → #184 → #185 →
  #159 → #150 → #152 → E4.1/E5 per `docs/v2-opus5-execution-plan.md`.

## s326 (Opus, 2026-08-02): the IO/IO::Handle shim (#197) and the seven bugs under it

- **`use IO::Handle` works: `lib/IO.pm` supplies the XS half in plain Perl**
  (constants, `flush` via the `$|` idiom, `getline`/`getlines`/`gets`,
  `setbuf`/`setvbuf` mapped onto autoflush, `error`/`clearerr`/`untaint`
  answering 0 because PCL latches no error flag and has no taint), and core's
  own pure-Perl `IO/Handle.pm` runs on top of it.  `sync`/`blocking`/`ungetc`
  CROAK by name — no fsync, no fcntl, no pushback — rather than returning a
  plausible value (rule 12).
- **`lib/IO/Handle.pm` is a WORKAROUND, not a shim by choice** — core's file
  with exactly `autoflush` and `printflush` rewritten to save/restore the
  selection explicitly instead of via `SelectSaver`, whose contract is
  restore-on-DESTROY and PCL never calls DESTROY.  **Delete it when
  DESTROY-at-scope-exit lands (task #198).**  Same pattern as
  `lib/File/Basename.pm`.
- **`use Foo ()` and `use Foo qw()` must NOT call import** (verified against
  perl both ways); a bare `use Foo;` still imports with no args.  Decided
  BEFORE the import-args branch, because an empty list still parses to
  `(vector)` → `Pl/Parser.pm` `_use_has_empty_import_list`.
- **A statement modifier on `require` gates it** — `require Foo if COND;` is a
  runtime conditional, not an unconditional hoisted require.  PCL dropped the
  modifier, so `require VMS::Stdio if $^O eq 'VMS'` (File::Temp) died on Linux
  → `_include_statement_modifier`; if/unless only.
- **Perl flushes EVERY handle at exit and before fork, not just the standard
  three** — an unclosed `open my $fh,'>',$f; print {$fh} …` silently lost its
  buffer.  Registry `*p-open-output-streams*` (weak), flushed from the exit
  hook AFTER the END blocks and from `p-fork`.
- **`select(FH)` is real and `$|` is per-handle** — one-arg select sets the
  default output handle and returns the previous designator (so
  `select((select($fh), $|=1)[0])` works); setting `$|` true flushes that
  handle immediately and makes later writes flush.  Both were stubs: select
  returned a constant string and `$|` was one global flag nothing consumed.
- **open() on a GLOB handle must not overwrite the scalar** — `IO::Handle->new`
  / `\*FH` / `Symbol::gensym` produce a (usually blessed) globref; the stream
  belongs in the glob's IO slot, keyed by the glob's name.  PCL box-set the
  stream over it, so `ref($io)` stopped being `IO::Handle` and later method
  calls went elsewhere.  `%p-close-impl` now resolves through `%p-resolve-fh`
  (one resolver, every handle shape) with `%p-forget-fh` mirroring
  `%p-install-fh`.
- **A symbolic container designator resolves in `exists`/`delete` too** —
  `$$p{k}` and `$$p[i]` (and `"Pkg::"` stashes) were resolved on the
  read/write paths only, so `exists $$p{k}` silently answered NO and
  `delete $$p{k}` crashed SBCL's GETHASH.  One resolver:
  `%p-designator-hash`/`%p-designator-array`, and `p-ensure-hashref`/
  `p-ensure-arrayref` delegate their string branch to `p-cast-%`/
  `%p-symref-array` instead of keeping a second copy.
- **`delete $ref->[i]` lowers to `p-delete-array`** — `exists` had both ref
  arms all along, `delete` only the hash one, so the array form fell through
  to a one-argument `p-delete` and crashed on arity (both pipelines).
- **Capture-Tiny is NOT cleared by the shim**: its 23 FAILs move off the IO
  cause onto two string-eval bugs (task #199) — a prototyped sub defined by
  `eval "sub NAME(&;@){…}"` is never installed, and a p-eval-thunk reaches CL's
  `push` with too many arguments.  The "23 of 48" estimate in #197 assumed one
  blocker; there were three.

## s327 (Fable, 2026-08-02): s326 review + the post-#197 order

- **s326 APPROVED with two review fixes (`87f23f6`)** — both found by probing
  the seams the diff touched: close() on a name-string handle cleared the
  scalar (%p-forget-fh now mirrors %p-install-fh's name-string case), and the
  `use Foo VERSION LIST` version-skip was dead code (PPI's `$stmt->version` is
  empty for module versions) so the import args emitted a PARSE ERROR progn —
  version now recognized positionally, with the no-operator-follows guard
  (`use Foo 1.5, "x"` keeps the number a list element; probed).
- **ORDER RULING: #199 runs BEFORE #189** — the two Capture-Tiny string-eval
  bugs are the COMPLETION of post-R1 item 1, whose justification was those 23
  board FAILs.  Fix the eval-string prototyped-sub install first (the
  API-existence bug, a silent-nothing), then the p-eval-thunk→push arity;
  both are #79's area; the #142/#153 stop-rule applies if a trace lands in
  `$end_pars`.  Exit criterion: Capture-Tiny board re-run with per-file causes.
- **#198 (DESTROY at scope exit) is PARKED, design-first** — it interacts with
  #163's referent-kind tag and E5 boxed aggregates; Fable designs it after
  #163 lands (or a CPAN cause line forces it earlier).  Payoff when done:
  delete `lib/IO/Handle.pm`, unblock Try::Tiny/Scope::Guard/File::Temp
  /SelectSaver.

## s328 (Opus, 2026-08-02): #199 — four Capture-Tiny blockers, all silent

- **`goto EXPR` where EXPR is a CODE REF is a TAIL CALL, not a computed
  label** — `goto \&NAME`, `goto $coderef`, `goto $h->{cb}` all route through
  `p-goto-sub` via `%p-goto-target`.  `p-goto-computed` used to be a `defun`
  documented as "not implementable in CL; silently ignore": the sub returned
  undef with no error and no output.  A NON-coderef operand (a real computed
  LABEL) now **names itself on stderr and falls through** — announced-not-
  silent, the #155 tie shape, deliberately NOT the rule-12 die: measured, a
  die costs `perl-tests/state.t` 88 verified passing rows (157/166 → 69/166),
  because its one computed goto sits two thirds up an otherwise-passing file,
  and any CPAN module using one would abort entirely.  The old behaviour's sin
  was the silence, not the fall-through.  Guard `Pl/t/goto-sub-phase-01.t`
  rows 1-6.
- **A `local(...)` inside a BEGIN/END/CHECK/INIT/UNITCHECK block must close
  its `let` before the block does** — `_process_special_block`'s `$process`
  closure calls `_process_children`, which (unlike `_process_block` and the
  sub-body path) does NOT close local-lets.  v1 therefore emitted ONE PAREN
  TOO FEW and the block's `(push (lambda …) *end-blocks*)` swallowed every
  later top-level form; File::Temp's `END { local($.,$@,$!,$^E,$?); … }` made
  the whole module fail to load ("too many elements in (push …)"), taking
  Capture::Tiny with it.  v2 refuses this shape loudly ("statement fallback
  left 1 open scope(s)") and falls back to v1 — that gate is unchanged.
- **`require_ok`/`use_ok` must actually LOAD the module.**  The old
  `cl/pcl-test.lisp` comment claimed the transpiler had already resolved the
  `use` at compile time; nothing under `Pl/` mentions either name, so they
  reported ok and loaded nothing — every row after them failed for no visible
  cause.  Both now load (`%test-load-module`, path-ish name → `p-require-file`)
  and report the real result; `use_ok` imports `:into *pcl-current-package*`,
  with Test::More's "lone numeric arg is a VERSION" rule.
- **`Test::More->builder` exists and answers ONLY the output handles**
  (`output`/`failure_output`/`todo_output` → PCL handle-name designators
  "STDOUT"/"STDERR"/"STDOUT", which `binmode`/`print {…}`/`fileno` accept).
  Every other Test::Builder method is deliberately absent so dispatch dies
  naming it — a stub would corrupt a file's counts.  22 of Capture-Tiny's 24
  t-files died on the missing `builder` alone.
- **The embedded-`my` veto does not fire for a sibling sub that DECLARES the
  same name** (`Pl/Parser2.pm` `_sub_declares_name`).  The veto falls back to
  "the old forward-defvar'd global", which is never emitted when every other
  mention is itself a declaration — so `sub s1 { my $fh; … }` next to
  `sub nf { open my $fh, … }` left BOTH unbound (`Utils::$fh is unbound`).
  A sub that only REFERENCES the name still vetoes (probed; inverse guard).
- **Board result**: Capture-Tiny 1 PASS / 0 PARTIAL / 23 FAIL → **4 PASS /
  4 PARTIAL / 16 FAIL of 24**; the 16 residual causes are measured per file
  in task #201 (File::Temp template check, tie-on-glob arity, closed-stream
  writes, `local $ENV{...}`).

## s329 (Fable, 2026-08-02): s328 review + rulings on its asks

- **Rule-12 boundary RULED: DIE vs ANNOUNCE is decided by whether a VALUE
  flows onward.**  A missing case that should have produced/written a value
  the program consumes → DIE naming it.  An EFFECT-ONLY missing case (jump,
  tie, attribute) in code that otherwise runs correctly → ANNOUNCE on stderr
  + `not-supported.md` entry; unclassifiable-in-a-minute counts as
  value-producing.  De-dup announcements once per (site, operand) per
  process via ONE shared helper, introduced at the start of #152 — not
  per-site hashes.  → `fable-answers-s328.md` §1; gates how #152 runs.
- **Sweep TOTAL-passing is a GATE, machine-checked**: the total must not
  fall vs baseline; any fall explained per file.  `sweep-diff` grows a
  fourth bucket (LOST = baseline-passing rows the run did not produce),
  non-empty LOST = not clean.  Must land BEFORE #152.  →
  `fable-answers-s328.md` §4, task #204.
- **#202 runs FIRST (before #189)** as harness-trust calibration; after it,
  re-verify the four-dist baseline PASSes that contained fake assertions
  (File-Which 01_use.t is nothing but one).  use_ok's TAP description must
  match Test::More's (`use Foo;` — no import list).  →
  `fable-answers-s328.md` §3.
- **#201 File::Temp layer: probe the failing PREDICATE, not the module** —
  run the exact template check under perl and PCL; PCL-only divergence →
  fix the core mechanism (no File::Temp name under `Pl/`/`cl/`), both-fail →
  fix the shim.  Same procedure for the rest of #201.  →
  `fable-answers-s328.md` §2.
- **`goto` restores the ORIGINAL caller's `*wantarray*`** (s329 review fix;
  pre-existing hole in `p-goto-sub`, both spellings ran the target in the
  goto statement's own context).  Guard `goto-sub-phase-01.t` (17 rows).
- **Deep `goto &sub` chains are BOUNDED under PCL** (binding-stack, ~10^5),
  where perl is constant-depth → `not-supported.md` §goto-trampolines.
- **Embedded-`my` veto exemption requires NO FREE REFERENCE** — declaring an
  inner shadow is not enough (`_sub_freely_references_name`: doc-order +
  block-containment; compound-header `my` scopes to the body; `<$fh>` and
  interpolations count as uses — Symbol-only scans cannot see them).
  Residual poisoned-name defvar hole (crashes loudly in every era) → task
  #205.

## s330 (Opus, 2026-08-02): #202 — the TAP layer audited for unfalsifiable assertions

- **An assertion that cannot EVALUATE its claim reports `not ok` naming the
  reason — it does not die.**  Rule 12's "a verdict is a value" reading would
  abort the file and cost every row after it (the s328 88-row lesson applied
  to the instrument).  The one exception is `plan()`, which dies: there is no
  row to attach a diagnostic to and the file's whole count claim is void.
  Full argument + inventory → `docs/tap-assertion-audit.md`; ratification
  asked in `opus5-review-requests-s330.md` §1.
- **`unlike` could not fail**: a scanner error was swallowed into a PASS
  (`(error () t)`).  `like`/`unlike` are now ONE function over one matcher,
  and an unusable pattern is `not ok` in both directions.
- **`eq_hash` had never worked** (double unwrap → type-error on every
  hashref, killing the file).  **`cmp_ok` manufactured verdicts** for `<=>`,
  `cmp`, `=~`, `!~` (all now implemented).
- **TAP descriptions are join keys, so they must be Test::More's**: `use
  Foo;` without the import list; isa_ok's four kinds get four wordings;
  can_ok names the class and the single method.  Zero baseline rows keyed on
  the old texts (checked).
- **`skip_without_dynamic_extension` asks the loader** instead of skipping
  unconditionally.  **A diagnostic the harness prints mid-run can split a TAP
  row**: its first version cost undef.t 35/35 PASS → 30/35 PARTIAL with
  nothing failing, because a failed load's stderr banner landed inside a row
  (the sweep folds stderr into stdout).  Probe now runs with `*error-output*`
  bound to a broadcast stream — a trap #152's announcements must avoid.
- **`scalar()` never dereferences** (runtime, found by the audit's probes):
  `p-scalar` unboxed first, so `scalar($aref)` was the element COUNT and
  `scalar(\5)` was the referent.  An array variable is a raw adjustable
  vector and is never boxed, so a box holding a vector is unambiguously a
  ref; the hash branch already carried that guard.  +7 sweep rows; array.t
  t128 was passing only because both sides flattened to undef (blessed, with
  the cause, into `docs/fail-baseline.tsv`; 689 → 683).
- **FILED**: #206 `UNIVERSAL::isa` ignores the reftype rule (gate on #163);
  #207 `which_perl`/`run_perl` are unverified stubs; #208 the CPAN board
  `.tsv` baselines drifted since s322 (verified NOT from this session — a
  HEAD worktree gives identical boards).

## s330b (Opus): #204 — the sweep's TOTAL-passing gate is machine-checked

- **`tools/sweep-diff.pl` has a fourth bucket, LOST**: per file, baseline
  PASSING rows the current run did not produce, read from a blessed
  `docs/pass-baseline.tsv` (`save-status` writes it).  Non-empty LOST = the
  run is NOT clean, same exit code as NEW.  Every run prints
  `TOTAL passing: baseline N, current M`.
- **A FULL sweep runs the gate itself and exits with its verdict**
  (`sweep-perl-tests.pl` with no file arguments; `--no-gate` opts out).  A
  sweep of named files stays informational — a partial run is not comparable
  to a whole-corpus baseline.
- **Two anti-silence rules in the implementation**: when no pass baseline can
  be found the tool prints `LOST: NOT CHECKED — …` rather than nothing, and
  an explicit `--pass-baseline` that does not exist is FATAL, never a silent
  fall-back to the default file.  A check that goes quiet when it cannot run
  is indistinguishable from one that passed.

## s330c (Opus): #189 writes_args — the `@_` aliasing divergence closed for known callees

- **A sub that writes through `@_` is detected in its BODY** (`Parser2::
  _sub_writes_args`), the fact rides `sub_info` as `writes_args`, and
  VarAnnotator turns it into an `arg-to-writer` boxing event at that sub's
  call sites — the same marking `chomp $x` already uses.  Only files with
  such a sub pay.  Conservative by construction: an `@_`/`$_[N]` occurrence
  the scan cannot prove is a read (including `\$_[N]`, `\@_`, `&callee;`,
  `goto &sub`, handing `@_` to an unknown callee) sets the flag.  The runtime
  "Cannot modify non-boxed value" warning STAYS as the backstop for coderef
  calls, method dispatch and cross-file callees.
- **`lib/File/Basename.pm` DELETED** — core's copy now answers `dirname`
  correctly under PCL.  Guard `Pl/t/writes-args-01.t`.
- **`s///` / `tr///` bound to an ARRAY or HASH ELEMENT write the element**
  (`p-aref-box`, not `p-aref`).  Before: a silent no-op on an array element,
  a "Cannot modify non-boxed value" warning on a hash element — for one of
  perl's most common idioms.
- **Element VIVIFICATION follows perl exactly**: taking the lvalue creates
  the element, so only the ops that perl treats as lvalues take it — `s///`
  and a MODIFYING `tr/x/y/` vivify; a count-only `tr/N/N/` (identical lists,
  no d/s/c; an empty replacement replicates the search list), any `/r` form
  and a plain match do not.  Getting this wrong cost `perl-tests/tr.t` two
  passing rows — **caught by #204's LOST bucket on its first live run**,
  which is what that gate is for.

## s332 (Fable, 2026-08-03): the s330/s331 asks ruled + the writes_args scan's blind spot closed

- **Harness verdict rule RATIFIED**: inside the TAP layer, an assertion that
  cannot evaluate its claim reports `not ok` naming the reason; only
  `plan()` dies.  Not an exception to the s328 §1 boundary — an adverse
  verdict IS the produced value; a die in the instrument costs every row
  after it.  #152 inherits this split: harness → adverse verdict, runtime
  → s328 §1.  → `fable-answers-s331.md` §1.
- **Announcements must never interleave with a TAP stream** (a mid-run
  stderr banner SPLIT a row: undef.t 35/35 → 30/35 with nothing failing) —
  hard rule for #152's `%p-announce-unsupported`: emit before the plan or
  outside the folded stream.  → `fable-answers-s331.md` §2.
- **`like`/`unlike` with a plain STRING pattern follows test.pl (interpolate),
  both callers** — accepted divergence from Test::More; a dist that passed
  upstream CI cannot contain the failing spelling.  Revisit on a real CPAN
  cause line.  → `fable-answers-s331.md` §3.
- **UNSTABLE + LOST stay independent buckets; no promotion rule** — UNSTABLE
  is descriptive, not exculpatory; the TOTAL/LOST line decides, and it
  already fails the gate.  → `fable-answers-s331.md` §5.
- **No whitelisting non-core callees in the writes_args scan (9a stands);
  the `writes_args => 0` provably-safe-callee refinement is approved in
  principle, NOT scheduled** — revisit on measured boxing cost (R2).  →
  `fable-answers-s331.md` §6.
- **The element-vivification rule is probe-CONFIRMED** on all seven corners
  incl. `/d`+empty replacement (DELETE → vivifies) and `/s`,`/c` on equal
  lists (vivify); `_rhs_writes_match_target` is the normative statement.
  → `fable-answers-s331.md` §7.
- **writes_args scan: four probe-found FALSE NEGATIVES fixed (gen v2-100)**
  — implicit-`$_` writers have NO Symbol token (`s/b/X/ for @_`,
  `for (@_) { s/// }`), plain-token roots were skipped entirely (even
  `$_ = uc $_ for @_`), and map/grep alias `$_` like foreach but sat in the
  value-consumer list.  All were silent-wrong with no backstop warning.
  Rule for any Symbol-keyed scan: **probe the spellings that produce NO
  Symbol token — implicit `$_` is the standing example.**  Guards:
  `Pl/t/writes-args-01.t` rows 14–15.  → `fable-answers-s331.md` §9.

## s333 (Opus, 2026-08-03): #163 — a reference's type and address are the REFERENT's

- **`%p-ref-referent` is the ONE rule** for "what does this reference point
  at", and `is-ref` on the p-backslash wrapper is its only discriminator.
  **No new box slot was needed**: the ruled referent-KIND tag would have
  answered the same question the flag already answers, at one word per box
  on every scalar in every program.  A value reaching a stringifier is
  either the wrapper itself (`\$x` into `print`, an element, a raw param) or
  a variable box holding one; box-sv counted levels instead, which is why the
  same reference printed `SCALAR` through a variable and `REF` straight into
  `print`.  → `docs/ir-spec.md` §2.5 (normative), guard
  `Pl/t/ref-identity-01.t`.
- **The address is the referent's, never the wrapper's** — a wrapper is fresh
  per `\`, so `\$x == \$x` was false and one variable printed two addresses.
  `p-ref`, `box-sv`, `stringify-value` and `box-nv` now read the one rule, so
  the word and the number agree on every path.
- **`ref(\$aref)` is `REF`, not `ARRAY`**: p-ref's aggregate arms read the
  referent's *value* when handed a wrapper directly.  Fixing that widened
  `\$aref`/`\$href`/`\$qr`/`\$cref` to REF; the INVERSE (`\@a`, `\%h`,
  `$list[0]` holding an aggregate wrapper) is guarded row by row.
- **A reference's string is NOT cached** on the holding box: `SCALAR` vs
  `REF` depends on what the referent holds *now* (`my $r=\1; my $rr=\$r;` →
  REF; `$r=5` → SCALAR), and the referent is a different box whose writes
  cannot invalidate a cache here.  Same reason box-nv never caches an
  address-based NV.
- **#154's two shapes are closed** (`@$sref`, `$sref->{k}`, `%$sref`,
  `$sref->[0]` → perl's fatal): `%p-scalar-referent-p` separates a ref to a
  plain scalar from the representation layer #154 documented as
  indistinguishable.  The ref-to-REF half stays lenient **because the parser
  drops the outer level of `$$refref->{k}` (#211, filed s333)** — the runtime
  leniency is a workaround for that parse bug, not a semantic choice.

## s335 (Fable, 2026-08-03): s333+s334 asks ruled; two byte-identical compile-time fixes shipped

- **s333 §1 CONFIRMED**: no `ref-kind` slot — the s318/s320 tag ruling is
  superseded by the better measurement (see the annotated bullet under
  "Aggregate state model" above).  §2 ACCEPTED: ref strings stay uncached.
- **#211 is parked behind #153** (term-machinery region, no guard-patches);
  its leniency covers exactly the two measured spellings — a NEW shape
  wanting leniency needs its own probe and task, #211 is not a blanket
  citation → `fable-answers-s334.md` §s333-3.
- **Sweep gate vs load noise (#204 follow-up)**: record min MemAvailable
  beside any LOST report, AND re-run a LOST file once at `--jobs 1`; the
  serial numbers REPLACE that file's rows for the verdict, the report prints
  both.  Whitelisting noisy files REJECTED; "operator re-runs" REJECTED →
  `fable-answers-s334.md` §s333-4, task #215.
- **#213 re-scoped**: the TIME was `_ends_in_comment` (fixed s335,
  byte-identical); remaining = cosmetic bytes (clamp approved, bump
  generation) + recursion depth (real fix = `let*` runs at E5; depth-keyed
  defvar flattening REJECTED) → `fable-answers-s334.md` §1, and the Coding
  rules bullet above.
- **W10 spanning-rename loops converted to the #184 rule** (the two
  remaining per-token `_ref_shadowed` callers): 86.6 s → 0.98 s on a
  two-package 200-statement file, emission byte-identical, guard rows in
  `Pl/t/parser2-02.t` → `fable-answers-s334.md` §5.
- **#152 audit scope is CLOSED at the runtime** (s338): pcl-test done under
  #202; skip-registry is data; pack/mro artifacts are audited via their Perl
  sources' oracles; ONE bounded extension = constant-default grep of
  `cl/pcl-xs.lisp` (task #222).  Widening to non-constant default arms
  REJECTED — rule 12 is prospective; fuzzer + sweep TOTAL are the net →
  `fable-answers-s337.md` §3.
- **Default-off perl diagnostics are ABSENT, never unconditional** — PCL has
  no `use warnings` model (grepped s337c); minimal design filed UNSCHEDULED
  as task #221, triggered by the first concrete consumer →
  `fable-answers-s337.md` §5a.  The print-on-unopened measurement lives in
  `%p-out-fh-or-fail`'s docstring; do not retry it.
- **Closed-handle value APPROVED** (defined not-writable value left by
  `close`), queued behind #153/E4.1 as task #220; same magic-cell family as
  #144, shrinks #198 → `fable-answers-s337.md` §4.
- **#215 gains a warm-first half** (transpile one file before fanning out;
  per-worker cache dirs REJECTED); serial re-run stays as backstop →
  `fable-answers-s337.md` §5c, task #222.
  **SHIPPED s339, and the race had a CAUSE**: `p-load-module-cached`'s
  **`.lisp` branch (the DEFAULT) wrote the cache file IN PLACE** while the
  FASL branch beside it already used pid-temp + atomic `rename-file`.
  Measured: SBCL's `:supersede` truncates and writes the real file, so a
  worker whose `p-cache-valid-p` saw the fresh mtime `load`ed a half-written
  module.  Both branches are atomic now; the sweep also warms one file first
  and reports min MemAvailable + a serial re-run of any LOST file (the serial
  verdict replaces, both shown).  Cold-cache `--jobs 8` repro now gives
  do.t 65/3/5 and a clean gate.
- **XS callbacks cannot DIE — the announce IS rule 12's loud ending there**
  (s339 audit of `cl/pcl-xs.lisp`, task #222): `with-xs-guard` converts every
  condition into the on-error constant BY DESIGN (pclxs rule O4, nothing
  unwinds into C), so `%p-unsupported-value` in a callback would be silently
  downgraded.  `xs-ref-type` now enumerates SCALAR/LVALUE and announces any
  other reftype (REGEXP today) as "answered as a scalar reference"; the
  contract enum has no code for it.
- **getprotobyname/getprotobynumber read `/etc/protocols`** (lazy, four-entry
  table only when unreadable), wantarray-sensitive, EXACT name-or-alias match
  (`Tcp` misses), scalar context = number by name / NAME by number →
  `fable-answers-s337.md` §4-secondary, s339.  **Fallback-host divergence
  ACCEPTED** (no `/etc/protocols` → the 4-entry table may differ from perl's
  NSS answer; not a validated host class) → `fable-answers-s339.md` §4.
- **XS announce-not-die RATIFIED for the whole of `cl/pcl-xs.lisp`; NO
  die-across-the-boundary mechanism** (PS_DIED stays the only die channel —
  it is for contracts that carry a Perl-die outcome, not for answerable
  cases; revisit only if the pclxs contract grows an UNKNOWN code) →
  `fable-answers-s339.md` §3, s340.
- **Baseline hygiene (task #223): fail-baseline rows leave by EDIT with the
  cause named, NEVER by regeneration; pass-baseline (a generated file)
  re-blesses ONLY from a gate-green run after a per-file audit** — every
  delta attributed, no file down, an unexplained delta is a finding.
  `save-status` stamps `# taken-at: <sha> <date>`; readers skip `#` lines →
  `fable-answers-s339.md` §5, s340.
- **The `+8` was a STALE BLESS, not drift (#223 executed s341).**  Measured,
  not inferred: at `73d43ac` — the commit that installed
  `docs/pass-baseline.tsv` — the six drifting files already produced the
  CURRENT numbers, so the blessed `_status.tsv` came from a run older than its
  own commit.  Attribution: push.t/unshift.t +1 each = **#159** read-only
  arrays (measured across `1b0a7e4`, where the row goes SKIP→PASS); scalar.t
  +2 = the **s333** `scalar()` fix (the two rows s333 left un-edited);
  ref.t +2 / state.t +1 / tr.t +1 = **no code cause** — all three are PARTIAL
  (abort) files that gave the higher count at every commit tested, i.e. the
  baseline row was taken from a memory-pressured run (the s337 ref.t
  184-vs-186 oom measurement).  **Rule that follows: a baseline blessed
  without a `taken-at` stamp cannot be attributed later, only re-measured.**
- **`our $x OP= …` is a legal `our` declaration** — the initialiser may use any
  assignment operator (perl's Exporter.pm opens `our $Verbose ||= 0;`); the
  gate asks `TokenUtils::is_assign_op` (the #140 one-true set), s341b.
- **The rule-2 live-v1 audit has two traps** (s341b): a `git worktree` at an
  older commit SHARES `~/.pcl-cache`, so its entries pollute a `pipeline=v1`
  grep — filter on the current `gen=`; and `PCL_V2_VERBOSE=1` goes to stderr,
  which a sweep folds into TAP, so the eval-mode half needs a file
  side-channel, not a global env var → `session-log.md` s341b, task #225.
- **The v1 fallback is MASKING a v2 defect in Math::BigInt** (#224, s341b): the
  v2-compiled module recurses (tie `STORE` → `round_mode` → symbolic-ref write
  → `STORE`) until the binding stack dies, costing pack.t everything (~70 s →
  not finishing at 600 s).  The cond-my poison narrowing that would move it to
  v2 is correct, probe-verified, and parked on `wip/s341-condmy-narrowing`; it
  lands WITH #224, never before.  **Both landed s342** — the narrowing is on
  main, BigInt is on v2, pack.t is 66 s / 5636 passing.
- **#224 FIXED s342 — a tie handler runs with ITS OWN cell's magic off.**  Perl
  (mg.c `save_magic`/`restore_magic`) turns an SV's magic off for the duration
  of any of its magic callbacks; PCL now does the same by swapping the proxy's
  `saved-value` (which IS the raw slot) into the box for the call.  Three
  companion rules probed the same session: a write hits the raw slot BEFORE
  `STORE` runs, a `FETCH` result is written BACK to the raw slot, and re-tying
  REPLACES the magic instead of nesting proxies.  `tied()` still reports the
  object while magic is off.  Normative: `docs/ir-spec.md` §2.2b; guard
  `Pl/t/tie-01.t`.
- **The live v1 share is SIX families, not zero — E4.1 §5a.2 is unsatisfied**
  (#225 DONE s342c, `docs/v1-live-share-audit.md`; tasks #226–#230).  Measured
  60 v1 routes on a cold cache (24 sweep + 36 CPAN board).  **F5 cleared s342d
  (#229) → five families left**, board 36 → 31 events.
- **The live-v1 audit's populations are sweep + board + the Pl/t gate ITSELF,
  and the E4.1 flip needs zero TODO events across all THREE** (RULED s353,
  `fable-answers-s352.md` §1.1).  s342c never measured `Pl/t/`, which hid 27
  gate routes — 15 of them Moo/Sub::Quote — behind a "ZERO events" ruling.
  Gate families M1–M6 = tasks #247/#248, all pre-flip, order M3→M6→M2→M1→M4→M5.
  **All six SHIPPED (M3/M6/M2/M1 s353, M5 s353c, M4 s354): the Pl/t gate is at
  ZERO unaccounted TODO events** — the 2 that remain are the DELIBERATE
  inverse-guard rows in `transpile-test-09.t` (M1-predicate refusals, on
  #242's step-2 rephrase list), and 5 DIE-class events are v2 correctly
  reporting a perl error (exempt, they self-resolve at the flip).
- **E4.1 STEP 2 IS DONE — THERE IS ONE PIPELINE** (#242, s356).  `PCL_V1`,
  `PCL_V1_FILES`, `parse_with_fallback` and the `PCL_V2_AUDIT_LOG` side-channel
  are gone; `pl2cl` has a single `parse_source` entry and an unknown transpile
  mode is a loud error rather than a silently-dropped option.  `Pl::Parser` is
  still LOADED (v1's expression seam runs inside v2) — the unreachable
  file-level chunks are #243.  What the flip required and got:
  **(a)** the four ruled refusals rephrased perl-shaped and trappable
  (`PCL: unsupported in string eval: …` reaches `$@` with perl's own
  `at (eval N) line M.` suffix), each with a `docs/not-supported.md` entry —
  multi-switch, trailing-declaration, F6 oversized run form, and the M7
  our-alias residue; **(b)** `--lenient-ppi` accepted-and-inert with the PPI
  failure now **naming the file** (§5a.4); **(c)** the `#228` `[perl #129069]`
  registration; **(d)** the cache key's pipeline component frozen to `"v2"` so
  this generation's paths keep hashing where they did.  A `pl2cl server:`
  prefix that was leaking into `$@` was removed — the transpiler's message IS
  the error.  Verified: corpus emission **identical across 111 files**, gate
  131/4658 PASS, sweep GATE clean at 18499 (+0 vs the edited baseline).
- **THREE-POPULATION ZERO IS MET (s354 measured, s355 closed the board).**
  Pl/t clean (above); the sweep clean at 17 events, all named, and **one
  BETTER than the blessed 18** — M1's widened collapse absorbed the sweep's
  lone multi-switch event; the CPAN board clean after #251/M7, its only
  remaining TODO being the ruled true-multi-switch.  §5a.2's precondition
  for the E4.1 flip is satisfied.  Also recorded: **the board has no blessed
  Moo baseline** — `docs/cpan-board14-s343.tsv` covers Role-Tiny but not Moo.
- **An `our` alias runs to the end of its block OR to the next declaration
  of the same name — so a re-declaration ENDS the requalification, it does
  not defeat it** (#251/M7, s355, `_requalify_block_our_after_pkg_switch`).
  The pass refused whenever the switched region re-declared the name, which
  gated ordinary Perl: one bare block declaring `our @ISA` in each of four
  successive packages (Role-Tiny `subclass.t`; perl runs it 6/6).  Two
  fixes, both the M4 lesson again: **truncate** the region at a block-level
  `PPI::Statement::Variable` (its binding runs to the block's end, so it
  partitions cleanly — the tail gets its own turn with its own decl_pkg),
  and make the re-declaration test **sigil-exact** (`foreach my $d` binds
  the SCALAR and must not end an `@d` alias).  Measured: board FAILs
  `Moo/accessor-default.t` 0 → **PASS 40 ok** and `Role-Tiny/subclass.t`
  0 → 4 ok, no row anywhere else changed, corpus emission identical across
  111 files.  **The one surviving refusal is a re-declaration NESTED in an
  inner block or sub** — there the alias RESUMES after the inner scope,
  which a truncation cannot express; it is the shape step 2 must rephrase,
  and its v1 fallback is silently wrong today (probed), so the flip
  improves it.  Guards: `Pl/t/transpile-test-04b.t`.
- **The spanning-lexical RENAME resolves variables canonically, exactly like
  the CHECKER** (M4, s354, `Parser2.pm` `_rename_spanning_lexicals`).  Two
  sites had stayed bare-name: the declaration count (so `my %mix` beside
  `my $mix` read as a re-declaration of `$mix` and refused → the checker then
  died → whole-file v1), and the span test itself (so a sibling `my @x` used
  in a later segment promoted `$x` gratuitously).  Both now go through
  `->symbol` / `_canon_refs_in` — the invariant being kept is *the rename
  never refuses a name the checker will die on*, and the pass must not rename
  one it won't.  Family USES were already blessed as safe here by M-F; the
  matching family DECL simply had not followed.
- **Sub::Quote's `my …; my …; package X;` eval shape collapses via a WHITELIST
  leading-`my` predicate on #226's collapse — never general multi-section
  assembly** (RULED s353, `fable-answers-s352.md` §1.2): accept only
  initializers built from lexicals/magic vars/literals; anything unrecognized
  refuses.  True multi-switch (`package A; …; package B;`) stays a ruled
  refusal; s345 §2's "ZERO measured events" premise is SUPERSEDED for the
  leading-statements shape.
- **An eval's trailing `our`/`my` declaration has a VALUE, and it is whatever
  perl gives** (#227, s342f): `our NAMES [OP= RHS]` → the assignment expression
  (or, with no init, the READ — v1 answered with the variable NAME, a
  silent-wrong); `my ()` → the EMPTY LIST, emitted as the `(progn)` a bare `()`
  lowers to (`(p-undef)` is a 1-element list, `(vector)` an ARRAY ref — both
  measured wrong); a bare multi `my ($c,$d)` → the LIST of its names, lowered
  through the expression machinery with `'inherit'` so both context rules come
  for free.  `eval "our $VERSION = '…'"` is a routine module idiom, and it was
  the entire CPAN half of audit family F2.
- **A BINDING of a name is not a use of it** (s342d, #229) — the same mistake
  made twice, in two different checks, each costing a perl CORE module a
  whole-file gate.  `$attrs{$_}` is a slot of `%attrs`, so a TEXT scan for
  `$attrs` in a `my $attrs = …` initialiser is wrong: ask PPI's `->symbol`,
  which canonicalises `$a[0]`→`@a` and `$a{k}`→`%a`.  And `my ($vobj, $err);`
  DECLARES `$err`, so it is not evidence that a package global `$err` is live
  (`our` still is).  Climb to `PPI::Statement::Variable`, not merely the
  nearest `PPI::Statement` — for `my (…)` that is the Expression in the parens.
- **A `pipeline=v1` cache grep UNDER-COUNTS the live v1 share** (s342c): it is
  blind to every route whose output never becomes a cache entry — eval-strings,
  `fresh_perl` children, temp `.t` transpiles.  The s341 grep saw 2 of 60.  The
  measurement tool is `PCL_V2_AUDIT_LOG=<path>` (a file side-channel in
  `pl2cl`), which also splits **TODO** (real v2 gap) from **DIE** (v2 correctly
  raising a Perl-level error that the fallback needlessly retries on v1 — no
  work, self-resolves at the flip).  `PCL_V2_VERBOSE` cannot do it: stderr is
  folded into TAP by the sweep.
- **Tie machinery must stay OUT OF LINE** (s342): `unbox` is `declaim`ed inline
  and `p-scalar-=` expands into generated code, so an `unwind-protect` written
  at either site multiplies across the whole image — the first attempt made
  SBCL exhaust a 1 GB heap compiling `Pl/t/socket-01.t`.  The macro has exactly
  three callers, all `defun`s.
- **The CPAN board's FAIL rule is "zero ok", so it counts files perl itself
  SKIPS as failures** (s343, `docs/cpan-board14-survey-s343.md`): 12 of 53
  FAILs on the 14-dist board are author-only tests (`1..0 # SKIP`) or files
  perl cannot run either.  **Always run the perl oracle beside the board** —
  real failures were 41, not 53.
- **The perl oracle must NOT get `-I<dist>/lib` for an XS dist with no built
  `.so`** (s344, `docs/cpan-board14-partials-s344.md`): perl then dies at `use`
  and produces no TAP, so real failures read as "board artifact" — that is how
  21 Scalar-List-Utils files (110 rows) were first misread. Run the oracle
  against the *installed* module, and record that it may be an older version.
- **The 635 PARTIAL rows of the 14-dist board are 8 families, 3 of them 85%**
  (s344, #231, `docs/cpan-board14-partials-s344.md` + `-partial-causes-s344.tsv`):
  Text-Balanced 300 (goto/tagbody #232, scope, extract offsets #237),
  `caller()` fidelity 127 (#233 — `caller` returns 4 elements not 3, filename is
  the generated `.lisp`, `$0` is `sbcl`, `#line` ignored), List::Util shim 110
  (#238). Nothing there is a board artifact.
- **There is no module called `XSLoader` to support — it is the XS boundary**
  (s343).  `Can't locate loadable object for module …` is PCL dying *exactly*
  as perl would without the `.so`, which is deliberate and load-bearing (it is
  what makes dual-life modules fall back to pure Perl).  The literal
  "module **this module**" is `XSLoader::load()` called with NO arguments —
  perl 5.10+ infers the caller's package, PCL's shim does not; worth fixing for
  diagnosis, not a blocker.

## s345 (Fable, 2026-08-06): s341–s344 batch reviewed + the E4.1 pre-work ruled

Full rulings: `docs/fable-answers-s345.md`.  All eleven commits (`0e73b13`…
`fd20bb9`) approved as shipped; gate independently re-verified 131/4629 PASS.

- **#228 ASK → REGISTER, never tolerate NUL** (§1): `[perl #129069]` joins its
  five NUL-byte siblings in the skip-registry, IN the E4.1 step-2 flip commit
  (earlier and the stale-detector fires — the row still passes via lenient
  truncation).  Pass-baseline row leaves by EDIT.
- **#226 → qualified emission APPROVED** (§2): a leading-`package X;` eval
  lowers AS section X through the D1-lite nested-package QUALIFIED emission —
  no new mechanism.  Five blast-radius probes + the s342g INVERSE guard are
  the acceptance tests; audit F1 events must reach 0.  Residual multi-switch
  stays refused but must reach `$@` perl-shaped
  (`PCL: unsupported in string eval: …`), rephrased at the flip.
- **#230/F3 → route through #78** (§3): the #26 gate guards the v1-seam HOIST
  of `--anon-block-N--` defuns out of their lexical `let`; the inline_lambda
  re-host removes that path, so **#78 is E4.1 pre-work**.  Never a new
  mechanism; in-place-defun fallback only if #78 exceeds ~2 sessions, as an
  ask.  **#230/F6 → SPLIT the oversized run form** at statement boundaries;
  never raise the limit.
- **Board tasks prioritized** (§4): fillers #236 (explain — diagnosis
  multiplier, do first) → #234 (silent-wrong filetest-before-`=>`; fix at the
  filetest-default mechanism, probe the breaking case) → #235; post-E4.1
  convergence #232 → #233 (cheap faces first; filename/#line/frame-hiding is
  an ASK before building) → #237/#238/#239.
- **Queue**: #78 → #226 → #230 → E4.1 steps 1–4 → STOP, Fable takes #153/E5.0
  steps 1–2.

## s346 (Opus, 2026-08-06) — #78 DONE: block-form arg bodies host IN PLACE

- **A `&`-prototype block arg lowers as an inline lambda at the call site**, not
  as a hoisted `--anon-block-N--` defun: `Pl/PExpr.pm`'s block-proto branch asks
  `_v2_embedded_body($block,'sub')` (the anon-`sub {}` sibling's own route), and
  when that DECLINES asks v1 for a LAMBDA (`$return_lambda=1`), never a defun.
  The second half matters as much as the first: the seam localizes the embed
  hook OFF during its discarded native attempt, so the old code left a DEAD
  defun in the bucket whose text still tripped the gate.
- **F3 is CLEARED**: live-v1 board events 8 → 0 (`docs/v1-live-share-audit.md`).
  Corpus emission identical; gate 131/4633; sweep 0 new/0 fixed/0 LOST.  Board
  statuses match the s343 snapshot except S-L-U `reduce.t` 21 ok/11 → 23 ok/9
  (two v1 defects, both now agreeing with perl).
- **The #26 gate STAYS as an unreached backstop** — the three decline shapes
  (`package` stmt, named sub, `use`) all lower in place and match perl, so it
  has no producer; deleting it is E4.1 step 3's reachability job, with its
  three proofs, not a side effect of this change.
- **Queue now**: #226 → #230 (F6 split; F3 half done) → E4.1 steps 1–4 → STOP.

## s346b (Opus, 2026-08-06) — #226 DONE: a leading-`package X;` eval lowers AS X

- **The fix is SUBTRACTION**: the leading `package X;` is no longer CONSUMED at
  segment level, so it reaches `_lower_block`'s D1-lite nested-package path,
  which pushes X onto the Environment while the SECTION package stays the
  eval's root — the `current ne cur_pkg` condition the QUALIFIED emission
  already keys on.  Three supporting facts, each fed to an existing mechanism:
  (1) skip the segment-level sub extraction for that segment (it runs before
  lowering and would name subs unqualified — the s342g silent-wrong);
  (2) the package ENTER forms lead the eval BODY, ahead of the defs/sched
  interleave (a `use` is in sched, and its import records the package in
  effect — Role-Tiny create-hook.t); (3) that `use` gets `:into "X"`, supplied
  by setting the two facts v1's EXISTING `:into` branch keys on
  (`_seam_outer_pkg`, `_block_depth`), never a second predicate.
- **F1 board events 18 → 0.**  All five ruling probes + the s342g inverse guard
  pass with zero v1 routes.  Gate 131/4640; corpus emission identical; sweep
  0 new / 0 fixed / 0 LOST.
- **`our` declared in the region and read back UNQUALIFIED stays REFUSED** (v2's
  native emitter lacks ExprToCL.pm ~900's our-qualify branch; probed 0 vs
  perl's 10) — task **#240**.  A silent-wrong is never shipped to close a
  family; the narrow v1 retry is.
- **A refusal's TEXT is load-bearing before the flip**: `parse_with_fallback`
  keys the v1 retry on `/^Parser2\b/`, so the perl-shaped rephrase of
  eval-mode's residual refusals belongs to the E4.1 step-2 commit, not earlier.
- **Board labels are not the measure**: `role-basic-composition.t` went
  PASS(8 ok) → PARTIAL(10 ok / 3 not-ok) because it now runs 13 rows where it
  ran 8 — more coverage AND more passes.  Read rows, not the PASS/PARTIAL bit.
- **Queue now**: #230 (F6 split) → E4.1 steps 1–4 → STOP, Fable takes #153/E5.0.

## s347 (Fable, 2026-08-06) — review of s346–s346d: all four commits APPROVED; the two asks ruled (`docs/fable-answers-s346.md`)

- **Standing rule (from ask 1a, CONFIRMED)**: when a ruling's acceptance is met
  but a hole is found beside it — gate the hole LOUDLY, clear the family, file
  the task with the measurement.  Never ship a silent-wrong to close a family.
- **#240 split (ask 1b)**: step 1 is PRE-FLIP — narrow the eval-region `our`
  gate to DECLARE-THEN-USE only, because the gate as written refuses the
  routine write-only idiom (`eval 'package Foo; our $VERSION = …; 1'`, probed
  s347) and would break it user-visibly at the flip.  Step 2 (the two-half
  emitter fix) parks but is SCHEDULED: first post-E4.1 compiler item, or with
  E5.4.  §5a.2 binds MEASURED events, not synthesized probe shapes.
- **§5a.3 AMENDED**: "zero eval-mode fallbacks" → "zero UNEXPLAINED eval-mode
  fallbacks"; ruled refusals (multi-switch, #240 read-back, F6 if it lands
  there) are excepted, each REQUIRED to have perl-shaped `$@` text and a
  not-supported entry by the step-2 commit.  A not-supported entry that names
  its own remover (owner task) is not a write-off.
- **F6 re-scope ACCEPTED (ask 2)**: the run bucket is already per-statement, so
  the s345 "chunk the bucket" wording described an existing split.  Locate
  first (audited sweep); top-level-`my` shape → extend `_oversized_top_decls`
  (reuse); generic let-body chopping REJECTED as a first move; eval/fresh_perl
  source → NO pre-flip fix, ruled refusal + baseline row edit instead.
- **Review find, verified live**: #226's `_fallback_stmt_capture` edit also
  fixed a pre-existing file-mode silent-wrong — `sub f { package X; use M; }`
  imported into main before, X after.  Unclaimed, therefore unguarded:
  guard row REQUIRED in #240 step 1's commit.
- **All eval-mode refusal rephrases land in the E4.1 step-2 commit, none
  earlier** — `parse_with_fallback` keys the v1 retry on `/^Parser2\b/`, so an
  early rephrase converts a silent retry into a user-visible die.
- **Queue**: #240 step 1 → #230/F6 locate+fix-or-rule → E4.1 steps 1–4 →
  STOP (Fable #153/E5.0); post-E4.1 compiler queue gains #240 step 2 at its
  head.

## s348 (Opus, 2026-08-07) — #240 step 1: the eval-region `our` gate narrowed to DECLARE-THEN-USE

- **The gate now refuses only a `our` name that is USED AGAIN in the region**
  (read, later write, interpolated read, read inside a nested sub, one member
  of a list decl).  A **WRITE-ONLY `our` collapses natively**:
  `eval 'package Foo; our $VERSION = "1.25"; 1'` and
  `our @ISA = ("Exporter")` are v2-native and correct — they were v1 retries
  at s346 and would have died user-visibly at the E4.1 flip.
- **The mechanism is `_block_captures_name` + one new option, `our_targets`**:
  an `our` declaration's targets count as declarations rather than uses, but —
  unlike `my`/`state` — never as SHADOWS, so a later use of the same canon is
  still a use.  Sigil-exact canons, the inner-`my` shadow rule and the
  string/regex/heredoc conservatism come along unchanged (reuse, rule 11).
- **One arm the ruling did not specify**: the gate also fires when the region
  declares an `our` AND contains a `Cast`+`Block` (symbolic) deref.  A symbolic
  ref names the variable WITHOUT a sigil, so no token scan can attribute it;
  `%p-symref-box` interns an unqualified name in `*package*` (the CALLER's CL
  package) while `_lower_our_decl` qualified the write into X.  Probed:
  `eval 'package D1; our $Z = 5; my $n = "Z"; ${$n}'` → undef, perl 5 — NEW
  from the narrowing, so gated in the same commit rather than shipped.
- **#240's silent-wrong is WIDER than the `our` read-back, and it is s346's,
  not step 1's** (verified identical at `41907a9` in a worktree): EVERY
  unqualified package global in an eval package region binds to the CALLER's
  package.  `eval 'package F2; $Zz = 5; 1'` → perl `$F2::Zz`, PCL `$main::Zz`.
  Cause, one layer up from the ruling's: the eval-mode free-var scan makes the
  name a `p-eval-thunk` parameter and `p-eval-lex-lookup` resolves an
  alist-miss with `(intern … *package*)`.  `our` escapes only because its write
  is qualified; the read-back is the same bug from the other side.
- **MEASURED before deciding: the wider hole has ZERO live events.**  All 20
  eval-region collapses across the entire F1 source (Role-Tiny 23 files +
  Try-Tiny 11) have an EMPTY free-variable set — every one is the
  `package X; use Role::Tiny; …` idiom.
- **NOT gated, and why**: the only compile-time predicate covering it ("the
  region has a free variable") cannot tell a package global from a caller
  lexical — that distinction exists only at the runtime alist lookup — so it
  would also refuse the legitimate `my $x; eval 'package Foo; sub f { $x }'`,
  the exact over-firing the s347 §1.2 ruling reversed the s346 gate for.
  Escalated as the s348 ask (`docs/opus5-review-requests-s348.md` §2).
- **Possible re-scope of #240 step 2**: passing the region package X into
  `p-eval-thunk`, so `p-eval-lex-lookup` interns an alist-miss in X, appears to
  fix the wider hole AND the ruled read-back (the qualified `X::$Z` write and
  the bare read would resolve to the same symbol) — without the two-half
  emitter fix and without touching native variable naming in the deletion
  window.  NOT implemented: it is the parked step 2, and its blast radius
  (which specials can reach that miss path) is unmeasured.
- **The file-mode `sub f { package X; use M; }` guard row is in**
  (`Pl/t/use-require-01.t`, 3 rows) — imports into X, NOT main, callable.
- Gate 131 files / **4648** PASS; corpus emission identical across 111 files;
  CPAN board (Role-Tiny + Try-Tiny) identical to HEAD but for
  `extend-role-tiny.t`, which reproduces at HEAD (drift on task #208).
- **No cache-generation bump**: eval transpiles are in-memory only
  (`*p-eval-string-cache*`); `p-transpile-string` never touches `~/.pcl-cache`.

## s349 (Fable, 2026-08-07) — review of s348: #240 step 1 approved; step 2 RE-SCOPED to the runtime route and PROMOTED pre-flip

Full rulings: `docs/fable-answers-s348.md`.  Gate independently re-verified
(131 / 4648 PASS); every s348 acceptance row and both measurements re-probed
live and reproduced; routing verified via `PCL_V2_AUDIT_LOG`.

- **s348 approved as shipped, symbolic-deref arm included** — the arm is the
  standing gate-the-hole rule applied correctly (the narrowing would have
  opened a NEW silent-wrong; over-fire keeps the correct v1 retry).
- **The wider hole gets NO interim gate** — (ii)-as-gate's plumbing (region
  package threaded into `p-eval-thunk`) is the whole cost of the FIX, so the
  interim artifact is waste.  (iii) parser gating on free variables stays
  REJECTED (over-fires on the legitimate caller-lexical capture).
- **#240 step 2 RE-SCOPED and PROMOTED pre-flip**: the s347 parking reason
  (native-emitter surgery in the deletion window) does not apply to the
  runtime route.  **Mechanism ruled: bind `*package*` to X's CL package around
  the thunk's free-name resolution AND body** — NOT a patch to
  `p-eval-lex-lookup` alone.  Two review-added probes force this: a
  lookup-only fix REGRESSES `eval 'package D2; $W = 7; my $n = "W"; ${$n}'`
  (accidentally right today: write and symref read both mis-land in caller,
  value 7 correct while both package slots are wrong), and the hole has a
  READ spelling too (`$main::G9 = 9; eval 'package X9; $G9'` → PCL 9, perl
  undef — a caller global wrongly satisfies a region read).  The binding
  covers all three spellings; the step-1 gate + Cast+Block arm are then
  DELETED in the same commit (rows flip to native-and-correct with inverse
  guards).
- **Measurement first, stop-rule attached (one-session cap)**: instrument
  `p-eval-lex-lookup`'s miss path across sweep + board — piggybacked on
  #230/F6's audited locating sweep, one run serves both — plus a per-site
  survey of the runtime's ~10 `*package*` fall-throughs.  If a magic/special
  name reaches the miss path, or the `our`-write container does not converge
  with the thunk parameter, STOP: ship (ii)-as-gate (die at the miss — value
  arm of rule 12; inside a string eval that is `$@` + undef, loud and
  perl-shaped) and step 2 returns post-E4.1.
- **Queue**: #230/F6 (carrying the miss-path instrumentation) → #240 step 2 →
  E4.1 steps 1–4 (refusal-rephrase list shrinks to multi-switch +
  F6-if-applicable) → STOP (Fable #153/E5.0).  Post-E4.1 queue head reverts
  to the board items (#232 → #233 → …).

## s350 (2026-08-07, Opus) — #230/F6 measured and CLOSED; #240 step-2 instrumentation run, stop-rule did not fire

Detail: `docs/eval-region-measurements-s350.md`.  No behaviour change; gate
131/4648 PASS, sweep gate clean (0 new / 0 fixed, TOTAL 18498 = baseline).

- **F6 = `perl-tests/tr.t:474-498` (RT #132608), a STRING EVAL** — a runtime
  `tr///` whose replacement list is 40960 chars, lowering to a 73769-char run
  form.  `parse_code`, so the s346 §2.3 branch applies: **no pre-flip fix,
  ruled refusal.**  `$RUN_FORM_MAX` unchanged.  **Flip cost MEASURED: 2 tr.t
  rows** (241→239 passing; the file already aborts at exactly those rows) —
  edit them into `docs/fail-baseline.tsv` with their cause, plus the
  not-supported entry, IN THE E4.1 STEP-2 COMMIT (never earlier: rephrasing a
  `Parser2 TODO:` before then turns a silent retry into a user-visible die).
- **#230 is closed** — F3 was already routed through #78 (s345) and the #26
  gate stays an unreached backstop for step 3 to retire.
- **The sweep's whole v1-fallback inventory is 18 events and all are named**
  (F4/#228 ×6, F2 residual ×5, multi-switch ×1, F6 ×1, 5 `DIE` = v2 correctly
  reporting a perl error).  §5a.3's "zero UNEXPLAINED" is satisfied for the
  sweep half.  **The F2 residual costs nothing and likely GAINS 4**: its four
  `my $$x` strings are `eval.t:239-246`, blessed FAILURES today because PCL
  accepts what perl rejects (principle 9); the refusal makes `$@` non-empty
  and they pass.
- **#240 step-2 stop-rule did NOT fire.**  Miss-path listing over the whole
  sweep: 2720 events, 29 distinct names, **zero punctuation/magic variables**.
  The `*package*` consumer survey is 13 real runtime sites, every one
  implementing "unqualified → current package" — the binding moves all
  thirteen toward perl, none away (`p-resolve-invocant` is the only one that
  *removes* a resolution, and removing it is the correction).
- **Two sizing facts step 2 must now assume, neither available at s349**:
  (a) the s348 "20 collapse events, all EMPTY free sets" was measured on the
  module SOURCES — under the running board it is **108 region events, 86 with
  free names** (Class::Method::Modifiers' installer); the alist wins for every
  real capture and the only redirected names are 50 phantom `$method`s (a
  `_eval_scope_free` false positive: the eval string declares it with `my`);
  (b) **a region with no free names emits no `p-eval-thunk` at all**, so the
  binding cannot live only on the thunk's parameter path — the body is where
  all 108 events feel it.
- **Instruments to DELETE with the step-2 commit**: `PCL_EVAL_LEX_MISS_LOG`
  (`%p-eval-lex-miss-audit`, `cl/pcl-runtime.lisp`) and `PCL_EVAL_REGION_LOG`
  (`_assemble_eval_mode`, `Pl/Parser2.pm`).
- **Queue**: #240 step 2 → E4.1 steps 1–4 → STOP (Fable #153/E5.0).

### s350 addendum — the step-2 wrap fork PROBED away (one mechanism, not two)

A region with no free names emits no `p-eval-thunk`, so step 2 must wrap its
body — and wrapping is what `_cap_inlining_if_huge` refuses to do to
`eval-when`/`p-sub`/`defvar`/`p-defpackage`.  **Probed with
`PCL_EVAL_REGION_WRAP=1` (forces the wrap on every #226 region): emission
really changes, and every row of Role-Tiny + Class-Method-Modifiers + Try-Tiny
(48 files, cold cache) is IDENTICAL**; sub installation, `use` import target
and `our` read-back all still match perl.  **So step 2 uses ONE mechanism** —
emit the thunk whenever a region package is present and bind `*package*` on
`p-eval-thunk`, per s349 §2c.  WHY the wrap is safe (do not re-derive): eval
mode has no compile-file phase to lose — `p-eval` reads/evals form by form and
the region's defs are already hoisted inside the one eval'd unit; the
`_cap_inlining_if_huge` prohibition is about FILE-mode top-level forms.
Third instrument to delete with step 2: `PCL_EVAL_REGION_WRAP`.

## s351 (2026-08-07, Opus) — #240 step 2 SHIPPED: `p-eval-thunk` binds `*package*` to the region package; #240 CLOSED

Mechanism ruled s349 §2c, measured s350, implemented here.  The whole of
E4.1's pre-work is now done.

- **`p-eval-thunk (free-names fn &optional region-pkg)`.**  When REGION-PKG is
  given it binds `*package*` to that CL package (find-or-create with
  `:use '(:cl :pcl)` — the `(setf %p-symref-box)` convention) around BOTH the
  free-name resolution and the body.  `p-eval-lex-lookup` is UNCHANGED: its
  miss path already interns in `*package*`.  Normative text:
  `docs/ir-spec.md` §string eval piece 2, `docs/eval-lexical-capture.md` §3.
- **The emitter passes it only from the #226 collapse** (`_assemble_eval_mode`),
  as `_cl_pkg_designator`'s spelling, recorded where the enter forms are built
  (`_lower_block`'s Statement::Package branch) so the perl-name → CL-name rule
  is never derived twice.  **The thunk is emitted whenever a region package is
  present, even with an empty free-name list** — the s350 §6 forced-wrap probe
  measured that shape as row-identical over 48 region-heavy files first.
- **All three spellings of #240 are closed** and agree with perl: a bare write
  (`eval 'package F2; $Zz = 5; 1'` → `$F2::Zz`, not `$main::Zz`), a bare read
  that must NOT see the caller's global (`$main::G9 = 9; eval 'package X9;
  $G9'` → undef), and `our` declared-then-read-back (`package F1; our $Z = 5;
  $Z * 2` → 10).  The D2 regression the s349 review predicted for a
  lookup-only fix does not occur: value AND package slot are both right.
- **The step-1 gate is DELETED** — `_eval_region_our_readback`, its
  `Cast`+`Block` arm, and `_block_captures_name`'s `our_targets` option (the
  gate was its only consumer) are gone; a single-region eval now refuses
  NOTHING.  Only the multi-switch shape (two `package` statements in one eval)
  is still refused, and it keeps its `Parser2 TODO:` prefix until the E4.1
  step-2 commit.
- **All three s350 instruments are deleted**: `PCL_EVAL_LEX_MISS_LOG`,
  `PCL_EVAL_REGION_LOG`, `PCL_EVAL_REGION_WRAP`.
- Guards: `Pl/t/transpile-test-09.t` "eval package-region: unqualified names
  resolve in X" (10 assertions against the perl oracle) for the VALUES;
  `Pl/t/parser2-02.t` for the shape, including the INVERSE guard that a
  region-less eval gets NO region argument.
- **No cache-generation bump**: file-mode emission is byte-identical across
  all 111 corpus files (`tools/corpus-diff.pl`), and eval transpiles are
  in-memory only (`*p-eval-string-cache*`).

## s358 (2026-08-08, Fable) — s357 approved; #252/#243 USER-decided; Text::Balanced restored

- **s357 (#244 step-4 verification) APPROVED as measured** — gate, the
  15,129-row arithmetic, the pre-flip `pipeline=v1` attribution, the board
  totals and the 27-file #243 blocker all independently reproduced.  One
  nit: the Text-Balanced dist carried 780 ok rows (766 is the NET board
  delta after +14 expected gains) → `docs/fable-answers-s357.md` §1.
- **The audited populations are FOUR: sweep + CPAN board + Pl/t gate +
  the perl suite** — a closed list; any "zero events" precondition must
  name all four (§5a.2 re-amended; same miss happened twice) →
  `docs/fable-answers-s357.md` §2.
- **A live-v1-style audit runs on a COLD cache** — transpile-time
  instruments report zero for exactly the already-cached modules (how the
  audited board missed Text::Balanced) → `docs/fable-answers-s357.md` §2.
- **#252 = SPLIT (USER, 2026-08-08): phase 1 (Text::Balanced) executed
  s358; phase 2 (capture/spanning + poisoned-`my` families) is a PLAN
  awaiting scope approval** → `docs/e41-suite-families-plan.md`, task #254.
- **#243 = PORT the 27 Pl/t files off v1's file entry (USER, 2026-08-08)**
  — then complete the step-3 deletion; assertions stay semantically
  equivalent, v1-implementation-detail rows are dropped with a note, never
  weakened → task #255, `docs/fable-answers-s357.md` §3.
- **Forward `goto` to standalone labels is GENERAL for the all-forward
  case** — nested catches, one per label, each enclosing everything before
  its label; leading decls handled by ordinary my-branch nesting (NOT
  hoisted).  Backward/mixed keep the tagbody machinery + gate →
  `docs/fable-answers-s357.md` §4.1, guard rows in
  `Pl/t/transpile-test-09.t`.
- **A textual "reads $name" scan must not fire on `$name[`/`$name{`**
  (elements of @name/%name, not the scalar) — `_reads_name_rx`, four scans
  routed through it; the list-decl self-ref path emitted an unbound
  package-var read on the false positive →
  `docs/fable-answers-s357.md` §4.2.
- **Interpolation of a subscripted variable continues its chain**
  (`"$_[0]->{k}"`, `"$a[0]{k}"`) — chains parse via PPI when a second
  group follows; a lone subscript keeps the legacy path byte-for-byte →
  `docs/fable-answers-s357.md` §4.3.
- **op/for.t (#253) is an upstream PPI 1.291 lexer bug, not a PCL
  regression** — `for ${*$f} (LIST) { }` kills the PPI lexer whole-file;
  its s323e snapshot rows were `--lenient-ppi` truncation (the #228
  accident class), retired by ruling s356 → `docs/ppi-upstream-bugs.md`
  §6; registration with cause via #254 §4.

## s358i (2026-08-08, Fable) — E4.1 COMPLETE: v1's file-level transpile surface deleted (#243/#255)

- **All 27 Pl/t files ported to Pl::Parser2** (#255, `5ba6155`); porting
  rule applied throughout: assertions stay semantically equivalent,
  v1-implementation-detail rows dropped with notes (#132 echoes, call
  qualification), one perl-vs-v2 divergence TODO-marked (#256:
  retroactive prototype application to pre-declaration calls).
- **Pl::Parser::parse survives ONLY as the prototype-collection walker**
  (`2d9aa8b`) — the extractors are its sole callers
  (collect_prototypes_only => 1, _emit no-op); a guard dies on any other
  use.  parse_file/parse_code and lenient_ppi/_ppi_with_fallback are
  DELETED.  Statement/expression v1 code remains live through Parser2's
  seam, which never calls parse().  E5 burns that seam down.
- **Two production silent-wrongs found by the port, both fixed
  (`5ba6155`)**: (1) the v2 prototype premerge now seeds transpile-time
  inc_paths from the document's literal `use lib` paths (v1 got the
  ordering free; the pre-pass didn't, so a file's own lib dir lost its
  modules' block-form prototypes); (2) `_merge_module_prototypes` tags
  merged entries from_module and never overwrites a LOCAL declaration
  (v2's sub hoisting put the seam re-merge AFTER the local override,
  which it clobbered — wrong auto-boxing on every later call).
  Local-before-use divergence (perl's import clobbers with a warning; PCL
  keeps the local) accepted, noted in #256.

## s359 (2026-08-08, Fable) — #153/E5.0 steps 1–2: the term-grammar walker, decline-not-guess

- **`_term_extent` is THE term-grammar walker** (`term := cast* primary
  postfix*`, Pl/PExpr.pm) — and **undef is a first-class answer**: bare
  words, prefix operators, `->method(args)`, cast-block slice groups
  (`@{$r}[0]` — PPI spells the group as a Constructor) DECLINE so the
  call site keeps its legacy derivation.  The walker never guesses and
  never stops inside a term.  Unit tests: Pl/t/reduce-term-01.t.
- **Site flips are measured, then flipped**: `PCL_TERM_DIFF=1` dual-run
  probes at both operand sites; the s359 inventory over all 111 corpus
  files found ZERO real disagreements, and `defined` was flipped onto the
  walker (corpus byte-identical, gate 132/4717, cold-cache sweep GATE
  clean, TOTAL 18499 = baseline).  Steps 3–5 (per-site migration,
  reduction fold-in, `$end_pars`/`$deref_skip` deletion) = Opus, recipe
  in task #153.  Design: docs/pexpr-term-parsing-review.md (Option B).
- **PPI gotcha (test infra)**: a token array goes HOLLOW when its
  PPI::Document is garbage-collected (recursive DESTROY); keep the
  document alive when holding tokens.  Noted in reduce-term-01.t.
- **Bug-hunt sequencing recommendation** (RULED s360, see below):
  docs/bug-review-s359.md — no new campaign now; #254 first if approved;
  big hunt (widened board + full suite + fuzzer with two new axes) is the
  E5 exit gate, pre-R2.

## s360 (2026-08-08) — USER rulings: #254 approved, big bug hunt deferred

- **#254 APPROVED** ("so we can get a functioning compiler"): fix scope of
  `docs/e41-suite-families-plan.md` §§1–3 authorized — capture/spanning
  (9 suite files) + poisoned condition-`my` (4 files), ~12k rows; Opus
  executes, session 1 = measurement, recipe in task #254.  **§4's residue
  registration (the other 15 dark files) is NOT yet signed off** — comes
  back with real shapes after measurement.
- **Big bug hunt DEFERRED** ("in the future when things are stable"):
  the docs/bug-review-s359.md §4 recommendation accepted — E5 exit gate,
  pre-R2; the four standing audit populations remain the hunt until then
  (ruling recorded in that doc's §6).

## s373 (2026-08-09, Fable) — s372 review: both commits APPROVED; five asks ruled

- **s372 APPROVED as shipped** (`docs/fable-answers-s372.md`): gate
  independently re-verified 132/4747; #269's numbers reproduced from the
  suite file; #271's one-file population reproduced by grep; four live
  block-boundary probes vs perl all match.
- **The DIE-SCAN is a ruled NARROWING of the gate-SET bar** for one edit
  class: when the sole non-comment edit is *decline → die on a
  value-returning helper*, emission is identical-or-die by construction, so
  the bar is met by a both-populations die-scan (exit codes checked, every
  pre-existing non-zero triaged) + corpus-diff + the sweep TOTAL/LOST
  verdict.  All three legs required; missing any leg → full file-by-file
  gate-SET diff as ruled (`fable-answers-s372.md` §2).
- **#272's un-widened boundary is probed COSTLESS**: embedded `my` in
  `map`/`sort`/`grep` BLOCKs and a bare file-level block all match perl
  today without the rename — boundary stands, NO residue task.  If a block
  divergence ever surfaces, widening must first answer per-iteration
  freshness (§3).
- **#271 goes BEHIND #153's FOLD** (one-file population; the fix lives in
  the `$end_pars` maze `pexpr-term-parsing-review.md` forbids patching);
  the `pipe my ($r, $w)` shape joins the FOLD's acceptance probes.
- **#269's park behind #196 CONFIRMED** — re-opens when #196 moves, not
  before.

## s372 (2026-08-09, Opus) — #274 shipped; #269 measured and PARKED; #272

- **A FAILED ALIAS ANCHOR DIES AT EVERY k** (#274, ruled s371 §2, shipped
  `033f088`): the v1 seam's sole-element `_apply_foreach_alias_rewrite` no
  longer swallows a non-outermost head — same die as the k>1 site.  Measured
  before flipping: 751 sources across BOTH populations (528 perl-suite + 223
  CPAN-board), **0 new dies**; corpus-diff identical; sweep GATE clean, TOTAL
  18499 (baseline 18498).
- **The two foreach comma walks now carry PAIRED invariant comments** naming
  each other (`_foreach_scalar_elements` ↔ `_ev_foreach_alias_list`): on a
  QUALIFYING list they provably agree; the veto is deliberately a SUPERSET
  elsewhere.  A third walk in the family reopens the shared-primitive question.
- **#269 is PARKED BEHIND #196, measured not guessed**: `re/reg_eval_scope.t`'s
  snapshot `C_ok` is **5** of perl's 48, and all 47 assertions exercise
  `(?{ … })` blocks, which PCL does not run at all — so de-gating buys ~0
  verified rows.  The gate itself names a REAL capture; do not delete it.
- **A gate-SET compare must normalize the COMPILER'S OWN ROOT, not just line
  numbers** — the emitted preamble embeds it (`*pcl-pl2cl-path*`, the @INC
  pushes, `*p-core-inc-dirs*`: task #217), so a worktree-vs-tree diff reports
  EVERY file as changed until both roots fold to one token.
- **The embedded-`my` veto's condition is "inside ANY sub body", not "inside a
  NAMED sub"** (#272) — whether the body has a name says nothing about who can
  see its lexicals; the anon spelling fell through to the package global and
  crashed.  `_enclosing_named_sub` → `_enclosing_sub_body`.

## s371 (2026-08-09, Fable) — s370 review: #267 APPROVED; asks 1–4 RULED

- **s370 APPROVED as shipped** (gate independently re-verified 132/4744;
  ten live probes vs perl identical, incl. same-scalar-twice, k=2
  vivification, named loop var) → `fable-answers-s370.md`.
- **Probes + guard rows ARE the verification bar when a shape occurs in no
  corpus** — do NOT widen the population (new axis; file-don't-grow).
  Corpus-diff-first is the standing cheap-first order → §1.
- **The k=1 foreach-alias anchor-miss must DIE like k>1** — a failed anchor
  is always a compiler self-inconsistency, never a benign decline.  Own
  filler commit, two-population gate SET + sweep TOTAL/LOST first → §2.
- **The two foreach comma walks (qualifier vs VarAnnotator veto) stay TWO**
  — on every qualifying list they provably agree; the veto is deliberately
  a superset on non-qualifying lists.  Paired invariant comments required;
  a third walk in the family reopens the question → §3.
- **Mixed-list residue CORRECTED**: `for ($x, @a)` DOES alias `@a`'s
  elements (boxes survive `p-flatten-args`); what misses is an
  ELEMENT-shaped slot (`$h{a}`) and `values %h` in a non-qualifying list.
  E5 axis, DO-NOT-START — but probe the right boundary → §5.

## s370 (2026-08-09, Opus) — #267 SHIPPED (both ruled commits); #273 filed

- **An all-single-scalar foreach list is `(vector E1 … Ek)` at EVERY k**, and
  each aliasable element carries its BOX head — the N=1 rule IS the N=k rule
  (#267, ruled s369 §2, shipped `f2c7c25` + `0e5b088`).  Resolver:
  `Pl::Parser::_foreach_scalar_elements` (depth-0 split via the SHARED #138
  splitter; all-or-nothing per list).  Boxes must NEVER pass through
  `p-flatten-args`.  **Mixed lists (`for ($x, @a)`) stay flattened and boxless**
  — aliasing an aggregate's elements is the boxed-aggregates axis (E5,
  DO-NOT-START).
- **A LITERAL foreach element is writable, not read-only** — `for ($x, 3)
  { $_++ }` where perl dies.  Probed, **pre-existing at N=1**; accepted
  divergence, `docs/not-supported.md`.
- **A gate-SET stderr diff must normalize compiler LINE NUMBERS**
  (`ROOT/(Pl|tools)/\S+ line \d+`) — a `.pm` that gains lines otherwise reports
  phantom drift on every file that warns.  18 such hits in s370, 0 real.
- **Run `corpus-diff.pl` BEFORE spending a full sweep**: identical emission over
  `perl-tests/*.t` proves the sweep's `.t` half cannot move, for minutes instead
  of an hour.
- **A sweep can FINISH and have its verdict swallowed** (#273): an orphaned
  `sbcl` + `pl2cl --server` deadlock inherits the run's stdout, so the pipeline
  never sees EOF, and **`timeout N` cannot kill it** (SBCL catches SIGTERM; the
  handler cannot run while blocked on the pipe).  Check `.faillog/_status.tsv`'s
  row count + mtime before believing a sweep is still running; reproduce the
  verdict from disk with `tools/sweep-diff.pl diff docs/fail-baseline.tsv
  .faillog`.

## s369 (2026-08-09, Fable) — s368 asks RULED; #267 sizing decided

- **s367 + s368 APPROVED as shipped** (gate independently re-verified
  132/4739 cold; all probes reproduced) → `fable-answers-s368.md`.
- **#265 rename-not-narrow CONFIRMED; third-sibling pre-pass shape
  CONFIRMED** (populations disjoint via the complementary `$in_sub`
  guards) → `fable-answers-s368.md` §1.
- **#267: the N=1 rule IS the N=k rule** — an all-single-scalar
  multi-element foreach list emits `(vector E1 … Ek)` (per-element
  `_foreach_single_scalar_p`; depth-0 split = the shared #138
  `lowprec_idx` machinery, never a new comma scan); boxes must NEVER pass
  through `p-flatten-args`.  Mixed lists stay flattened, no boxes (E5
  axis).  **TWO commits**: wrapper switch first with a ZERO-change full
  sweep (TOTAL/LOST, cold cache) as its own discriminating measurement,
  then the per-element box verdict → `fable-answers-s368.md` §2.
- **#269 stays (capture is REAL, refusal conservative not blind)**; measure
  `reg_eval_scope.t`'s reachable rows before any session — `(?{…})` blocks
  not running is the #196 regex-engine axis → `fable-answers-s368.md` §3.
- **#271 layer call endorsed**: fix at argument-run lowering (`my (LIST)`
  contributes N args), never per-builtin at the runtime →
  `fable-answers-s368.md` §4.
- **NEW residue #272 (review probe, PRE-EXISTING at 88258a8)**: embedded
  `my` inside an ANON sub body still veto-refused → global read →
  type-error crash; the pre-pass keys on `_enclosing_named_sub`, correct
  condition is "inside ANY sub body".  Measure the route first.

## s368 (2026-08-09, Opus) — #265 closed: embedded `my` in a sub vs a same-named global

- **The embedded-`my` let-hoist's veto is RIGHT at file level and SCOPE-BLIND
  inside a named sub.** A sub mentioning the name can share a FILE-level
  `open my $fh` cell (Capture-Tiny's Utils.pm, #199) — it cannot possibly see
  a lexical declared inside ANOTHER sub's body. `++my $x->{k}` in `sub foo3`
  therefore wrote the package global and persisted across calls (op/my.t t47).
- **Narrowing the veto would have been WRONG — the fix is a rename.** Letting
  the hoist fire registers `$x` in `_seg_lex`, which suppresses the GLOBAL's
  forward defvar and strands the other sub. `$x__emb__N` is a name nobody
  else mentions: the veto stops firing AND `$x` keeps its defvar. (Probed
  both ways.) Rename root = the enclosing BLOCK, which is exactly perl's
  scope for an embedded `my`. → `_rename_vetoed_embedded_mys`, ir-spec §2b.3.
- **ONE veto predicate** (`_embedded_my_veto_names`), read by the refusal and
  by the pre-pass that removes the need for it — the s363 detector/rewriter
  rule, third instance after #264 and #265's promoter.
- **`__emb__N` strips in `_eval_lexical_alist`** like `__cond__`/`__shadow__`:
  the renamed decl is LET-BOUND, so a string eval naming the original `$x`
  still finds it (ir-spec §2b.4's first route). Probed live.
- Measured: gate 132/4739 PASS; `corpus-diff` **1 of 111 files** (my.t, the
  fix itself); CPAN board **0 of 223 sources changed**; perl-suite gate SET
  **2 of 523, 0 new dies** (my.t + pat_advanced.t, which has the same shape
  and gets the same fix); op/my.t 51/8 → **52/7 = its `perl-suite-run.tsv`
  snapshot**.
- **A HEAD-compare is INVALID if the tree changes mid-run** — bumping
  `*pcl-cache-generation*` during the first perl-suite gate-SET pass made
  194 of 523 files "differ" by their `;;; pcl: … gen=…` header line alone.
  Normalize the header, or finish the compare before touching the tree.
- **`grep` goes binary-silent on emitted CL too, not just `.tsv`** —
  pat_advanced.t builds `chr 0..255`, so its output holds NUL bytes and a
  plain `grep 'x__emb__'` printed nothing where `grep -a` finds 9 hits.

## s367 (2026-08-09, Opus) — #270 fixed: the `:prototype($)` silent statement drop

- **A prototype whose text ends in `$` mis-lexes TWICE** — PPI §7 turns
  `sub :ATTR` at expression start into a Label, and inside that run the
  closing paren of `prototype($)` is tokenized as the magic variable `$)`,
  so the attribute's paren group swallows the sub's block.  Affects `($)`,
  `(;$)`, `($;$)`, … ; `($$)`, `(\@)`, `($_)` lex correctly (probed).
  → `ppi-upstream-bugs.md` §7b.
- **The stolen `)` belongs to the ENCLOSING structure, so tree surgery
  CANNOT fix it** — the enclosing List is left unfinished and, inside a
  `for` list, the damage spreads across sibling statements (both measured
  s367 by building the hoist first and watching it destroy the text).  The
  repair is at SOURCE level on the RAW TOKEN STREAM, before any §7 tree
  surgery, then one `_reparse_doc` — the mechanism the state prepass already
  uses (`_state_reparse` renamed and shared).  → `Pl/Parser2.pm`
  `_repair_swallowing_prototypes`.
- **`Statement::Null` is INSIGNIFICANT**, so an `schildren` walk silently
  skips the `;` in `(;$)` — the tell was a stray `;` surviving into the
  rewritten source.  Walk `children` when you are reconstructing TEXT.
- **A `sub :` Label is only ever produced by this mis-lex**, so a run that
  does not end at a Block is known-mangled input: the §7 repair now DIES
  naming the shape instead of `next`ing into the silent statement drop
  (rule 12, value side).  Gate SET measured over both populations: 15
  sources carry a `sub :ATTR` spelling, stderr identical to HEAD on every
  one, `corpus-diff` identical across 111 files.
- **`run_cl` in `Pl/t/transpile-test-09.t` no longer folds transpile stderr
  into the `.lisp`** — a `PCL: …` announce at the top of the file is a CL
  read error, and an announce has no perl counterpart.  Stderr is still
  appended when pl2cl FAILS.  (Each `transpile-test-NN.t` carries its own
  copy of the helper; only -09 is fixed.)
- **§8 guard shipped**: PExpr's attribute strip announces a LIVE
  `prototype(...)` Attribute before dropping it — the residual silent path
  left by `_extract_prototype_attributes`' bail-outs.

## s366 (2026-08-09, Fable) — review of s365: all nine asks ruled, one new bug

- **All eight s365 commits APPROVED as shipped** — gate independently
  re-verified (132/4737), every semantic ask probed live against real perl.
  → `docs/fable-answers-s365.md`.
- **#254 and #252 are CLOSED** — worklist empty; A-ii parked behind E5
  (s364); residue lives in #265/#267/#269/#271 with their own snapshot bars.
  → fable-answers-s365.md §5.
- **A-i's ordering-independent-promotion design is STRUCK, not parked** —
  the s363 premise is marked SUPERSEDED in the measurement doc; nobody
  re-derives it.  → fable-answers-s365.md §1.
- **"One capture test, shared by promoter and gate" is a STANDING rule** —
  the promoter is the third instance of detector-and-rewriter-share-one-
  resolver.  → fable-answers-s365.md §9.
- **Filler-scope rule**: a filler may grow while the growth is the SAME
  mechanism, each widening step is measured against the gate SET over both
  populations, and new design axes are FILED, not fixed.
  → fable-answers-s365.md §4.
- **Timeout registry RATIFIED; the suite runner gets NO blind retry** — an
  unregistered slow file must surface as TIMEOUT so it gets a row with a
  cause; a row is deleted when the file gets faster.
  → fable-answers-s365.md §6.
- **NEW BUG (#270), found by review probes**: `sub :prototype($) {…}` at
  expression start is a SILENT STATEMENT DROP — any prototype text ending in
  `$` triggers a second layer of PPI §7 (`$)` lexes as the magic var, the
  block is swallowed into the attribute's parens) and the #268 repair's
  block-check declines silently.  Ruled: the decline DIES, the repair
  extends to the `$)` swallow, PExpr's strip gains a `^prototype\(` announce
  guard.  → fable-answers-s365.md §10.
- **#265 rename-half shape approved** (renamed lexical via shadow-aware
  `_rename_decl_within`; eval route = ir-spec §2b.4 let-bound → site alist),
  ordered BEFORE #267; #267 gets a SIZING step against the CLForm
  per-element sketch; #269 gets a PROBE step (the nested-sub capture may be
  real).  → fable-answers-s365.md §9, §11.

## s365 (2026-08-08, Opus) — #254's registration + A-iv/A-i/A-iii/B-ii, and #263

- **A per-file TIMEOUT ALLOWANCE registry exists**: `docs/perl-suite-timeouts.tsv`
  (`rel<TAB>seconds<TAB>cause`), honoured by `tools/run-perl-suite.pl` as
  `max(seconds, --timeout)` and printed per run.  A file that needs longer than
  the default is registered WITH ITS CAUSE, never left to TIMEOUT into "no rows"
  (#176's lesson generalised).  First row: re/pat_advanced.t = 900 s.
  → `docs/e41-suite-families-s365.md` §0.
- **A capture/span refusal that is SIGIL-blind or SCOPE-blind is a bug, not
  conservatism** — three of #254's four remaining causes were exactly that, and
  all three are now deleted rather than narrowed: the scalar promotion's
  `family use (@x/%x/$#x)` veto (A-iv), `_block_captures_name`'s blindness to a
  `my` EMBEDDED in another statement (A-i), and the span detector's lack of a
  scope for a block-form `package Foo { … }` segment (A-iii).  Each one is the
  same standing rule one layer over: the pass that DETECTS and the scoping the
  program actually has must agree.  → `docs/e41-suite-families-s365.md` §1–3.
- **A-i needs NO extent design** (the s363 measurement's reading is superseded):
  op/getppid.t's sub never captured the file lexical at all — it declares its
  own `$first` in a statement-modifier `my`.  Ordering-independent promotion is
  NOT required and was not built.  → `docs/e41-suite-families-s365.md` §2.
- **A block-form package's lexicals never span**, but the outer lexicals it
  encloses still do: the fix SKIPS that segment's declarations, and must not
  kill the live set on entry (probed both ways).  → §3 of the same doc.
- **The cond-my rename is shadow-aware** (B-ii): `_rename_decl_within` skips an
  inner re-declaration's target and everything it shadows (`_ref_shadowed` —
  the span pass's resolver), so `multiple declarations` is waived for callers
  that rename that way.  → §4.
- **foreach aliasing is decided by ONE peeler** (`_foreach_list_unwrap`) shared
  by the rewrite, the single-scalar wrap and the annotator's veto — the two
  spellings (block form / statement modifier) wrap the list differently and the
  passes disagreed, which WAS #263.  The rewrite is anchored at the outermost
  call, so a wrong head guess is a no-op instead of an inner box.
- **Never run the Pl/t gate under `nohup`**: nohup ignores SIGHUP, perl then
  reports `$SIG{HUP}` as defined, and transpile-test-06.t's %SIG row fails
  against a PCL that correctly knows nothing of the inherited disposition.
- **The capture PROMOTER and the capture GATE must use the SAME test.**
  `_captured_in_subs` ran its own shadow-blind Symbol/ArrayIndex loops before
  calling the shadow-aware `_block_captures_name`, so a sub's OWN `my $x` uses
  promoted a same-named file lexical.  Harmless for a statement-level `my` (its
  let shadows the cell), SILENT WRONG for an EMBEDDED one (the embedded-`my` let
  is skipped for promoted names, so the sub wrote the shared cell).  Deleted
  (#265).  **Open sub-case**: when the name is a package GLOBAL used by another
  sub, the embedded-`my` veto is RIGHT to refuse a plain `let` — the symbol is
  defvar'd, so `let` is a dynamic rebinding; that shape needs a renamed lexical
  (`_rename_decl_within`).  → `docs/e41-suite-families-s365.md` §8.
- **An anon sub's ATTRIBUTE at the START of an expression is lexed by PPI as a
  LABEL** (`(sub :lvalue {…})` → `Label('sub :') Word('lvalue')`; inside a
  `for` list each label even gets its own STATEMENT) while the SAME text
  mid-expression lexes correctly.  Upstream bug, registered
  `docs/ppi-upstream-bugs.md` §7; repaired at the document level by
  `Pl::Parser2::_normalize_anon_sub_attrs`, with the `PPI::Structure::For`
  re-bless (a C-style `for` never has a loop VARIABLE before its parens) as the
  second half.  Before it, the statement became a PARSE ERROR comment — a
  SILENT CODE DROP, same family as #138/#259.  → `docs/e41-suite-families-s365.md` §7.

## s364 (2026-08-08, Fable) — review of s363: all seven asks ruled

- **All seven s363 commits APPROVED as shipped** — gate independently
  re-verified (132/4731), step-5 die guards adversarially probed (regex-token
  operands, empty operand: no death), all six diffs read.
  → `docs/fable-answers-s363.md`.
- **Step 5's `die` stands — die-side of the s329 boundary**: a wrong-sized
  operand is a VALUE the program consumes; announce-and-continue would commit
  the silent wrong it announces.  → fable-answers-s363.md §1.
- **"Detector and rewriter share one resolver" is a STANDING rule** (third
  confirmation: M2 s353, A-v, #264), and the gate-SET file-by-file diff over
  both populations is the standing verification for detection-widening
  changes.  → fable-answers-s363.md §3.
- **The THREE-ROUTE eval-visibility rule is normative** (ir-spec.md §2b.4):
  let-bound rename → site alist + suffix strip; defvar'd package cell →
  alias rule + span pairs; neither → hard refusal.  A new rename family picks
  its route before it may pass `eval_ok`.  ir-spec's stale cond/state and
  "falls back to v1" text fixed s364.  → fable-answers-s363.md §4.
- **A-ii PARKED behind E5**: stop-rule fired + all three files have snapshot
  C_ok = 0 (svleak 0/0; shm XS-blocked on #117; taint never transpiled, no
  taint model exists) — zero v1-era recovery by the ratified bar.  Design the
  declaration-shape enumeration into E5's promotion layer.
  → fable-answers-s363.md §6.
- **The s317 bareword probe claim was STALE**: the declared-before spelling
  CALLS correctly today; only the undeclared spelling diverges (perl
  stringifies, PCL prints empty) → **#266**.  → fable-answers-s363.md §2.
- **Before more #254 fixes**: register pat_advanced.t's `--timeout 900` and
  re-snapshot the three B-i files, else the +1765-row recovery evaporates
  (the #176 lesson).  → fable-answers-s363.md §5.

## s363 (2026-08-08, Opus) — #153 steps 4–5, plus #254's first three fixes

- **`${x}` at CODE level IS a use of `$x`** (`36b4d7f`, #264): PPI spells it
  Cast + Block-holding-one-Word, so there is no Symbol token and every
  Symbol-driven pass was blind — the span checker never fired (silent wrong,
  not even a gate) and the refusal written for that shape sat downstream of the
  span test, unreachable.  ONE helper, `_brace_name_refs`, now serves BOTH the
  detector and the renamer; `_has_code_brace_deref` is a filter over it.
  **A pass that DETECTS and a pass that REWRITES must share the resolver** —
  the same rule that made A-v (op/exec.t) a bug.
- **Adding DETECTION can turn silently-wrong files into DYING ones, so diff the
  GATE SET, not just the gate**: transpile every file of both populations
  before and after and compare per file (scratchpad `xstat.pl`, s363).  #264
  measured 30 → 30 (zero new gates); B-i measured 30 → 27 (exactly its three
  files).  Adopt this whenever a fix widens what a checker sees.
- **B-i needed no new mechanism** (`04316ab`): a poisoned condition-`my` rename
  mints a LET-BOUND `$x__cond__N`, so teaching `_eval_lexical_alist`'s key
  function that fourth suffix (beside `__lex__`/`__shadow__`/`__file__`) and
  passing `eval_ok` at the cond site is the whole fix.  `state` keeps the
  refusal — `__state__` is a defvar'd cell, never a let, so it never enters
  `_let_bound_vars`.  1257 rows.
- **De-gated ≠ done**: the ratified bar is the file's `perl-suite-run.tsv`
  snapshot C_ok.  Of B-i's three, only re/regexp_unicode_prop.t lands on it;
  op/my.t is one row short (**#265**) and re/pat_advanced.t is 137 short with a
  regex-engine residue.  Report both numbers.

## s363 (2026-08-08, Opus) — #153 steps 4–5: walker widened, unreachable operand branches deleted

- **The term walker claims method-call ARGS and cast-deref SLICE GROUPS**
  (`f322b19`, #153 step 4): `-> method ( args )` consumes the List and
  continues the chain; a Block/Constructor group after a cast-deref
  (`@{$r}[0]`, `\$V{V}`) is one slice postfix and ENDS the term (a slice
  yields a list; nothing postfixes a list).  PPI spells that group as a
  Constructor only because a `}` precedes it.
- **Bare WORDS and PREFIX operators stay DECLINED — by design, not by
  omission.**  Whether a bareword is a call, a filehandle, a class name or a
  constant is not the term grammar's question (it is decided in the main
  loop), and a prefix operator is phase-2 operator binding.  The plan text
  listing "bare words" among step 4's widenings is superseded by the walker's
  own rule.  → `_term_extent`'s header, session-log s363.
- **The operand CEILING cannot fall inside a postfix chain**, so a walker
  decline on a Symbol/Magic/Structure/already-parsed operand is IMPOSSIBLE —
  the ceiling only ever falls at a top-level low-precedence operator or a
  ternary `:`.  That argument (plus a 110-decline inventory over both
  populations in which every decline led with a Word, an Operator or a Cast)
  licensed deleting those branches at BOTH operand sites (`57086d8`, step 5);
  what is left handles only the two by-design declines, and anything else
  DIES naming the shape (rule 12) rather than leaving `$end_pars` at the
  ceiling — a wrong-sized operand is a value-producing silent wrong.
- **`tools/corpus-diff.pl`'s corpus IS `perl-tests/*.t` — the sweep's own
  input set.**  So "emission identical across 111 files" already proves the
  sweep's `.t` transpiles are identical; the only thing a post-parser-change
  sweep adds is MODULE transpiles, which corpus-diff does not diff.  (Reason
  to still run it cold, not a reason to skip it.)
- **A foreach loop-var write must reach EVERY bare scalar operand, in BOTH
  spellings** (`70e6e5c`, #262): one helper `_ev_foreach_alias_list` owns the
  `foreach-alias-list` veto; the block form calls it from `_tw_stmt`, the
  statement-modifier form from `_tw_stmt_expr`, and it vetoes every top-level
  comma slot that is a lone bare `$name`.  `$_ = "w" for ($s)` and
  `for ($p,$q){$_="w"}` were both silent-wrong.  Elements/derefs need no veto
  (they arrive as live boxes) — EXCEPT in the modifier spelling, where the v1
  seam lowers to `p-gethash` instead of `p-gethash-box` (#263, open).

## s361 (2026-08-08, Opus) — #153 step 3: both operand sites on the walker; the measurement population rule

- **MEASURE ON PERL'S OWN t/ TOO, not just the 111-file corpus.** s359's
  `PCL_TERM_DIFF` inventory over the census corpus said ZERO disagreements;
  the same probes over all 604 files of `t/*/*.t` produced THREE real shapes,
  two of them live silent-wrongs.  Any "measured then flipped" step in the
  #153 family runs over BOTH populations before the flip (helper:
  `tools/term-diff-sweep.pl`).  → session-log s361, task #153.
- **#153 step 3 DONE**: named-unary site (`3509115`) and strictly-1-arg site
  (`ece9d35`) both take their operand extent from `_term_extent`; a decline
  still falls to the legacy branches.  Both `PCL_TERM_DIFF` probes DELETED —
  once a site's answer IS the walker's, the probe can only report equality.
  Steps 4–5 (widen the walker to the declined shapes, fold reduction in,
  delete the dead `$end_pars` chains + `$deref_skip`) remain.
- **Paren-less call arity is decided by the PROTOTYPE, not by `min_params`**
  (`1279be6`): `min_params` is a MINIMUM, and reading it as "exactly one"
  made `sub f ($;$) {…}; f $a, $b;` silently drop every argument after the
  first.  The site now uses max-args AND perl's **trailing-`;` = LIST-operator
  precedence** rule (`sub unilist ($;)` ⇒ `unilist 0 || 5` is
  `unilist(0||5)`, while a plain `($)` is a named unary ⇒ `unilist(0) || 5`).
  Builtins are untouched (no `min_params` in their records).
- **A foreach LIST that is a single SCALAR is ONE element — decided in the
  EMITTER** (`d2bb91c`): the runtime cannot tell a box-wrapping-a-vector
  (`\@a`, `[1,2]`) from an `@array` box, and the ref-kind slot was rejected
  by measurement (s335), so `%p-flatten-for-list` was spreading the referent.
  The sigil is compile-time knowledge: `Pl::Parser::_foreach_single_scalar_p`
  is the ONE predicate, consumed by BOTH lowering sites (Parser2 block form
  and the v1 statement-seam modifier form), and the single scalar is routed
  through the SAME `(vector …)` shape a multi-element list already used.
  A scalar used as a foreach LIST is also force-boxed (VarAnnotator event
  `foreach-alias-list`) so `for ($x) { $_ = … }` writes back.
- **A sweep row that starts failing after a PARSE fix may have been passing
  under the wrong parse** — check the emission diff before calling it a
  regression.  pos.t t21 is the worked example: it scored "pass" while the
  statement parsed as `(ok($_[3])) =~ /\Ge/`; with the correct parse it fails
  for a real reason (#261, the `=~` target is a copy, not the arg box).
  Row added to `fail-baseline.tsv` by EDIT with that cause; hash.t's
  concurrent churn was measured PRE-EXISTING at the session-start commit
  (an empty-description baseline row — descriptions are join keys).
- **A bare NAME is a CALL only if it is callable WHERE the call site sits**
  (s374, task #266): perl decides top-down at compile time, so the answer is
  both QUALIFICATION-aware and POSITION-aware, and `Pl::PExpr::_bareword_callable_here`
  is the ONE place that knows it — consulted by both handle_subcalls branches
  that face a no-argument bareword (the binary-operator branch and the
  end-of-expression branch), which each used to carry their own copy of a name
  test that could answer neither half.  It is THREE-valued, and the two
  negatives are not interchangeable: `not-yet` (this file declares the name
  BELOW — positive knowledge that perl does not know it here either) reads as
  the string wherever it sits, while `no` (nothing this compiler can see) keeps
  answering CALL unless the word sits in operator context, because PCL's
  compile-time name knowledge is INCOMPLETE — measured: treating `no` as a
  string turned `next`, `goto again` and File::Spec's `curdir` into strings.
  Declaration SITES ride on `declared_subs` (`Pl::PExpr::TokenUtils::decl_site`
  / `site_precedes`); positions from two documents are incomparable and answer
  "callable", the old whole-file answer.  Control-flow words
  (`last`/`next`/`redo`/`goto`/`return`) are callable everywhere via
  `Config::control_flow_ops` — they are absent from `known_no_of_params`
  because their operand is a LABEL.  Guard `Pl/t/bareword-call-01.t`.
- **`explain()` DUMPS a ref** (s374, #236): Test::More renders every ref with
  Data::Dumper under Indent(1)/Terse(1)/Sortkeys(1) and passes a non-ref
  through; PCL stringified, so an is_deeply failure that printed its operands
  read `got 'ARRAY(0x53)'`.  The renderer in `cl/pcl-test.lisp` reuses
  `test-deeply-equal`'s shape test, so anything is_deeply can WALK, explain can
  PRINT, and it prints Dumper's `$VAR1` back-reference for a cycle or a shared
  ref.  Two deliberate differences, both commented at the code: no trailing
  newline (pl-diag would emit a bare `# ` line) and integer-vs-quoted decided
  by the CL type.
- **`-BAREWORD` autoquotes before `=>` and inside a HASH subscript** (s374,
  #234): perl reads `(-f => 4)` as the key "-f" and `$h{-f}` as that key, and
  the STRING reading beats the operator one.  A SINGLE-letter `-f` arrives from
  PPI as the FILETEST operator token, so both autoquote sites missed it (`-foo`
  and `-1` tokenize differently and were always right), the `$_` default took
  over, and the filetest's result ATE the next list element.  Settled at the
  three sites that own an autoquote decision — the fat-comma rewrite in
  `cleanup_for_parsing`, `_subscript_autoquote_text` (NEW: the ONE answer both
  subscript paths now ask, they had drifted into two copies), and the
  interpolation key regex.  NOT in the `$_`-default pre-pass: by then the
  element is already split and there is no `=>` left to key on.
- **`use` ARGUMENTS are compiled, never wrapped as raw text** (s374, #235):
  `use lib "$ENV{HOME}/x"` and `use constant X => "…"` are compile-time perl
  expressions.  Two hand-rolled sites wrapped `$tok->string` in CL quotes,
  which dropped interpolation AND mangled escaping four ways — `"a\nb"` → CL
  "anb", `'a\b'` → "ab", `'a"b'` → unreadable CL that killed the file at load.
  Both now use the ordinary expression path; the single-quote-token
  short-circuit in `_compile_constant_value` is DELETED (its own comment
  already said why NUMBERS must not be short-circuited — strings were the same
  trap one type over).  A general `use Module LIST` already compiled its import
  args, so the scope really is those two sites.

## s375 (2026-08-09, Fable) — s374 review: all four commits APPROVED; six asks ruled

- **s374 APPROVED as shipped** (`docs/fable-answers-s374.md`): gate
  independently re-verified **133/4773**; the #236 renderer probed
  byte-identical to perl (incl. integer-vs-quoted on stringified numbers);
  the #234 four-shape probe identical; both new filings (#275, #276)
  reproduced.
- **#266's three-valued asymmetry IS the ruling**: `not-yet` (declared BELOW
  in this file) is positive knowledge → the string anywhere; `no` (nothing
  visible) keeps answering CALL, because PCL's callable set is INCOMPLETE and
  a wrong CALL fails loud where a wrong STRING is silent.  Do NOT complete
  the callable set as a campaign — missing entries are fixed as ordinary
  bugs when a cause line names them (#149 precedent).  **Correction: the
  ask's mutual-recursion residue (`sub a { b } sub b { 1 }`) does NOT
  reproduce** — the site rule covers it, probed `a=b` both sides
  (`fable-answers-s374.md` §1).
- **The package-blind unqualified bareword path is ACCEPTED, unscheduled**
  (§2): probed PRE-EXISTING at `f44e947` (identical `B::pl-f undefined`
  crash), loud not silent, zero cause lines on any population.  Re-raise on
  a real cause line, like #191.
- **The #234 comment-linked interpolation twin STANDS** (§3): token sites
  share `_subscript_autoquote_text`; the text site cannot consume tokens,
  and lifting to a text predicate would re-encode tokenization as string
  matching (forbidden).  Two sites, different INPUTS → named-twin comments,
  not shared code.
- **#236's two Dumper deviations CONFIRMED** (§4): no trailing newline
  (pl-diag splits on newline); integer-bare-vs-quoted decided by CL type —
  the ir-spec data model's honest carrier of the SV IOK/POK distinction.
- **Cadence RULED** (§6): a parse/emission change may skip the same-session
  full sweep when corpus-diff is IDENTICAL over all 111 **and** the lib/
  shim transpiles in the change's argued reach are byte-compared **and** the
  Pl/t gate is green; the every-3rd–5th-change sweep cadence still applies
  and its TOTAL/LOST verdict is still the gate.  Fold a `--lib` mode into
  corpus-diff.pl the first time a change's lib/ reach is unclear, not
  before.
- **Queue**: Opus next = #275 (TAP plan line, FIRST — one mechanism with
  `plan()`) → #276 (empty-brace list-op argument; probe `map {}`/`grep {}`
  before widening) → #238 → #239; then #237.  The FOLD (#153) = Fable,
  begun s375.

## s375b (2026-08-09, Fable) — #153 FOLD chunk 1: phase-1 term reduction in the main loop

- **`_fold_terms` is the FOLD's first chunk**: parse() reduces every embedded
  postfix-bearing term to ONE node via `_reduce_term` before the arrow/
  subscript machinery runs.  NOT folded, each guard unit-tested in
  reduce-term-01.t: Word-led terms, Block-led terms (chunk 2, `$deref_skip`),
  terms followed by a raw Block/Constructor, the whole array (recursion
  guard), positions preceded by a Cast or `->` (mid-term), and Constructor
  starts not preceded by an operator — **PPI classifies `[...]` by its
  PREDECESSOR**, so after `)` (list slice) it is a SUBSCRIPT, and folding it
  as an anon-array orphans the term it subscripts (found live in ref.t/
  grep.t; same family as `_retag_braced_deref_subscript`).
- **The fold is emission-preserving BY MEASUREMENT over all four audit
  populations**: corpus-diff IDENTICAL 111/111 vs HEAD; A/B (same compiler,
  `PCL_NO_FOLD=1` toggle — no worktree, no normalization needed) SAME
  604/604 suite + 183/183 board + 21/21 lib shims, zero exit diffs; gate
  133/4784 PASS; perf neutral.  Hence NO sweep and NO cache-generation bump
  (s375 cadence ruling, all three legs).  `PCL_NO_FOLD` is the standing A/B
  instrument for chunks 2–3, deleted with the legacy branches.
- **Chunk-2 boundary probed vs perl**: `grep {a=>1}->{a}, LIST` = expr-form
  (deparse), `eval {…}->[0]` derefs eval's RESULT — both already match PCL,
  so Block-led folding must key on hash-constructor SHAPE, never on a block
  after a block-taking word.  #271/#211 unchanged from HEAD, still the
  FOLD's later acceptance targets.

## s375c (2026-08-09) — USER: the v0.1 public-release track is opened

- **Release plan = `docs/release-plan-v0.1.md`** (tasks #277–#283 + #217/#128
  promoted).  Two USER rulings: (1) **the runtime + saved core are COMPILED
  AT INSTALL** on the target — the XS bridge's install-time model, never
  first-use (#277); (2) **a pre-release IR pass**: re-measure the generated
  code, introduce the macro vocabulary where it costs no speed, and update
  `docs/ir-spec.md` normatively (#281 — the scoped pull-forward of Target B;
  #75 stays the full flag-day; emission-changing, full verification).
- Track order: FOLD chunks 2–3 → #217→#277/#278/#128 → #281 → #279/#280 →
  big bug hunt (the s360 E5 exit gate — this release is what it was deferred
  FOR) → #282 fresh-machine gate → #283 CI → tag v0.1.
- Open USER decisions listed in the plan doc: public name (PCL collides in
  the CL world; percolisp org exists), publish process docs or not, LICENSE
  body check, pclxs bundling, hosting/remote.

## s376 (2026-08-09, Opus) — #275 + #276 + the #238 shim batch

- **`use Test::More tests => N` is a PLAN, and it now reaches the TAP layer
  (#275).**  Test::Builder::Module::import strips an `import => [...]`
  export list and hands EVERYTHING ELSE to `plan()`, so `tests => N`,
  `'no_plan'` and `skip_all => REASON` all mean the matching `plan()` call.
  `p-use` returned as soon as it recognised a PCL-provided module, dropping
  the whole list — so such a file published TAP with **no plan line and no
  `done_testing`**, one no harness can judge (the #202 family).  Fix:
  `p-use` forwards the import list to `%test-import` in `cl/pcl-test.lisp`
  (that is where Test::More's semantics belong); it consumes `import`,
  hands the rest to the existing `pl-plan`, and lets `pl-plan` reject an
  unrecognised form as it already did.  A bare VERSION (`use Test::More
  0.88`) is not an import arg and stays plan-less, as in perl.  Guards:
  `Pl/t/tap-assert-01.t` (every expected block now opens with its `1..N`,
  plus one row for `no_plan` / `skip_all` / VERSION) and
  `Pl/t/goto-sub-phase-01.t`.  58 board files gain a plan line; no board or
  sweep row moves (the counters read `ok`/`not ok`, never the plan).
- **An empty `{}` in TERM position is an anonymous HASH, not a block
  (#276).**  perl's toke.c decides on the character after the brace, with
  no ambiguity to resolve.  PPI already labels `{}` a Constructor
  everywhere it can; the one place it still says Block is after a bareword
  in paren-less list-operator position — `f {}`, `explain {}` — where PCL
  fell through to the block-body parse and produced an empty ARRAY (`ref`
  said `""` and the call lost an argument).  Fixed in `Pl/PExpr.pm`'s
  single term-position Block arm via `_block_is_empty`, deliberately NOT
  folded into `_block_is_hash_constructor`: that predicate answers the
  map/grep/`(&@)` BODY question, where a bare `{}` is not valid perl at all
  (`map {} (1,2)` is a syntax error), so widening it would be a claim about
  input principle 9 says we need not read.  Emission moves in ONE corpus
  file (method.t's three `new{}` rows, already-failing "pl-new is
  undefined" either way — verified 93+38/163, identical to baseline).
  Gen bumped to **v2-126**, both transpiled artifacts restamped
  (byte-identical below the header).
- **#238 List::Util/Scalar::Util shim parity, first pass: 319 ok/120 not-ok
  → 398 ok/75 not-ok on the dist's own 38 files** (12→17 PASS, 5→2 FAIL).
  Implemented: `maxstr`/`minstr`/`reductions`/`sample`/`zip_shortest`/
  `mesh_longest`/`mesh_shortest` (were dying stubs); `zip` returns
  ARRAYREFS and `mesh` flattens (they were both the flat list);
  `product()` of nothing is 1; `head`/`tail` take `($@)`, clamp both ends
  and die "Not enough arguments"; the four `uniq*` separated by what they
  compare and what they RETURN (`uniqnum`'s key is `pack "d"` — the raw
  double, because stringifying collapses 1.4142135623730951 and
  ...54 into one key); `$List::Util::RAND` honoured by `shuffle`/`sample`;
  `pairgrep`/`pairfirst` scalar-context answers; one shared `_need_code`
  gives all ten block-taking functions perl's two error texts
  (t/undefined-block.t 0/18 → 18/18); `looks_like_number` accepts
  Inf/Infinity/NaN and `0 but true`, uses `[0-9]` not `\d` (MONGOLIAN
  DIGIT FIVE), and answers on an overloaded object's stringification;
  `isdual`/`isvstring` route through new `builtin::is_dual` /
  `builtin::is_vstring` (box-representation facts no plain Perl can ask).
  **Remaining 75 rows are all parked families**, listed on task #238:
  tie (#155), REF-vs-SCALAR (#163), prototype introspection, `weaken`,
  read-only SCALAR storage, taint, get-magic, `subname`, `@_` aliasing,
  error text (#149).

## s376b (2026-08-09, Opus) — #239 DIAGNOSED: `package X;` in a BLOCK does not re-home unqualified globals

- **An in-block `package X;` (inside `eval { … }` / `do { … }`) leaves every
  unqualified global resolving in the ENCLOSING package.**  Not
  $a/$b-specific, not eval-specific — reproduced with a plain `$z` in both
  `eval {}` and `do {}`.  perl puts the write in `Foo::z`; PCL puts it in
  `main::z`.
- **Mechanism**: the block lowers to ONE top-level CL form containing both
  `(in-package :Foo)` and the statements.  CL's reader interns every symbol
  in a top-level form BEFORE evaluation, so an `in-package` nested inside a
  form cannot change how the symbols around it were read.  A FILE-level
  `package X;` works precisely because D1-lite splits it into separate
  top-level forms.  The emitter already spells that region's PRE-DECLARED
  globals qualified (`(defvar Foo::$a …)`); only the use sites stay bare.
- **Fix shape**: #226's QUALIFIED-emission rule, applied to code-level
  blocks — a sibling trigger for
  `_requalify_block_our_after_pkg_switch` (Pl/Parser2.pm:157), which already
  performs exactly this rewrite for `our`-DECLARED names in the same region.
  The new part is deciding which bare names are globals; route it through
  the SAME resolver the detector/promoter use, never a sigil- or scope-blind
  scan.
- **Population**: 31 occurrences in 14 files across the audit populations,
  including NINE Class-Method-Modifiers t-files and Role-Tiny's
  method-conflicts.t (both dists carry open board failures, #135) — so this
  is a family, not the Sort::Versions one-off it was filed as.
- Two s344 hypotheses stay RULED OUT and must not be re-tried: `(caller)[0]`
  reporting the wrong package (it reports `Foo` correctly — instrumented in
  the real dist), and the symbolic read `${"Foo::a"}` (the READ is right;
  the WRITE went to the wrong package).  Full detail on task #239.

## s376c (2026-08-09) — USER: the three s376 questions

- **#239 goes NEXT, before #237.**  The in-block `package X;` bug is a
  measured family (31 occurrences / 14 files, nine of them
  Class-Method-Modifiers, plus Role-Tiny method-conflicts.t), its cause is
  pinned, and the fix reuses a rewriter that already exists.
- **#238's `subname` family is SIZED FIRST, not implemented** (task #284).
  exotic_names.t (1554 rows) + subname.t (21) both hang on PCL not being able
  to name a compiled CL closure.  Measure two things before any code: whether
  a function-object → name registry answers `subname()` at no hot-path cost,
  and how many of the 1554 rows need only a NAME versus the exotic
  NUL/newline/latin-1 stash names.  Bring the answer back before writing.
- **The CPAN board gets a fresh baseline, but only after a PER-FILE audit**
  (#208) — the s376 whole-board run is saved as `docs/cpan-board14-s376.tsv`
  (2053 ok / 483 not-ok at beb4187).  Same discipline as #223's sweep
  pass-baseline: a blanket refresh absorbs a loss silently.  Read ROWS, not
  the PASS/PARTIAL label; the one known LOSS (Role-Tiny extend-role-tiny.t,
  `Package Role::Tiny does not exist` at load) is diagnosed on #208 and must
  be explained or hand-edited in with its cause.  Best taken AFTER #239
  lands, since #239 moves board files.

## s377 (2026-08-09, Fable) — review of the s376 batch: all four commits APPROVED

Full rulings in `docs/fable-answers-s376.md`.  Gate independently re-verified
133/4785 PASS; #276/#275/#239 probed live vs perl.

- **Expectation-rewrite rule (Ask 1)**: a gate expectation may be rewritten
  only when (a) the new text is real perl's output probed live, (b) the diff
  is exactly the corrected divergence, (c) the edit STRENGTHENS the assertion
  (never deletes/loosens — that stays "never simplify"), and (d) the same
  commit carries an explicit guard row for the fixed behaviour.
- **`builtin::` is PCL's shim-dispatch seam for box-representation
  primitives (Ask 5)**: perl-shared names must match perl exactly; PCL-only
  names allowed only for box facts plain Perl cannot express, consumed by
  `lib/` shims; perl claiming a name later wins.  `readonly` → same seam
  when the storage fact exists; `prototype` is a CORE builtin → runtime,
  not the seam.
- **A change under `lib/` (or `cl/`) is invisible to corpus-diff (Ask 8)**:
  the s374/s375 second-sweep optionality NEVER applies to it — the shipping
  session runs the full sweep + the owning dist's files.
- **#239 fix shape ruled (Ask 7)**: sibling trigger on `_rewrite_var_uses`;
  four-way resolver (lexical incl. in-scope `our` → declaring package /
  magic-special / qualified / else → X) from the SAME scope walk the rename
  machinery uses; unclassifiable name DIES (s372 gate-SET bar applies);
  variables only — probe the in-region CALL shape during verification.
  NEW probe finding: the bare-block `our`-alias case diverges TODAY in the
  opposite direction (`our $x; { package Bar; $x = 1 }` writes Bar::x, perl
  writes main::x) — mandatory guard probe on #239.
- **uniqnum key (Ask 6) = HYBRID, measured**: integer-valued args within
  |v| < 2^64 key as exact integer text, else `pack "d"` (integral NVs beyond
  2^64 MUST stay pack "d" — 15-digit stringification collapses distinct
  doubles); ship undef→0 output coercion with it; buys 4 of the 6 rows.
- **Two predicates stand (Ask 3)**, s370-comma-walk style: paired comments,
  no refactor; a THIRD consumer of the strip/unwrap shape forces extraction.
- **Filed**: #285 (perl's no-plan ending diagnostic: stderr + exit 254 —
  the last #202 spelling; detection-widening bar applies); #286 (ambiguous
  `f {$k,$v}` / `f {%h}` intuit_curly shapes — deferred, #191 pattern,
  re-raise on a real cause line).

## s378 (2026-08-09, Opus) — #239 DONE: an in-block `package X;` re-homes globals

- **A `package X;` inside a BLOCK re-homes every unqualified package VARIABLE
  for the rest of that block — and v2 could not express that with the nested
  `(in-package :X)` it was emitting.**  CL's reader interns every symbol of a
  top-level form before evaluation, so a package switch nested inside one form
  changes nothing about the symbols around it.  Measured s378 across NINE block
  kinds: `eval`/`do`/named-sub/`BEGIN`/`if`/`while`/labelled/anon-sub/`sort`
  all wrote the ENCLOSING package's variable; only the plain bare block (the
  one shape D1-lite splits into separate top-level forms) was right.  Fix =
  `_requalify_block_globals_after_pkg_switch` in `Pl/Parser2.pm`, a sibling
  trigger beside the `our` one, rewriting the region's bare names to `X::name`
  through the same `_rewrite_var_uses` family rewriter → task #239,
  guard rows `Pl/t/transpile-test-09.t`.
- **The four-way resolver, and where "left alone" is not enough**: an `our`
  alias in scope is requalified to ITS DECLARING package, not left bare —
  leaving it bare let the section package answer, which is the s377 §9.1
  divergence (`our $x; { package Bar; $x = "X" }` wrote Bar::x where perl
  writes main::x).  That bare-block sibling is FIXED by the same pass, not
  filed as a companion.
- **`$a`/`$b` are immune to the switch, deliberately**: the sort lowering
  emits `(lambda ($a $b) …)`, i.e. it LEXICALLY binds those two symbols in the
  section's package, so requalifying them would leave a `sort { $a <=> $b }`
  inside a switched region reading `X::$a` while the lambda bound the
  section's — a working sort turned silently wrong.  Listed with the
  always-main specials in `%PKG_SWITCH_IMMUNE_VARS`; `@a`/`@b` are ordinary
  globals and are NOT immune.
- **Variables only — sub definitions, `*glob` installs and bareword CALLS
  inside such a region were already correct** (probed s378): they resolve
  through the package stack at lowering time, never through the CL reader.
- **"$Foo::bar" is ONE qualified name to every interpolation scanner**:
  `_interp_canon`'s captures now run to the end of a qualified name and
  `_interp_fixer`'s unbraced arms carry `(?!::)`.  Without that, renaming a
  variable called `Foo` reached into an already-qualified `@Foo::bar` and
  produced `@main::Foo::bar` (caught live by the M7 gate row).  The BRACED
  form `"${x}::y"` genuinely IS `$x` then text and keeps no guard.
- **`local` is not a binder for this pass**: `_symbol_is_declarator` grew an
  optional keyword-regex argument so the requalifier can pass the
  my/state/our subset — `local $v` in the region localizes the REQUALIFIED
  global and its symbol must be rewritten with the rest.
- **NEW ASK, measured (task #287)**: the `$a`/`$b` immunity is exactly what
  stops #239 clearing its board population.  Dropping it takes Sort-Versions
  `versions.t` from 65/31 to **96/0**, and simultaneously makes every
  `sort { $a <=> $b }` inside an eval/do/named-sub package-switched region
  return the list UNSORTED.  Both halves measured live s378.  The real fix is
  two-part — drop the immunity AND make the sort lowering bind the region
  package's pair — and it needs a design call, so it did NOT ride #239.

## s378b (2026-08-09, Opus) — #237 RE-SCOPED: regex interpolation drops direct subscripts

- **`$a[i]` / `$h{k}` interpolated inside a REGEX are silently dropped** — the
  subscript is left as literal pattern text, so `/^$a[1]$/` compiles as `$a`
  followed by the character class `[1]`.  Every regex consumer is affected
  (`m//`, `s///`, `split`, `qr//`, the braced `${a[1]}` form); DOUBLE-QUOTED
  strings are all CORRECT.  That asymmetry is the tell: `ExprToCL.pm`'s
  `_gen_interp_regex_pattern` is a SECOND hand-rolled interpolation scanner
  beside `PExpr/StringInterpolation.pm`, and it only ever learned `${name}`,
  `$name` and the `$name->[i]` / `$name->{k}` ARROW chains → task #237.
- **#237's two original premises are DEAD, do not retry them**: `pos()`/`\G`
  through a scalar ref is faithful (six probes, all identical to perl), and
  the Text::Balanced symptom is not an offset error but a total extraction
  failure — `qr/\G$_[1]/` compiles to `(?^:\G[1])`.
- **The fix needs perl's `intuit_more` heuristic, not a guess** (same bar
  #286 was deferred under, and this is the cause line that re-opens that
  family).  PCL's current "always regex syntax" rule is RIGHT for the shapes
  perl reads that way and they pass today — `/$x[abc]/`, `/$x[^a]/`,
  `/$x{2,3}/` — so a naive "always a subscript" fix would break exactly those.
  `/$x[0]/` and `/$x{k}/` are the failing side; `$x[0]` / `$x{2}` are
  genuinely ambiguous shapes where perl picks the subscript.

## s379 (2026-08-09, Fable) — s378 review: both commits APPROVED; six asks ruled; two resolver fixes shipped (`docs/fable-answers-s378.md`)

- **Absorb-vs-file-a-companion, refined**: a sibling bug may be ABSORBED into
  a ruled fix's scope only when (i) zero new mechanism — the ruled mechanism
  inherently answers it, (ii) loudly recorded, (iii) guarded in the same
  commit.  Absent any one, file the companion → `fable-answers-s378.md` §1.
- **#287 fix shape RULED**: two halves in ONE commit — drop `$a`/`$b` from
  `%PKG_SWITCH_IMMUNE_VARS` AND make the sort lowering bind the pair the
  comparator block actually reads (reuse `_pkg_in_effect_at`, rule 11); the
  perl gotcha list to probe (`sort Other::cmp` = caller's pair, etc.) is in
  §2.  Slot: after #237.
- **#237 fix shape RULED = (b′)**: ONE shared variable-reference EVENT
  scanner extracted from StringInterpolation.pm; dq-strings take every
  chain, the REGEX consumer applies the intuit_more bracket classifier per
  event; `_gen_interp_regex_pattern`'s private walk is DELETED as the
  acceptance criterion.  Do NOT route regexes through the dq interpolator
  wholesale (escape semantics differ).  **#286 is NOT folded in** — intuit_curly
  is a different heuristic at a different site; it stays deferred on its own
  cause line → §3.
- **Guard-row placement**: wall-time headroom beats thematic grouping; a
  family's rows may scatter — the task/DECIDED entry is the anchor → §6.
- **Signature params ARE binders to the package-switch resolver** (s379 fix
  `2af263f`): the head scan reads Prototype/Signature through the seam's own
  `_signature_param_specs`; a Symbol in a DEFAULT expr is a use; a pure
  prototype binds nothing.  And `my`/`our`/`state` in NON-declarator
  positions (fat-comma key, hash-subscript key, method name, `my sub`)
  contribute no binding and never die → §7.
- **#288 filed (pre-existing)**: a bareword CALL in a package-switched
  region resolves to the ENCLOSING package's sub where perl dies
  "Undefined subroutine &P::hello" — wrong-callee silent-wrong, task carries
  the layer-discriminating measurement.
- **Raw `.fails.tsv` row counts are NOT comparable to baseline row counts**
  (signatures.t: 90 raw vs 34 keys, 0 new) — compare through `sweep-diff`
  or by its join key → §7.
- **Variable-handling design review** (USER ask) →
  `docs/var-handling-review-s379.md`: 51 % of Parser2.pm is variable
  identity; 26 interpolation-scanner sites; directions A (bind-once symbol
  table, W12-style dual-run), B (one interp scanner = #237's mechanism),
  C (one promotion engine), D (defglobal + save/restore `local` dissolves
  poisoned-my — MEASURE FIRST, changes the IR contract), §7 (hoist only
  compile-time-referenced subs — measure first).  Standing: no new scanner
  fixes, no new suffix family, no new scope walk (§8).

## s379b (2026-08-09) — USER: the sign-off rule

- **A design change needs NO user sign-off when all four hold: (1) simpler,
  (2) clearer, (3) generated code FASTER (or unchanged — a change that
  SLOWS generated code is still flagged), (4) total compile/transpile time
  < 50 % worse.**  Take the measurements first; if they hold, proceed.
  This waives the DESIGN ask only — correctness gates (Pl/t gate,
  corpus-diff, sweep TOTAL/LOST, probes vs perl) apply in full, semantic
  behavior changes still follow the probe-and-rule process, and queue ORDER
  remains the user's.  Immediate effect: `var-handling-review-s379.md`'s
  directions D (defglobal + save/restore `local`) and §7 (selective
  hoisting) are no longer "user decisions" — they are measurement-gated
  work items under this rule.

## s379c (2026-08-09) — USER: three planning answers

- **#237 is SPLIT**: Fable designs/builds the shared variable-reference
  event scanner + the intuit_more bracket classifier + its probe table;
  Opus wires in the consumers (regex/dq/rename) and the guard rows after.
- **IR STABILITY GATES v0.1**: the intermediate code (emitted CL) must be
  STABLE at the first release — any change to the emitted shape happens
  BEFORE v0.1, not after.  Derived ordering: after #237 → #287, run the
  IR-affecting batch first — the §6 defglobal measurement (+ implementation
  if the s379b conjuncts hold), the §7 selective-hoisting measurement
  (same), and the #281/#75-scoped macro-vocabulary pass — THEN the
  mechanical v0.1 track (#277–#280, #282, #283) and the tag.  IR-NEUTRAL
  work (direction A binder + ports, #153 FOLD, direction C's internal
  fold) may land any time; direction A step 3 (emission-time naming) is
  v0.2 material unless it makes the pre-v0.1 batch.
- **The s379b < 50 % compile-time budget is PER CHANGE, drift watched**:
  each change is judged against the tree it lands on, but cumulative
  whole-corpus transpile time is tracked and the user is flagged if the
  total creeps past ~50 % over the s379 baseline (~65 s whole-corpus,
  measured s378).

## s380 (2026-08-09/10, Opus) — #287 DONE: `sort` binds the pair of the package it was COMPILED in

- **perl's rule, now PCL's: `sort` sets the `$a`/`$b` of the package the SORT
  was compiled in, never the comparator's.**  So a comparator compiled in
  `main` and called from inside a `package P;` region reads `$main::a` and
  sees nothing — `sort bylen`/`sort Other::cmp` from a region legitimately
  return the list UNCHANGED, in perl and in PCL.  Matching perl here REMOVED
  three PCL divergences; it added none.
- **`$a`/`$b` are no longer immune to the package switch** (the s378 entry
  above is SUPERSEDED on this point): they are ordinary globals, `#239`
  requalifies them like any other, and the sort lowering binds *the pair the
  comparator body actually reads* — `Pl::PExpr::_sort_pair`, feeding all four
  sort-lambda sites (paren block, block form, `sort NAME LIST`,
  `sort $scalar LIST`) and `ExprToCL`'s `$node->{params}`.
- **The discriminator is "was this node REQUALIFIED", not "what package is in
  effect"** — a FILE-level `package X;` is split by D1-lite into its own
  top-level form, so a bare `$a` there is already read as X's and rewriting
  it would only churn bytes.  `Pl::Parser2::_pkg_region_at` answers exactly
  that question (BLOCK-level `package X;` statement, word-shaped namespace —
  deliberately the same condition `_requalify_block_globals_after_pkg_switch`
  uses), which is why corpus-diff moved **1 of 111 files** (`sort.t`, only
  inside its `{ package Foo; … }` block).  It shares `_pkg_in_effect_at`'s
  ONE walk via the extracted `_pkg_stmt_in_effect_at` (rule 11 — no third
  package-in-effect resolver).
- **A qualified pair MUST be `(declare (special X::$a X::$b))` in the lambda.**
  The region's `defvar` is emitted INSIDE the enclosing top-level form, so it
  has not proclaimed the symbol special when the lambda is compiled — probed
  in bare SBCL, interpreted and compile-file both: without the declaration the
  parameter is a plain LEXICAL and `(symbol-value 'X::$a)` still reads the
  defvar's value.  A block comparator would survive that; a NAMED comparator
  and the `${(caller)[0]."::a"}` symbolic read would not.  The bare pair needs
  nothing (its defvars are top level), so every unswitched sort is emitted
  byte-for-byte unchanged.
- **A guard row must not make a claim the SORT ALGORITHM answers.**  Caught
  live in this session: a comparator with a constant NON-ZERO verdict prints
  perl's mergesort merge order (`2 3 1`) vs the runtime's `stable-sort`
  (`3 1 2`) — nothing semantic.  Zero-verdict rows are fine (both are stable,
  deliberately: `p-sort` uses `stable-sort` because perl's sort is stable);
  for a non-zero one, print what the comparator OBSERVED instead of the sorted
  list.  Guard rows: `Pl/t/transpile-test-09.t` ("package-switched sort binds
  the region's $a/$b pair (#287)").
- **Sort-Versions `versions.t`: 65 pass / 31 fail → 96 / 0** — the dist that
  filed the ask, now clean.

## s382 (2026-08-10, Fable) — #237 Fable half SHIPPED: Pl::InterpScan (event scanner + intuit_more port + probe table)

- **`Pl/InterpScan.pm` is THE variable-reference scanner for interpolating
  text** (ruled b′, `fable-answers-s378.md` §3; split USER s379c): `scan` /
  `scan_one` emit events (sigil/form/name/canon/span/name_span/chain/
  slice/postderef); `intuit_more` + `regcurly` are line-faithful ports of
  perl 5.40.3's toke.c/regcomp.c, read from the running perl's own source.
  NO consumer is wired yet — emission unchanged; Opus wires consumers +
  guard rows per `docs/interp-scan.md` (the contract + divergence table).
  Verified by `Pl/t/interp-scan-01.t`: a live-perl probe table (re-derived
  every run), classifier-vs-probe, and event-shape pins.  Per
  `var-handling-review-s379.md` §8, new interpolation behavior goes in this
  module or not at all.
- **CORRECTION to the s378b entry above**: probed, `/$x{2}/` is a
  QUANTIFIER (regcurly: `{2}` `{2,}` `{2,3}` `{,3}` `{ 2, 3 }` all are),
  NOT a subscript as the parenthetical guessed.  Subscript side: `{k}`
  `{'k'}` `{$i}` `{-3}` `{}` `{2x}`.
- **Only the FIRST bracket group is classified** — continuations always
  bind (`/$m[0][abc]/` dies on the bareword, `/$h2{k}{2,3}/` dies "Not a
  HASH reference"), and `->[`/`->{` bind unconditionally everywhere.
- **Braces CLOSE a reference in interpolation, both modes**: `"${x}[0]"`
  is $x then literal `[0]` (`"${m}[0]"` under strict dies on the SCALAR
  $m); `@{x}[0]`, `${ar}->@*` likewise.  Consequence: `_interp_fixer`'s
  `${x}[`-is-@-family arm has the sigil family WRONG (pre-existing; fix
  lands when the fixer becomes an event consumer — divergence table §2).
- **The weigher's symbol-table hook is real and ported** (`known_name`):
  `/$x[\n@foo]/` flips subscript↔charclass with @foo's existence; only
  1–2 digit guts are subscripts (`[100]`/`[123]` are charclasses).

## s382c (2026-08-11, Fable) — Direction-D audit DONE: GO, mechanism CORRECTED (docs/defglobal-audit-s382.md)

- **`sb-ext:defglobal` is DEAD for direction D** — probed: SBCL refuses
  `let` of a global name outright ("cannot be used in LET"); the
  var-handling-review §6 premise of free lexical shadowing was FALSE.
- **The working mechanism is the SYMBOL-MACRO GLOBAL**:
  `(define-symbol-macro $x (symbol-value '$x))` over an initialized,
  unproclaimed value cell.  Probed: `let` = plain lexical shadow (perl's
  `my`), closures capture it, save/restore-under-unwind-protect = `local`
  (die path included), read cost at parity with specials (111 vs 106 ms /
  100M).  Special and symbol-macro are mutually exclusive BOTH directions
  — a partition bug dies at load, never silently.
- **Corpus classification: NO third class** (675 defvar'd names, 113
  files): 416 my/param collisions (accidental dynamics today — D makes
  them correct lexical shadows), 348 runtime-magic rebinds (stay defvar),
  54+ `local` lowerings (become save/restore; `p-local-glob` already
  ships the idiom, runtime has NO progv), 32 declare-special = sort pairs
  only.  0 bare defvars, so the cell-init requirement has no gap.
- **Exception set (stays defvar), name-based and image-global**: `$a`/`$b`
  in every package + the runtime-magic list.
- **Payoff measured**: 809 poisoned-my renames in corpus emission
  (`__shadow__` 611 / `__cond__` 194 / `__emb__` 4) + 3 veto predicates +
  ~300 Parser2 lines delete; #205 closes by construction.
- **s379b conjuncts all hold → implementation proceeds without an ask**
  (pre-v0.1 IR batch; ir-spec.md + gen bump in the same commit).

## s382d (2026-08-11, Fable) — #289 mechanism FINALIZED after USER perf challenge (docs/direction-d-plan.md)

- **Access macro is `(sb-ext:symbol-global-value '$x)`, not `symbol-value`**
  — direct cell access, valid because partition symbols are never
  dynamically bound (holds by construction).  Measured: READS 20% FASTER
  than today's specials (63 vs 79 ms/100M, boxed shapes), my-shadow binds
  36% faster (29 vs 46 ms/10M), call-level parity, name-based
  symbol-value/boundp/makunbound interop probed intact.
- **FLAGGED regression (s379b conjunct-3 duty): `local` of an ORDINARY
  user global costs ~41 ns vs ~4.6 ns** (the unproclaimed-symbol setter
  is a full call; open-coding requires a proclamation that forbids `let`).
  Accepted because: rare (54+~20 corpus sites, none hot), absolute cost
  ≈ two hash lookups, and hot locals target MAGIC vars which stay defvar
  at today's speed.  bench-exec.pl before/after is a step-2 gate.
- **Rejected by probe**: plain symbol-value access (local 8.4×), progv
  local (4.8×), two-symbol defglobal cell (same speed as chosen, but a
  runtime-wide name→cell mapping audit; recorded as the upgrade path if
  a profile ever shows ordinary-global local hot).
- Plan of record for #289: `docs/direction-d-plan.md` (steps, gates,
  guard rows, artifact regeneration, rollback).

## s384 (2026-08-12, Opus) — #294: `foreach my $x` states its declaration in the IR (`:my t`)

- **Since the flip a lexical and an ordinary global are the SAME symbol, so
  "is this loop variable a package cell?" is not answerable from the emitted
  form alone.**  `%p-cell-loop-var-p` reads the macroexpansion environment,
  which is right for `foreach $x` (an enclosing `my $x` shadows the symbol
  macro; no global → plain `let`) and WRONG for `foreach MY $x` — the loop's
  own declaration is not in the environment yet, so a same-named package
  variable made the loop LOCALIZE the cell.  Measured: a sub called from the
  body saw the loop value (#294), and a closure made in the body saw the
  post-loop restore.
- **The compiler states it: `:my t`, after `:label`, before the body**, from
  BOTH foreach emitters (Parser2's compound branch and the v1 seam's
  `_process_foreach_statement`, still live for block-form args/anon subs —
  measured reachable in 20+ corpus files).  The runtime consults the key
  FIRST; `%p-cell-loop-var-p` is now only ever asked about an UNDECLARED loop
  variable.  Normative: `ir-spec.md` §6.2 "Loop variable: lexical or
  localized"; guard rows (incl. the must-not-break `foreach $global` case) in
  `Pl/t/transpile-test-10.t`.
- **This is a PRECONDITION for #291**: the poisoned-`my` deletion makes more
  names carry cells, which is exactly what turns this latent guess into
  live miscompiles.

## s384b (2026-08-12, Opus) — #291 MEASURED NOT SHIPPABLE AS PLANNED; four blockers filed

- **The poisoned-`my` renames have TWO causes, and `direction-d-plan.md` §4
  step 3 names only one.**  Besides "a `defvar` poisons the section's own
  `let`", `_seg_lex` makes `_forward_global_decls` SKIP any name the section
  let-binds, so without a rename the GLOBAL loses its declaration and is
  unbound at load (probed: deleting the block-my pass alone drops
  `(p-defcell @a …)`).  The enabler is to drop that exclusion in FILE mode
  only — **EVAL mode keeps it**, because there the same list is the
  p-eval-thunk's capture PARAMETERS from the CALLER, a question the flip did
  not touch.
- **The renames were IMPLEMENTING three things, not just dodging one.**  Each
  is now a blocker task with a live reproducer: **#296** `my $a`/`my $b` is a
  DYNAMIC bind because the exception partition keeps them `defvar` (closures
  lose the value; PRE-EXISTING, probed on plain main); **#297** a `my` in a
  C-for CONDITION has no `let` at all and writes the package cell; **#298**
  `my $x = <self-ref with a list-operator comma>` becomes a hard refusal,
  because `_rename_decl_within` skips the decl's own RHS and so was producing
  perl's "RHS reads the OUTER variable" for free.
- **#299: a "dead" cell is NOT emission-neutral.**  The enabler alone (declare
  every referenced name) costs 5 closure.t rows; corpus-diff shows the ONLY
  change to that file is 47 added `p-defcell` lines.  Same risk class as
  #294's `%p-cell-loop-var-p` — **before widening what gets a cell, grep for
  every place that reads "this name has a cell" as a proxy for "this name is
  a global"**.
- **Guard rows outlive a reverted mechanism**: all 20 rows of
  `Pl/t/transpile-test-10.t` pass both with and without the renames, because
  they assert perl's semantics via `test_transpile` rather than the emitted
  spelling.  They are the acceptance bar for the retry.

## s384c (2026-08-12, Opus) — #298: when a syntactic pre-check and PExpr disagree about a comma, ASK PExpr

- **`my $c = bless $c, "C3"` refused the whole file.**  `_lower_block` treats a
  depth-0 low-prec token in a `my`-init as a statement tail and hands the run to
  PExpr INSIDE the fresh `let`, where a self-referential init would read the new
  binding — hence the refusal.  But that comma is a LIST-OPERATOR argument
  separator, and only PExpr can tell (`my $c = h 1, 2` really does pass both).
- **The rule: lower the run in the OUTER scope and read the RESULT.**  Exactly
  one assignment to the declared name ⇒ there was no tail ⇒ its RHS goes into
  the `p-box-init` let binding.  Anything else keeps the refusal.  Lowered
  exactly once — PExpr's cleanup mutates the shared tokens, so a speculative
  re-lowering is not available.
- **Reachable on plain main**: `our $c1` stops the block-shadow rename from
  covering the name.  op/bless.t survives only because ITS `$c1` is poisoned.
- Guard rows in `Pl/t/transpile-test-10.t`, incl. the inverse (`my $x = $x, 1`
  still refuses).  corpus-diff identical across 111 files — a die became code.

## s385 (2026-08-12, Opus) — #297: EVERY `my` in a loop HEAD gets its own `let`

- **The head of a C-style `for` is one lexical scope, and every `my` in it
  scopes to the loop.**  The init counter always had a `let`; a `my` in the
  CONDITION or the STEP, and an init `_single_scalar_decl` declines
  (`my ($x) = …`, `my @a = …`), had none — the declaration lowered to a bare
  write into the package cell, so the name stayed defined after the loop and
  shared storage with a same-named global.  `while (my $x = …)` / `if (my $x
  = …)` have had the wrap since forever; this makes C-for consistent with
  them, through the SAME `_cond_my_names` + `_wrap_cond_mys` pair (rule 11).
- **Why it had to land before #291**: the `__cond__` rename was the only thing
  scoping those declarations.  Verified live by disabling the rename and the
  `_seg_lex` forward-decl exclusion (#291's enabler) together: all five shapes
  then run identical to perl.
- **The registry save/restore now covers the WHOLE head** (it used to start
  after the multi-counter branch), so the multi-`my` init no longer leaks its
  counters into a later sibling's string-eval capture alist either.
- corpus-diff: exactly 2 of 111 files (`my.t`, `loopctl.t`), every hunk the
  let-wrap plus the now-unneeded top-level `p-defcell` for the renamed name.
  Both files re-measured against HEAD in the sweep: identical (67/0, 52/0).
- **Known divergence, filed as #300 and normative in `ir-spec.md` §6.2**: the
  wrap is ONE binding for the whole loop, where perl gives the declaration a
  fresh instance per iteration.  Observable only by closing over it in the body
  — and it is NOT new: `while (my $x = shift @l) { push @c, sub { $x } }`
  diverges the same way and is untouched by #291.

## s385b (2026-08-12, Opus) — #301: ONE predicate answers "does this heredoc interpolate?"

- **A heredoc is RAW exactly when its terminator is SINGLE-quoted**, and perl
  allows both `~` and whitespace between `<<` and a QUOTED terminator:
  `<<'E'`, `<< 'E'`, `<<~'E'`, `<<~ 'E'` are all raw.  PCL asked this with FOUR
  hand-written regexes and every one was narrower than perl — PExpr's `/^<<'/`
  missed both `~` and the space, the three Parser2 copies missed the space.
- **A miss is SILENT-WRONG**: the text goes through string interpolation, so
  `$x`/`@y` vanish and `\n` collapses to a real newline, with no diagnostic.
  Same class as the #138 silently deleted statement.
- **`Pl::PExpr::TokenUtils::heredoc_is_raw` is now the only spelling** — it
  reads `{_heredoc_content} // content` (the rename passes rewrite the marker
  there) against `/^<<~?\s*'/`.  All four sites call it.
- Found by #299's triage: the one perl-tests assertion that catches it
  (closure.t RT #23265) lives inside a top-level form that dies at load today
  (#205), so the row never ran.  corpus-diff: 1 of 111 files, the one hunk
  being that heredoc going from a `p-string-concat` of interpolated pieces to
  a plain string literal.  Guard row covers all seven spellings and was
  verified to FAIL against the old predicate.

## s385c (2026-08-12, Opus) — #296: an EXCEPTION-partition `my` is RENAMED, not re-partitioned

- **The bug (pre-existing since the #290 flip, not caused by #291).**  An
  ORDINARY global is a symbol macro over a cell, so `my $x` beside one is a
  plain lexical `let`.  An EXCEPTION name ($a/$b and the runtime-owned set) is
  still a `defvar` — PROCLAIMED SPECIAL — so `(let (($a …)) …)` is a DYNAMIC
  rebinding: a closure made inside loses the value at scope exit.
  `sub mk { my $a = shift; sub { $a } }` printed nothing where perl prints the
  value.  Same for `my $b`, `my %ENV`, and `for my $a`.
- **CL cannot lexically bind a proclaimed special**, so no emission fixes this
  while the name stays `$a`: the declaration must get a different SYMBOL.
  `symbol-macrolet` over a special is undefined behaviour, so that is not an
  escape either.
- **RULED: rename the declaration (option (a)), do NOT shrink the partition.**
  Shrinking it — making $a/$b ordinary and having the sort lowering bind them
  with `p-local-cell` — would cost every sort CALL ~41 ns (plan §2), still
  leave `my %ENV` broken, and disturb #287's just-shipped package-qualified
  pair.  The rename costs nothing at runtime and is name-decidable: ONE
  `Pl::GlobalPartition::is_exception_global` call.
- **This is NOT the poisoned-my family.**  No poison test, no analysis of
  WHETHER — only the declaration's own scope, which perl states syntactically.
  It OUTLIVES #291, which deletes the other three passes.
- **Ordering is load-bearing**: it runs before `_rename_captured_file_lexicals`
  and before the three poisoned-my passes, so every later pass sees the final
  name.  Probed the other way round: a file-level `my $a` captured by a named
  sub then died "Parser2 TODO: file lexical 'a__excl__0' captured by sub foo".
- **Perl's own rule is preserved, and it is the surprising one**: a `my $a` in
  scope makes a sort comparator read the LEXICAL, not the sort-bound pair.
  Probed — perl `a=LEX`, PCL before `a=9` (the package pair, WRONG), after
  `a=LEX`.  A guard row asserts what the comparator OBSERVED, never the
  resulting ORDER: an inconsistent comparator's order is the sort ALGORITHM's
  answer (perl mergesort vs SBCL stable-sort), not a claim PCL can make.
- **corpus-diff: 42 of 111 files, four explained buckets** — (1) the rename
  plus pretty-printer re-wrap; (2) `__shadow__`/`__cond__` counter
  RENUMBERING, because the earlier pass now renames fewer names; (3) a
  promoted lexical moving from the exception partition to ORDINARY
  (`defvar $a` → `p-defcell $a__excl__N`), which is correct — it IS ordinary
  now; (4) the VarAnnotator and the params fast path giving the renamed
  lexical their NORMAL verdict (raw slot, `p-raw-params`) instead of the
  conservative one the special name forced — correct and faster.
- Nine Pl/t expectations that spell `$a`/`$b` as sample variables were updated
  to the renamed spelling (STRENGTHENS: the suffix pins that the rename fired
  AND the shape held); one was made whitespace-tolerant for the re-wrap.
- **B1 (string-eval capture) SHIPPED s387 (`a062914`)**: in EVAL-MODE
  compilation the CAPTURE ALIST beats the special table — a free $a/$b whose
  spelling the caller's alist carries compiles as that captured lexical (a
  fresh non-special `$a__evalcap__N` bound as a thunk param whose LOOKUP name
  stays `"$a"`); no key → today's special path, so an eval'd comparator with
  no `my $a` in scope still reads sort's dynamic binding.  The five-row
  acceptance table + both reproducers are identical to perl.
- **A capture-dependent emission makes the capture names part of the eval CACHE
  KEY** (s387): eval-mode compilation runs in the `pl2cl --server` SUBPROCESS
  (its request was `pkg\nlen\ncode`), so the names now travel in a `<captures>`
  header line and `*p-eval-string-cache*` is keyed on
  (source, pkg, capture-names).  Without that key, one eval string used from
  two scopes would reuse whichever emission compiled first — silent-wrong.
- **Block-scoped and FILE-level lexicals reach an eval by DIFFERENT mechanisms**
  (s387): a block `my` reaches it through the site capture alist; a file-level
  one reaches an eval inside a NAMED SUB only through promotion to a package
  cell (the sub is hoisted out of the file-level `let`), and that pass finds
  the declaration by its PERL name in the eval text.  So the #296 rename stands
  aside for exactly that case — file-level decl + a string eval inside a named
  sub — at no cost, because a promoted cell is not a `let`.  Keyed on "file has
  ANY string eval" instead, it reverted four corpus files' renames;
  corpus-diff caught it.
- **B2 (later declaration wins) SHIPPED s388; #296 CLOSED, branch merged.**
  `_rename_decl_within` walked from its declarator to the end of the scope, so
  an earlier `my $a`'s rename claimed uses belonging to a LATER `my $a`; the
  later declaration's own pass then found nothing left to rename and its binding
  sat unread.  The walk now ends its claim at a later declaration of the same
  canonical name, in the two shapes `_lexical_decl_scope` distinguishes:
  **same scope → STOP at the LAST TOKEN OF THE STATEMENT** (perl does not
  introduce the new name until the statement finishes, so the redeclaration's
  own initializer still reads the EARLIER variable — probed,
  `my $a = "X"; my $a = "[$a]"` prints `[X]`); **construct scope → SKIP the
  construct**, resuming after it, except the region `_lexical_decl_scope` names
  as evaluated outside it (`for my $x (LIST)`, `while (my $x = …)`,
  `for (my $x = 0; …)`).  A NESTED BLOCK redeclaration is unchanged — still
  `_ref_shadowed`'s call, which is positionally exact there.
- **`_ref_shadowed` cannot see EITHER kind of later declaration** (s388): it
  inspects Block/Sub parents, and neither a same-scope sibling statement nor a
  construct HEAD is a sibling of one.  That is why both spellings were live at
  once, and why the fix belongs in the rename walk, not in the shadow reducer.
  The construct-scoped twin was found by PROBING the family, not by the failing
  rows — `{ my $a = "O"; while (my $a = …) { print $a } }` printed the outer
  value and no test named it.
- **The Pl/t gate was green with BOTH regressions live** — for a change this
  wide the full sweep is the gate, not prove.

## s386 (2026-08-12, Fable) — s385 review: both commits APPROVED; #296's two fix shapes ruled

- **s385 APPROVED as shipped** (#297 + #301): gate independently re-verified
  cold 138/5103; eight probes vs perl identical (five C-for head shapes +
  shadow inverse + body read/write; seven heredoc spellings).  The one
  divergence found is exactly the filed #300, behaving as documented →
  `fable-answers-s385.md`.
- **#296 design call RATIFIED (option (a) rename), and taking it was right**
  — the sign-off rule selected the option needing no ask →
  `fable-answers-s385.md` §1.  Do not re-litigate.
- **#296-B1 RULED: NOT the progv seam** — a progv rebuilds the old
  dynamic-extent approximation one level down (fails the escaping-closure and
  comparator-under-`my` probes, 3/5 vs perl).  THE FIX: in eval-mode name
  resolution the CAPTURE ALIST beats the special table — an exception name
  that is an alist key compiles as the renamed captured lexical (the ordinary
  `__shadow__` path: read/write/#295 pad chain inherited); no key → special,
  unchanged.  Alist membership IS perl's "was a `my $a` in scope here".
  Five-row acceptance table in `fable-answers-s385.md` §2a + task #296.
- **#296-B2 DIAGNOSED: sibling same-scope redeclaration** — an earlier
  exception-`my`'s rewrite region must STOP at a sibling redeclaration of the
  same name (B-ii only covered NESTED redecls); two-line reproducer in task
  #296.  The "not isolated, file-context dependent" conclusion was a
  by-NUMBER TAP join across shifted numbering pointing at a region that in
  fact passes → `fable-answers-s385.md` §2b.
- **STANDING RULE (ask 3, adopted): a sweep-diff bucket count is meaningless
  without the file's row TOTAL, in BOTH directions — and a row NUMBER is only
  meaningful within the run that produced it.**  Join TAP by description;
  for unnamed rows re-derive number→source from the CURRENT tree's own TAP,
  never from the other tree's numbering → `fable-answers-s385.md` §3.
- **Shape-assertion expectation edits (ask 4)**: substitute conjunct = the new
  text is copied from the emission of a build whose RUNTIME behavior for that
  snippet is perl-verified; default on sample-name collision = RENAME the
  sample + one dedicated interaction pin row; the branch's nine edits stand →
  `fable-answers-s385.md` §4.
- **#300 stays filed, unscheduled** (probe-confirmed to behave exactly as
  documented; interleave on a real cause line, no campaign) →
  `fable-answers-s385.md` §5.
- **runpcl/runt no longer delete the program's blank lines** — the blanket
  `s/^\s*\n//gm` noise filter falsified byte-compares vs perl for any output
  containing `\n\n` (leading blanks only now; fixed s386, verified
  byte-identical on three probes).  `tools/run-dist-t.pl` never had it.

## s386b (2026-08-12, Fable) — USER-asked duplication review: the numbers behind the queue

- **v1 is still the PRIMARY expression compiler — 88% of seam expressions
  fall back to `_parse_expression_form` (16,897/19,165); ExprToCL2 native
  ~12%, unchanged since s316t.**  Measured by a full-corpus dynamic call
  trace.  #153's FOLD is the ruled answer; fallback rate = its progress
  metric; chunk 0 = move `_let_bound_vars`/`_catch_labels`/
  `_eval_span_captures` out of the fallback_parser object →
  `compiler-duplication-review-s386.md` §1, task #153.
- **~3.5k lines of the compiler are confirmed dead on the corpus** — the
  pre-E2 TEXT emitters beside their `*_form` twins (ExprToCL 2,238 lines),
  BlockAnalyzer (whole module), 437 lines of superseded v1 handlers,
  VarAnnotator's W12 text-scan remnant.  Deletion batch = task #303,
  AFTER #291; bar = corpus-diff byte-identical.  Eval-mode/bundle/
  instrumentation paths excluded from dead claims (trace = file
  transpiles only) → review doc §2.
- **`PExpr::DEBUG` is a real sub called 4.3M times per corpus transpile**
  (`sub DEBUG { $DEBUG_VAL }` never inlines; SET_DEBUG has zero callers).
  Fix = `use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0` — #303 step 0.
  Hot-spot numbers (accessors, `CLForm::_flat` ×1.24M) recorded in #213.
