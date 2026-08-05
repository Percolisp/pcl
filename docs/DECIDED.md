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
