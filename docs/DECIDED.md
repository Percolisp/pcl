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
  shapes) → `fable-answers-s318.md` §1.
- **tie on ARRAY/HASH**: interim = loud stderr WARNING + not-supported
  subsection (NOT a die — would CRASH avhv.t-class files); real support
  waits for boxed aggregates → `fable-answers-s318.md` §1, task #155.
  SHIPPED s320: `%p-warn-aggregate-tie` in `pcl-runtime.lisp`, one line per
  (kind, class) per process so a tie in a loop stays one line.
- **Read-only aggregates** (`Internals::SvREADONLY(@a,1)`): storage-swap to
  a simple vector, post-R1; never a weak-hash probe on the push path; do
  not bless as not-supported → `fable-answers-s318.md` §2, task #159.
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
- **Copied-file corpus split**: t/ originals are authoritative; the runner
  no longer hides a t/ file behind a `perl-tests/` copy of the same name;
  drifted copies re-sync post-R1 → task #150, `fable-answers-s316v.md` §6i.
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
