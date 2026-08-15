# Fable answers to the s399 + s400 review requests (ruled s401, 2026-08-15)

Two batches, reviewed together (s399's review had not happened when s400
started).  Requests: `docs/opus5-review-requests-s399.md` (five commits, §6
+ §7 the two design asks, the addendum's two decisions, the USER portfolio
ASK) and `docs/opus5-review-requests-s400.md` (four commits, §1 quoting, §3
the #348 release condition, §4 the gen-stamp promise).

**Both batches are APPROVED as shipped.**  No review fix was needed.  Two
NEW silent-wrongs were found by the review's own probes and filed (**#349**,
**#350**, §9); #217 is re-scoped by #349.  The plan for the coming sessions
is a separate document, **`docs/plan-post-s400.md`** — this file holds the
rulings it rests on.

## 1. Independent verification (not inherited from the requests)

- **Gate** (`tools/prove-core`, warm cache): **143 files / 5278 tests**,
  failures exactly the 13 pclxs xs rows (`xs-01.t` ×5, `xs-02.t` ×4,
  `xs-03.t` ×4 — the ABI-6-vs-8 drift the user has said to ignore).
  Matches s400's count to the row.
- **Full sweep RE-RUN** (the s400 `.faillog` held only the two targeted
  re-runs, closure.t and pack.t, so the verdict could not be recomputed from
  artifacts): `perl sweep-perl-tests.pl --jobs 8` → **GATE clean, 0 new /
  0 fixed, TOTAL passing 18516 = baseline (+0)**, the standing 2 UNSTABLE
  (postfixderef.t / ref.t crash-file noise) + 8 unverified; 62 fully passing
  files (s399 quoted 64 — the difference is which 0-fail files stopped early
  this run, chop.t/reset.t/state.t; TOTAL and LOST are the bar and both
  hold); min MemAvailable 1.9 GB (the run overlapped the gate).
- **Cache generation** v2-148; all THREE artifacts stamped v2-148
  (`Pl/t/artifact-staleness-01.t` green).
- **Diffs read end to end** (s399a–e, s400a–d).  Notes, none a blocker:
  - s400a: `quote => 0` for the sweep and `./runpcl` — preserved
    byte-identity, ruled below (§7.1).
  - s400c: `run_perl`'s CL arm now says it returns nothing rather than a
    silent `*p-undef*` — correct rule-12 shape; the transpilable stub is
    the implementation both populations reach.
  - s400d: the guard reads bytes and COUNTS its exclusions.  That count row
    is exactly why the third artifact was found in s399a; keep writing
    exclusion counts into guards.
- **Fresh probes vs perl 5.40.3** — the review looked at what the checked-in
  artifacts DO when they load, since s399a/s400d touched them twice without
  running them under a program that modifies `@INC`.  Two divergences, both
  new, both filed (§9): the artifact preamble resets `@INC` at first
  `pack()`; a top-level `require Bareword` is hoisted above a preceding
  runtime `push @INC`.

## 2. s399 §1 — #331: an emission-changing commit regenerates the artifacts in the same commit: APPROVED

Right trade.  The alternative (a separate follow-up commit) is exactly the
gap that produced eleven generations of drift.  Cost is `tools/rebuild-pack`
(~40 s) plus pack.t only when pack's emission really moves — bounded, and
the staleness row says the same session when it is skipped.  Standing rule
(DECIDED s399 already carries it).

## 3. s399 §3 — op/const-optree.t NOT registered: CONFIRMED, and the general point is a standing rule

**A registration authorised in the abstract still has to survive its per-row
read** — second time (s393 `unlike` was the first).  My s397 §4 ruling said
"every diverging row a `B::` readout per the request"; the request's summary
was wrong by 9 rows and the per-row read caught it.  The split (53 / 5 / 4)
stands in not-supported.md; the file re-registers only after #337 lands and
the four `retval of my sub` rows pass.  DECIDED gets the one-line rule.

## 4. s399 §4 — #221's trigger has fired: SCHEDULED, but POST-v0.1

The trigger was "the first test family whose failure is *warning not
emitted*"; it fired with 145 rows (24 sweep + 121 companion) and one cause.
**Ruling: #221 moves from UNSCHEDULED to the first item of the post-release
correctness backlog — not into the v0.1 track.**  Reasons:

1. It is a compiler + runtime feature (`use warnings`/`no warnings` compiled
   per lexical scope into a dynamic var; the runtime consults it), and its
   two consumers touch the HOTTEST coercions ("Use of uninitialized value"
   lives on the undef branch of `to-string`/`to-number`).  The design
   constraint is *zero cost on the non-undef path* — the check may live only
   on the branch that already knows the value is undef — and that needs the
   bench, which is R2's instrument, not v0.1's.
2. Nothing in the v0.1 track needs it: the rows are honest RED with one
   named cause and an owner; CPAN board rows blocked on it are zero today.
3. v1 shape is unchanged from the s338 ruling: ONE lexical boolean, no
   categories, and only diagnostics with a MEASURED consumer (uninitialized
   value; odd-elements hash assign; the print-on-unopened-handle warning
   from s337c).  Text fidelity per the user's standing rule (shape, not
   bytes).

The standing rule holds until then: default-off diagnostics stay ABSENT,
never unconditional.

## 5. s399 §5 — #338 (F-D container residue): unscheduled filler; direction D is the fix SHAPE for b1

Zero rows behind it, and it is a decline on ordinary Perl (three `my @a` in
different scopes across a package boundary is normal code).  Ruling: stays a
filler behind the release track.  When taken, b1's fix is to DELETE the
container loop's file-uniqueness rule if direction D's "a `let` of a
symbol-macro name is legal CL and shadows it" makes it unnecessary (the
comment justifying the rule predates D) — measured at the s372 gate-SET bar
over both populations, not argued.  b2 (container inside a list decl needs
the decl lowering) is a separate half-session; do not fold them.

## 6. s399 §6 + §7 — the drop family: the census is phase 2's metric, the announcement moves to the DROP site, and the file-level lvalue refusal keeps dropping (loudly)

Three rulings, one design.

**6.1 — the census + reproducer go into Option B phase 2's acceptance set:
YES.**  `f ref $u, "m" or g "fb"` (with #259 and #335, the same
fall-through) is the `$end_pars` region, and CLAUDE.md's "do not patch in
place" holds.  `docs/parse-error-drop-census-s399.tsv` is phase 2's metric:
after phase 2, `tools/drop-census.pl` must show the number FALL with every
remainder explained by cause.  Phase 2's sizing is Fable's, after #281
(plan §Fable).

**6.2 — #339: option (b), APPROVED with four amendments.**  Say nothing at
`Pl/PExpr.pm`'s die (a decline is not an event); announce ONCE where the
statement is replaced by nil — `Pl/Parser.pm`'s two `PARSE ERROR` emitters
via `_shape_expr_error` — naming FILE, LINE, the statement's source text and
the reason (the die's first line).  Amendments:

- (i) fixed stderr prefix `PCL: statement dropped at F line L: <text> —
  <reason>` so runners and `tools/gate-set-scan.pl` can key on it; it is a
  TRANSPILE-time diagnostic (the runtime never sees a drop) and pl2cl's exit
  status stays 0 — dying is 6.4 below.
- (ii) the emitted comment text stays byte-identical, so `corpus-diff` is
  clean and the bar is the s372 gate-SET scan over both populations + gate.
- (iii) the two `$SIG{__WARN__}` workarounds (`Pl/VarAnnotator.pm` ~695,
  `Pl/Parser2.pm` ~8415) are DELETED in the same commit if this warn was
  all they silenced (their comments say so; verify by running without them
  first) — two workarounds for one line was the tell, and the reuse rule
  says the workaround leaves with its cause.
- (iv) the `ref=''` case (comp/final_line_num.t, op/closure.t, re/pat.t,
  re/pat_re_eval.t) is probed BEFORE the commit: a node with no class
  reaching term dispatch is either a repair-pass artefact (then a bug of its
  own — file it) or a legitimate decline (then say which token).  One
  probe, on the task.

**6.3 — the 33 file-level "Can't modify non-lvalue subroutine call in
assignment" drops: KEEP the drop, make it loud, cite the entry.**  perl
refuses the whole FILE; PCL drops one statement.  User `:lvalue` subs are a
blessed not-supported feature (`not-supported.md` §lvalue subs), so this is
not a missing case but a refusal, and the s329 boundary decides it: dying at
transpile takes every other row of substr.t / op/sub_lval.t with it (the
state.t lesson, 88 rows), while the drop loses one statement and the sin is
only its silence — which 6.2 removes.  Add one sentence to that
not-supported section ("in file mode PCL drops the assignment statement and
announces it; in eval mode the eval fails, as perl's compile error does").
The eval-mode half is untouched.

**6.4 — when does a drop DIE?**  At the END of phase 2, as its last step:
once the census is explained and near zero, the `PARSE ERROR` emitter flips
from announce to die (rule 12 proper) — not before, because today a die
means TRANSPILE-FAIL for 72 files, i.e. every row of bless.t for one dropped
statement.  Recorded on #343.

**6.5 — the drop GATE: a runner COLUMN, not a Pl/t row.**  #343's piece 3
proposed a blessed per-file drop count that fails the session a new drop
lands.  Approved in this shape: the sweep already has every file's emitted
CL — it records `drops` per file in `.faillog/_status.tsv`, and
`sweep-diff.pl` gains a **DROPS bucket** compared against the census tsv
(the census IS the baseline; a drop leaves it by EDIT, like fail-baseline
rows).  `tools/run-perl-suite.pl` records the same column in its snapshot
for the companion population.  Zero extra cost, both populations covered on
every run they already get, and `corpus-diff`'s counter (s399f) stays the
per-change instrument.  NOT a Pl/t row that transpiles 658 files.

## 7. s400 asks

**7.1 §1 — `quote => 0` (sweep + `./runpcl` unquoted, suite quoted):
UNIFY TO QUOTED**, as a filler inside #277's session.  A checkout under a
path with a space is a fresh-machine reality (#282), and one builder that
quotes is the whole point of #344; the byte-identity was the right bar for
the MOVE, not a property to keep.  The diff for every path in this repo is
empty (nothing to escape), so the change is provably inert here and only
matters where it should.

**7.2 §3 — #348's release condition: CONFIRMED as "#346 and #347 fixed",
in that order, and #346 goes FIRST.**  Land-early-and-register is rejected:
a HANG in the companion run is precisely the tail #345 exists to remove
(adding one on purpose is backwards), and perl-tests/closure.t PARTIAL
loses ten rows the sweep would then never see — the #176/#204 hole in the
population that matters most.  Nothing is hidden by waiting: the vacuous
rows are counted on the task and the diff is four lines.  Order: **#346**
is a harness bug in `tools/pclperl-for-tests` (fd 3 is OPEN in the PCL
child and not in a perl child — a leak to every program it runs; the cheap
discriminating measurement is on the task: `/proc/<pid>/fd` at the stall)
and is worth doing on its own before any child-spawning test is trusted;
**#347** is a compiler gap (own half-session, `Pl/t/closure-01.t` extended
first); then #348 lands with row-by-row baseline edits.

**7.3 §4 — the gen-stamp promise: YES, in `ir-spec.md`.**  One paragraph
under the load model: *line 1 of every emitted file is
`;;; pcl: pipeline=v2 gen=<generation>`; it is a promise tools may key on
(`artifact-staleness-01.t`, `no-hardcoded-paths-01.t` cite it).*  Ride in
the next docs commit; the two guards then cite the spec section.

**7.4 s399 addendum (1) — re/pat_psycho.t + re/speed.t: REGISTER the
allowance.**  Their 12 rows are real (the files now run what they exist to
time), and with `--quick` as the default companion form (§8) the full run
that pays their minutes is rare and deliberate.  In quick mode the
allowance cap lists them NOT-RUN.

**7.5 s399 process note — "if you want the cadence rule to win, say so":
YES.**  The decision table in §8 replaces the count rule — and applied
honestly to s399 it says ALL FOUR sweeps were owed: #331 (a `cl/` artifact
change), #323 (a harness change), F-D (a span RENAME change: the sweep IS
the gate) and #332 (corpus-diff identical, but it added a `cl/` runtime arm,
`p-alias-array-elements`).  The request's "two were predicted-null" applied
its own criterion loosely — "no `cl/` change and not name-resolution" was
false for both.  That is exactly why the rule is a TABLE keyed on the change
and not a judgement per session: the sweeps it removes are the ones with no
`cl/`, `lib/`, harness or rename row firing, and only those.

## 8. The USER's portfolio ASK (#345): what runs when — a decision table keyed on WHAT CHANGED

The count rule ("every 3rd–5th change") is RETIRED — it cannot say WHY a
run happens, so it under-fires exactly where it hurts (s386/#296: a
Pl/t-green RENAME change with two live sweep regressions — a count rule
would have batched it) and invites "run it to be safe" everywhere else
(§7.5: the s399 request mis-read its own criterion in both directions).  What replaces
it is keyed on the KIND of change, because each measurement in the
portfolio is blind to a known set of change kinds.  Rows are additive: every
row that applies fires.  Costs measured s399/s400.

**Always, per change:** `tools/prove-core` (~4.5 min).  If anything under
`Pl/` changed: `tools/corpus-diff.pl` (~2 min — READ its SILENT-DROP line)
and `tools/emission-ab.pl --ref <base> --list lib/**/*.pm` (the lib reach,
seconds).  Plus the targeted files the change names.

| what changed | full perl-tests sweep (~10 min) | companion suite (`--quick` ~15–25 min; full `--all` 30–60) | also |
|---|---|---|---|
| `Pl/**`, corpus-diff IDENTICAL, lib byte-identical, NOT a name-resolution change | **NO — it cannot move; do not run it "to be safe"** | no | — |
| `Pl/**`, corpus-diff shows diffs | YES, after every diff is explained per file and probed vs perl | the dirs whose files carry the shape (`grep -a`); `--quick` once if the shape is broad | gen bump + `tools/rebuild-pack` (the staleness gate enforces it) |
| `Pl/**` name-resolution / scoping / rename / capture / promotion (Parser2 `_rename_*`, `_promote_*`, VarAnnotator, GlobalPartition, eval capture, span passes) | **YES — the sweep IS the gate** (#296) | `--quick` once | gate-SET scan over both populations when a checker / refusal / decline WIDENS (s372) |
| `cl/**` runtime | YES (invisible to corpus-diff) | the dirs the change touches (op/ for an operator, io/ for IO, re/ for regex) | rule-12 read of the touched dispatch |
| `lib/**` shim | YES | the files that `use` the module (`grep -a`) | — |
| harness: `perl-tests/t/test.pl`, `cl/pcl-test.lisp`, `cl/skip-registry.lisp` | YES | **`--all --quick`** (both populations reach it) | baselines edited ROW BY ROW |
| runners: `sweep-perl-tests.pl`, `tools/run-perl-suite.pl`, `tools/lib/PCLSbcl.pm`, `tools/pclperl-for-tests`, `Pl/t/PCLCore.pm` | the runner that changed, once; verdicts compared file-by-file against the previous run | same | `PCL_SHOW_SBCL=1` before/after diff |
| `docs/**`, `tools/t/**`, memory | nothing beyond the gate | no | — |

Companion cadence: **`--quick` is the default form** (skips the #326
hang set and caps registered allowances at 120 s, LISTING every skipped or
capped file as NOT-RUN — never silently); the FULL `--all` at most once per
session and only when a row above says so, before a snapshot re-bless, or
for a Fable review.  The #326 set is never worth its timeout except in the
full run.  Fable review sessions: cold gate + full sweep + probes
(unchanged) + a `--quick` companion when the reviewed batch touched name
resolution or the harness.

The three new tools: `corpus-diff`'s drop counter is per-change (free);
`gate-set-scan.pl` is on-demand and MANDATORY only for the checker-widening
row; `drop-census.pl` becomes the runner column of §6.5 (no extra step) and
the standalone tool re-blesses the census.

`--quick`'s bar (#345): it prints what it skipped and why, and one `--all`
vs `--all --quick` comparison shows identical verdicts for every file that
runs in both.  The table goes into CLAUDE.md verbatim (short form) in this
session's docs commit; #345 implements `--quick` + the allowance cap.

## 9. Review probes — two NEW silent-wrongs, filed

**#349 — the checked-in artifacts' preamble RESETS `@INC` (and
`*pcl-pl2cl-path*`, `*p-core-inc-dirs*`) when they load, i.e. at the first
`pack`/`unpack`/`mro::*`/`warnings::enabled` call.**  Probed:

    push @INC, "/tmp/mylib"; my $p = pack("N", 42);
    print scalar(grep { $_ eq "/tmp/mylib" } @INC);     perl: 1   PCL: 0

    push @INC, $dir; pack("N",1); require MyLocal;       perl: loads
                                                          PCL: Can't locate MyLocal.pm

`p-load-extension` `load`s `cl/pcl-pack.lisp`, whose first lines are a
PROGRAM preamble — `(setf pcl::@INC (make-array 0 …))` + the build
machine's dirs.  A program that modifies `@INC` at runtime and then calls
pack loses the modification, silently (a `BEGIN`/`use lib` push survives
only when nothing needs `@INC` after the first pack).  This is also the
whole of #217's problem: the artifacts embed the author's paths BECAUSE
they carry a program preamble they never needed.  **Fix shape (one
mechanism, closes #217): `pl2cl --extension` (module-artifact mode) omits
the program preamble entirely** — the artifact keeps its gen stamp on line 1
and gains ZERO machine paths, so `no-hardcoded-paths-01.t` TIGHTENS to zero
exclusions, the installer (#277 item 2) no longer needs to regenerate
anything, and `p-load-extension` gets a rule-12 guard (assert `@INC`
unchanged across the load, die naming the extension).  Measurement first:
a temporary before/after `@INC` compare in `p-load-extension` over one full
sweep says how many perl-tests files hit it today.  User emission is
untouched → corpus-diff identical → no gen bump.

**#350 — a top-level `require Bareword;` is HOISTED into the declarations
bucket (`p-eval-always`), ahead of every runtime statement — so a preceding
runtime `push @INC, $d;` has not run yet.**  Probed:

    push @INC, $dir;  require MyLocal;  print MyLocal::hi();
        perl: hi from MyLocal        PCL: Can't locate MyLocal.pm in @INC (…)
    BEGIN { push @INC, $dir }  require MyLocal;              both: works

Emission shows `(p-eval-always (p-require "MyLocal"))` BEFORE
`(p-run-compile-phase-blocks)` and the `(p-push @INC …)`.  Nested requires
already stay inline (s149, s3xx); only the file-top case hoists.  Loud, not
silent, and one companion file (io/perlio.t) by static census — a filler,
but the fix is a MEASUREMENT: emit the file-top `require` at its own
position (read-time package existence is separately guaranteed by the
emitted `p-defpackage`), corpus-diff both populations, explain every diff.
If something depends on the hoist, the task records what.

## 10. Sweep verdict

    TOTAL: 18516 passing, 917 failing, 11977 skipped across 108 files (+ 3 files skipped)
    TOTAL passing: baseline 18516, current 18516 (+0)
    summary: 0 new, 0 fixed, 2 unstable (crash-file noise), 8 unverified (did not run)
    GATE: clean
