# Fable answers — s439 (2026-08-23): the s438 batch (s438 → s438i) REVIEWED, and the distance to the first release

The nine Opus commits since the s437 review, taken as ONE batch because they
are one queue walked in order (`docs/plan-post-s433.md` §1):

* **s438** — the two census instruments (#473 cpan-`.t` population, #472
  `PCL_DROP_LOG`), the companion scan's five unrefreshable rows (s434 ask 1).
  No product change; #478–#483 filed.
* **s438b + s438c** — Q4: #453 (a user named unary takes the named-unary
  operand site) and #365 (an imported `()`-prototype sub is a term).
* **s438d + s438e + s438f** — Q5: #454 (a signature parameter is a
  declaration), #455 (the signatures feature is on from the pragma's own
  line), #435 (every fragment re-parse runs the token repairs).
* **s438g + s438h + s438i** — Q6: #452 (a qualified bareword handle is a
  name), #451 (punctuation-array elements interpolate), #450 (a
  metacharacter-free glob pattern is itself); #449 deliberately left with
  #418.

All asks are in `docs/opus5-review-requests-s438.md` (12 of them, four
parts).  Verdict first, then what was re-run, then the asks, then what this
session changed, then — the USER's question — where the project stands and
what is left until the first release.

## 0. VERDICT: all nine commits APPROVED as shipped

Nothing to revert, nothing to re-do.  Every number I could re-measure
reproduced exactly; every probe matched perl except the pre-existing
residues listed in §1.3 (attributed on a `d0b52e9` worktree, filed).  One
review fix (§3) — a one-regex residue of #455 that the probes found.

| leg | s438i claimed | s439 re-measured (COLD cache, this tree) |
|---|---|---|
| gate `tools/prove-core` | 166 / 5779, only the 13 pclxs xs rows | **166 files / 5779 rows COLD before the review fix, 166 / 5780 after it; failures = xs-01 ×5, xs-02 ×4, xs-03 ×4 and nothing else** |
| full sweep (`--jobs 8`, own gate) | TOTAL 18312 (+0), 0 new / 0 fixed, drops 5 = census, GATE clean | **TOTAL passing baseline 18312, current 18312 (+0); 0 new / 0 fixed; drops census 5 = current 5; CHILD DROPS 241 in 98 files; GATE clean** (7 unstable / 10 unverified = the usual PARTIAL-file noise) |
| companion `--all --quick --jobs 4` | 528 files, zero real movers, both SNAPSHOT holes zero | **528 files, ZERO real movers — the same four noise files (§1.4); both SNAPSHOT holes zero; DROPS with no +/- line** |
| probes vs live perl 5.40.3 | the per-task tables | 61 shapes across nine files — §1.1–§1.3 |
| the `(_)` / `-pi` / `main::STDOUT` / `Foo::H1` / `s/Ｘ/$ｉ/` divergences | — | all PRE-EXISTING on `d0b52e9`; filed #491, #492, #493 (pointer); #476 widened |

## 1. What was independently re-run, and what it found

### 1.1 Q4 — the operand site and the imported term (21 probes)

`sub f ($)` / `g (*)` / `h (;$)` / `l ($;$)` / `u (_)` across 14 call
shapes (`f "a" . "b"`, `f $x + 1`, `"R=", g + 1, "\n"`, `h` alone, `h 3 * 2`,
`l 1, 2`, `f -1`, `f($x) . "|"`, `f 1 == 1 ? … : …`, `f $x x 2`,
`f $x, "tail"`, `(f 1, 2, 3)` in a list, `f "q" x 3 . "z"`, `u "given"`):
**all identical to perl**.  The one DIFF, `print u` with `(_)` and no
argument (perl `u(dflt)`, PCL `u()`), is **#260** — the call-site `$_`
default of the `_` prototype, filed s361, unchanged by #453 (which routed
the PARSE right).  `use Math::Trig` + `pi` in seven positions (bare, `2 *
pi`, `pi + 1`, `pi / 2`, in a list, in a comparison, as a hash key and
`$h{pi}`): identical.  `-pi` prints `0.0000` (perl `-3.1416`) — the
IMPORTED spelling of **#476** (`-NAME` with a declared sub), pre-existing;
#476's description now carries that row.

### 1.2 Q5 — the sub head (12 probes)

Signature parameter shadowing a later file `my` (`f(3), " $x"`), a default
(`g()`/`g(2)`), a slurpy beside a file `@x` of the same bare name
(`k(9)`/`k(9,8,7)`), a default reading an EARLIER parameter (`m2(1)` →
`1,2`): identical.  The same-line pragma in three spellings: `use feature
"signatures"; sub f (…)` ✓, `use v5.36; sub f (…)` ✓, **`use feature
qw(signatures say); sub f ($x, @r) {…}` ✗** — PCL `-u|2 3-2`, perl `0-u|2-2`:
the predicate `_signatures_enabled_at` matched only a QUOTED `'signatures'`
/ `"signatures"`, so the `qw()` spelling (and a `:5.36` bundle) fell back to
the old-prototype lowering #455 fixed for the other spellings.  **Review fix,
§3.**  Non-ASCII interpolation: `"$Ｘ[$ｉ] $Ｘ[$ｉ+1] $ｈ{ｋ} @Ｘ[0,1] $#Ｘ"`
identical; the s/// REPLACEMENT side is not — **#492**, pre-existing and not
#435's mechanism (§1.3).

### 1.3 Q6 — handles, punctuation arrays, glob (28 probes)

Punctuation arrays, one character at a time as Opus did: `"$c[1]"` for `?`
`!` `.` `/` `~` `&` `%` `=` `<` `>` gives 22 in perl and `$^[1]` is a syntax
error — the interp set is confirmed, and so is the reason `^` is not in it.
Glob, 12 shapes: the literal return (`/nope-xyz`, `/home/`, `~nosuchuser`,
scalar-context `glob("/nope-one")`), the word list (`*.c /nope` → three
results, `a.c b.c`), empty / all-blank → nothing, no-match wildcard → nothing,
`<*.c>`: identical; the two DIFFs are exactly **#488** (`{a,b}.c`) and
**#490** (`'x y'` quoting), both filed by Opus.  Handles: `print main::FH5`,
`<FH6>`, `<main::FH2>`, `readline(main::FH3)`: identical — and three
PRE-EXISTING residues of the same family (identical on `d0b52e9`):
`print main::STDOUT "a"` is a CALL (`main::pl-STDOUT` undefined);
`open(Foo::H1, …)` with no `package Foo` anywhere is a CL READ error at
load ("Package Foo does not exist", whole file — the open/close sites emit a
BARE symbol, #452 quoted readline and the print slot); and the registry is
keyed by the SPELLING, so `open(main::FH)` + `print FH` do not meet.
**Filed #491**: one family, one fix — canonicalise the perl handle NAME at
the Environment seam (`main::X` → `X` for every handle; perl forces only the
standard handles into `main::` from other packages, the inverse probe is
`print Foo::STDOUT`).

`s/Ｘ/$ｉ/` writes the literal `$ｉ`, `s/Ｘ/$Ｘ[1]/` the literal `$Ｘ[1]`,
while `s/Ｘ/${ｉ}/`, the ASCII twin and the dq string are right — **#492**,
pre-existing; the replacement text most likely reaches InterpScan as bytes
(its `\w` is Unicode-aware on a character string).  A silent wrong in
zero corpus files; the task carries the probes as its rows.

### 1.4 The companion scan

Run because the batch touched scoping (#454) and both runners (s438).
`--all --quick --jobs 4`: **528 files**; 4 files differed from the snapshot
in the parallel pass and the #366 serial re-run resolved them as the known
four: io/open.t and op/utf8cache.t = contention (serial matches the
snapshot), uni/variables.t = the known unstable TIMEOUT (rows move between
21104/27485/27934 with the same 1248 C_ok), io/pvbm.t = 20/8 in parallel
AND serial vs 23/5 alone — **the EIGHTH time**, not edited (standing since
s437: a fresh_perl-driving file's serial verdict is a signal, not a proof).
`SNAPSHOT: 0 … NO row` and `SNAPSHOT: 0 row(s) for files this --all scan
does not run`; DROPS printed with no `+`/`-` census line.  Exit 1 is the
runner's UNEXPLAINED-count verdict, as always.

## 2. The twelve asks, ruled

**Ask 1 — where the child-drops measurement is BLESSED.**  (c), with a
narrower gate than the question implies: its own file,
`docs/child-drop-sites-sNNN.tsv`, one row per SITE (`file:line`, files
reached, text), and `sweep-diff.pl` gates the SITE SET only — a site not in
the file fails the run like a NEW failure; a site that disappears is printed
as a LOST-style line and leaves by edit; the files-reached column is
reported, never gated (it moves with which files abort, which is the other
buckets' business).  Not (a): a header paragraph cannot be compared
mechanically, and the s437 ruling was "gate after one blessed run".  Not
(b): the census TSV's unit is a transpile-time drop per FILE and its TOTAL
line is summed from column 1 by the recipe in plan-post-s430 §3 — rows in
another unit inside it make that arithmetic lie.  Bless it AFTER #479's
harness half lands (ask 4) so the first blessed set is the honest eight
sites, not ten.

**Ask 2 — `tools/run-perl-suite.pl` and `PCL_DROP_LOG`.**  Yes, same shape
(set it around the run, report per file + the distinct sites, no gate) — the
companion has a population of child programs the sweep does not (fresh_perl
under `tools/pclperl-for-tests`), and it is the population where
`runperl_and_capture` actually has CALLERS (`t/run/runenv*.t`; none in
`perl-tests/`).  Do it in the SAME session as ask 4's harness fix: the
harness row of the WHAT-TO-RUN table already requires `--all --quick` for
both populations, so one run measures both.  Its sites join the ask-1 file
under a `companion:` prefix, same gate.

**Ask 3 — #478's skip exists for compile time.**  MEASURE, and let the
measurement decide — but the default is that the name list goes, because it
is exactly CLAUDE.md 9a's hard-stop smell (two literal module-name lists in
`Pl/Parser.pm`, `Test2::`/`Test::` and the core list above it) and it is
now the cause of 79 of 83 census drops in a population the project ships
for.  The measurement: `time ./pl2cl` on a Test2::V0-using `.t` with the
skip on and off (the walk is memoised per module AND the sweep runs under
`pl2cl --server`, so the cost is per process, not per file — say which), and
`tools/corpus-diff.pl`'s time line.  If the cost is real, the MECHANISM is a
budget on the recursive walk (depth or module count, announced when it
fires), never a name.  Registering 79 rows against a name list is not on the
table.  Test::More's shim must keep winning in @INC for its scalar-forcing
prototypes — that is a separate fact and stays.

**Ask 4 — #479's harness half.**  Harness FIRST and ALONE, then the compiler
gap as its own filler.  The harness line is wrong in perl (probed here too:
`length 0` for a file that has content; perl's own `t/test.pl` opens the
handle in an `if`), it has no callers in `perl-tests/` and four in the
companion, and fixing it (declare `$f` before the ternary, or perl's own
`if (open …)` shape) removes 196 of the 241 child drops — after which the
compiler gap `COND ? <$f> // "" : ""` is measured by its census row, not by
98 files of noise.  Bar = the harness row of the WHAT-TO-RUN table (full
sweep — expect TOTAL +0, CHILD DROPS 241 → ~45, sites 10 → 8; companion
`--all --quick` with the run/runenv*.t rows read individually).  The
compiler half then needs only a `Pl/t` row (the shape occurs in no
population once the harness is fixed — s371 rule).

**Ask 5 — the over-import trade in #365.**  Accept AS IS; reject the
cross-check.  The export scan is the thing #365 just proved unreliable
(Math::Complex's `@EXPORT` is NON-empty and does not list `pi` to a literal
scan), so "cross-check when it HAS one" would re-break the measured case.
Keying on the prototype's SHAPE is the same layering rule the slot imports
follow; the divergence it buys (a non-strict bareword colliding with a used
module's non-exported `()` sub) is a divergence perl itself warns about,
and it moved nothing in 951 files.  Noted in DECIDED as the standing trade.

**Ask 6 — #484's ordering.**  Measure shape (a): `_premerge_include_prototypes`
BEFORE the repair block, repairs, then a SECOND pre-merge after them
(memoised by module name, so the second pass should be free — measure it
with corpus-diff's time line), and `_word_is_declared_term` asks
`Pl::Environment::proto_is_zero_arg` on `get_prototype`.  Not (b): a lazy
per-name path in `_word_is_term` duplicates a walk the pre-merge owns (rule
11) and would become the second source the s438 sessions kept finding.
The seam risk is real and is the measurement: a repair that `_reparse_doc`s
must not leave the environment with records from stale elements — the
records are NAME-keyed facts, not element references, so they survive a
re-parse; confirm that by reading `_premerge_include_prototypes` before
moving it.  Bar as the task says + the four-population A/B.

**Ask 7 — two probe-found pre-existing shapes.**  File them now, as ONE
task, not two, and it belongs with #266, not beside it: both are the
bareword-operand reading of a prototyped sub's argument (`fh FOO` → unbound
symbol for `(*)`; `one BAR` → a call for `($)`; `close G ? … : …` read as
`close(G ? …)`) — "what does a bare WORD mean in operand position" is
#266's question at three more sites.  #481/#482 point the same way.  Do not
patch the `(*)` site alone: an unbound-symbol crash is at least loud, and a
per-site string answer would be the fourth copy.

**Ask 8 — #486, the illegal prototype with names.**  Out of scope for the
compiler (principle 9: PCL compiles valid Perl; `sub t000 ($a)` where the
feature is off is a prototype perl warns about and ignores), BUT the
baseline row is not "wrong input" — it is a perl test asserting perl's
documented recovery, and `_signatures_enabled_at` is now one call away.  So:
a FILLER, cheap, when a session is already in that code — bind the names
only when `_signatures_enabled_at` (or PPI's Structure::Signature) says
signature; otherwise the prototype is a prototype and the body's `$a` is
the package variable.  Leaves `fail-baseline` by edit.  Not a queue item on
its own.

**Ask 9 — #485, a signature default reading an outer lexical.**  PROMOTE,
do not refuse — the machinery exists (file-lexical promotion is what #454
just walked), a default is an expression evaluated in the sub (Opus's own
subtlety in #454) and perl's answer is unambiguous.  The refusal would be
a third scope predicate for the same record (`_signature_param_canons`
already splits names from defaults — a default's free names are the capture
set, and it is ONE more consumer of that split, not a new walk).  Filler
priority; zero population occurrences; guard rows.

**Ask 10 — "no corpus can guard it", five changes running.**  YES, name it:
a small deliberately-awkward corpus — `docs/shapes/` or `Pl/t/shapes/*.pl`,
one file per family (operand grammar, sub heads, handles, interpolation,
glob) — that `tools/corpus-diff.pl` and `tools/emission-ab.pl` accept as a
population, so that a change which is emission-identical over the four real
populations is still A/B'd over the shapes that exist only to exercise the
grammar.  It does NOT replace the rows (rows assert perl's answer; an A/B
only asserts "unchanged"), and it must not grow into a second test suite:
the rule for admitting a file is "a shape that moved a fix in some session
and occurs in no population" — the s438 guard files are the seed.  Task for
the Q7 filler list, Opus, one session; name it in plan-post-s433 §1.

**Ask 11 — #489, the glob iterator keyed by pattern, perl by call site.**
Accepted divergence for v0.1, NOT forever: the shape that matters,
`while (my $f = glob("*.c"))`, works; the two-call-sites-same-pattern shape
is one perl test row.  The fix shape Opus names (`:site N` from a
compile-time counter, every glob emitter) is right when it comes, and it is
an emission change with the full bar — queue it behind the release with a
`not-supported.md` entry naming #489 so the op/glob.t row is explained.

**Ask 12 — "a fix that makes values real exposes an accidental pass".**
Yes, a runbook note — in `docs/test-debugging-runbook.md` under the
sweep-TOTAL/LOST section, one paragraph: when a fix turns an EMPTY/undef
value into a real one, the rows that move are not only the ones that
compare against perl's value — grep the populations for rows comparing TWO
results of the changed builtin (`eq`/`is` of two calls, `ok($a eq $b)`),
because both sides were empty and the row was passing on nothing.  That is
what the baselines are FOR, but the baselines only say "moved"; the grep
says "and here is why", before the run.  Written this session (§3).

## 3. What this session changed

1. **Review fix — #455's predicate recognises every enabling spelling.**
   `_signatures_enabled_at` matched `'signatures'` / `"signatures"` only;
   `use feature qw(signatures say); sub f ($x, @r) {…}` on one line still
   took the old-prototype lowering (PCL `-u|2 3-2`, perl `0-u|2-2`).  Now: a
   `use feature`/`use experimental` statement naming `signatures` as a word
   in ANY quoting, or a `:5.NN` bundle with NN ≥ 36, or `use v5.36`+; a `no
   feature …` statement is never an enabling site.  Guard row in
   `Pl/t/sig-param-shadow-01.t` (8 → 9); corpus-diff identical over 111
   (the same-line spelling occurs in no population — rows are the bar); the
   gate.  Same commit: the `$a` that a heredoc had eaten out of that file's
   #486 comment is restored.
2. **Filed #491** (qualified handle NAME canonicalisation — the three
   residual spellings), **#492** (s/// replacement side vs non-ASCII
   identifiers), **#493** (pointer record: `-pi` → #476, `(_)` → #260,
   #484 → shape (a)); **#476** widened with the imported-sub row; **#277**'s
   record corrected to COMPLETED (the installer shipped s405c; the task had
   stayed `pending`).
3. Runbook §4b for ask 12; the #489 accepted-divergence entry in
   `docs/not-supported.md` (ask 11); DECIDED §s439; session log; plan
   `docs/plan-post-s433.md` §1 next-step line; memory STATE line.

## 4. Where the project is, and what is left until the first release

Measured on this tree (`e0f9116` + the review fix): gate **166 / 5780**
(only the 13 pclxs xs rows, user-deferred), sweep **18312 passing / 894
failing = 95.3 %, 62 of 108 files fully passing**, GATE clean, drop census
**81 files / 185 drops** over six populations (the flip is IN: a drop dies
when reached, trappable), companion **528 files** with both snapshot holes
at zero, generation **v2-181**.  The Q1–Q6 queue of plan-post-s433 is
DONE; Q7 (fillers) is open and is ordinary bug work, none of it a release
precondition.

**The release is v0.1, and its preconditions were fixed in DECIDED s425:
the tag comes after the first GREEN CI run.**  What that means concretely,
in order, with who does it:

| # | step | owner | state |
|---|---|---|---|
| 1 | **The push** — force-push `main` to `github.com/Percolisp/pcl` (origin is 69 commits of the OLD history; local main is 1261 commits of the rewritten one), keep `snapshot-2026-05` | **USER** (scheduled: week of 2026-08-24) | not done |
| 2 | **First CI run** — `.github/workflows/ci.yml` (#283, authored s419c) runs the installer, `tools/t/install-pcl.t`, `tools/prove-core`, corpus-diff vs the PR base; expected image breakage: the SBCL tarball's sourceforge redirect, apt package names.  Fix what it finds. | Opus (one session, reactive) | waits for 1 |
| 3 | **#282 = that green run** — the fresh-machine install test's container half (the sanitized-fresh-HOME rehearsal already ran green, s419) | — | waits for 2 |
| 4 | **Refresh the release docs ON THE TREE THAT IS TAGGED** — README/STATUS/CHANGELOG say 155/5600 and "a dropped statement is loud but not fatal"; both are stale (gate 166/5779; since s435 a drop DIES when reached).  Re-measure once, write the numbers, and make the drop paragraph say the flip.  Filed as **#494**. | Opus, ≤ 1 hour | not done |
| 5 | **Tag v0.1.0**; CHANGELOG's "unreleased" becomes the date | USER (or Opus on instruction) | waits for 2–4 |

That is the whole list.  Everything else the queue carries — Q7 fillers,
#281's items 4/5 (the IR design half: control chars in literals, `p-cond`),
#221 (warnings model), boxed aggregates, E5.3 `local`, #489, the #399
scalar-invocant spelling — is **post-v0.1 by ruling** (s425/s433: tag
decoupled from the flip; the flip has since landed anyway).  **#281** is
"finish with the tag" in the Fable queue; my recommendation is to call
items 1+2+6 + the ir-spec update (merged s415) the v0.1 IR pass and move
items 4/5 to the post-v0.1 list explicitly — they change emission shape and
each needs the bench, which is not release work.

Two things I would NOT do before the tag: run the cpan board re-bless
(nothing in the release depends on its labels), and start any Q7 filler
that widens a checker (the gate-SET scan row) in the same week as the push
— keep the tagged tree boring.

## 5. Queue after this review

* **Opus next**: Q7 in the ruled order — #463 item 2 first (18 rows behind
  one drop), then #464 → #466 → #465, #468, #470; then the s439 fillers in
  the order they pay: #479 harness half + ask 2 (one session, one `--all
  --quick`), #478 measured (ask 3), #491, #485 (promote), #492, #484 shape
  (a), ask 7's task, ask 10's shapes corpus; #494 at tag time.
* **Fable**: rule the asks as they come; #281's decision above; post-v0.1
  the boxed-aggregates design.
* **USER**: the push (step 1).  Nothing else is waiting on you.
