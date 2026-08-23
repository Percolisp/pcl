# Plan after s433 (Fable, s433, 2026-08-22) — THE LIVE QUEUE after the s431/s432 review

*Supersedes `docs/plan-post-s430.md` §1 as the Opus queue (its §2 Fable
queue, §3 recipes and §4 USER decisions still stand — copy recipes from
there, do not re-derive).  Written at the end of s433, the session that
ruled the s431 flip re-census and the s432 #456 half (a)
(`docs/fable-answers-s433.md`).  Read CLAUDE.md's lookup order first
(DECIDED.md → not-supported.md → runbook → probe), then this.*

## 0. Where the project is (measured s433)

* main `a5e1609`, generation **v2-177**; cold gate **160 files / 5705 rows**
  (only the 13 pclxs xs rows fail — `~/pclxs` is at abi 8, the pin says 6;
  user-deferred, ignore); full sweep **TOTAL 18366, GATE clean, drops 5 =
  census**; drop census **27 files / 82 drops**, all 82 classified and PRICED
  (`docs/drop-census-s431-flip-gate.md`): exempt 39 / registered 7 /
  deliberate 1 / indirect object 4 / gap 31, every gap owned.
* **The flip has a SHAPE now (RULED s433, fable-answers-s433 §A.1): a
  perl-shaped, trappable RUN-TIME die at the drop site, one shape for every
  drop in every mode, no classifier.**  Its unit is the statement, not the
  file — so the 5300/3022/441-row prices of the transpile shape do not
  apply, the module-mode increment is dissolved (a module with a drop still
  loads), and the flip no longer waits for the 31 gaps.  It waits only for
  the two instruments (#467, #462) that let its price be measured.
* **#456 half (b) is RULED as perl's PHASE model across sections** (§A.4 of
  the answers doc; #469 is the general shape): compile-phase forms of every
  section before run-phase forms of any section.  The "hoist one
  definition" sketch is unsafe (symbol-macro cells).
* Release: the push week is **2026-08-24 (USER executes: force-push main,
  keep `snapshot-2026-05`)** → first CI run (`.github/workflows/ci.yml`, the
  #282 container half) → the **v0.1 tag, DECOUPLED from the flip** (DECIDED
  s425: tag after the first green CI run).  The flip is v0.2's headline.
  Nothing in Q1–Q3 is a precondition of the push or the tag.

## 1. Opus queue, in order

**s437 (Fable review of s434–s436, `docs/fable-answers-s437.md`): Q1, Q2 and Q3 are DONE and APPROVED.  The next Opus session OPENS with the census instruments — #473 (cpan `.t` population, program mode), #472 (`PCL_DROP_LOG` side channel + the sweep's `child-drops` line, measure first) and s434 ask 1 (the five never-refreshed rows → NOT-RUN rows or joined scan, by measurement) — then Q4 below.  Q7 is re-ordered: the PROMOTED #463 item 2 (`++${"23::foo"}`, 18 op/universal.t rows behind one drop) first, then #464 → #466 → #465; new fillers #475, #476, #477 and the prove-core MemoryMax scope (s436 ask 1, IN) join the Q7 list.**

**s438 (Opus) DID the census instruments: #473 (a sixth census population — `cpan-tests/modules/**/t/**/*.t`, 42 files / 83 drops, blessed), #472 (`PCL_DROP_LOG` — the seventh population measured for the first time: 241 drops in 98 files, TEN sites) and s434 ask 1 (the companion `--all` scan is 528 files, not 523; both SNAPSHOT holes now read zero).  No product change; six findings filed (#478–#483).  Record: DECIDED §s438, `docs/opus5-review-requests-s438.md`.  THE NEXT SESSION IS Q4 = #453 + #365 (P3 of plan-post-s430: the two named-unary operand sites become one; the `()`-prototype bareword).**

**s438b + s438c (Opus) DID Q4: #453 (the two named-unary operand sites become ONE — `is_named_unary` answers for a declared sub whose prototype is perl's named-unary class) and #365 (an imported `()`-prototype sub is a TERM — the prototype crosses the `use` on its own shape, not on the literal-`qw()` export scan).  Both emission-IDENTICAL over the four populations (951 files A/B, 0 DIFF, twice), so the guards ARE the bar: `Pl/t/user-unary-01.t` + `Pl/t/imported-term-01.t`, inverse-guarded.  Gate 165/5760; sweep TOTAL 18312 (+0) GATE clean; companion 528 files, ZERO real movers.  #484 filed (the #351 repair cannot see an IMPORTED term — the pre-merge runs after the repairs).  Record: DECIDED §s438b+c.  **NEXT IS Q5 (P4: #454, #435, #455)**, then Q6, then the Q7 fillers.**

**s438d + s438e + s438f (Opus) DID Q5 (P4): #454 (a signature PARAMETER shadows a same-named file lexical — the detector AND the rewriter both ask one helper), #455 (the signatures feature is in force on the pragma's OWN line; the repair is the BOUNDARY, not the text — corpus-diff proved a textual rule wrong against signatures.t), #435 (ONE `Pl::Parser::fragment_doc`, nine fragment re-parse sites routed through it).  All three emission-IDENTICAL over the four populations (951-file A/B each), so the guards are the bar; #454 also ran the gate-SET scan over both populations, IDENTICAL.  Gate 166/5771; sweep TOTAL 18312 (+0) GATE clean.  #485 + #486 filed (both pre-existing).  Record: DECIDED §s438d+e+f.  **NEXT IS Q6 (P5: #451 / #452 / #449 / #450)**, then the Q7 fillers.**

**s438g + s438h + s438i (Opus) DID three of Q6 (P5): #452 (a package-qualified bareword filehandle is a NAME — one predicate, two sites, both a load-time crash), #451 (a punctuation ARRAY element interpolates; the set is NOT the token repair's — `^` differs), #450 (a metacharacter-free glob pattern is ITSELF, and a pattern is perl's whitespace-separated LIST).  **#449 stays with #418 by its own ruling** — the CL-unsafe punctuation arrays need a pipe-quoted emission that does not exist yet, and keep dropping loudly.  Gate 166/5779; sweep TOTAL 18312 (+0) GATE clean.  Filed #487–#490.  Record: DECIDED §s438g+h+i.  **NEXT IS Q7 — the module fillers, #463 item 2 FIRST (18 op/universal.t rows behind one drop), then #464 → #466 → #465, #468, #470, and the new fillers #475–#490.**

Cross-cutting (unchanged): the WHAT-TO-RUN table in CLAUDE.md decides what
runs; every probe-found silent-wrong is FILED with its reproducer; a
review-request doc per session (`docs/opus5-review-requests-sNNN.md`) — write
one even if it says "no asks"; grep `Pl/t` for the message text of ANY
behaviour a fix removes (the s416 stale-guard rule); a filler is "same
mechanism + gate-SET measured + new axes filed" (s366); **a companion row
count is not comparable to a sweep row count for a change that makes
something die, until Q1 lands** (s433).

**Session Q1 — the two instruments: #467 + #462 (+ the missing-row report).  No product change.**  **DONE s434** (`docs/opus5-review-requests-s434.md`, DECIDED §s434): both runners load with recovery and the snapshot is re-blessed (+2325 C_ok, 0 rows lost); the census has five populations and the blessed file gained the 12 module rows (39 files / 102 drops); the snapshot hole is printed from BOTH sides.  **Q2 is unblocked.**

1. **#467**: `tools/run-perl-suite.pl` loads the emitted CL with
   `pcl::p-load-with-recovery` like the sweep does (one-line change at the
   sbcl command, ~line 601; the runner already loads `cl/pcl-test.lisp`,
   where the function lives).  Then RE-BLESS `docs/perl-suite-run.tsv` in
   ONE measured pass (`--all`, not `--quick`, once — the files that died
   mid-way are exactly the ones that change) and land the re-bless in the
   SAME commit, every moved file explained in the header block (expected:
   files that died mid-way report MORE rows; a file that reports FEWER rows
   is a finding, not a re-bless).  Write the rule into
   `docs/test-infrastructure.md`: both runners load with recovery; a
   form-abort is counted and printed; users (runpcl) are plain load.
   `PCL_SHOW_SBCL=1` before/after diff of the spawned command (the runner
   row of the WHAT-TO-RUN table).
2. **The missing-row report** (s431 Ask 4): the runner prints, at the end of
   every run, the suite files that have NO snapshot row (count + names) as a
   named discrepancy.  The five s431 rows were found by counting; the next
   ones must not need counting.
3. **#462**: `tools/drop-census.pl` gains the `cpan-tests/modules`
   population (each `.pm` transpiled with `--module`, so the emission is the
   one the runtime caches) and the 14-dist board's `lib/` behind a flag
   (`~/.cpan/build` is outside the repo); prints the module population
   separately; the blessed census gains those rows with their causes (the
   s431 doc §5 table is the measurement: 3 board modules / 5 drops, 9
   cpan-tests modules / 15).  `tools/corpus-diff.pl`'s SILENT-DROP counter
   stays perl-tests-only (per-change instrument); the census is the
   baseline.
   Bar for Q1: gate; the runner's own before/after on a handful of files
   incl. one that dies mid-way (op/method.t, op/sort.t are the s432 cases);
   the census tool's row total equals the blessed TOTAL line.

**Session Q2 — THE FLIP, in the ruled shape (fable-answers-s433 §A.1).**

4. In `Pl/Parser.pm`'s two `PARSE ERROR` emitters
   (`_parse_expression_internal`, `_parse_expression_form`) the comment stays
   byte-identical and the `nil` becomes
   `(pcl:p-die "PCL: statement not supported at FILE line N: <text> -- <reason>\n")`.
   ONE helper builds the message for both the stderr announcement and the
   emitted die (factor it out of `_announce_dropped_statement`; rule 11);
   the source text goes through the CLForm string escaper.  The
   announcement, `#363`'s eval-string die, `--module`'s silence and the
   Track A `_ruled_refusal_for_drop` transpile refusals all stay as they
   are.  `not-supported.md` §Lvalue subroutines: the s400 §6.3 sentence
   becomes "dies when the statement runs (trappable)".  `docs/ir-spec.md`
   load-model: what the drop form means at run time.  Generation bump + the
   three artifacts.  Guard: a `Pl/t` row of the `$@` form on a shape that
   stays a drop (`eval { f() = 7 }; like $@, qr/statement not supported
   at/`), plus the inverse that a file with no drop is byte-identical.
   BAR (all legs, in this order): `corpus-diff` — diffs in exactly the four
   perl-tests census files, one line each, SILENT-DROP count unchanged;
   four-population A/B — diffs = exactly the 27 census files + the 12 module
   files, every one the same one-line shape; full sweep TOTAL/LOST per file
   — a lost row is accepted only as a row AFTER the dying statement in its
   top-level form or a row that ran ON the dropped statement's `nil` (the
   accidental-pass kind), edited into `pass-baseline` with that cause,
   anything else is a finding; companion `--all --quick` on the Q1 runner
   with the A/B attribution recipe (plan-post-s430 §3); the board re-run for
   the three dists with drops (Text::Balanced's rows will move where line
   118/397 is reached — that is #457's price; explain per row); gate; DECIDED
   line.  Then update `docs/parse-error-drop-census-s399.tsv`'s header note:
   the census now counts LOUD-AT-RUN drops.

**Session Q3 — #456 half (b) = #469, the PHASE model across sections (fable-answers-s433 §B.4).**

5. Measure first: what the five section lists hold (`Pl/Parser2.pm`
   ~1770–1830 assembly + the "Cross-section forward sub calls" block
   ~1845); how many files are multi-section; whether any `sched` form is
   ENTITLED to an earlier section's run-time state (perl: no — probe
   `our $x = 5; { package Q; sub q1 {1} } BEGIN { print "B=[$main::x]" }`,
   perl `B=[]`, PCL `B=[5]`); compile time (MOVE, never copy — nothing is
   compiled twice).  Then emit every section's compile-phase forms (decls,
   captured, defs, sched — source order kept) before any section's
   run-phase forms, each group under its own `(in-package …)` switches; the
   on-demand stub block becomes unnecessary for the cases it served (keep
   it only if a def still needs an earlier-section reader — measure).
   NEVER the def alone: a body above its section's decls compiles a
   `p-defcell` symbol-macro as a free variable (probed s433).
   Bar: the #456 reproducer + the BEGIN probe + the inverses
   (single-section BEGIN, same-section call, sub-first, no-package-switch)
   vs perl; four-population A/B with every diff explained (expected class:
   ordering only); full sweep TOTAL/LOST; companion on the Q1 runner; gate;
   generation bump + artifacts; ir-spec load-model line; sort.t's bug-36430
   row returns to `pass-baseline` by EDIT; the two `decl-ordering-02.t`
   guard rows become one `both_agree`; #456 + #469 close together.  If the
   two-phase A/B shows a diff class the session cannot explain, STOP and
   bring the classes as an ask — the fallback is the on-demand hoist of the
   needed section's decls+defs.

**Session Q4 — P3 of plan-post-s430: #453 + #365** (the two named-unary
operand sites become ONE; an imported `()`-prototype sub is a bareword
string in operator positions).  Text and bar unchanged from plan-post-s430
§1 items 4–5 (the s429 16×10 matrix; four-population A/B; gate-SET scan
both populations).

**Session Q5 — P4: #454 (signature param vs a later file-level `my` →
false capture REFUSAL; scoping ⇒ the sweep IS the gate), #435 (the `"$Ｘ[$ｉ]"`
fragment re-parse skips the #410 repair), #455 (low value, if room).**

**Session Q6 — P5: #451 / #452 / #449 / #450** (punctuation-array
interpolation twin; `<main::FH2>` bare symbol; the CL-unsafe punctuation
arrays — decide loudly; `glob("/nope")` returns the pattern — `cl/` ⇒ sweep).

**Session Q7 — the module fillers, by rows behind them: #457 → #464 → #466
→ #465**, then **#468** (the never-declared plain call routes through
`%p-call-of-undefined-sub`: measure where the program's dynamic extent is
established before choosing handler-vs-stub-emission), then **#470** (the
identity-promoted file lexical aliases its package variable; scoping ⇒ the
sweep IS the gate + gate-SET scan), then the former flip-gap list as
ordinary fillers (#458, #460, #461, #463, #415 rows) — after Q2 they are
loud at run time, not blockers.

**The release gate runs independently of Q1–Q3** — push (USER, 2026-08-24)
→ first CI run (#282's container half; fix what the runner finds —
`tools/t/install-pcl.t` is the local rehearsal) → the v0.1 tag.  #359 stays
behind the release; #221 (warnings model) first post-release; #409 (server
RSS) and #326 (the hang set) are infrastructure items for a session with
nothing rows-shaped.

## 2. Fable queue

1. Rule the asks as they come (Q1–Q7 each end with a review-request doc);
   the Q2 flip and the Q3 phase model each get a review session with the
   full bar independently re-run (they are the two emission-wide changes).
2. **#281** (v0.1 IR pass, in_progress) — finish with the tag.
3. Post-v0.1: boxed aggregates (design — do not start before), #221, E5.3
   `local`, the #399 scalar-invocant spelling if the USER re-raises it.

## 3. Recipes — see `docs/plan-post-s430.md` §3 (unchanged).

## 4. USER decisions open

None new.  Standing: the push week (2026-08-24, USER executes); the tag
after the first green CI run (DECIDED s425); indirect object MAYBE LATER
(USER s425).
