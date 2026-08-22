# Plan after s430 (Fable, s430, 2026-08-22) — THE LIVE QUEUE for the Opus sessions that follow B3

*Supersedes `docs/plan-post-s420.md` §1 as the Opus queue (its §4 USER
decisions and §5 guardrails still stand).  Written at the end of s430, the
session that closed B3 (task #153) — the compiler's structural track is DONE;
what is left before v0.1 is fillers, the release gate, and one measurement
that feeds a Fable design (the announce→DIE flip).  Read CLAUDE.md's lookup
order first (DECIDED.md → not-supported.md → runbook → probe), then this.*

## 0. Where the project is (measured s430)

* main `7d5c67b`, generation **v2-177**; cold gate **160 files / 5700 rows**
  (the only failures are the 13 pclxs xs rows — `~/pclxs` is at abi 8, the pin
  says 6; user-deferred, ignore); full sweep **TOTAL 18367, GATE clean**
  (s428 — not re-run since: every later commit was corpus-diff IDENTICAL +
  lib byte-identical, the s401 WHAT-TO-RUN row); drop census **27 files /
  82 drops** (`docs/parse-error-drop-census-s399.tsv` — the census IS the
  baseline; a row leaves by EDIT with its cause).
* **ONE compiler, ONE pipeline, B3 COMPLETE.**  B1 (#372) s417, B2 (#343)
  s418, B3.1 (#411) s428, B3.2 (#259) s429, B3.3 (#374(b)) s430 — none of the
  three B3 widenings needed a `_term_extent` change (each cause sat in a
  pre-pass, a prototype reading, or the lexical-sub renamer);
  `docs/b3-operand-collapse-s428.md` has the record.  The `$end_pars` region
  of PExpr is STILL the maze `docs/pexpr-term-parsing-review.md` describes —
  do not add guards there; the §1a reachability re-run
  (`tools/term-diff-sweep.pl --env PCL_TERM_DECL=1`, BOTH populations — corpus
  9, perl-t 72 declines, all Word/Operator/Cast-led) is the regression check
  for anyone who touches the walker.
* Release: the push week is **2026-08-24 (USER executes: force-push main,
  keep `snapshot-2026-05`)** → first CI run (`.github/workflows/ci.yml`, the
  #282 container half) → the **v0.1 tag, DECOUPLED from the flip** (DECIDED
  s425: tag after the first green CI run).  The flip becomes v0.2's headline.
* What the census says now (27/82): **39 lvalue-sub rows** (sub_lval.t 33,
  substr.t ×2, signatures.t, try.t — permanently EXEMPT, ruled
  fable-answers-s400 §6.3); **~14 registered / deliberate** (regex code blocks
  + `(*SKIP)` in re/, `print 1+` in comp/parser.t, format in write.t, hex
  float, the 4-arg-substr comma); **indirect object 4** (ref.t ×2, method.t ×2
  — #399 MAYBE LATER, USER s425; #381 is the crash twin); **family-4 glob /
  symbolic-ref surgery ~12** (`$${$_[0]}`, `*{;undef}=3`, `*X=*-`, `++${"…"}`
  — by-design walker declines + PPI's `$$` mislex, gv.t / uni/ / lex.t /
  utf8cache.t / open.t); **the `x`-named lexical sub 4** (lexsub.t — PPI lexes
  a sub named `x` as the repetition operator, #361/#376 family); **non-ASCII
  names ~6** (uni/method.t, uni/readline.t, uni/gv.t — family 4 spellings of
  #410's family, #410 itself is DONE).  That is the whole 82.

## 1. Opus queue, in order

Cross-cutting (unchanged): the WHAT-TO-RUN table in CLAUDE.md decides what
runs; every probe-found silent-wrong is FILED with its reproducer; a
review-request doc per session (`docs/opus5-review-requests-sNNN.md`) —
write one even if it says "no asks"; **grep `Pl/t` for the message text of
ANY behaviour a fix removes** (the s416 stale-guard rule — s430 hit it again:
lexical-sub-01.t's #374 rows asserted the very DROP B3.3 removed; the
targeted-guard batch is what catches it, so run the batch); a filler is
"same mechanism + gate-SET measured + new axes filed" (s366).

**Session P1 — the flip re-census, measured and classified (feeds the Fable
design; NO flip code).**

> **DONE s431 (Opus 5) — `docs/drop-census-s431-flip-gate.md`; asks in
> `docs/opus5-review-requests-s431.md`.**  Census identical (27/82) on a cold
> cache; all 82 classified (exempt 39 / registered 7 / deliberate 1 /
> needs-a-ruling 4 / gap 31, every gap owned).  **Priced**: flip everything
> 5300 rows over 27 files, exempt lvalue+indirect object 3022/19, also exempt
> the registered absences 441/15, fix the 31 gaps first 0 — the decisive
> number being that four REGISTERED absences sit in files worth 2581 rows.
> Item 2's module count is NOT zero: 3 board modules (5 drops, one of them
> Text::Balanced with 780 passing rows) + 9 cpan-tests modules (15), so the
> module-mode increment goes behind its own unblock list.  Filed #457–#466 (three of them the minimised module drops);
> #415 updated.  Fable's design question is Ask 1 of the review doc.

1. Run `tools/drop-census.pl "$PWD" <out.tsv> 8` (both populations,
   `PCL_PERL_SUITE_T` set) on a COLD cache and diff against the blessed
   census — expect identity (27/82).  Then re-classify every row into the
   s419 families (`docs/drop-census-s419-flip-gate.md` §2) and write the
   table into a short `docs/drop-census-s431-flip-gate.md`: per family the
   count, the owner, and the one-word verdict **exempt / registered /
   gap-with-task / needs-a-ruling**.  The §0 paragraph above is the expected
   shape; the point is the ROW-LEVEL table, because the flip's ruled bar is
   "every remaining drop is a deliberate error test, a registered absence, or
   an odd single explained" (option-b-phase2-plan §3).  Bring the one open
   question as a review ask: **what does a flip do with the 39 lvalue rows
   and the 4 indirect-object rows that are deliberately NOT fixed** — a
   perl-shaped REFUSAL (trappable, not-supported entry) at the drop site vs
   keep them as announced drops while everything else DIES.  That is Fable's
   design (§2.1); Opus measures the file cost of each option (which files
   TRANSPILE-FAIL, how many passing rows each contributes today —
   `docs/perl-suite-run.tsv` + the sweep's `_status.tsv` have the numbers).
2. The module-mode increment (flip-gate §5): re-run the cpan board with the
   census instrument on (a module-mode DIE aborts the program at the first
   cold-cache transpile of a dropping module) and bring the count of board
   modules carrying drops.  If zero, or only modules already FAIL, it is the
   first measured single-mechanism flip increment — still a Fable call to
   land.

**Session P2 — #456 (silent empty from a `p-declare-sub` stub) — rule 12.**
3. `{ package Q; print main::nm(), "|\n"; } sub nm {"PKG"}` prints EMPTY.
   Two halves, same session: (a) the forward-declaration stub a
   `p-declare-sub` installs must DIE naming the sub when called before its
   definition (rule 12: a missing case announces itself — today it ANSWERS
   undef); (b) the hoist order: why does a file-level named sub land AFTER a
   preceding block that carries a `package Q;` switch (the block is emitted
   as its own `;;; package Q` section) — perl compiles every named sub
   before run time, so PCL's hoist should put it before the first section.
   Bar: the reproducer + the no-package-switch inverse vs perl; corpus-diff;
   if (b) changes the hoist order the emission moves everywhere → full
   sweep + the four-population A/B, every diff explained per file.  (a)
   alone is runtime + guard row.  Do (a) first — it turns the silent wrong
   into a loud one even if (b) slips.

**Session P3 — the two named-unary operand sites become ONE (#453) + #365.**
4. **#453**: `sub f($) …; f "a" . "b"` is `f(a)b` (perl `f(ab)`); `(*)`'s
   `g + 1, "\n"` swallows the comma.  `Pl/PExpr.pm handle_subcalls` has TWO
   operand sites — the builtin named-unary site applies `_extend_high_prec`,
   the user-prototype site does not; the task names both and the sibling copy
   at the `$prev_is_named_unary` check (rule 11: one helper).  Acceptance:
   the s429 16×10 matrix (session-log s429 has the table; regenerate it) —
   every changed cell toward perl; the four-population A/B; gate-SET scan over
   both populations (PExpr term reading = the s372 rule).
5. **#365**: an IMPORTED `()`-prototype sub (`use Math::Trig; 2 * pi`) is a
   bareword STRING in operator/list positions — the operator-loop Word reading
   does not ask the environment's prototype table (`_is_zero_arg_func` does,
   at the single-element site).  B3 did not absorb it (B3 never touched the
   walker); it is the #266 classifier asked at one more site.  Same session
   as #453 if it is the same site, else next.  Bar: the task's probe table vs
   perl + the #356 shapes; corpus-diff; gate-SET rule.

**Session P4 — the scoping / naming fillers.**
6. **#454** — a signature parameter `$x` + a LATER file-level `my $x` →
   REFUSAL "file lexical 'x' captured by sub f".  A signature param is the
   sub's own `my`; the capture detector must treat it as the same shadow a
   body `my $x` is.  Scoping change → the full sweep IS the gate (CLAUDE.md
   table) + gate-SET scan both populations.  Guard row.
7. **#435** — `"$Ｘ[$ｉ]"` / `"$Ｈ{$ｋ}"` inside a dq string read element 0 /
   "" (the fragment re-parse skips the #410 `$Ｘ` Cast+Word repair).  The
   repair runs on the fragment mini-parse too, ONE helper.  Acceptance in the
   task (four rows vs perl, `Pl/t/utf8-source-01.t`, uni/+mro/ emission A/B
   with every mover explained, ASCII byte-identical everywhere).
8. **#455** — `use feature "signatures"; sub f ($x, @r) {…}` on ONE LINE
   warns "uninitialized value in join" for an empty `"@r"`: the feature
   region looks line-based.  The 2-minute discriminator is in the task
   (compare the q5/q3 emissions).  Low value; do it if P4 has room.

**Session P5 — the punctuation / readline / glob fillers (s427 residue).**
9. **#451** (`"$?[1]"` interpolation twin of the s427 `@?` fix — InterpScan's
   `$` arm; ONE `%PUNCT_ARRAY_CHARS` source), **#452** (`<main::FH2>` emits a
   bare symbol — unbound at load; `readline(main::FH3)` already right),
   **#449** (the CL-unsafe punctuation arrays `@,` `@;` … — loud drops today,
   ZERO corpus rows; decide with a not-supported entry or a pipe-quoted
   spelling, not silently), **#450** (`glob("/nope")` returns the pattern in
   perl — a `cl/` change → the full sweep is NOT optional).  Guards in
   `Pl/t/punct-array-glob-01.t`.

**Then the release gate** — push (USER, 2026-08-24) → first CI run (#282's
container half; fix whatever the runner finds, the installer test
`tools/t/install-pcl.t` is the local rehearsal) → the v0.1 tag.  #359 stays
behind the release; #221 (warnings model) first post-release; #409 (server
RSS) and #326 (the hang set, ~7500 rows) are infrastructure items for a
session with nothing rows-shaped.

## 2. Fable queue

1. **The flip design** — from P1's table: the refusal-vs-die shape for the
   exempt families, the order of increments (module-mode first if the board
   count allows), and the bar (s373 three legs + sweep TOTAL/LOST + the
   per-file row cost P1 brings).  Then land it, or hand a ruled recipe to Opus.
2. Rule the asks as they come (P1–P5 each end with a review-request doc).
3. **#281** (v0.1 IR pass, in_progress) — finish with the tag.
4. Post-v0.1: boxed aggregates (design — do not start before), #221, E5.3
   `local`, the #399 scalar-invocant spelling if the USER re-raises it.

## 3. Recipes (copy, do not re-derive)

* **Four-population emission A/B** (the B3 bar; seconds to minutes):
  `tools/corpus-diff.pl` (working tree vs HEAD, 111 files — read its SILENT-
  DROP line); `tools/emission-ab.pl --ref <base> --list L` with L = `find lib
  -name '*.pm'` (22), `find cpan-tests/modules -type f \( -name '*.t' -o
  -name '*.pm' \) | grep -v tar.gz` (402), `ls $PCL_PERL_SUITE_T/*/*.t`
  (604).  Every DIFF explained per file, or the change is not done.
* **Gate-SET scan** (mandatory when a checker / refusal / decline WIDENS;
  cheap enough to run for any PExpr/Parser2 change): `git worktree add
  <scratch>/wt-base <sha>`; `tools/gate-set-scan.pl <scratch>/wt-base
  before.tsv 8`; `tools/gate-set-scan.pl "$PWD" after.tsv 8`; `diff` — 638
  rows each; set `PCLXS_DIR=~/pclxs` when you run anything xs-shaped in the
  worktree.
* **A companion row**: `tools/run-perl-suite.pl op/foo.t` (parallel + serial
  confirm; it prints "REAL MOVE" or not) → splice the row into
  `docs/perl-suite-run.tsv` with a `# sNNN` cause comment ABOVE it (see the
  s428/s430 blocks there).  A mover is "pre-existing" only with its CAUSE
  (s421).
* **The census**: `tools/drop-census.pl`; edit the row AND the TOTAL line
  AND add a `#   sNNN` note in the header (the s429/s430 notes are the
  template); `perl -ne 'next if /^#/; my @f=split /\t/; $s+=$f[1];
  END{print $s}'` must equal the TOTAL.
* **Generation**: any emission change bumps `*pcl-cache-generation*`
  (`cl/pcl-runtime.lisp`) and regenerates the THREE artifacts —
  `tools/rebuild-pack`; `./pl2cl --extension lib/mro.pm > cl/pcl-mro.lisp;
  tools/tag-license cl/pcl-mro.lisp`; same for `lib/warnings.pm` →
  `cl/pcl-warnings.lisp` — then `git diff cl/` must be stamp-only unless the
  change really reaches them (`Pl/t/artifact-staleness-01.t` enforces the
  stamp).
* **Long measurements**: give each its OWN background invocation with a long
  timeout.  A Bash call that times out kills every background child it
  started (s430 lost the perl-t reachability sweep that way and had to re-run
  it) — never chain a 20-minute job behind a foreground command.
* **Probe discipline**: ~10 shapes vs `perl` (5.40.3), INCLUDING the inverse
  the change could break (feedback rule), before the first A/B.  Write the
  probe as a `Pl/t` row when it passes; the `test_lexsub`-style "same source
  through perl and PCL" row is the cheapest honest form.

## 4. USER decisions open

None new.  Standing: the push week (2026-08-24, USER executes); the tag
after the first green CI run (DECIDED s425); indirect object MAYBE LATER
(USER s425).
