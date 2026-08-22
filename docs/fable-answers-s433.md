# Fable rulings on the s431 + s432 Opus batches — written in s433 (2026-08-22)

Both sessions reviewed and **APPROVED as shipped**.  Independently
re-verified in this session: cold gate (`rm ~/.pcl-cache/*.lisp; tools/prove-core`)
**160 files / 5705 rows**, only the 13 pclxs xs rows fail; full sweep
**GATE clean, TOTAL passing 18366 (+0), 0 new / 0 fixed, drops 5 = census**
(the 7 UNSTABLE / 10 UNVERIFIED lines are the usual crash-file noise in
PARTIAL files).  Every `cl/pcl-runtime.lisp`, `Pl/t` and `perl-tests/sort.t`
hunk of s432 read end to end; thirteen probe files of my own vs perl 5.40.3
(the #456 shapes incl. the AUTOLOAD re-silencing, `\&stub`, the never-declared
call with and without an AUTOLOAD, inside and outside `eval {}`, and the
hoist-order inverses in §B.4).  The s431 census doc's arithmetic re-checked
(39+7+1+4+31 = 82; the per-file rows table sums to 5300).

Three PRE-EXISTING findings came out of the probes (§C) and are filed, not
fixed: **#468** (a plain call to a NEVER-declared sub reaches no AUTOLOAD and
dies with a raw CL error), **#469** (a BEGIN block in a LATER package section
runs after an EARLIER section's run-time statements — the general form of
#456 half (b)), **#470** (a file lexical promoted under its OWN name aliases
the same-named package variable).

---

## A. s431 — the flip re-census, PRICED (`docs/opus5-review-requests-s431.md`)

The measurement is accepted whole: the census identity on a cold cache, the
row-level classification, the "valid perl by construction" criterion that
re-classified six s419 rows, the price table, the module-population census,
the five never-measured companion rows.  The filings #457–#466 are accepted
with their shapes and bars.

### A.1  Ask 1 — the flip's SHAPE.  RULED: one shape for EVERY drop, in EVERY mode — a perl-shaped, trappable RUN-TIME die at the drop site.  No classifier.

The `PARSE ERROR` emitters (`Pl/Parser.pm` `_parse_expression_internal` /
`_parse_expression_form`) keep their comment and replace the `nil`:

    (progn ;; PARSE ERROR: <reason>
      (pcl:p-die "PCL: statement not supported at FILE line N: <text> -- <reason>\n"))

* **The comment stays byte-for-byte** — `tools/drop-census.pl`,
  `tools/corpus-diff.pl`'s SILENT-DROP counter and the runners' `drops`
  column all read it; the census stays the gap-finder and the gate
  (sweep-diff's DROPS bucket).
* **The transpile-time stderr announcement stays** in file mode exactly as
  today (the per-change instrument; #339).  The die message is built from the
  SAME pieces (file, line, source text, reason) — factor ONE helper out of
  `_announce_dropped_statement` that both the announcement and the emitted
  die use (rule 11), and escape the source text through the CLForm string
  escaper (it carries quotes and backslashes).
* **No exempt/registered/deliberate/gap classifier in the emitter.**  That
  distinction is the CENSUS's (owners), not the emission's.  A classifier
  would be asymmetric in the dangerous direction — a MISS on a registered row
  would die a 1631-row file at transpile — and the only thing a
  transpile-DIE-for-gaps would add over the run-time die is caught already by
  the census gate, which fails a run on any NEW drop.
* **The deliberate row (`print 1+`) takes the same shape.**  No special case
  for one file's one statement.
* **#363's eval-string transpile-time die STAYS** — perl's `eval STRING`
  contract is compile-time `$@`, and that route already exists.
* **Module mode is covered in the SAME commit** — the die is IN the emission,
  so it is cache- and mode-independent; `pl2cl --module` keeps announcing
  nothing (ruled s403) and the statement says so itself when it is reached.

Why this and not the transpile DIE the s400 §6.4 ruling assumed: nothing in
that ruling required transpile time — it said "not before [the census is
near zero], because a die costs whole files".  P1's price table is the new
fact: the transpile shape costs 2581 rows for FOUR registered statements,
and "flip-legal per ROW" is false per FILE.  The run-time shape makes the
flip's unit the STATEMENT: every row before it survives, a program that
never reaches it is unaffected, a program that does gets perl's own `die`
(trappable in `$@`, assertable in a test row), and the silence — the whole
point — is gone everywhere, warm cache and module mode included.  Rule 12's
boundary (s329: DIE when a VALUE flows on, announce for an EFFECT-only case
in code that otherwise runs) does not give a dropped statement the
announce arm: absent code loses its values AND its effects.  The s400 §6.3
lvalue sentence in `not-supported.md` ("in file mode PCL drops the
assignment statement and announces it") is rewritten to "dies when the
statement runs".

**Order and bar (recipe in `docs/plan-post-s433.md` §1, session Q2):** lands
AFTER #467 + #462 (§A.3/§B.3 — the companion runner must measure FORMS and
the census must SEE modules before a price can be stated).  Then:
`corpus-diff` (diffs in exactly the four perl-tests census files, one
`nil`→`p-die` line each; SILENT-DROP unchanged at the same count);
four-population emission A/B (diffs = exactly the 27 census files + the 12
module files, every one the same one-line shape); full sweep TOTAL/LOST per
file — a lost row is accepted only as (i) a row AFTER the dying statement in
the same top-level form or (ii) a row that ran on the dropped statement's
`nil` (the accidental-pass kind, s418 ×2 + s432 ×1), edited into
`pass-baseline` with that cause; anything else is a finding; companion
`--all --quick` on the #467 runner with the A/B attribution recipe; the
board re-run for the three dists with drops (Text::Balanced's 780 rows will
MOVE where line 118/397 is reached — that is #457's price, not the flip's;
explain per row); gate; generation bump + the three artifacts;
`docs/ir-spec.md` (load model: what the drop form means at run time);
`not-supported.md` lvalue sentence; DECIDED line.  Guard: a `Pl/t` row of
the `$@`-shaped form (`eval { <a dropped shape> }; like $@, qr/not
supported at/`) — pick a shape that stays a drop (the lvalue assignment).

### A.2  Ask 2 — the module-mode increment.  DISSOLVED by A.1.

A module carrying a drop still loads; only the statement dies when reached.
There is no per-mode die to stage and no unblock list.  The 20 module drops
are ordinary fillers, ordered by the rows behind them: **#457** first
(Text::Balanced, 780 board rows, one repair in the §12/§15 `_ends_term`
family with its rule-13 §25 + `ppi-bug-report.t` row) → **#464** (the
modifier on `require SCALAR` / `local LIST` — Test2 ×4 + Sub::Uplevel; one
cause suspected, probe before assuming two) → **#466** (`$_` in the
filehandle slot) → **#465** (`$\`/`$,` defined — runtime, sweep-owing).
Confirmed: #457 is the first of them.

### A.3  Ask 3 — #462, precondition or filler.  PART OF THE FLIP'S BAR — lands before it, in the instruments session (Q1) with #467.

The flip newly reaches the module populations, and a price nobody can count
is not a price.  `tools/drop-census.pl` gains `cpan-tests/modules` (transpiled
with `--module`, so the emission is the one the runtime caches) and the board
dists behind a flag; the blessed census gains those rows with causes (the
§5 table of the s431 doc is the measurement).  It is also what makes the
four module fillers' progress countable.

### A.4  Ask 4 — the five never-measured companion rows.  The splice is the right form; the HOLE gets a report line.

A snapshot row's provenance is its comment (`# s431 first measurement`), no
new status value.  But "523 rows for 528 files and nobody noticed" is an
instrument hole of the #176 kind (a hole inferred from an absence): the
runner must PRINT, at the end of every run, the suite files that have no
snapshot row (count + names) as a named discrepancy.  One line in
`tools/run-perl-suite.pl`'s summary; same session as #467 (Q1).

---

## B. s432 — #456 half (a) (`docs/opus5-review-requests-s432.md`)

The fix is right and minimal: `%p-call-of-undefined-sub` follows perl's
order for a body-less sub (own-package AUTOLOAD, no `@ISA` walk — inherited
AUTOLOAD for a non-method call was deprecated in 5.004 and fatal since 5.28;
`(eq (symbol-package al) pkg)` correctly excludes a `:pcl`-inherited symbol
and the `:stub` test excludes a forward-declared AUTOLOAD), the stub body
calls it, `\&stub`'s trampoline is untouched, the guard rows fail the day
(b) lands (B.2), and the restored `perl-tests/sort.t` assertion is exactly
the kind of simplified test principle 5 forbids — good that it came back.
The pass-baseline edit is by hand with its cause; the companion splices
carry their A/B attribution.

### B.1  Ask 1 — AUTOLOAD before die.  CONFIRMED.

perl's rule, for the case that is actually perl's.  The re-silencing
(`sub AUTOLOAD {"AUTO"} { package Q; print main::nm(), "|\n"; } sub nm {"PKG"}`
→ perl `PKG|`, PCL `AUTO|`; probed) is an artefact of half (b), which is
promoted (B.4).  **And the same helper has a second customer today (§C.1,
#468):** a plain call to a NEVER-declared sub — `sub AUTOLOAD {…} print
nope(1)` — reaches no AUTOLOAD at all and dies with SBCL's raw
`undefined-function` (perl prints `A(main::nope)`; without an AUTOLOAD perl
says `Undefined subroutine &main::nope called`, PCL says `The function
main::pl-nope is undefined`, trappable but not perl's text).
`%p-call-of-undefined-sub` is the ONE mechanism for "a plain call reached no
body"; #468 routes the never-declared path through it.

### B.2  Ask 2 — the guard shape for a known-wrong-but-loud row.  RIGHT as shipped; the criterion is stated.

The question is never "TODO or not" — it is **does at least one row of the
guard turn RED the day the locked behaviour changes?**  Row 1
(`like /Undefined subroutine &main::nm called/`) fails the moment (b) makes
the program print `PKG`, which forces the edit the note asks for; row 2
(`unlike /^\|/`) is the inverse of the old silent output and keeps passing
harmlessly.  A `TODO` marker would make the row NOT COUNT, the opposite of
what is wanted.  Standing form for this kind of guard: assert the LOUD text,
name the task that will flip it, and make sure one row fails when it does.

### B.3  Ask 3 — rule 12 at −94 companion rows.  CONFIRMED: keep the die; #467 is the fix for the 94.

The 94 rows are the RUNNER's: plain `--load` ends a file at the first die,
in three files that already crashed; the sweep (recovery) priced the same
change at one accidental pass.  The state.t precedent was a die in the same
plain-load context.  Standing rule until #467 lands: **a companion row count
is not comparable to a sweep row count for any change that makes something
die.**  #467 RULED: recovery in BOTH runners (`p-load-with-recovery` is test
infrastructure, lives in `cl/pcl-test.lisp`, and the companion runner already
loads that file); the re-bless of `docs/perl-suite-run.tsv` lands in the
same commit, one measured pass, every moved file explained (expected: files
that died mid-way report MORE rows; a file that reports FEWER is a finding);
the rule "both runners load with recovery, a form-abort is counted and
printed" goes into `docs/test-infrastructure.md`.  The "hides a regression"
worry is answered by the runner's own output: the form-abort count and the
PARTIAL/INCOMPLETE status stay visible, and the loss is LOCALISED to the
form instead of the file — recovery hides less, not more.  Users are
unaffected (runpcl is plain load: a die ends the program, as in perl).

### B.4  Ask 4 — half (b)'s priority and SHAPE.  PROMOTED to the session after the instruments (Q3, before P3), and the shape is RULED from two new probes.

1. **It is not one sub's hoist — it is perl's PHASE model across sections.**
   Probed (§C.2, #469):

       our $x = 5;
       { package Q; sub q1 { 1 } }
       BEGIN { print "B=[$main::x]\n" }
       print "end\n";

   perl `B=[]` then `end` (BEGIN runs at compile time, before the
   assignment); PCL `B=[5]`.  The single-section inverse (`our $x = 5; BEGIN
   {…}`) is RIGHT — so the bug is exactly that section 1's RUN forms are
   emitted before section 2's compile-phase forms.  #456's sub is the same
   bug seen through a sub definition.  **Shape: emit every section's
   compile-phase forms (decls, captured, defs, sched — source order kept)
   before any section's run-phase forms, each group under its own
   `(in-package …)` switches.**
2. **The on-demand "hoist the DEFINITION alone" shape is UNSAFE** (the
   s432 sketch): the #456 reproducer with a captured file lexical
   (`my $x = 5; { package Q; print main::nm() } sub nm { $x }`) emits the
   body as `(block nil main::$x)` where `main::$x` is a `p-defcell`
   SYMBOL-MACRO defined in the sub's own section's decls.  Symbol-macro
   expansion is compile-time: a body compiled above its section's decls
   compiles `main::$x` as a plain free variable, not the cell — silent
   wrong.  The two-phase emission hoists decls with defs by construction;
   the FALLBACK if the two-phase A/B shows a diff class the session cannot
   explain is the on-demand hoist of the needed section's decls+defs — never
   the def alone.
3. **Measure first**: what the five section lists hold
   (`decls/captured/defs/sched/run`, Parser2 ~1770–1830 and the
   "Cross-section forward sub calls" block ~1845); how many files are
   multi-section at all (the 75 is the on-demand count — the two-phase
   moves every multi-section file's ORDER); whether any `sched` form is
   ENTITLED to run-time state from an earlier section (perl: no); compile
   time (nothing is compiled twice under the two-phase shape — MOVE, never
   copy).
4. **Bar**: the reproducer + the §C.2 shape + the inverses (single-section
   BEGIN, same-section call, sub-first, no-package-switch) vs perl; the
   four-population A/B with every diff explained per file (expected class:
   ordering only); full sweep TOTAL/LOST; companion on the #467 runner;
   gate; generation bump + artifacts; `docs/ir-spec.md` load-model line
   ("compile phase precedes run phase across sections"); sort.t's bug-36430
   row returns to `pass-baseline` by EDIT; the two decl-ordering-02 guard
   rows become one `both_agree`; #456 and #469 close together.

---

## C. Findings from the review probes (all PRE-EXISTING; filed, not fixed)

### C.1  #468 — a plain call to a NEVER-declared sub: no AUTOLOAD, a raw CL error

    our $AUTOLOAD; sub AUTOLOAD { "A($AUTOLOAD)" }   print nope(1), "\n";
        perl A(main::nope)      PCL: Unhandled undefined-function (SBCL backtrace)
    print nope(1), "\n";
        perl dies "Undefined subroutine &main::nope called at F line 1."
        PCL: the same SBCL error; inside eval {} $@ = "The function main::pl-nope is undefined."

The emission is a direct `(pl-nope 1)`; nothing turns SBCL's condition into
perl's order.  s432's `%p-call-of-undefined-sub` IS that order.  Two
candidate routes, to be decided by measurement: an `undefined-function`
handler at the program's dynamic extent (SBCL offers a `use-value` restart:
hand it `(lambda (&rest a) (%p-call-of-undefined-sub sym a))`; find where
runpcl / the installed wrapper / `p-eval-block` establish that extent), or
emit `p-declare-sub` for every called-but-undeclared name (the stub route —
a generation bump; check that `p-import` / `p-sub` clear the `:stub` mark,
they set `:defined`).  Either way ONE helper.  Bar: the two rows above with
and without `eval {}`, `sort nonexistent LIST`, a method call unaffected;
full sweep (runtime) + the A/B if the emission route is taken.

### C.2  #469 — cross-section phase order (the general #456(b))

See B.4.1.  Closes with #456 half (b).

### C.3  #470 — a file lexical promoted under its OWN name aliases the package variable of that name

    my $y = 7;  sub nm { $y }  print "[$main::y]\n";      perl []   PCL [7]

`_promote_captured`'s identity branch (Parser2 ~3035: "a name with exactly
ONE my/state binding file-wide can be renamed to the PLAIN package global")
counts only `my`/`state` declarations when it decides "unique"; an explicit
`$main::y` / `$::y` / `our $y` spelling of the same bare name in the file is
not counted, so the two variables share one cell (the emission shows
`(p-defcell main::$y …)` for the qualified mention and `(p-defcell $y …)`
for the promoted lexical — the same symbol under `(in-package :main)`).
Deliberate per its comment (v1's defvar model, string-eval resolution), but
the co-occurrence is a silent wrong.  Fix shape: the identity condition also
requires no package-global spelling of the bare name in the file (qualified,
`our`; `${"…"}` cannot be seen — accept), else mangle.  Scoping change → the
full sweep IS the gate + gate-SET scan over both populations.  Measure the
population first (files where a unique captured lexical co-occurs with its
package-global spelling); `docs/ir-spec.md` §cells row says "fresh name" —
amend it to say when the name is NOT fresh.  Filler.

---

## D. Standing rules added this session

* **The flip is a RUN-TIME die at the drop site, one shape, every mode**
  (A.1).  The s400 §6.4 "announce→die" ruling is satisfied in this shape;
  option-b-phase2-plan §3's "≤30 drops, all explained, THEN the emitters
  flip" precondition was for the transpile shape and no longer gates the
  flip — the census stays the gap-finder and the DROPS gate.
* **A companion row count is not comparable to a sweep row count for a
  change that makes something die** — until #467 makes both runners recover
  per form (B.3).
* **A guard for a known-wrong-but-loud outcome asserts the loud text and
  must have one row that turns RED when the behaviour is fixed** (B.2).
* **"Hoist one definition" is never the fix for a cross-section ordering
  bug** — symbol-macro cells make a body's compile position matter; the
  phase model is the fix (B.4).
