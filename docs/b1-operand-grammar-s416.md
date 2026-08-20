# Option B phase 2, Track B1 — stacked filetests, DESIGNED FROM MEASUREMENT
(Fable, s416, 2026-08-20.  Supersedes the grammar sketch in
`docs/option-b-phase2-plan.md` §2 Track B1 and in task #372: the premise
"the drop happens at the operand-taker" was measured FALSE this session, and
the task both shrinks and changes shape.)

## 1. Where `-f -d $x` actually dies (measured at `ef192d6`, worktree)

The token run never reaches `_reduce_term`.  The pre-pass
`Pl/PExpr.pm _default_filetest_operand` walks the expression BEFORE the
operator loop and, for every filetest Operator whose next token is not a
"term start", splices a `$_` in.  Its oracle is `_is_print_term_start`, which
answers **0 for every Operator except `!`/`~`/`not`** — so in `-f -d $x` the
`-d` after `-f` reads as "no operand", `$_` is inserted, and the mangled run
`-f $_ -d $x` falls through the main loop ("Bug. Fell through" — two reduced
nodes side by side, no operator).  That is the whole story of the ~27 census
drops: **the operator loop itself already handles stacked prefix runs** —
the "adjacent prefix operators reduce INNER-first" walk (PExpr.pm ~line 1602)
takes the RIGHTMOST of a consecutive prefix run, and `_is_prefix_op_token`
already lists `-[A-Za-z]`.

Verified live: with the one predicate widened (§2 step 1), every #372
acceptance shape PARSES and `-f -d $x` emits `(p--f (p--d $x))` — the
recursion the plan's grammar production wanted, with zero walker changes.
`_term_extent` keeps declining prefix ops BY DESIGN; the `$end_pars` region
is not touched; there is no new grammar production.  **Do not implement the
`operand := NAMED-UNARY operand` production — nothing needs it.**

## 2. The three pieces (one commit each, in this order)

### Step 1 — the predicate: a filetest Operator STARTS A TERM

In `_is_print_term_start` (Pl/PExpr.pm ~4582), beside the `!`/`~`/`not`
exception:

    return 1 if $op =~ /^-[A-Za-z]$/;   # a filetest starts a term

This is the rule-11 fix: the function is the shared "does a term begin
here?" oracle, and in perl a filetest DOES begin a term at every one of its
three call sites — `_default_filetest_operand` (the bug), print bareword-FH
detection (~4069: `print STDERR -e $f` — STDERR is the FH *because* `-e`
starts a term), and paren-form FH extraction (~4631, same logic).  Scoping
the fix to `_default_filetest_operand` alone would leave the print sites
answering the opposite of perl; if the probe sweep (§4) turns up a
regression at a print site, narrow there and record why — but the default is
the shared oracle.

KNOWN residue this exposes (measured): `print $fh -e "x"` now takes `$fh` as
the filehandle (correct) but the argument path lowers `-e "x"` into a CALL of
sub `e` (`pl-e` undefined at run time).  That is a crash, not a silent wrong,
but it is a REGRESSION for any program that ran the old mis-parse — step 1's
commit must make the print-argument path parse a leading filetest correctly
(it is one more consumer of the same run; probe `print $fh -e $f`,
`print STDOUT -f -d "/tmp" ? "y" : "n"`, `print -f -d "/tmp"` bare) or die
naming the shape.  Do NOT ship the predicate without this leg.

### Step 2 — the LOWERING: a stacked run is the `_`-chain, not a nest

Naive nesting is SILENT-WRONG, probed:

    perl:  -f -d "/tmp"          == -d "/tmp" && -f _     → ""   (false, defined)
    naive: (p--f (p--d "/tmp"))  → -f applied to the STRING "1"  → undef
    perl:  -e -f "/etc/passwd"   → 1;   naive → undef            ← the smoking gun

perldoc -f -X: `-f -w -x $file` ≡ `-x $file && -w _ && -f _` — RIGHTMOST
test runs first on the real operand, each earlier test applies to the stat
buffer `_`, `&&`-short-circuited (a false/undef inner IS the result).  PCL
already implements `expr && -f _` correctly (probed at HEAD:
`-e "/etc/passwd" && -f _ && -r _` → 1 = perl).

So: at the prefix-run reduction site (the ~1602 walk, which already knows the
run's extent), when a consecutive run of ≥2 prefix ops is ALL filetests,
lower the run as the equivalent `&&`-chain over `_` instead of recursing
one-at-a-time.  A run MIXED with `!`/`~`/`-`(negate) keeps today's per-op
recursion around the filetest sub-run (`! -e $f` stays `(p-not (p--e $f))`).
Implementation freedom: either build the chain node directly, or token-rewrite
the run into its documented `&&` spelling and let the ordinary machinery
lower it — whichever reuses more; the emitted shape must be the short-circuit
chain, and `p-and`'s value semantics already carry the false value through.

FIDELITY NOTE, filed with the change: PCL's `-f _` on a successful stat of a
non-plain-file answers **undef** where perl answers **""** (defined-false).
`defined(-f -d "/tmp")` will diverge until the runtime's filetest false value
is perl's.  Fix it in the same commit if it is one site
(`p--f`/`%p-filetest` family — likely one shared returner), else file it
with the probe as its reproducer; op/filetest.t asserts `defined` on these.

### Step 3 — the census + baselines

Population (from the census): t/op/filetest.t 19, t/op/filetest_t.t 5,
t/op/stat.t 2, t/op/tie_fetch_count.t 1 (the tie one is inside a refused
file — it moves only if the file un-refuses; expect it NOT to move).  The
perl-tests twins as measured.  Census rows leave by EDIT with the cause;
sweep TOTAL/LOST; the companion files re-read per row.

## 3. A/B recipe (the ruled s398 fold recipe, scaled to the size)

`PCL_B1=1` guards steps 1+2 together; `tools/emission-ab.pl --env PCL_B1=1
--list` over the four populations — byte-identical except the explained
diffs (files containing stacked filetests or print-FH-filetest shapes);
gate-SET scan over both populations (a decline becomes an accept AND print-FH
classification can move: this is the s372 bar, all three legs); full sweep
TOTAL/LOST; then flip the default and DELETE the flag in the same session.

## 4. Probe list (perl-oracle, before the flip; rows land in
`Pl/t/reduce-term-01.t` — new file, it does not exist yet)

MUST-WORK (probed s416 in the worktree unless noted):
`-f -d "/tmp"` (→ "" defined-false), `-f -d "/etc/passwd"`,
`-e -f "/etc/passwd"` (→ 1), `-l -e _`, `-e -t -t $tty` (three deep),
`-f -d $x ? 1 : 0`, `(-f -d $x && 7)`, `lc -e "/tmp"` (→ "1"),
`my @a = (-f -d "/tmp", "tail")` (comma continuation, 2 elements),
`ok(-f -r $1, "d")` (funcall arg position).
MUST-NOT-MOVE (inverse guards, all work today): `defined -e $f`, `!-e $f`,
`!!$y`, `- -$y`, `~~$y` (the #370 complement), `-d` bare (`$_` default,
r9), `print $x - 3` (binary minus: Operator `-`, not `-X` — the predicate
keys on the ATTACHED letter), `f(-e $file => 1)` (fat-comma guard, PExpr
~5597).
PRINT-FAMILY (step 1's leg): `print STDERR -e $f`, `print $fh -e $f`,
`print -f -d "/tmp" ? "y" : "n"` (the s407 probe that DROPS today),
`print STDOUT -e "/tmp"`.
MIS-LEX BOUNDARY (record, don't chase): `$n -e $b` — PPI lexes one `-e`
Operator where perl may mean `$n - e($b)`; PPI's choice predates this change
and the predicate does not read that position (only what FOLLOWS a filetest
changed) — probe that today's behaviour is unchanged, whatever it is.

## 5. What B1 is NOT

No `_term_extent`/`_reduce_term` change, no `$end_pars` edits, no new
grammar production, no named-unary WORD-site change (`lc -e $f` already
worked via `_extend_high_prec`).  If a shape turns up that genuinely needs
the operand-taker to take a leading named unary, it is B2/#343 territory —
measure there, don't widen here.
