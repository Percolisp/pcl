# Review requests — session 415 (Opus 5, 2026-08-19/20)

Six commits.  The first finishes what s414 parked; the rest are Option B phase
2 — Track A (#371) plus the three drop families that could be fixed rather than
refused.  The asks that need a ruling are §7; everything else is a record.

`4aeabd2` #281 items 1+2+6 verified + merged · `048b687` #371 Track A ·
`2ff282b` #370 · `daa26c9` #369 · `829bcf5` #401 half · `f669d50` the
`_ends_term` fix that #370's first cut needed.

## §0  Status

| measurement | value |
|---|---|
| Gate `tools/prove-core` | **151 files / 5564 rows**, PASS except the pclxs xs rows (they produce 0–14 rows on their own; 13–14 red every run) |
| Full perl-tests sweep | **GATE clean** at each step.  TOTAL 18513 → **18369**: −158 (state.t, the ruled given/when refusal), +12 (sprintf2.t, the hex-float fix), +2 (bop.t, #370) |
| corpus-diff | Track A moved 4 of 111 files, each explained; #370 moved 1 (bop.t); #369, #401 and `_ends_term` **IDENTICAL** |
| emission-ab (lib) | 19/19 SAME after every commit |
| Companion `--all --quick --jobs 4` | run twice.  Buckets: 87 OK / 29 NOTAP / **115 XDIFF** / 1 FIXTURE / **291 UNEXPLAINED** (was 102 / 304 — the 13 newly-registered refusals) |
| Drop census | **73 files / 378 drops → 49 / 165**, re-measured over both populations at the end and matching the blessed file exactly |
| Probes vs perl 5.40.3 | ~35 shapes; every one identical except the two recorded divergences (#402, and concat.t:203 which is #402) |
| Tasks | #281 items 1+2+6, #371, #369, #370 closed; #401 half; **#399, #400, #401, #402 filed** |

## §1  #281 items 1+2+6 — the four legs s414 left

Bench A/B was the only interesting one: at K=5 `cfor` read +5.4 % and `gcdrec`
+3.2 %.  Both dissolved — `cfor` REVERSED at K=7, and `gcdrec`'s two
interleaved repeats gave branch 0.0921 / main 0.0970 / branch 0.0990 / main
0.0911, a within-tree spread wider than the between-tree delta.  `perl(s)`, an
identical binary, moved ±3 % across the same runs.  The structural argument
came first and is the real answer: the two trees' emission of the bench differs
in exactly the `let` → `p-…-ctx` renames.

`docs/ir-spec.md` is normative for the macros now (§4 table + **two legal
spellings**, §5.4 the comparator frame, §10 family row, §12's worked example
corrected — it had shown a context wrap around a call the `insensitive-call`
rule emits without one).

## §2  #371 Track A — and why two of its seven rows did not survive

The classifier is one function asked by the drop announcer, so it runs only
where a statement was already lost.  Five families refuse; the guard file pairs
every refusing shape with the sibling that must not.

**Indirect object was removed from the table** (#399): its only two census
drops are in perl-tests/ref.t and method.t, files worth 191 and 97 passing
rows, and perl still parses the syntax.  ~288 rows to convert 2 drops is the
opposite trade from the other families.

**Format was inverted**: its drops were in productive files, so the source-level
stripper got fixed instead.  Its `($str_re)|` pass-through is not a weak guard
but a wrong one — any quote imbalance opens a "string" that swallows what
follows.  **t/op/write.t stripped 39 of its 104 formats**, and each of the 65
survivors ate the statement after it.  The strip is line-anchored now; the
shared skip pattern also consumes comments, which fixed two more silent wrongs:
the hex-float pass was rewriting hex-float text INSIDE string literals
(`is(sprintf("%a",-0.0), "-0x0p+0", …)` had its EXPECTATION turned into `"-0"`),
and `CORE::state` was never normalised.

## §3  #370, #369 — statements that should compile

`~~`: PPI lexes it as one operator everywhere; perl reads two complements where
a term is expected.  Sixth token repair, rule 13 obligations in the same commit
(`ppi-upstream-bugs.md` §21, bug 20 in `ppi-bug-report.t`, canary + behaviour
rows).  `qx`: every delimiter is a `QuoteLike::Command` and only backticks had
their own class, so `my $c = qx{echo hi}` left `$c` undef — one primary arm now
takes both, reading the body and the interpolate-or-not answer from PPI's own
section record.

## §4  The one I got wrong, and what caught it

s415c's repair keyed on `_ends_term`, which did not list a REGEXP — so
`/X/ ~~ @a` (t/op/smartmatch.t) was split into two complements: a silent wrong
of exactly the kind the commit set out to remove.  My four must-not-fire probes
all had a Symbol or a bracket on the left.

**The drop census caught it** — re-measured after the session's edits,
t/op/smartmatch.t came back with 4 drops against a blessed 0.  Nothing else
would have: corpus-diff is perl-tests only and that file is perl's; no gate row
has the shape.  `f669d50` widens `_ends_term` with the tokens that yield a value
and are not Symbols (match/substitution, heredoc, `…`/qx…, `<FH>`) — right for
all four repairs, three of which were carrying the same latent hole — and adds
an END-TO-END guard row, because the unit rows never run the repair.

## §5  #401 half — refusals that predate the fix they distrust

Both `state` rename refusals were written against a renamer that has since been
made shadow-aware (#254 B-ii) and region-limited (#296-B2); one of them was
simply a caller not passing the `shadow_ok` waiver its own blocker already
took.  Probed in all three orderings, then removed.  t/opbasic/concat.t is back
to C 248/6.  The string-eval half is real and stays (see §7.3).

## §6  What this cost, stated plainly

`perl-tests/state.t` (158 rows) and `t/op/state.t` (126) refuse whole for one
`given` block each — ruled with Track A, recorded in #400, and not avoidable by
ordering: at the end of phase 2 every remaining drop dies and given/when will
still not be implemented.  Every other file the five families touch was already
producing 0–27 rows.  Both baselines were edited ROW BY ROW with the cause.

A refused file's OTHER drops leave the census with it (t/op/coreamp.t's 4
lvalue-sub drops, t/op/tie_fetch_count.t's 3), so the census falls faster than
the underlying problem.  That is written into the census header.

## §7  Asks

**7.1 — Track A's table vs the measurement.**  I removed indirect object from
the refusal list and inverted format from "refuse" to "fix the stripper", on
the numbers in §2.  Both are deviations from the written plan.  Ratify, or say
which should have been paid as written.

**7.2 — the census as a metric now hides refused files' other drops.**  Phase
2's exit criterion is "≤ 30 drops, every one explained".  With 15 files refused
whole, that number is easier than it was.  Should the criterion be restated
against a population that counts a refused file as (say) its pre-refusal drop
count, or is "refused is explained" the intent?

**7.3 — #401's remaining half touches eval capture.**  Making a string eval see
a `state` variable means adding original→state-cell pairs to
`_eval_lexical_alist`, the way the span-mangled file cells already are.  The
trap to answer first is the p-eval source+package cache key: a capture-dependent
emission must key that cache.  Worth 52 companion rows — schedule it, or leave
the refusal standing until the eval-capture work happens anyway?

**7.4 — #402 (interpolation stringifies instead of concatenating, so an
overloaded `.` never runs).**  Same family as #119.  Both are "we took a
stringify shortcut where perl runs an operator".  One task or two, and before
or after v0.1?

**7.5 — `class Foo;` (statement form) is a silent wrong the Track A boundary
cannot reach** (#399 (2)): it PARSES, as the indirect-object call `Foo->class`.
Refusing it needs a rule outside the drop site, keyed on `use feature 'class'`
being in scope — a new refusal on COMPILING code, so the s372 gate-SET bar.
Small and safe, but it is a new class of refusal: authorize?

## §8  Next

`docs/plan-post-s408.md` §2: J–L continue at **#372 (Track B1)**, which waits
on Fable's operand grammar; #343 sits behind it; the re-census and the
announce→DIE flip close the phase.  #279/#280/#282/#283 (release phases 3–5)
are unblocked but out of order.
