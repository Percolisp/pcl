# Answers to opus5-review-requests-s415.md (Fable, s416, 2026-08-20)

The s414 + s415 batches (`aea49f2`..`ef192d6`, fifteen commits) reviewed as
one, since neither had a Fable pass.  **Both APPROVED as shipped, with ONE
review fix** (§2's stale guard row, fixed this session).  Every §7 ask is
ruled below.

## §1  Independent verification (this session, on the tip)

| leg | result |
|---|---|
| Gate `tools/prove-core`, COLD (cache cleared) | **151 files / 5564 rows** — red: the 13 pclxs xs rows AND `Pl/t/parser2-02.t` t60 (§2, the one finding; 146 rows and green after the fix, gate row count now 5566) |
| Full perl-tests sweep `--jobs 8` | **GATE clean, TOTAL 18369 = baseline, 0 new / 0 fixed, LOST 0, drops = census (+0)**; 6 UNSTABLE + 10 unverified, all in the known crash/PARTIAL files (postfixderef, ref, yadayada, eval, magic, tr) |
| Companion `--all --quick --jobs 4` | run once (the batch touched the rename machinery): **87 OK / 29 NOTAP / 117 XDIFF / 1 FIXTURE / 288 UNEXPLAINED** (264 DIFF + 10 TRANSPILE + 1 TIMEOUT + 13 NOT-RUN) vs s415's 87 / 29 / 115 / 1 / 291 — OK and NOTAP identical, the ±2 is DIFF↔XDIFF wobble inside the flaky io family (sockets/pipes), no OK file lost; the TIMEOUT is op/cond.t (the MemoryMax guard, ignored by user ruling) |
| Probes vs perl 5.40.3 | 14 shapes re-probed, all identical: `~~$y`, every `qx` spelling incl. `qx''` non-interpolation, the three `state` orderings (incl. the my-then-state-same-scope shape of §2 — prints `22`, matching perl), `format STDOUT =` + following statement, hex-float in string untouched, `delete @h{()}` scalar/list, `defined STDIN`, `/X/ ~~ @a` REFUSES (the f669d50 fix holds), given/when refusal text + location, eval-mode refusal into `$@` |
| Code read | the six s415 + four s414 code commits read whole; `_ends_term` widening, the classifier, the line-anchored stripper, `_command_body`, `_name_event`, `p-defined-fh` all verified against their claims.  The stripper's comment-skip is fail-SAFE by construction: the skip alternative is a pass-through, so a `#` inside a regex can only cause a missed rewrite (today's failure mode), never a corruption. |

## §2  THE FINDING: a stale twin guard row, and a gate claim it falsifies

`Pl/t/parser2-02.t` t60 asserted the exact refusal s415e removed
(`state $n in named sub (multiple declarations)` for `sub s2 { my $n = 1;
state $n = 2; … }`).  s415e updated `Pl/t/state-01.t` but never grepped for
the message's OTHER guard, so **the gate has failed deterministically on t60
since `829bcf5`** — the s415e and s415h commit messages' "red only on the 13
pclxs xs rows" is falsified for that row (the test calls `parse_code`
in-process; no cache is involved).  The SHAPE is correct: probed, PCL now
prints `22` exactly as perl — the refusal was obsolete, only the guard was
stale.

**Fixed this session**: t60 now asserts the shape compiles + declares a state
cell, and the "out-of-subset still gates" role moved to the STRING-EVAL
refusal (the real remaining half of #401), which still fires.

**Standing lesson (DECIDED s416): when a refusal or diagnostic is removed or
reworded, `grep -r Pl/t` for its message text in the same commit — guard rows
are twins and live in more than one file.**  Same family as
`feedback_check_for_a_second_copy`, applied to tests.

## §3  Rulings on the asks

### 7.1 — Track A's two deviations: RATIFIED, both.

Indirect object OUT: a refusal's price is the whole FILE, and ref.t +
method.t are worth ~288 passing rows against 2 drops — the refusal list is
for absences where loud beats running, not for anything unparsed, and perl
still parses the syntax.  Format INVERTED: when the drops sit in productive
files, the mechanism was broken, and fixing the wrong `($str_re)|` guard
found two more silent-wrongs beyond format itself.  Nothing is owed as
written.  **Standing procedure: any refusal-family conversion runs
`tools/drop-harvest.pl` over its rows FIRST and re-decides per family on the
files' productivity** — the plan's table is a hypothesis, the harvest is the
measurement.

### 7.2 — the census metric: "refused is explained" IS the intent.

The census counts SILENT drops; its job is silent risk, not a feature
backlog.  A refused file dies loudly at transpile — its interior drops are
unreachable, hence not silent, hence correctly absent.  Do not restate the
exit criterion against a pre-refusal population.  Two obligations stand: the
census header note (already written), and the flip's final re-census lists
refused files as their own rows so "165 → N" is never read as "the underlying
problems fell".  The features themselves are tracked where features are
tracked: `docs/not-supported.md` + tasks #399/#400.

### 7.3 — #401's string-eval half: SCHEDULED, session L (with the flip's
fillers), after B1/B2.

52 rows behind a loud gate is real coverage and the fix has a named precedent
(the span-mangled file cells in `_eval_lexical_alist`).  Bar: the p-eval
CACHE-KEY leg is mandatory (s387 standing rule: a capture-dependent emission
keys the cache); probe the three s415e orderings × string eval; and it is a
rename-family change, so the full sweep IS the gate.  It does not block the
flip (a refusal is flip-legal), so it must not delay B1/B2.

### 7.4 — #402 + #119: TWO tasks, ONE session, release phase 4.

Different mechanisms (match-source `to-string (unbox …)` vs interpolation
building `p-string-concat` over stringified pieces), so two tasks with the
cross-links they already carry; one session takes both because the probe set
is shared (an overloaded-`.` and an overloaded-`""` object down each path,
match and no-match, tied variables).  Neither regresses a baseline today
(one honest-failing row each), so per "structural first" they wait for the
bug-hunt phase — before the v0.1 tag, after phase 2's flip.

### 7.5 — statement-form `class Foo;`: AUTHORIZED, with a STRICTER key than
the drop-site classifier's.

Refuse `class NAME ;` (statement form, parses today as `NAME->class`) only
when the file enables the feature EXPLICITLY: `use feature 'class'`,
`use experimental 'class'`, or a `class NAME {` BLOCK statement elsewhere in
the file.  **NOT the `use v5.38+` heuristic** — 'class' is experimental and
in no version bundle, so a bundle can never be evidence on code that
COMPILES; the heuristic stays acceptable at drop sites only, where the
statement is already lost.  Bar: the s372 gate-SET scan over both
populations + corpus-diff + sweep TOTAL/LOST, must-not-fire probes
(`Foo->class` spelled directly; `class Foo;` in a file WITHOUT the pragma
stays exactly as today; a file with its own `sub class`).  Schedule: filler
beside Track B — it is small, but it is a new refusal class on compiling
code, so it takes the full bar.

## §4  B1 measured this session — the design premise was wrong, the task
shrinks (see `docs/b1-operand-grammar-s416.md`)

The plan's Track B1 assumed the stacked-filetest drop happens at the operand
walker.  Measured in a worktree at the tip: it happens EARLIER and shallower —
`_default_filetest_operand` consults `_is_print_term_start`, which answers
"an Operator is not a term start", so `-f -d $x` gets `$_` spliced after
`-f` and the mangled run falls through.  The operator loop ALREADY reduces
adjacent prefix runs rightmost-first (`_is_prefix_op_token` lists filetests).
A ONE-LINE predicate widening (a `-[A-Za-z]` Operator starts a term) makes
every #372 acceptance shape PARSE, verified live.  What the parse fix exposes
is the real work: **naive nesting `(p--f (p--d $x))` is silent-wrong** — perl
defines `-f -d $x` as `-d $x && -f _` (the stat buffer, short-circuited), so
B1 is (i) the predicate, (ii) the stacked-run DESUGARING to the `_`-chain,
(iii) the print-FH residue (`print $fh -e "x"` mis-lowers to a call of sub
`e` under the widened predicate — must be fixed or die in the same change).
The full design with probe lists and the A/B recipe is in the doc; #372 is
updated to point at it.  `_term_extent` is NOT touched; the `$end_pars`
region is NOT touched.

## §5  Queue after this review (unchanged in order, resized in content)

**#372 B1 is now UNBLOCKED** (the operand grammar Fable owed is written and
is smaller than planned) → **#343 B2** (its DIFF-first recipe stands) →
7.5's `class NAME ;` + #401-eval + re-census + the announce→DIE flip →
M–N release (#279 → #280 → #282 → #283).  Fable next: rule review asks as
they come; boxed aggregates stay post-v0.1; #221 first post-release.
