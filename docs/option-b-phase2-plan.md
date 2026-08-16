# Option B phase 2 — SIZED (Fable, s407, 2026-08-16)

*The `$end_pars` collapse was planned as "operator binding over the term-node
stream: a named unary or 1-arg function takes the next node, ~180 lines
deleted" (`docs/pexpr-term-parsing-review.md` §Phase 2), with the #138 drop
census as its metric and the announce→DIE flip as its last step (`docs/
fable-answers-s400.md` §6.4, task #343).  This document sizes it FROM THE
CENSUS TEXT rather than from the parser, and the answer changes the plan.*

## 1. The measurement

`tools/drop-census.pl` at `e79f0a6`: **73 files / 373 drops** (blessed
`docs/parse-error-drop-census-s399.tsv`, now edited to match — the one stale
row was `t/re/pat_advanced.t` 11 → 4, s404's #351 repair).  The new
`tools/drop-harvest.pl` then transpiled every census file with
`PCL_DROP_ANNOUNCE=all` and collected the announced statement TEXT: 369 rows
(the 4 missing are duplicate-line statements the announcer de-duplicates).
Read row by row and grouped by what the statement IS:

| family | drops | files (top) | what it is |
|---|---:|---|---|
| **`given` / `when` / `default`** | ~117 | op/switch.t 112, coreamp.t 2, state.t 1, perl-tests/state.t 1 | feature 'switch' — REMOVED in perl 5.42; PPI does not lex it as a compound |
| **lvalue-sub assignment** `f() = …`, `&{…}(…) = …` | ~41 | op/sub_lval.t 35, coreamp.t 3, substr.t 2, try.t 1, signatures.t 1 | the RULED file-level refusal that keeps dropping loudly (`fable-answers-s400` §6.3) |
| **stacked filetests** `-f -d $x`, `-l -e _`, `-t -e $tty` | ~27 | op/filetest.t 19, filetest_t.t 5, stat.t 2, tie_fetch_count.t 1 | **TERM GRAMMAR** — a named unary whose operand begins with another named unary |
| **`class` / `field` / `method` / `ADJUST`** | ~25 | class/*.t (7+6+5+…) | feature 'class' (5.38, experimental) |
| **hex float literals** `0x1.18p+0` | ~20 | op/sprintf2.t 19, perl-tests/sprintf2.t 1 | a NUMBER LITERAL PCL does not read (`%a` tests) |
| **unicode identifiers in stashes/globs** `delete $ＦŌŌ::{Ƒ}`, `$ᛁ앛::ISA[1]`, `<Ạ>` | ~16 | uni/gv.t, uni/stash.t, uni/caller.t, mro/*_utf8.t, uni/readline.t | non-ASCII package/glob names — E5-adjacent naming |
| **`defer { … }`** | 13 | op/defer.t | feature 'defer' (5.36) — implementable: `unwind-protect` on the enclosing block |
| **`format NAME = …`** | ~9 | write.t, uni/write.t, gv.t, uni/gv.t, closure.t, rt119311.t, comp/parser.t 2 | formats — not supported (registered) |
| **`qx{…}` / `qx(…)` / `qx[…]` / `qx/…/`** | 8 | fflush.t, alarm.t, fork.t 2, getpid.t, override.t 2, stat.t | **A BUG, not an absence** — only backtick-spelled commands are handled; `my $c = qx{echo hi}` leaves `$c` undef, silently (task #369) |
| **`my $c = end { … }`** | 6 | die_unwind.t 4, die_except.t 2 | a `(&)`-prototyped user sub named `end` — the block-form call; sizing needed (prototype visible? keyword clash?) |
| **`~~` prefix** `is(~~$y, 3)` | 2+ | bop.t (both populations) | **PPI MIS-LEX** — `~~` at term start is two complements in perl, one smartmatch operator in PPI 1.291 (task #370, rule 13) |
| **`~~` smartmatch** (infix) | 5 | smartmatch.t 4, tie_fetch_count.t 1 | removed in 5.42 — not supported |
| **the #343 shape** `f ref $u, "m" or g "fb"` | ~4 | perl-tests/bless.t, split.t, t/op/bless.t, split.t | **TERM GRAMMAR** — parenless call × named-unary first arg × low-prec `or/and` after |
| **indirect object** `method $obj "a"`, `doit $object "FOO"` | 4 | method.t, ref.t (both populations) | indirect-object syntax — not supported (die) |
| **`__SUB__` in an anon sub** | 4 | op/current_sub.t | #368 |
| **`(sub {…})[0]()`, `$${$_[0]}`, `*{;undef}`, `@119797`, `local *a = *1`, `*X = *-`, `*^R = …`, `${no strict; \$_}`, `++${"23::foo"}`, `substr $x,0,1, = …`, `ok <~>`, `any_tainted @_`** | ~14 | scattered singles | odd spellings — most are the fold's BY-DESIGN residue (DECIDED s398) or PPI mis-lexes; each is one row |
| **deliberate syntax-error / torture tests** `print 1+`, `"${; =pod =cut }"`, `sub 'Hello'_he_said (_);`, `<<EOW;`, `open(…) , || _diag $!` | ~7 | comp/parser.t, comp/final_line_num.t, io/open.t | perl itself rejects or the test asserts the error |
| **regex-body drops** `1 while /(a+b?)(*SKIP)(?{…})(*FAIL)/g` | 4 | re/pat_advanced.t | PPI mis-tokenizes a `/…/` that carries `(?{…})` — the "Ternary `?`" message |
| **`x` / `x();` as a statement** | 4 | op/lexsub.t | `my sub x` called bare — PPI lexes a lone `x` as the operator (#361's family, statement-initial) |

**So: of 373 drops, roughly 300 are FEATURE ABSENCES or deliberate error
tests, ~40 are the term grammar, and ~15 are lexer bugs / small gaps.**  The
`$end_pars` collapse as designed — "take the next node" — buys the ~40 and
nothing else; it does not touch the census's bulk.  A phase 2 that begins with
the parser rewrite would spend its risk budget on 10% of the metric.

## 2. The re-scoped plan — three tracks, in this order

### Track A — feature absences become RULED REFUSALS (Opus, one session, no parser risk)

At the two PARSE-ERROR emitters (Pl/Parser.pm, where the drop is announced),
classify the dropped statement's LEADING keyword / shape and DIE perl-shaped
with a not-supported entry, instead of dropping:

| leading shape | die text | not-supported.md |
|---|---|---|
| `given` / `when` / `default` (also `CORE::given`) | `PCL: given/when (feature 'switch') is not supported — removed in perl 5.42` | new section; op/switch.t registers XDIFF |
| `class` / `field` / `method` / `ADJUST` (statement-initial, and `method` only when a `class` block is open or `use feature 'class'` is in scope) | `PCL: feature 'class' is not supported` | new section; class/*.t register |
| `format NAME =` | existing formats entry (`Pl/t/format-skip-01.t`) | cite it |
| lvalue-sub assignment | already ruled: **stays a loud DROP** (§6.3), no change | — |
| `~~` infix | `PCL: smartmatch (~~) is not supported — removed in perl 5.42` | new |
| indirect object `WORD $obj LIST` | `PCL: indirect object syntax is not supported` | new (perl itself discourages it; `no feature 'indirect'` is the 5.36 default) |
| `defer {` | see Track C — implement, do not refuse | — |
| hex float literal | see Track C — implement | — |

Boundary: this classifier runs ONLY at the drop site — a statement that
compiles never reaches it — so it cannot break working code; and the keywords
are perl CORE features (language, 9a's smell test does not apply).  Bar: the
census falls by the family's count with NO other row moving; op/switch.t etc.
move from DIFF/NOTAP to XDIFF in the companion with the registration; the
sweep TOTAL/LOST (state.t's one `given` — check what it costs: a die where a
drop was may abort the file EARLIER; if it does, that file's registered rows
must be re-blessed row by row, never a blanket re-bless).

### Track B — the term grammar (the real Option B phase 2; Fable-designed, Opus-executed)

**B1 — a named unary's operand may BEGIN with a named unary** (stacked
filetests, ~27 drops).  Today the operand-taker (`_reduce_term` via
`_term_extent`, then `_extend_high_prec`) declines a leading prefix op BY
DESIGN and the site falls back to the `$end_pars` machinery, which has no case
for `-f -d $x`.  Design: in the ONE operand-taker, `operand := NAMED-UNARY
operand | term high-prec-tail` — a named unary (or filetest) at operand start
recurses, producing a `funcall`/`prefix_op` node that is itself the term.
Precedence is already right: a named unary binds tighter than the comparison
operators and looser than arithmetic, and `_extend_high_prec` applies to the
INNER operand exactly as it does today for `ref $x + 1`.  Acceptance rows
(perl-oracle, `Pl/t/reduce-term-01.t`): `-f -d $x`, `-l -e _`, `-e -t -t $tty`,
`defined -e $f` (works today — inverse guard), `!-e $f` (works — the plain
prefix path), `-f -d $x ? 1 : 0`, `-f -d $x && …`, `ref -e $f`?? no —
`lc -e $f` (perl: named unary applied to filetest result), and the ternary/
comma continuation after the chain.  Population: filetest.t 19, filetest_t.t
5, stat.t 2 must stop dropping and their rows must be spliced with causes.
A/B recipe: the s398 fold recipe — `PCL_B1=1` flag, `tools/emission-ab.pl
--env` over the FOUR populations, byte-identical except the ~27 explained
diffs, then flip.

**B2 — the #343 shape**: parenless call × named-unary FIRST argument × a
low-precedence `or`/`and` after (`f ref $u, "m" or g "fb"`; the inverses
`f $u, "m" or g`, `f(ref $u, "m") or g`, `f ref $u, "m";` all work today —
probed s407).  Mechanism NOT yet located; it is in the interplay between
`handle_subcalls`'s argument-extent for a user sub without parens and the
named-unary operand ceiling (`$last_low_prio_op`).  The task carries the
minimal reproducers and the DIFF that must be taken first: dump `@$e` at the
"Fell through" die for `f ref $u, "m" or g "fb"` vs `f $u, "m" or g "fb"`.
Perl-oracle rows: the three probed shapes + `is ref $u, "main", "d" or diag $@`
(bless.t's own row).  This is the ONLY family that touches the paren-less-call
argument extent — do it AFTER B1, alone.

**B3 — the operand-site collapse itself** (the original phase-2 text): once B1
and B2 land, re-measure how many operand-site fallback branches are still
REACHABLE (the `_term_probe` DECL arm is live; s363's argument-plus-measurement
method), and delete only what is unreachable — as steps 3–5 of phase 1 did.
The `$end_pars` machinery does not need to disappear for the census to hit
zero; it needs to stop being the only path for shapes the walker can own.  Do
NOT rewrite `parse()`'s main loop for this: the census does not ask for it.

### Track C — small gaps and lexer bugs (fillers, any session)

* **#369** `qx{…}` / `qx(…)` / `qx[…]` / `qx/…/` are DROPPED (silent undef): the
  term walker's primary set knows backticks only.  8 drops.  One primary
  arm + guard rows.
* **#370** PPI lexes a term-initial `~~` as the smartmatch operator (perl: two
  complements).  Token repair (`~~` where a TERM is expected → `~ ~`), rule
  13 logged, canary, report row.  2+ drops (bop.t in both populations).
* `defer { … }` (13): a `p-defer` = register a thunk on the ENCLOSING BLOCK's
  unwind-protect, run LIFO on any exit — small, and perl 5.36+ code uses it.
  Own task when someone wants it; until then Track A refuses it perl-shaped
  (`PCL: defer blocks are not supported`).
* hex float literals (20): `0x1.18p+0` → `parse-perl-number` + PPI's Number
  token; small.  Own task.
* `my $c = end { … }` (6): size (is `end` a keyword collision or the `(&)`
  prototype path?) — one probe.
* `x` / `x();` statement-initial (lexsub.t 4): the #361 repair for the
  statement-initial position (no previous Word — a lone `x` statement is a
  call when the document declares `sub x`).

## 3. Then the flip

When Track A + B1 + B2 + #369/#370 have landed, re-run the census: the
expectation is **≤ 30 drops, every one a deliberate error test, a registered
absence, or an odd single explained in this document**.  THEN the emitters
flip from announce to DIE (file mode too) — the last step of phase 2 as ruled
(§6.4) — with the s373 three-leg bar (gate-set scan over both populations,
sweep TOTAL/LOST, corpus-diff), and #363's eval-mode die is already in place
by then.  A remaining drop that is neither test-deliberate nor registered
blocks the flip; it does not get an exemption.

## 4. What this changes upstream

* Task #153's "phase 2" text and #343's "belongs in Option B phase 2" pointer
  both meant the term grammar; that scope is Track B here (~40 drops), and it
  is SMALLER than planned.  The maze may stay a corridor.
* `docs/pexpr-term-parsing-review.md` §Phase 2's "~180 lines disappear" is a
  by-product to measure at B3, not a goal.
* Fable's next design item is #281 (the IR pass) — this sizing was the
  prerequisite for scheduling phase 2 at all, and it says phase 2 is mostly
  Opus-sized work with one Fable-designed piece (B1's operand grammar).

## 5. Recipe (reuse)

    tools/drop-census.pl . scratch/census.tsv 8          # counts per file (~3.5 min)
    tools/drop-harvest.pl scratch/census.tsv scratch/drops.tsv   # the TEXT (~2 min)
    # then cluster: cut -f4 (reason) | sort | uniq -c;  and read the texts per file
    tools/emission-ab.pl --env PCL_FLAG=1 --list FILES  # A/B a flagged widening
