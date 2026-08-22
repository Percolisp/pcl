# B3 — the operand-site collapse, SIZED from measurement (Fable, s428, 2026-08-22)

*Track B3 of `docs/option-b-phase2-plan.md` §2 (task #153, the last piece of
Option B).  B1 (#372, stacked filetests) landed s417 and B2 (#343, the stale
operand ceiling) landed s418, so B3 is unblocked.  This document runs B3's
own step-1 measurement — "re-measure how many operand-site fallback branches
are still REACHABLE, and delete only what is unreachable" — and sizes the rest
of B3 from it.  It supersedes the one-paragraph B3 sketch in
option-b-phase2-plan §2 Track B and the "~180 lines disappear" line in
`docs/pexpr-term-parsing-review.md` §Phase 2.*

## 1. The measurement (both populations, s428, gen v2-174, tree `53dcd2e`)

### 1a. Reachability of the operand-site fallback branches

`tools/term-diff-sweep.pl --env PCL_TERM_DECL=1` (the live `_term_probe` DECL
arm; the s361 rule requires BOTH populations, the 111-file corpus alone is not
enough):

| population | files | declines | first operand token |
|---|---:|---:|---|
| `perl-tests/*.t` | 111 | 9 | **Word ×9** (all `unary`) |
| perl's own `t/*/*.t` | 604 | 72 | **Word ×72** (47 `single`, 25 `unary`) |

**Every one of the 81 declines leads with a Word, an Operator or a Cast** — the
two shapes `_term_extent` declines BY DESIGN (a bare word: call? filehandle?
class name? constant?; a prefix-op run: `~0`, `+($r//0)`, `-e _`).  Not one
leads with a Symbol, a Magic, a Structure or an already-reduced node.  The
corpus declines are `exists +($r//0)->…` / `delete +($r//0)->…` (leading unary
`+`) and `length ~0` (leading `~`); the perl-t set is the same two families at
scale.

**Conclusion — B3's deletion half is ALREADY COMPLETE.**  Steps 3–5 (s363)
deleted the Symbol / Magic / cast-deref-chain / Structure-arrow / already-parsed
operand branches at both sites and replaced them with a rule-12 `die` asserting
"unreachable".  This measurement re-confirms, over 715 files, that the `die` is
never hit and the surviving branches (cast-over-word, `~`/`!` prefix, and the
`_extend_high_prec` fall-through for Word/Operator/Cast) are exactly the
reachable set.  **There is nothing left to delete.**  The `$end_pars` machinery
does not disappear — the plan already predicted it would not; it has simply
stopped being the only path for the shapes the walker owns.

### 1b. The drop population (what B3 could still WIDEN to claim)

`tools/drop-harvest.pl` over the blessed census (`docs/parse-error-drop-census-
s399.tsv`, 32 files / 104 drops) → the statement text of every drop, classified:

| bucket | drops | B3? |
|---|---:|---|
| lvalue-sub assignment (`f()=…`, `&$r(0)=…`, `keeze=64`, `continue{…}`) | 39 | **no** — permanently exempt, ruled `fable-answers-s400.md` §6.3 |
| **#411 postfix call-of-call/subscript** (`$s2->()()`, `$subsubs[0]()(0)`, `(sub{})[0]()`) | 8 | **YES — the cleanest widening** |
| **#374(b) `WORD WORD WORD` lexsubs** (`my $x = if__lexsub if__lexsub if__lexsub`) | 4 | **yes — hardest, last** |
| **#259 parenless proto list-op arity** (`1 == a_hash 'a'`, `(unilist3 0 \|\| 5)`) | 3 | **yes — after #411 — SHIPPED s429** |
| family-4 glob/symbolic-ref surgery (`$${$_[0]}`, `*{;undef}=3`, `*X=*-`, `local *a=*1`, `*^R=*…`, `++${"23::foo"}`, `${no strict;\$_}`) | 12 | no — by-design walker decline + PPI mislex; #410/E5-adjacent |
| non-ASCII stash/glob/readline names (`<Ạ>`, `++${"\xff::foo"}`) | ~6 | no — #410 naming |
| indirect object (`method $obj "a"`, `doit $object "FOO"`) | 4 | no — #399, MAYBE LATER (USER s425) |
| regex code blocks / control verbs (`(?{…})`, `(*SKIP)`) | ~7 | no — registered |
| deliberate torture / registered (`print 1+`, heredoc bodies, hex float, format, 4-arg-substr-comma) | ~15 | no — principle 9 / registered |
| `if if if` (unrenamed) / statement-`x` residue | ~3 | no — #374(a) done s408; the rest is lexer |

**So of the 104 drops, exactly ~15 are B3-widenable term grammar** (#411 8,
#259 3 — four with t/op/utftaint.t's `(@)`, found s429, #374(b) 4), not the ~40 the earlier estimate carried — that number
folded in family 4 (by-design declines) and the lexsub crashes.  The rest are
exempt, registered, feature absences, or naming/lexer work with other owners.

## 2. The widening half — three shapes, in order

The change is always in `_term_extent` (`Pl/PExpr.pm`) — the one walker — so it
is visible to both operand sites AND `_fold_terms` at once.  Each is landed
alone, with acceptance rows in `Pl/t/reduce-term-01.t` FIRST, then the
four-population emission A/B (`tools/emission-ab.pl`, byte-identical except the
explained diffs), then the gate + full sweep + the s373 three-leg bar
(gate-SET scan over both populations, sweep TOTAL/LOST, corpus-diff).

### B3.1 — a `(args)` List after a COMPLETED postfix step is a call (#411) — SHIPPED s428

PPI hands these over as (probed s428):

    $s2->()()        Symbol  Op(->)  List()   List()
    $subsubs[0]()(0) Symbol  Subscript[0]  List()  List(0)
    (sub{})[0]()     List(sub{})  Constructor[0]  List()
    $x->{a}()        Symbol  Op(->)  Subscript{a}  List()
    $a[0]()          Symbol  Subscript[0]  List()
    $h{k}()          Symbol  Subscript{k}  List()

perl allows an elided `->` between chain elements, so a `Structure::List`
directly after a completed postfix element is a call of that element's result.
(`f()()` is NOT one of these — a paren-less call's result cannot be called
without `->`, it is a perl SYNTAX ERROR; the design's earlier list was wrong.)

**What the measurement changed about the plan.**  The sketch above (a guard in
`_term_extent`) was based on the assumption the walker STOPPED SHORT.  It does
for the subscript cases (`$a[0]()` ext=1) but for `$s2->()()` / `$r->{m}()` the
walker already claimed the full extent (ext=3) and the statement STILL dropped —
so the real gap was in the REDUCTION (`parse()`), not the extent: the trailing
`(...)` reached the arrow/subscript reducer as a bare List with no operator and
fell through ("Missing case: [").

**The fix that shipped is smaller and needs no `_term_extent` change at all.**
A single normalizing pre-pass `_insert_elided_call_arrows` (beside the other
`_retag_*` passes, run before `_fold_terms`) makes the elided arrow EXPLICIT —
`$a[0]()` → `$a[0]->()` — so the ONE existing `-> ( args )` path (walker W2 +
reduction Case 2, which already re-tags following braces as implicit-arrow
subscripts) handles every shape.  Building a fresh token list makes the
insertion CASCADE: `$subsubs[0]()(0)` → `$subsubs[0]->()->(0)`.  The
discriminator is on the token BEFORE the List: a Subscript (`$a[0]`, `$h{k}`,
`$x->{m}`), a `-> ( )` call result (a List whose own predecessor is `->`), or a
list-slice (a Constructor `[` preceded by a List/Condition/qw primary — the W4
discriminator) — never a bare Symbol/Word primary (so `$foo(1)`, `func(1)`,
`$cr->(9)` are untouched).

**Verification (s428, tree vs base `b70ccaf`):** guard `Pl/t/elided-call-01.t`
(4 rows, 4 s) + reduce-term-01.t still 127/127; **corpus-diff: only closure.t +
ref.t differ, each ONLY the previously-dropped statement now emitting a
`p-funcall-ref`, silent drops 7 → 5**; **lib A/B SAME=20 / DIFF=0**; cold gate +
full sweep below.  Population: perl-tests closure.t + ref.t un-drop; perl's own
t/ op/closure.t, op/current_sub.t, op/ref.t un-drop via the companion (current_sub
also needs anon `__SUB__` #378 for its rows to PASS, so measure DROP→OK there).

**Risk:** `_term_extent` feeds `_fold_terms`; a `Structure::List` after a term
is the review doc's named regression zone (indirect object / filehandle).  The
`$next > $i` guard is precisely the "not the bare primary" line that keeps those
out, but the four-population A/B is mandatory — byte-identical except the eight
#411 sites.

### B3.2 — a declared sub's call parses by its PROTOTYPE'S SHAPE (#259) — SHIPPED s429

`1 == a_hash 'a'` (`(%)`), `2 == a_hash 'a','b'`, `(unilist3 0 || 5) == 6`
(`(;$;)`), and — found by the same measurement — `any_tainted @_` (`(@)`,
t/op/utftaint.t), `taint_these @list[…]` (`:prototype(@)`, t/op/taint.t) and
`f 5` for `sub f($x = 1)`: every `min 0` prototype and all-defaulted signature.

**What the measurement changed about the plan.**  The sketch above guessed "the
operator-loop term reading does not consult the prototype" and "touches
`handle_subcalls`'s parenless-argument extent".  The `@$e` dump at the die said
otherwise: `[funcall, node]` — the sub had become a ZERO-ARGUMENT call and its
real arguments were left juxtaposed.  `Pl::PExpr::no_params_of_sub` returned
the declared sub's `min_params` (an ARITY fact) and its callers read 0 as "takes
no arguments" (Config's convention).  `($)` worked only because min = max = 1.
The 16×10 matrix (prototype/signature shape × call context, vs perl) then
showed the same misreading on the other side: `(*)` (min 1, `_proto_max_args`
declines `*`) was a LIST op, and a 1-param / slurpy SIGNATURE was a NAMED UNARY.

**The fix is ONE helper, no extent change.**  `Pl::PExpr::_proto_parse_spec`
reads the prototype's shape the way perl's toke.c `just_a_word` does and
answers in Config's convention (`()` → 0; after leading `;`s exactly one of
`$ _ * +` or one `\X`/`\[…]` → 1, `[0,1]` when `;`-led; else → -1; a signature
→ -1; builtin records keep Config's table); `no_params_of_sub` returns it for
any DECLARED record; the s361 second look at the prototype at the
strictly-single site is deleted (redundant).  `handle_subcalls`' argument
extent is untouched: with the right class, the existing zero-arg / strictly-
single / list branches already do the right thing.

**Verification (s429, vs `374fcf7`):** matrix SAME 61 → 103 of 160, every change
toward perl; corpus-diff IDENTICAL (111 files, drops 5); lib A/B SAME=22;
perl-t A/B 602/604 SAME (proto.t + utftaint.t, only the un-drops); cpan A/B
SAME=402; gate-SET scan exactly proto.t/utftaint.t drop→OK + taint.t's first
line (its pre-existing s///e TRANSPILE-FAIL now first); companion rows
unchanged (proto.t's three were ACCIDENTAL passes; per-row A/B: no move); cold
gate 159/5693; census 29/94 → 27/90; guard `Pl/t/proto-parse-class-01.t`.
Sweep not run (the s401 row: corpus-diff identical + lib byte-identical).
Residue filed: #453 (user named-unary operand extent through `.`/`+` — the
builtin site's `_extend_high_prec` is missing at the user site; the two
operand sites should be ONE), #454, #455, #260's `(_)` row.

### B3.3 — `WORD WORD WORD` of declared empty-prototype lexsubs (#374(b), last)

`my $x = if__lexsub__N if__lexsub__N if__lexsub__N` — three juxtaposed 0-arg
declared-`()` sub calls (t/op/lexsub.t; #337 renamed the keyword `if` to a
lexsub cell).  The walker declines bare words by design; claiming this needs "a
Word that is a declared empty-prototype sub is a 0-arg call primary", which is a
bareword-primary decision the walker has deliberately stayed out of (s364).
Hardest and least corpus-supported (one file); do it last, possibly as its own
sub-task, and only once B3.1/B3.2 have proven the widening method here.

## 3. What B3 is NOT

The census's other ~89 drops are not B3's: 39 lvalue-sub (exempt), the family-4
glob/symbolic-ref surgery (by-design declines + PPI `$$` mislex — #410 / E5),
non-ASCII names (#410), indirect object (#399 — MAYBE LATER, USER s425), regex
code blocks and the deliberate torture rows (registered), hex float / format /
4-arg-substr-comma (registered or their own tasks).  Widening the walker for any
of these is out of scope; several would be actively wrong (family 4 is the
walker's correct decline, the Xsub/fallback owns them).

## 4. The flip, after B3

When B3.1–B3.3 land, re-run `docs/drop-census-s419-flip-gate.md`'s census.  The
flip's precondition (that doc §4) is the unblock list #410 / #374(b)+#365 via
B3 / #153-B3 residue / #411 / #413(done) / #412(done) / #399 / #259 / #414(done)
/ #415(items 1+4 done) — B3 clears #411, #259 and #374(b) from it; #410 (the
21-row non-ASCII-names family) and #399 remain the largest holdouts.  B3 does
not itself enable the announce→DIE flip; it removes three of the flip's
blockers.  The recipe is in option-b-phase2-plan §5; the reachability
re-measurement in §1a here is repeated as B3's own regression check (it must
stay all-by-design after each widening — a new Symbol/Structure decline would
mean the widening introduced a walker inconsistency).
