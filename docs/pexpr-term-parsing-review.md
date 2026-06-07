# PExpr term parsing — review and refactor plan

Status: **Option A implemented** (session 237b, the `_term_end` helper).
**Option B is the target architecture, NOT yet done** — this document records the
design so a future session can pick it up without re-deriving it.

---

## The problem area

`Pl/PExpr.pm`'s `parse()` loop does two entangled jobs:

1. **Reduction** (~lines 748–1090): collapse a run of tokens like
   `$h -> {a} [2] -> @*` into a single internal AST node. This is the postfix-`->`
   block (method calls, `->[]`/`->{}`, postfix deref `->@*`/`->%*`/`->$*`, postfix
   slices `->@[..]`/`->@{..}`/`->%[..]`/`->%{..}`) plus the subscript/slice builder.

2. **Operand-boundary finding** (~lines 2606–2790): when a named-unary op
   (`defined`, `ref`, `exists`, `keys` via the 1-arg path, …) or a strictly-1-arg
   function needs "just the next *term*", figure out **where that term ends** — the
   `$end_pars` machinery.

Job #2 is the maze. It exists *because of how job #1 is scheduled*: when an operator
needs its operand, the postfix chain to its right has not necessarily been reduced
to a single node yet, so each operator re-derives the term boundary by hand-walking
raw tokens.

## Why it is a maze (the concrete smell)

A Perl **term** has a single grammar:

```
term     := cast* primary postfix*
cast     := $ @ % & *
primary  := symbol | magic | number | quote | qw
          | block{...} | (list) | [ctor] | word-call | already-reduced-node
postfix  := [subscript] | {subscript}
          | -> [..] | -> {..} | -> method(...) | -> method
          | -> @* | -> %* | -> $* | -> $#*
          | -> @[..] | -> @{..} | -> %[..] | -> %{..}
```

But the "consume a postfix chain" walk was **re-implemented five-going-on-six
times** (pre-Option-A: lines ~2620, 2642, 2671, 2694, 2712, plus the `keys`
special case), each a near-copy:

```perl
while ($end_pars + 1 < scalar(@$e)) {
    my $nx = $e->[$end_pars + 1];
    if    (ref($nx) eq 'PPI::Structure::Subscript') { $end_pars++; }
    elsif ($nx is '->' and next is Subscript)        { $end_pars += 2; }
    else { last; }
}
```

…and they **disagreed** on coverage: some handled `Cast + Symbol` (`@$list`), some
`Symbol + Subscript`, some `Symbol -> Subscript`, some `Block -> Subscript`; **none**
handled `Symbol -> Cast` (`$hr->%*`) until a sixth variant was bolted on for `keys`.

Worse, the boundary rules are **forked into two parallel blocks** that re-implement
the *same* `Cast+Symbol / Symbol+Subscript / Symbol+Block` decisions with subtly
different coverage:
- named-unary ops: ~2606–2740
- strictly-1-arg functions: ~2742–2790

That fork is exactly why `keys $hr->%*` (1-arg path) and `defined $hr->%*`
(named-unary path) needed *separate* fixes.

**Root cause: there is no single notion of "a term."** Each operand-grab is an
incomplete, hand-rolled re-derivation of the one grammar rule above.

---

## Option A (DONE) — `_term_end` helper

A single `_term_end(\@e, $start) -> $end_index` that consumes `cast* primary
postfix*` and returns the index just past the term. Every operand-grab site calls
it instead of its bespoke walk. This deletes the duplicated lookahead and gives all
named-unary/1-arg operators identical, complete coverage. `keys $hr->%*` and friends
work without per-operator special cases.

Option A is purely **boundary computation** — it changes *where* an operator's
operand ends, not *how* terms reduce. It is the low-risk, mechanical win, validated
by the full `Pl/t` gate + the perl-tests sweep. See the commit that introduced
`_term_end`.

**Option A's residual limitation:** it still computes a boundary as a token *range*
and then re-parses that range. The reduction (job #1) still happens in the same
tangled loop. The operator-binding still happens over partially-reduced tokens.
Option A makes the maze a corridor; Option B removes the corridor.

---

## Option B (TARGET, NOT DONE) — two explicit phases

Split `parse()` into two passes over the token array, matching how Perl's own
parser is layered (build terms, then bind operators):

### Phase 1 — term reduction

Walk the token array left to right. Wherever a `cast* primary postfix*` run begins,
reduce the **entire** run — including the full postfix chain — into a single
internal node, in place. After phase 1 the token array contains only:
- already-reduced **term nodes**, and
- **operators** (binary ops, `,`, named-unary words, function words, `?:`, etc.).

No operator ever sees a raw `->`, `[..]`, `{..}`, or deref `Cast` again — those are
all *inside* term nodes.

A single routine drives this: `_reduce_term(\@e, $start) -> ($node_id, $next_index)`,
the reducer form of Option A's `_term_end`. It is the **one** place that knows the
`term` grammar; the postfix-`->` block and the subscript/slice builder become its
internals (or are called by it), rather than firing opportunistically from the main
loop.

### Phase 2 — operator binding

Run the existing precedence/operator logic over the phase-1 stream. Because every
operand is already a single node, **the entire `$end_pars` machinery disappears**
(~180 lines): a named-unary op or 1-arg function simply takes *the next node*. No
lookahead, no chain-walking, no named-unary-vs-1-arg fork. `keys $hr->%*` is just
`keys <node>`.

### What Option B deletes

- All five-going-on-six "consume postfix chain" while-loops.
- The named-unary operand block (~2606–2740) and the 1-arg-function operand block
  (~2742–2790) collapse to "take the next term node."
- The `$deref_skip` bookkeeping (~lines 2074, 2164) that threads "extra elements
  consumed by a `->` deref chain" through the block/anon-sub handling.
- The special cases added for `keys $hr->%*`, `defined EXPR->%*`, etc.

### Why it is correct by construction

Phase 1 reduces terms **maximally and uniformly** before any operator binds, so the
"where does my operand end" question has exactly one answer (the end of the term
node), computed in exactly one place. New postfix syntax (a future `->&*`, say) is a
one-line addition to the `postfix` rule in `_reduce_term`, automatically visible to
every operator — instead of N call sites to update.

---

## Risk and validation

- **Phase 1 is the hot path.** Term reduction runs for every expression; a subtle
  ordering change (e.g. a primary that is itself a parenthesised sub-expression, or
  a `word-call` whose args must reduce first) can shift behaviour broadly.
- **Indirect-object / filehandle disambiguation** (`print $fh @x`, `print STDERR …`)
  leans on the current loop's ad-hoc ordering and `%p-fh-arg` markers — Phase 1 must
  preserve "is this Word a function call or a bareword/filehandle?" decisions. This
  is the most likely place for Option B to regress; budget for it.
- **`local`/`my`/`our` parenthesised LHS** and the slice-vs-kv-slice sigil rules
  must survive the move into `_reduce_term`.

Validation path (same safety net as Option A):
1. `prove -j8 Pl/t/` — full gate (3285+ tests).
2. `perl sweep-perl-tests.pl --jobs 8` + `tools/sweep-diff.pl diff
   docs/fail-baseline.tsv .faillog` — zero new regressions.
3. The feature matrices in `/tmp/refmatrix*.pl` / postfix probes from session 237b
   (reference & deref family) re-diffed against Perl 5.40.

## Recommended sequencing for Option B

1. Land `_reduce_term` as a pure addition that *returns the same node* the current
   loop would, but for one term — and unit-test it in isolation against PPI inputs.
2. Switch **one** operand site (e.g. `defined`) to "take next node via phase-1
   reduction"; gate+sweep.
3. Migrate the rest of the named-unary / 1-arg sites; delete the `$end_pars` blocks.
4. Fold the postfix-`->` reduction and subscript/slice builder *into* `_reduce_term`
   so the main loop no longer reduces terms opportunistically; delete `$deref_skip`.
5. Final gate+sweep; re-bless baseline.

Do it as its **own** change set, not bundled with feature work.
