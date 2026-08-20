# Option B phase 2, Track B2 (#343) — the mechanism, LOCATED and measured

(s417, 2026-08-20.  `docs/option-b-phase2-plan.md` §2 Track B2 said "Mechanism
NOT yet located" and named the measurement to take first — the `@$e` dump at
the "Fell through" die for `f ref $u, "m" or g "fb"` vs `f $u, "m" or g "fb"`.
This file is that measurement, taken, plus the population scan it justified.
**No fix is proposed here: Track B is Fable-designed.**  What follows is the
diagnosis, the blast radius, and the two things a design has to decide.)

## 1. The mechanism — a STALE INDEX, not a grammar gap

`Pl/PExpr.pm handle_subcalls` scans `@$e` **right to left** (~line 3564).  When
it passes an `and`/`or`/`xor` it records the position:

```perl
    $last_low_prio_op = $i;      # ~3575
```

and when it later reaches a paren-less list-operator word it uses that position
as the argument ceiling:

```perl
    my $end_pars = scalar(@$e)-1;
    $end_pars    = $last_low_prio_op-1  if defined $last_low_prio_op;   # ~3758
```

The scan **mutates `@$e` as it goes**: every word it reduces splices its
operand run down to a single node.  Those splices happen at positions to the
LEFT of the saved index — which shifts the operator left — and the saved index
is never adjusted.  So `$end_pars` is computed from a position that no longer
means what it meant.

Traced live (`PCL_B2_TRACE=1`, the probe added in this commit):

| statement | `@$e` when `f` is reached | `or` is now at | saved | `end_pars` | result |
|---|---|---|---|---|---|
| `f $u or g "fb"` | `[f, $u, or, <g>]` | 2 | 2 | 1 | **correct** |
| `f ref $u or g "fb"` | `[f, <ref>, or, <g>]` | 2 | 3 | **2 = the `or`** | swallowed |
| `f ref $u, "m" or g "fb"` | `[f, <ref>, `,`, "m", or, <g>]` | 4 | 5 | **4 = the `or`** | swallowed |

The error is `(elements the intervening reduction consumed − 1)`, so it is
**not always off-by-one**: `ref $h{k}` consumes three elements and the ceiling
lands two positions late.

That single fact explains every probed inverse in #343 without further
theory — `f $u, "m" or g` has no reduction between the word and the `or`;
`f(ref $u, "m") or g` puts the arguments in a List that is reduced before this
scan; `f ref $u, "m";` has no `$last_low_prio_op` at all.

## 2. What the swallow produces — and it is not always a drop

At the die, `@$e` is two funcall nodes **side by side with no operator between
them** (this is the same end-state as the B1 failure, from a different cause):

```
FAIL  f ref $u or g "fb"
  DIED: Bug. Fell through. Missing case: [
    bless({ id => 6, type => "funcall" }, "PPIreference"),
    bless({ id => 0, type => "funcall" }, "PPIreference") ]

  #6  funcall
    #9  Word[f]
    #8  Operator[or]        <-- f's ARGUMENT is the whole `or` expression
      #3  funcall(ref $u)
      #7  <g "fb", parsed a second time>
  #0  funcall(g, "fb")      <-- and g "fb" is ALSO a top-level node
```

versus the working shape, where `or` is correctly the root:

```
WORK  f $u, "m" or g "fb"
  #7  Operator[or]
    #3  funcall(f, $u, "m")
    #0  funcall(g, "fb")
```

**When the shift is larger the statement does not drop — it RUNS, wrong.**
Measured:

```perl
sub f { print "f(@_)\n"; 0 }   sub g { print "g(@_)\n"; 1 }
my %h = (k => "v");
f ref $h{k} or g "fb";
#   perl:  f()        g(fb)
#   PCL:   g(fb)      f(1)          <- inverted order, wrong argument
```

So this family has a counted half (the drop) and an **uncounted half** (the
silent wrong).  The drop census can only ever see the first.

## 3. Blast radius — the whole population, scanned

`PCL_B2_TRACE=1` over both populations (658 files: perl-tests, perl's own
`t/`, `lib/`; `op/cond.t` excluded as always), 1m34s at 8 jobs:

**10 stale-index events in 3 distinct source files.**

| source | shape | verdict, PROBED against perl |
|---|---|---|
| `bless.t:179` (both populations) | `is ref $untied, "main", 'blessing through tied refs' or diag $@;` | **DROP** — the census's known bless.t row, and #343's headline reproducer |
| `split.t:503` (both populations) | `my ($sp) = grep /\s/u, map chr, reverse 128 .. 255 or skip '…', 9;` | **SILENT WRONG, in no count** — perl runs the `or` branch, PCL does not |
| `t/re/reg_fold.t:165` | `eval join ";\n", "plan tests=>"…, @tests, "1" or die $@;` | **benign** — the index is stale but the emitted code is correct (probed E1/E2 below) |

The `reg_fold.t` row is the important caveat: **a stale index is necessary but
not sufficient for a divergence.**  Any design that turns this probe into a
gate must probe each site rather than trusting the disagreement.

Probes (perl-oracle, `cmp.pl` row harness):

```
E1 eval join … or die            perl[head A B]              PCL matches
E2 eval join, body dies          perl[FALLBACK fired]        PCL matches
E3 grep …, map chr, reverse … or perl[SKIPPED sp=[undef]]    PCL[sp=[undef]]   <- or branch lost
E4 same, empty result            perl[SKIPPED z=[undef]]     PCL[z=[undef]]    <- or branch lost
E5 is ref $x,"main","d" or diag  perl[is2(…) diag(err)]      PCL[]             <- dropped
```

Note the words involved are **not only user subs**: `map` and `eval` fire too.
#343's framing ("a parenless USER-sub call") is narrower than the mechanism.

## 4. What a design has to decide

1. **Recompute or adjust?**  Either derive the ceiling from the CURRENT `@$e`
   at the moment it is used (one scan rightward from `$i`, which is what the
   probe already does and what the ternary-`:` boundary block immediately below
   line 3760 already does for its own boundary), or adjust `$last_low_prio_op`
   by each splice's delta.  The first has one source of truth and no bookkeeping;
   the second preserves the current single-pass shape.  Both are small.  Neither
   is a `$end_pars` GRAMMAR change — this is a bookkeeping bug inside it, which
   is why it is worth deciding whether `docs/pexpr-term-parsing-review.md`'s
   "do not add rules in that region" applies at all here.

2. **Is the probe kept, and does it become a gate?**  It is emission-neutral
   and env-guarded (same shape and cost as the existing `PCL_TERM_DECL` probe
   beside it).  With only 3 sites in the whole population, a
   die-on-disagreement gate is affordable — but `reg_fold.t` proves a
   disagreement can be harmless, so a gate needs either a whitelist or a
   sharper predicate.

**Acceptance set** when it is designed: #343's three reproducers, the two real
sites above (bless.t:179, split.t:503), the benign one (reg_fold.t:165, which
must not change), the four probed inverses, and the siblings #259 and #335 that
the plan groups here.  Bar: the s372 three legs (gate-set scan over both
populations, sweep TOTAL/LOST, corpus-diff) — a decline becoming an accept.
