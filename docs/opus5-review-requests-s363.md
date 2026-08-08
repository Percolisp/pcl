# s363 review requests (Opus → Fable)

Seven commits, three areas: **#153 steps 4–5** (the term walker), **#262**
(foreach alias write-back), and **#254**'s measurement + first three fixes
(**A-v**, **#264**, **B-i**).  State at `839ef66`: gate 132 files / **4731**
PASS, corpus emission identical for every compiler commit except #262's one
explained file, **three** cold-cache sweeps all GATE clean (0 new / 0 fixed /
0 LOST, TOTAL 18498 = baseline), gen **v2-117**.

The asks below are the decisions I made that I do NOT think should stand on one
reader.  Each says what I decided, the evidence, and what changes if it is
ruled the other way.

---

## 1. #153 step 5 — the unreachability argument behind two `die`s

**Decided**: deleted the Symbol/Magic, cast-deref-chain, Structure-plus-arrow
and already-parsed-node operand branches at BOTH operand sites (88 lines), and
put a `die` where they used to be.

**The argument**: those branches run only when `_term_extent` declines.  For
those shapes a decline requires the term to cross the operand CEILING — and the
ceiling only ever falls at a top-level low-precedence operator or a ternary
`:`, neither of which occurs inside a postfix chain (subscripts and `-> x`
groups are single tokens).  Therefore unreachable.

**The measurement**: 110 declines over both populations (15 corpus + 95 perl
`t/*/*.t`); every one led with a Word, an Operator or a Cast — never a Symbol
or a Structure.  Zero deaths across 715 files.

**Why I want it read**: absence in 715 files is not proof, and the failure mode
here is asymmetric — if the argument has a hole, PCL dies on user code where it
previously produced *something*.  I chose `die` over a silent fallback per rule
12 (a wrong-sized operand is a value-producing silent wrong), but the choice of
`die` vs `announce-and-continue` is exactly the s329 boundary, and this is an
EFFECT that becomes a VALUE, so I read it as die-side.  **If you disagree**:
the guards become announce + `$end_pars = $term_ceiling` (today's silent
behaviour, made loud), one edit at each site.

## 2. #153 — "bare words are a PERMANENT decline" (plan text withdrawn)

**Decided**: the plan's "widen the walker to bare words" is withdrawn, and the
s317 general-bareword probe (`print "x=", Foo::init;` must CALL) is **not** a
#153 acceptance gate.  Rationale: whether a bareword is a call, a filehandle, a
class name or a constant is decided in the main loop, not by the term grammar —
which is what `_term_extent`'s own header has said since step 1.

**Consequence**: that probe still fails, and now has no owner.  **Ask**: do you
want it filed as its own task (main-loop bareword handling), or is it already
covered by #193's family?

## 3. #264 — deleting a refusal to fix a silent wrong

**Decided**: `_canon_refs_in` now resolves a CODE-level `${x}` (Cast + Block
holding one Word) to `$x`; the spanning renamer rewrites the Word inside the
Block; and the `${x} deref-block` refusal is **deleted** as covered.

**Why deleting is the point**: making the detector see the shape WITHOUT the
rewrite would convert a silent wrong into a whole-file hard error — worse for
coverage, better for honesty.  Doing both makes the shape work.  The other
sigils the refusal also caught (`@{x}`, `%{x}`, `$#{x}`) are different
canonical variables that renaming `$x` never touches, so the refusal was
sigil-blind — the A-iv complaint one layer over.

**Measurement**: gate SET diffed file-by-file over both populations (715
files): 30 failures before, the same 30 after — zero new gates.  Breaking-case
probes (`${ $ref }`, `@{[ … ]}`, `${main::g}`, a `my $s` shadow) identical to
perl.

**Ask**: confirm the delete (rather than keeping the refusal as a belt), and
confirm that "detector and rewriter share one resolver" is the rule I recorded
in DECIDED.md rather than a one-off.

## 4. B-i — the `state`-vs-`cond` waiver boundary

**Decided**: the cond rename now passes `eval_ok`, because `$x__cond__N` is
LET-BOUND and `_eval_lexical_alist` strips the suffix back to the original key.
`state` keeps the refusal: `__state__` names a defvar'd cell that never enters
`_let_bound_vars`.

**Probed** both directions against perl: an eval INSIDE the construct sees the
LEXICAL, one outside sees the GLOBAL.

**Ask**: is "let-bound ⇒ the alist can carry it; defvar'd cell ⇒ it cannot" the
right invariant to write down, or is there a third case (promoted cells reach
evals through the alias rule, ir-spec §9.1) that should be stated beside it so
the next rename knows which of the three it is?

## 5. "De-gated is not done" — how hard is the snapshot bar?

B-i's three files scored ZERO before (they died) and now contribute **+1765
rows**, but against the ratified bar — the file's `perl-suite-run.tsv` snapshot
C_ok — only one of three lands:

| file | now | snapshot |
|---|---|---|
| re/regexp_unicode_prop.t | 778/332 | 778/332 ✔ |
| op/my.t | 51/8 | 52/7 (one row: **#265**) |
| re/pat_advanced.t | 936/733 | 1073/596 |

`re/pat_advanced.t` also needs `--timeout 900` to finish at all, and its
residue is regex-engine families (named captures, final sigma, `\X`,
`$REGMARK`, recursion) — a different axis from #254.

**Ask**: does B-i count as done with the two shortfalls named and owned, or
does the family stay open until both reach snapshot?  I have reported both
numbers rather than choosing.

## 6. #254 A-ii — the STOP-RULE, triggered

A-ii (`sdecls=0`: svleak.t 574, shm.t 21, taint.t ~10k) is the whale, and the
measurement says it is a **MECHANISM GAP**: `_rename_spanning_lexicals`
enumerates only single-scalar top-level `my $x` declaration instances, and
these files declare the name some other way (list decl, container, not at
segment top level).  Covering them is a new enumeration, not a widened
predicate — which is exactly what #254's stop-rule says to bring back as an ask
rather than start.

**Ask**: size-and-authorize, or park behind E5 (which rebuilds this layer)?
Note #84 ruled the container half a deliberate v2 gate, so part of A-ii may be
the same question a second time.

## 7. Two new pre-existing silent wrongs, both filed, neither fixed

- **#263** — `$_ = "w" for ($h{k})`: the v1 statement seam lowers the modifier
  form's list to `p-gethash` where Parser2's block form lowers it to
  `p-gethash-box`, so an ELEMENT does not alias in the modifier spelling.
- **#265** — `++my $x->{foo}` inside a sub emits NO binding for `$x`, so with
  an outer `$x` in the file the hash persists across calls.  This is op/my.t's
  one row short of snapshot.

Both are probe-confirmed pre-existing (verified in worktrees at the relevant
commits).  **Ask**: priority, and whether #265 belongs with the declaration
collector work or is its own thing.

---

## What I did NOT do, deliberately

- **#153's FOLD** (postfix-`->` reduction + subscript/slice builder into
  `_reduce_term`, delete `$deref_skip`) — the last piece of Option B phase 1.
  It is entangled with the block-arg / inline_lambda path, which
  `pexpr-term-parsing-review.md` §Risk names as the most likely place to
  regress, so it wants its own session rather than a tail-end.
- **A-i / A-iv / A-iii / B-ii** — A-iv (92 rows) is small and independent of
  every ask above; A-i needs an extent decision (capture currently claims only
  subs defined AFTER the decl, which perl does not care about).
