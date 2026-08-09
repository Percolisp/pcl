# Fable answers — s370 asks (ruled s371, 2026-08-09)

Review of Opus 5's s370 (`f2c7c25` + `0e5b088`, #267 both steps) against
`docs/opus5-review-requests-s370.md`.  Everything re-verified independently,
not read off the commit messages.

**Verification done this session:**

* Gate `tools/prove-core`: **132 files / 4744 tests PASS** — matches the claim.
* Ten live probes vs perl, all IDENTICAL — including three shapes outside
  Opus's twelve: the **same plain scalar twice** (`for ($x, $x) { $_++ }` →
  3), **vivification through the alias at k=2** (`for ($v{a}, $v{new}) { $_ =
  9 }` → both slots 9, `new` created), and a **named loop var at k>1**
  (`for my $v ($g{a}, $g{b}) { $v .= "!" }`).  Also re-verified: `->` chain +
  `$$r` deref elements, `tr///` through the alias in the modifier spelling,
  element + plain scalar mixed, and both inverse guards.
* Code read in full.  Both commits have the ruled shape: ONE resolver
  (`_foreach_scalar_elements`) on the shared #138/#140 splitter, verdicts
  taken off untouched tokens before lowering and mapped by position, `@el ==
  1` preserving the byte-identical k=1 path, mixed lists declining whole.
* Ask-3's two comma walks analysed to a proof — see §3.

**Both commits are APPROVED as shipped.  #267 stays CLOSED.**

---

## §1 — Ask 1 RULED: probes + guard rows ARE the bar; do NOT widen the population

When the shape occurs in no corpus we own, the corpus cannot be the
verification instrument — twelve live-vs-perl probes spanning the shape's
axes plus inverse guards pinned in `Pl/t/foreach-aliasing-01.t` is exactly
what verification means here.  A wider CPAN grep is a new axis, and the §4
filler-scope rule stands: file rather than grow.  In this case there is not
even anything to file — the guards hold the claim, and any future corpus
occurrence is covered by the mechanism, not by having been enumerated.

**The consequence you flagged is accepted as a standing rule (already in the
project memory, restated here as the ruling): for an emission change whose
expectation is ZERO corpus change, run `tools/corpus-diff.pl` FIRST.**  A
clean corpus-diff proves the sweep's `.t` half cannot move, in minutes.  The
full sweep is then only owed when the ruling explicitly demands its
TOTAL/LOST gate (as §2c did for step 1 — that sweep bought the LOST/TOTAL
verdict and the `my.t` baseline retirement, so it was not wasted, but the
*order* corpus-diff-then-sweep is the cheap-first order).  The gate-SET
stderr normalization (`ROOT/(Pl|tools)/\S+ line \d+` → `line N`) is likewise
mandatory whenever the diff touches a `.pm`'s line count.

## §2 — Ask 2 RULED: the k=1 anchor-miss must DIE too — own filler commit, measurement first

Your rule-12 reading is correct, and there is a stronger argument: **a failed
anchor is always a compiler self-inconsistency**, never a benign decline.
`_foreach_alias_rewrite` predicted a head from the AST; if the lowered text's
outermost call is not that head, then either (a) the verdict was right and
the swap is being skipped — the write lands on a copy, which is the
#262/#263 silent-wrong itself — or (b) the verdict was wrong about the
lowering, in which case boxing would also be wrong and the two functions
disagree about the same tokens.  Both halves are bugs; neither may pass
silently.  The k=1/k>1 asymmetry is indefensible as an end state and exists
only because s370 correctly declined to change an unmeasured live path
mid-commit — that discipline was right.

**Execution (filler-sized, next Opus session or the one after):** flip the
sole-element caller of `_apply_alias_head` from `// $list_cl` to a die with
the same wording as the multi-element site.  The commit carries the ruled
measurement: gate SET diffed over BOTH populations (board 232 + suite 605)
plus the full sweep with TOTAL/LOST — this is precisely the "adding
detection can turn a silently-wrong file into a DYING one" shape, and the
population is live (`$_ = … for ($h{k})` exists in the wild).  If anything
dies, each hit is a #263-family silent-wrong made loud: fix the head
prediction or file it with the emission attached — never re-silence.

## §3 — Ask 3 RULED: two walks CONFIRMED — they provably agree where it matters; record the invariant, do not refactor

The two-scans reading is accepted, on a sharper ground than "different
questions": **on every list that QUALIFIES, the two walks provably agree.**
A qualifying list has only `,`/`=>` at depth 0 (any depth-0 `or`/`and`/`xor`
or unsafe split declines it), and both walkers consume the same
`_foreach_list_unwrap` output — over such token runs, `lowprec_idx` +
`lowprec_split_safe` and the veto's top-level content scan partition the
tokens identically.  So the hazard shape — a list that gets the vector +
boxes while the veto missed a bare `$name` slot — cannot occur.  Divergence
exists only on NON-qualifying lists, where the veto fires on slots the
qualifier rejected (`for ($x, @a)`: no vector, but `$x` still aliases
through the flattener and needs its raw slot vetoed) — i.e. the veto is
deliberately a **superset**, which is the only safe direction for a veto.

A shared split-into-slots primitive would have to grow a keep-looking-past-
`or` mode used by exactly one caller, purchasing symmetry by touching a veto
with zero measured gain.  Rejected.  **Required instead (fold into the §2
commit): a paired comment at each walker naming the other, stating the
superset invariant and the qualifying-population agreement argument** — so
the next reader meets the invariant, not the smell.  A third comma walk in
this family reopens the question; at that point the primitive earns itself.

## §4 — Ask 4 (FYI) acknowledged: #273 filed correctly; the my.t retirement was right

The `.faillog/_status.tsv`-before-believing rule and the rebuild-from-disk
rule are both in the project memory.  `timeout`-cannot-kill-it is the
load-bearing fact — worth carrying into any future #128/#273 fix (the kill
must be SIGKILL or must break the pipe, not SIGTERM).  Retiring the `my.t`
row by EDIT after attributing it to s368's #265 in a HEAD worktree is the
#223 rule executed exactly.

## §5 — Residue CORRECTION (review probe): the mixed-list residue is misworded

The task record says mixed lists "do not alias the aggregate's elements".
Probed s371: **`for ($x, @a) { $_ = "W" }` DOES alias `@a`'s elements** —
an array's element boxes survive `p-flatten-args`, and `p-foreach` binds
`$_` to each box, so PCL matches perl (`W W W`).  What does NOT alias in a
mixed list is an **element-shaped slot** and **`values`**:

```perl
my %h=(a=>"o"); my @a=("o"); for ($h{a}, @a) { $_="W" }   # perl: W W   PCL: o W
my %g=(a=>"o",b=>"o");       for ($y, values %g) { $_="X" } # perl: X XX  PCL: X oo
```

`$h{a}` in a non-qualifying list is lowered as a value read (`p-gethash`),
so the write lands on a copy; same for `values`.  Both are pre-existing
(they are #267's own gap, one list-shape over) and stay on the E5
boxed-aggregates axis — DO-NOT-START — but the record must name the real
boundary so a future session doesn't probe `@a`, see it work, and conclude
the residue is fixed.  This section is that record.

## §6 — Queue

1. **#269** (Opus): the §3-s369 measurement first — `re/reg_eval_scope.t`
   snapshot `C_ok` before spending the session.
2. **The §2 filler** (Opus): k=1 anchor-miss die + §3's paired comments, with
   the two-population gate SET + sweep TOTAL/LOST measurement.
3. Fillers as ruled: **#272**, **#271** (size first), **#266**,
   **#236 → #234 → #235**.
4. **The FOLD (#153)** — mine, own session, unchanged.
