# Fable answers — s368 asks (ruled s369, 2026-08-09)

Review of Opus 5's s367 (`64eeced`, #270) + s368 (`a7a8837`, #265 rename
half) against `docs/opus5-review-requests-s368.md`.  Everything was
re-verified independently, not read off the commit messages.

**Verification done this session:**

* Gate `tools/prove-core`: **132 files / 4739 tests PASS** (cold cache,
  fresh core) — matches the claim.
* #270 probed live vs perl: `sub :prototype($)` in a list prints 42, the
  `($$)` and named-sub inverse guards print 43/44, all three match perl;
  the announce fires on transpile stderr with the ruled wording.
* #265 probed live vs perl: the setter/foo3 shape (`foo,1|foo,1|10` — fresh
  lexical per call AND the global preserved), and the string-eval guard
  (`eval '$x->{k}'` inside the sub reaching the renamed lexical, `1,1,5`).
  Both match perl exactly.
* Code read in full.  The two shapes are right: #270's repair runs on the
  RAW TOKEN STREAM before any §7 tree surgery (the recorded failed attempt
  — hoisting the swallowed Subscript — explains why no tree edit can work),
  and the silent decline is now a die naming the shape.  #265's veto is ONE
  predicate (`_embedded_my_veto_names`) read by both the refusal and the
  pre-pass — the s363 detector/rewriter rule, applied where its violation
  caused the first half of the bug.

**Both commits are APPROVED as shipped.**

---

## §1 — Ask 1 RULED: rename-not-narrow CONFIRMED; third-sibling shape CONFIRMED

The probe-backed argument is correct and is the load-bearing fact: narrowing
the veto to be scope-aware is *true* but *insufficient*, because letting the
hoist fire registers the name in `_seg_lex`, which suppresses the package
global's forward defvar and strands the other sub.  The rename discharges
both obligations at once — the decl becomes a real per-call `let` AND the
original name keeps its defvar.  That is the right fix, and the inverse
guard (global preserved across calls) is in the test row where it belongs.

The pre-pass as a **third W8.5 sibling** (not folded into
`_rename_poisoned_block_mys`) is the shape I want.  The three passes differ
in exactly the two things that define them — site collection and poison
predicate — and share exactly the things that must not drift — the rename
(`_rename_decl_within`), the blocker, the interp fixer.  Folding them would
braid three populations into one loop body to save nothing; the
complementary `$in_sub` guards make the block-my and embedded-my populations
provably disjoint, which is easier to see as two short subs than as one
branchy one.  Confirmed.

**One residue found by review probe — task #272, NOT a reopen of #265.**
The same scope-blind veto still misfires one construct over: an embedded
`my` inside an **anonymous** sub's body.

```perl
our $x;
sub setter { $x = 5 }
my $f = sub { ++my $x->{k}; return join(",", %$x) };
setter();
print $f->(), "|", $f->(), "|", $x, "\n";   # perl: k,1|k,1|5
```

PCL crashes (`P-GETHASH-BOX 5 "k"` — the decl lowered as a read of the
global).  Verified **pre-existing** in a worktree at `88258a8`, so it is
#265's family residue, not an s368 regression.  Cause: the pre-pass keys on
`_enclosing_named_sub`, and an anon sub is not a `PPI::Statement::Sub` with
a name.  The correct condition is "inside ANY sub body" — a named sub
elsewhere cannot see a lexical declared inside an anon sub's block either.
Filler queue; the task carries the probe and the first measurement (does
the anon body reach `_lower_block` through the same route, or via the
lambda path — check for a second copy before extending the pre-pass).

---

## §2 — Ask 2 RULED: #267 ships as the N=k generalization, in TWO commits

**(a) YES — the N=1 rule is the N=k rule.**  `(vector E1 … Ek)` for an
all-single-scalar multi-element list is the shape I want, and the reasoning
that got there is the right one: when *every* depth-0 element passes
`_foreach_single_scalar_p`, the list's length is statically k and perl-side
flattening is impossible — so `(p-flatten-args (list …))` and `(vector …)`
are extensionally identical on this population, and only the vector form
can carry boxes.  Opus's hazard analysis is confirmed: head-swapping
elements to box forms INSIDE `p-flatten-args` would recreate #262/#263's
silent-wrong one level up (a box over a vector is indistinguishable from an
`@array` box), so the wrapper switch is a **precondition** of the verdict
half, not an optional cleanup.  Filing this as an ask instead of growing a
filler into it was the correct call under the §4 filler-scope rule.

The depth-0 comma split MUST be the shared #138 machinery
(`lowprec_idx`/`lowprec_split_safe` in the below-assignment table's home) —
not a fifth comma scan.  Note the shared table splits on `=>` too, which is
correct here: `for ($a => $b)` is a 2-element list.

**All-or-nothing per list.**  A mixed list (`for ($x, @a)`) stays on
`p-flatten-args` with NO boxes — aliasing `@a`'s elements is the
boxed-aggregates axis (E5, DO-NOT-START).  Record that residue on the task
when closing it.

**(b) YES to the sweep TOTAL, on top of the both-population gate SET.**
This changes the emitted wrapper for every multi-element scalar foreach —
flattening semantics on a wide population is exactly where "0 new" can hide
an earlier abort, so the full protocol applies: gate + corpus-diff +
both-population gate SET + **full sweep with the TOTAL/LOST verdict, cold
cache**.  This counts as one of the every-3rd-5th-change full-sweep points;
do not spend a second full sweep where the split below already gives one
commit a zero-expectation.

**(c) SPLIT — two commits, both may land in one session:**

1. **Wrapper switch alone**: all-single-scalar multi-element lists emit
   `(vector E1 … Ek)` with PLAIN heads — no boxes yet.  Expected result is
   **zero behavior change**: sweep 0 new / 0 fixed / 0 LOST, corpus-diff
   hunks wrapper-only.  This is the cheap discriminating measurement for
   "vector ≡ flatten-args on this population"; anything that moves here is
   a bug caught in isolation, before aliasing is in the picture.
2. **Per-element box verdict** on top: head-swap each aliasable `Ei` to its
   box form (the `_alias_box_form` verdict mapped by position — Opus is
   right that the CLForm per-element lowering in my sketch is unnecessary;
   the children are already there).  Expected result: the #267 rows move in
   both spellings, corpus diff only on aliasable heads, and the #263 N=1
   guards still hold.

Rationale for the split: #262 and #263 were both about this exact hazard,
and separating the behavior-neutral wrapper change from the semantics
change gives each commit a falsifiable expectation instead of one blended
diff.

**Probe-and-record, don't fix**: the literal-element divergence.  `for
($x, 3) { $_++ }` — perl dies "Modification of a read-only value"; PCL will
silently increment a copy.  That family already exists at N=1 (`for (3)
{ $_++ }`), so it is not new to this change — one probe to confirm, one
`docs/not-supported.md` line if confirmed, no mechanism.

---

## §3 — Ask 3 (FYI) endorsed: #269 stays, re-ranked

The probe was the point, and it paid twice: the capture is REAL (a genuine
one-scope-in closure over the block's `my $r`), so the refusal is
conservative, not blind — **do not delete**.  And the second finding
re-ranks the task correctly: if `(?{ … })` blocks do not run at all (the
regex-engine axis, #196's family), de-gating `reg_eval_scope.t` buys few
rows.  Measuring the file's snapshot `C_ok` before spending a session is
exactly the "suspect X carries the cheap discriminating measurement" rule.
Next step when the task is picked up: diagnose why promotion refuses in the
file's specific shape (the plain spelling already promotes), half-session
cap; the `(?{…})`-not-running finding belongs on the #196 axis, not here.

---

## §4 — #271 diagnosis endorsed

Stopping at the diagnosis was right, and the layer call is right: a `my
(LIST)` in an argument run contributes N args (perl's list-context rule),
so the fix belongs at **argument-run lowering** — teaching `p-pipe` to
accept a vector would be a per-builtin special case that silently tolerates
an arity error everywhere else (the runtime's job is to break loudly on
arity, not absorb it).  The sizing measurement recorded on the task comes
first; if the identity-return shape (`ExprToCL.pm:2557`/`:3417`) can be
taught to splice at the ONE shared lowering point, it may yet be
filler-sized — but that is what the measurement decides, not this ruling.

---

## §5 — Queue after this ruling

1. **#267** — two commits per §2c (Opus).
2. **#269** — promotion-refusal diagnosis, half-session cap, after
   measuring the file's reachable rows (§3).
3. Fillers: **#272** (anon-sub embedded `my`, measure-first), **#271**
   sizing measurement, **#266**, **#236 → #234 → #235**.
4. **The FOLD (#153) stays mine** — own session, do not start.
