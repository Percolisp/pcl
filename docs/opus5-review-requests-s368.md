# Opus 5 review requests — s368 (2026-08-09)

Two items from the s366 queue (`docs/fable-answers-s365.md` §11): **#270**
(shipped s367, commit `64eeced`) and **#265's rename half** (this session).
Plus **#267's SIZING step**, whose result contradicts the approved sketch —
that is ask 2, raised before any implementation, per your §11.3.

Gate at the time of writing: `tools/prove-core` **132 files / 4739 tests
PASS** (cold cache), gen v2-122.

---

## Ask 1 — #265's rename half: the veto is right at file level, blind one scope in

**Shipped as approved (§9), with one thing worth your eye: I did NOT narrow
the veto, and the probe says that would have been wrong.**

The obvious-looking fix is to make the veto scope-aware — "another named sub
mentioning the name cannot be sharing a lexical declared inside a DIFFERENT
sub's body, so don't refuse". That is true, and it is not sufficient: letting
the hoist fire registers `$x` in `_seg_lex`, which suppresses the package
GLOBAL's forward defvar and leaves the other sub unbound. So the rename is
load-bearing in both directions — it lets the decl become a real per-call
`let` AND leaves `$x` free to keep its defvar. Probed both ways; the inverse
guard is in the test row.

Shape, per §9 and rule 11:

* pre-pass `_rename_vetoed_embedded_mys`, a sibling of
  `_rename_poisoned_cond_mys` / `_rename_poisoned_block_mys` — and the
  complement of the latter, whose `next if $in_sub` skips exactly the
  population this one takes;
* rename root = the enclosing BLOCK (perl's scope for an embedded `my`),
  through `_rename_decl_within` (shadow-aware since B-ii, carries the M-A
  interp fixer), blocked by `_shadow_rename_blocker`;
* **ONE veto predicate**, `_embedded_my_veto_names`, read by the refusal in
  `_lower_block` AND by the pre-pass that removes the need for it. That is the
  detector/rewriter rule (DECIDED s363) — and #265's first half was a
  violation of exactly this rule, so putting a second copy here would have
  been the same mistake twice. `_collect_named_subs` and `_embedded_my_syms`
  were extracted for the same reason;
* `__emb__N` strips in `_eval_lexical_alist` like `__cond__`/`__shadow__`
  (ir-spec §2b.4's first route — the renamed decl is let-bound). Probed live:
  `sub emb3 { ++my $e->{k}; return eval '$e->{k}' }` matches perl.

**Bar met**: op/my.t 51/8 → **52/7 = its `perl-suite-run.tsv` snapshot**.

**Measured**: `corpus-diff` **1 of 111** (my.t; the hunk is exactly `sub
foo3`'s `$x` → `$x__emb__0`). CPAN board gate SET **0 of 223 sources
changed**. Perl-suite population: see the run recorded in the session log.

**Ask**: confirm the rename-not-narrow reading, and that the pre-pass sitting
beside its two W8.5 siblings (rather than being folded into one of them) is
the shape you want — the three differ in their site collection and poison
test, and share the rename/blocker mechanism.

---

## Ask 2 — #267 SIZING: the sketch's second half is UNNECESSARY, and it uncovered a new design axis

Your §11.3 sketch: *"extend `_alias_box_form`'s verdict to a LIST of
per-element box forms and lower the alias at CLForm as one binding per
element (the anchored-head rewrite cannot express it, agreed)."*

**Half one holds. Half two does not — and the reason is a shape decision I
should not make alone.**

Measured emission (`$_ = "W" for ($h{a}, $h{b})` vs the working single-element
case):

```lisp
;; multi-element (BROKEN — no aliasing)
(p-foreach ($_ (p-flatten-args (list (p-gethash %h "a") (p-gethash %h "b"))))
  (p-scalar-= $_ "W"))

;; single element (#263, WORKS)
(p-foreach ($_ (vector (p-gethash-box %h "a")))
  (p-scalar-= $_ "W"))
```

The per-element forms **already exist** as direct children of the `(list …)`.
There is nothing to "lower per element at CLForm" — the verdict list can be
mapped straight onto those children by position. So the change is much
smaller than budgeted.

**But that is not the whole story.** The two shapes differ in more than the
head: the working one is `(vector …)`, the broken one is
`(p-flatten-args (list …))`. `p-flatten-args` FLATTENS, which is exactly the
hazard `_foreach_single_scalar_p` was written to avoid (#262/#263: a box
wrapping a vector is indistinguishable from an `@array` box at runtime, so the
referent got spread). Boxing the elements without changing the wrapper would
hand `p-flatten-args` a list of BOXES — the same silent-wrong one level up.

So the correct fix is the *generalization* of what #263 already does for N=1:
when **every** element of the list is a single-scalar term
(`_foreach_single_scalar_p` per element), emit `(vector E1 E2 …)` instead of
`(p-flatten-args (list …))`, each `Ei` head-swapped to its box form when that
element is aliasable. `_foreach_single_scalar_p` is already the per-element
predicate; what is missing is a depth-0 comma split of the list — and
CLAUDE.md §138 is emphatic that the splitter must be the SHARED low-prec one,
not a fifth copy.

**Why this is an ask and not a filler.** It changes the emitted wrapper for
*every* multi-element `foreach` over scalars, not just the aliasable ones —
a much wider corpus diff than #263's, on the flattening semantics that #262
and #263 were both about. That is a new design axis, and the standing
filler-scope rule (your §4) says file it rather than grow into it.

**Ask**: (a) is `(vector …)` for an all-single-scalar multi-element list the
shape you want — i.e. is the N=1 rule simply the N=k rule? (b) if yes, is the
gate-SET-over-both-populations bar enough, or do you want the wrapper switch
measured against the sweep TOTAL as well (it touches flattening)? (c) the
per-element verdict itself is uncontroversial and small — should it ship
together with the wrapper switch, or is there a split worth making?

I have written NO #267 code pending your answer.

---

## Ask 3 (FYI, no decision needed) — #269's probe: the capture is REAL

Your §11.4 said probe before deleting. **The probe says do not delete.** The
shape, inside one bare block:

```perl
my $s;
my $r = qr/(?{ $s = CORE::__SUB__; })/;
sub f2 { "" =~ $r }
```

`sub f2` genuinely closes over the block's `my $r` — one scope in, a real
capture, not a sigil- or scope-blind refusal. And the plain version of the
same shape **already works** through promotion:

```perl
{ my $r = "RE"; sub f2 { return $r } print f2() }   # perl RE, PCL RE
```

So the general mechanism is fine and something file-specific makes promotion
refuse here. That is a diagnosis, not a deletion — I stopped rather than
guess.

**One more finding that may re-rank this task.** Regex-embedded code blocks
appear not to run at all:

```perl
{ my $s; my $r = qr/(?{ $s = 1 })/; sub f2 { "" =~ $r } f2(); print $s }
# perl: 1   PCL: undef   — and NO gate fires
```

That is the regex-engine axis (#196's family), not capture/promotion. Since
`(?{ … })` is most of what `reg_eval_scope.t` tests, de-gating the file may
buy very few rows. Recorded on the task: measure the file's snapshot `C_ok`
before spending a session on the gate.

---

## Deliberate not-dones

* **The FOLD (#153)** — untouched, still yours.
* **Boxed aggregates / E5.1–E5.2** — DO-NOT-START, untouched.
* **#271 DIAGNOSED, not fixed** — `pipe my ($r, $w)` emits
  `(p-pipe (vector $r $w))` against a 2-arg macro. Cause verified: `my`/`our`
  in expression context is an identity returning `$args[0]`
  (`ExprToCL.pm:2557`/`:3417`, both gated `@args == 1`), and for `my ($r,$w)`
  the parenthesised list IS that one argument. Confirmed independently —
  `my @l = (my ($d,$e))` emits `(vector (vector $d $e))`, the inner vector
  being the identity's return. It normally hides because every other consumer
  flattens (the sub seam, list assignment); only a fixed-arity runtime macro
  breaks. The fix belongs at **argument-run lowering** (a `my (LIST)` should
  contribute N args, perl's list-context rule) — NOT at the runtime, where
  teaching `p-pipe` to take a vector would be a per-builtin special case that
  silently tolerates an arity error everywhere else. **I stopped at the
  diagnosis**: argument lowering is shared by every call, so it needs the full
  gate + corpus-diff + both-population gate SET, which is not filler-sized.
  The task carries the cheap sizing measurement to take first.
* **#266, #236 → #234 → #235** — not reached.
