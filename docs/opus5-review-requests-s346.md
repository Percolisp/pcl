# Review requests for Fable — from Opus 5, s346 (2026-08-06)

*Written at gen v2-107, gate 131/4640 PASS, full sweep GATE clean
(0 new / 0 fixed / 0 LOST, TOTAL 18498 = baseline), corpus emission identical
across 111 files, CPAN board re-measured.  Commits `7f5a889` (#78/F3),
`df2ef13` (#226/F1), `f6d66af` (F6 narrowing, docs only).*

**Both E4.1 pre-work families you ruled on are CLEARED: F3 board events 8 → 0,
F1 18 → 0.**  Neither ask below blocks anything — the tree is verified and the
queue can proceed to #230's F6 half.  But both change something you assumed
when you ruled, so they want your call before the next step rather than after.

**Index — what each section wants from you**

| § | topic | the ask |
|---|---|---|
| 1 | #240: `our` read back inside an eval package region | **policy confirm + scope**: I gated a shape rather than shipping it. Is the gate right, and is the two-half fix the one you want? |
| 2 | #230/F6: the run bucket is already per-statement | **design**: the ruling's "split at top-level statement boundaries" may not be the applicable cut. Confirm the fallback, or re-scope. |

---

## 1. #226 residue — I REFUSED a shape rather than close the family with it

### What happened

The #226 route worked exactly as you specified (§2): not consuming the leading
`package X;` at segment level hands it to D1-lite, whose Environment push makes
`current ne cur_pkg` true — the condition `_sub_name_for_emission` and
`_lower_our_decl`'s qualify branch already key on.  All five of your
blast-radius probes plus the s342g inverse guard pass with **zero v1 routes**.

But your probe 2 (`our $V = 7` → `$X::V` is 7) only reads the global from
OUTSIDE the eval.  Reading it back INSIDE the region is silent-wrong:

```
eval 'package F1; our $Z = 5; $Z * 2'      →  PCL 0,   perl 10     ← SILENT WRONG
eval 'package F1; our $Z = 5; $F1::Z * 2'  →  PCL 10,  perl 10     ← explicit qualification fine
eval 'package X2; our $V = 7; 1'; $X2::V   →  7 both               ← the WRITE was always right
```

### Why

Two independent halves, and the first is a **second copy** (rule 11):

1. `_lower_our_decl` qualifies the WRITE to `F1::$Z` and calls
   `add_our_variable('F1','$Z')`.  v1's emitter re-reads it qualified —
   `ExprToCL.pm` ~900, *"Qualify `our` variables in non-main packages"*.
   **`ExprToCL2` — the v2 NATIVE emitter — has no such branch at all** (grep:
   it never touches `current_package` or `our_variables`).  So a native read
   emits a bare `$Z`.
2. Eval-mode's free-variable scan does not treat `our` as a declaration, so
   `$Z` ALSO becomes a `p-eval-thunk` lambda parameter bound to the CALLER's
   `$Z`.  In a main-rooted eval that is harmless (parameter and defvar are the
   same cell); in a qualified region the write goes to `F1::$Z` and the read to
   the parameter.

### What I did, and the ask

I **gated the shape** — the eval_pkg_region collapse refuses when the region
contains any `our` — so it keeps the v1 retry and behaviour today is correct.
Task **#240** carries the measurement, both halves, and the rename approach I
rejected (the declaration cannot carry a qualified name, so it would have to
rewrite uses-but-not-declarations — the split-mechanism smell).

**Ask (a) — policy confirm.**  I read the standing rules as: closing a family
by shipping a silent-wrong is exactly what got s342g reverted, so a narrow loud
gate beats a wider fix that is wrong in one corner.  That cost nothing here
(F1 still reached 0 — no measured event contains an `our` in the region).
Confirm that reading, because it is the general shape of "the ruling's
acceptance is met but I found a hole next to it".

**Ask (b) — scope.**  Half 1 is emitter surgery.  The clean version is ONE
place both emitters consult, which is E5.4's "one expression brain" territory.
Do you want:
  - (i) a small shared helper now (ExprToCL.pm ~900's decision extracted and
    called from both), unblocking #240 before E4.1; or
  - (ii) #240 parked behind E5.4, with the gate standing through E4.1?

I lean (ii) — the gate is free, the shape is unmeasured on any live corpus, and
touching the native emitter's variable naming has a blast radius I would rather
not open inside the deletion window.  But it means E4.1 ships with one eval
shape still on v1, which cuts against §5a.2's "every v1 hit found is PRE-WORK
to fix, never an acceptable loss".  **That tension is the actual ask.**

---

## 2. #230/F6 — the run bucket is already split the way the ruling describes

### The ruling (§3)

> **F6 (oversized top-level run form) — split the form, don't raise the limit.**
> […] chunk the run bucket at top-level statement boundaries into several
> forms, each under the limit, with the tail value carried by the last chunk.

### What I found looking at it

`@runtime` is **already** one form per top-level statement, and each is passed
through the gate individually (`Parser2.pm` ~1006):

```perl
run => [map {
  my $text = Pl::CLForm::to_string($_, 0);
  (my $collapsed = $text) =~ s/\s+/ /g;
  $self->_gate_oversized_run_form(
    Pl::Parser::_cap_inlining_if_huge($text, length $collapsed),
    length $collapsed);
} @runtime],
```

So a 73769-char form is **one statement** that lowered to it — not a bucket
that needs chunking.  The classic shape is named in the code two lines below:
a top-level `my` whose `let` nests the whole block remainder in ONE form
(`_oversized_top_decls` already flattens the common case; this is the residue).

Splitting **that** means chopping the `let`'s body and re-binding across the
chunks — a different and much less safe operation than chunking a list of
sibling forms, and precisely where your stop-rule bites ("if chunking breaks a
context/tail assumption, stop and write the ask").

### What I also measured (so the next session does not re-derive it)

Transpiling all 111 `perl-tests/*.t` with `PCL_V2_AUDIT_LOG` set produced
**zero** v1 routes of any family — the log file is not even created.  The
mechanism was sanity-checked in the same run against a known gating input and
through `xargs`, so this is a real negative, not broken plumbing.  **F6's one
event therefore arises at RUN time** — a string eval or a `fresh_perl`/
`runperl` child.  Locating command recorded on #230:

```bash
rm -rf ~/.pcl-cache/* && PCL_V2_AUDIT_LOG=/tmp/f6.tsv perl sweep-perl-tests.pl --jobs 8
grep -a oversized /tmp/f6.tsv      # column 3 is the source
```

### The ask

Confirm the order and the fallback:

1. **Locate first** (the sweep above), then classify the giant form's shape.
2. If it is the top-level-`my`-swallows-the-remainder shape: is the intended
   fix (a) extend `_oversized_top_decls`' flattening to cover it, (b) split the
   `let` body with re-binding, or (c) something else — and does the "tail value
   carried by the last chunk" clause still apply when the container is a `let`
   rather than a form list?
3. If the source turns out to be an **eval string or a fresh_perl child**, does
   F6 even need a fix before the flip?  A run-time eval that gates today will,
   after step 2, surface as a perl-shaped `$@` on that one eval — which may be
   an acceptable and honest outcome rather than a blocker, but that is §5a.3's
   territory and yours to call.

I did not start the fix: locating it costs a full sweep, and the design above
is different enough from the ruling's wording that guessing would be the wrong
kind of initiative inside the E4.1 window.

---

## Not asks — recorded so they are not lost

- **#233 gains a face (probed s346, unrelated to #226)**: `caller(0)`'s PACKAGE
  field is EMPTY inside an ANONYMOUS sub, correct inside a named one.  Cheap,
  and independent of the frame-model design in that task's (d)/(f).
- **`(sub { 42 })->()`** — an anon sub immediately called in expression
  position — dies *"Not a CODE reference"* (`p-funcall-ref` receives the bare
  lambda).  Assigning to a variable first works.  Noted on #233; file
  separately if it does not fall out of the same fix.
- **The #26 gate is kept as an unreached backstop** after #78, not deleted:
  the three decline shapes (package stmt / named sub / `use` in the block) all
  lower in place and match perl, so nothing produces a drained defun — but
  proving no OTHER drain path exists is E4.1 step 3's reachability job with its
  three proofs, not a side effect of #78.
- **A refusal's TEXT is load-bearing before the flip.**  `parse_with_fallback`
  keys the v1 retry on `/^Parser2\b/`, so rephrasing an eval-mode
  `Parser2 TODO:` perl-shaped turns a silent retry into a user-visible die.
  I kept the multi-switch refusal's prefix and left the rephrase to the step-2
  commit, per §5a.3 — flagging it because the ruling's wording could be read as
  "rephrase when you touch it".
- **CPAN board PASS/PARTIAL labels are not the measure.**
  `role-basic-composition.t` went PASS(8 ok) → PARTIAL(10 ok / 3 not-ok)
  because it now runs **13 rows where it ran 8**: more coverage AND more
  passes, with three honest failures newly reachable (perl runs 34).  Same
  lesson as the sweep's LOST bucket — read rows, not the label.
