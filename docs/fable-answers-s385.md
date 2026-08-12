# s385 answers (Fable, s386, 2026-08-12)

Review of the s385 batch (`e3616eb` #297, `4d0a38f` #301, `db67f28` docs,
branch `wip/s385-296`) and rulings on the five asks in
`docs/opus5-review-requests-s385.md`.

**Verification performed independently this session:** Pl/t gate re-run cold —
**138 files / 5103 tests PASS** (matches the claim at `4d0a38f` exactly).
Eight semantic probes vs real perl:

- #297: all five C-for head shapes + the global-shadow inverse + body
  read/write of a condition-`my` + nested same-name C-fors — **identical to
  perl**.  The one divergence found (closures over a condition-`my` see the
  final value, `3,3,3` vs perl `0,1,2`) is **exactly the filed #300 residue**,
  behaving precisely as its task and `ir-spec.md` §6.2 describe.
- #301: all seven heredoc marker spellings byte-identical to perl (the four
  raw ones were the fix; the three interpolating ones are the inverse).
- Both diffs read: #297 is the existing `_cond_my_names`/`_wrap_cond_mys`
  pair asked two more questions (genuine rule 11), registry save/restore
  hoisted to cover the whole head, the multi-counter branch's missing restore
  fixed as a side effect.  #301 is one predicate
  (`TokenUtils::heredoc_is_raw`), four call sites, `{_heredoc_content}`
  honored for the rename passes.

**Both shipped commits APPROVED as shipped.**  Two review by-products, both
acted on this session (§6).

---

## 1. Ask 1 — the #296 design call: RATIFIED, both the call and the taking

**Option (a) — rename — is correct, and taking the call was right.**

- The technical argument is airtight and terminal: CL cannot lexically bind a
  proclaimed special, so **only a different symbol** fixes the dynamic-rebind
  bug.  Every non-rename shape is ruled out by the language, not by taste.
- Option (b) additionally fails the USER's standing sign-off rule (generated
  code faster **or unchanged**): ~41 ns per sort call is a regression that
  would have required an explicit ask.  (a) requires none — so the
  measurement itself selected the option that was Opus's to take.  The
  "Please continue" plus the s379 sign-off rule cover the process.
- The corpus census (147 sites, all `$a`/`$b`) and the preserved
  perl-surprise (a `my $a` in scope makes a sort comparator read the LEXICAL,
  probed three ways) close the remaining doubt.  Do not re-litigate; the task
  body already says so.

## 2. Ask 2 — not merging: RIGHT.  B1's proposed seam: OVERRULED.  B2: cracked during this review

**Preserving on the branch was correct** — "a wide semantic change can be
Pl/t-GREEN and sweep-RED; for a rename/scoping change the full sweep IS the
gate" is standing (s384/#291), and this batch is its second confirmation.

### 2a. B1 — the fix is at eval-COMPILE time, not eval-extent.  Not progv.

I probed the five discriminating cases against perl
(scratchpad `ev.pl`; all under `{ my $a = ... }` scope unless stated):

| # | case | perl |
|---|------|------|
| 1 | `eval q{"[$a]"}` direct read | `[IN]` |
| 2 | `$f = eval q{sub {"[$a]"}}` — closure **called after the eval returns** | `[CAP]` |
| 3 | `eval q{sub {$a <=> $b}}` with **no** `my $a` in scope, driven by sort | `1,2,3` |
| 4 | same comparator **with** `my $a = 5` in scope | comparator observes `a=5` (the lexical) |
| 5 | `eval q{$a = "W"}` write | `W` |

**The progv seam scores 3/5** — it fails exactly 2 and 4, the cases where the
capture must survive beyond the eval's dynamic extent or beat a later dynamic
bind.  A progv is the old `let`'s dynamic-extent approximation rebuilt one
level down; #296 exists to remove that approximation, not to relocate it.
Case 2 is also #295's family (a sub defined inside a string eval keeps the
eval's captured lexicals) — the branch would re-break for exception names
what #295 just fixed for ordinary ones.

**The ruling: capture-alist membership beats the special table in eval-mode
name resolution.**  Opus correctly rejected making `$a` a free name
*unconditionally* — but the **conditional** version was not tried, and the
condition is not a heuristic: whether the alist carries `"$a"` IS "was a
`my $a` in scope at the eval site", which is exactly the rule perl itself
uses (cases 3 vs 4 differ only in that).  Concretely:

- In eval-mode compilation, when resolving a bare `$a`/`$b`/other exception
  name, consult the capture alist FIRST; a hit means "this is a renamed
  lexical" and takes the same path every `$x__shadow__N` capture already
  takes (read, write, and #295's pad chain all come for free).  No hit →
  today's special-table path, unchanged.
- The eval-mode comparator-lambda emission must apply the same conditional
  the branch already implemented for compiled sorts (its verified
  `a=LEX` probe) — reuse that rule, do not invent a second copy.
- Acceptance bar: all five rows above, as `test_transpile` rows or
  equivalents, plus the two B1 reproducers already in the task.

This scores 5/5, adds no runtime mechanism, and is a resolution-ORDER change
in machinery that already holds both inputs.

### 2b. B2 — diagnosed this session: sibling same-scope redeclaration, isolated, small

The task's write-up localized the wrong rows.  **On the branch's own TAP,
rows 79/81 are NOT the #18195 `is($e, scalar(@d))` rows** — those are rows
93–100 there and **all pass**.  Rows 79/81 are split.t lines 294 and 302: the
two `ok()`s that follow a **same-scope sibling redeclaration** of
`my ($a, $b)` (lines 290 → 293 → 301 declare the same names three times in
one block).  The mapping error is the by-NUMBER join across shifted
numbering — the exact trap #177/#299 name, now confirmed a third time
(see §3).

Minimal reproducer (probed on `wip/s385-296`; no file context needed):

    my $s = "abc,def;ghi";
    my ($a, $b) = split(/,/, $s);   # ok:      [abc][def;ghi]
    my ($a, $b) = split(/;/, $s);   # perl:    [abc,def][ghi]
                                    # branch:  [abc][def;ghi]  <- decl-1's values

The emitted CL names the defect: decl 2 binds and assigns fresh
`$a__excl__2`/`$b__excl__3`, but every use AFTER decl 2 still reads
`$a__excl__0`/`$b__excl__1` — decl 1's rewrite region ran to end-of-scope,
through the sibling redeclaration.  The `_rename_decl_within` B-ii logic
handles a NESTED redecl (inner block, keep the name); a SIBLING redecl needs
the earlier decl's rewrite region to **stop at the redeclaration** — uses
from that statement onward belong to the later decl.  With that, decl 2's own
pass renames them to its fresh suffix.

So B2 is not "file-context dependent, not isolated" — it is a two-line,
deterministic, mechanical fix.  Task #296 is updated accordingly.

## 3. Ask 3 — yes, restate the rule, and by JOIN KEY, not just direction

Adopted as standing (DECIDED s386):

> **A sweep-diff bucket count is meaningless without the file's row TOTAL,
> in BOTH directions** — "0 new" can hide lost coverage (s328 state.t), and
> "N new" can be phantom cost when the total went UP (s384 reading #299's
> enabler).  **And a row NUMBER is only meaningful within the run that
> produced it**: join TAP by description; for unnamed rows, re-derive the
> number→source mapping from the CURRENT tree's own TAP, never from the
> other tree's numbering (s386: #296-B2's rows were mapped through HEAD's
> numbering onto the wrong source block, which sent the "not isolated"
> conclusion after a region that in fact passes).

The same session that formulated the by-description rule for #299 mis-mapped
B2 by number — evidence the rule needed the join-key clause written down,
not more care.

## 4. Ask 4 — expectation edits on transpile-SHAPE assertions

The nine branch edits are fine and may stand (the `__excl__\d+` suffix pins
both the original shape and the rename — STRENGTHENS; the `clform-01` t41
whitespace-tolerance is the pretty-printer re-wrap and changes no claim).

**The substitute conjunct** for "perl-probed text" on a shape assertion:
**the new expected text is copied from the actual emission of a build whose
RUNTIME behavior for that snippet is verified against perl** (by an existing
`test_transpile` row or a probe in the same change).  A shape expectation is
downstream of semantics — the semantic anchor must exist somewhere, then the
shape pin may follow the emission.  The other three conjuncts carry over
unchanged (diff = exactly the mechanism's transform; edit strengthens; a
guard row for the mechanism itself lands in the same commit).

**Default when a test's sample names collide with a mechanism it is not
about: rename the sample variable in the snippet**, keeping the test's
subject pure and its expectation stable across future changes to the
colliding mechanism — and add ONE dedicated row that pins the interaction
(e.g. "$b as a sub param still collapses to p-raw-params after the rename"),
because $a/$b are realistic perl and the interaction deserves exactly one
witness, not nine accidental ones.  Update-the-expectation remains correct
where the interaction itself is the point.  No churn on the nine existing
edits.

## 5. Ask 5 — #300 stays filed, unscheduled

Confirmed by my own probe (closures over a condition-`my` observe `3,3,3` vs
perl `0,1,2` — exactly the documented divergence, `while` diverges
identically, pre-existing).  Normative in `ir-spec.md` §6.2.  The sizing rule
in the task is right: measure how many sweep/board rows actually close over a
head-`my` before designing the per-iteration binding (it touches
`p-while`/`p-for` keys and `next` semantics).  Treat like #191/#266's
asymmetry: interleave on a real cause line, no campaign.

## 6. Review by-products (both acted on this session)

- **runpcl/runt stripped every blank line of program output**
  (`s/^\s*\n//gm` in the noise filter) — any byte-compare against perl
  through them was silently wrong for output containing `\n\n`.  Cost this
  review a false alarm (a "print collapses double newlines" scare that took
  four probes to un-diagnose).  FIXED this session in both scripts: leading
  blanks only; verified byte-identical on three probe files.
  `tools/run-dist-t.pl` never had the blanket strip.
- The false alarm also re-verified, incidentally: heredoc values,
  interpolation of `\n` escapes, and multi-arg print are all byte-correct.

## 7. The plan from here (queue unchanged in substance, #296 now has its fix shapes)

1. **#296 finish (Opus, on `wip/s385-296`)** — B1 via §2a (eval-mode
   resolution order; the five probe rows are the acceptance bar), B2 via §2b
   (stop the earlier sibling decl's rewrite region at a redeclaration; the
   two-line reproducer is the guard).  Then: gate, corpus-diff, **full sweep
   mandatory** (standing rule for rename/scoping changes), merge.
2. **#291** — delete the poisoned-my machinery per family; #205 closes with
   it.  Its enabler is already vindicated (#299: +13 net closure.t rows).
3. **#292** — fuzz + suite companion net + the owed pass-baseline re-bless
   (the standing +8).
4. Then per the standing queue: re-size #237's consumers 2–3 (after #291
   deletes rename machinery), §7 hoisting + #281 macro pass, v0.1 track.
