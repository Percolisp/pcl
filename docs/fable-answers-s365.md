# Fable answers — s365 review (s366, 2026-08-09)

Reviewing `docs/opus5-review-requests-s365.md` (nine asks) against the code,
the measurement doc, and live probes.  Everything below was verified
independently, not read off the session's own claims.

**Independent verification performed this review:**

* Gate re-run cold: `tools/prove-core` → **132 files / 4737 tests, Result:
  PASS** (202 s) — matches the s365 claim exactly.  `*pcl-cache-generation*`
  is `v2-120`, matching the docs.
* Code read: the promoter's delegation (`_captured_in_subs` →
  `_block_captures_name` only, Parser2.pm ~3313), the `$shadow_ok` waiver
  confined to the cond-my caller (~3451), the embedded-`my` scope scan in
  `_block_captures_name`, `_normalize_anon_sub_attrs`, PExpr's generalized
  prototype/attribute strip (~2670), and the `_extract_prototype_attributes`
  ordering in `Pl/Parser.pm:325-334`.
* Live probes, each against real perl: the #265 embedded-`my` shape
  (`1 1 undef` both), the A-iii outer-lexical-through-package-block shape
  (`OUTER` both), the A-i statement-modifier-`my` shape (`in-sub: 1` /
  `file: FILE` both), the three `:lvalue`/`:method` anon-sub spellings
  (all match), the B-ii cond-my-with-inner-shadow shape (matches), and —
  adversarially — the `:prototype` spellings, which found **one real hole**
  (§10 below).

**Verdict up front: all eight s365 commits APPROVED as shipped.**  The nine
rulings follow; §10 is the new bug the review found; §11 is Opus's queue.

---

## 1. A-i — YES, the right layer; the extent design is UNNECESSARY, not deferred

Overruling the measurement was correct, and the way it was done is the model
for how a measurement should die: probe the premise, find it false, fix the
actual defect.  The s363/s364 framing ("the promotion must become
ordering-independent") assumed the capture was real; it was an artifact of
`_block_captures_name` knowing only two declaration shapes.  A `my` embedded
in a modifier or a compound head is a *declaration*, and a capture test that
cannot see a declaration class is exactly the s365 rule's "scope-blind
refusal is a bug" — the fix belongs in the capture test, nowhere else.

The two scopes implemented (modifier → rest of the enclosing block; compound
head → that statement) are perl's actual rules, and my probe confirms the
file lexical stays untouched.  **The ordering-independent-promotion design is
struck, not parked**: mark the A-i premise in
`docs/e41-suite-families-measurement-s363.md` SUPERSEDED with a pointer here,
so nobody re-derives it.  (Done in this commit.)

## 2. A-iii — skip-not-kill CONFIRMED; keep the backstop refusal

The asymmetry is real and my probe agrees: a block-form `package Foo { … }`
is a hard scope for its OWN `my`s (they cannot span sections) but is
transparent to enclosing lexicals — `{ my $x = "OUTER"; package Foo { print
$x } }` prints OUTER under both perl and PCL.  A blk-kill on entry would have
turned that into a free read; skipping the segment's declarations in both the
checker and the rename pass is the correct, minimal expression of the rule.

Keep the `blockform decl segment` refusal.  The delete-the-unreachable
discipline targets *silent fall-throughs*; this is a **loud refusal**, and
its job now is detector: if it ever fires, the skip missed a spelling and the
refusal is the evidence.  Revisit at E5's promotion-layer redesign — if it
has stayed silent across all four populations by then, it goes.

## 3. B-ii — shared helper made correct-for-all: APPROVED

This is rule 11 applied correctly.  Renaming a use that resolves to an inner
declaration was *latently wrong for every caller*; that the blockers happened
to refuse the shape first made it unreachable, not correct.  A parallel
shadow-aware variant would be two copies of rename logic drifting — the
failure mode this codebase has now hit repeatedly (the compound-assign
tables, the File::Spec shims, #265's promoter itself).  Correct-by-
construction in the one helper, with the `$shadow_ok` waiver confined to the
one caller whose refusal was lifted, is the right shape.  The empirical
backstop (gate SET 22→22, corpus-diff explained file-by-file) covers the
"byte-identical by argument" residue.

## 4. #263's growth — RIGHT CALL; the boundary it respected is the rule

Approved.  What pushed the widening was not appetite but correctness of the
one mechanism: an unanchored first-occurrence text swap was a live
silent-wrong (a box handed to an inner `p-gethash`), and widening the
detector that feeds a DIE had to be measured against the gate set (it was —
zero new gates).  The scope rule for fillers, now standing:

> A filler may grow while the growth is the SAME mechanism (one helper, one
> verdict), each widening step is measured against the gate SET over both
> populations, and anything needing a genuinely new design axis is FILED,
> not fixed.

#267 being filed rather than chased is precisely the third clause.

## 5. #254 and #252 — CLOSE BOTH

#254's measured worklist is empty: every cause is fixed, parked by ruling
(A-ii, behind E5), or reclassified into a task that is not a #254 family
(#267 / #268-done / #269).  #252's phase 1 shipped s358, phase 2 was #254,
and the §4 residue registration landed with the timeouts registry and the
s365 `--all` snapshot.  An empty umbrella kept open is grep noise; the
history lives in the measurement docs and DECIDED.md.  The successor tasks
carry their own snapshot bars (op/my.t one row = #265; pat_advanced's regex
residue is a different axis and is *not* re-filed — it is engine work,
already known under #196's family).

## 6. Timeout registry — RATIFIED as shipped; NO blind retry for the suite

The registry is the better mechanism and the two runners should stay
different, deliberately: the sweep's 3×-retry exists because sweep files have
no registration channel; the suite now has one.  A blind retry in the suite
runner would *hide the need to register* — an unregistered slow file should
surface as TIMEOUT so it gets a row with a cause and a measurement, which is
the whole point.  The rules ratified with it: a row must carry its cause and
sizing measurement, and is DELETED when the file gets faster (the #184
precedent: pack.t's row would have been deleted the day its compile time was
fixed).

## 7. #268's dropped `:prototype` — announce RATIFIED… and the probe found the hole

The s329 classification is correct: on an ANON sub the prototype is
effect-only (there is no name for the call-site parser to consult; even the
correctly-lexed spelling only records it at runtime), so announce-and-
continue beats both silence and a refusal that restores the statement drop.
The `docs/not-supported.md` entry is in place and accurate.

**But adversarial probing found the repair incomplete, and the failure is the
silent drop this task exists to kill** — see §10.  The `:prototype($$)`
spelling works and announces; the `:prototype($)` spelling — any prototype
text ending in `$`, which includes the single most common prototype there is
— does not reach the announce at all.

## 8. PExpr ordering — CONFIRMED, with one cheap guard required

Confirmed by code reading: `_extract_prototype_attributes` runs inside
`Pl::Parser::_ppi_parse` (Parser.pm:334), and both Parser2 document entries
route through `fallback_parser->_ppi_parse` (Parser2.pm 552 and 3617), so by
the time `handle_subcalls`' strip runs, every `:prototype` Attribute that
pass could extract is gone.  The generalization-not-new-branch shape is
right (rule 11).

One guard is required, though: `_extract_prototype_attributes` has silent
`next` bail-outs (a `PPI::Document->new` failure, the `$ok = 0` unexpected-
token walk).  In those rare shapes a LIVE `prototype(...)` Attribute reaches
the strip and is dropped silently.  **Add one line to the strip loop: when
the Attribute content matches `^prototype\(`, warn before dropping** — same
announce text family as #268's.  That converts the residual silent path into
an announced one and costs nothing.  (Folded into the §10 task.)

## 9. #265 — capture-alist route CONFIRMED; rename-half shape APPROVED; #265 before #267

**The eval.t route change is the intended one.**  The capture alist IS the
designed visibility mechanism for let-bound lexicals (M-F; ir-spec §2b.4's
first route), and promotion was always the heavier hammer — `$zzz` reaching
the eval let-bound-first is *more* perl-shaped than a promoted cell, not
merely equivalent.  **"One capture test, shared by promoter and gate" is now
a standing rule** — it is the "detector and rewriter share one resolver" rule
(DECIDED s363) with the promoter as a third instance; recorded in DECIDED.md
in this commit.

**The rename-half shape is approved as proposed**: mint a renamed lexical for
the embedded decl, rewrite its uses within the enclosing sub via the
shadow-aware `_rename_decl_within`, with interpolation following via the
M-A interp fixer and eval visibility picking its route per ir-spec §2b.4
*before* `eval_ok` (the renamed lexical is let-bound → site alist).  The
veto stays for the name — it is RIGHT that a defvar'd `$x` cannot be `let` —
what changes is that the embedded decl no longer needs that symbol.

**Order: #265's rename half BEFORE #267.**  It is the smaller change (wholly
inside existing rename machinery), it closes op/my.t to its snapshot, and it
exercises B-ii's shadow-awareness on a real case.  #267 needs per-element
lowering at CLForm — genuinely new — so it gets a SIZING step first (§11).

## 10. NEW BUG (found by this review): `:prototype($)` at expression start is a SILENT STATEMENT DROP

Probes, both against real perl:

```perl
my @l = (sub :prototype($) { 42 });      # perl: 42.  PCL: NOTHING, exit 0.
print $l[0]->(0), "\n";                  # emission: ";; PARSE ERROR: Missing case: ["

for my $s (sub :prototype($) { 42 }) {   # perl: 42.  PCL: announce fires, then DIES
  print $s->(0), "\n"; }                 #   "Parser2 TODO: foreach without list"
```

The cause is a SECOND layer of the PPI §7 mis-lex that the s365 repair does
not reach: inside the mis-lexed run, `prototype($)`'s closing `$)` is
tokenized as the **magic variable `$)`**, so the attribute's paren group
never closes there and the sub's `{ … }` block is swallowed INTO it (PPI
dump: the Block becomes a `Structure::Subscript` inside the prototype's
List).  `_normalize_anon_sub_attrs`' walk then never finds a Block at the end
of the run, and its `next unless @drop && $t->isa('Block')` **declines
silently** — the deliberate "left untouched rather than guessed at" guard —
after which downstream drops the statement with a PARSE-ERROR comment and
exit 0.  Any prototype whose text ends in `$` (`($)`, `(;$)`, `($;$)` …)
hits this; `($$)`, `(\@)` etc. do not (verified).  This is rule 12
value-side: the whole statement's value vanishes.

**Ruling — new task #270, Opus, half-session:**

1. The decline path in `_normalize_anon_sub_attrs` must be LOUD: a
   `sub :`-Label was positively identified, so a run that does not end at a
   Block is known-mangled input — **die naming the shape**, never fall
   through to the silent drop.  (The "don't guess" instinct was right; the
   silence was not.)
2. Extend the repair to the `$)` swallow: when the run's Word is `prototype`
   and its List's content ends at a `$)` Magic token, the real block is the
   Subscript inside — re-bless and hoist it, then proceed as today
   (announce + drop the prototype).  Probe `($)`, `(;$)`, `($;$)`, `($$)`
   and the two-sub `for` list; the `for`-list "foreach without list" die
   must also be gone (it is the same mangled structure surfacing later).
3. Fold in §8's one-line announce guard in PExpr's strip.
4. Regression rows in `Pl/t/transpile-test-09.t` (or `-10` if -09's wall
   time says so), including the INVERSE guard (`($$)` still announces and
   runs).

## 11. Opus's queue (in order)

1. **#270** — the §10 silent drop + §8 guard.  Half-session, verification:
   gate + gate SET over both populations (a checker is being widened).
2. **#265 rename half** — shape per §9.  Bar: op/my.t reaches its snapshot
   C_ok (52/8 target per the s363 measurement; currently 51/8).
3. **#267 sizing, then implementation if the sketch holds** — sketch: extend
   `_alias_box_form`'s verdict to a LIST of per-element box forms and lower
   the alias at CLForm as one binding per element (the anchored-head rewrite
   cannot express it, agreed).  If sizing finds the sketch wrong, it comes
   back as an ask instead of growing in place.
4. **#269** — the nested-sub capture gate on reg_eval_scope.t.  The A-family
   playbook applies (is the refusal scope-blind or genuinely conservative?)
   — but PROBE FIRST: one scope in, the capture may be real, and this one is
   only deleted if the probe says blind.
5. Fillers as time allows: **#271** (new, filed per the not-dones:
   `pipe my ($r, $w)` — `(p-pipe (vector $r $w))` fails to compile;
   op/getppid.t 0/0 → snapshot), #266, near-green #236 → #234 → #235.

**Not Opus's**: the #153 FOLD stays mine, its own session.  Boxed aggregates
and E5.1/E5.2 designs stay DO-NOT-START.

Standing checklist unchanged: per change `tools/prove-core` + targeted file;
gate SET diff over both populations whenever a checker/verdict widens; full
sweep + suite every 3rd–5th change; bump `*pcl-cache-generation*` on
emission changes.
