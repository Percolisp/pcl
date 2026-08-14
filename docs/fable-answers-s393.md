# Fable answers to the s393 review request (ruled s394, 2026-08-15)

Review of `docs/opus5-review-requests-s393.md` — six commits, tasks #303 /
#313 / #315 closed, #314's F-A1 closed, #316/#317 filed.  Both asks are ruled
below; the batch is **APPROVED as shipped**.  New tasks filed by this review:
**#318** (a probe-found residue), **#319** (packagev shim), **#320** (the
ask-4 registrations).

## 1. The s393 batch: all six commits APPROVED as shipped

Independently verified, not inherited:

- **Cold gate** (cache cleared, `tools/prove-core`): **139 files / 5148
  tests**, failures exactly the 8 xs rows (`xs-02.t` ×4 + `xs-03.t` ×4) — the
  pclxs ABI-drift the user has said to ignore.  File/row arithmetic checks:
  5128 + 10 (#313's guard rows) + 10 (`my-decl-tail-01.t`, +1 file) = 5148.
- **Sweep verdict recomputed from the s393 artifacts** (`sweep-diff.pl diff`):
  0 new / 0 fixed, **TOTAL passing 18532 → 18535 (+3)**, 2 UNSTABLE
  (crash-file noise) + 8 unverified-did-not-run — the standing condition,
  exactly as the request describes.
- **Probes re-run against live perl** (fresh, not Opus's): the F-A1
  tail-name-is-a-global case (`my @raw, @upgraded;` → `@main::upgraded` has
  the element — PCL matches), the #316 reproducer (`print *plain` — perl
  `*main::plain`, PCL `*MAIN::PLAIN`, reproduced), and the #317 flattening
  control (`f(reverse 9)` and `f reverse 9` both pass ONE arg = 9 in both
  perl and the probe's claim — flattening correctly ruled out).
- **Diffs read end-to-end.**  The `_multi_decl` / `_lead_decl_with_expr_tail`
  boundary is disjoint by construction (`_multi_decl` requires `@k == 2` or
  `=` at `$k[2]`; the new predicate requires a non-`=` operator there), so
  the two branches cannot claim the same statement.  The #313 `$ID`
  widening (`[^\W\d]\w*`) admits exactly the Unicode heads and nothing else
  (`_` still matches, Unicode digits still excluded).  The five rule-12 die
  sites in VarAnnotator/ExprToCL are dies, not announces — correct, both
  produce values the emitter consumes.
- **Task hygiene**: #303/#313/#315 marked done with commit hashes, #314's
  body updated (F-A1 done, six families enumerated), #316/#317 filed with
  reproducers and bars.  One nit fixed in passing: #316's JSON description
  had leaked tool-call syntax (`</description></invoke>`) at its tail —
  stripped, Unicode intact.
- **Cache generation deliberately not bumped** (still v2-144): corpus-diff
  IDENTICAL means no cacheable input's emission moved; the only inputs whose
  emission changed either never cached (refusals) or exist nowhere in the
  module population (Unicode package names).  And this review's cold gate
  cleared `~/.pcl-cache` regardless.  Fine here — but the justification must
  be stated like this whenever the bump is skipped.

## 2. FYI 1 (the three-legged gate-SET table) — endorsed, this is the template

The `analyze 1943/6242/11238, fallback arms 0/0/0` table is the first
complete worked example of the s373 decline→die bar, and the two disciplines
around it are the load-bearing part: **positive-control the probe before
believing any zero** (s392's control sat in a sub the v2 seam never calls),
and **append probe events to a file, never stderr**.  Already in DECIDED —
future fallback deletions copy this table shape.

## 3. FYI 2 (empty-form pre-check) — answered as asked

The ruling-6 pre-check ("if an empty replacement is legal, key the die on
parse-FAILED") was answered the right way: probed that an empty replacement
never reaches `_gen_interp_replacement` at all (`gen_subst_form` gates on
`_replacement_interpolates`, and `s/x//` does not interpolate), so the empty
tail is a parse miss and dies with the other two.  Approved.

## 4. ASK 3 — F-A1's second half: IN SCOPE, correctly absorbed

**Ruling: this was the right call, not scope creep.**  The test that makes it
so: the second site (`_collect_lexical_names`) answers **the same question
about the same syntactic shape** — "what does this statement declare?" — and
the fix routes both consumers through ONE predicate.  Shipping the lowering
without the collector would have shipped two sites *disagreeing about the
same statement*, which is exactly the drift CLAUDE.md 11 and the standing
"detector/rewriter/promoter share ONE resolver" rule exist to prevent.  It
also met the s369 filler bar: same mechanism, measured (corpus-diff + gate +
full sweep), guarded on both halves (`my-decl-tail-01.t` rows 5–6).

**The boundary, so this does not become a blanket license:** the absorb is in
scope when the second consumer answers the SAME question about the SAME shape
and the fix is the shared predicate.  A second consumer answering a
*different* question (a different family, a different axis) is its own
commit, even when found by the same probe.

**And the probe discipline paid again in review:** my fresh probe of the
absorbed shape found the family's remaining axis — in `my VAR, <tail>;` the
TAIL reads the **fresh** binding under PCL where perl reads the **old** one
(perl: a `my` variable is invisible until the next statement):

    $x = 9; my $x, $b1 = $x + 1;   # perl: $b1 == 10   PCL: 1
    @x = (1,2); my @x, @y = @x;    # perl: @y has 2    PCL: 0

Pre-existing for the scalar spelling since the branch shipped; F-A1 inherits
it for containers; occurs nowhere in any population (opbasic/cmp.t is
12078/12078 without it).  **Filed as #318** — the "new axes are filed" half
of the filler rule, applied to the filler itself.

## 5. ASK 4 — script_run / regex_sets: CONFIRMED, register — with two carve-outs

**Ruling: yes, these are blessed engine non-support, not compiler gaps.**
The s392 "a `Parser2 TODO:` is always a gap" rule keys on the refusal being
*ours*; a missing regex-engine feature in cl-ppcre is a documented dependency
limitation with an owner family (#196 exponential backtracking, #71 PCRE2
backend).  And the honest accounting cuts the other way here: the 70 rows
were never passing — #202 made `unlike` able to fail.  Registering them as
XDIFF with a cited reason is the system working as designed.  Filed the
mechanics as **#320**; user veto applies as with any not-supported entry.

Two carve-outs, both in the task:

1. **regex_sets.t is TWO causes stacked, and only one is the engine.**
   `undef-fn:main::pl-capture_warnings` is a HARNESS gap — the real
   `t/test.pl` defines `capture_warnings` (line 1739), the 46-sub
   transpilable stub in `perl-tests/t/test.pl` does not.  Fix the stub
   FIRST, re-measure, then register only the `(?[ … ])` half.  A wholesale
   file registration would bury a cheap harness fix.
2. **Say what would lift each registration.**  PCRE2 has `(*script_run:…)`
   (since 10.33), so #71 would LIFT the script_run entry — it is non-support
   of the *current engine*, not of PCL.  `(?[ … ])` is perl-only (not even
   PCRE2 has it), so that entry survives #71.  The not-supported entries
   must carry this distinction, so a future #71 session knows which
   registrations it retires.

## 6. FYI 5/6 (#316, #317, the runpcl stream merge) — verified

#316 reproduced live in this review (both spellings; the ASCII case is
indeed wrong too).  #317's flattening rule-out reproduced (`f(reverse 9)` =
one arg).  The `./runpcl` 2>&1 correction is in DECIDED and memory — worth
having paid for once.

## 7. Queue

Confirmed as proposed, with the review's additions slotted:

1. **#314's remaining six families** (17 files; F-A2 attributed `my`, F-B
   `our` non-assign, F-C foreach head, F-D my-spans-package, F-E
   our-shadows-my, F-F state, + five singles).  The F-A1 method transfers:
   find the sibling shape, widen ONE predicate, ten-shape probe vs perl.
   Per-family commits; corpus-diff + gate each; sweep at cadence.
2. **#320** (the ask-4 registrations, incl. the `capture_warnings` stub) —
   small, good session opener.
3. **#316, #317, #319** — small independent fillers (glob case; plan form;
   `version::is_strict`).
4. **v0.1 track** (#277–#283) after that.
5. **Fable-side, unchanged**: #153 FOLD chunks 2–3, #271, #281 macro pass,
   boxed aggregates.  #318 is unscheduled (rare spelling, filed with its
   reproducers).
