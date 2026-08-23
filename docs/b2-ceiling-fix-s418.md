# B2 (#343) — the paren-less list-operator ceiling, recomputed at use (s418, Fable design)

*(Answers `docs/b2-stale-operand-ceiling-s417.md` §4; ruled in
`docs/fable-answers-s417.md` §2.  The mechanism, trace table, node dumps and
population scan are in the s417 doc — this file is the fix: what changes, why
it is exactly the old intent computed correctly, and the acceptance bar.)*

## 1. The change — complete, in `Pl/PExpr.pm handle_subcalls`

**(a) Delete the cache.**  The `my $last_low_prio_op;` declaration (~3563)
and the assignment inside the `and`/`or`/`xor` branch (~3572–3577).  The
branch KEEPS its `next` — those tokens are boundaries, never call words.

**(b) Replace the two ceiling lines (~3757–3759)** with a rightward scan of
the CURRENT `@$e`:

```perl
    # - - - Parse parameters of fun:
    # The argument list of a paren-less list operator ends before the nearest
    # and/or/xor AT THIS LEVEL.  Derive that boundary from the CURRENT @$e,
    # never from an index saved earlier in the scan: this loop's own
    # reductions splice @$e and shift everything right of the reduction
    # leftward, so a saved position no longer means what it meant — the
    # arguments then swallow the operator that should end them (#343;
    # docs/b2-stale-operand-ceiling-s417.md has the trace and the scan).
    my $end_pars = scalar(@$e)-1;
    for (my $j = $i + 1; $j <= $end_pars; $j++) {
      my $jop = $self->is_token_operator($e->[$j]) // '';
      if ($jop eq 'and' || $jop eq 'or' || $jop eq 'xor') {
        $end_pars = $j - 1;
        last;
      }
    }
```

**(c) Delete `_stale_low_prio_probe` and its `PCL_B2_TRACE` call site.**
With the cache gone the saved-vs-actual comparison has no subject (ruled: it
does not become a gate).

Nothing else changes.  This is a bookkeeping correction inside an existing
boundary, not a grammar change — no new rule, no new production, no
`_term_extent`/`_reduce_term` involvement.

## 2. Why the scan is exactly the old intent, computed correctly

Two invariants:

* **Membership is invariant; only positions shift.**  A reduction replaces a
  word plus its argument run with one opaque node.  A CORRECT reduction never
  consumes a same-level `and`/`or`/`xor` (that is what the ceiling is for),
  and cannot introduce one (structures are opaque; an `or` inside parens was
  never an element of `@$e`).  So the set of same-level boundary operators to
  the right of `$i` is the same set the right-to-left scan passed — at their
  CURRENT positions.  (Inductively: the rightmost paren-less word reduces
  with no reduction to its right, hence correctly; each further-left word
  then sees only correct reductions to its right.)
* **The cached value's intended meaning was "the nearest boundary to the
  right of `$i`"** — the right-to-left scan overwrote at every boundary, so
  the last write was the leftmost passed, i.e. the nearest.  The fresh
  rightward scan computes precisely that, on true positions.

Therefore emission is **byte-identical by construction wherever no reduction
intervened between the word and the boundary** — 648 of the 658 scanned files.
Only the 10 traced events can change; that is the A/B prediction, checkable
file-by-file.

Cost: one O(region) walk per paren-less word — the same walk, in the same
place, as the ternary-`:` boundary block immediately below (and as the probe
already did).  The 50 % compile budget is trivially met.

## 3. What must NOT change

* **reg_fold.t:165** (`eval join … or die $@`), the stale-but-benign site.
  Expected byte-identical: `eval` is a named unary, so the term walker — not
  the ceiling — decides its operand.  If bytes DO move, the behavior must
  still match perl (probes E1/E2 in the s417 doc pass today).
* The four probed inverses: `f $u or g "fb"`, `f $u, "m" or g`,
  `f(ref $u, "m") or g`, `f ref $u, "m";`.
* Everything outside the 10-event set: the emission A/B over the FOUR
  populations must show diffs ONLY in bless.t (both populations) and split.t
  (both populations) — plus reg_fold.t only under the caveat above.

## 4. Acceptance set (perl-oracle rows)

1. `f ref $u or g "fb"` → `f(main)` `g(fb)` — the drop closes.
2. `f ref $u, "m" or g "fb"` → `f(main m)`; no `g` — #343's headline.
3. `f ref $h{k} or g "fb"` → `f()` `g(fb)` — the shift-2 SILENT WRONG closes.
4. The `and`/`xor` spellings of row 1 (the scan treats all three alike).
5. Two boundaries: `f ref $u or g "a" or h "b"` — the nearest wins.
6. A reduction inside the args AND a boundary after:
   `f ref $u, ref $v or g "x"`.
7. bless.t:179's own text: `is ref $untied, "main", 'd' or diag $@`.
8. split.t:503's shape: `my ($sp) = grep /\s/u, map chr, reverse 128 .. 255
   or skip 'x', 9;` — the `or` branch must run, as perl runs it.
9. Siblings **#259** and **#335**, EXPECTED NO CHANGE (neither shape contains
   a same-level `and`/`or`/`xor`: #259 is `u3 0 || 5` under a leading-`;`
   prototype, #335 is a PPI structure-label bug).  Annotate each task with
   the post-fix probe result; if one unexpectedly closes, it leaves the
   census/notes BY EDIT with cause.

## 5. Bar and bookkeeping (the s372 three legs — a decline becomes an accept)

* **emission A/B** (`tools/emission-ab.pl`) over the four populations vs a
  base worktree; every diff explained per file against §3's prediction.
* **gate-SET scan** (`tools/gate-set-scan.pl`) over BOTH populations vs base.
* **full sweep**: GATE verdict + TOTAL/LOST read.  Census: the bless.t drop
  rows (perl-tests + t/op) leave `baselines/parse-error-drop-census-s399.tsv` BY
  EDIT with cause; the sweep's own drops count moves 10 → 9 (verify).  If the
  newly-running bless.t row FAILS (it tests blessing through tied refs), it
  enters `baselines/fail-baseline.tsv` BY EDIT with its cause; split.t rows probed
  and edited row-by-row; `baselines/pass-baseline.tsv` likewise.
* **companion `--quick` once** — the affected shapes live in t/op/ and t/re/.
* **generation bump v2-160 → v2-161** + the three checked-in artifacts
  regenerated (expect gen-stamp-only — none of their sources uses the shape).
* **guard rows**: new file `Pl/t/listop-ceiling-01.t` for §4 rows 1–6
  (run rows where behavior is the point, transpile-shape rows otherwise;
  measure wall time per the file-size rule).
* Close #343 pointing here; stamp the s417 measurement doc "FIXED s418".
