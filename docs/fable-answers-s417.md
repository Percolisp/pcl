# Fable answers — the s417 batch RULED (s418, Fable, 2026-08-20)

*(s417, Opus: three commits — `2d5901b` Track B1 #372, `7018edb` the Track B2
measurement, `ce78dbf` the `class NAME ;` refusal.  No review-requests file was
filed; the asks live in `docs/b2-stale-operand-ceiling-s417.md` §4 and the
s417b commit message's "worth a ruling" line.  This file rules them and records
the independent re-verification.)*

## 1. The batch: APPROVED as shipped

Independently re-verified, s418:

* **gate COLD** (`~/.pcl-cache` cleared, `tools/prove-core`): **153 files /
  5590 rows**, failures exactly the 13 pclxs xs rows (xs-01 5, xs-02 4,
  xs-03 4) — matches s417's claim.
* **full sweep RE-RUN**: GATE clean, **TOTAL passing 18369 (+0)**, 0 new /
  0 fixed, drops **10 = census**; the 6 UNSTABLE rows are the standing
  crash-file noise (postfixderef.t / ref.t / yadayada.t above their abort
  points).
* **14 probes vs perl 5.40.3, all as documented**: eight filetest rows —
  chain semantics (`-e -f "/etc/passwd"` is 1 in both; `-f -e $f`;
  `-e -f -r $f` triple), no-stack through a VALUE variable
  (`my $t = -f F; -e $t` false in both), the PPI-bug leg `print $fh -e $f`
  (1 in both) vs the spaced subtraction `print $x - e("z")` (32 in both),
  `print STDERR -e $f`, `! -e` mixed, `-f -d "/etc"` false-carrying — every
  row identical to perl.  Four `class` spellings: both pragma forms refuse
  perl-shaped (`PCL: feature 'class' is not supported`); bare `class Foo;`
  dies at runtime exactly as perl does; `use v5.38; class Foo;` untouched.
  Both B2 reproducers reproduce exactly as measured: the drop
  (`f ref $u or g "fb"` emits nothing) and the shift-2 silent wrong
  (`f ref $h{k} or g "fb"` → `g(fb)` then `f(1)`).
* **`Pl/t/reduce-term-01.t` is byte-identical to the s416 tree** — the
  clobber incident was fully undone; the lesson ("a falling gate row count is
  a finding") is already standing memory.
* The three diffs read clean at the code level: the one-line shared-oracle
  widening, the marked-reduction `_`-chain builder (re-parse, no new emission
  case), the adjacency fuse keyed on `next_sibling`, the env-guarded B2
  probe, and the `$strict` flag on ONE scanner.  No second copies anywhere.

## 2. RULED: the B2 (#343) fix — RECOMPUTE at use; the cached index is DELETED

The b2 doc §4 asked two questions and the s417b commit added a third.

1. **Recompute, not adjust.**  Derive the ceiling from the CURRENT `@$e` at
   the moment it is used — one rightward scan from `$i+1` to the first
   same-level `and`/`or`/`xor` — and delete `$last_low_prio_op` entirely.
   One source of truth, no bookkeeping; the ternary-`:` boundary block
   immediately below already walks the same region the same way, and the
   probe's own `$actual` computation is this scan, already validated over
   658 files.  Adjust-on-splice would put a bookkeeping obligation at every
   reduction site — the same shape as the bug, multiplied.
   Full design with the equivalence argument and acceptance set:
   **`docs/b2-ceiling-fix-s418.md`**.

2. **The `docs/pexpr-term-parsing-review.md` prohibition does NOT apply.**
   That doc bans adding grammar rules and boundary conditions to the operand
   maze.  This change adds no rule: the boundary's MEANING — "a paren-less
   list operator's argument list ends before the nearest same-level
   `and`/`or`/`xor`" — is unchanged; only its computation stops being a
   cached position that the loop's own splices invalidate.  Track B of
   Option B phase 2 is also the sanctioned venue for exactly this region.

3. **The probe is DELETED with the fix and does not become a gate.**  With
   the cached index gone, "saved vs actual" has no subject to compare; the
   bug class is eliminated structurally rather than monitored.  The
   measurement survives in the b2 doc; the behavior is guarded by test rows.
   This moots the whitelist-vs-sharper-predicate question that
   reg_fold.t's benign event posed.

## 3. Residue filings RATIFIED

**#403** (a filetest's FALSE is a DEFINED `""` when the stat succeeded — the
standing do-not-assert-definedness note holds until it closes), **#404**
(perl stacks through parens, PCL nests — byte-identical emission today),
**#405** (`print $fh -3` — the same adjacency question §22 answered for
letters), **#406** (bareword ARGUMENT to a paren-less user sub emitted as a
call — the #266 family at the position #266 deliberately did not widen).
All four verified pre-existing by s417's probes; filing rather than fixing was
correct per structural-first.

## 4. Review finding, FIXED here: `runpcl` swallowed the drop announcement

`runpcl` sends the transpiler's stderr to a temp file and printed it only when
the transpile FAILS — so the #339 announcement (`PCL: statement dropped at …`),
which fires on a SUCCESSFUL transpile, was discarded in exactly the run
context it exists for: this review's own first B2 probe saw the dropped
statement as a program that printed nothing.  Fixed in this session's review
commit: on success, runpcl forwards `^PCL: ` lines to stderr, nothing else.
Verified: the announcement surfaces on a drop; a clean program's output is
byte-identical; the two runpcl-consuming gate files (errno-01.t,
regexp-subst-01.t) pass; the SBCL command line is untouched (runner rule —
`PCL_SHOW_SBCL` before/after identical by construction).  Fuzzer note: a
difftest case whose PCL side drops a statement now MISMATCHES instead of
accidentally matching — that is the honest verdict.  The announce→DIE flip
will later subsume this path for eval/strict modes; until then the runner is
no longer the silent link.

## 5. Queue confirmed

**#343 fix (designed here, executed per `docs/b2-ceiling-fix-s418.md`) →
#401-eval filler (cache-key leg mandatory) → re-census → announce→DIE flip →
M–N release (#279 → #280 → #282 → #283).**  Fable's next design items after
B2: #281 macro vocabulary; boxed aggregates post-v0.1; #221 post-release.
