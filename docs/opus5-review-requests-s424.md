# Review requests — session 424 (Opus 5, 2026-08-22)

One task: **#423**, item O2.3 of `docs/plan-post-s420.md` — the glob-value /
glob-ref representation question, and the op/gv.t row s419d cost.  One commit.

The task said MEASURE FIRST and branch on the answer.  §1 is that measurement
and which fix shape it selected; §2 is what shipped; §3 the numbers; §4 the
three pre-existing bugs the probe table found, all filed, none fixed here;
§5 the asks.

## §1  The measurement — they DIFFER, and by a flag that already exists

The task's discriminating question was: *is `\*FOO` structurally different
from `*FOO` in a box today, or identical?*

**Different — and the discriminator is `p-box-is-ref`, which is already there
and already read on one path.**

| | `my $g = *foo` (VALUE) | `my $g = \*foo` (REF) |
|---|---|---|
| box `value` | the `p-typeglob` | the same `p-typeglob` |
| box `is-ref` | **NIL** | **T** |
| set by | `box-set`'s typeglob rule (line 1721) | `p-backslash`'s typeglob arm (12885) |
| `box-nv` (the NUMBER) | 0 — **reads is-ref, always did** | the address — **reads is-ref** |
| `box-sv` (the STRING) | `GLOB(0x1)` ✗ | `GLOB(0x…)` ✓ |
| `p-ref` | `GLOB` ✗ | `GLOB` ✓ |

So this was never a representation gap: it was **two readers ignoring a
discriminator the third one had been using since #163**.  A box that printed
`GLOB(0x1)` and numified to `0` is the #163 rule broken in place — the word
and the number disagreeing about the same value.

**The s335 "no `ref-kind` slot" ruling is therefore untouched: nothing was
added to the box.**  Fix shape (a) applies, and it is re-raised only in the
sense that this note records that the measurement CONFIRMS s335 — the tag
would have answered a question `is-ref` already answers, exactly as s333 said.

A second, weaker shape exists and matters for one bug below: a `\*foo`
**literal assigned to a PACKAGE variable** takes `p-scalar-=`'s
reference branch and is stored box-in-box (`box(box(typeglob, is-ref))`),
where a lexical is stored flat.  Both resolve through `%p-ref-referent`
already; only `%p-ref-string` had no arm for the typeglob referent, which is
why `our $r = \*STDOUT; print "$r"` printed `SCALAR(0x…)` while the identical
`my $r` printed `GLOB(0x…)` (§2, reader 2).

## §2  What shipped — one rule, three readers, three copy paths

The rule, now normative in `docs/ir-spec.md` §2.5:

> A typeglob is the **one payload whose ref-ness lives on the box, not on the
> object**.  `is-ref` is the whole distinction; every reader asks it, every
> copy path carries it, and a **raw** `p-typeglob` outside a box is a glob
> VALUE — the convention `stringify-value` already fixed for the string half
> (#316).

Readers (each one line of behaviour, via the new predicate
`%p-glob-value-box-p`):

1. **`box-sv`** — the typeglob arm prints `GLOB(0xADDR)` only for an is-ref
   box; a glob value goes to `stringify-value`, i.e. the same `*main::foo` the
   raw path already produced.  (This is the arm s419d's `s///` and `tr///`
   now reach; the `""`-overload/tie fix stands unchanged.)
2. **`%p-ref-string`** — added the **missing typeglob-referent arm**
   (`GLOB(0xADDR)`) and the glob-value discrimination inside the box-referent
   branch.  The missing arm fell through to `(t nil)` and the caller's
   `SCALAR(0x…)` fallback — a closed-set dispatch with a hole (rule-12 family),
   found by the probe table, not by reading.
3. **`p-ref`** — a referent box holding a glob VALUE answers `GLOB` (perl makes
   that SV a GV: `perl-tests/substr.t 784`); a box holding a glob VALUE answers
   `""`; an is-ref box still answers `GLOB`.

Copy paths — **this is the half the task did not predict, and it is where the
first cut of the fix regressed**.  Making the readers honest immediately
exposed three places that DROP the flag, each of which silently demoted a glob
reference to a glob value:

4. **`%p-flatten-list`** — its "snapshot what box-set will store" cond is a copy
   of box-set's own reference list and **omitted typeglobs**, so `my $x = shift`
   on a glob ref snapshotted the raw glob.  It now snapshots a **fresh
   flag-carrying box** — see §2.1, the first cut got this wrong.
5. **`%p-array-store-scalar`** — the typeglob shared the "fresh container around
   the same object" arm with array/hash/code/qr, whose stated reason ("the ref
   type is unchanged, the object is unchanged") is **false for a glob**: the
   type is on the container.  Own arm now, carrying `is-ref`.
6. **`p-aref-unbox-elem`** — unboxed a glob-ref element to the raw glob; now
   keeps the box when the element is is-ref.  (`p-flatten-args`,
   `p-return-value` and `p-copy-scalar-arg` already carried it; hash entries
   already go box-in-box via `%p-make-hash-entry`'s is-ref arm.)

**Generalisable finding: a copy path that drops a discriminator is the same
bug class as a reader that ignores it**, and it is the harder half to see —
the readers were wrong in a way probes catch on line 1, the copiers were wrong
only once the readers became honest.

### §2.1  The first cut of item 4 was wrong, and the breaking-case probe caught it

Preserving the SOURCE BOX in `%p-flatten-list` (the same thing its
reference/blessed/dualvar arm does) carries the flag — and **aliases**.  A list
assignment snapshots its RHS precisely so that `($a,$b) = ($b,$a)` works; with
the box preserved, `($g1,$g2) = ($g2,$g1)` printed `*main::bar|*main::bar`
where perl swaps, and two swapped glob REFS collapsed to one address.  The
snapshot has to be a **fresh box carrying `is-ref`** — which is exactly what
`%p-array-store-scalar`'s new arm does, so the two now say the same thing.

This is the second time in this session the answer was "the sibling arm is not
the right sibling": the array-store arm's stated reason (*the ref type is
unchanged because the object is unchanged*) is false for a glob, and the
flatten arm's preserve-the-box is right for a reference and wrong for a glob.
Guarded: `Pl/t/ref-identity-01.t` t37–t39 (a glob-value swap, a glob-ref swap,
a mixed swap), all three perl-probed.

## §3  Measurements

| leg | result |
|---|---|
| Gate `tools/prove-core` (PCLXS_DIR set) | **155 files / 5631 rows**; failures = exactly the 13 pclxs xs rows (xs-01 5, xs-02 4, xs-03 4).  Baseline 155/5614 + the 17 new guard rows |
| Full sweep `--jobs 3` | **GATE clean**, TOTAL 18364 → **18365 (+1)**, drops **7 = census (+0)**, 0 new / 0 fixed, 7 UNSTABLE + 10 unverified = the usual above-abort-point noise on the same PARTIAL files as s421 (§3.1) |
| Companion op/ + io/ leg | 12 files, `--jobs 2`: **op/gv.t 49/48 → 61/36**, **op/substr.t 376/24 → 377/23**, ten identical (§3.2) |
| Probes vs perl 5.40.3 | 6 files, ~60 shapes — p2/p3/p6/guard **byte-identical to perl**, also under `PCL_OPT=none`; p1 differs in 2 rows and p4 in 3, every one A/B-verified pre-existing (§4) |
| Artifacts | all three regenerated; diff vs the checked-in copies = **the `gen=` stamp line only** (the change is runtime-only, as expected) |
| Generation | v2-163 → **v2-166** (this agent's private namespace; the merge renumbers) |
| corpus-diff / emission-ab | **not run — nothing under `Pl/` changed**; the artifact byte-compare above is the emission evidence |

### §3.1  Full perl-tests sweep

Run twice (the first on an intermediate tree, before §2.1's correction) — both
runs report **GATE clean, TOTAL 18365, drops 7 = census, 0 new / 0 fixed**, so
the §2.1 correction is invisible to the sweep (no perl-tests file swaps two
globs).  The one row: `perl-tests/substr.t` **374 → 375** —
`is ref \$x, 'GLOB', '\substr does not coerce its glob arg just yet'` at
substr.t:784, which is `my $x = *foo` verbatim.  Its fail count is unchanged
(8) and its skip count unchanged (14): the row was previously producing
nothing countable, and now passes.

The UNSTABLE/unverified buckets name exactly the files s421's run named
(method/postfixderef/ref/yadayada above their abort points, eval/magic/ref/tr/
yadayada unverified) with the same counts.

### §3.2  Companion (the leg the s421 ruling made mandatory for a `cl/`
stringification change)

Twelve files chosen by `grep -rlE '=\s*\\?\*[A-Za-z_:]'` over `t/op`, `t/io`,
`t/re`, `t/uni`, `t/base`, `t/comp`, restricted to the ones with nonzero
`C_ok` in the snapshot: op/gv, op/ref, op/magic, op/substr, op/stash,
op/select, op/reset, op/readline, op/gmagic, io/open, io/argv, io/defout.

| file | snapshot | this run | verdict |
|---|---|---|---|
| **op/gv.t** | 49/48 | **61/36** | mine — A/B on an `a2ac578` worktree measures **49/48** |
| **op/substr.t** | 376/24 | **377/23** | mine — A/B measures **376/24** (same row as the sweep's) |
| the other ten | — | identical | — |

Both movers were re-run ALONE by the runner's own #366 rule and both runs
agree.  op/gv.t recovers s419d's row and eleven more: the file is nothing but
glob semantics, and `ref($glob)`/`ref(\$glob)`/`"$glob"` are now perl's
answers.  Spliced into `docs/perl-suite-run.tsv` **with the cause**, per the
s421 ruling.

## §4  Found and FILED, not fixed (all pre-existing, all from the probe table)

Every one was A/B'd by restoring `a2ac578`'s `cl/pcl-runtime.lisp` into this
worktree and re-running the probe files: `ref(\$fh)` `SCALAR`, `\*foo == \*foo`
`diff`, the `*b = $pkgvar` crash and the `$n = \*foo; $n = "plain"` REF residue
are **byte-identical at the base**.  The only p1/p4 row that CHANGED and is
mine is `ref(*main::foo)`: `GLOB` at the base, `""` now — perl's answer, and
ask 3 below.

* **#436 — a lexical filehandle is not a GLOB to `ref()`.**
  `open my $fh, …; ref($fh)` is `""` and `ref(\$fh)` is `SCALAR`; perl says
  `GLOB` and `REF`.  PCL stores a raw STREAM in the box; `stringify-value` has
  a `streamp` arm (so `"$fh"` is `GLOB(0x…)`, correct) but `p-ref`,
  `%scalar-holds-ref-p` and `%p-ref-string` have none — the same
  word-vs-string disagreement this session fixed for typeglobs, one payload
  kind over.  **Deliberately not widened here**: `ref($fh) eq 'GLOB'` becoming
  true changes every IO path (`lib/IO/Handle.pm:390` is
  `if (ref($fd) && "$fd" =~ /GLOB\(/o)`, a branch perl takes and PCL does not),
  so it needs its own io/ leg and its own probe set.
* **#437 — `\*foo == \*foo` is FALSE and the two print different addresses.**
  `p-make-typeglob` mints a fresh struct per mention, so glob referent identity
  is unstable: #163's defect one level below where #163 fixed it (a fresh
  WRAPPER per `\`; here a fresh REFERENT per `*foo`).  Fix = intern typeglobs
  per (package, name).
* **#438 — `*main::b = $pkgvar` where the package variable holds `\*foo`
  installs the SCALAR slot** and the sub never appears (`main::b` undefined
  crash).  `%p-glob-assign-slots` dispatches on one `unbox`, which sees the
  box-in-box shape's inner BOX rather than the typeglob.  Same rep-2 blind
  spot whose *stringification* half this session fixed.

## §5  Asks

1. **Scope of the copy-path half (§2 items 4–6).**  It is three more sites than
   the task's "one place in `box-sv`", but without them the readers regress the
   common `my $x = shift` idiom (measured: `ref` `""` + `*main::foo` where perl
   says `GLOB` + `GLOB(0x…)`).  I judged them the same rule rather than a
   second mechanism — one predicate, one invariant, all three sites named in
   the ir-spec paragraph.  Confirm, or say the flag-carrying belongs in its own
   task behind its own bar.
2. **`p-aref-unbox-elem` is a hot path** and now carries one extra
   `p-typeglob-p` test on the miss path (after four existing type tests, before
   the more expensive `%p-dualvar-box-p`).  The gate's wall time moved 257 s →
   343 s between my two runs, but those runs are not comparable (the second ran
   against three concurrent agents), and pack.t — the compile/run time canary —
   is unchanged in the sweep.  If a Fable review wants a real number, the A/B
   is one `tools/prove-core` pair on a quiet machine.  I did not spend one.
3. **`ref()` on a RAW typeglob** (`ref(*main::foo)`, and any glob that reaches
   `ref()` without its box) now answers `""` (perl) where it used to answer
   `GLOB`.  That is right *given* §2 items 4–6; if a copy path I did not find
   still strips a glob ref's box, its `ref()` silently becomes `""`.  The
   conservative alternative — keep `GLOB` for a raw glob and accept
   `ref(*foo)` staying wrong — is one line away.  I chose perl-correct because
   the sweep + companion + 60 probes found no stripping path left; flag if you
   want the belt.
4. **Not a question, a note for the queue.**  #436 (the lexical filehandle) is
   the same one-rule fix one payload kind over, and it is the last piece that
   would make `ref`/`"$x"`/`0+$x` agree for every reference kind PCL has.  It
   is a natural O3 filler *if* someone is willing to spend the io/ leg on it;
   I did not fold it in because "it changes every IO path" is exactly the kind
   of widening the filler rule says to size first.
