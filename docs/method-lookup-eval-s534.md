# Method lookup: how to make it faster (s534a evaluation, 2026-11-08)

The USER's ask (s534): *evaluate how to make method lookup faster*.  The reading
"method calls are about par with perl" is true of exactly one shape -- a method
defined in the object's OWN class (`methret`, the shape #582's own-class half
caches).  It was never measured for INHERITED calls, which is what PPI, Moo and
every class hierarchy make: profiling PPI under PCL in s534
(`docs/eval-in-cl-plan.md` §4.6) put the `@ISA` re-walk at 27 % of the whole run.

This is an EVALUATION, not a ruling: three options were prototyped in the s534a
worktree and measured on the same instrument; the recommendation in §6 is
awaiting the USER.  No product code changed on the merged branch -- the
prototypes are runtime files under `scratch/s534a/rt/` (paths in §4), so the
batch that builds the chosen one starts from them.

Base for every measurement: main's CODE sha **`95a1de03`** (generation v2-11400).
Main moved to `06a4b8ba` (s533c) while this ran; its runtime differs from the base
only in the generation string and a `local $a[i]` initializer (#4441), which no
row here reaches, so the base stays `95a1de03`.

> **Recommendation (awaiting the USER): build (f) + (d) -- VALIDATE, don't
> invalidate.**  Cache every inherited resolution, hit AND miss, per (class,
> method), and the linearization per class, each with the links that licensed
> it (every passed class's `@ISA` names and its own `pl-NAME` symbol), re-read
> on every use; the overload lookup the same way plus one counter its two
> writers bump; and guard `%p-isa-symbol`'s weak-table probe (one line).
> Measured on the prototypes: inherited calls 3.9-7.4× -> **1.3-1.7× of perl**,
> an inherited `bool` overload 6.3× -> 2.1×, the PPI parse 4.4× -> **2.75×**;
> own-class calls, `json-rt`, `moo-objs` unchanged; identical to the base on
> all 61 probe-matrix cases.  #582's original design (c) -- invalidate on every
> `@ISA` write -- is REJECTED by measurement: an `@ISA` is not a box, so the
> mark costs every array write (PPI +19 % slower) and a missed writer among 217
> is a silent wrong (12 of 61 probe cases went stale with the obvious audit).
> One runtime-only session; tasks #4523 (methods + ancestry), #4524 (overloads),
> #4525 (the guard).

## 1. Today: the inherited rows (Q1)

Four rows joined `tools/bench-exec.pl` (verified against perl's output like
every row): `methinh` (methret's callee one `@ISA` level up), `methinh3` (three
levels up -- PPI's Token -> Element chains are 2-4 deep), `methpoly` (ONE call
site over objects of four sibling classes that inherit the method -- PPI's
shape), `ovlbool` (a boolean test of an object whose `bool` overload is two
levels up).  Best of 5, startup subtracted; column B is a byte-identical COPY
of the runtime, so B/A is the run's own noise band (`scratch/s534a/out/bench-q1.log`,
02:04, load 1.99 -> 2.39):

| row | perl (s) | PCL (s) | PCL / perl | control B/A |
|---|---:|---:|---:|---:|
| `methret` (own class) | 0.0949 | 0.1167 | **1.23×** | -5.4 % |
| `methinh` | 0.0946 | 0.3769 | **3.98×** | +0.4 % |
| `methinh3` | 0.0955 | 0.7500 | **7.85×** | +3.2 % |
| `methpoly` | 0.1102 | 0.4124 | **3.74×** | -1.7 % |
| `ovlbool` | 0.0782 | 0.5107 | **6.53×** | -5.6 % |
| `ovlsub` | 0.0454 | 0.1172 | 2.58× | +7.6 % |

perl pays the same per call whatever the depth (its per-stash method cache);
PCL pays ~0.28 µs per call per `@ISA` level, because an own-class MISS re-walks
`@ISA` by name every time.  The control column says what a difference must
exceed to be real on this box: about ±8 % on these ~0.1 s rows.

## 2. Where an inherited call's time goes today (Q2)

sb-sprof (`:cpu`, 0.5 ms) of each row at N = 4 M on the base core, self samples
grouped (`scratch/s534a/group-prof.pl`, profiles `scratch/s534a/prof/base-*.prof`):

| group | `methinh3` | `methinh` | `methpoly` | `ovlbool` | `methret` |
|---|---:|---:|---:|---:|---:|
| `find-symbol` (`pl-NAME` and `@isa`, by string, per class) | **32.5 %** | 26.7 % | 24.3 % | 12.4 % | 6.2 % |
| weak-table LOCK (`%p-isa-symbol`'s unguarded `*p-stash-deleted*` probe) | **19.1 %** | 11.2 % | 11.4 % | 18.2 % | 0 |
| equal tables (stash memo, own-class cache, `pl-NAME` memo) | 19.9 % | 23.9 % | 21.8 % | 22.4 % | 28.4 % |
| walk bookkeeping (`visited` list, `"CORE"` test, `@ISA` reads) | 12.9 % | 6.2 % | 6.1 % | 0 | 1.4 % |
| prologue (`p-method-call`, class, normalisation) | 5.8 % | 13.9 % | 9.4 % | 0 | 21.5 % |
| overload derive + call (`%p-true-p-slow`, `p-call-overload`) | 0 | 0 | 0 | 11.6 % | 0 |
| callee (the sub body, `@_`) | 3.3 % | 7.0 % | 10.0 % | 2.7 % | 27.2 % |
| loop + other | 1.2 % | 3.3 % | 7.3 % | 19.5 % | 13.1 % |

So **~85 % of an inherited call is the lookup** (methret, the own-class shape,
spends 27 % in its callee; methinh3 3 %).  No allocation is involved on the
method rows (the walk's `visited` conses are a fraction of the bookkeeping);
`ovlbool`'s "other" is mostly `p-call-overload`'s two fresh argument boxes and
the `(cons cls op)` key `p-find-overload` builds for every lookup.

**One finding needs no cache at all**: `%p-isa-symbol` -- the `@ISA` reader
every walk calls once per class -- asks the WEAK table `*p-stash-deleted*`
with NO count guard, and every SBCL weak-table operation takes the global
system lock.  Its sibling `%p-stash-deleted-p` has the guard; this reader does
not.  That is 11-19 % of every inherited call and the dispatch-path half of
#4501 (lever (d) below, task #4525).

## 3. The options

### (a) A cheaper walk, no cache of the answer

Per class name a record -- its package, its interned `@isa` symbol, and per
method name the `pl-NAME` symbol INTERNED in that package whether or not a sub
exists -- so each step of the walk is a table probe and an `fboundp`, never a
`find-symbol`.  `@ISA` is still read on every call, so an `@ISA` edit is seen
with nothing invalidated.  Prototype: `scratch/s534a/rt/proto-a.lisp`
(`proto-a.diff`, +60 lines; built by `scratch/s534a/mk-proto-a.pl`).

**The deciding question -- does interning a symbol for a sub that does not exist
become visible to Perl?  NO, and perl itself is MORE visible here than PCL.**
Probe `scratch/s534a/pr/intern.pl` (before/after three inherited calls and one
missing-method die, on `B` and `C` of `C isa B isa A`):

| what the program asks | perl 5.40.3 after the calls | PCL base | (a) | (b) | (c) |
|---|---|---|---|---|---|
| `keys %C::` | `AUTOLOAD,ISA,can,m,nosuch` (perl's method-cache globs) | `ISA` | `ISA` | `ISA` | `ISA` |
| `exists $C::{m}` | 1 | 0 | 0 | 0 | 0 |
| `defined &C::m`, `exists &C::m`, `*C::m{CODE}` | 0, 0, 0 | 0, 0, 0 | same | same | same |
| `C->can('m') == \&A::m`, `UNIVERSAL::can` | A, A | A, A | same | same | same |
| a Data::Dumper of the stash keys | grows | unchanged | unchanged | unchanged | unchanged |

perl's `gv_fetchmeth` CACHES an inherited method as a glob in the calling
class's stash (and a negative entry for a miss), so `keys %C::` grows in perl;
PCL's stash view lists only symbols with a sub body or a bound cell
(`p-stash`, `%p-stash-variable-name`), so an interned, unbound `pl-NAME` is
invisible to every Perl-level question.  PCL already differs from perl here,
in the direction no program can trip over; interning changes nothing.  A side
table keyed on (package, name) is therefore not needed for (a) or (b).

The two runtime operations that could make a stored symbol stop answering for
its name are `unintern` / `delete-package` (never called) and a re-homing
`shadow` / `shadowing-import`: `%p-reserve-sub-name` and `%p-symref-intern`
shadow only an INHERITED name (a cache never stores one: both prototypes decline
a symbol whose home is another package), and `p-import-perl-symbol`'s last arm
`shadowing-import`s a function-less symbol, which uninterns a local one.  That
last case is LATENT for the shipped own-class cache too; the batch that builds
(b) adds the one-line flush there (§6).

### (b) A cached resolution per (class, method), validated on every hit

The own-class table (s473s) gains INHERITED entries: (class, method) -> the
resolved symbol plus one LINK per class the walk passed before the hit -- that
class's `@isa` symbol, a COPY of the parent names it read, and its own interned
`pl-NAME` symbol.  A hit re-reads every link: the `@ISA` contents compared
name by name with the copy (any write, by any path, is seen because nothing is
trusted), the `pl-NAME` symbol still has no live sub (a method defined later
in a closer class wins), and the found symbol is still live.  NOTHING is
invalidated anywhere and NO array writer changes; the only inputs not re-read
are a name's package (changed only by `p-stash-assign`, which flushes the
table) and a symbol's identity.  The overload lookup gets the same links plus
one fixnum: every handler comes out of `*p-overload-table*`, whose only writers
(`p-register-overloads`, `%p-overload-unimport`) bump `*p-overload-gen*`; misses
are cached too.  Prototype: `scratch/s534a/rt/proto-b.lisp` (`proto-b.diff`,
+157 lines).

### (c) #582 as designed: a generation bumped by every `@ISA` write

The same entries, but validated by ONE fixnum compare against a global
generation that every write to an `@ISA` array bumps; the per-link `pl-NAME`
shadow test stays (it is what makes definitions free, s473s), only the `@ISA`
comparison goes.  The design's precondition is a MARK on the `@ISA` array that
every array writer tests -- and that is where it fails:

* **An `@ISA` is not a box.**  It is a bare adjustable CL vector; the box magic
  hook (`reference_box_magic_hook`) is on SCALARS.  A vector has no spare slot
  for a mark, so the mark is a membership test in an EQ table -- and since every
  package has an `@ISA` (p-defpackage makes one), the table is never empty, so
  every array write in every OO program pays a hash probe.  (A weak table would
  also take SBCL's global system lock on every probe -- #4501.)  Cost measured
  in §4 on the array rows.
* **The audit is 217 runtime definitions wide**, plus the compiler's open-coded
  element stores: `scratch/s534a/audit-writers.pl` counts every top-level
  definition in `cl/*.lisp` whose body holds a vector-writing primitive
  (`vector-push-extend` 70, `(setf (aref` 41, `sort` 31, `(setf (symbol-value`
  25, `(setf (svref` 21, `(setf (fill-pointer` 21, `%p-array-store-scalar` 20,
  `(replace` 19, ...); `Pl/` emits direct vector writes from four files.  The
  `@ISA` write spellings it must cover: push / pop / shift / unshift / splice /
  list assign / element store / `$#ISA =` / `undef @ISA` / `local @ISA` entry
  AND restore / `local $ISA[0]` / `*C::ISA = \@x` / `*C::ISA = [...]` /
  `local *C::ISA` / a write through a reference (`@$r = ...`, `push @$r`,
  `$r->[0] = ...`) / a symbolic `@{"${c}::ISA"}` (the CPAN idiom: base.pm,
  parent.pm, Moo) / `delete $C::{ISA}` / an element modified in place
  (`$ISA[0] .= ...`, `=~ s///`, `chomp`, `tr`, lvalue `substr`) / aliasing
  writes (`$_ = ...` in `for (@ISA)`, `map`/`grep`, `\$ISA[0]`, `@_` of a call
  `f(@ISA)`) / `@ISA = sort ...` / `tie @ISA` / XS through the bridge
  (`cl/pcl-xs.lisp`, 6 definitions) / a stash alias `*C:: = *D::`.
* **A missed writer is SILENT** (rule 12 cannot fire: the stale entry looks like
  every other hit).  The only loud form is a CHECK MODE that runs the full walk
  on every hit and dies on a disagreement, over the gate, the sweep, the
  companion and the board -- which proves only the spellings those populations
  happen to exercise.

Prototype: `scratch/s534a/rt/proto-c.lisp` (`proto-c.diff`, +189 lines, derived
from (b) by `mk-proto-c.pl`, `-c2` the writer tests in the nine writers the
array rows reach, `-c3` / `-c4` the marks at the `@ISA` cells, `local @ISA` and
`*ISA =`) -- deliberately the OBVIOUS audit, to measure what it misses (§5).

### (d) Found by the profile: guard `%p-isa-symbol`'s weak-table probe

One line, answer-identical: `(not (%p-stash-deleted-p sym))` in place of the
bare `(gethash sym *p-stash-deleted*)` -- an empty table answers NIL either
way, and the count test keeps the lock out while nothing was ever deleted.
Prototype `scratch/s534a/rt/proto-d.lisp` (`mk-proto-d.pl`).  It helps every
`@ISA` reader, cached or not (`can`, `isa`, `SUPER::`, AUTOLOAD, the overload
walks).  A program that deletes ANY stash entry (namespace::clean, YAML::Tiny,
Moo's cleanups) makes the count non-zero for the rest of its run; the table's
keys are interned symbols, which their package keeps alive, so it need not be
weak at all -- that half is #4501's.

### (e) = (b) + the MISS

The PPI parse moved only 6.6 % under (b), because PPI's hottest inherited call
is a MISS: `Params::Util::_INSTANCE` asks `->isa(CLASS)` of every element, and
`->can` / `->DOES` / `->import` / AUTOLOAD are misses too -- no class in the
chain defines them, so the call walks the whole chain, then UNIVERSAL's, then
answers from a built-in.  (b) caches hits only, and its recorder made a miss
walk TWICE.  (e) records the UNIVERSAL walk too and caches a MISS entry with
the links of both walks; a valid miss goes straight to the built-in answers
(`%pcl-after-chain-miss`, the same `cond` in the same order).  A method
defined later anywhere in either chain, or any `@ISA` change in either, fails
the links exactly as for a hit.  Prototype `scratch/s534a/rt/proto-e.lisp`
(`mk-proto-e.pl` over (b), +184 lines over the base).

## 4. The prototypes measured (Q3)

Every number is a RUNTIME A/B of one transpile: `BENCH_RT_B=scratch/s534a/rt/proto-X.lisp
perl tools/bench-exec.pl ...` (A = the base runtime, B = the prototype, all series
interleaved, best of 5, through `heavy.sh s534a bench`, which waits for a quiet box);
the PPI parse is the same tool over one row (`scratch/s534a/bench-ppi.pl`: four parses
of `lib/Math/BigInt/Calc.pm` with `find('PPI::Token')`, token count verified against
perl).  B/A in %, negative = faster; the control band (B = a copy of A) is about
±8 % on the ~0.1 s rows (§1).  Logs: `scratch/s534a/out/bench-X.log`.

| row | (a) walk records | (b) cache, hits | (c) generation | (d) weak guard | (e) = b + misses | **(f) = e + ancestry** |
|---|---:|---:|---:|---:|---:|---:|
| `methret` | -2.9 | -5.9 | +11.6 | -3.8 | +2.4 | -6.7 |
| `methinh` | -12.6 | -65.6 | -66.5 | -18.1 | -65.5 | **-65.1** |
| `methinh3` | -33.2 | -78.6 | -82.1 | -23.8 | -77.3 | **-77.3** |
| `methpoly` | -8.9 | -63.7 | -64.3 | -16.3 | -63.1 | **-63.6** |
| `ovlbool` | -3.3 | -66.6 | -73.1 | -31.5 | -66.8 | **-67.2** |
| `ovlsub` | +1.3 | +3.2 | +0.1 | +2.3 | -1.7 | -2.4 |
| `json-rt` | +0.7 | -4.2 | +6.8 | +1.1 | +0.1 | +2.0 |
| `moo-objs` | -1.8 | +11.7 | +1.2 | -0.8 | -0.9 | -2.6 |
| **PPI parse** | +2.2 | -6.6 | **+19.0** | -10.1 | -24.1 | **-37.3** |
| run (uptime load) | 02:38, 1.5-1.9 | 02:07, 1.9-2.9 | 02:59, 1.1-2.0 | 02:43, 1.9-3.0 | 02:54, 1.1-1.9 | 03:03, 1.7-2.4 |

What the rows mean in perl's terms (f, the base A column of its own run): `methinh`
0.367 -> 0.128 s = **3.87× -> 1.35×** of perl; `methinh3` 0.700 -> 0.159 s = **7.38×
-> 1.68×**; `methpoly` 3.45× -> 1.26×; `ovlbool` 6.34× -> 2.08×; the PPI parse
3.06 -> 1.92 s per four parses = **4.38× -> 2.75×** of perl.  The `moo-objs` +11.7
under (b) is one reading outside the band that no other run repeats (e and f carry
(b)'s whole method half: -0.9, -2.6); `methret` +11.6 under (c) is the same row's
noise next to (c)'s writer tests.

**(c)'s array rows** (`out/bench-c-array.log`, 02:14, load 1.9): the mark test on
the per-element writers -- `arrhash` +6.4, `pushloc` +7.9, `shiftq` +14.0,
`unshiftq` +6.6, `splice0` +2.7, `arrfill` +4.9; whole-array assignment pays one
test per assignment (`listcopy` -0.1, `sliceasgn` -0.3, `sortnum` -2.0).  And on
real code the writer tests cost more than the generation saves: the PPI parse is
**+19 % SLOWER** under (c) (the tokenizer pushes onto arrays all the time), while
(c)'s dispatch rows beat (b)'s by only 1-6 points (`methinh3` 0.116 vs 0.169 s).

**Where PPI's remaining time is** (`scratch/s534a/prof/ppi-base.prof`, `ppi-proto-b.prof`,
six parses under sb-sprof): the method lookup groups are 18.4 % of the samples on
the base (find-symbol 5.7, the weak lock 5.4, equal tables 7.1); SBCL COMPILING
code is ~20 % (`sb-c::ir1-optimize` and friends -- `PPI::Node::find` builds its
matcher with a STRING EVAL on every call, the cheap lever (a) of
`docs/eval-in-cl-plan.md` §4.6, outside this evaluation); the rest is the generic
boxed code.  That is why the PPI gain needed the MISS path (e: `->isa` through
`_INSTANCE`) and the cached ancestry (f: `->isa`'s answer walked the whole chain
again).

## 5. The probe matrix (Q4)

`scratch/s534a/pr/gen-matrix.pl` writes `matrix.pl`: 57 cases, each with its OWN
class family (no case sees another's edits), each calling BEFORE and AFTER an
event so a cache that answered the first call is asked the second.  Expected =
real perl 5.40.3 (`matrix.perl`); run on each core by `scratch/s534a/run-matrix.sh`.

__MATRIX__

## 6. Recommendation

**Build (f) + (d), in one runtime-only batch: #4523 (the method half -- hits,
misses, the ancestry), #4524 (the overload half), #4525 (the one-line guard).**

* **Why (f), not (a) or (c).**  (a) keeps the walk and makes each step cheaper:
  -9 to -33 % on the method rows, nothing on overloads or PPI.  (c) is (b) with a
  counter instead of the `@ISA` comparison: 1-6 points better on the dispatch rows,
  paid for on EVERY array write by every program (+3 to +14 % on the per-element
  writer rows, PPI +19 %), and correct only if an audit of 217 writing definitions
  plus the compiler's open-coded stores misses nothing -- with no loud failure when
  it does.  (f) needs no audit because it trusts nothing it did not just re-read.
* **Expected gain** (measured, §4): `methinh` 3.87× -> 1.35×, `methinh3` 7.38× ->
  1.68×, `methpoly` 3.45× -> 1.26×, `ovlbool` 6.34× -> 2.08×; the PPI parse
  -37 % (4.38× -> 2.75×) -- PPI under PCL being the gate for running the compiler
  in the image (`docs/eval-in-cl-plan.md` P1-P2).  (d) on its own: -16 to -31 % on
  the same rows and -10 % on PPI; inside (f) it still serves every uncached walk
  (`can`, `SUPER::`, AUTOLOAD, the first call of each pair).
* **Risk and guard.**  The residual inputs (a name's package, a (package, name)
  symbol) each have ONE writer that flushes (`p-stash-assign`; the
  `shadowing-import` arm of `p-import-perl-symbol`, whose flush also closes the
  latent hole in the shipped own-class cache).  The guard is the probe matrix as a
  `Pl/t` file, a CHECK MODE run once over the gate, the sweep and the board (every
  hit also takes the full walk and dies on a disagreement), and an inverse
  mechanism row.  The cost of a validated hit grows with the number of classes the
  walk passed (one name compare + one `fboundp` each) -- for a wide hierarchy whose
  method sits in the last parent that is still cheaper per class than today's walk.
* **Cost: one session** (the prototypes are ~200 lines and run the matrix
  already).  **Bar:** #4523's (the four inherited rows + `methret` `ovlsub`
  `moo-objs` `json-rt` A/B with a control column, the PPI parse BEFORE -> AFTER,
  gate + full sweep + the mro / method / universal / overload companion legs, the
  check-mode runs, EVERYDAY).
* **Not in this lever:** `p-call-overload`'s two fresh argument boxes per call
  (part of `ovlbool`'s "other" in §2, not measured on its own), `SUPER::` (uncached, unchanged), and PPI's
  string-eval matcher -- each its own measurement.

## 7. What NOT to retry

* **Invalidation on `@ISA` writes (#582's original design, option (c))** -- measured:
  the mark is a hash probe per array write (an `@ISA` is a bare vector, not a box),
  PPI +19 %, and the obvious audit left 12 of 61 probe cases stale.  Do not retry
  unless `@ISA` stops being an ordinary vector.
* **A side table keyed on (package, name) instead of interning** -- unnecessary:
  an interned, unbound `pl-NAME` is invisible to every Perl-level question (§3 (a));
  perl's own stash shows MORE after a method call.
* **A hit-only cache** -- (b) gained 6.6 % on PPI because the hot inherited call
  there is a MISS (`->isa`); cache misses with the same links (e).
* **The per-call-SITE inline cache** -- still not needed (USER s444); nothing here
  changes the reason: a per-(class, method) table answers a polymorphic site
  (`methpoly`) as fast as a monomorphic one.
* **(a) alone** -- a cheaper walk is worth a third of (f) on the deepest row and
  nothing on overloads or PPI.

## 8. Found on the way (filed)

* **#4521** -- `use parent` / `use base` inside a `do BLOCK` or `eval BLOCK`
  that switches package dies `The function common-lisp:nil is undefined`: the
  use's preamble forms are emitted as ARGUMENTS of the block's `funcall` (matrix
  row 45).
* **#4522** -- an inherited OVERLOAD is resolved breadth-first (every direct
  parent's table first) where perl walks the class's MRO: `C isa (B1, B2)`,
  `B1 isa A1`, A1 and B2 both overload -- perl takes A1's, PCL B2's (silent).
  The method walk is depth-first; the two must share ONE order.
* Cited, not refiled: **#3631** (`use mro 'c3'` is ignored by a non-empty-`@ISA`
  dispatch, and `docs/not-supported.md`'s "PCL always uses C3" is stale; matrix
  row 35), `next::method` (not-supported `ns-mro`; row 36), **#1530** (`undef
  &C::m` removes the sub; rows 26-27), **#960(a)** (`fallback => 0` does not
  refuse; row 56), the stash alias after a bless (ir-spec §7.3: run-time
  resolution answers the alias's target; row 52).
