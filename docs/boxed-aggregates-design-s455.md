# Boxed aggregates → raw element storage: the design analysis (s455d, Fable)

**Status: DESIGN COMPLETE AND HANDOFF-READY (2026-08-31).  The
sort-comparator ruling is in (§7.1, USER); the one remaining §7 item (the
gate shape, Q2) is DELEGATED to the implementing session by design — it
decides with a measurement in phase 0.  Execution: an Opus session takes
phases 0–2 (zero-change bars), a second takes phase 3 (the flip + the full
boat), Fable reviews at each boundary.  Task #816.  CHECKPOINT LOG at the
bottom.**

The question this doc answers: Perl array/hash ELEMENTS are stored as boxes
today, unconditionally.  The measured cost is the whole remaining
aggregate-class loss (bench: arrhash 1.42×, arrfill 3.9×, slices 4.8×,
sliceasgn 2.6×; catalogue §5: raw element read ~3.5×, fill-pointer push ~7×;
the pack-oracle profile: `%make-p-box` 14 % of a plain-data loop).  What
representation change removes that cost without breaking the aliasing
semantics the boxes exist for — and in what order do we get there?

---

## 1. The one-sentence design (established up front, argued below)

**Store elements RAW by default; promote an individual element to a box IN
PLACE at the moment something needs its identity (an alias event); make
every write honor "slot holds a box → write THROUGH it, else store raw."**

This is not a new invention for PCL — three pieces already exist in the
runtime, which is the strongest evidence the shape is right:

1. **The read paths are already raw-tolerant, normatively.**  ir-spec §2.3:
   "Elements are boxes (or nil for holes; elements of proven-safe
   intermediate lists MAY BE RAW VALUES)"; §2.4 for hashes: "Values are
   boxes or raw values".  `p-aref`/`p-gethash` unbox scalars on read — an
   unboxed slot passes through `unbox` as identity.
2. **The promotion mechanism already exists**, verbatim, in
   `p-aref-argbox` (cl/pcl-runtime.lisp:7177): an element in @_-aliasing
   argument position that holds a RAW value is promoted —
   `(setf (aref a actual-idx) (make-p-box elem))` — and the box is both
   stored in the slot and handed to the callee.  That IS the element-cell
   promotion this design generalizes.  (`p-gethash-argbox` is the hash
   twin.)
3. **The lazy-alias mechanism already exists** for holes:
   `%p-defelem-box` (line 7131) — a magic cell whose first WRITE vivifies
   the slot and re-dispatches through box-set.  perl's own defelem magic,
   already shipping (s316e).  It generalizes to "alias of a raw slot that
   must not promote eagerly" if we ever need that (see §4.3 @_ flattening).

The js-target-plan Part II (s453) independently arrived at the same shape
for the JS backend: "Elements stored RAW; `cell(i)` promotes element *i* to
a Box in place ... reads unwrap transparently."  This doc is the CL-side
design that makes that sentence true at home first.

## 2. Why elements are boxed at all — the obligations

A box in an element slot serves exactly one master: **identity** — some
OTHER holder may write through it, or observe writes to it.  The complete
list of makers of such identity (the alias events):

| # | event | today's mechanism | raw-default consequence |
|---|---|---|---|
| E1 | `f($a[0])` / `f($h{k})` — @_ aliasing of one element | `p-aref-argbox` / `p-gethash-argbox` (emitted only in user-sub arg position, ir-spec §5) | ALREADY PROMOTES a raw slot.  No change. |
| E2 | `f(@a)` — @_ aliasing of EVERY element via flatten | flatten passes the slot boxes; `%p-flatten-list`/`p-flatten-args` | the expensive case — §4.3.  Options: promote-all on flatten (bad), writes_args-gated (facts exist, Parser2.pm:1766), or defelem-style lazy view (mechanism exists). |
| E3 | `foreach my $x (@a)` — loop var aliases each element | `%p-foreach-elt` (line 8556): box slot → alias; **raw slot → FRESH TEMPORARY (writes NOT aliased)**; hole → defelem | **SEMANTICS FLIP REQUIRED**: the raw arm is correct only while raw slots are provably unaliased intermediates.  Under raw-default it must PROMOTE (one-line change: the argbox pattern). |
| E4 | `\$a[0]` / `\$h{k}` — explicit element ref | `p-aref-box` / `p-gethash-box` (the eager lvalue accessors — vivify + return the slot box) | already the promotion shape; verify the raw arm promotes rather than wrapping a detached copy. |
| E5 | `local $a[0]` / `local $h{k}` | the local-element machinery (V's #541 family) saves/restores through the slot | must promote first, or save/restore raw values by slot index (either is correct; promotion is simpler). |
| E6 | sort comparator `$a`/`$b` | bind to elements | perl documents element modification during sort as undefined; READS dominate.  Verify current binding; raw slots may bind raw (fast) with promotion only if the comparator writes (rare; may DIE instead — decide in §6). |
| E7 | `map`/`grep` `$_` aliasing | same family as E3 | same fix as E3. |
| E8 | array holes aliased | `%p-defelem-box` | unchanged (holes are nil either way). |
| E9 | tie/magic arrays (`tie @a`, `@-`/`@+`) | tied storage + magic element boxes (#680 relies on @-/@+ element boxes being MAGIC) | OUT OF SCOPE: tied/magic aggregates keep today's fully-boxed representation.  The promotion design applies to PLAIN containers only; the dispatch already distinguishes them. |
| E10 | `Internals::SvREADONLY` arrays | simple-vector storage, boxed elements | raw storage fine; element writes still legal (perl), same write rule. |

**The write rule that keeps every alias correct** (the design's one
invariant): *a slot that holds a box is written THROUGH (`box-set`), never
replaced; a slot that holds a raw value or nil is stored raw.*  Once
promoted, always promoted — promotion is monotone per slot, so an alias
taken at any time stays live, exactly as today.  (`@b = @a` COPIES —
list-assign reads values (unboxing), never shares slots — so copies do not
propagate promotion; that matches perl, where copying breaks aliasing.)

## 3. What gets faster, concretely

The allocations that stop happening on the fast path (none of which serve
identity in the common program):

- `$a[$i] = $v` / `$h{k} = $v` — today vivifies/box-sets a slot box; raw
  stores a value.  (The catalogue's 3.5× read / element-write win.)
- `push @a, $v` — today `make-p-box` per element (the ~7× fill-pointer gap
  is partly this, partly `p-push-impl` dispatch).
- list assignment / literal construction `@a = (1..1000)`, `my %h = (...)`
  — one box per element today.
- every READ keeps its current shape (`unbox` of a raw value is identity)
  but stops chasing a pointer + touching box fields — cache locality.
- hash VALUES same as array elements (keys are already raw strings).

What does NOT get faster here: scalars (the #758–#761/#811/#815 territory),
tied/magic containers (out of scope), and slices' remaining cost beyond
element boxes (slice machinery itself — measure again after).

---

## 4. The mechanism, decided per event

### 4.1 The two rules every path shares

- **READ**: `unbox` of a raw value is identity — every existing read path
  (`p-aref`, `p-gethash`, flatten-for-print, coercions) already handles a
  raw slot.  No read site changes.
- **WRITE** (the invariant from §2): `slot holds p-box → (box-set slot v)`;
  else `(setf slot v-raw)` where `v-raw` = `(unbox v)` — storing a VALUE,
  never someone else's box (storing a caller's box would create aliasing
  perl doesn't have; today's box-set into a fresh slot box has the same
  copy semantics).  One `p-box-p` branch per element write, replacing
  today's vivify-or-box-set — strictly less work.

### 4.1a The write arms, ENUMERATED (s457ai, phase 1)

The sizing table (§5) warned that "element WRITES have ≥2 lowerings".  Counted
in the runtime, they are FOUR value-store arms plus a fenced set of cell
producers — and the two element-assignment *lowerings* (`p-setf` and the
Kind-A `elem-setf` CL `setf`) both land in the same function, so they are one
arm, not two:

| # | arm | reached from | write rule |
|---|---|---|---|
| A1 | `(setf p-aref)` | `$a[i] = V` via `p-setf` **and** via CL `setf` (`elem-setf`); the array-slice assign arm of `p-setf` | box → `box-set`; else raw-or-fresh-box |
| A2 | `%p-array-store-scalar` | `push` / `unshift` / `p-splice-impl` / `p-array-fill` (`@a = …`) / `p-array-init` (`[…]`) / the flatten walkers | APPEND — no existing slot, so the gate alone decides |
| H1 | `(setf p-gethash)` | `$h{k} = V` (both lowerings); the hash-slice assign arm | box → `box-set`; else raw-or-fresh-box |
| H2 | `%p-make-hash-entry` | `p-hash` (`{…}`) / `p-hash-fill` (`%h = …`) | NEW slot — the gate alone decides |

Raw-storability is its own predicate, `%p-storable-raw`: only a plain number,
or a plain string with no cached numeric half, may live unboxed.  Everything
else carries identity on the CONTAINER — a bless class, the `is-ref` flag
(`\$x`, `\*foo`), a dualvar's two halves, a magic cell, a tie proxy, a
box-in-box scalar ref — and keeps its box.  That is exactly the split the
READING side already makes (`p-aref-unbox-elem` / `%p-hash-unbox-elem` return
the box for those kinds and the bare value otherwise), which is why no read
path changes.  `nil` is never a legal raw value, so "not raw-storable" and
"hole" cannot be confused.

Unchanged by design: the CELL producers (`p-aref-box`, `p-gethash-box`,
`p-aref-argbox`, `p-gethash-argbox`, `%p-elem-cell`, `%p-hash-elem-cell`) store
a BOX because the box IS the cell; the defelem setters (`%p-defelem-box`,
`%p-hash-defelem-box`) likewise; and `%p-extend-to` writes `nil` holes.
`cl/pcl-xs.lisp`'s `xs-av-store` / `xs-hv-store` are the same family and are
phase 3's business (`xs-av-fetch` already promotes — §7 Q4).

### 4.2 Promotion — ONE function

`%p-elem-cell (vec i)` / hash twin: if slot holds a box, return it; if raw
(or nil under lvalue rules), `(setf (aref vec i) (make-p-box raw))`, return
the box.  This is `p-aref-argbox`'s existing arm, extracted and named; the
consumers are E1 (argbox — already does it), E3/E7's aliasing arm, E4
(`p-aref-box`/`p-gethash-box` raw arm), E5 (`local` element).  Promotion is
**monotone**: nothing ever demotes a slot (a demotion would detach live
aliases).  Whole-container assignment REPLACES slots wholesale with raw
values — that is not demotion, it is perl's own copy-breaks-aliasing.

### 4.3 E2, the flatten-into-@_ problem — amortized monotone promotion

`f(@a)` must let the callee write `$_[N]` through to `@a`.  Three options
weighed:

- *(a) lazy defelem-style view per element* — allocates a cell per element
  per CALL: same cost as today's flatten or worse.  Rejected.
- *(b) pass raw values* — breaks @_ writes silently.  Rejected outright
  (the one unaffordable failure mode).
- **(c) promote-on-flatten (CHOSEN)**: flattening a plain array into a
  user-sub argument list promotes each raw slot ONCE (`%p-elem-cell`) and
  passes the boxes, exactly today's @_ shape from there on.  The cost is
  one allocation per element the FIRST time; an array repeatedly passed
  whole converges to all-boxed — i.e. to today's representation, never
  worse.  Arrays that are never flattened into calls (loop accumulators,
  temporaries, build-and-return lists — the hot-path population) stay raw
  forever.
- **(c′) the fast path over (c)**: when the callee is KNOWN and its
  `writes_args` fact (Parser2.pm:1766, shared scan with VarAnnotator:855)
  is false, pass raw values without promoting — reads of `$_[N]` work on
  raw values as they do everywhere.  Same closed-world conditions as every
  sub_info consumer.  This is an optimization on top of (c), not a
  correctness requirement, and can ship later.

Builtins never alias their list args through @_ (they are language, and
`p-flatten-args` for builtins reads VALUES) — builtin calls do not promote.
The exceptions that WRITE their operand aggregate (`sort` in-place,
`splice`, `reverse @a` lvalue…) operate on the container, not through
element identity — container ops follow the write rule per slot.

### 4.4 E3/E7, foreach/map/grep binding — two arms

- **Proven arm**: the VarAnnotator's existing unboxable verdict for the
  loop VARIABLE (the same facts that drive `p-foreach-range-raw`, extended
  to foreach-LIST) ⇒ bind the RAW slot value directly.  No promotion, no
  allocation — the read-only loop over a raw array touches nothing.  This
  arm also finally gives #810's read-only literal-list case its honest
  shape.
- **Unproven arm**: `%p-foreach-elt`'s raw case flips from "fresh
  TEMPORARY" (correct today only because raw slots are provably-unaliased
  intermediates; a silent-wrong under raw-default) to **promote in place
  and bind the box** — writes through the loop var reach the slot, exactly
  perl.  Holes keep the defelem arm unchanged.

### 4.5 What stays fully boxed, permanently

Tied containers, magic containers (`@-`/`@+` — #680's in-place magic boxes
are the semantics), `%ENV`-marker machinery (#736 family), and any
container the tie/magic dispatch already routes specially.  The promotion
design touches PLAIN containers only; the existing dispatch is the fence.

### 4.6 Emission side

Almost none.  The representation change is runtime-internal: accessors and
their setf expansions change ARMS, not names.  Candidate Kind-A gate
(`raw-elems`, PCL_OPT-switchable) for the WRITE-side raw arm so the whole
design is bisectable at runtime — but note the storage is shared state:
mixing gate-on and gate-off code over one container is fine BY DESIGN
(boxed slots are always legal; the gate only decides whether writes
manufacture boxes), which makes the A/B honest.  `PCL_OPT=none` must
reproduce today's all-boxed behavior bit for bit.

---

## 5. Sizing (perl-tests corpus emission, static site counts, s455d)

Transpiled at `eab6c97`, grep over the emitted CL (indicative, not
per-execution weights):

| family | sites | reading |
|---|---:|---|
| plain element reads (`p-aref`/`p-gethash`) | 926 | unchanged by design; get faster (no pointer chase) |
| E1 argbox (already-promoting @_ element args) | 443 | volume proof the promotion arm is battle-tested |
| E3 list-foreach (`p-foreach`) | 324 | the biggest alias-relevant population → the two-arm strategy pays here |
| builtin flatten (`p-flatten-args`) | 292 | reads values; never promotes |
| `push` | 91 | pure fast-path win |
| lvalue element accessors (`p-aref-box`/`p-gethash-box`) | 73 | plus the `elem-setf` Kind-A lowering's CL-setf arm — element WRITES have ≥2 lowerings; the write rule must land in each (enumerate in phase 1) |
| static `make-p-box` | 3476 | how box-saturated the emission is overall |

Owed measurements for the implementing session (cheap, listed so they are
not re-derived): the E2 population (user-sub calls flattening a whole
array) — countable from the user-sub call lowering; `writes_args`-true
frequency across sub_info; per-execution box-allocation counts before/after
(sb-sprof alloc profile on arrhash/arrfill).

**Two events the §2 catalogue must also carry (caught while sizing):**

- **E11 `values %h` / `each` aliasing**: `for (values %h) { s/a/b/ }`
  WRITES THROUGH to the hash in perl.  The list builder for aliasing
  constructs (`values`, and E12) must pass promoted cells (or existing
  boxes), never raw copies — same two-arm treatment as E3: proven
  read-only body ⇒ raw values; else promote each visited slot.
  **PROBED (s455d, 90da9e8): ALREADY BROKEN on today's fully-boxed tree**
  — perl `v=1x`, PCL `v=1`; the values list builder COPIES.  Filed **#817**
  (pre-existing, fixable independently; phase 2 is also its fix).
- **E12 slice-in-foreach / slice lvalues**: `for (@a[1..3])`,
  `@a[0,1] = ...` — slice machinery aliases elements; its element
  collection routes through the same `%p-elem-cell` promotion on the
  lvalue/alias paths and raw values on the value paths.
  **PROBED: the foreach half is ALSO ALREADY BROKEN today** — perl
  `sl=10 20 3`, PCL `sl=1 2 3`.  Filed **#818** (sibling of #817; the
  lvalue-slice half IS correct today).  Direct-array foreach (`fe=6 7`)
  and @_ aliasing (`ar=p! q`) probed CORRECT — the misses are exactly the
  intermediate-list builders, which sharpens phase 2's worklist.

## 6. The phased plan (each phase lands alone, with its bar)

The ORDER is the design's safety argument: every consumer is hardened for
raw slots while no raw slot can yet exist, so each early phase must be a
ZERO-CHANGE sweep — any movement is a bug in that phase, cheaply
attributed.

- **Phase 0 — the battery + the gate skeleton.**  A `Pl/t` differential
  battery vs real perl covering E1–E8+E11+E12 (both spellings each: write
  through `$_[0]`, foreach-var write, `\$a[i]` write-after, local-element
  restore, values-write, slice-alias write, hole defelem, copy-breaks-
  aliasing `@b = @a`).  Register the Kind-A gate `raw-elems`, default OFF,
  in Pl/Passes.pm — but note it is consumed by the RUNTIME write arms
  (a runtime-consulted gate: settle the PCL_OPT plumbing here; the
  registry's names are compile-time today).  Bar: battery green vs perl on
  the UNCHANGED tree (it must pass BEFORE the design so it can catch the
  design).
- **Phase 1 — the write rule, inert.**  Every element-write arm (the
  `p-aref-box`/`p-gethash-box` setf expansions, the `elem-setf` CL-setf
  lowering arm, push/unshift/splice/list-assign/literal-construction
  stores, hash stores) gains the `p-box-p slot` branch.  All slots are
  still boxes ⇒ behavior identical.  Bar: full sweep TOTAL/fail-rows
  byte-identical; battery green; bench unchanged (branch cost noise).
- **Phase 2 — alias consumers promote, inert.**  `%p-elem-cell` extracted
  from `p-aref-argbox`; `%p-foreach-elt` raw arm flips to promote (the E3
  semantics flip); `p-aref-box`/`p-gethash-box`/local-element raw arms
  promote; flatten-into-user-sub promotes (E2c); values/slice alias paths
  (E11/E12) promote.  Still no raw slots exist ⇒ zero-change bar again
  (sweep + gate-SET + battery).
- **Phase 3 — THE FLIP.**  `raw-elems` ON by default: the write arms store
  raw.  Bar: the full boat — battery byte-identical to perl, full sweep,
  companion op/+uni/+re/+io/ legs, board, census, `PCL_OPT=none`
  equivalence (none ⇒ all-boxed world, bit-identical to phase-2 tree),
  bench (targets: arrhash ≤1.0×, arrfill ~1×, push-loop toward the 7×
  win), ir-spec §2.3/§2.4 rewritten (normative change), generation bump +
  artifacts.
- **Phase 4 — the fast paths on top.**  E2c′ (`writes_args`-gated raw
  pass), E3 proven-arm (annotator verdict extension to foreach-LIST vars —
  closes #810's family honestly), slices re-measure (how much of 4.8× was
  element boxes vs slice machinery), sort-comparator decision (§7).

Sizing: phases 0–2 ≈ one Opus session (mechanical, zero-change bars);
phase 3 ≈ one session dominated by its measurement boat; phase 4 ≈ one
more.  Fable reviews at each phase boundary (the round pattern).

## 7. Open questions (for the USER / the next Fable session)

1. ~~sort comparator writes (E6)~~ **RULED (USER, 2026-08-31: "I accept
   your suggestion")**: bind existing boxes as today when present, bind
   raw values RAW — a comparator write to a raw-element array is not
   written through, which sits inside perl's own "undefined behavior" for
   element modification during sort.  The implementing session adds the
   `docs/not-supported.md` entry (cite this doc + the ruling) in the
   phase-3 commit, since only the flip makes the case reachable.
2. ~~The gate's runtime consultation~~ **DECIDED (s457ai, phase 0, by
   measurement + an arm enumeration): a RUNTIME-CONSULTED GLOBAL, not an
   emission-keyed Kind-A gate.**  `pcl:*p-raw-elems*` is an `sb-ext:defglobal`
   whose value comes from `PCL_RAW_ELEMS` (empty or `0` = off), re-read from an
   `sb-ext:*init-hooks*` entry so a SAVED CORE cannot freeze the answer its
   build machine happened to have.  Three reasons, in order of weight:

   - **Structural.**  The enumerated write arms are RUNTIME functions that no
     emission site names (see §4.1a): `%p-array-store-scalar` is reached from
     push / unshift / splice / `p-array-fill` / `p-array-init`, and
     `(setf p-aref)` is reached BOTH from `p-setf` and, under the existing
     Kind-A `elem-setf` rule, from CL's own `setf` — the two lowerings the
     sizing table warned about turn out to land in the SAME function.  Keying
     those on emission means a `-raw` twin of each, which is rule 11's hard
     stop.
   - **Correctness.**  An emission gate has to enter the module-cache key, or
     a module transpiled under one setting gets loaded into a program running
     the other.  A runtime flag has no cache interaction at all, and mixed
     storage is legal by design.
   - **Cost: none measurable.**  With the gate OFF (phase 1, tools/bench-exec.pl,
     best-of-5, against a base tree at `0237940`; the run-to-run noise band on
     this machine is 7–10 %, measured by running the base twice):
     arrhash 0.1877 → 0.1814 s, arrfill 0.1892 → 0.1852 s,
     slices 0.3195 → 0.3203 s, sliceasgn 0.0717 → 0.0757 s.  The gate read is
     one memory load in a function that already allocates.

   `PCL_OPT` is a COMPILE-time registry (`Pl/Passes.pm` is a perl module the
   SBCL side never sees), so `raw-elems` is deliberately NOT registered there —
   the two are orthogonal knobs.  *Ask for Fable: if the project wants one
   knob, `Pl::Passes` could grow a runtime-consulted kind that emits a setter
   into the file preamble; that trades the cache-key hazard back in, so it was
   not done unasked.*
3. ~~Hash `values` in LIST-copy positions~~ **RESOLVED (probed, C4)**:
   `my @v = values %h` and `my ($first) = values %h` COPY in perl AND in
   PCL today (`copy=1 lst=2` both sides) — the position split already
   exists and is correct; only the foreach-alias position is broken
   (#817).  Phase 2 touches the alias position only.
4. ~~XS element-box assumptions~~ **RESOLVED (code-read, C4)**:
   `xs-av-fetch` (cl/pcl-xs.lisp:679) **already promotes a raw slot in
   place** — `(unless (p-box-p elem) (setf elem (make-p-box …) (aref a i)
   elem))`, comment "the ELEMENT BOX, not its value: that is what makes an
   lvalue fetch write through (rule O3)".  The XS bridge is raw-ready; no
   phase-3 blocker.  This is the THIRD live copy of the promotion arm in
   the tree (argbox, defelem-vivify, xs-av-fetch) — the design
   consolidates an existing idiom, it does not invent one.

## CHECKPOINT LOG (execution, s457ai — phases 0–2)

- **P0a (#817 + #818, `9c0cf75` + `baf8f29`):** the two live silent-wrongs are
  CLOSED, at the list-builder layer.  `p-values` / `p-aslice` / `p-hslice` /
  `p-kv-aslice` / `p-kv-hslice` hand out the container's own slots through
  `%p-alias-aelem` / `%p-alias-helem`, which is `p-aref-argbox` /
  `p-gethash-argbox` with a fall-back for containers that cannot hold a slot
  box.  **The key measurement that made this the right layer**: a plain `@a`
  in list context has ALWAYS handed out its slot boxes, and every copy
  consumer already unboxes — probed over twelve copy positions.  So this is
  the sibling shape, not a new one.  **The sweep caught the other half**:
  `@a[0,1] = @a[1,0]` needs perl's read-the-whole-RHS-first rule, which PCL
  already had inside `%p-flatten-list` and the slice-assign arms did not run;
  extracted as `%p-assign-snapshot`.  Cost measured and clawed back to parity
  (three hot-path narrowings, `baf8f29`), residual +6 % on sliceasgn.
  `Pl/t/elem-alias-01.t` is the phase-0 battery — E1–E12, both spellings,
  byte-compared against real perl at test time; the E11/E12 rows were RED
  before the fix.  Filed **#840** (the real `experimental.pm`'s second
  blocker, found when the shim's DELETE-WHEN trigger fired).
- **P0b (the gate decision):** §7 Q2 RESOLVED — a runtime-consulted defglobal,
  with the enumeration and the numbers in §7.2 and the arms in §4.1a.
- **P1 (the write rule, inert):** the four arms of §4.1a carry the write rule
  behind `*p-raw-elems*`, default OFF.  Zero-change bar met: gate PASS
  190/6367, sweep GATE clean TOTAL 18339 (+0), drops 5 = census, battery
  byte-identical to perl, bench at or below base.  **Gate-ON preview** (phase 1
  alone, nothing else done): arrhash 1.41× → **1.21×**, arrfill 3.91× →
  **2.94×**, sliceasgn 3.04× → **2.71×**, slices 4.96× → 5.17× (promotion
  allocates on first read of each slot; §4.4's proven arm and phase 4's slice
  re-measure own that).  The gate-ON battery ALSO showed exactly which E-events
  still need phase 2: **E2, E3, E7 and only those** — E1, E4, E5, E8, E10, E11,
  E12 already work over raw slots.

## CHECKPOINT LOG (continued)

- **C2:** §4 mechanism (write rule; ONE promotion fn extracted from
  argbox; E2 amortized-monotone promote-on-flatten with writes_args fast
  path; E3/E7 two arms; tie/magic fence; emission mostly untouched), §5
  corpus sizing (926 reads / 443 argbox / 324 foreach / 73+elem-setf
  lvalue sites; E11 values-aliasing and E12 slice-aliasing ADDED to the
  catalogue), §6 the four-phase plan with zero-change bars for phases 0–2,
  §7 the four open questions (sort-write policy needs a USER ruling; the
  runtime-gate shape; values-copy positions; xs element-box assumptions).
- **C3:** the probe battery found #817/#818 (values/slice foreach aliasing
  ALREADY broken on the fully-boxed tree — the intermediate-list builders
  copy); #816 filed as the design task; DECIDED §s455d + plan-post-s433
  §3b pointers landed.
- **C4 (design COMPLETE except two rulings):** §7 Q3+Q4 RESOLVED by
  probe/code-read — the values copy/alias position split is already
  correct, and the XS bridge already promotes raw slots (`xs-av-fetch`,
  the THIRD live copy of the promotion arm).  Remaining before phase 0
  closes: Q1 (sort-comparator write policy — USER ruling; recommendation
  in §7) and Q2 (runtime-gate vs emission-keyed gate — the implementing
  session decides with a measurement).  The design is otherwise ready to
  hand to an Opus session for phases 0–2.

## CHECKPOINT LOG

- **C1 (this commit):** problem, the design sentence, the three
  already-existing mechanisms (argbox promotion, defelem, raw-tolerant
  reads), the E1–E10 obligation catalogue with the one write-rule
  invariant, the what-gets-faster list.  Established by code reading at
  `362667a`: `p-aref-argbox` promotes raw slots TODAY (line ~7190);
  `%p-foreach-elt`'s raw arm binds a TEMPORARY (would be a silent-wrong
  under raw-default — flagged as the E3 semantics flip); the setf
  machinery routes element lvalues through `p-aref-box`/`p-gethash-box`
  (lines 6024–6120).
