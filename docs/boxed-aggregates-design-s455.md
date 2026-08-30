# Boxed aggregates → raw element storage: the design analysis (s455d, Fable)

**Status: IN PROGRESS — written in checkpoints (USER: "save at checkpoints").
Each checkpoint is committed; a stopped session resumes at the last one.
CHECKPOINT LOG at the bottom.  Task #816.**

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

*(§4 mechanism details, §5 corpus event sizing, §6 phased plan + bars, §7
open questions — next checkpoints.)*

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
