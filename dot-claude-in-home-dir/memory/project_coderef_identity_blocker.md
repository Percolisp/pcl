---
name: project_coderef_identity_blocker
description: "Moo subclass bootstrap blocked by UNSTABLE CODEREF IDENTITY — object-address (live SBCL pointer) re-boxes/changes, breaking Sub::Defer's coderef-keyed %DEFERRED. Fix candidates: stable per-object id / canonicalize by underlying fun."
metadata: 
  node_type: memory
  type: project
  originSessionId: 7af6ebee-0865-4499-b0c0-edbf2a4bd7ce
---

# Coderef identity instability — Moo subclass wall (session 249)

## Symptom
`Dog->new` (Moo `extends 'Animal'`) infinite-loops / empty object.

## Failure chain (root causes peeled in order)
1. `_CAN_SUBNAME=0` (no `Sub::Util::set_subname`) → `Sub::Defer::defer_sub`
   takes its **string-eval** branch; PCL string eval can't capture lexicals
   → broken deferred stub. **FIXED s249**: `lib/Sub/Util.pm` shim
   (set_subname returns its coderef) → `_CAN_SUBNAME=1` → pure-Perl closure
   branch. See [[project_string_eval_lexical_capture]].
2. `defined &Sub::Util::set_subname` false for MULTI-SEGMENT pkgs (the gate
   Sub::Defer's BEGIN checks). **FIXED s249**: `p-sub-defined`/`p-sub-exists`/
   `p-undef-sub` in cl/pcl-runtime.lisp now use `%pcl-find-package` instead of
   `(find-package (string-upcase pkg))` (same class as s248b p-super-call fix).
3. **CURRENT WALL — coderef identity is unstable.** Sub::Defer keys
   `%DEFERRED` BY CODEREF and cross-checks `$deferred eq $deferred_sub`.
   The deferred Animal::new stub fires, reads `$deferred_info->[4]`
   (=`CODE(0x..A)`, present in %DEFERRED), passes it to `undefer_sub`, which
   RECEIVES `CODE(0x..B)` (a DIFFERENT address) → `has=0` MISS → never runs
   `$maker->()` → `goto &$undeferred` self-loops forever.

## Diagnosis (what it IS / ISN'T)
- `object-address` (cl/pcl-runtime.lisp:994) = `(sb-kernel:get-lisp-obj-address
  obj)` — the LIVE pointer. Used for `CODE(0x..)` stringify (line 1272) AND
  refaddr; coderef hash KEYS derive from it.
- NOT weaken-collection: PCL `weaken` is ~a no-op (weak value stays `defined`;
  Perl drops it). Verified.
- NOT reproducible raw GC relocation: forced full+generational `(sb-ext:gc)`
  did NOT move even a capturing closure in isolation.
- NOT plain arg-passing: passing `sub{}` / `$aref->[4]` to a sub preserves the
  address in isolation.
- IT IS: in the live Moo path the SAME logical coderef presents MULTIPLE
  `object-address` values (re-boxing/wrapping when a glob-installed named-sub
  coderef is read out of a closure-captured container and threaded through
  `goto &`). `$stub == \&Foo::bar` → NO (diff box) though same string;
  `Foo->can('bar') == $stub` → YES. Adding a `warn` that re-reads `[4]` right
  after store is a HEISENBUG that temporarily stabilizes it.

## Fix A IMPLEMENTED (s249, user-approved) — loop FIXED, subclass still incomplete
- **A. Stable per-object id** — DONE in `object-address` (cl/pcl-runtime.lisp
  ~994): weak `eq` table `*p-object-id-table*` (`:weakness :key`) +
  `*p-object-id-counter*`; assign monotonic id on first request, reuse for
  life. Replaces raw `get-lisp-obj-address`. Backs ALL ref identity (refaddr,
  == on refs, CODE/HASH/ARRAY(0x..) stringify).
- **RESULT**: the infinite `goto` self-loop is GONE — `%DEFERRED` coderef keys
  now stay consistent, `undefer_sub` succeeds, deferred ctors materialize.
  Single-class Moo still fine. So A was the right fix for the identity blocker.
- **BUT Moo subclass STILL returns empty attrs** (`name=/sound=/breed=` blank)
  — a SEPARATE downstream bug, see [[project_moo_subclass_empty_attrs]] /
  below. Verdict on the open Q: didn't need B; A sufficed for identity.

## NEXT WALL — refined (s249, instrumented, NOT yet solved)
Stable-ref (string-identity) trace of `MGC->can('new')` at each maker build:
- MGC build  → BOOTSTRAP (0x1), bootstrap fires, install_delayed sets deferred.
- Animal build → **OTHER (0x4)** — a SECOND, distinct function that STILL runs
  the bootstrap body (MGC_BOOTSTRAP warn fires) but is a different object than
  the load-time `\&new` (0x1).  The installed DEFERRED never fires
  (no GENMETHOD into=MGC ever).
- Dog build → MooObject — MGC::new now absent, falls through @ISA to
  Moo::Object::new → bare bless {} → empty maker (madepkg=UNDEF) →
  Dog ctor empty → `name=/sound=/breed=` blank.
**Unexplained crux:** where does the 2nd bootstrap-body function (0x4) come
from?  `sub new` is compiled once.  RULED OUT by isolated repro (all PASS):
require idempotency (top-level AND inside a repeated sub), glob-overwrite vs
dispatch, method-cache invalidation, self-deleting-bootstrap + defer_sub +
recall (single AND multi-seg pkg), delete+glob-reinstall round-trip.  The
trigger only manifests in the full multi-class Moo load.  Black-box probing
exhausted — NEXT approach must be white-box: read PCL's sub storage model
(defun PL-NAME vs glob CODE-slot side table) + how p-method-call resolves vs
how `*{glob}=code` / stash-delete (s247) mutate it, to explain the 2nd fn +
why the deferred MGC::new is never dispatched.

## NEXT WALL (original notes, distinct bug, exposed after the loop fix)
`Dog->new` no longer loops but builds an EMPTY object. Construction-time
`MGC->new(%construct_opts)` for Dog still returns `madepkg=UNDEF`. Trace:
literal `MGC->new` (MGC/Animal makers) hits the hand-written BOOTSTRAP `sub
new` and stores keys; but Dog's call is `(ref $con)->new` (con=Animal's
maker) and dispatches to NEITHER the bootstrap NOR a generated MGC::new — it
falls through @ISA to `Moo::Object::new` → bare `bless {}` → empty. Root
suspect: after the bootstrap's `delete _getstash(MGC)->{new}` (multi-seg pkg)
+ `install_delayed` reinstall, `p-method-call` (computed dispatch) can't see
MGC::new. NOTE: a PLAIN multi-seg sub dispatches fine both literal+computed
(probed) — so it's specifically the delete-self + deferred-reinstall combo.
NEXT: trace what `p-method-call "Method::Generate::Constructor" "new"` resolves
to right before Dog's `_constructor_maker_for`; check whether the s247
stash-delete fmakunbound's the bootstrap defun for a multi-seg pkg and whether
the install_delayed glob-installed stub is visible to p-method-call.

## Repro / tooling
`/tmp/moo_sub.pl` (Animal+Dog). Instrument by `cp` real Moo/Method::Generate::
Constructor/Sub::Defer into lib/ + `chmod u+w` + warns (REMOVE after; they
shadow site_perl). `SUB_QUOTE_DEBUG=1 ./runpcl` dumps generated subs. Banked
(uncommitted): lib/Sub/Util.pm + the 3 `%pcl-find-package` edits.
See [[project_moo_progress]].

## s406 update: the neighbouring bug is FILED as task #362, and it is NOT (yet) this one

`\&NAME` builds a NEW reference on every evaluation in PCL, so `\&f == \&f`
is false where perl says true (named subs only; hash/array/scalar/anon refs
compare equal, stringified coderefs are stable).  Measured against THIS wall
s406, and the shapes separate:

    my $stub = sub {…}; *{"Pkg::new"} = $stub;   # the Sub::Defer shape
    $stub == \&Pkg::new       perl same   PCL same   <- install/lookup WORKS
    \&Pkg::new == \&Pkg::new  perl same   PCL DIFF   <- task #362

So do not assume #362 closes this wall: it does only if Sub::Defer takes
`\&NAME` twice for one sub.  Instrument `undefer_sub` to settle it.
