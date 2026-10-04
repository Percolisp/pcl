---
name: project-array-aassign-review-gate
description: RESOLVED (session 218) — the array.t AASSIGN_COMMON review found there is no bug; real target is the map+() parse bug
metadata: 
  node_type: memory
  type: project
  originSessionId: 1d298274-c1fc-465d-8648-954085717cee
---

**RESOLVED — review done in session 218.** The review gate asked to present the
array.t AASSIGN_COMMON fix approach before implementing. Outcome: **there is no
AASSIGN bug to fix.**

Verified directly: `@a=@a`, `(undef,@a)=@a`, `@a=('X',@a,'Y')`, the `my`/`local`/`our
@bee` blocks (tests 37–62), AND `my %x = %$x` ([perl #70171] ref-self-assign) ALL pass.
`p-hash-=` already snapshots into a flat vector before `clrhash`; the array path already
snapshots too. The old "~27 tests, snapshot RHS in `p-list-=`/`p-array-=`" claim was a
STALE catalog entry (the work was done in sessions 209 + 215).

What array.t actually had (session 218, 38 fail → **21 fail / 17 skip**):
- **17 registered** not-supported in `cl/skip-registry.lisp` (error-detection of
  non-creatable negative index; `&PL_sv_undef`/SV identity; `@_` alias to nonexistent
  elem; sparse-array holes / lazy creation / map-no-vivify). Backed by not-supported.md
  §"Sparse arrays (holes), element aliasing, and SV identity".
- **Held back as fix targets (NOT registered):**
  - **arylen magic** (`\$#array`, freed-array length, `arylen_p`): tests 83–88, 92–114,
    126, 172. **NOT as hard as first claimed** — PCL already has runtime get/set
    interception: the `tie` proxy. `unbox`/`box-set` are the two chokepoints, both dispatch
    on a magic marker in the box `value` slot. Arylen = sibling `p-arylen-magic` struct +
    one arm each in `unbox`/`box-set`/`p-ref` + one codegen rule for `\$#array`. Live
    write-through (92–114, 126, 172) passable; only freed-array (83–88) stays hard (weak
    pointer → GC nondeterminism). Generalize to `p-magic-cell` (getter/setter closures) to
    also back `\substr`/`\pos`/`\vec`/lvalue-substr. See sweep-bug-catalog.md array.t entry.
  - **`map +(LIST)` unary-plus parse bug** (tests 118, 121): `map +($_,$h{$_}), LIST`
    misparses `+(` (a no-op disambiguator in Perl) as unary numeric plus → collapses the
    list to only the value ("2 4" not "1 2 3 4"). REAL fixable cross-cutting PExpr bug —
    this is the genuine target that replaces the phantom hash fix. Fix area: unary `+`
    before `(`/`{` must be list-preserving pass-through, not `p-+`.

See the array.t entry in `docs/sweep-bug-catalog.md` (rewritten session 218) and
[[project-wantarray-followup]].
