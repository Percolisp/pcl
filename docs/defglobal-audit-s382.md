# Direction-D audit (s382c): dissolve the poisoned-my family — VERDICT: GO, with the mechanism CORRECTED

**The go/no-go measurement ruled by `docs/var-handling-review-s379.md` §6
and USER s379c (the pre-v0.1 IR batch).  Verdict: GO — no third class
exists in the emitted corpus — but the review's proposed mechanism
(`sb-ext:defglobal`) is DEAD ON PROBE; the working mechanism is the
symbol-macro global.  Population: the 111-file corpus snapshot at
`87b8df3` + `cl/pcl-pack.lisp` + `cl/pcl-mro.lisp`; probes on SBCL 2.6.0.**

## 1. The headline probe: the review's mechanism does not exist

`var-handling-review-s379.md` §6 claimed: "Under defglobal, a `let` of
`$x` is a plain lexical binding."  **False — probed:** SBCL refuses the
form outright:

    COMMON-LISP-USER::X-G names a global lexical variable, and cannot be
    used in LET.

`sb-ext:defglobal` proclaims the name GLOBAL, and global variables can
never be locally bound, lexically or dynamically.  There is no free
lexical shadowing.  (This is why measurement-first is the rule — the
premise died in probe P1.)

**The corrected mechanism — the symbol-macro global:**

    (define-symbol-macro $x (symbol-value '$x))     ; access
    (setf (symbol-value '$x) (make-p-box nil))      ; storage, initialized

All semantic probes pass (`dg-probe2/3.lisp`, s382c):

| probe | result |
|---|---|
| free read/write through compiled fns | ✓ global cell |
| `(let (($x 99)) …)` | ✓ plain LEXICAL shadow; fns still see the cell — perl's `my` |
| `setf` under the let | ✓ writes the lexical |
| closure capture of the shadow | ✓ |
| save/install/restore via `symbol-value` (+ `unwind-protect`, die path) | ✓ — the `local` lowering |
| `progv` binding visible through the access macro | ✓ (unused, but sound) |
| read cost, 100M reads | 111 ms vs 106 ms special — parity |
| symbol-macro on a special / special on a symbol-macro | **ERROR both directions** — the partition must be name-disjoint, and a partition bug DIES at load (rule-12 friendly) |
| read of an uninitialized cell | **UNBOUND-VARIABLE error** — every cell must be initialized |

## 2. The corpus classification: no third class

Every `let`/`let*` binding of a defvar'd name across the emitted corpus
(675 distinct defvar'd names; 113 files, 72 with hits):

| class | count | disposition under D |
|---|---|---|
| my/param declarations colliding with a defvar'd name (fresh inits: `make-p-box`, `p-copy-scalar-arg`, `make-array`, sig-rest, computed values) | **416** | ACCIDENTAL dynamic binding today — becomes the correct perl lexical shadow, for free |
| runtime-magic rebinding — `@_` 321, `$@` 10, `$1` 6, `@INC` 3, `$_` 3, `%ENV`/`%SIG` 2, `$?` 1 | **348** | deliberate; these names STAY defvar |
| `local` scalars (`p-box-for-local`) | **54** | re-lower to save/restore (§3b) |
| `local` containers (`p-copy-array`/`p-copy-hash` incl. `local @ISA`) | in the 416-adjacent scan, all accounted | same re-lowering |
| `(declare (special X::$a X::$b))` | 32 sites, **only** sort pairs | deliberate; `$a`/`$b` STAY defvar (§3a) |
| anything else (true third class) | **0** | — |

Supporting facts: **0 bare defvars** (every emitted defvar has an init, so
§1's initialization requirement has no gap); the runtime contains **no
progv**, and `p-local-glob`/`p-local-hash-elem` already localize by
save/restore through `(setf symbol-value)`/`makunbound` + `unwind-protect`
— cell-compatible as-is.

What D deletes, measured on the corpus: **809 poisoned-my renames**
(`__shadow__` 611, `__cond__` 194, `__emb__` 4) plus their three
near-duplicate veto predicates and ~300 lines of Parser2 machinery; task
#205 closes by construction.  (`__file__` 960 and `__state__` 265 stay —
different causes: spanning/capture and `state` cells.)

## 3. Implementation constraints (the design, settled by the probes)

a. **The dynamic exception set is name-based and image-global**: `$a`,
   `$b` (every package spelling — the sort pair is read via
   `symbol-value`/`declare special`, s380) and the runtime-magic defvar
   list (`@_`, `$_`, `$@`, `$1`…`$N`, `%ENV`, `%SIG`, `@INC`, `$?`, the
   punctuation set) remain defvar/special.  Everything else becomes a
   symbol-macro global.  The rule must be derivable from the NAME alone
   so separately-transpiled modules agree; SBCL enforces violations with
   a load-time error (never silent).
b. **`local` lowering** (scalars AND containers) becomes
   save/install/restore + `unwind-protect` — the exact `p-local-glob`
   idiom, which the runtime already ships.  `local` on an EXCEPTION-set
   name keeps today's let-based dynamic binding.
c. **Every symbol-macro cell is initialized where today's defvar init
   runs** (forward-decl pass; measured: no bare defvars exist).
d. **`*pcl-cache-generation*` bump is mandatory** — a stale cached module
   transpile carrying `defvar $x` against a new `define-symbol-macro $x`
   dies loudly at load (P6), which is correct but must not be reachable
   from a warm cache.
e. **Behavior moves toward perl and each move wants a probe+guard row**:
   a called sub no longer sees a caller's `my` that shadowed a global
   (perl: correct); `$$name` symbolic deref under a `my $x` shadow now
   reads the PACKAGE variable as perl does (today it wrongly reads the
   my).  The rename machinery (`__cond__`/`__shadow__`/`__emb__` + vetoes)
   deletes in the same change or immediately after, gated per family.
f. **`docs/ir-spec.md`** (data model / load model) updates in the same
   commit — this changes the emitted contract (defvar → symbol-macro +
   cell init), which is exactly why it is pre-v0.1.

## 4. The s379b sign-off conjuncts

1. Simpler ✓ — three rename families + three vetoes + #205 delete.
2. Clearer ✓ — `my` means lexical, globals mean cells; no accidental
   dynamics.
3. Generated code faster or unchanged ✓ — read parity probed; every
   my-near-a-global sheds a dynamic bind/unbind pair.
4. Compile time ✓ — emission-shape change only, no new analysis pass
   (it REMOVES passes).

Per the rule, implementation proceeds without a further ask.  Scope: one
emitter+runtime session with the two-population gate (corpus-diff over
the flip is expected to be total — every file with globals changes — so
the gate is the Pl/t gate + full sweep TOTAL/LOST + board, per the
lib-reach rule), then the machinery-deletion session.
