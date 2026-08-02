# PCL: Type-Flow Annotations, Codegen Consumption, and Representation Specialization

**Written:** 2026-06-10
**Status:** Design report — synthesizes and extends two existing plans
**Companion docs (read first, this report does not duplicate them):**
- `docs/ast-annotation-plan.md` — the expression-level annotation taxonomy
  (`returns_list`, `needs_wantarray`, `lvalue`, `var_kind`, `closure_captured`,
  `unboxable`) and the two new passes (`VarAnnotator`, `ASTAnnotator`).
- `docs/two-phase-compiler.md` — the block/statement-level scoping fix
  (`BlockAnalyzer`, `_emit_scoped_block`, `_stmt_pre_hook`, `type_hint`).
- `docs/let-scoping-problem.md`, `docs/closure-lexical-scoping.md` — the bugs
  those two solve.

This report answers three questions the existing plans leave implicit:

- **(a)** What data structures are actually built and live *during* code
  generation, and how they relate.
- **(b)** How code generation reads those structures and changes its output.
- **(c)** *(the deep part)* Given a variable whose value comes from an opaque
  source — a sub call — but is then consumed only by string operations, what can
  we legitimately do, and where is the line between a real optimization and a
  semantics bug.

The short version of (c): **the source type is irrelevant; the *consumption*
type is everything.** The one real correctness constraint is that you must not
materialize a value in a representation that loses information a later use will
observe in a *different* representation. The lossy direction is **number↔string**
(float display rounding: `(0.1+0.2)` prints `"0.3"` but the bits aren't `0.3`).
**Boolean is NOT a hazard for string representation** — Perl's truthiness *is*
the string-flavoured test (false iff it stringifies to `""` or `"0"`), so
`bool(v) == bool(to-string(v))` for every scalar (verified empirically; see c.4).
Everything in this report falls out of that asymmetry.

> **Correction note (2026-06-11):** an earlier draft listed "no boolean use" as a
> disqualifier for `repr=string`. That was wrong — bool rides along with
> stringification for free. The real disqualifier is *numeric* use. §c.4 below has
> the corrected analysis; bool only blocks eager *numification* (see c.4.1).

---

## Measured payoff: where the time actually goes (2026-06-25)

Before designing the analysis, it is worth knowing what unboxing can *and cannot*
buy, measured rather than guessed. Microbenchmarks were run perl-vs-PCL with
**startup excluded** (each runtime's empty-program time subtracted), then the
representation cost was bracketed by hand-writing the same loop as compiled CL at
three levels. All times are pure compute (seconds).

**Current gap (PCL / Perl, startup excluded):** regex 3.7×, strings 4.8×, hashes
6.4×, fib 7.1×, intmath 10.0×, arrays (push+`sort`) 15.0× slower. Geometric mean
≈ **6.7× slower than Perl**.

**The representation brackets** (same workload, all compiled CL):

| | intmath (5M) | fib(32) |
|---|---:|---:|
| current PCL | 2.27 s | ~1.6 s |
| **minimal box-struct** (alloc+read each op) | — | **0.067 s** |
| unboxed, *generic* CL arithmetic | 0.0072 s | 0.026 s |
| native fixnum + type decls | 0.0045 s | 0.013 s |
| Perl (reference) | 0.229 s | 0.55 s |

**The decisive finding: the box *wrapper* is not the cost.** A minimal box-struct
(allocate + read on every op) in compiled CL runs fib in 0.067 s — already **8×
faster than Perl** — yet current PCL takes ~1.6 s. So ~95% of PCL's overhead is
*not* `make-p-box`; it is what the box model **forces**:

1. **Generic operator dispatch** — every `p-+`/`p-<`/`p-%` unboxes both sides,
   checks undef, checks `use overload`, checks string-vs-number, then dispatches.
   Dozens of instructions where native CL emits one.
2. **Calling convention** — `p-flatten-args` builds an adjustable vector per call,
   plus the `*wantarray*` dynamic binding and context-sensitive `p-return-value`.
3. **`p-box` is a heavy struct** — class slot, magic/tie hooks, NV cache —
   `make-p-box` ≫ a bare struct.

This pipeline's `repr`/`unboxable` work attacks **#1 and #3**: when a value is
provably numeric *and consumed numerically*, skip the box and emit a native
arithmetic op instead of the generic `p-` one. It does **not** by itself fix #2
(the calling convention is a separate axis — see "sequencing" below).

**Realistic recovery, by workload shape:**

| workload | now vs Perl | with full unboxing | why |
|---|---|---|---|
| arithmetic inner loops | 10× slower | **faster than Perl** (10–100×) | fully unboxable; SBCL native math wins outright |
| numeric call-bound (fib) | 7× slower | ~2–4× (≈10× if the calling convention is also leaned out) | arithmetic unboxes; `@_`/wantarray/return remain |
| string code | 5× slower | ~1.5–2.5× | strings are *already* CL strings; the box is a thin wrapper |
| array/hash code | 6–15× slower | ~2–4× | comparator/`+=` unbox; element *storage* stays boxed |
| regex | 3.7× slower | ~1.2–1.5× | already native-bound (cl-ppcre); little headroom |

**Blended expectation for mixed CPAN-style code: ~2–4× overall** — moving PCL from
~6.7× slower to roughly **2–3× slower than Perl**, with any tight numeric inner
loop dropping *below* Perl. The eye-popping per-benchmark numbers (intmath 500×)
are pure micro-loops; real code rarely spends all its time in one unboxable loop,
so the blended figure is the weighted average over the hot path.

**Sequencing consequence (highest ROI first).** Because the wrapper itself is
cheap, the biggest, most *local* win is **operator specialization** — emit native
arithmetic when both operands are provably numeric — *before* threading unboxed
representations through the whole pipeline. The intmath bracket shows native vs
generic-unboxed is only ~1.6× apart (0.0045 vs 0.0072 s), but generic-unboxed vs
current PCL is ~300×: nearly all the win is in *replacing generic `p-+`/`p-<`
dispatch with a native op*, not in shaving the box allocation or adding `fixnum`
declarations. This directly supports keeping `repr=number` as a boxed-but-
native-arithmetic first cut (see open question 2) and deferring `fixnum`/`float`
declarations until a specific hot path demands them.

---

## 0. Where this fits the pipeline

```
Perl Source
  → PPI
  → BlockAnalyzer.analyze()        [statement scan: declarations, captures]   ── two-phase-compiler.md
  → parse_expr_to_tree()           [PExpr → OpcodeTree per statement]
  → annotate_contexts()            [VOID/SCALAR/LIST context per node — EXISTS]
  → VarAnnotator.annotate()        [Phase 0: var_kind, decl link, capture, repr] ── ast-annotation-plan.md (+ this)
  → ASTAnnotator.annotate()        [Phase 1–2: returns_list, needs_wantarray, lvalue] ── ast-annotation-plan.md
  → ExprToCL.generate()            [reads metadata, emits CL]
  → CL output
```

The annotation passes do **not** emit code. They decorate one canonical
OpcodeTree per statement (via `OpcodeTree::set_metadata`/`get_metadata`) and
build a few side tables. Codegen then becomes a *reader*: every decision it
makes today by string-matching its own output or threading a mutable flag
becomes a `get_metadata` lookup.

**Critical reuse rule (from `two-phase-compiler.md` §"PExpr Objects: Save and
Reuse"):** there must be exactly **one** OpcodeTree per statement, parsed once,
annotated once, consumed once. The current code parses some statements twice
(once in `BlockAnalyzer`'s would-be usage pass, once in codegen) — that must
collapse to a single cached tree, or annotations set in the analysis pass won't
be visible to codegen. The `pexpr_cache{$stmt_idx}` hand-off is the mechanism.

---

## (a) Data structures built during code generation

There are four, at three lifetimes. Naming them precisely is half the design.

### A1. OpcodeTree node metadata — per node, persists into codegen

This is the backbone. The `OpcodeTree` already stores `node_data` (PPI token or
`{type=>...}` op-hash) and `children_ids` per node id. Metadata is a parallel
`{node_id}{key} => value` map. The full key set after both annotation passes:

| Key | On which nodes | Producer | Consumer in codegen |
|-----|----------------|----------|---------------------|
| `context` | every node | `annotate_contexts` (exists) | already used; gates `=~`/list wrapping |
| `returns_list` | array leaves, list-builtin funcalls, slices, comma-lists | `ASTAnnotator` | `gen_tree_val` — skip `(vector …)` wrapper |
| `needs_wantarray` | `=~` in LIST_CTX | `ASTAnnotator` | `gen_tree_val` — wrap `(let ((*wantarray* t)) …)` |
| `lvalue` | assignment/`\`/`++`/`--`/modifying-builtin targets | `ASTAnnotator` | `gen_array_access`/`gen_hash_access` — `p-aref-box` vs `p-aref` |
| `var_kind` | every variable leaf | `VarAnnotator` | `let` vs `defvar`/dynamic choice |
| `var_decl_node` | every resolved lexical leaf | `VarAnnotator` | shadowing; which `let` a use binds to |
| `closure_captured` | the **decl** node | `VarAnnotator` | `__lex__N` rename decision |
| `loop_var` | `for my $x` decl | `VarAnnotator` | fresh binding per iteration |
| `sort_special` | `$a`/`$b` inside sort blocks | `VarAnnotator` | scope-limited `defvar` |
| `unboxable` | `my $scalar` decl | `VarAnnotator` Phase 0b | skip `make-p-box`/`unbox`/`box-set` |
| **`repr`** *(new, this report)* | `my $scalar` decl | `VarAnnotator` Phase 0c | choose native CL representation + ops |
| **`coerce`** *(new, this report)* | a variable-leaf **use** | `VarAnnotator` Phase 0c | elide/insert a single coercion |

The last two are the contribution of part (c). `unboxable` answers *"does this
scalar need a heap box?"*; `repr` answers the follow-on *"if not, what CL type
does it hold?"* and `coerce` answers *"at this use site, what coercion (if any)
must wrap the read?"*.

### A2. The `VarInfo` record — per variable, per block scope (transient, drives A1)

`BlockAnalyzer` already returns `vars{$name} => { sigil, scope, decl_type,
captured, type_hint, usages[] }`. This report promotes that to the single
authoritative per-variable record the annotation passes fill in and the
`repr`/`unboxable` decisions read. Proposed final shape:

```
VarInfo {
  name            '$x'
  sigil           '$' | '@' | '%'
  scope           'local' | 'outer' | 'param'
  decl_type       'my' | 'our' | 'state' | 'local' | undef
  decl_node_id    OpcodeTree node of the declaration (anchor for repr/captured)
  source_kind     'literal' | 'arith' | 'string' | 'call' | 'array' | 'unknown'
                  (type of the RHS that initializes it — see (c))

  # disqualifiers for unboxing / eager coercion
  ref_taken       bool      \$x appears anywhere
  tied            bool
  localized       bool      local $x in an inner scope
  captured        bool      referenced inside a nested sub{}
  captured_mut    bool      ...AND written there
  passed_opaque   bool      handed to a sub/method as an argument

  # consumption profile (the engine of part c)
  uses            [ UseSite ]
  demand          Set<Demand>     # union of every use's demand
  written_after_decl bool         # any assignment past the initializer

  # decisions
  repr            'any'|'box'|'fixnum'|'float'|'number'|'string'|'bool'
  unboxable       bool
}

UseSite {
  node_id
  role            'read' | 'write' | 'both'   (both = .=, +=, ++, --)
  demand          'numeric'|'string'|'bool'|'ref'|'key'|'list'|'opaque'
  in_loop         bool         # nested under a for/while — accumulator hint
}

Demand  ∈ { numeric, string, bool, ref, key, list, opaque }
```

`demand` per use comes from the **parent operator** in the OpcodeTree (this is
why the analysis must be OpcodeTree-level, not PPI-sibling-level — the tree
already encodes precedence, so `$b` in `$a + $b * $c` correctly has parent `*`,
demand `numeric`). `BlockAnalyzer`'s `_context_from_op` is the seed; part (c)
refines its lattice.

### A3. The scope stack — transient, internal to `VarAnnotator.annotate`

A list of frames, innermost last:

```
Frame { vars => { name => VarInfo-stub }, is_sub => 0|1 }
```

`is_sub => 1` marks a closure boundary (entered via `anon_sub`); `is_sub => 0`
marks a non-capturing lexical scope (`inline_lambda`/do-block, bare block).
Closure capture = a use resolves to a decl in a frame *below* at least one
`is_sub => 1` frame. This is the `_lookup` five-liner in
`two-phase-compiler.md` §VarAnnotator. It supersedes the PPI-level
`_find_closure_captures` and `_vars_referenced_in_closures` (both less accurate
across deep nesting).

### A4. The per-sub return-type table — cross-statement, optional (Tier 3)

A package-global `{ sub_name => repr }` filled by a pre-pass over each sub body's
`return`/tail expressions. This is the *only* thing that can type the
"value from a sub call" **source** (A2.`source_kind = 'call'`). Without it,
a call source is `unknown`. With it, `my $n = count_items()` where
`count_items` provably returns an integer lets `$n` start as `fixnum`. Deferred
to a later tier (interprocedural), but the data structure belongs in this
inventory because part (c) keeps bumping into it.

---

## (b) How code generation changes

The mechanical changes (already specified in `ast-annotation-plan.md` §Implementation):

| Handler | Today | After annotations |
|---------|-------|-------------------|
| `gen_tree_val` | `if ($child =~ /\(p-=~\s/)` string match; `_child_is_list_expr` hardcoded list | `get_metadata($kid,'needs_wantarray')` / `'returns_list'` |
| `gen_array_access` | reads `lvalue_context` mutable flag | `get_metadata($node,'lvalue') ? 'p-aref-box' : 'p-aref'` |
| `gen_hash_access` | same flag | `'p-gethash-box'` vs `'p-gethash'` |
| `gen_prefix_op` (`$$var++`) | shape-inspects the AST | `lvalue` annotation already correct |
| closure rename | `_vars_referenced_in_closures` at parse time injects `__lex__N` | `closure_captured` on decl node; codegen emits mangled name |

The new changes (this report), all driven by `repr`/`coerce`:

| Site | `repr` = `any`/`box` (status quo) | `repr` = `string` (specialized) |
|------|-----------------------------------|----------------------------------|
| declaration | `(let (($x (make-p-box nil))) …)` | `(let (($x "")) …)` |
| initializer `my $x = E` | `(box-set $x E)` | `(setf $x (to-string E))` — **one** coercion |
| read in string op | `(p-. (unbox $x) …)` | `(p-. $x …)` — `$x` already a CL string |
| `length $x` | `(p-length (unbox $x))` | `(p-length $x)` or native `(length $x)` |
| `$x eq $y` | `(p-str-cmp (unbox $x) …)` | `(p-str-cmp $x …)` / native `string=` |
| `$x .= E` in a loop | repeated `(box-set $x (p-. (unbox $x) E))` → O(n²) | adjustable fill-pointer string + append → O(n) |
| as hash key | `(p-gethash h (to-string (unbox $x)))` | `(p-gethash h $x)` |
| write `$x = E` | `(box-set $x E)` | `(setf $x (to-string E))` |

The dispatch point is one helper, read once per variable-bearing node:

```perl
# ExprToCL.pm
sub _var_repr { $_[0]->expr_o->node_tree->get_metadata($_[1], 'repr') // 'box' }
```

`repr eq 'box'` reproduces today's output byte-for-byte — that is the safety
fallback and the default. Specialization only ever *narrows* from `box`, never
the reverse, so an incomplete analysis can never produce wrong code, only
miss an optimization. **This monotonicity is the whole safety story for codegen.**

A second helper handles use-site coercion:

```perl
# At a variable-leaf read, the analysis may have decided the value is already
# in the demanded representation (no coercion) or needs exactly one:
sub _emit_var_read {
  my ($self, $node_id, $name) = @_;
  my $repr   = $self->_var_repr($node_id);
  my $coerce = $self->expr_o->node_tree->get_metadata($node_id, 'coerce'); # undef|'str'|'num'
  my $raw    = $repr eq 'box' ? "(unbox $name)" : $name;
  return "(to-string $raw)" if ($coerce // '') eq 'str';
  return "(to-number $raw)" if ($coerce // '') eq 'num';
  return $raw;
}
```

---

## (c) Deep dive — a sub-sourced, string-consumed variable

> *"What kind of operations can we do on a variable that gets its value from
> calling a sub, but is then only used for string operations?"*

### c.1 Why the source being a sub call is the interesting case

A variable's representation can be decided two ways:

1. **Forward (source-driven):** type the RHS that initializes it.
   `my $n = length($s)` → integer. `my @a = split …` → list. `my $r = \$x` →
   ref. These are easy because the operator/builtin has a known result type.
2. **Backward (consumption-driven):** type by how the variable is *used*.

The phrase "gets its value from calling a sub" is precisely the case where
**forward typing fails**: a sub's return type is unknown without
interprocedural analysis (A4). So the source contributes nothing
(`source_kind = 'call'`, type `unknown`). The *only* lever left is backward,
consumption-driven typing. The user picked the one source that forces us onto
the hard path — correctly.

And that path turns out to be the *more powerful* one, because Perl's notion of
a scalar's "type" is not the type of the bits it holds — it is the coercion the
operator demands. `$x . "y"` does not care what `$x` "is"; it stringifies it.
So if **every** consumer stringifies, the variable is, observably, a string —
regardless of what the sub returned.

### c.2 The consumption lattice and the monomorphic rule

Each use imposes a **demand** (from its parent op in the OpcodeTree):

| Demand | Triggering parents | Coercion applied at runtime |
|--------|--------------------|-----------------------------|
| `string` | `.` `x` `eq ne lt gt le ge cmp` `=~` `!~`, string-`sprintf`, interpolation, `length` `substr` `index` `rindex` `uc` `lc` `reverse`(scalar), hash **key**, `join` sep/args | `to-string` |
| `numeric` | `+ - * / % ** < > <= >= == != <=> << >> & | ^` (numeric mode), `abs int sqrt`, array **index** | `to-number` |
| `bool` | `if while unless until ? : && || // ! and or not`, `defined` | `p-true-p` — **representation-neutral under stringify** (see c.4); does NOT disqualify `repr=string` |
| `ref` | `\`, `->`, `@{ }`, `%{ }`, `${ }`, `ref` | none (identity) |
| `key` | a hash subscript `$h{$x}` | `to-string` (subset of string) |
| `list` | flattening into a list/array context | none |
| `opaque` | argument to a user sub/method; `return` value; stored into aggregate | unknown — callee/consumer decides |

**Monomorphic-consumption rule.** Let `D` = the set of demands over all uses.
A `my $scalar` is `repr = string` iff:

```
D ⊆ { string, key, bool }    # every use stringifies OR is bool (bool is free, c.4)
AND  not ref_taken           # no \$x
AND  not tied                # no STORE/FETCH magic
AND  not localized           # no dynamic save/restore of the box
AND  not captured_mut        # not mutated through a closure
AND  not passed_opaque       # never handed to a sub that might numify it
# NOTE: `numeric` ∉ D is the real constraint — eager stringify rounds floats.
#       `bool` IS allowed; it is correct on the materialized string form.
```

The exception inside `D`: a `numeric` demand is fatal to `repr=string` because
materializing the display string and then numifying loses float precision
(`(0.1+0.2)` → `"0.3"` → `0.3` ≠ the original bits). A `numeric` use forces
`repr=any`/`number` instead (lazy coercion — see below), **not** `box`.

For the numeric side, the asymmetry matters: a value used numerically (and maybe
also as string and/or bool) is best stored as `repr=any`/`number` — keep the
*number* and stringify lazily on demand (lossless, matches Perl's own
print-on-demand). You only commit to a native `repr=number`/`fixnum`/`float`
when the value is **provably** a real number; consumption-as-numeric alone does
not prove it (`"3abc"+0` works), and eager numification additionally breaks bool
(c.4.1). Bottom line: **mixed `{string,numeric,bool}` → `repr=any` (unboxed,
lazy coercion both ways), never `box`** unless a box-magic disqualifier applies.

### c.3 What we may legitimately emit for `repr = string`

Given the rule holds, and the value is evaluated once at the binding:

1. **Coerce once, at the source.**
   `my $x = some_sub();` → `(let (($x (to-string (CALL)))) …)`.
   The sub is called exactly once anyway; we fold the (otherwise per-use)
   stringification into that single point. N coercions → 1.

2. **Unbox.** Store the raw CL string, not `(make-p-box …)`. Skip `make-p-box`
   at decl, `unbox` at every read, `box-set` at every write. (This is exactly
   `ast-annotation-plan.md` §E `unboxable`, now with a concrete *representation*
   attached, not just "no box".)

3. **Native string ops at every use** (Perl-semantic wrappers retained where
   Perl differs from CL — negative `substr` offsets, `index` returning −1, etc.,
   but operating on a known `string`, never re-checking the type):
   - `$a . $b . $c` → `(concatenate 'string $a $b $c)` instead of nested `p-.`.
   - `length $x` → `(length $x)`.
   - `$x eq $y` → `(string= $x $y)` (when both are `repr=string`).
   - `substr/index` → `subseq`/`search` cores.

4. **Accumulator fast path (the biggest single win).** If the *only* mutation
   is `.=` and the variable is initialized to `""`/a string and read as a string
   at the end, then a naive transpile is quadratic:
   ```perl
   my $out = '';
   $out .= $_ for @lines;     #  O(n²): each .= copies the whole prefix
   ```
   With `repr = string` + "only-appended" detected from the `UseSite` roles
   (`both` via `.=`, all `in_loop`), emit an adjustable fill-pointer string and
   `vector-push-extend` the appended pieces, or wrap the whole accumulation in
   `with-output-to-string`:
   ```lisp
   (let ((out (make-array 0 :element-type 'character
                            :adjustable t :fill-pointer 0)))
     (dolist (x lines) (loop for c across (to-string x)
                             do (vector-push-extend c out)))
     out)   ; O(n)
   ```
   This is the canonical case where representation choice changes *complexity
   class*, not just a constant factor.

5. **Direct hash-key use** — `$h{$x}` skips the key-stringification step since
   `$x` is already a CL string.

### c.4 Why boolean is FREE under stringification (the corrected analysis)

Perl's truthiness is **the string-flavoured test**: a scalar is false iff it
stringifies to `""` or `"0"` (or is undef). That is literally how the rule is
defined, and it is why numbers fall out correctly (`0`→`"0"`, `0.0`→`"0"`). The
consequence is a theorem:

```
bool(v) == bool(to-string(v))      for EVERY scalar v
```

- false values `{undef, 0, 0.0, "", "0"}` all stringify into `{"", "0"}` → still false ✓
- true values all stringify to something ∉ `{"", "0"}` → still true ✓

So **eager stringification preserves every boolean test.** A variable used as
both string and bool is safe to store as a native CL string. The bool use
compiles to the trivial `(or (string= s "") (string= s "0"))` test — exactly
"is it the empty/zero string", nothing representation-sensitive.

**Empirically verified** (`./runpcl`, 2026-06-11):

| Perl expr | stringifies to | bool |
|-----------|----------------|------|
| number `0.0` | `"0"` | false |
| string `"0.0"` | `"0.0"` | **true** |
| `0.1 + 0.2` | `"0.3"` | true |
| `"0 but true"` | `"0 but true"` | **true** (numifies to 0) |

The earlier draft cited "number `0.0` false vs string `"0.0"` true" as a hazard.
That compares `0.0` against an *unrelated* value (`"0.0"`); the stringification of
`0.0` is `"0"` (false), so there is no flip. The comparison that matters is
`bool(v)` vs `bool(to-string(v))`, and those always agree.

#### c.4.1 The genuine asymmetry: bool breaks eager *numification*, not stringification

```
bool(v) == bool(to-number(v))      is FALSE for "0.0" and "0 but true"
```

`"0.0"` and `"0 but true"` are boolean-**true** strings that numify to `0`
(boolean-false). So if you ever **eagerly numify** an opaque value at the binding
(`$x := to-number(SUB())`) and there is a bool use, you flip true→false on those
stringy-zeros. **That** is where bool is fatal — to eager numify, never to eager
stringify.

The deep reason for the asymmetry: stringification is **total and faithful**
(every scalar has a canonical string form), while numification is **lossy and
non-total** (`"abc"+0 == 0`). Perl's bool is defined on the string side, so it
rides stringification for free and conflicts with numification.

#### c.4.2 The one real constraint on `repr=string`: numeric use

Eager stringification *is* lossy in exactly one way — float display rounding.
`(0.1+0.2)` materializes as `"0.3"`; numifying that back gives `0.3`, not the
original bits, so `(0.1+0.2)*10 == 3` flips. Therefore a **numeric** use (not a
bool use) is what disqualifies `repr=string`. If a value is used numerically,
keep it as `repr=any`/`number` and stringify lazily on demand (lossless), which
is still unboxed and still skips the per-read box indirection — just not the
native-string materialization.

Clean, corrected position for the first cut:

> **Set `repr=string` when the demand set ⊆ `{string, key, bool}` (numeric
> absent) and no box-magic disqualifier applies.** Set `repr=any` (unboxed, lazy
> coercion) for any other non-magic mix — including `{string, numeric, bool}`.
> Fall back to `box` only for the box-magic cases (`\$x`, tie, `local`, mutable
> capture, blessed/overload). The fallback is always correct; we only ever skip
> work we can prove unobservable. `bool` is never a reason to fall back.

### c.5 Flow-sensitivity (deferred tier)

The rule above is **flow-insensitive**: it unions all uses regardless of order.
That is conservative for re-typed variables:

```perl
my $x = read_thing();   # used as string below
$x = compute();         # later reused as a number
... $x + 1 ...
```

Flow-insensitive analysis sees `{string, numeric}` → `any`. A flow-sensitive
(SSA-style) pass could split `$x` into two single-assignment values with
distinct reprs. This is strictly more powerful and strictly more work
(needs dominance/liveness). **Recommendation: ship flow-insensitive
monomorphic typing first; revisit SSA only if a real CPAN hot path demands it.**

### c.6 Direct answer to (c), enumerated

For `my $x = SUB(); …` where every use stringifies-or-is-bool (no numeric use)
and no box-magic disqualifier applies, codegen may:

1. emit one `to-string` at the binding instead of one per use;
2. store `$x` as a native CL `string`, skipping `make-p-box`/`unbox`/`box-set`;
3. lower each string op to its native/typed core (`concatenate`, `length`,
   `string=`, `subseq`, `search`) with Perl-semantic edge wrappers only;
4. if the mutation pattern is append-only, use an adjustable string / output
   stream to collapse O(n²) `.=` into O(n);
5. use `$x` directly as a hash key with no re-stringification;
6. compile any boolean test as `(or (string= $x "") (string= $x "0"))` — bool is
   free on the string form (c.4).

It falls back from `repr=string` to `repr=any` (still **unboxed**, lazy coercion)
only when `$x` is also used **numerically** (float round-trip loss). It falls all
the way back to a `box` only for box-magic: `\$x`, tie, `local`, mutable closure
capture, or a blessed/overloaded value. A bool/`defined` use is **never** a
reason to fall back — that was the earlier draft's error.

---

## (d) Worked examples

**E1 — pure string, specialized.**
```perl
sub greet { my $n = fetch_name(); my $g = "Hello, " . $n . "!"; return length $g; }
```
`$n`: demands `{string}` (concat), source `call`, no box-reqs → `repr=string`.
`$g`: demands `{string}` (length) → `repr=string`.
```lisp
(p-sub pl-greet ()
  (let ((n (to-string (pl-fetch_name))))        ; one coercion
    (let ((g (concatenate 'string "Hello, " n "!")))
      (length g))))                              ; native, no unbox
```

**E2 — append accumulator.**
```perl
my $csv = ''; $csv .= "$_," for @rows; return $csv;
```
`$csv`: demands `{string}`, role pattern = init + `.=`-in-loop, terminal read →
`repr=string`, accumulator path. Emits a fill-pointer string built in O(n).

**E3 — string + bool, SPECIALIZED (this is the corrected case).**
```perl
my $x = maybe(); print "x=$x\n"; return "default" unless $x;
```
Demands `{string, bool}`, no numeric, no box-magic → `repr=string`. bool rides
the string form for free (c.4):
```lisp
(let ((x (to-string (pl-maybe))))
  (p-print (concatenate 'string "x=" x (string #\Newline)))
  (unless (or (string= x "") (string= x "0"))
    (p-return "default")))
```
The earlier draft wrongly left this boxed.

**E3b — string + NUMERIC → unboxed `any`, not box.**
```perl
my $y = measure(); my $label = "v=$y"; my $double = $y * 2;
```
Demands `{string, numeric}` → `repr=any` (unboxed; keep the raw value, stringify
and numify lazily). Skips the box, but not the per-use coercion — float fidelity
is preserved for `$y * 2`.

**E4 — blocked by ref-taking.**
```perl
my $s = build(); my $ref = \$s; $$ref = uc $s;
```
`ref_taken` true → `repr=box` (the box *is* the referent).

---

## (e) Safety checklist (codegen must verify before specializing)

Two gates: first **unbox at all**, then **which native repr**.

**Gate 1 — eligible to leave the box (`repr ≠ box`)?** Requires ALL:

- [ ] `scope = local` and `decl_type = my` (not `our`/`state`/`local`/package)
- [ ] not `ref_taken` (`\$x`)
- [ ] not `tied`, not `localized`
- [ ] not `captured_mut` (read-only capture is OK)
- [ ] value cannot be a blessed/overloaded ref (overload dispatch needs the box)
- [ ] not in a statement that mutates `Pl::Environment` mid-block where the
      cached OpcodeTree could be stale (`Include`/`Package` skip the cache)

If Gate 1 fails → `repr = box` → byte-identical to current output.
**Note: a `bool`/`defined` use does NOT fail Gate 1.** `passed_opaque` does not
fail Gate 1 either (the raw value is passed exactly as the box's contents would
be) — it only blocks the typed reprs in Gate 2.

**Gate 2 — which native repr?** (Gate 1 passed.)

- `repr = string` if demand set ⊆ `{string, key, bool}` (numeric absent),
  not `passed_opaque`.
- `repr = number`/`fixnum`/`float` if the value is **provably** a real number
  (forward-typed source or A4 return type), not `passed_opaque`. bool OK here too.
- `repr = any` (unboxed, lazy coercion both directions) for every other Gate-1
  case — including `{string, numeric}`, `passed_opaque`, or unknown mixes. This
  is the safe unboxed default and still removes the box allocation + indirection.

`bool` is never a disqualifier in either gate. The only thing that demotes
`string`→`any` is a numeric use; the only thing that demotes `any`→`box` is
box-magic.

---

## (s) Scoping, shadowing & declarator semantics  *(pedagogical)*

This section is the foundation the whole analysis sits on. Everything above
(`repr`, Gate 1/2, unboxing) assumes we have already answered: *which declaration
does this use of `$x` refer to, and how does its declarator behave?* Get this
wrong and the representation analysis silently cross-contaminates unrelated
variables. So we spell it out from first principles, with Perl↔CL pairs.

### s.1 The foundational rule: identity is the declaration site, not the name

> **The first sweep identifies each variable by its *declaration site* — a unique
> `decl_node_id` (and the scope frame it lives in) — never by its Perl name. The
> name is only a lookup key used to *resolve* a use-site to a declaration.**

Why this is non-negotiable: the same name routinely denotes *different*
variables.

```perl
{ my $x = "text"; print $x; }   # variable A — used as a string
{ my $x = 41 + 1; print $x; }   # variable B — used as a number
```

`A` and `B` share the name `$x` but are unrelated. If `VarInfo` is keyed by the
name `"$x"`, the sweep merges them into one record and **unions their demands**
(`{string} ∪ {numeric}` → `any`), so *neither* gets specialized even though each
is perfectly monomorphic. Keyed by decl-site, they are two `VarInfo`s with
independent `repr`, disqualifiers, and capture flags. Every downstream fact keys
off decl-site identity; the name is just for resolution.

### s.2 CL gives lexical shadowing for free — but only for *non-special* symbols

The good news: if we emit a nested `let` at each `my`'s declaration point (the
scoping fix), CL's own lexical scoping resolves nested shadowing with zero extra
work.

```perl
my $x = 1;
{ my $x = 2; print $x; }   # 2
print $x;                  # 1
```
```lisp
(let (($x (make-p-box 1)))          ; decl-site A
  (let (($x (make-p-box 2)))        ; decl-site B — CL's let shadows naturally
    (p-print $x))                   ; → 2 (inner)
  (p-print $x))                     ; → 1 (outer)
```

**The trap:** in Common Lisp, `(let (($x ...)) …)` where `$x` is a *special*
(dynamic) variable does **not** create a lexical binding — it creates a *dynamic*
one. PCL `defvar`s package globals, `our` vars, `state` vars, and `$a`/`$b`. So a
`my` that shadows any of those, if emitted under the same symbol, would silently
become a dynamic rebind — visible to called subs, wrong lifetime, and not a real
shadow.

```perl
our $x = "global";   # package var → defvar'd (SPECIAL) in CL
my  $x = "lexical";  # must be a TRUE lexical, shadowing the global
print $x;            # "lexical"
```
```lisp
(defvar $x (make-p-box "global"))        ; special

;; WRONG — rebinds the special dynamically, not a lexical shadow:
;;   (let (($x (make-p-box "lexical"))) (p-print $x))

;; RIGHT — rename the my to a fresh, never-defvar'd symbol → genuine lexical:
(let (($x__lex__7 (make-p-box "lexical")))
  (p-print $x__lex__7))
```

> **Rename rule:** emit a `my`/`state` var under its plain name `$x` only when no
> special variable of the same CL symbol is in scope **and** it is not
> closure-captured-needing-a-fresh-cell. Otherwise rename to `$x__lex__N`
> (`__case__N` for case-fold collisions, s.4 case 6). This is the *entire* reason
> the `__lex__`/`__case__` machinery exists — it is a shadowing concern, not an
> optimization.

### s.3 Resolution is position-aware *within* a block (the let-scoping bug)

A `my` enters scope only *after* its own declaration statement. Before that, the
same name resolves to the outer/package variable. So within one block a name can
mean two entities at two line numbers:

```perl
$x = "g";          # $x here is the PACKAGE global
my $x = "lex";     # from here down, $x is the lexical
print $x;          # "lex"
```
```lisp
(box-set $x "g")                      ; package global (special $x)
(let (($x__lex__3 (make-p-box "lex"))) ; my opens a fresh scope from here on
  (p-print $x__lex__3))               ; resolves to the lexical
```

This is why the sweep records `decl_stmt_idx` per declaration and resolves each
use by source position, not just by frame membership. (It is the exact bug
`docs/let-scoping-problem.md` describes.)

### s.4 The six shadowing / collision cases, with CL

| # | Case | Resolved by |
|---|------|-------------|
| 1 | nested `my` shadows outer `my` | nested `let` (CL, free) |
| 2 | same name, disjoint sibling blocks | decl-site keying (independent `VarInfo`s) |
| 3 | intra-block before/after the `my` | position-aware resolution (`decl_stmt_idx`) |
| 4 | `state $x` in two subs / in a loop | per-decl-site persistent cell + once-guard |
| 5 | `my $x` vs `local $x` (different namespaces) | frame lookup vs global namespace |
| 6 | `$x` vs `$X` (CL case-fold) | `__case__N` rename |

**Case 2 — the merge bug shown both ways:**
```perl
{ my $x = "a"; print $x; }
{ my $x = 1 + 1; print $x; }
```
```lisp
;; RIGHT (scoped, decl-site keyed): two independent reprs
(let (($x "a")) (p-print $x))      ; site A → repr=string
(let (($x 2))   (p-print $x))      ; site B → repr=number

;; WRONG (flat-hoist + name key): one merged box, unioned demands → repr=any
(let (($x (make-p-box nil)))
  (box-set $x "a") (p-print $x)
  (box-set $x 2)   (p-print $x))
```

**Case 4 — `state` per-site uniqueness + once-init:**
```perl
sub a { state $n = 0; ++$n }
sub b { state $n = 0; ++$n }   # DISTINCT cell from a's
```
```lisp
(defvar $n__state__1 (make-p-box nil))   ; a's cell
(defvar $n__state__2 (make-p-box nil))   ; b's cell — name-keying would collide these
```

**Case 6 — case-fold collision in one scope:**
```perl
my $x = 1; my $X = 2;   # distinct in Perl, same CL symbol $X after upcase
```
```lisp
(let (($x 1) ($x__case__4 2)) …)         ; second renamed to stay distinct
```

### s.5 Suggested hoisting model — *where each binding/init/restore lands*

"Hoisting" is not one thing; each declarator places its binding, its initializer,
and its teardown in a different spot. The model:

| What | Binding placement | Init placement | Teardown |
|------|-------------------|----------------|----------|
| `my` (statement) | `let` at the **decl statement**, body = rest of block | at the binding | none (GC) |
| `my` (in expression, e.g. `open(my $fh,…)`) | `let` at the **enclosing statement**, body = rest of block | at the binding | none |
| `my` (loop var `for my $i`) | `let` **inside the loop body** (fresh per iteration) | per iteration | none |
| `state` | cell **hoisted** to persistent scope (`defvar`/closure cell) | **guarded, runs once** | none |
| `our` | **no binding** — name resolves to the package `defvar` | n/a | none |
| `local` | **no binding hoist** (it is a global) | set after save | `unwind-protect` **restore at scope exit** |

Three of these are "anti-hoists" worth stressing:

- **`my` is *nested*, not hoisted.** The whole point of the scoping fix: a `my`
  at statement *k* opens a `let` wrapping statements *k..end*, so it does not
  shadow earlier lines (s.3) and disjoint same-name `my`s stay separate (s.4#2).
  The legacy flat-hoist-to-block-top is exactly what breaks both.
- **A loop var is bound *inside* the loop** so each iteration is a fresh cell —
  required for closures that capture `$i`:
  ```perl
  my @subs; for my $i (1..3) { push @subs, sub { $i } }  # 1,2,3 — not 3,3,3
  ```
  ```lisp
  (p-foreach ($i ...)            ; p-foreach must establish $i fresh per iteration
    (vector-push-extend (lambda () $i) @subs))
  ```
- **Expression-level `my`** (`open(my $fh, …)`, `while (my $l = <$fh>)`) scopes
  from its point to block end; emit the `let` at the **statement** granularity
  (not block-top), wrapping that statement onward.

### s.6 The declarator matrix

| declarator | scope | lifetime | dynamic? | box required? | unbox candidate? |
|------------|-------|----------|----------|---------------|------------------|
| `my` | lexical (decl→block end) | per scope-entry | no | only if ref-taken / tied / mut-captured | **YES** (Gate 1/2) |
| `our` | lexical *alias* to a package global | program (the pkg var) | no, but global | **yes** — reachable by symbolic ref, other packages, `eval`, `local` | **NO** → box + `defvar` special |
| `local` | **dynamic** extent of the block | save/restore window | **YES** | **yes** — the box *is* the save/restore unit; also pins the target global boxed | **NO** → box |
| `state` | lexical visibility | **program** (persists) | no | persistent cell + once-guard | conservatively box; specialize later |

**The front-gate rule:** only `my` (and eventually `state`) can ever leave the
box. `our` and `local` are always boxed because they operate on the
dynamic/global namespace that code outside this lexical scope can reach. The
declarator type is therefore checked *before* any usage analysis — it can
short-circuit Gate 1 to `box` immediately.

### s.7 Per-declarator generated shapes

**`my`** — lexical `let`, unboxed when Gate 1/2 allow (here string-typed):
```perl
my $name = lookup();  return "hi $name";
```
```lisp
(let ((name (to-string (pl-lookup))))
  (p-return (concatenate 'string "hi " name)))
```

**`our`** — no binding; reference resolves to the package special:
```perl
package Foo; our $count = 0; sub inc { $count++ }
```
```lisp
(defvar $count (make-p-box 0))      ; special, package-scoped
;; in inc:
(p-++-post $count)                  ; direct reference to the special — no new binding
```

**`local`** — save / set / `unwind-protect` restore; box mandatory, callees see it:
```perl
our $depth = 0;
sub recurse { local $depth = $depth + 1; ... }
```
```lisp
(let ((--saved-- (p-box-value $depth)))     ; save old
  (unwind-protect
       (progn
         (box-set $depth (p-+ $depth 1))     ; set new — visible to called subs (dynamic)
         ...)
    (setf (p-box-value $depth) --saved--)))  ; restore even on die/unwind
```

**`state`** — cell hoisted to persistent scope, initializer guarded to once:
```perl
sub counter { state $n = 0; return ++$n; }
```
```lisp
(defvar $n__state__1 (make-p-box nil))      ; persistent cell
(defvar $n__state__1--init nil)             ; once-guard
(p-sub pl-counter ()
  (unless $n__state__1--init
    (box-set $n__state__1 0)                ; runs exactly once, ever
    (setf $n__state__1--init t))
  (p-return (p-++-pre $n__state__1)))
```

### s.8 What the first sweep records per variable (declarator-specific fields)

Extends the `VarInfo` of §(a) with the identity/scoping fields this section
needs:

```
VarInfo {
  decl_node_id     # ← THE KEY (s.1). Unique per declaration. Not the name.
  name             # lookup key only
  var_kind         # 'my' | 'our' | 'local' | 'state' | 'package' | 'special'
  frame_id         # which lexical scope frame it was declared in
  decl_stmt_idx    # position within the block → position-aware resolution (s.3)
  decl_form        # 'statement' | 'expression' | 'loop_var'  → hoisting model (s.5)
  cl_name          # emitted symbol: plain '$x' or renamed '$x__lex__N'/'__case__N' (s.2)
  shadows          # decl_node_id of the binding this one shadows, or undef
  dynamic_extent   # local only: the block whose exit triggers restore
  persistent       # state only: needs hoisted cell + once-guard
  # ...plus the §(a) fields: uses[], demand, repr, captured, ref_taken, ...
}
```

Resolution (`name` → `VarInfo`) is the position-aware, innermost-frame-first walk
of the scope stack: at a use at statement *i* in frame *F*, search *F*'s
declarations with `decl_stmt_idx ≤ i`, then *F*'s parent (any position), outward;
fall through to the package/special namespace for `our`/`local`/globals. The
first hit is the binding; whether the walk crossed an `is_sub` frame sets
`captured` on that binding (`ast-annotation-plan.md` §D3).

---

## (f) Analysis-pass build steps  *(sequenced by the unified plan)*

> **The authoritative end-to-end ordering lives in
> `docs/codegen-rewrite-spec.md` §Unified implementation phases.** This table is
> just the *analysis-side* detail (what each pass produces + its gate); the
> `Phase` column maps each step onto that plan so the two never drift.

| Phase | Step | Work | Gate |
|-------|------|------|------|
| 0 | — | (done) `BlockAnalyzer` + `_emit_scoped_block` scoping fix | suite green |
| 2 | A | `VarAnnotator`: recursive scope stack (all blocks), decl-site keying (§s.1), position-aware resolution (§s.3), `var_kind`/`var_decl_node`/`closure_captured`, my/our/local/state classification (§s.6), renames (§s.2). Retire `_vars_referenced_in_closures`. **Correctness, no perf.** | closure.t + shadowing (§s.4) + full suite, no regression |
| 3 | B | `ASTAnnotator`: `returns_list` + `needs_wantarray`. Delete `_child_is_list_expr` + the `p-=~` string match. | gen_tree_val tests |
| 3 | C | `ASTAnnotator`: `lvalue`. Remove `lvalue_context` mutable-flag threading. | aref/href lvalue tests |
| 4 | D | `unboxable` (Gate 1: box or not), **no repr yet**. | suite green; spot-check generated CL |
| 4 | E | `repr`+`coerce` (Gate 2): `repr=string` (demand ⊆ `{string,key,bool}`, numeric absent), else `repr=any` for non-magic mixes. Flow-insensitive. Codegen `_var_repr`/`_emit_var_read`. | new `Pl/t/type-flow-*.t`; suite green; CL inspection on E1/E2/E3 |
| 4 | F | Accumulator (`.=` O(n)) special case. | benchmark + correctness |
| 5 *(deferred)* | G | A4 interprocedural return-type table → type `call` sources, enabling native `repr=number`/`fixnum` for sub-sourced numeric vars. | — |
| 5 *(deferred)* | H | Flow-sensitive (SSA) re-typing of reassigned scalars. | — |

**Every step is independently shippable and the specialization steps (D–H) are
strictly opt-in narrowings from `box`** — an incomplete or buggy analysis loses
an optimization, never correctness. That property is why this order is safe.

---

## (g) Open questions

1. **Where does `repr` analysis run** — inside `VarAnnotator` (it has the scope
   stack and use lists) or a dedicated `Phase 0c`? Leaning `VarAnnotator`,
   because `unboxable` already needs the same use-site list.
2. **`number` vs `fixnum`/`float` split** — worth it? Native `double-float`
   declarations help tight numeric loops, but Perl's auto-bigint promotion on
   overflow (`docs/sweep-bug-catalog.md` `**` issue) means `fixnum` is only safe
   when overflow is provably impossible. Probably keep `number` (boxed CL number,
   no SV) for the first cut and skip declared `double-float` until a hot path
   asks for it. **The 2026-06-25 brackets (see "Measured payoff") settle this for
   the first cut:** native-fixnum vs generic-unboxed CL is only ~1.6× apart, while
   generic-unboxed vs current PCL is ~300× — so almost the entire win is in
   *replacing generic `p-+`/`p-<` dispatch with a native op on raw CL numbers*,
   not in the `fixnum`/`float` declaration. Keep `number` and defer the split,
   which also sidesteps the bigint-overflow safety problem.
3. **Interaction with `*wantarray*`/context eval** — `not-supported.md`
   §"Context propagation into string eval" wants a `(let ((*wantarray* ctx)) …)`
   wrapper around `p-eval` once context annotations exist. That is the same
   `context` metadata this pipeline already produces (`annotate_contexts`), so
   step 1 likely unblocks kvhslice.t 9–12/25–28 as a side effect — worth
   verifying.
4. **Inter- vs intra-procedural boundary, and a "sealed sub" opt-in.** Everything
   above is **intra-procedural** and therefore safe no matter what is redefined
   at runtime: redefining `g` can't change the dataflow inside `f`, so unboxing
   locals + specializing operators *within a sub body* needs no guard. Runtime
   sub replacement (`*foo = sub{…}`, `local *foo`, AUTOLOAD, glob/stash writes)
   only caps the **inter-procedural** layer — inlining a callee, propagating a
   callee's return type into the caller (the Tier-3 return-type table, §A4, is
   deferred for exactly this reason), and devirtualizing method dispatch. Note
   SBCL gives us the safe default for free: ordinary calls go through the
   symbol's function cell, so a replaced sub is picked up automatically and
   nothing is inlined — we already pay the indirect call and already lose
   inlining. Two ways to opt back into the inter-procedural wins:
   - **(c) per-call-site guard (sound, preferred default):** specialize against
     the callee's *current* code object and emit one pointer-compare before the
     fast path — same code object → specialized; else → generic call. No deopt
     machinery; monkey-patching just falls back automatically.
   - **(b) a "sealed" promise (fast, unsound if violated) at two granularities:**
     a **whole-program compiler flag** (closed-world: assume no sub is redefined,
     inline/specialize freely), and a **per-sub marker**. The per-sub marker
     should be a **comment**, e.g. `sub fib { # pcl: sealed` …, *not* a sub
     attribute: a comment is invisible to stock perl, so the identical source
     still runs under real Perl (preserving "transpiler for valid Perl input"),
     while PCL reads it as a hint to seal just that sub. (A `:sealed` attribute
     is the fallback if a structured form is ever wanted, but it risks warnings
     under stock perl unless declared.) Decision: ship (c) as the default; add
     the seal flag/comment only for the last few percent on a proven hot path.
   Other hazards in the same callee-identity-dependent bucket (record here so the
   guard covers them too): `goto &sub`, tie/overload installation,
   `wantarray`-sensitive callees, `local` on globals. Escape/aliasing facts that
   do **not** depend on the callee's body (e.g. "the call didn't receive `\$x`,
   so `$x` is still a number after") stay valid across a call and need no guard.

## See also
- `docs/codegen-rewrite-spec.md` — the *code-shape* spec (this doc decides
  per-variable representation; that one decides the surrounding generated code +
  Tier 1–3 perf transforms + north-star output)
- `docs/ast-annotation-plan.md`, `docs/two-phase-compiler.md` (the two parents)
- `docs/let-scoping-problem.md`, `docs/closure-lexical-scoping.md`
- `docs/declaration-ordering.md` (defvar vs let at module load)
- `cl/pcl-runtime.lisp` — `to-string` (:1767), `to-number` (:1666), `p-.` (:1691),
  `p-true-p` (:1407, the string-flavoured truthiness rule — bool is free under
  stringify, see c.4), `unbox`/`box-set` (:647/:740),
  `p-aref`/`p-aref-box` (:4593/:4634), `p-gethash`/`p-gethash-box` (:4984/:5044)
