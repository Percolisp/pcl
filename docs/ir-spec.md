# PCL IR Manual — Semantics of the Generated CL

**Status:** normative. Where this document and the runtime disagree, the
runtime (`cl/pcl-runtime.lisp`) is the reference implementation and this
document has a bug — please fix it.
**Audience:** anyone (person or AI) consuming PCL's generated Common Lisp:
translating it to another target environment, building tooling on it, or
debugging it. You should be able to reimplement the semantics from this
document without reading Lisp.
**Companion:** `docs/generated-cl-ir-review.md` (what the output looks like,
its friction points, and the improvement roadmap). This manual covers what
the constructs *mean*.
**Verified against:** full per-claim verification at cache generation
v2-7 (2026-07-06); maintained incrementally since (each semantic
emission/runtime change updates its section — standing rule), last
section-level review at **v2-44, 2026-07-20 (s303: §2.2 freeze-licensed raw
slots — B-regime strict coercers); previously v2-43 (s302: §2.2 raw compound-assign
`-raw` twins, task #62 step 1)**. Section references
name the defining function so you can re-verify against the runtime.

---

## 0. The one-paragraph model

PCL compiles Perl to a tree of calls into a closed runtime vocabulary
(`p-*`). Perl scalars become mutable cells ("boxes") or, when the compiler
proves it safe, raw numbers/strings. Perl's implicit coercions live inside
the ops (`p-+` numifies, `p-.` stringifies), not in the code shape. Perl's
scalar/list context becomes a dynamically-scoped flag bound at call sites.
Non-local control (`return`, `last`, `die`) becomes tagged non-local exits.
Packages become namespaces loaded strictly in source order. A translator
reimplements the vocabulary and the five protocols below (§2–§8); the tree
itself carries no host-specific semantics beyond them.

## 1. Reading the output

- **Parse with an S-expression reader.** Symbols contain `$ @ % & : + - * /
  . < > = ~ ! # | ^` — never tokenize by regex. Strings are
  double-quoted with `\"` and `\\` as the only escapes; they may contain
  raw control characters (real newlines/tabs inside the quotes — see the
  review doc §3.2).
- **Comments (`;` to end of line) are non-semantic** — source echoes for
  humans.
- **The tree is explicit.** Perl's implicit operands are materialized at
  parse time: `$_` defaults (§8), `@_`/`@ARGV` for bare `shift`/`pop`,
  filetest operands. What you see in the tree is the complete argument
  list — there is **no** runtime-side argument default (bare
  `print;`/`say;`/`printf;` arrive as `(p-print $_)` etc., materialized by
  the codegen like every other `$_`-default form).
- **`(declare …)` forms are host compiler advice** — droppable wholesale.
- **No host-implementation symbols appear in the tree** (normative, and
  cheaply checkable: `./pl2cl FILE | grep -ac 'sb-[a-z]*:'` is **0** over
  the whole corpus, measured s382g). The runtime is free to use SBCL
  internals — it does, extensively — but every such mechanism reaches the
  emitted code wrapped in a `p-*` macro, so a translator reimplements the
  vocabulary of §10 and never an implementation detail.
  **This is an invariant to hold, not just a fact about today**: it would
  be easy to lose. Direction D (task #289) declares each ordinary package
  global as a symbol macro over a directly-addressed global cell, which in
  its natural spelling would put `sb-ext:symbol-global-value` at *every
  global declaration and every `local`* — thousands of sites. It does not,
  because both halves are runtime macros (`p-defcell`, `p-local-cell`) and
  the emitted forms stay `(p-defcell $x (make-p-box nil))` /
  `(p-local-cell $x INIT …)`. A port then owes two macro definitions
  instead of a new convention at every declaration. Any future change that
  needs a host primitive in emitted code should reach for the same shape.
- **Order is the program.** A file is a sequence of top-level forms
  executed in order by `load`. There is no linker step: a name must be
  defined by an earlier form (or a forward declaration) before a later
  form *executes* it. `(eval-when (:compile-toplevel :load-toplevel
  :execute) …)` marks forms that Perl requires visible to `BEGIN` blocks
  earlier in the same file; a translator with no compile/load phase split
  simply executes them in order — but see §9 for `BEGIN` itself.
- **The first line is the pipeline marker** — `;;; pcl: pipeline=v2
  gen=v2-NN` — a comment stamping which pipeline and cache generation
  emitted the file. Non-semantic like all comments, but it is the
  version key tooling should read, and it is a **promise**, not an
  incidental: see §9.2.
- **File layout:** preamble (`in-package`, `@INC` setup) → per-package
  *sections* in source order, each: package preamble → declarations
  (`p-declare-sub`, `defvar`) → definitions → runtime (top-level
  statements). Within the definitions stream, `p-sub` forms and
  scheduled blocks (`p-BEGIN`/`p-END`/…) are **interleaved by source
  position** (s300b, #55): a `BEGIN` form loads after exactly the subs
  defined above it in source and before those below — this ordering is a
  semantic guarantee (compile-time sub-existence introspection relies on
  it), not a formatting choice. A `package` statement mid-file starts a
  new section; a reopened package gets only `(in-package …)` +
  `p-set-current-package`.
- **A `package X;` INSIDE A BLOCK is not expressed by the emitted
  `in-package`** (#239, s378). A block lowers to ONE top-level form, and
  the CL reader interns every symbol of a top-level form before it is
  evaluated, so a nested `(in-package :X)` cannot re-home the symbols
  around it. Perl's switch is nonetheless lexical and covers the rest of
  the block — nested blocks and nested sub bodies included. The compiler
  therefore **spells the region's variables package-qualified at the
  source level** before lowering (`Pl/Parser2.pm`
  `_requalify_block_globals_after_pkg_switch`): `$z` → `$X::z`,
  `@a` → `@X::a`, `$a[0]` → `$X::a[0]`, `$#a` → `$#X::a`, and the same in
  interpolating text. A consumer reading the emitted CL therefore sees
  explicit `X::$z` symbols, never a bare name whose package depends on an
  enclosing `in-package`. Lexicals, in-scope `our` aliases (which resolve
  to their DECLARING package), already-qualified names and the always-main
  specials are left where they are; `$a`/`$b` are excluded because the sort
  lowering lexically binds those two symbols. Sub definitions, `*glob`
  installs and bareword calls in such a region never had the problem —
  they resolve through the package stack at lowering time.

## 2. The data model

### 2.1 undef

Perl's `undef` is the singleton `:undef` (the keyword — held in
`*p-undef*`), **not** the host's null. The host null (`nil`) *also*
appears, with a different meaning: an array slot that was never assigned
(a hole), or "absent". `p-exists-array` distinguishes a hole (`nil`) from
a stored `undef`. Rules: `undef` numifies to 0, stringifies to `""`, is
false. A translator needs two distinguishable values here.

### 2.2 Scalars: boxes and raw slots

A Perl scalar is by default a **box** (`p-box`, defined at
`defstruct p-box`): a mutable cell with fields

| field | meaning |
|---|---|
| `value` | the authoritative value |
| `nv`/`nv-ok` | cached numeric view + validity flag |
| `sv`/`sv-ok` | cached string view + validity flag |
| `class` | blessed class name (nil unless blessed) |
| `is-ref` | t when the box was created by the reference operator `\` |

The nv/sv caches are **pure optimization** — every write
(`box-set`) invalidates them; a translator may omit caching entirely.
`class` and `is-ref` are semantic.

When the compiler (VarAnnotator) proves a `my` scalar is only ever written
raw-value shapes, it emits a **raw slot** instead: a plain host variable
holding a number or string directly. The shapes tell you which is which:

| kind | declaration | write | read |
|---|---|---|---|
| boxed lexical | `(let (($x (make-p-box nil)))` | `(p-my-= $x V)` | `$x` |
| raw lexical | `(let (($n 10))` | `(setf $n V)` | `$n` |
| package var | `(p-defcell $g (make-p-box nil))` — `defvar` for the exception set (§2b.1) | `(p-scalar-= $g V)` | `$g` |

A **coercing compound assignment** on a raw slot (`$n += V` and the whole
`-= *= /= %= **= x= .= <<= >>= &= |= ^= &.= |.= ^.=` family — every op
whose stored value is an operator result, so a raw number/string by
construction) lowers to the boxed macro's **`-raw` twin**:
`(p-incf-raw $n V)`, `(p-.=-raw $s V)`, … — each expands to
`(setf slot NEW)` with the *same* new-value form its boxed macro
(`p-incf`, `p-.=`, …) computes, so the two store disciplines cannot
diverge semantically (task #62; `docs/raw-numeric-verdict.md`).  The
non-coercing `||= &&= //=` store the RHS unchanged (it may be a
reference), so their targets never become raw slots.  A statement-root
`$x++;`/`$x--;` likewise lowers to `(p-incf-raw $x)`/`(p-decf-raw $x)`
when every other write to `$x` is numeric-valued (the A-num rule —
magical string increment is then unreachable); a tail-position postfix
wraps in `prog1` to return the old value.

**Invariant: a raw slot never holds a box or a reference** — only host
numbers and strings. Ops always accept either form (they unbox
internally), so reads look identical.

A raw slot may also be **freeze-licensed** (the B-regime, s303, task #62):
when every USE of the variable is provably numeric (resp. string/boolean)
but a write's value shape is unproven (`my $n = $h{k}`, `$x = $y`, a bare
sub call), the slot stays raw and every native write routes through
`(%pcl-to-number-strict V "$n")` / `(%pcl-to-string-strict V "$n")` — an
eager coercion that preserves the invariant (the stored value is always a
plain host number/string), applies box-set's aggregate scalar-context
collapse, and **dies loudly** if an overload-capable blessed ref or a
genuine dualvar arrives (never freeze what per-use code must observe).
Semantics and the full licensing/disqualifier tables:
`docs/raw-numeric-verdict.md`.

A freeze-licensed slot whose only writes are plain roots + `.=` and whose
every use is a *transient* stringify/boolean read may further become a
**str-buffer** (S1): the slot holds an adjustable fill-pointer string,
plain writes replace it via `(%pcl-str-buffer V)`, and `.=` appends in
place via `(%pcl-str-append $s V)` — O(1) amortized instead of a fresh
concatenation per append.  **Consumer contract: the buffer object never
crosses the IR boundary.**  Every escape channel — sub return, call
argument, store into a box/container, package var, hash key (the table
retains the key object) — is an opaque/retaining use that disqualifies
the verdict, so host code calling PCL-generated CL only ever receives
ordinary simple strings; and even internally, every standard string
operation respects the fill pointer.

`box-set` semantics worth knowing: assigning a whole array to a scalar box
stores its element **count** (Perl array-in-scalar-context); assigning a
box created by `\` (is-ref) stores the reference value, not the wrapper.

### 2.2b Tied scalars — the raw slot behind the magic

`tie $x, 'Class'` replaces the box's `value` with a **`p-tie-proxy`**
holding the tie object and the box's previous value.  Every read
chokepoint (`unbox`, `box-nv`, `box-sv`, the invocant resolution in
`p-method-call`) dispatches to `FETCH`; every write chokepoint
(`box-set`, `p-scalar-=`) dispatches to `STORE`.

The proxy's `saved-value` is not merely a restore-on-untie snapshot — it
**is the SV's raw slot**, and the following rules make it behave like
perl's (mg.c).  A port that models tie must reproduce all four:

1. **A write hits the raw slot first, then calls `STORE`** (perl's
   `sv_setsv` + `mg_set` order), so `untie` after `$tied = "A"` leaves
   `"A"` behind.
2. **`FETCH`'s result is written back to the raw slot** (`magic_methpack`
   ends in `sv_setsv(sv, result)`), so `untie` after a read leaves the
   fetched value.
3. **While a handler for a cell is on the stack, that cell's magic is
   OFF** (`save_magic`/`restore_magic`): reads and writes of the tied
   variable *inside its own* `FETCH`/`STORE` hit the raw slot instead of
   re-entering the handler.  This is not an optimization — Math::BigInt's
   `sub STORE { $rnd_mode = (ref $_[0])->round_mode($_[1]) }` assigns to
   the very variable it proxies, and without suppression it recurses
   until the stack dies (task #224).  Suppression is **per cell** (a
   second tied variable still dispatches from inside), ends with the
   handler including on a non-local exit, and does **not** hide the tie
   from `tied()`, which still reports the object.
4. **Re-tying replaces the magic, it does not stack**: a second `tie` on
   an already-tied cell carries the existing raw slot forward rather than
   saving the old proxy as the new "value".

PCL implements 1–4 by swapping the raw value into the box for the
duration of the handler call (`%with-tie-magic-off`), so no read/write
site needs a special case; only `tied`/`untie` consult the suppression
list.

### 2.3 Arrays

A Perl array is an **adjustable vector with a fill pointer** (growable,
ordered, integer-indexed). Elements are boxes (or `nil` for holes;
elements of proven-safe intermediate lists may be raw values). Negative
indices count from the end. Reading past the end yields `undef` without
growing; **writing** past the end extends the vector, filling the gap with
holes.  Growing an array by **assigning to `$#a`** (`p-set-array-length`,
incl. the `$#a++` idiom) likewise fills the new slots with holes (`nil`),
never with fresh boxes — `exists $a[$i]` on the new slots must stay false
(s295; a port that models holes as "slot absent" gets this for free). `p-aref` (read) **unboxes scalar elements but returns reference
elements still boxed** (`p-aref-unbox-elem`) — so `==` on two references
compares object identity, not content. An array in numeric/scalar position
coerces to its length.

**Read-only arrays (s337, task #159):** the ONE case where the storage is not
adjustable.  `Internals::SvREADONLY(@a, 1)` replaces the variable's storage
with a **simple vector** — same element boxes, no fill pointer, not adjustable
— because perl's read-only AV is precisely a *fixed-size* array whose elements
remain writable.  The predicate is therefore the storage itself
(`%p-array-readonly-p` = a non-string vector with no fill pointer), and every
size-changing entry point (push/unshift/pop/shift/splice/delete, whole-array
assignment, `undef @a`, the out-of-bounds extend inside element writes, and
growth via `$#a`) checks it and raises perl's `Modification of a read-only
value attempted`.  A port that has no equivalent of "fixed-size vector" needs
an explicit per-array flag consulted at those same points; nothing else in this
spec changes.

**Hole aliasing (defelem, s316e):** when a hole slot is *aliased* — by a
foreach/grep/map `$_` binding or by spreading the array into `@_` — the
alias is a **deferred-element box** (`%p-defelem-box`): a box whose value
is a `p-magic-cell` of `:kind :defelem`, getter → `undef`, setter →
de-magic the box, store it into the source slot, re-dispatch through
`box-set`.  Reads never vivify; the first write through the alias does,
exactly perl's defelem magic.  Consequences for a port: array slots
themselves never hold a still-magic defelem (the setter de-magics before
storing), but *flattened views* (`@_`, a foreach binding) can —
`p-exists-array` and `p-aref-unbox-elem` treat that state as a hole.  A
bare lexical `@array` as the sole foreach list is iterated **live** (no
copy): `push` during the loop extends the iteration, like perl.

### 2.4 Hashes

A Perl hash is a string-keyed equality hash table. **All keys are
stringified on the way in** (`(to-string key)`). Values are boxes or raw
values; `p-gethash` unboxes scalars and preserves reference boxes, exactly
like `p-aref`. A hash in numeric position coerces to its key count.
`%ENV` is special-cased: the table holds a marker and reads/writes go to
the process environment.

### 2.5 References

`(p-backslash X)` — Perl `\X` — returns a **box with `is-ref` = t** whose
`value` is:
- for a scalar: the scalar's *box* (so writes through `$$r` hit the
  original — a box-in-box);
- for an array/hash/code/glob: the underlying vector/table/function
  itself;
- for a raw value (`\42`): a fresh box wrapping it (so `$$r += 1` works).

Dereference ops unwrap one level. Reference identity = identity of the
referenced structure. Stringification of a reference yields
`"HASH(0x…)"`-style text; numification yields the object address.

**The printed type and the address are properties of the REFERENT, never of
the wrapper** (normative, task #163). `\` allocates a fresh wrapper on every
evaluation, so any rule that reads the wrapper makes `\$x == \$x` false and
prints two addresses for one variable. `%p-ref-referent` is the one rule that
resolves the referent, and `is-ref` on the wrapper is its only discriminator:

- the value handed to a stringifier is **either the wrapper itself** — `\$x`
  reaching `print`, an array/hash element, or a raw (`p-raw-params`) parameter
  — **or a variable box holding one** (`my $r = \$x`).  `is-ref` says which;
  counting box levels cannot, and a level-counting box-sv is exactly why the
  same reference used to print `SCALAR` through a variable and `REF` straight
  into `print`.
- `p-ref`, `box-sv`, `stringify-value` and `box-nv` all read that one rule, so
  the word and the number agree no matter which path a reference took.
- **`SCALAR` vs `REF` is dynamic**: it is decided by what the referent holds
  *at that moment*, exactly as perl does. `my $r = \1; my $rr = \$r;` prints
  `REF`; after `$r = 5` the same `$rr` prints `SCALAR`. A reference's string is
  therefore **not cached** on the holding box — the referent is a different box
  and writing to it cannot invalidate a cache there. (`box-nv` refuses to cache
  address-based NVs for the same reason.)
- A reference **to a plain scalar is not a container**: `@$sref`, `%$sref`,
  `$sref->{k}`, `$sref->[0]` are perl's fatal `Not a(n) HASH/ARRAY reference`.
  What PCL still tolerates is one *representation* layer (a box left over after
  one unwrap) — see `%p-scalar-referent-p`; that leniency exists only because
  the parser drops the outer level of `$$refref->{k}` (task #211), and goes
  away when that is fixed.
- **A TYPEGLOB is the one payload whose ref-ness lives on the box, not on the
  object** (normative, task #423). Perl distinguishes a glob *value*
  (`$g = *foo`, which turns the SV into a GV: `ref($g)` is `""`, `"$g"` is
  `*main::foo`, `0+$g` is 0, and `\$g` is a **GLOB** ref) from a glob
  *reference* (`$g = \*foo`: `ref($g)` is `GLOB`, `"$g"` is `GLOB(0x…)`,
  `0+$g` is the address, and `\$g` is **REF**). Both hold the same
  `p-typeglob` in the box, so **`is-ref` is the whole distinction** — every
  reader asks it (`box-sv`, `box-nv`, `p-ref`, `%p-ref-string`, via
  `%p-glob-value-box-p`), and every path that *copies* a scalar must carry it
  (`box-set`, `%p-flatten-list`, `%p-array-store-scalar`, `p-aref-unbox-elem`,
  `p-flatten-args`, `p-return-value`). A copy that drops the flag silently
  demotes a glob reference to a glob value; a copy that keeps the *source box*
  instead of a flag-carrying snapshot **aliases**, and `($g1,$g2) = ($g2,$g1)`
  collapses to one glob. A **raw** `p-typeglob` outside a box is a glob VALUE
  by the same convention `stringify-value` uses (#316).

**Ref identity is a monotonic id, deliberately NOT a machine address**
(`object-address`, `cl/pcl-runtime.lisp`). A translator must reproduce the
*invariants*, not the digits:

- **Stable for the object's lifetime.** SBCL's compacting GC relocates
  objects and PCL re-boxes refs on some paths, so the raw pointer is not an
  identity. The failure this was built for: a coderef threaded through
  Sub::Defer's coderef-keyed `%DEFERRED` presented two different
  `CODE(0x…)` strings for one logical sub, breaking Moo's lazy/subclass
  bootstrap.
- **Never reused.** Ids come from a monotonic counter, so two distinct
  objects can never share one — *stronger* than perl, which recycles freed
  addresses. Code that relies on address reuse is relying on a defect.
- **Non-leaking.** The object→id table is weak-on-key, so dead objects drop
  out.
- **Consistent between the two views.** `refaddr`, `==` on refs, and the
  `0x…` in stringification are the same number, which is what
  `t/op/bless.t`'s `expected()` checks (`hex($addr) == $ref+0`).

The numbers are therefore small and dense (`ARRAY(0x1)`), not
address-shaped, and that is correct rather than approximate. This is the
worked example of the general rule: **match Perl's shape and invariants, not
its bytes — and where Perl's bytes carry a defect, do better.** The
deliberate exception is a value a program can legitimately branch on, e.g.
the seeded `rand` sequence, where PCL reproduces perl's drand48 exactly
(`p-srand`/`p-rand`).

### 2.6 Blessed objects, strings, numbers

`bless` records the class on the thing all aliases share, mirroring
perl's referent-attached stash: for a hash object the class rides in the
hash itself (`:__class__` key); for a **scalar ref** it is written to the
*referent box* (s314, `%p-scalar-ref-referent`), and `ref($x)` /
`p-get-class` consult the referent first (`%p-referent-class`) before any
class cached on a wrapper or variable box. Those cached `class` slots
still exist — `p-bless` writes them and `box-set` copies them — but only
as caches for fast is-object checks; the referent is the truth, which is
what makes a second `\$x` wrapper and a re-bless through one alias
behave, and what XS reads as `SvSTASH(SvRV(rv))`. Array/code/glob refs
keep the class on the wrapper box (their raw referents cannot carry a
slot). `box-set` copies a class only when the assigned *value* is itself
a reference: copying a plain value out of a blessed referent yields an
unblessed scalar, exactly as perl's SV-attached stash does not travel
with the value. The *identity* of an object is the underlying
hash/array/referent — two references to it share blessing.

Strings are host Unicode strings (character, not byte, semantics — see
`docs/not-supported.md` §Unicode for divergences). Numbers are host
integers (arbitrary precision) and IEEE doubles; `*read-default-float-format*`
is double-float, i.e. every float literal in the output is a double.

## 2b. Declarations, scoping, and the rename families

How a Perl declaration becomes CL is the one place where the transpiler
does real *surgery* on the program: some variables are silently renamed to
fresh unique names. A consumer will meet symbols like `$tick__file__0` and
`$x__shadow__1` in the output; this section explains why they exist, what
each family means, and the rules that keep the surgery sound.

### 2b.1 The fundamental tension

Perl file/block lexicals (`my`) and CL top-level forms don't line up:

- In Perl, a **named sub sees file lexicals declared above it**, and
  `BEGIN` blocks can run code that touches variables mid-file.
- In PCL's output, named subs are **hoisted** into the definitions bucket
  (so calls work regardless of definition order), and each top-level form
  is loaded independently — a CL lexical `let` cannot span top-level
  forms, and a hoisted `defun` cannot see a `let` in the runtime bucket.

The **v1 pipeline** resolved this with a hammer: every `my` became a
global `defvar`'d box (dynamically scoped). That makes hoisted subs and
BEGIN blocks work, but it breaks real lexical semantics — `defvar`
*proclaims the symbol special globally*, so **every** later `let` of the
same name in the file silently becomes a dynamic binding: closures
capture one shared cell instead of per-instance cells, and a `my $x`
inside a `map` block writes through to the outer `$x`
(`docs/closure-lexical-scoping.md`). v1 patched the worst of this by
renaming *closure-captured* block lexicals to fresh `$x__lex__N` names
(fresh ⇒ never `defvar`'d ⇒ the `let` stays truly lexical).

The **v2 pipeline** (the default) inverts the model: a `my` is a true CL
lexical — `(let (($x INIT)) …rest-of-block…)` — and the hard cases are
handled by *renaming the variable to a fresh package-level cell* instead
of poisoning the name. The prime invariant, worth memorizing:

> **A let-bound name is never `defvar`'d.** (`_forward_global_decls`
> excludes every name in the cumulative lexical set `_all_lex`.)
> When a lexical must be globally visible after all, it gets a **fresh
> unique name** first, so the `defvar` cannot poison any other `let`.

Since s382h (task #289, "direction D") the *poisoning* half of that
tension is gone at the root, because **an ordinary package global is no
longer a special variable at all**. It is a symbol macro over its own
global value cell:

    (p-defcell $x (make-p-box nil))     ; ≈ (define-symbol-macro $x <cell>)
                                        ;   + initialize the cell once

Two consequences a consumer must know:

- A `let` of such a name is legal CL and installs an ordinary **lexical**
  binding — it does *not* rebind the global. That is precisely what Perl's
  `my $x` shadowing a package `$x` means, so the shadow no longer leaks
  into called subs.
- `local` therefore cannot be a `let`: it lowers to `p-local-cell`, which
  saves the cell, installs the new value, and restores under
  `unwind-protect` (§7.2).

**Exception set** — names that keep `defvar` and the dynamic `let`, decided
by name alone (`Pl::GlobalPartition`, the one authority both emitters ask):
everything not word-shaped (punctuation and caret magic — `$@`, `$1`, `$!`,
`|${^WARNING_BITS}|`, …) plus the runtime-owned word-shaped set
`$_ @_ %_args @ARGV $ARGV @ARGVOUT @INC %INC %ENV %SIG` and the sort pair
`$a`/`$b` in every package. For those a dynamic binding *is* the mechanism
(sort binds the pair; the magic vars are where `local` actually runs hot),
and mixing the two declarers on one symbol is a load-time error in SBCL —
loud, never silent.

**Punctuation names are `main::` everywhere** (perlvar: "forced to be in package
main").  The punctuation SCALARS and the regex arrays/hashes (`$_ $0 $! $, $\
@- @+ %+ %-` …) are runtime symbols exported from `:pcl`, so every user
package (which `(:use :pcl)`) reads the same symbol; since s440 (#498) the
punctuation ARRAYS `@? @! @. @/ @~ @^ @& @% @= @< @>` and the synthesized
`@#` are owned the same way — the compiler emits the bare name and declares
nothing, so `@?` written under `package A` and read under `package B` is one
array.  A per-package `(defvar @? …)` was the old emission and made them two.

### 2b.2 The declaration forms

| Perl | v2 emission |
|---|---|
| `my $x = INIT;` (boxed) | `(let (($x (make-p-box nil))) (p-my-= $x INIT) …rest…)` |
| `my $x = INIT;` (raw slot, §2.2) | `(let (($x INIT)) …rest…)` |
| `my $i = $i + 1;` (self-ref init) | `(let (($i (p-box-init $i))) …)` — CL `let` inits evaluate in the *outer* environment, matching Perl's "RHS sees the outer variable" rule |
| `my @a; my %h;` | `let` binding to a fresh vector / hash table |
| `our $g = V;` | `(p-defcell $g (make-p-box nil))` hoisted to the section's declarations + `(p-scalar-= $g V)` in place (`defvar` instead for an exception-set name). `our` shadowing a `my` gates to v1 |
| `state $n = 0;` in a **named sub** | native since s277c: the variable is renamed to a per-sub package cell (§2b.3) — `(p-defcell $n__state__K (make-p-box nil))` + raw once-flag `(p-defcell $n__state__K__init nil)` hoisted to the declarations; the statement lowers to `(unless $n__state__K__init (box-set $n__state__K INIT) (setf $n__state__K__init t))` followed by the bare cell as the statement value. One cell per named sub = exactly Perl's named-sub `state` semantics. `state` *outside* named subs, in anon subs / map-grep-sort blocks (per-closure instances), list/non-scalar `state`, and blocked renames still gate → v1, which uses its own `$state__<sub>__<name>__N` cells |
| undeclared globals | swept up at assembly time (`_forward_global_decls`, a text scan over the finished section): every referenced name gets `(p-defcell NAME <fresh container>)` — `(defvar …)` for the exception set — under "Forward declarations"; package-qualified refs get the declaration in *their* package |
| `state $n` cells, promoted lexicals (`__file__`/`__state__`) | the same `p-defcell`, since the renamed name is word-shaped and therefore ordinary |

**A name may be BOTH declared here and `let`-bound in the same section**, and
that is not a contradiction: a `p-defcell` is a symbol macro, which a `let` of
the same name lexically shadows.  So the scan declares a cell for every name it
sees and never asks which role the name plays — under `defvar` it had to ask
(a special proclamation would have turned the section's own `let`s into dynamic
rebinds), and the three approximate answers it used were the `__shadow__` /
`__cond__` / `__emb__` renames, deleted in #291.  The cost is an inert cell for
a name that is only ever lexical here; the benefit is that "is this a lexical
or a global?" stops being a question the compiler has to guess (see also §6.2's
`:my`, the one place the guess survives and is stated instead).

The rest-of-block nesting means scope is structural (review doc §2.3):
reading a `)` closes a scope. At file level the `let`s nest the remaining
*runtime* statements of the section — the hoisted definitions sit outside,
which is exactly why the rename families below exist.

Bookkeeping a consumer can ignore but a debugger should know: every `my`
registers in three sets (`_reg_lex`) — `_let_bound_vars` (scoped, drives
the seam's my-vs-package decisions and the string-eval capture alist),
`_live_lex` (scoped, drives capture *gates*), `_all_lex` (cumulative,
drives the never-defvar exclusion).

### 2b.3 The rename families

All renames happen **at the PPI token level, before lowering**
(`set_content` on the parse tree), so the native path, the v1 fallback
seam, and every analysis pass see the new name with no further plumbing.
Numbering comes from per-file counters iterated in **sorted order** —
hash-order iteration once made the numbering nondeterministic per process,
churning the module cache. Every rename preserves one Perl visibility
rule: in `my $x = $x + 1`, the RHS reads the *outer* `$x`
(`_rename_decl_within` skips tokens inside the declaration statement).

| symbol shape | family | meaning |
|---|---|---|
| `$x__file__N` | v2 file-cell promotion (W5/W10) | a file lexical that must be visible outside the `let`s — because a named sub captures it, because a **BEGIN/END scheduled block references it** (their `p-BEGIN` forms live in the definitions stream, outside the runtime `let` chain — s295c, source-position interleaved since s300b: `my $x; BEGIN { $x = 5 }` / END-cleanup idioms), or because it spans a `package` boundary — promoted to a package-level CELL (the `our` shape: p-defcell + `p-scalar-=`, no `let`). The fresh name is the whole point: declaring `$x__file__0` cannot disturb an unrelated `let $x` |
| `$Pkg::x__file__N` | v2 spanning refs (W10) | uses of the above from *later package segments* — package-qualified so their section's reader (sitting in its own package) reaches the declaring section's cell |
| `$x__shadow__N` | v2 seam-shadow rename (W8.5) | a `my $x` *inside a block that lowers through the v1 seam* (`map { my $x = … }`, `do { my $x … }`) while an outer lexical `$x` is live. Unrenamed, the seam's defvar-based handling would write through the outer variable (the v1 bug); renamed, the inner block gets its own unique cell |
| `$x__cond__N` | v2 poisoned-condition rename (W8.5) | `if (my $x = …)` / `for (my $x…)` where the *same bare name* is also used outside the construct as a package global. The construct's lexical takes the fresh name so the global keeps `$x` and gets its forward defvar |
| `$x__emb__N` | v2 embedded-`my` rename (W8.5, #265) | an *expression-embedded* `my` **inside a named sub** (`++my $x->{k}`, `open my $fh, …`, `… if my $x = …`) whose bare name is also mentioned by another named sub in the segment. Unrenamed, the let-hoist refuses the decl (it cannot tell that other sub apart from one sharing a file-level cell) and the `my` writes the package GLOBAL — persisting across calls. Renamed, the sub gets its per-call `let` AND the global keeps `$x` and its forward defvar |
| `$x__lex__N` | v1 closure-capture rename | v1's fix for defvar-poisoned closures: a block `my` captured by a nested sub becomes a fresh, never-defvar'd name so its `let` stays truly lexical. Appears in v2 output too, inside seam-lowered map/grep bodies |
| `$x__state__N` (+ `…__init`) | v2 state cells (s277c) | a named sub's `state` variable promoted to a per-sub package cell + raw once-flag (see the declarations table above); same blockers as the other renames |
| `$state__<sub>__<name>__N` (+ `…__init`) | v1 state cells | same idea, v1's spelling — seen in v1-dialect files |
| `--anon-block-N--` | both | hoisted anonymous-block functions (block-form prototype args: `first { … }`, `sort` comparators via the seam) |
| `--pcl-if-ret--N`, `%_args`, `$state…` | both | compiler temporaries; never user-visible names |

For a translator the practical takeaway is reassuring: **renames need no
special handling**. By the time the tree reaches you, a renamed variable
is just an ordinary variable with an unusual name — read its kind off the
emitted shape exactly as for any other name (§2b.2): `__file__` cells are
defvar'd package vars; `__cond__`/`__emb__`/`__lex__` are `let`-bound lexicals;
`__shadow__` is whichever the lowering path produced (observed: the v1
seam emits it as a defvar'd cell — sound *because* the name is unique).
The suffixes matter only for mapping output back to source (strip
`__family__N` to recover the Perl name).

### 2b.4 The guard rails (when renaming refuses)

Renaming by token rewrite is only sound when the token walk can *reach
every use*. Each pass checks blockers (`_shadow_rename_blocker`,
`_scan_lex_facts` disqualifiers) and, on any hit, refuses the file with
a hard `Parser2 TODO:` error rather than renaming unsoundly (before the
E4.1 flip these conditions gated the file to the v1 pipeline; v1 is
gone, so a refusal is now terminal):

- **Interpolated uses** — `"$x"`, `/$x/`, heredocs: the name lives inside
  a quote token the Symbol walk can't rewrite.
- **Brace-deref** — `${x}`: same reason — for the passes that still walk
  Symbols only.  The *spanning* rename resolves and rewrites this shape
  since s363 (#264): one helper (`_brace_name_refs`) answers "which
  canonical variables does this node mention that way" for both its
  detector and its rewriter.
- **Shadowing / multiple declarations** of the bare name: a single
  positional rename would merge distinct scopes.
- **Array/hash family sharing the bare name** — `@x`, `%x`, `$#x`,
  `$x[i]`, `$x{k}` are *different variables* whose element-access tokens
  share the `$x…` spelling.
- **String `eval` in scope** — *narrowed (s294/295)*: a renamed cell is
  made reachable by its original source name (the alias rule + span
  pairs, §9.1), so string eval alone no longer refuses a rename. What
  still refuses: a **container** (`@x`/`%x`) promotion with a post-decl
  string eval (only scalar cells are aliased today), and a renamed decl
  **nested inside an outer `my` of the same bare name** (the site
  alist's let-bound pair precedes the global in resolution order, so
  the deeper cell could never win the by-name lookup). The `cond`
  family no longer refuses (s363, #254 B-i): `$x__cond__N` is
  let-bound, and `_eval_lexical_alist` strips `__cond__N` like the
  other let-bound suffixes. `state` keeps the blanket refusal — see the
  three-route rule below.
- **`state`** — its per-instance semantics run through the separate
  `state_var_renames` machinery; token-renaming would bypass it.

**The three-route eval-visibility rule (s363/s364, normative).** Which
mechanism makes a renamed cell reachable from a string eval under its
original source name decides whether a rename may waive the string-eval
refusal (`eval_ok`):

1. A **let-bound** rename (`__lex__`, `__shadow__`, `__cond__`) is
   carried by the eval-site alist; `_eval_lexical_alist` strips the
   suffix to recover the key, and the pair exists only while the
   binding is in scope — so an eval inside the construct sees the
   lexical and one outside sees the global, matching perl.
2. A **defvar'd package cell** (`__file__` span/capture promotions) is
   reached through the alias rule (`p-alias-eval-cell`) plus the
   cross-package span pairs (§9.1).
3. A cell reachable by **neither** mechanism keeps a hard refusal:
   today `state`'s `__state__` cells (separate per-instance machinery)
   and container promotions with a post-decl eval.

A new rename family must decide which of the three it is before it may
pass `eval_ok`.

Where no rename applies and the capture would misbehave, the same
conditions exist as hard *gates* (`_check_sub_captures`,
`_check_my_spanning`, the block-form-arg capture gate): the file is
refused with an error naming the condition. (Pre-E4.1 these routed the
file through v1's defvar model; that fallback no longer exists.)

## 3. Coercion — the heart of Perl semantics

Every op coerces its operands itself. The three canonical functions:

### 3.1 `to-number` (numification)

| input | result |
|---|---|
| number | itself |
| box | its value, numified (cached) |
| `undef`, `nil` | 0 |
| host boolean true | 1 |
| string | leading-numeric-prefix parse, below |
| array | length |
| hash | key count |
| anything else | 0 |

String parse (`parse-perl-number`): trim leading whitespace; accept
optional sign, then the longest prefix that reads as integer / float /
scientific-notation / `Inf`/`Infinity`/`NaN` (case-insensitive); ignore
everything after it (`"3rd"` → 3, `"3.14foo"` → 3.14); no numeric prefix
→ 0. (Real Perl warns on trailing garbage; PCL does not.)

### 3.2 `to-string` (stringification)

Strings pass through; boxes stringify their value (cached); `undef` → `""`;
integers print exactly; floats print in Perl's `%.15g`-equivalent shortest
form (`0.5` not `0.5d0`; integral floats print without `.0`); references →
`"TYPE(0xADDR)"`; blessed references → `"Class=TYPE(0xADDR)"`.

### 3.2b Interpolation extent — which text belongs to a `$`/`@` reference inside a dq string, regex or heredoc (normative, s426)

ONE reader decides it — `Pl/InterpScan.pm` (`scan_one`), consumed by the
regex-pattern interpolator (s382f) and by `Pl/PExpr/StringInterpolation.pm`
(s426); the contract is `docs/interp-scan.md`.  A translator must reproduce
these rules (every one probed on perl 5.40.3):

- `$name` / `@name` continue into a subscript CHAIN — `"$x[1]"`,
  `"$h{k}[0]"`, `"$r->[0]{k}"`, `"@x[0,1]"`, `"@h{'a','b'}"`; an arrow is
  taken only when `[` or `{` follows it (`->@*` only under `postderef_qq`).
- A DEREF base continues the same way: `"$$r[1]"` is `$r->[1]`, `"@$r[0,1]"`
  is a slice, `"$$h{k}"` an element, `"${$r}[1]"` is `$r->[1]`.
- A braced NAME **closes** the reference — `"${x}[0]"` is `$x` followed by a
  literal `[0]`; so do the braced magic spellings `${^NAME}`, `@{^NAME}`,
  `${+}`, `@{+}` (the magic arrays `@-`, `@+`, `@{^CAPTURE}` join with `$"`).
- A braced EXPRESSION does **not** close it: `"${$r}[1]"` is 20,
  `"@{$hr}{'a','b'}"` is a hash slice, `"@{[1,2]}[0]"` is 1, and
  `"${\ $x}[0]"` dies "Not an ARRAY reference" — perl TOOK the group.
- The `$#` family — `$#name`, `$#{name}`, `$#$r`, `$#{EXPR}`, `$#-`, `$#+` —
  never chains (a following `[` is literal text; in code it is a syntax
  error).
- The VALUE of a reference is what the equivalent CODE gives: a deref base,
  a braced expression, a second subscript group or an explicit arrow is
  compiled from the reference's own source text through the ordinary
  expression pipeline, so a string and the code it abbreviates cannot
  disagree (the #443 wrong-kind-deref leniency is therefore shared with
  code, not an interpolation bug).  Residue: the `@{ EXPR }` arm unescapes
  the block text and the `${ EXPR }` arm does not (#444).

### 3.3 `p-true-p` (truthiness)

False: the number 0 (but **NaN is true**), the strings `""` and `"0"`,
`undef`, `nil`, empty array, empty hash. Everything else is true —
including `"0.0"`, `"00"`, `" "`, and all references. Boxes test their
value. Blessed objects consult `use overload 'bool'` first.

### 3.4 What ops return

- **Numeric/string operators** (`p-+`, `p-.`, …) return **raw** host
  numbers/strings — never boxes. This is what makes raw slots sound.
- **Comparison operators** return Perl booleans: `1` (number) for true,
  `""` (empty string) for false — *not* host booleans. `p-<=>`/`p-cmp`
  return -1/0/1.
- **Logical operators** `p-&&`, `p-||`, `p-//` return the deciding
  *operand's value* (Perl's `$a || $default` idiom): `p-&&` returns the
  first operand if false, else the second; `p-||` symmetrically; `p-//`
  keys on definedness. They are macros — the second operand is not
  evaluated unless needed.
- **Overload protocol:** every arithmetic/string/compare op first checks
  whether a blessed operand's class declares `use overload` for that
  operator and dispatches to it (`p-find-overload`/`p-call-overload`).
  Translators supporting objects must preserve this hook order:
  left operand, then right (with the swapped-args flag).

## 4. Context (scalar / list / void)

The dynamic variable `*wantarray*` carries the calling context:

| value | meaning |
|---|---|
| `t` | list context |
| `nil` | scalar context |
| `:void` | void context |

**Call sites bind it** where the callee is context-sensitive, and the bind
is **named** (s414, #281 item 1) — these four macros are the emitted
spelling:

| emitted form | expands to | meaning |
|---|---|---|
| `(p-list-ctx BODY…)` | `(let ((*wantarray* t)) BODY…)` | list context |
| `(p-scalar-ctx BODY…)` | `(let ((*wantarray* nil)) BODY…)` | scalar context |
| `(p-void-ctx BODY…)` | `(let ((*wantarray* :void)) BODY…)` | void context |
| `(p-caller-ctx BODY…)` | `(let ((*wantarray* *pcl-caller-wantarray*)) BODY…)` | propagate the caller's context (`goto &sub`, tail call) |

The expansion is exactly the `let` it replaces — same binding, same body,
identical code after macroexpansion, no runtime cost. There is no fifth
context: the compiler-side builder (`Pl::CLForm::ctx_bind`) dies on an
unknown one.

**A consumer must accept both spellings.** The bare
`(let ((*wantarray* V)) …)` still appears wherever the context bind rides
along with a second binding — the `sort $var LIST` comparator binds
`*package*` alongside it — and in statement classes still emitted as v1
text. Anything that pattern-matches through a context wrap must peel
either form; the runtime does it with one `%p-strip-ctx` (used by
`%p-fh-arg`'s bareword-filehandle recovery and `p-list-=`'s
undef-placeholder test — a wrap those cannot see past silently changes
behaviour).

`return EXPR` and sub-tail positions inherit the frame's context. `p-sub`
snapshots the value at entry into `*pcl-caller-wantarray*` so nested binds
inside the body don't lie to `(p-wantarray)`, which maps t→`1`, nil→`""`,
:void→`undef`.

**Statement (void) position — the sub-body regime.** A sub body with more
than one statement (or a single compound) is wrapped ONCE in
`(p-void-ctx …)`; every non-tail statement inside then
trusts that ambient and emits no bind of its own.  The tail (implicit
return) statement restores the caller's context at the innermost
expression-statement level: `(p-caller-ctx TAIL-FORM)` — a compound tail
(if/elsif chain) carries the restore on each
branch's leaf value statement, never around the whole compound (its
non-tail inner statements stay in the :void ambient).  Explicit `return`
needs no restore: the `p-return` macro evaluates its values under
`*pcl-caller-wantarray*` itself.  A body that is a single non-compound
statement skips the regime entirely (no binds at all — the tail already
inherits the caller's dynamic context).  `do{}`/`eval{}` blocks and
map/grep/sort bodies are regime *boundaries*: they run in their own
caller's context, so void statements inside them carry per-statement
`(p-void-ctx …)` wraps.  Toplevel (non-sub) statement
position emits per-statement binds only where the form is
context-sensitive (user funcalls, g-modifier matches).

For a translator: this is a hidden argument threaded through every call,
defaulting to "inherit". Context-sensitivity is *observable* — `wantarray`,
list-vs-scalar returns, `=~` in list context returning captures — so it
cannot be erased statically in general.

## 5. Calling convention

### 5.1 Definition

```lisp
(p-sub pl-NAME LAMBDA-LIST body…)
```

registers the function (visible to compile-time `BEGIN` code — the whole
form is inside `eval-when`) and wraps the body so that at **every call**:
1. the caller's package and sub-name are pushed on stacks (for
   `caller()`),
2. `*pcl-current-package*` becomes the sub's home package,
3. `*pcl-caller-wantarray*` snapshots `*wantarray*`,
4. the body runs inside `(catch :p-return …)` — the sub's return frame.

`(p-declare-sub pl-NAME)` is a forward stub so earlier code can reference
the name; it is normally overwritten by the real `p-sub` before anything
calls it.

**A plain call that reaches no body is never a value (normative, s432 +
s441c).** Whether the sub was forward-declared and never defined, or never
mentioned at all, the answer is perl's, in perl's order:

1. if the *sub's own package* defines `AUTOLOAD` — that package's own symbol,
   with a body, **no `@ISA` walk** (inheritance is the METHOD rule, §7) —
   call it, with `$AUTOLOAD` set to the fully-qualified name and the original
   arguments;
2. otherwise die `Undefined subroutine &Pkg::name called` (trappable by
   `eval {}` like any other die; PCL does not append perl's
   `" at FILE line N."` because the emitted call carries no location).

One runtime entry point implements it (`%p-call-of-undefined-sub`) and four
paths reach it: the forward stub's body, the trampoline `p-backslash-sub`
returns for `\&NAME` when the name has no body, a SYMBOLIC call whose name
resolves to no body (`&$name(…)` / `$name->(…)` / `&{"name"}(…)`, in the
package §7.1's rule picks — perl reaches AUTOLOAD through those too, probed
s442d), and — for a name the file
never declared, whose call the codegen emits as a direct `(pl-NAME …)` — an
`undefined-function` handler that resumes through CL's `use-value` restart.
A translator to another host needs the same four, or the equivalent of a
per-package "no such function" hook: the decision belongs at the CALL, which
is where perl makes it, and not at an error boundary, because `eval {}` must
be able to see the AUTOLOAD *value*.

### 5.2 Arguments — two body shapes

**General shape** (body may use `@_`):

```lisp
(p-sub pl-f (&rest %_args)
  (p-args-body            ; binds @_ = (p-flatten-args %_args)
    …body…))
```

**Fast path** (body provably never touches `@_`; emitted for
`my ($a,$b) = @_;` prologues and coalesced leading `my $x = shift;` runs):

```lisp
(p-sub pl-f (&optional ($a (p-undef)) ($b (p-undef)) &rest %_args)
  (declare (ignore %_args) (dynamic-extent %_args))
  …body…)
```

Missing arguments are `undef`; extra arguments are silently ignored (they
sit in the unused rest list). The two shapes are call-compatible — every
call site just applies the function to the flattened values.

**Flattening:** Perl has no argument structure — at every call, array and
hash arguments splice into one flat value list (`p-flatten-args`). A hash
flattens to key, value, key, value…

**Aliasing (perl's defelem, task #131):** `@_` slots hold the caller's
LIVE boxes for most argument shapes — globals/`our`/`$_`, array/hash
spreads (holes as lazy `%p-defelem-box` cells), and named-container
elements: `f($h{k})` / `f($a[i])` emit `p-gethash-argbox` /
`p-aref-argbox` in argument position (all user-call shapes: named,
coderef, method), which return the slot box when the element exists and
a lazy defelem magic cell when it does not — reads see undef and never
vivify; the first write through `$_[N]` creates the key / extends the
array.  **Copying OUT of `@_` is by VALUE**: `my ($x) = @_` / `my $x =
shift` snapshot the getter's value (`%p-flatten-list`, `box-set`), never the
magic cell — a later `$x = 0` must not write through the alias (s411; the
raw-params fast path had hidden the general path's aliasing).  A plain `my` lexical scalar is boxed **when the callee is known
to write through `@_`** (task #189): the callee's body is scanned once,
the fact rides `sub_info` as `writes_args`, and the VarAnnotator's
`arg-to-writer` event boxes that call site's arguments — so
`sub setit { $_[0] = "x" } setit($lex)` writes the caller's variable
while a read-only callee's arguments stay raw slots.  What still COPIES:
a lexical handed to a callee the scan cannot identify (coderef call,
method dispatch, cross-file sub — the runtime's "Cannot modify non-boxed
value" warning is the backstop there) and deref-element args
(`f($ref->{k})`).  See `docs/not-supported.md` §`@_` argument aliasing.

**Element targets of `s///` / `tr///`** are the element's BOX:
`$a[0] =~ s/…/…/` emits `(p-=~ (p-aref-box @a 0) (p-subst …))`, not
`p-aref`.  A plain match is a read and keeps `p-aref`.

**`&`-sigil calls without an argument list** re-use the caller's `@_`
(Perl's `&foo;` rule).  Named form: `&foo;` → `(pl-foo @_)`.  Deref forms:
`&$ref;` / `&{expr};` / `&{"name"};` → `(p-funcall-ref EXPR @_)`;
`p-funcall-ref` accepts a code ref OR a symbolic sub-name string
(no-strict-refs, `'` = `::`).  The closed set of parents that want the
coderef *mention* rather than a call — `\`, `defined`, `exists`, `undef`,
`goto` — lower it themselves (`(p-backslash (p-get-coderef EXPR))`,
`(p-coderef-defined-p EXPR)`, …), so `(p-get-coderef EXPR)` in emitted
code always means "the coderef itself", never a suppressed call.

### 5.3 Return

`(p-return V…)` throws to the nearest `:p-return` catch — normally the
sub frame, but `eval { }` installs its own (§6.3), matching Perl. On the
way out `p-return-value` adjusts the value:
- scalar box → unboxed value (blessed boxes stay boxed to keep the class);
- array value in **scalar** context → its element count;
- `undef`/empty in **list** context → empty list;
- multiple values `return (5,3,1)` → the list in list context, the *last*
  element in scalar context.

A sub body falling off the end returns its last evaluated form (the
`catch`'s value).

### 5.4 Comparator frames — `p-sort-cmp`

A `sort` comparator is the one *block* that gets a return frame of its
own, and since s414 (#281 item 6) it is named:

```lisp
(p-sort-cmp (PAIR…) [(declare …)…] BODY…)
  ≡ (lambda (PAIR…) [(declare …)…] (catch :p-return (block nil BODY…)))
```

**Why the frame exists:** `return` inside a `sort` block exits the
*comparator*, not the enclosing sub, so the comparator installs the same
`:p-return` catch a `p-sub` does (§5.1). `block nil` is the ordinary CL
exit target. Leading `(declare …)` forms stay at the lambda head, where CL
requires them: a comparator inside a block-level `package X;` region binds
the region's *qualified* pair (`X::$a` / `X::$b`, §1), whose `defvar`s have
not yet been evaluated when the lambda is compiled, so without
`(declare (special …))` the parameters would bind lexically and a
comparator reading the global would see nothing.

The body is emitted in scalar context, i.e. the full shape is
`(p-sort-cmp ($a $b) … (p-scalar-ctx BODY…))`. All three comparator
spellings share it: a literal block, `sort NAME LIST` — whose body is
**just the call**, `(p-sort-cmp ($a $b) (p-scalar-ctx (pl-NAME)))`, with the
pair passed as arguments as well when NAME has a `($$)` prototype — and
`sort $var LIST` (resolved at runtime by `p-sort-get-fn`, and the only one
that still wraps the lambda in a `let` capturing `*package*` at creation
time, so a *string* comparator name resolves in the user's package rather
than in `:pcl`).

**`sort NAME LIST` has no comparator-specific undefined-sub handling, and a
translator must not add any** (s442d, task #501). Until then the named form
wrapped its call in an `undefined-function` handler that dispatched to
`AUTOLOAD` for perl bug #30661; it was a third copy of §5.1's rule, its
no-`AUTOLOAD` arm returned `nil` — so `sort nonexistent LIST` quietly
compared everything equal where perl dies — and it could never fire (it
interned `"PL-AUTOLOAD"`, not the symbol `%pcl-cl-sub-name` produces).
§5.1's rule at the CALL covers every case, and matches perl measurably: a
sort comparator name **does** reach the package's `AUTOLOAD` (probed on
5.40.3), and with no `AUTOLOAD` it dies. One divergence remains, and it is
about *when*: perl resolves the sort sub on entry, so `sort nonexistent (7)`
and even `sort nonexistent ()` die although the comparator is never called,
while PCL resolves it at the first comparison and those two spellings
succeed (found and filed s442d; the die itself is right, only its *timing*
is late).

`grep`/`map`/`eval` block bodies are plain `(lambda …)` with **neither**
the catch nor the context bind — `return` inside them must propagate to the
enclosing sub's frame.

## 6. Control flow

### 6.1 Conditionals

`(p-if COND THEN [ELSE])`, `(p-unless COND THEN [ELSE])` — plain
conditionals over `p-true-p` (§3.3). Ternary `?:` is also `p-if`.

### 6.2 Loops and loop control

`p-while`, `p-until`, `p-for` (C-style: `(p-for (INIT) (COND) (STEP)
body…)`), `p-foreach ((VAR LIST) body…)`. All accept trailing keys
`:label NAME` (must be first key) and `:continue (progn …)`.  The foreach
family additionally accepts `:my t` (after `:label`, before the body) —
see "Loop variable: lexical or localized" below.

The compiled shape per iteration: an outer named block (the loop's exit
target), a `tagbody` with a `:next` label (the continue target). Loop
control:

| Perl | unlabeled | labeled |
|---|---|---|
| `last` | exit the innermost loop block | throw to tag `LAST-<label>` |
| `next` | jump to `:next` (runs `:continue` form) | throw to `NEXT-<label>` |
| `redo` | jump to `:redo` (body restart, no cond/step) | throw to `REDO-<label>` |

Labeled control uses dynamic tags (catch/throw), so `last LABEL` works
across function boundaries (Test::More's `skip` relies on this). A bare
block `{ … }` is a loop-that-runs-once: same shape, so `last` inside it
exits the block.

`p-foreach` evaluates its list in list context, flattens it, and binds VAR
to **each element's box** (`ensure-boxed`) — so mutating the loop variable
writes through to the array, matching Perl's foreach aliasing (raw
elements get a fresh box; files where that aliasing would be observable
gate to the v1 pipeline).

**Loop variable: lexical or localized.**  Perl's `foreach $pkgvar (…)`
implicitly *localizes* the package variable — the loop variable is aliased to
each element for the body's dynamic extent, so a sub called from the body sees
the current element, and the old value is restored on exit (including on
`die`).  `foreach my $x (…)` declares a fresh *lexical* instead, which no
called sub can see.  Since the direction-D flip (§2) an ordinary global and a
lexical are spelled the SAME symbol, so the two cases are told apart like this:

- `:my t` present → **lexical binding**, always.  The compiler emits this key
  for `foreach my $x`, whose declaration the macro cannot otherwise see.
- no `:my` key → the macro asks whether VAR names a global cell (a
  `p-defcell` symbol macro) *in its macroexpansion environment*.  Yes →
  save/set/restore over the cell.  No (a plain lexical is in scope, or the
  name is exception-partition, or there is no such global) → lexical binding.

A consumer that lowers this IR must reproduce the same split; treating `:my`
as decoration silently leaks the loop value into every sub the body calls.

**Declarations in a loop/condition HEAD.**  A `my` anywhere in an `if`/`while`
condition, or in ANY of a C-style `for`'s three head sections, scopes to the
whole construct — head *and* body — and to nothing after it.  The IR states
that structurally: the construct is wrapped in a `let` binding a fresh cell per
declared name, and the section itself lowers to the per-iteration assignment
into it (`(let (($k (make-p-box nil))) (p-for () ((p-< (p-my-= $k $i) $j)) …))`).
The C-for INIT counter is the one exception in *spelling* only — it gets its own
`let` one level in, because it may also take an unboxed raw slot.  There is no
"declare it in the enclosing scope" fallback: a head `my` that reached the
package cell would stay defined after the loop and collide with a same-named
global (#297).  Known divergence: the wrap is ONE binding for the whole loop,
where perl gives the declaration a fresh instance per iteration — observable
only by closing over it in the body (`while (my $x = shift @l) { push @c, sub
{ $x } }` yields perl's 1,2,3 vs PCL's last value); tracked as #300.

**Counting-loop range foreach** (s286b): a foreach whose list is a **sole
range** `A..B` lowers to `(p-foreach-range ((VAR A B)) body…)` or its
`-raw` variant instead of materializing the range vector.  Semantics:

- **Endpoints are evaluated exactly once, before the first iteration** —
  this is Perl's own behavior (perl builds the foreach list first;
  assigning to `$w` inside `for my $i ($q..$w)` never changes the trip
  count), verified against perl.  Numeric ranges allocate no list at all;
  a magical *string* range (classified by `%p-range-classify`, the same
  oracle `p-..` uses) falls back to iterating a materialized vector inside
  the same skeleton.
- Same `:label`/`:continue`/`next`/`redo` protocol as `p-foreach`.
- `p-foreach-range-raw` binds VAR as a **raw host value** (no box).  This
  is sound without any overload/dualvar analysis because range elements
  are **fresh plain scalars by construction** (perl numifies/stringifies
  the endpoints when building the range — even an overloaded endpoint is
  coerced once, at construction), every body write to VAR must be
  arith-shaped (VarAnnotator), and any `eval` word in the region vetoes
  the raw variant entirely (`eval-in-region`).  Contrast with the
  *planned* raw-numeric verdict for arbitrary-source variables, which DOES
  need a no-overload scan plus a strict checked write —
  `docs/raw-numeric-verdict.md` §"Scope boundary".
- Known divergence: perl's range elements are read-only (`$i = 9` in the
  body dies in perl); PCL permits the write (next iteration rebinds) —
  same family as the `!0`/`!1` read-only non-emulation in
  `docs/not-supported.md`.

**Bare-block `continue`** (s287): `{ … } continue { … }` places the
lowered continue block **after the tagbody, inside the loop-once block**
(labeled shape: after the NEXT catch, inside the LAST catch).  Effect:
`last` skips the continue, `next` (and normal completion) reaches it,
`redo` re-runs the body without it — Perl's semantics.  The continue
block is its own lexical scope.

### 6.3 Exceptions: `die` / `eval { }` / `$@`

`die` signals a `p-exception` carrying either a string or an arbitrary
Perl value (object exceptions). **PCL does not append " at FILE line N."**
to string messages (documented divergence).

`eval { body }` compiles to `p-eval-block`:
1. installs its own `:p-return` catch (Perl: `return` inside `eval{}`
   exits the eval, not the enclosing sub);
2. runs the body; on success sets `$@` to `""` and yields the body value;
3. on `p-exception` sets `$@` to the payload (string or object) and
   yields `nil`; any other host error is stringified into `$@`.

`$@` is an ordinary global box. `die` with no arguments re-raises `$@`.
`$SIG{__WARN__}` fires on `warn`; `$SIG{__DIE__}` does **not** fire
(documented divergence).

`try BLOCK catch (VAR) BLOCK [finally BLOCK]` (perl 5.34's `use feature
'try'`) compiles to `p-try`, and it is **deliberately not** `p-eval-block`:

```lisp
(p-try (progn TRY…) ($e (progn CATCH…)) (progn FINALLY…))   ; finally optional
```

1. **no `:p-return` catch and no loop-tag catch** — `return`, `last`, `next`,
   `redo` and `goto` inside the try block belong to the ENCLOSING sub or loop,
   which is the sharpest difference from `eval {}`;
2. `$@` is **localized to the construct**: set to `""` around the try body and
   again around the catch body, and restored to its pre-`try` value in an
   `unwind-protect` cleanup — so a `finally` block, and everything after the
   construct, reads the OLD `$@`.  The caught value reaches the program only
   through VAR;
3. the catch body runs whenever an exception was signalled — never on a test of
   `$@`'s truth, so `die 0` and a bool-overloading object are caught;
4. the construct's VALUE is the executed block's last form, evaluated in the
   caller's context (`*wantarray*` is not rebound), and `finally`'s value is
   discarded.  No new frame is pushed, so `caller()` does not see the block;
5. `finally` is an `unwind-protect` cleanup: it runs on every exit path,
   including a `return` out of the try block.

The Perl value of a caught condition (object payload, or message text with
perl's " at … line …" tail) is `%p-caught-perl-value`, shared by `p-eval-block`
and `p-try` — the two places a program can see a caught error.

### 6.4 goto

Four source forms, four fates:

- `goto &sub` (tail call) — supported; re-dispatches with the current `@_`.
- `goto EXPR` **where EXPR evaluates to a code ref** (`goto \&NAME`,
  `goto $coderef`, `goto $h->{cb}`) — Perl treats this as the *same* tail
  call, and so does PCL: it lowers to `(p-goto-computed EXPR)`, a macro over
  `(p-goto-sub (%p-goto-target EXPR))`, so the frame-replacement semantics
  (caller package/`@_` handling) are the ones §6.4's `goto &sub` describes.
  Before s328 this lowered to a runtime no-op that returned undef silently.
- `goto LABEL` to a **standalone label** in the same statement list —
  fully lowered (backward and forward; details below).
- `goto LABEL` to a label PPI glues onto the statement it marks — a **loop**
  (`LBL: while (…) {…}`) or a **bare block** (`LBL: { … }`), the only two
  shapes that swallow their label (s316) — same lowering, with the tagbody
  opening *at* that statement so the jump re-runs it from the top, as in
  Perl.  In value position the run is bracketed in `(setf RET (progn …))`
  and RET read after the tagbody (the §6.2 bare-block tail regime), since a
  `tagbody` yields nil.  The label's own loop-control `block`/`catch` tags
  are a separate namespace — `last LBL` inside is unaffected.
- **Computed** `goto EXPR` where EXPR is a **label name string** — a CL
  tagbody tag is not a first-class value, so this cannot be expressed.
  `%p-goto-target` ANNOUNCES the operand on stderr and execution falls
  through past the goto (announced-not-silent; a die measurably aborts
  whole files — the rule-12 boundary ruling, `fable-answers-s328.md` §1);
  documented divergence in `docs/not-supported.md`.

Context: every goto tail call restores `*wantarray*` to the goto-ing sub's
`*pcl-caller-wantarray*` around the apply (s329) — the target inherits the
ORIGINAL caller's context, exactly as `p-return` restores context for its
argument.  The frame replacement is throw-based, so a goto CHAIN still
nests dynamic bindings: bounded at ~10^5 in practice where perl is
constant-depth (`not-supported.md` §goto depth note).

**Not a gap: `goto` INTO a construct.**  Jumping to a label that sits
*inside* a loop/block body from outside it ("Use of `goto` to jump into a
construct") has been deprecated since perl 5.12 and is a **fatal error as
of perl 5.44**.  PCL does not implement it and must not grow support for
it.  This is a different shape from the labeled-loop case above, where the
label marks the *statement*: perl's own `lib/sigtrap.pm` still uses that
one in blead (`Arg_loop:` + `goto Arg_loop;`), which is what motivated the
s316 lowering.

**The lowering has two regimes, chosen by the label's position relative to
the goto.**  Both start from the same label lowering: a goto-target label
statement (`again:`) turns the *remainder of its enclosing statement list*
into `(tagbody :again <remainder>)`.

**Backward goto — a lexical jump.**  A `goto again` textually *after* the
label sits inside that tagbody and lowers to the lexical `(go :again)`:
the loop-retry idiom is a plain local branch.  A `my` jumped back over
re-binds fresh (its `let` sits inside the tagbody), matching Perl.
*Porter mapping:* any local jump/loop construct — a `while(true)` +
`continue`, a real label if the target has one.

**Forward goto — a dynamic escape (s295, #63).**  A `goto fwd` textually
*before* the label cannot be a lexical jump (the jump target does not
exist yet at that point in the form), and Perl also allows the goto to
fire from *inside a lambda run during the prefix* — e.g. out of a `map`
block — unwinding whatever frames are between.  So the prefix statements
(everything before the label) are wrapped in a **catch** whose tag names
the label, and the goto lowers to a **throw** to that tag:

```lisp
;; goto fwd; print 1; fwd: print 2;
(catch :pcl-goto-fwd
  (throw :pcl-goto-fwd nil)      ; the goto site — may be nested arbitrarily
  (p-print 1))                   ; skipped statements
(tagbody :fwd (p-print 2))       ; the label and everything after it
```

Semantics of the pair: the throw **unwinds all frames up to the enclosing
catch** (including any lambda/sub invocations entered during the prefix)
and control falls out of the catch's end, straight into the tagbody — i.e.
to the label.  The tag is the keyword `:pcl-goto-<label>`, one per label
name.  *Porter mapping:* any exception/non-local-unwind mechanism — wrap
the prefix in `try { … } catch (GotoFwd) {}` and compile the goto as
`raise GotoFwd`; the fall-through after the handler IS the label.  The
throw's value is irrelevant (always `nil`): the wrap is only applied in
statement position, so nothing consumes the catch's value.

Composition rules (what a port must reproduce):

- **Multiple labels**: the wrap applies to the first standalone label of
  the list and the lowering recurses on `[label..end]` — a second forward
  label produces its own catch *inside the first label's tagbody*, so
  sequential forward gotos nest naturally.
- **Backward gotos are unaffected** by the catch machinery: only gotos
  lowered *inside a wrapped prefix* consult the label→tag map and become
  throws; a goto inside the tagbody remainder stays a lexical `(go)`.
- **Scope guard**: the prefix is only catch-wrapped when it contains no
  `my`/`state`/`local` declaration — wrapping would cut the declaration's
  `let`/save scope short (the catch closes before the label).  Such
  goto-before-declaration shapes are not lowered natively (v1 fallback via
  gate), as is a label in **value position** (tagbody yields `nil`).

Perl's own restrictions (may not jump *into* a construct, nor into a
different sub) are assumed, not checked — PCL compiles valid Perl (§design
rule 9).

## 7. Packages, variables, and OO

### 7.1 Namespaces and case

Each Perl package is a host namespace. Because CL uppercases unquoted
symbols, PCL uses the reader's `:invert` convention: single-segment
package names are stored upcased, and the original spelling is registered
via `(p-register-pkg-name :Animal "Animal")` — string-level lookups
(method dispatch, `ref`) go through that registry. Multi-segment names
(`File::Basename`) keep their exact spelling as `|File::Basename|`.
A qualified Perl variable `$Foo::x` is the symbol `$x` in namespace `FOO`.

**Two "current packages", and a translator must not confuse them
(normative, s442d):** the host's own reader package (`*package*` in CL) is
set by the emitted `(in-package …)` forms *as the file is read*, so once
loading is past a file's last one it is `MAIN` for the rest of the run —
it says nothing about which Perl package a running form was written in.
The Perl-level current package is `*pcl-current-package*`: codegen sets it
at every `package` statement, `p-sub` rebinds it per call to the sub's home
package, `p-require`/`p-use`/`p-eval` rebind it around a load, and
`p-eval-thunk` binds it (with `*package*`) to a string eval's `package X;`
region. **Every "unqualified name → current package" rule that runs at
RUN time reads that one** — notably the symbolic sub-name resolver
(`&$name(…)`, `$name->(…)`, `&{"name"}(…)`, `\&$name`, `goto &$name`,
`sort $name LIST`, `defined`/`exists &{…}`, all through
`%p-resolve-sub-symbol`), which is perlmod's "if the string is unqualified
it is looked up in the current package". Reading `*package*` there resolved
`package NA; sub p { my $s = "nafun"; &$s(3) }` in `main` (task #503). The
*compile-time* symbolic paths (a name the emitter itself qualifies) are
unaffected, and a string containing `::` always names its own package.
The residual gap: an **anonymous** sub does not rebind
`*pcl-current-package*`, so a symbolic call inside one that is invoked from
another package resolves in the caller's package, where perl uses the
package the closure was compiled in.

### 7.2 Package variables and `local`

Package vars are globally-registered boxes. **How** the box is registered
depends on the partition (§2b.1): an ordinary variable lives in a global
value cell reached through a symbol macro (`p-defcell`); the exception set
keeps `defvar`. Name-based access — `symbol-value`, `boundp`,
`makunbound`, the glob and symbolic-ref helpers — reaches the same storage
either way, so nothing in the runtime has to know which side a name is on.

`local` saves the current **value** and restores it on scope exit
(dynamic, not lexical): `p-local-cell` for a cell, a dynamic `let` for an
exception name, plus the dedicated `p-local-*` macros for hash/array
elements, typeglobs and the magic cells (`$.`, `$|`, `$!`). Every flavor
restores through a non-local exit as well as a normal one. Restore also
invalidates the box caches.

`foreach $pkgvar (LIST)` is an *implicit* `local` of the loop variable —
the body and everything it calls see the current element, and the old
value is restored on exit, including via `last`/`die`. The loop macros
decide which mechanism to use from the expansion environment (a symbol
macro ⇒ localize the cell; a special ⇒ dynamic `let`; a lexical `my` loop
var ⇒ plain lexical binding, no localization at all).

### 7.3 Method dispatch

- Method names travel as **strings**. `(p-method-call OBJ "name" ARGS…)`:
  flatten args; find the invocant's class (the box `class` field for an
  object, the package-name string for `Class->method`); walk the class's
  **C3 linearization** (backed by CLOS proxy classes `plc-*`, one per
  package, whose superclass edges mirror `@ISA`); the first package
  defining the method wins. Method-name string → function mapping goes
  through the `%pcl-cl-sub-name` registry (case-preserving).
- `SUPER::name` dispatches starting *after* the current sub's home
  package in the linearization. `$obj->$coderef(@a)` calls the code ref
  directly with the invocant prepended.
- `AUTOLOAD` is honored (walks `@ISA`, skips `DESTROY`). `can`/`isa` work.
  `DESTROY` is **never called by GC** (documented divergence).
- PCL always linearizes with C3 (stock Perl defaults to DFS; documented
  divergence — `docs/not-supported.md` §mro).

### 7.4 Scheduled blocks

`BEGIN`/`CHECK`/`INIT`/`END` compile to definitions-stream forms that run
at their Perl-mandated points relative to the *file's* load: `BEGIN`
bodies execute as soon as their form loads (before later statements),
`END` blocks run at program exit in LIFO order. Since s300b (#55) the
scheduled forms sit **at their source position among the `p-sub` defs**
(see §1 file layout), so a `BEGIN` observes exactly the subs defined
above it — `defined &f`/`->can` snapshots at compile time match Perl.
In a **fork child**, `END` blocks inherited from the parent still run at
the child's exit (as in Perl); test-harness plan-checking is pid-guarded
separately and is not an IR concern.

## 8. Magic globals

All are dynamically-scoped boxes exported from the runtime namespace:

| var | semantics |
|---|---|
| `$_` | default topic. **The transpiler materializes it explicitly at parse time** (`add_implicit_default_param` / `_default_filetest_operand` in `Pl/PExpr.pm`, and the print family in `ExprToCL` `gen_funcall`): Perl's omitted-operand forms — `uc;`, `chomp;`, `length;`, bare `-e`, a bare `/re/` match, and bare `print;`/`say;`/`printf;` — arrive in the tree as `(p-uc $_)`, `(p-=~ $_ …)`, `(p-print $_)`, etc. **A translator implements no per-op defaulting** — there is no longer any runtime-side `$_` default. |
| `@_` | current sub's args (lexical per `p-args-body`, §5.2) |
| `$@` | last eval error (§6.3) |
| `$1`…`$N`, `%+` | capture groups; set by the most recent successful match (`p-=~` family); dynamically saved/restored around scopes like Perl |
| `$0`, `@ARGV`, `%ENV` | program name, args, environment (`%ENV` writes through to the process) |
| `$!` | last OS error (dualvar: numifies to errno, stringifies to message) |
| `%SIG` | signal handlers. **Pre-populated at load with every platform signal name, values undef** (`*p-signal-numbers*`, Config's sig_name order; 67 keys on Linux, `ZERO` excluded exactly as perl does), so `exists $SIG{HUP}` is true before any handler is installed — pragmas like `sigtrap` probe it that way. `__WARN__`/`__DIE__` are *not* keys until assigned. The same table resolves `kill`'s name designators. |
| `$.` | line number of the last-read filehandle (per-handle) |
| `$a`, `$b` | sort comparator operands (per-package defvars) |
| `$\`, `$,` | output record / field separator. **Both are UNDEF until the program sets one** (task #465) — the separator defaults are asymmetric and a translator must copy the asymmetry, not normalize it: `$/` is `"\n"`, `$;` is `"\034"`, `$"` is `" "`, `$!` is the errno dualvar, all DEFINED. An empty string here is invisible on the write side and wrong on the read side (`defined($,)`, `$\ // ","`, `length($\)`), which is what made it silent. `print` treats undef as "print nothing between/after": its readers test *non-empty string*, never `defined`. **`say` appends `"\n"` INSTEAD of `$\`, never as well as it** (task #500), while `$,` still separates its arguments; `printf` appends neither. perl does not *localize* `$\` over the call — an overload or tie handler that runs while an argument stringifies still reads the program's value (probed s442d) — so the terminator is passed to the one writer (`%p-write-list`), not bound over it. |

Regex match state is *global-with-dynamic-save*, exactly Perl: a failed
match leaves `$1` from the previous successful match intact.

## 9. The load model and string eval

Generated files are loaded form-by-form; a `use`/`require` triggers
transpilation (or cache lookup) of the target module and loads it inline,
recursively.

**The COMPILE PHASE of the whole file precedes the RUN PHASE of any of it
(normative, s436).** Perl compiles a file before it runs a line of it: every
named sub is defined and every `BEGIN` has run before the first run-time
statement, no matter where in the file they sit. A generated file reproduces
that with two groups of top-level forms, in this order:

1. **compile phase**, one group per package section in source order — the
   section's package preamble and `(in-package …)`, its declarations, a
   `package NAME VERSION` section's `$VERSION` assignment (perl sets it as
   the `package` statement is COMPILED, so it precedes every `BEGIN`, `use`
   and sub of the section — s437), its
   captured `use`/`require`/`BEGIN` declarations, and its sub definitions and
   scheduled blocks interleaved by SOURCE POSITION (so a `BEGIN` sees exactly
   the subs written above it and none below);
2. `(p-run-compile-phase-blocks)` — UNITCHECK and CHECK in reverse
   registration order, then INIT in source order (§the phase boundary);
3. **run phase**, one group per section in source order — an `(in-package …)`
   to put the READER back where that section's symbols resolve, a
   `(p-set-current-package …)` for `caller()`/`__PACKAGE__`, then the
   section's run-time forms.

A section is a top-level `package` switch, so a single-package file has one of
each group and the split is invisible. The rule is load-bearing exactly when a
file has more than one: emitting a whole section at a time — its run phase
included — puts a LATER section's compile phase after an EARLIER section's
run-time code, which perl never does (tasks #456, #469; the two shapes are a
cross-section forward call and a `BEGIN` that could see a run-time
assignment). A translator that emits sections whole reproduces the bug.

Note what may NOT be done instead: hoisting a definition on its own, above its
own section's declarations, compiles that section's `p-defcell` symbol-macro
as a plain free variable. The unit is the whole compile phase.

**Declarations are define-once.** Both declarers initialize a variable only
when it is not already bound — `defvar` by definition, `p-defcell` by an
explicit `boundp` guard. This is load-bearing, not tidiness: the same name
can be declared by more than one section of a file *and* by more than one
file, and a module can be loaded twice; an unconditional initialization
would wipe a value an earlier declaration's code had already assigned. A
translator that lowers these declarations to plain assignment introduces a
silent wrong. `eval "string"` calls the transpiler *at runtime* on the
string (`docs/eval-lexical-capture.md` is the original design note;
§9.1 below is normative). Calling context does not propagate into
string eval (documented divergence). Translators targeting environments
without a runtime compiler must either bundle one or reject `eval EXPR`
programs — there is no static escape.

### 9.1 The string-eval protocol (normative, s295)

String eval must let the eval'd code *read and write* the enclosing
scope's `my` variables by their **source names**, even though the
compiler may have let-bound, renamed, or package-promoted those
variables. Perl's own mechanism is pad lookup at the eval site; PCL
reproduces it with three cooperating pieces. An implementer in another
language needs exactly these three; each is described by its observable
contract.

**Piece 1 — the eval site: `(p-eval STRING ALIST)`.** At every `eval
EXPR` call site (literal or dark/dynamic string — the two are handled
identically), codegen passes an alist literal of the **let-bound**
lexicals in scope:

```lisp
(p-eval $code
  (list (cons "$x" $x__shadow__2) (cons "$x" $x) (cons "$y" $y__lex__0)))
```

Each key is the variable's *original Perl source name* (the string the
eval'd code will use); each value is the live container — a scalar box,
array, or hash — that the compiled code binds under its (possibly
renamed) symbol. Keys may repeat: resolution is first-match, so the
alist is ordered **innermost binding first**. Concretely,
`_eval_lexical_alist` strips the `__lex__N`/`__shadow__N`/`__file__N`/
`__cond__N` rename suffixes to recover the key and, within one key, orders shadow renames
by descending `N` (deeper shadows have higher counters) with the plain
unrenamed name last. After the let-bound pairs it appends the
**cross-package span pairs** (see piece 3). The alist is rebuilt at each
call — it snapshots which bindings are live at that site, not their
values (values live in the shared containers).

**Piece 2 — the eval body: free variables become parameters.** The
runtime transpiles the string in the caller's Perl package (result
cached under `(string . package)`), then reads and evaluates the
generated forms one at a time (so an `(in-package …)` inside the eval
text takes effect before later forms are read). Variables that are
*free* in the eval'd code — used but not declared by it — are compiled
to parameters of a wrapper lambda:

```lisp
(p-eval-thunk '("$x" "$y") (lambda ($x $y) …body…))
```

`p-eval-thunk` resolves each name via `p-eval-lex-lookup` (piece 3) and
applies the lambda to the resulting containers.

An eval whose whole body is one `package X; …` region takes a third
argument — the region's CL package designator:

```lisp
(p-eval-thunk (list "$Z") (lambda ($Z) …body…) :|Foo::Bar|)
```

With it, `p-eval-thunk` binds `*package*` to X's CL package (find-or-
create, `:use '(:cl :pcl)`) around **both** the free-name resolution and
the body. Perl says the current package inside such a region *is* X, and
every unqualified-name resolution in the runtime asks `*package*` that
question: `p-eval-lex-lookup`'s stop 2/3 below, `%p-symref-box` and its
array/hash siblings, `p-use`'s default import target, `p-bless`'s empty
class, the symbolic funcall/coderef resolvers. Without the argument
(any eval that is not a single region) `*package*` stays the caller's,
which is equally what Perl says. The thunk is emitted whenever a region
package is present, even with an empty free-name list — the binding's
main effect is on the body. The eval TEXT was read in the caller's
package before the thunk runs, so the lambda's own symbols are
unaffected by the binding.

Because the containers
are bound as ordinary lexical parameters, everything inside the body —
including closures and **named subs the eval defines**, which outlive
the eval — captures the *containers themselves*. Writes (`$x = 84`
box-sets the scalar box) are visible to the enclosing compiled code and
vice versa, with no copy-back step.

**The pad-chain continuation (`%p-eval-env%`, s383/#295).** Perl
compiles eval'd text *in the pad chain of the eval site*: an eval site
**inside** the eval'd text sees the text's own lexicals and then the
enclosing scope at the outer site — and a named sub the eval defines
keeps that chain when called after the eval has returned. The site
alist reifies the outer link, but `*p-eval-lex-alist*`'s dynamic
binding dies with the eval, so the compiled body lexicalizes it: when
any eval site occurs in the compiled text, the body (inside the thunk
lambda when one is emitted, as the single body form otherwise) is
wrapped in

```lisp
(let ((%p-eval-env% pcl:*p-eval-lex-alist*)) …body…)
```

and every eval site *emitted in eval mode* appends `%p-eval-env%` to
its site alist — `(p-eval $code %p-eval-env%)`, or
`(p-eval $code (append (list own-pairs…) %p-eval-env%))` when the site
has let-bound pairs of its own (own pairs first: they are inner scope).
Named subs close over `%p-eval-env%` like any lexical, which is what
makes the chain survive the eval's extent; nested evals thread it (each
nesting level's `%p-eval-env%` binds to the alist its own `p-eval`
received, which already carries every outer level). This is **not** a
fourth resolution stop — it rides the site alist, stop 1 below.
Implementers must not substitute an ambient/dynamic hand-off: a sub
merely *called* from eval'd code must not see the eval's lexicals, and
only lexical capture makes that distinction.

**Piece 3 — name resolution: `(p-eval-lex-lookup NAME)`.** Exactly
three stops, in order:

1. **The site alist** (`*p-eval-lex-alist*`, dynamically bound by
   `p-eval` for the extent of the eval): first `string=` match wins.
   This is how let-bound lexicals — including seam shadows — are found,
   and why alist order encodes shadowing depth.
2. **The named global of the current package**: intern NAME (through the
   same case transform the reader applies to generated code) in the
   package current at the eval site; if that symbol is bound, its value
   is the container. This is how file-scope lexicals are found — see
   the alias rule below.
3. **Autovivify**: an unbound name yields a fresh container chosen by
   sigil (`$` → undef box, `@` → empty array, `%` → empty hash),
   matching Perl's global autovivification inside eval. The fresh
   container is **installed** as the symbol's global value (s304), so a
   later eval of the same name finds it at stop 2 — a package global
   used *only* inside eval strings persists across evals, exactly like
   Perl's autovivified global. (Before s304 this accidentally worked
   via a phantom `defvar` the enclosing file emitted from scanning the
   eval string literal's innards; that scan bug is fixed, so the
   install is now load-bearing.)

**The alias rule (v2 renamed cells).** v1 `defvar`s every file-scope
lexical under its *original* name, so stop 2 finds it for free. v2
renames such cells (`$x__file__N`, §2b.3) precisely so they cannot
poison unrelated `let`s — which would make them invisible to stop 2.
The fix is one runtime primitive:

```lisp
(p-alias-eval-cell '$x $x__file__0)   ; (setf (symbol-value '$x) cell)
```

emitted by codegen **at the renamed declaration's run position,
immediately after its initializing assignment** (after, so the decl's
own RHS — including an eval in it — still resolves the name to the
*outer* variable). It stores the cell's container as the value of the
original-name symbol in the declaring section's package (the quoted
symbol is unqualified: the reader interns it under the section's
`in-package`, which is by construction the declaring package). Both
rename families that produce file cells flow through it: span promotion
(W10) and capture promotion (M-C/M-F).

Consequences an implementer must preserve:

- **One storage location per name.** The alias target is the *same*
  global slot a plain un-renamed file lexical or package global uses.
  There is deliberately no side registry: two storage locations with a
  fixed precedence would let a stale entry in one permanently shadow a
  live binding in the other (the s294 registry bug). Any later
  declaration of the same name — renamed (re-alias) or plain (defvar +
  assign on the same symbol) — takes the slot over from the moment it
  *executes*, giving v1's time-ordered last-declaration-wins model.
- **Emission is gated per file** (`_file_has_str_eval`): the alias call
  is emitted only when the file contains at least one non-block `eval`.
  A file without string eval is **byte-identical** to before and pays
  zero runtime cost; the gate may over-fire on non-eval uses of the
  word (`->eval`, `eval =>` …), which adds only an inert one-write
  call. Dark strings cost nothing beyond literal ones — the mechanism
  never inspects the eval'd text at compile time.
- **Nested/late evals work with an empty alist.** A sub defined inside
  an eval string, called later, whose body does `eval '$x'`: the site
  alist is empty by then, and stop 2 finds the aliased cell — the same
  path v1 takes. Nothing from any site alist is ever persisted.
- **Cross-package spans still need site pairs.** Stop 2 interns in the
  *eval site's* package, so a cell aliased in the declaring package is
  invisible to an eval in a later `package` segment. For those, the
  span pass records original-name → package-qualified-cell pairs per
  extent segment, and piece 1 appends them after the let-bound pairs
  (`(cons "$x" MAIN::$x__file__0)`). They are position-static and carry
  no lifetime hazard.

**Deliberate divergences** (all shared with v1, listed in
`docs/not-supported.md` where user-visible): after the alias executes,
an explicit fully-qualified `$Pkg::x` names the lexical's cell rather
than a distinct package global — exactly v1's defvar-under-original-name
behaviour. Calling context (`wantarray`) does not propagate into the
eval. Only scalar cells are aliased today; a file needing
container-capture-by-eval, or a renamed cell nested inside an outer
`my` of the same name (whose alist pair would always win stop 1), gates
to v1 (§2b.4).

### 9.2 The generation stamp is a promise (normative, s402)

**Line 1 of every file PCL emits is `;;; pcl: pipeline=v2 gen=<generation>`,
where `<generation>` is the value of `cl/pcl-runtime.lisp`'s
`*pcl-cache-generation*` at the moment of emission.** It is a comment, so it
carries no semantics for a translator — but it is a *stable interface*, and
tooling is entitled to key on it. Two consequences, both already relied on:

- **Staleness is checkable without re-running the compiler.** The three
  transpiled artifacts checked into the tree (`cl/pcl-pack.lisp`,
  `cl/pcl-mro.lisp`, `cl/pcl-warnings.lisp`) are just emitted files, so their
  line-1 stamp says which compiler built them. `Pl/t/artifact-staleness-01.t`
  compares each stamp against the live generation, which is why an
  emission-changing commit that forgets `tools/rebuild-pack` fails a row the
  same session instead of drifting for forty generations (#331). The
  artifacts are *discovered by the stamp*, not by a list — anything under
  `cl/` whose first line carries it is an artifact and is checked.
- **The module cache keys on it.** A cached module transpile is only reused
  when its generation matches, so bumping the generation string is what makes
  an emission change take effect for already-cached modules.

Two obligations follow, and both are gated: an emission-changing commit
**bumps** the generation (or stale cache entries are silently reused), and it
**regenerates the artifacts** in the same commit. The stamp's *format* is
part of the promise: a single line, first line of the file, matching
`^;;; pcl: pipeline=\S+ gen=(\S+)`. `Pl/t/no-hardcoded-paths-01.t` cites it
too — it is the marker that tells a generated artifact apart from a
hand-written source file, which is the difference between "this path is a
bug" and "this path is a build-machine artifact of the emitter".

### 9.3 The drop form: a statement the compiler could not lower (normative, s435)

Where a statement of the source program could not be lowered, the emitter puts
this in its place:

```lisp
(progn ;; PARSE ERROR: <reason>
  (p-die "PCL: statement not supported at FILE line N: <source text> -- <reason>
"))
```

**What it means at run time.** Reaching the form signals a Perl `die` carrying
that message. Every statement before it has already run; every statement after
it *in the same top-level form* does not. It is trappable exactly like any
other `die` — `eval { … }; $@` sees it — because it is one. FILE and N name the
**Perl** source, not the generated file; no `(:loc …)` marker is emitted, and
the message ends in a newline, so no ` at FILE line N.` suffix is appended
(§the `p-die` contract, and perl's own rule for newline-terminated messages).

**The unit of the decision is the statement; the unit of the LOSS is the
enclosing top-level form.** The compiler decides per statement, but a signalled
condition unwinds to whatever handler the loader established, and that is one
top-level form. A file-level `my` compiles to a `let` around the whole
remainder of the file, so a drop under it costs every row after it in that
form. This is not a regression against Perl: an untrapped run-time `die` at
file scope costs perl the whole remaining program, so a translator that loads
form-by-form with per-form recovery (as PCL's measurement runners do) loses
strictly less. A translator that loads a generated file as ONE unit will lose
the remainder, exactly as perl does.

**What a translator must do: reproduce the die.** Do not lower it to a no-op.
A silently discarded statement is the #138 family — the worst failure mode in
this codebase — and this form exists to end it: the statement takes a test row
or a side effect with it while every count still looks healthy.

**One shape, every drop, every mode.** There is no classifier in the emitter
that makes some drops die and others continue. The distinction between "a gap
PCL intends to close" and "a feature PCL deliberately refuses" lives in
`docs/parse-error-drop-census-s399.tsv` and `docs/not-supported.md`, never in
the emission — a classifier here would be asymmetric in the dangerous
direction, since one miss would kill a whole file. Module mode is covered by
construction: the die is *in* the emission, so it survives the module cache
(the transpile-time stderr announcement is suppressed there, ruled s403 —
the statement now says so itself, when it is reached).  A module whose drop sits
in a sub BODY loads; its load-time code is a run phase like any other, so a
load-time CALL into that sub dies at `require` time — trappable by the `use`'s
own `eval`, as perl's compile error would be (measured s436: Text::Balanced's
`gen_delimited_pat`, called from the module's top level, took all 958 board
rows until #457 closed the drop; ruled s437).

**The comment is load-bearing to tooling, not to semantics.** PCL's census,
`tools/corpus-diff.pl`'s SILENT-DROP counter and both measurement runners'
`drops` column all find drops by the exact text `;; PARSE ERROR:`. A
translator may discard the comment; PCL's emitter may not reword it.

## 10. Op inventory — family rules

The full inventory is the export list of the runtime namespace (~500
symbols; `grep '#:p-' cl/pcl-runtime.lisp`). You do not need each one
individually: every op follows its family's rule, and each `p-*`
function's docstring states its Perl contract. The families:

| family | members (representative) | rule |
|---|---|---|
| numeric ops | `p-+ p-- p-* p-/ p-% p-** p-<< p->> p-& p-\| p-^` | numify operands (§3.1), return raw number; overload hook first; `/` yields a double when inexact; `%` follows Perl sign rules; bitwise ops truncate to integer (Inf→0) |
| numeric compare | `p-== p-!= p-< p-> p-<= p->= p-<=>` | numify; return `1`/`""` (`<=>` −1/0/1; NaN comparisons → `""`/undef) |
| string ops | `p-. p-x p-lc p-uc p-lcfirst p-ucfirst p-length p-substr p-index p-reverse p-sprintf p-join` | stringify operands (§3.2), return raw string; Perl's `$_`-default forms arrive with `$_` already explicit in the tree (§8) |
| string compare | `p-eq p-ne p-lt p-gt p-le p-ge p-cmp` | stringify; return `1`/`""` |
| logical | `p-&& p-\|\| p-// p-! p-not` | short-circuit macros returning operand values (§3.4) |
| assignment | `p-my-=` (boxed lexical) `p-scalar-=` (package) `setf` (raw slot) `p-array-= p-hash-= p-list-=` | store per §2.2; list-assign in scalar context yields the RHS element count; all return the assigned target/value per Perl |
| compound assignment | `p-incf p-decf p-*= p-/= p-%= p-**= p-.= p-str-x= p-bit-and= p-bit-or= p-bit-xor= p-<<= p->>= p-str-bit-and= p-str-bit-or= p-str-bit-xor=` (any place) · `-raw` twins of each (raw slot, §2.2) · `p-and-assign p-or-assign p-//=` (no raw twin) | read-modify-write; boxed macros store back via box-set/setf per place shape, `-raw` twins are `(setf slot NEW)` with the identical NEW form; `&&=`/`||=`/`//=` short-circuit and store the RHS unchanged |
| increment | `p-++ p---- p-++-post p----post` · on raw slots `p-incf-raw`/`p-decf-raw` (statement-root only; tail postfix wraps in `prog1` for the old value) | numeric ±1 on the box/slot; `p-++` on a pure-alpha string does Perl string increment (`"az"→"ba"`) — a raw slot only takes root incdec when every write is numeric-valued (A-num), so the raw twins are never asked to do the magical form |
| elements | `p-aref p-gethash` (read) `(setf p-aref/p-gethash)` / `p-setf` (write) `p-exists p-delete p-aslice p-hslice` | reads unbox scalars, keep reference boxes (§2.3–2.4); writes through `p-setf` autovivify intermediate refs; `p-delete` returns the removed value |
| slice delete | `p-delete-hash-slice p-delete-array-slice p-delete-kv-hash-slice p-delete-kv-array-slice` | every one flattens its key/index arguments alike (`%p-flatten-slice-args`: a range or interpolated list contributes its elements, a STRING is one key — task #394), and every one answers **nil for an EMPTY slice** — undef in scalar context, the empty list in list context, per [perl #29127].  The emptiness test comes BEFORE the read-only check: perl allows `delete @ro[()]` on a read-only array and dies only on a real index (probed, s414) |
| array/hash builtins | `p-push p-pop p-shift p-unshift p-splice p-keys p-values p-each p-sort p-map p-grep p-wantarray p-scalar p-defined` | Perl signatures; `p-sort` default is string order, comparator lambda gets `$a`/`$b`; `p-defined` returns `1`/`""` |
| regex | `p-=~ p-!~` with `(p-regex "/pat/flags") (p-subst …) (p-tr …)` | match/substitute/transliterate against a box (writes back for s///, tr///); sets §8 match state; list context returns captures; `p-split` |
| compiled regex (qr) | `(pcl::p-qr "qr/pat/flags")` literal · `(pcl::p-regex-from-parts PAT "flags")` interpolated | A **Regexp object**, not a string: it carries its own flags and identity. It stringifies as perl's `(?^flags:SOURCE)` wrapper — from the SOURCE text as written, never from any backend-rewritten form, and `/xx` prints both x's (a one-x wrapper silently demotes an interpolated pattern to `/x`). Two rules a translator must implement, both about the wrapper (s322, task #181): **(1)** a pattern that is exactly ONE interpolated qr *is* that qr — `qr/$re/` and `/$re/` keep `$re`'s own flags and **ignore the outer modifiers** (`qr/$re/i` on `qr/abc/` does not match `"ABC"`), so the check must happen where the operand is still the object; **(2)** a qr used as PART of a larger pattern embeds its wrapper verbatim (`qr/x$re/` → `(?^:x(?^:abcdef))`), which is what keeps the inner flags scoped. Consequently a variable holding a qr must NOT be frozen to its string form by any raw-slot/unboxing optimization (`write-object` in `Pl/VarAnnotator.pm`): the stringification is lossy and is re-parsed by the next regex that interpolates it |
| I/O | `p-print p-say p-printf` (`:fh HANDLE` key) `p-open p-close p-readline p-eof p-binmode …` | Perl builtins; bareword handles are symbols; `p-open` boxes its handle argument. 2-arg `p-open` parses pipe/dup modes (s301, #70): `"|-"`/`"-|"` **fork** (returns child pid to the parent / `0` in the child, whose STDIN/STDOUT is rewired to the pipe; with command text the child execs it — `"| cmd"`/`"cmd |"` are the classic spellings); `p-close` on a pipe handle **reaps the child, sets `$?`**, and is true iff exit 0. Dup modes: `">&FH"`/`"<&FH"` dup the fd (fresh descriptor; onto the well-known fd for STD handles), `">&=FH"`/`">&=N"` are fdopen-style — same fd or stream alias, no dup |
| introspection | `p-ref p-bless p-caller p-can p-isa` | §7; `p-caller` returns package but file/line are stubs (divergence) |
| context & frames | `p-list-ctx p-scalar-ctx p-void-ctx p-caller-ctx` (§4) · `p-sort-cmp` (§5.4) | **names, not operations**: each expands to exactly the `let`/`lambda` shape it replaced, so a translator implements the expansion and nothing else. They mark where the IR says "this runs in context C" / "this is a comparator frame" |

Anything not covered: read the `p-NAME` docstring in
`cl/pcl-runtime.lisp` — by project rule the runtime implements *real Perl
semantics only*, so the function *is* the spec, and
`docs/not-supported.md` is the closed list of deliberate divergences.

## 11. What a translator may ignore

- Comments; `(declare …)`; `(dynamic-extent …)`.
- The box nv/sv caches (pure optimization).
- The preamble's `@INC`/path setup (environment bootstrap, not program).
- `eval-when` wrappers *if* the target has no phase split — execute in
  order; `BEGIN` ordering is preserved by form order alone.
- The per-section `$a`/`$b` defvars and duplicate `defvar`s (idempotent).
- `p-double-inf` — the single SBCL-specific symbol, meaning IEEE ±Inf.

## 12. Worked example

Perl:

```perl
sub greet { my ($name) = @_; return "hi " . $name; }
my @who = ("ann", "bob");
foreach my $w (@who) { print greet($w), "\n"; }
```

Generated CL (abridged):

```lisp
(p-sub pl-greet (&optional ($name (p-undef)) &rest %_args)
  (declare (ignore %_args) (dynamic-extent %_args))
  (block nil (p-return (p-. "hi " $name))))

(let ((@who (make-array 0 :adjustable t :fill-pointer 0)))
  (p-array-= @who (vector "ann" "bob"))
  (p-foreach ($w @who) :my t
    (p-print (pl-greet $w) "
")))
```

The call carries **no** context bind: `greet`'s body never observes
`wantarray`, so the `insensitive-call` emission rule (`Pl/Passes.pm`) drops
it. The general shape — what you see whenever the callee might look — is
`(p-list-ctx (pl-greet $w))`, §4.

Faithful JavaScript-flavored translation, applying this manual:

```js
// §5.2 fast path: params pre-bound, missing → undef; §5.3 catch frame
function pl_greet(ctx, $name = UNDEF) {          // ctx = *wantarray* (§4)
  try { throw new PReturn(p_concat("hi ", $name)); }   // p-. → §3.2 stringify
  catch (e) { if (e instanceof PReturn) return e.value; throw e; }
}
let who = new PArray(["ann", "bob"]);            // §2.3
for (const w of who.elementBoxes()) {            // §6.2 foreach binds boxes
  p_print(pl_greet(LIST_CTX, w.get()), "\n");    // call site binds context
}
```

(A real translator would elide the `PReturn` frame when the body has one
tail return — the same optimization PCL itself applies in reverse.)
