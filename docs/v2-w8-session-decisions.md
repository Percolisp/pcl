# V2 W8 — Session Decision Log (boxing & lowering decisions)

**Purpose:** a running log, for later review, of every decision taken this
session about *when a scalar stays boxed vs. becomes a raw slot*, and other
lowering/scoping choices in the v2 pipeline. Requested by the user so the
accumulating text-scan heuristics can be audited (and eventually replaced by the
W12 OpcodeTree-walk annotator, which should subsume all of them).

Context: W8 = "make the `Pl/t/` gate pass under `PCL_V2=1`". These files are
v2-native (unlike most `perl-tests/*.t`, which gate to v1), so they expose
native-lowering gaps. The safe direction for any boxing decision is **over-fire
= keep the box**: a false "must box" only costs a little speed; a false "may
unbox" produces silently wrong output.

---

## D1 — `p-my-=` now returns the PLACE (box), like `p-scalar-=`

**File:** `cl/pcl-runtime.lisp` (`defmacro p-my-=`).
**Was:** `(box-set ,place ,value)` → returned the *value*.
**Now:** `(progn (box-set ,place ,value) ,place)` → returns the *box*.

**Why:** an assignment used as an lvalue (`++($x = 5)`, `($x = 5)++`,
`($x = e) .= "s"`) needs the assignment expression to yield the mutable box so
the outer operator can modify it. `p-scalar-=` already returns `,place` for
exactly this reason; `p-my-=` (the v2 rewrite of `p-scalar-=` for let-bound
lexicals) diverged and returned the value, so lvalue-assignments silently
no-op'd on a discarded value.

**Safety:** `box-set` unboxes a box *value* argument (pcl-runtime.lisp ~L948), so
returning the box is safe when the assignment is chained as a value
(`my $y = ($x = 5)` → outer `box-set` copies the inner value out of the box).
This mirrors `p-scalar-=` exactly.

**Boxing implication:** none by itself — but it only *works* if `$x` is actually
a box at the lvalue site. See D2.

## D2 — VarAnnotator disqualifier: `($x = …)` parenthesized lvalue-assignment → keep boxed

**File:** `Pl/VarAnnotator.pm` step-3 per-name gates.
**Added regex:** `$text !~ /\(\s*$bare\s*=[^=~]/`  # `($x = …)`

**Why:** the existing gates catch `$x++` / `++$x` / `$x +=` / top-level
`$x = RHS;`, but NOT an assignment *embedded* in an expression and used as an
lvalue, e.g. `++($x = 5)`. Without this, `$x` had no detected write → it was
unboxed to a raw slot `(p-undef)` → `p-my-=`/`p-pre++` (box ops) no-op'd on the
non-box. This is §3 class-5 ("VarAnnotator must see every WRITE shape").

**Scope / false-positive guard:** the regex anchors `(` immediately before the
name, so C-style `for (my $i = 0; …)` counters (which have `my ` between `(` and
`$i`) still unbox — the intloop perf path is preserved. `=[^=~]` excludes `==`
and `=~` (the latter already gated separately).

**MAINTENANCE FLAG (user-raised):** this is *another* text-scan special case.
Each new lvalue shape (slice writes, `substr(...)=`, tied targets, …) would need
another regex. Logged in the VarAnnotator header's disqualifier list. The
principled fix is **W12** — walk the OpcodeTree and collect a structural
"is this scalar ever a write/lvalue target?" fact once, subsuming every regex.
Do NOT keep inventing cleverer combined regexes (that path produced the
`_preprocess_source`-hits-string-literals class of bugs).

## D3 — `_lower_scope` pushes/pops an Environment scope frame (lexical pragma scoping)

**File:** `Pl/Parser2.pm` (`sub _lower_scope`).
**Added:** `$self->environment->push_scope` before lowering the block body,
`pop_scope` after.

**Why:** lexical pragmas (`use integer` / `no integer`) are stored on the
Environment's scope-frame stack; v1 relies on `push_scope`/`pop_scope` around
every block to save+restore them. v2's `_lower_scope` scoped its own
`_live_lex`/`_let_bound_vars` but never touched the Environment stack, so a
nested `{ no integer; … }` permanently cleared the pragma for the enclosing
`use integer` scope (transpile-test-01 "no integer restores float division":
third `7/2` gave `3.5` instead of `3`). Division goes through the expression
fallback, which reads `has_pragma('use_integer')` at codegen time.

**Boxing implication:** none directly. It is a *pragma/scope* fix, not a box
decision, but it lives in the same "v2 didn't mirror v1's per-block state" family
as the class-4 environment-mirroring bugs (§3 of the completion plan).

**Breadth note:** this affects EVERY v2 block (if/loop/bare). Guards
(parser2-01/02) + transpile-test-01 stay green; full-gate re-run pending before
commit.

---

## D4 — `my` in an if/while/unless/until CONDITION → wrap construct in a fresh boxed let

**File:** `Pl/Parser2.pm` (`_cond_my_names`, `_wrap_cond_mys`, if/unless & while/until branches).

**Why:** Perl scopes a `my` declared in a condition head (`if (my $x = 5) {…}`,
`while (my $i = …)`, chained `my $x = my $y`, list `my ($p,$q)=…`) to the WHOLE
construct (cond + branches/body), NOT the enclosing block. v2 previously lowered
the condition as a plain expression, so `if (my $x = 5)` assigned to the *outer*
`$x` box and leaked `5` past the if. Now condition-`my` names are registered
let-bound around cond+body lowering, and the construct is wrapped in
`(let ((name (make-p-box nil)) …) …)`.

**Boxing decision:** condition-`my` scalars are **always boxed** (`(make-p-box
nil)`), no unboxing attempt — they are lvalue targets of the condition
assignment and usually read in the body. Deliberately NOT an optimization site.

**Shadowing without renaming:** v2 uses real lexical `let`s, so a nested `let`
of the same CL symbol shadows correctly — no `$x__lex__N` renaming (unlike v1,
which promotes the outer to a package `defvar`).

**Gate inside:** an array/hash `my` in a condition dies → v1 (needs a container
init); a `my` nested in a block/anon-sub in the condition is not hoisted.

## D5 — Multiple `my` decls in a C-for init → bind all counters, boxed

**File:** `Pl/Parser2.pm` (C-style-for branch).

**Why:** `for (my $i = 0, my $j = 10; …)` is the comma operator
`(my $i = 0), (my $j = 10)`; `_single_scalar_decl` misparsed the comma-list as
`$i`'s RHS (→ `$i` = 10, loop never ran). Now `_cond_my_names($init_s)` finds all
counters; when ≥2, all are bound in the wrapping `let` (**boxed**) and the init
lowers as one `progn` of assignments (v1's shape). Single-counter path keeps its
`$i++`-step unboxing optimization.

**Boxing decision:** multi-counter C-for → all counters **boxed**; the
single-counter unboxing fast path is intentionally not extended (rare; safety
over a niche perf win).

## D6 — `CORE::my`/`CORE::state`/… declarator prefix → gate → v1 (not a box decision)

**File:** `Pl/Parser2.pm` (early gate in `parse()`).

**Why:** v1's PExpr recursively normalizes `CORE::<decl>` → `<decl>` and reaches
tokens nested inside parens; v2's native expression seam hands PExpr a peeled
sub-token-list, so `my $r = (CORE::state $y = 7)` never normalizes and mis-lowers
to a `p-UNDEFINED` funcall. Plain `state`/`my` in the same position work.

**Decision:** gate → v1, NOT fix. The fix (make the shared PExpr CORE:: pre-pass
recurse) adds a per-structure `->content` stringify to the HOT expression parser
for a torture-test-only construct — a bad speed trade (CLAUDE.md §2). Gating has
zero hot-path cost. Not a boxing decision; logged for completeness.

## D7 — restore shared PPI tokens after `_expr_scalar_rooted`'s pre-parse (fat-comma corruption)

**File:** `Pl/Parser2.pm` (`_expr_scalar_rooted`).
**Not a boxing decision** — a destructive-shared-state bug, but high-impact so logged.

**Symptom:** `bless { key => shift }, $class` (every OO constructor) lowered the
hash KEY as a funcall (`(p-hash (Item::pl-val) …)` / `(p-hash (p-length $_) …)`)
instead of the auto-quoted string `"key"`, whenever `key` is also a known
sub/builtin name. Objects then had the wrong key → methods returned undef → `0`.

**Root cause:** `_sub_ctx_insensitive` (sub pre-registration) calls
`_expr_scalar_rooted` on the sub body's last statement, which runs
`parse_expr_to_tree` → `cleanup_for_parsing`. That pass DESTRUCTIVELY rewrites the
`=>` operator to `,` (`set_content`) on the SHARED PPI tokens as part of
fat-comma auto-quoting — and never restored them. So by the time `_lower_expr`
lowered the statement, the `=>` was already `,`, the fat-comma key auto-quote
never fired, and the fallback resolved the bareword as a function call.

**Fix:** snapshot every leaf token's content before the pre-parse and restore it
after (mirrors the exact protection `_lower_expr` already had around its own
native attempt). `bless { key => shift }` now emits `(p-hash "key" …)`.

**Lesson for future v2 work:** ANY code path that hands shared PPI tokens to
`parse_expr_to_tree`/`cleanup_for_parsing` for *analysis* (not final lowering)
must snapshot+restore, because cleanup mutates `=>`→`,` in place. Grep for
`parse_expr_to_tree` callers when adding analysis passes.

## D8 — fallback context mapping: `'inherit'`→INHERIT_CTX, `':void'`→VOID_CTX

**File:** `Pl/Parser2.pm` (`_lower_expr` fallback ctx map).
**Not a boxing decision.**

**Why:** `_lower_expr`'s fallback previously mapped every non-list ctx to
SCALAR_CTX (0). A `return EXPR` passes `'inherit'`; when EXPR falls back (e.g.
`return 1..4` — `..` isn't native), the collapse to scalar made `..` always emit
flip-flop. Perl: `return 1..4` is a RANGE in list context (`my ($a,$b)=f()` →
`1,2`), flip-flop in scalar. Now `'inherit'`→3 (INHERIT_CTX) so the operator
emits its runtime `(if (eq *wantarray* t) (p-.. 1 4) (p-flipflop…))` check;
`':void'`→2. (The flip-flop's compile-time state ID differs from v1's — that's a
per-flip-flop counter, semantically irrelevant.)

## D9 — bare `$#` magic (`$#[0]` on `@#`) → gate → v1

**File:** `Pl/Parser2.pm` (early gate).  Torture-test-only; v2 mis-parses it as
element access and never forward-declares `@#` → unbound crash. `$#array` is a
distinct ArrayIndex token, unaffected. Not a boxing decision.

## D10 — `return if COND` / `return unless COND` leading-modifier fix

**File:** `Pl/Parser2.pm` (`_lower_stmt` return branch).
**Not a boxing decision, but an important correctness bug.**

**Why:** `_split_modifier` scans from index 1 (so a leading compound keyword
isn't misread). The return path `shift`s `return` first, moving the modifier to
index 0, where the scan missed it — `return if $$r >= $n` mis-lowered to
`(p-return (p-if COND))` (malformed p-if, compile error; broke every guard-clause
recursion). Now the return branch detects a leading modifier explicitly (empty
return value). `return if` is extremely common, so this mattered broadly.

## D11 — list-assignment target must stay BOXED (broadened the existing detector)

**File:** `Pl/VarAnnotator.pm` — broadened the list-assign disqualifier from
`\([^()]*$bare…` to `\([^=]*$bare\b[^=]*\)\s*=[^=]` (now crosses nested parens).

**The user asked: why must a list-assignment target be boxed? It looks weird.**
Here is the reasoning, because it is the crux of the whole box/raw-slot model:

- A scalar may become a **raw slot** (a bare CL number/string in a `let`, no
  box) ONLY when *every* write to it is a simple `$x = <arithmetic>` statement
  that the codegen rewrites to `(setf $x <value>)`. That is the *only* write form
  that can target a raw `let` binding directly.
- A **list assignment** `($a, $b) = LIST` does NOT write via `setf`. It calls the
  runtime `p-list-=`, which walks the LHS vector and writes each value into the
  corresponding target **through box-set** — i.e. it expects each target to be a
  **box** (a mutable cell it can store into and that aliases the variable). A raw
  slot is just a value in a `let`; `p-list-=` has nothing to write *through*, so
  the assignment silently vanishes (the symptom: `(($a)x3,$b)=1..10` left `$a`,
  `$b` empty).
- So "list-assignment target ⇒ boxed" is not an arbitrary rule — it falls out of
  the runtime calling convention: `p-list-=` / `p-list-x` operate on boxes, full
  stop. Same reason `$x++`, `\$x`, `local $x`, `$x =~`, and `pos($x)` force a box
  (each needs a mutable cell, not a value).

**Why broaden the regex instead of adding a new disqualifier:** the write shape
was ALREADY meant to be caught ("`($x,…) = list-assign`"); the old `[^()]*`
simply couldn't reach a name behind a nested paren (`(($a)x3, $b) =`). Broadening
to `[^=]*` (anything up to the assignment's `=`) is the *same* rule, made
correct — not a new special case. Verified it does NOT disqualify the C-for
counter (`for (my $i = 0; …)`): the `=` inside `$i = 0` stops `[^=]*` before it
can span to a `)`, so the intloop unboxing fast path is preserved (checked: `$i`
still lowers to `(let (($i 0)) …)`, run gives the right answer).

**Still a text-scan heuristic** → W12 (OpcodeTree walk) is the real fix; a
structural "is this name an lvalue target of a list-assign?" fact subsumes it.

## D12 — `my $x = INIT if COND` splits the modifier; conditional-init stays BOXED

**File:** `Pl/Parser2.pm` (scalar variable-decl branch of `_lower_block`).

**Why:** v2 absorbed the postfix `if COND` as an ARGUMENT to the init
(`my $c = shift if @_>1` → `(p-my-= $c (p-shift (p-if …)))`, a malformed p-if
that crashed). Perl: the `my` declares ALWAYS, the ASSIGNMENT is conditional
(undef when false, re-bound fresh each call). Now the modifier is split off the
init and lowered as `(p-if COND (p-my-= $c INIT))`.

**Boxing decision:** a conditionally-initialized `my` scalar **must stay boxed**
— the unboxable raw-slot path is skipped whenever a modifier is present. Reasons:
(1) the conditional assignment writes through the box; (2) when the cond is
false the box must read as undef, which a boxed `(make-p-box nil)` gives for
free (a raw slot would hold a stale/garbage value). Loop modifiers
(while/until/for/foreach) on a decl → whole-statement v1 fallback.

## D13 — never forward-declare `$x__lex__N` (fallback closure-capture renames)

**File:** `Pl/Parser2.pm` (`_forward_global_decls`).

**Why:** the fallback's `_with_declarations` renames a `my` captured by a nested
sub to `$x__lex__N` (v1's per-scope closure-capture convention), e.g. inside a
`map { my $x=$_; sub {$x} }` block. v2's forward-decl pass didn't recognize the
name (it's created inside the fallback, never `_reg_lex`'d), so it defvar'd it at
top level. A defvar proclaims the symbol **special**, which collapses every
per-iteration `(let (($x__lex__N …)))` into ONE shared DYNAMIC cell → all map
closures saw the LAST element's value ("ccc" instead of "abc"). Fix: never
forward-declare a `*__lex__\d+` name — it is always a true lexical, let-bound in
the generated code.

**This is a boxing-adjacent invariant:** "a name let-bound anywhere must never be
defvar'd" (the special-proclaim poison). `__lex__N` names are let-bound by the
fallback, so they belong to the same exclusion set as `_all_lex`, just via a
name-pattern instead of a registration (they aren't visible to `_reg_lex`).

## D14 — route v2's PPI parse through `_ppi_parse` (shared PPI-bug workarounds)

**File:** `Pl/Parser2.pm` (`parse()`).  Not a boxing decision.
v2 called `PPI::Document->new` directly, skipping `_fix_modulo_magic` (`7%-3`
mis-tokenized as the magic hash `%-`, dropping the modulo → PARSE ERROR). Now it
calls `$self->fallback_parser->_ppi_parse($src)` (reuse, not duplicate), so all
of v1's PPI workarounds apply.

## D15 — `\substr($x,…)` / `\vec` / `\pos` ref forces the variable BOXED

**File:** `Pl/VarAnnotator.pm` (new disqualifier).

**Why:** `my $r = \substr($s,0,1); $$r = "J"` writes back into `$s` through a
magic-cell lvalue ref (`p-substr-ref`). That cell aliases `$s`'s **box**; if `$s`
is a raw slot (VarAnnotator saw only `my $s = "hello"`, a single literal init, no
detected write) there is no box to alias and the write vanished (`$s` stayed
"hello"). Same family as the plain `\$x` disqualifier — a reference taken to the
variable (even indirectly, via substr/vec/pos) means it needs a real cell.
Disqualifier: `\\\s*(?:substr|vec|pos)\s*\(\s*$bare`. Logged for W12.

## D16 — forward-declare caret specials (`${^MPE}` → `|${^MPE}|`)

**File:** `Pl/Parser2.pm` (`_forward_global_decls`).  Not a boxing decision.
`${^MPE}` compiles to the pipe-delimited CL symbol `|${^MPE}|`; the forward-decl
scan's `[A-Za-z_]` couldn't match the `{^`, so it was never defvar'd → unbound
crash. Added a caret-symbol scan; sigil (box/array/hash) taken from the char
after the leading `|`.

## D17 — gate a return-LIST with a list-valued element → v1

**File:** `Pl/Parser2.pm` (return branch).  Not a boxing decision.
`return (0, @a)` / `return ($i, map …)` — a parenthesized multi-element list
mixing scalars with a list-valued element — lowered natively to
`(p-return (if *wantarray* (vector 0 (p-flatten @a)) …))`; p-return does NOT
splice the flatten-marker inside a vector (`0,#S(p-flatten-marker…)`). v1 spreads
the elements as separate p-return args, which flatten. Gate → v1 when the return
expr has a comma AND an `@`/`%` sigil or a list-op (map/grep/sort/…). Single
list-valued returns (`return @a`, `return map …`) are not wrapped and work.

## D18 — drain the fallback's buckets after `_lower_expr`'s `_parse_expression`

**File:** `Pl/Parser2.pm` (`_lower_expr` fallback).  Not a boxing decision.
A block-form-prototype arg (`first { … } @list`, `reduce { … } …`;
List::Util `(&@)` protos) makes v1 EMIT a top-level `(defun --anon-block-N-- …)`
into a bucket during PARSING, while the returned expression string only
*references* `#'--anon-block-N--`. `_lower_expr` took only the string, dropping
the defun → "undefined function --anon-block-N--". Fix: run `_parse_expression`
with the fallback's `definitions` bucket active and drain
preamble/decls/definitions/runtime into `_captured_decls` (mirrors
`_fallback_stmt_capture`); a self-contained anon-block is safe hoisted to the
section top.

## D19 — void `m//g` statement binds `*wantarray* :void` (narrowed)

**File:** `Pl/Parser2.pm` (plain expression statement + `_stmt_has_global_match`).
A void `$a =~ /(.)/g;` in a list-called sub inherited the caller's LIST context
dynamically → global match → `$1` was the LAST char, not the first. v1 wraps void
statements in `(let ((*wantarray* :void)) …)`; v2 didn't. Fix: wrap the raw form
in `:void`, but ONLY when the statement has an `m//g` match — wrapping EVERY void
statement over-scopes wantarray and is needless overhead (it perturbed
`print $i;` shapes; the guard pins the unwrapped form). s///g / tr///g act
globally in any context, so they're excluded. Not a boxing decision.

## D20 — scalar `our $x = RHS` init uses `(setf (p-box-value $x) RHS)` (BEGIN-safe)

**File:** `Pl/Parser2.pm` (`_lower_our_decl`).  Not a boxing decision, but a
subtle load-order one.  v2 emitted `(p-scalar-= $x RHS)`; its `unless (boundp …)`
re-init path interacts badly with a BEGIN block that assigned the var at compile
time — the runtime p-scalar-= then ran AFTER the BEGIN and clobbered it
(`our $r=""; sub greet{…} BEGIN{ $r=greet() } print $r` gave "" not "hello").
Direct box-value setf (v1's shape, verified byte-for-byte across value/ref/expr
inits) preserves source-order semantics. Array/hash/list `our` inits keep the
generic `p-*-=` path. Guard parser2-01 t59 updated to pin the new shape.

## D21 — gate a BEGIN doing sub-existence introspection (def-ordering) → v1

**File:** `Pl/Parser2.pm` (scheduled-block branch).
**LIMITATION, gated:** v2 assembles ALL sub definitions before ANY scheduled
block, so a BEGIN sees subs defined LATER in source too. Perl runs BEGIN at
compile time, seeing only subs defined *before* it — so compile-time
introspection that snapshots which subs exist (`main->can("later")`,
`defined &{"Pkg::$_"}`, `keys %Pkg::` — the Moo::Role make_role pattern) gets the
wrong answer under v2. The correct fix is interleaving defs and BEGINs in SOURCE
ORDER — an assembly-model change beyond W8. Gate a BEGIN whose body does
can/isa/DOES, `defined &`, or `%Pkg::` stash introspection → v1 (correct order).
A BEGIN that merely CALLS a sub (the s272g "BEGIN calls sub defined before it"
case) is unaffected and stays native. **Revisit in a later work item: proper
source-order interleaving of sub defs and BEGIN blocks.**

## D22 — gate `state` in an anon-sub / map-grep-sort block → v1

**File:** `Pl/Parser2.pm` (early gate in `parse()`).  Not a boxing decision.
`state` in a NAMED sub body works in v2. But `state` inside an anonymous sub
(`sub { ++state $x }` returned from a factory → independent state per returned
closure) or a map/grep/sort block (`map { state $x = … }` → one persistent cell
across iterations) needs per-instance state scoping that v2's state-var rename
doesn't provide (it shared one cell across all closure instances — f2 continued
f1's count). Getting nested-block state right is its own feature → gate → v1,
which handles it. Detection: a `state` declarator whose ancestor Block is
preceded by `sub`/`map`/`grep`/`sort`.

---

# s273 review pass (Fable 5) — corrections and additions

The s273 session reviewed D1–D22 and the handoff below. Verdict: the decisions
are sound EXCEPT D20, which was wrong and is reverted (D23). The handoff's
"3 files remain" undercounted — the full `PCL_V2=1 prove -j8 Pl/t/` run showed
**5**: the 3 listed plus `bop-01` t17–18 (was on the earlier ~19 list, never
addressed) and `begin-end-01` t13–14 (REGRESSED by D20). All 5 are fixed below.

## D23 — REVERT of D20: scalar `our $x = RHS` back to `p-scalar-=`

**D20 was a wrong decision, driven by a wrong test.** The evidence chain:

- Real perl for D20's own example prints **""**, not "hello"
  (`perl -e 'our $r = ""; sub greet { return "hello" } BEGIN { $r = greet() } print $r'`):
  the runtime `our` init executes in source order AFTER the compile-time BEGIN
  and clobbers it. The pre-D20 v2 behaviour ("" ) was CORRECT.
- The test that drove D20 — decl-ordering-01 "BEGIN calls sub defined before
  it" (`our $result = ""; … BEGIN { $result = greet() } print $result` expecting
  "hello") — asserts v1's divergence-from-perl, not perl. It has been fixed to
  use `our $result;` (no init), which tests exactly what its name says and is
  perl-correct under both pipelines.
- The mechanism by which `(setf (p-box-value $x) RHS)` "preserved" the BEGIN
  value is a **stale-cache artifact**, not source-order semantics: the raw setf
  bypasses `box-set`'s sv/nv cache invalidation, so the box's VALUE slot held
  the new value while `p-print`/stringify read the old cached string.
  (Instrumented: `(p-box-value $config)` returned "default" immediately before
  a print that output "from_begin".) v1 emits this raw-setf shape and therefore
  carries the same latent stale-cache divergence — **open v1 bug, logged in the
  completion plan (W10 area)**.
- begin-end-01 t13/14 (added earlier in W8, pipeline-aware, pinning the
  perl-correct order for v2) failed under D20 and pass again after the revert.

Guards: parser2-01 t59 reverted to pin `p-scalar-=`; parser2-02 pins both the
positive (`p-scalar-=`) and negative (no raw `p-box-value` setf) shape.

**Lesson recorded for future sessions:** when a Pl/t expectation and real perl
disagree, perl wins — run the snippet under `perl` BEFORE "fixing" the
transpiler to satisfy the test (CLAUDE.md §5 cuts both ways: the test itself
can be the bug, and then it gets fixed to assert perl's behaviour, not
weakened).

## D24 — bitwise compound assigns are writes (`&=` `|=` `^=`, `&.=` `|.=` `^.=`)

**File:** `Pl/VarAnnotator.pm`. The compound-assign disqualifier class listed
arithmetic/string ops but not the bitwise family, so `$s &= "AAAAA"` was an
unseen write → `$s` became a raw slot → `p-bit-and=` (a box op) silently
no-op'd (bop-01 t17/18: "zzzzz" unchanged). Added to the class; logged in the
header checklist for W12.

## D25 — paren-less `\substr $x` / `\vec` / `\pos` also force the box

**File:** `Pl/VarAnnotator.pm`. D15's regex required a literal `(` after the
builtin name; the named-unary paren-less form `${\substr $t, 0} = …` slipped
through → `$t` raw → the magic-cell write vanished (misc-fixes-02 t27: `[$t]`
empty while the "" overload still fired). Paren made optional (`\(?`), same as
the existing `pos` gate.

## D26 — handle-vivifying builtins write their FH argument

**File:** `Pl/VarAnnotator.pm`. `open($h, '<', $path)` (and opendir, sysopen,
pipe, socket, socketpair, accept) mutates/associates through `$h`; a raw slot
loses the handle association (fileio-02 t25: both reads returned undef). New
disqualifier for the family; logged for W12.

## D27 — self-referencing `my $i = $i` init reads the OUTER variable (native path)

**Files:** `Pl/Parser2.pm` (`_lower_block` scalar branch), `cl/pcl-runtime.lisp`
(`p-box-init`, exported). The boxed emission ran `(p-my-= $i INIT)` INSIDE the
new `let`, so `my $i = $i;` read the fresh nil box (closure-01 t17: else-branch
closures returned undef). A CL `let` init-form is evaluated in the OUTER
environment, so the fix moves the init into the binding:
`(let (($i (p-box-init INIT))) …)` — `p-box-init` = fresh box + `box-set`
(Perl copy semantics). Applied only when the init text mentions the declared
name (over-fire harmless: the shape is semantically identical to the p-my-=
form). The raw-slot (unboxable) path was already correct — its init always sat
in the binding. `my $x = $x if COND` (shadow + modifier) is perl-undefined
behaviour and is not special-cased.

## D28 — gate: `my`-shadow of a live lexical inside a FALLBACK block → v1

**File:** `Pl/Parser2.pm` (`_gate_seam_my_shadow`, called from `_lower_expr`'s
fallback and `_fallback_stmt`). Found while extending D27's guard to the
`do {}` form: a `my $x` inside a Block WITHIN a fallback expression/statement
(map/grep/do/anon-sub bodies), where `$x` is already live in v2's scope, does
NOT create a fresh binding — v1's seam machinery consults `_let_bound_vars`
(which v2 pre-populated with the outer name) and emits a plain assignment, so
the "shadow" writes through the OUTER lexical. Observed corruption:
`my $x = "outer"; my @r = map { my $x = $_ * 2; $x } (1,2,3);` left `$x == 6`;
same for `do { my $x = …; }`. This was **silently wrong output** on a COMMON
idiom — worse than any of the 5 failing files.

Gate rule: a `my`/`state` whose declared name is in `_live_lex` AND whose
ancestry crosses a `PPI::Structure::Block` at-or-below the fallback root →
die → v1. A same-level `my` in a fallback statement stays allowed — that is
the sanctioned seam contract (v2 registers the name, v1 assigns into the
existing binding; container decls rely on it).

Note: v1 (and therefore the post-gate pipeline) still diverges from perl on
the map case via its own defvar-shadow limitation
(`docs/closure-lexical-scoping.md`) — prints "2 4 6 6" vs perl "2 4 6 outer".
That is the long-known pre-existing v1 bug, now at exact v1 parity. Reclaiming
these gated files natively (v2's real lets shadow correctly!) is a cheap,
high-value follow-up — see the completion plan's W8.5.

## D29 — W8.5: seam my-shadow RENAMED (`$x__shadow__N`) instead of gated

**File:** `Pl/Parser2.pm` (`_gate_seam_my_shadow` now renames when safe;
`_rename_decl_within` + `_shadow_rename_blocker` shared helpers).
D28's gate is now the FALLBACK, not the outcome: a `my $x` shadowing a live
lexical inside a fallback Block is renamed to a fresh `$x__shadow__N` across
its Block (= its whole Perl scope), so the seam sees a non-colliding name and
handles it as an ordinary block lexical.  **Perl visibility rule honoured:**
only the declared symbol and symbols AFTER the declaration statement are
renamed — `my $x = $x` / `my $foo = vec($foo,…)` RHS still reads the OUTER
variable (same-statement uses see the old binding, perlsub).  Element
accesses `$x[0]`/`$x{k}` are skipped via `->symbol` (they are @x/%x, not the
scalar).  Blockers (→ still die → v1): interpolated use of the name in the
block ("$x" — the token rewrite can't reach it; yadayada.t stays gated),
multiple declarations of the name in the block (re-shadow), `${x}`
brace-deref, string eval in the block (captures lexicals BY NAME), `state`
(per-instance semantics via state_var_renames), non-scalar.
**Result: the map probe is perl-CORRECT under v2-native (`2 4 6 outer`),
BETTER than v1 (`2 4 6 6`, its defvar-shadow bug) — documented v2>v1
divergence per the §2 rule.** do.t/vec.t reclaimed native; yadayada.t
stays v1 (interpolated `"($err)"`).

## D30 — W8.5: POISONED condition-my renamed in a pre-pass (defins.t crash)

**File:** `Pl/Parser2.pm` (`_rename_poisoned_cond_mys`, run per segment right
after the W5 rename, pre-analysis).
**The bug (found in the s273 parity sweep — v2 PARTIAL on defins.t):** D4
registers a condition-`my` name in `_all_lex`, which excludes it from
`_forward_global_decls` (the defvar-poison rule).  But defins.t ALSO uses the
same name as a package GLOBAL elsewhere (`($seen ? $dummy : $name) = <FILE>`)
— that global then has no defvar → unbound-variable crash at test 6.  W7 never
saw it because pre-D4 the condition-my leaked into a defvar'd global
(wrong scoping, right defvar) — D4 fixed the scoping and exposed the
exclusion conflict.
**Fix:** a segment pre-pass finds all condition-my declaration sites
(Structure::Condition of if/unless/while/until + Structure::For sections,
`my` not nested in a deeper Block) and, for each name **used anywhere outside
all of its cond-my constructs** (Symbol-exact; interpolated uses
approximately), renames the construct-scoped lexical to `$name__cond__N`
via the shared D29 machinery.  Un-poisoned (self-contained) loops are NOT
renamed — zero churn on the common `while (my $line = <FH>) { print "$line" }`
(whose interpolated body would otherwise force a gate).  If any of a poisoned
name's constructs is un-renameable (blocker) → die → v1 (leaving it would
crash at runtime).
**Known remaining sibling (logged, NOT fixed):** the single-counter C-for
carve-out (`for (my $i…)`) has the same _all_lex-vs-global conflict; renaming
it must map the VarAnnotator vi key through the rename or the intloop
unboxing is lost — left for W12 (structural events make it trivial).  Also
the interp-both-inside-and-outside approximation under-detects poison — that
case crashes exactly as before this pass (no worse).

## Session end state (handoff — superseded by s273 above for the failure list)

**Committed this session (all on `main`, v1 Pl/t gate stayed 100%):** 19 → a
handful of Pl/t files under `PCL_V2=1`. Root-cause fixes: lvalue-assign boxing
(p-my-= returns box), condition-`my` scoping, per-block pragma scope, C-for
multi-decl, fat-comma token-restore (every OO constructor), return-if modifier,
inherit/void ctx map, list-assign boxing (broadened detector), map-closure
per-iteration capture (`__lex__N` no-defvar), `my $x=INIT if COND`, modulo PPI
workaround, `\substr`/`\vec`/`\pos` boxing, caret-var forward-decl, return-list
flatten gate, block-form anon-block defun collection, void `m//g` context,
`our`-init BEGIN-order (setf), BEGIN-introspection gate, state-in-block gate.

**Remaining Pl/t failures under v2 (NOT yet addressed):**
- `fileio-02` t25 — symbolic-open then bareword read agree on handle name.
- `closure-01` t17 — nested my-shadow capture across if/else branches.
- `misc-fixes-02` t27 — box-set class identity on blessed substr (may be the
  documented scalar-SV-identity not-supported case — check `not-supported.md`
  "Scalar copy does not preserve reference/SV identity" before fixing).

**Next:** finish those 3 files, then re-run the FULL `PCL_V2=1 prove -j8 Pl/t/`
to confirm, and a perl-tests parity sweep (the broad changes — `_lower_expr`
bucket drain, void-wrap, our-init, push_scope — need a full v1-vs-v2 sweep to
confirm W7 parity still holds). Then W9 (flip default; CACHE KEYING FIRST).
decl-ordering-01/02, closure/state/wantarray/match-vars/lvalue-ref/bop/socket/
use-require/pcl-dash-m/fileio-02.
