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

## Pending / under investigation

Working through remaining W8 files: transpile-test-04/05, misc-fixes-01/02,
decl-ordering-01/02, closure/state/wantarray/match-vars/lvalue-ref/bop/socket/
use-require/pcl-dash-m/fileio-02.
