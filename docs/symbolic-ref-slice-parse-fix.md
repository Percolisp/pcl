# Parser-level fix for `@{EXPR}[slice]` / `@{EXPR}{slice}` (and `${EXPR}[i]`)

Design for resolving the session-245 open decision (option 3 — fix at the
parser layer). Replaces the fragile codegen string-rewrite in
`Pl/ExprToCL.pm gen_prefix_op` (committed in `2bc25da`, carries a live
regression) and the uncommitted `_is_symbolic_name` guard.

## 1. The question: is the information available at parse time?

**Yes — completely and unambiguously.** The two confusable cases differ in the
*position* of the subscript relative to the braces, and PPI preserves that
position exactly:

```
@{$h{a}}        =>  Cast<@>  Block<{$h{a}}>                      (deref — subscript INSIDE)
@{$h{a}}[1]     =>  Cast<@>  Block<{$h{a}}>  Constructor<[1]>    (slice  — subscript AFTER)
@{"av"}[1,3]    =>  Cast<@>  Block<{"av"}>   Constructor<[1,3]>  (slice)
@{"av"}{a,b}    =>  Cast<@>  Block<{"av"}>   Subscript<{a,b}>    (hash-value slice)
@{$s}[1,3]      =>  Cast<@>  Block<{$s}>     Constructor<[1,3]>  (slice — works today)
${"sv"}[0]      =>  Cast<$>  Block<{"sv"}>   Constructor<[0]>    (element access)
@{"a"."v"}[1]   =>  Cast<@>  Block<{"a"."v"}> Constructor<[1]>   (computed-name slice)
```

(`_retag_braced_deref_subscript` already re-blesses the trailing `[..]`
Constructor → Subscript for `$`/`@` casts, so by the time the dispatch loop
runs, every "subscript AFTER" case is a `Structure::Subscript` following a
`Structure::Block` following a `Token::Cast`.)

The ambiguity that sank the codegen approach **only exists after code
generation**: the parser merges `$h{a}` inside the block into a
`gethash-box`, producing the same `(p-cast-@ (p-gethash-box ...))` string
shape as the mis-parsed `@{"av"}{..}`. At the token level they were never
ambiguous — the bug is that the parser throws the position information away.

## 2. Why this is the right layer (and precedented in our own code)

The `%`-sigil forms already do exactly this, today, in `Pl/PExpr.pm`:

- `%{EXPR}[indices]` → `$is_kv_arr_deref_constructor` (line ~809): keyed purely
  on raw token shape `Cast('%') + Block + Constructor`, parses the block
  contents as an **arbitrary expression**, builds `kv_slice_a_acc`.
- `%{EXPR}{keys}` → `$is_kv_hash_deref_block` (line ~818): same, builds
  `kv_slice_h_acc`.

So `%{"hv"}{a,b}` parses correctly with no heuristic. The `@` and `$` casts
just never got the analogous treatment; they fall into the generic
`is_arr_or_hash_braces` branch (line ~1060), which only selects a slice node
when the *parsed* block contents collapse to a lone `$var`
(`is_var($pre_n) && content =~ /^\$/`, line ~1069). Any other block content —
string literal, concat, `$h{a}` element — falls through to plain element
access `a_acc`/`h_acc`, and the leading Cast is later applied as a prefix op,
yielding the inverted `(p-cast-@ (p-aref-box EXPR IDX))`.

## 3. The fix

### 3.1 `Pl/PExpr.pm` — generalize the slice selection in the generic branch

In the `is_arr_or_hash_braces($term)` branch, before (or instead of relying
on) the parsed-node `is_var` check, test the **raw token shape**:

```perl
my $cast_before = ($i >= 2) ? $e->[$i-2] : undef;
my $pre_is_deref_block =
     ref($e->[$i-1]) eq 'PPI::Structure::Block'
  && $e->[$i-1]->start() eq '{'
  && $cast_before
  && ref($cast_before) eq 'PPI::Token::Cast';
```

Type selection becomes:

| shape | trailing `[..]` | trailing `{..}` |
|---|---|---|
| `@{BLOCK}…` (Cast `@` + Block) | `slice_a_acc` | `slice_h_acc` |
| `${BLOCK}…` (Cast `$` + Block) | `a_ref_acc` | `h_ref_acc` |
| `%{BLOCK}…` | already handled by the kv flags — unchanged |

The base child is `parse([$block])` — the existing call; block contents are an
arbitrary expression (`$s`, `"av"`, `"a"."v"`, `$h{a}`, `[1,2,3]`,
`$obj->method`). No inspection of what's inside the block, ever.

The lone-`$var` case (`@{$s}[..]`) now hits the new raw-token rule instead of
the `is_var` accident — producing the **identical node** (`slice_a_acc`), so
nothing regresses. The `is_var` path must stay for the brace-less forms
(`@$s[0,2]`, `$$s[0]`, `%$s{k}`), where `pre` is a Symbol, not a Block.

The existing post-splice Cast-removal code (line ~1132: removes the leading
Cast `@`/`%` for `slice_*`/`kv_slice_*` types, and Cast `$` for
`a_ref_acc`/`h_ref_acc`) already covers both new paths unchanged.

### 3.2 What does NOT match the rule (the regression case, structurally)

`@{$h{a}}` / `@{$a[0]}` / `@{$obj->{x}}` **with no trailing subscript** never
enter the subscript branch for the block — there is no third token. They stay
a plain Cast-prefix deref → `(p-cast-@ ...)`. The session-245 regression
(`scalar @{$a[0]}` → 1) disappears structurally, not via base-sniffing.

`@{$h{a}}[1]` (deref of a container element **with** a trailing subscript) is
genuinely a slice in Perl, and the rule correctly builds
`slice_a_acc(h_acc($h,"a"), 1)` → `(p-aslice (p-gethash ...) 1)`.

### 3.3 `Pl/ExprToCL.pm` — delete the string-rewrite machinery

Remove entirely (both the committed `2bc25da` array branch and the
uncommitted hash/guard extension):

- the rewrite block in `gen_prefix_op`'s `@`/`%`/`$` cast arm
  (`$operand =~ /^\(p-aref-box (.+)\)$/` etc.),
- `_is_symbolic_name`, `_slice_indices`, `_split_first_sexp`
  (no callers outside this machinery).

`gen` for `slice_a_acc`/`slice_h_acc`/`a_ref_acc`/`h_ref_acc` already exists
and handles rvalue + lvalue (it's what `@{$s}[..] = ...` uses today).

### 3.4 Runtime — no change needed

The committed `p-aref`/`(setf p-aref)` string-resolution (`2bc25da`, keep) and
the pre-existing `p-gethash`/`(setf p-gethash)` string arms (lines ~5053/5079
of `cl/pcl-runtime.lisp`, via `p-ensure-arrayref`/`p-ensure-hashref`,
NUL-name → undef/no-op) mean every slice primitive bottoms out in a
ref-or-string check at runtime. This is the genuine "one path": the parser
contributes only *structure* (slice vs deref), the runtime decides
*ref vs symbolic name*.

## 4. Edge cases checked

- `@{[1,2,3]}[0,1]` — anon-arrayref deref slice (interpolation idiom): base
  parses to an arrayref constructor; `p-aslice` on a ref works natively.
  Strictly more correct than main today.
- `%`-cast forms can't be hijacked by the new rule: their trailing `[..]`
  stays a Constructor (retag is `$`/`@`-only) and `{..}` is a Block, so
  `is_arr_or_hash_braces($term)` is false and the dedicated kv flags fire as
  before.
- `&{$code}(args)`, `*{$glob}{CODE}`, `sort {…} @x`, hash constructors —
  unaffected: the rule requires Cast `@`/`$` + Block + Subscript-type term.
- `$h{k}[0]` chained subscripts: by the time `[0]` is dispatched,
  `$e->[$i-1]` is an *internal node* (h_acc), not a PPI Block — rule doesn't
  fire.
- Internal-node `pre`: guard the raw-token check with
  `!$self->is_internal_node_type($e->[$i-1])` for safety (internal nodes are
  hashrefs, not PPI objects).

## 5. Step plan

1. Revert the `gen_prefix_op` rewrite + 3 helpers in `Pl/ExprToCL.pm`
   (including the committed part — this removes the live regression).
2. Implement §3.1 in `Pl/PExpr.pm`.
3. Keep `Pl/t/misc-fixes-02.t` tests 17–19 exactly as written (they encode the
   target behavior); add deref-guard tests: `scalar @{$a[0]}` == 3,
   `@{$h{a}}`, `@{$o->{x}}`, and `@{$h{a}}[1]` (element-deref WITH slice).
4. Run: `prove Pl/t/misc-fixes-02.t`, then the fuzzer
   (`tools/difftest-ops.pl`, axes 22/23 included) — the 9 mismatches / 5
   clusters from session 245 should drop to the documented-deferred set.
5. `prove -j8 Pl/t/` gate (6+ min) + sweep-diff against baseline
   (`perl-tests/ref.t` must stay 237/245 PARTIAL, run unmodified).

## 6. Out of scope (separate small follow-ups)

- `$ar->$#*` postfix last-index deref (fuzzer finding, session 245) — small,
  unrelated fix target in the postfix-deref arm (`Pl/PExpr.pm` ~line 1007).
- `$#{EXPR}` last-index of a symbolic name — different cast token, untouched.
