# state.t — top-level `goto LABEL` and `tagbody` fix

## The problem

`perl-tests/state.t` test 62 uses a bare `goto LABEL` at file scope:

```perl
again:
    push @simpsons, shift @presidents;
    goto again if @simpsons;
```

PCL generates this as top-level CL forms:

```lisp
:again  ;; pcl-label
(p-push @simpsons ...)
(p-if @simpsons (go :again))
```

CL's `(go :again)` requires a lexically-enclosing `(tagbody ...)`. At the top level there is none, so SBCL signals:
> "attempt to GO to nonexistent tag: :AGAIN"

## The fix (implemented, session 143)

### Two-part solution

**Part 1 — Label sentinel** (`Parser.pm`, `_process_compound_statement` line ~1978):

Standalone Perl labels (`again:`) are emitted with a `;;pcl-label` comment suffix:
```lisp
:again  ;; pcl-label
```

In CL, `;;` is a line comment so this reads identically to `:again`. The suffix lets `_wrap_runtime_labels` distinguish real generated labels from `:word` patterns inside string literals (e.g., split.t has `":cd"` and `":ef"` inside a CL string that would otherwise be false positives).

**Part 2 — Minimal tagbody** (`Parser.pm`, `_wrap_runtime_labels`):

Rewrote the wrapping algorithm to build the MINIMUM tagbody:

1. Find each `;;pcl-label`-marked label (first occurrence)
2. Find the last **qualifying** `(go :LABEL)` for each label — "qualifying" means:
   - The `@rt` element starts at column 0 (not indented = not a nested form)
   - The `@rt` element is not a definition (`p-sub`, `eval-when`, etc.)
   - The text before `(go :LABEL)` in the element contains no `lambda` keyword
     (a goto inside a lambda can never reach a tagbody tag — CL `go` is lexically scoped)
3. Compute ranges: `[min(label_pos, last_goto_pos), max(...)]`
4. Merge overlapping ranges
5. Wrap each range in `(tagbody ...)`, hoisting any definition elements out
6. Everything outside the ranges is emitted as independent top-level forms

For state.t this shrinks the tagbody from ~530 lines (session 142) to **~10 lines**:

```lisp
(tagbody
:again  ;; pcl-label
(box-set $next (p-shift @simpsons))
...
(p-if @simpsons (go :again))
)
;; rest of state.t runs as independent top-level forms
```

### Why the three-test case works

Three test files have top-level bare labels:

| File | Labels | Situation | Result |
|------|--------|-----------|--------|
| **state.t** | `:again` | Top-level backward-goto loop | Minimal 10-line tagbody ✓ |
| **sort.t** | `:label` | Gotos only inside lambdas/functions | No tagbody (gotos don't qualify) ✓ |
| **split.t** | `:cd`, `:ef` | Inside a CL string literal | No tagbody (no sentinel) ✓ |

## Current state (session 143)

- state.t: **55+50/166 ran**, crashes at test 106 ("Not a CODE reference")
- Tagbody fix is fully implemented and regression-tested
- PCL suite: **74 files, 2868 tests, all passing**

## Remaining crash at test 106

```perl
$f = i_49522();
h_49522();   # initialise state $t
is $f->(), 99, "state var closure 3";
```

`i_49522` is a named sub defined INSIDE `h_49522`'s body:
```perl
sub h_49522 {
    state $t = 99;
    sub i_49522 {   # named sub inside named sub
        sub { $t };
    }
}
```

In Perl, named subs inside other subs are compiled at **package compile time**, so `i_49522` is available before `h_49522` runs. In PCL, `(p-sub pl-i_49522 ...)` inside `pl-h_49522` only runs when `pl-h_49522` is called. When `i_49522()` is called first (to get a sub ref), the stub `p-declare-sub` for `pl-i_49522` returns nil. Then `$f->()` calls `p-die "Not a CODE reference."`.

**Fix**: Hoist inner named subs to top level at codegen time (Option C). When `_process_sub_statement` detects it's inside another sub, emit the inner sub's `p-sub` as a definition in the outer section (via `_with_bucket('definitions', ...)` at the section's scope) rather than inside the enclosing function body.

This is moderately complex because:
- The inner sub can capture variables from the outer sub (`state $t` in this case)
- Hoisting requires those captures to work via closures
- For state variables specifically, the capture is already a box reference, so closure capture should work

## Plan for inner named sub hoisting

1. In `_process_sub_statement`, detect `$self->in_subroutine > 0` (nested sub)
2. For NAMED (not anonymous) nested subs, hoist by saving `$self->_cur_bucket`, switching to `definitions`, emitting `(p-sub ...)`, and restoring the bucket
3. The sub body still closes over any lexical/state variables via box references (already handled)
4. Add a `p-declare-sub` stub in the runtime position so calls to the sub before the definitions are loaded work (or rely on `eval-when` compile-time loading)

This would fix test 106 and potentially more tests in the 107-166 range that also use named subs inside subs.
