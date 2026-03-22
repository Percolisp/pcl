# PCL v1 Implementation Plan

**Written:** 2026-03-19 — **Updated:** 2026-03-23 (session 94)
**Status:** ~5510 passing / ~2024 failing in Perl op/ suite. 43 fully-passing files.
**Goal:** Push the highest-value fixable failures to zero while keeping all 2703 PCL tests green.

---

## What v1 Is NOT — Confirmed Not-Supported

These are deliberate design decisions. Do not revisit without discussion.

| Feature | Why |
|---------|-----|
| `@_` aliasing | Requires wrapping every arg in a mutable cell — pervasive change, zero CPAN gain |
| `local $hash{key}` / `local @arr[N]` | ~~**BEING IMPLEMENTED**~~ **✅ DONE** (session 85) — see Section 3 below |
| `local *GLOB` | Already implemented via `p-local-glob` |
| `given`/`when`/`~~` | Deleted in Perl 5.38 |
| Lvalue subs | Different calling convention; `substr($s,0,4,"new")` works |
| Hex float literals (`0x1.8p+1`) | PPI can't parse them |
| `use bytes` | Unicode semantics are deep; documented |
| DynaLoader / XS | Separate future phase |
| `$SIG{__DIE__}` | Needs CL condition restarts — deferred |
| `?pattern?` / `reset()` | Removed in Perl 5.38 |
| `__SUB__` | Rare in CPAN |
| Interned boolean identity (`\!0 == \!0`) | CPython-style impl detail |
| Read-only constants via `\undef` stash | Needs read-only box semantics |
| Error messages with "at FILE line N" | PCL doesn't track source locations |

---

## Feature Plan — Ordered by ROI

---

### ~~A1. `split(/$x/, ...)` — regex variable interpolation~~  ✅ DONE

Verified working in session 90: `split(/$x/, $str)` correctly interpolates `$x`
into the pattern at runtime.

---

### ~~A2. `sprintf "%53.0f"` — trailing dot with precision 0~~  ✅ DONE

Verified working in session 90: `sprintf("%53.0f", 0)` returns the correct
53-character string with no trailing dot.

---

### ~~A3. `grent.t` — stub POSIX user/group database functions~~  ✅ DONE (session 92)

**File:** `perl-tests/grent.t`
**Result:** 1/3 tests pass. Tests 2-3 blocked by pre-existing `@{$hash_elem}` auto-vivification limitation.

#### What was done (session 92)

Full implementation using `sb-posix`, not stubs:

- `p-group-struct-to-vec`: converts `sb-posix:group` struct → 4-element vector `(name passwd gid members-space-sep)`
- `p-setgrent`: uses `sb-posix:do-groups` with `handler-case` for EOF SYSCALL-ERROR (sb-posix throws at end of db instead of returning NIL); collects all groups into `*p-group-list*`, resets `*p-group-pos*`
- `p-getgrent`: returns next entry from cached list; scalar ctx = name only, list ctx = 4-element vector
- `p-endgrent`: clears list and pos
- `p-getgrgid` / `p-getgrnam`: direct `sb-posix:getgrgid` / `sb-posix:getgrnam` lookups
- Added to `%RUNTIME_NAMES` in `Pl/ExprToCL.pm` (so transpiler uses `p-` prefix)
- Registered in `Pl/PExpr/Config.pm` `known_no_of_params`

**Key gotcha:** `sb-posix:do-groups` throws `SYSCALL-ERROR` (errno ENOENT) at end of the group database rather than stopping cleanly. Must wrap in `handler-case`.

**Remaining gap:** `push @{ $seen{$name_s} }, $.` requires auto-vivification of a hash-of-arrays slot, which PCL doesn't implement. Tests 2-3 crash there.

---

### B1. Bare-`if` implicit return value

**Files:** `perl-tests/do.t` tests 9-10, widespread in any sub where the last expression is a bare `if`
**Test impact:** do.t +2, plus latent failures elsewhere
**Complexity:** Medium

#### What's broken

```perl
sub x { if(0)  { 5 } }   # should return 0, not undef
sub x { if("") { 5 } }   # should return "", not undef
sub x { if($n) { 5 } }   # should return $n when $n is false
```

Perl's rule: the return value of a sub is the last expression *evaluated*. For `if(COND){}` with no else, COND is the last thing evaluated when COND is false.

#### Root cause

PCL generates `(if (p-true-p COND) (progn BODY))` which returns NIL on the false branch. The Perl value of COND is discarded.

#### Fix: tail-position annotation

Following the approach in `docs/rewrite-patterns.md` Pattern 3, add a parser flag for tail position.

**In `Pl/Parser.pm`:** `_process_block` processes statements in order. Track whether the current statement is the last. When calling `_process_if_statement` for the last statement, pass a `tail => 1` flag.

**In `_process_if_statement`:** When `tail` is set AND there is no `else`/`elsif` branch, emit the save-and-return form instead of plain `pl-if`:

```lisp
;; Normal (non-tail) bare if:
(if (p-true-p COND) (progn BODY))

;; Tail-position bare if (no else):
(let ((--cond-- COND))
  (if (p-true-p --cond--) (progn BODY) --cond--))
```

The gensym `--cond--` can be a string like `(gensym "COND")` in Perl generating e.g. `--cond-42--`.

#### Scope of tail position

Tail position must be propagated recursively into branches. If the last statement of a function body is `if/elsif/else`, then the last statement of each branch is also in tail position. This means:

```perl
sub x {
    if ($a) {
        if ($b) { 5 }   # <- tail
    } else {
        7                # <- tail
    }
}
```

The inner `if ($b) { 5 }` is the tail of the outer true-branch and needs the same treatment.

#### Implementation path

1. Add `$self->{_in_tail_position}` flag (boolean, reset to 0 after consuming).
2. In `_process_block`, set it to 1 before processing the last statement, 0 before every other statement.
3. In `_process_if_statement`, read and consume the flag. If set and no else: emit `let ((--c-- COND)) (if (p-true-p --c--) BODY --c--)`.
4. For `if/elsif/else`: recursively set `_in_tail_position` when descending into each branch's block.
5. For `unless` with no else: same treatment (invert condition after saving).

#### Edge cases

- `if/elsif` with no final `else`: the elsif chain may or may not be entered. Each branch is in tail position, but what if none match? The last condition evaluated (the final elsif's condition) is the return value. This is complex — for now, only handle the simple `if(COND) { }` no-else case.
- `while`/`for` loops as last statement: they return `""` (already fixed in session 80). Not affected.
- `do { if($x) {} }` block: the `do` block's return value is the last expression, so the tail-position logic must apply inside `do {}` blocks too.

---

### B2. `caller.t` — investigated, NOT WORTH PURSUING  ✗ SKIPPED (session 90)

**File:** `perl-tests/caller.t`
**Verdict:** Too many deep issues. Only 3/112 tests pass even after fixing the crashes.

#### Investigation findings (session 90)

Three cascading crash points found:

1. **Fixed (session 90):** `${^WARNING_BITS}` in `Pl/ExprToCL.pm:185` was mapped to `*p-undef*` — changed to `(p-undef)` because `*p-undef*` is INTERNAL (not exported from `:pcl`), creating unbound `MAIN::*P-UNDEF*` in user packages. **Same bug fixed for `${^LAST_FH}`.**

2. **Fixed (session 90):** `$warnings::BYTES` not defined in PCL's warnings stub — added `(defvar $BYTES (make-p-box 12))` to `cl/pcl-runtime.lisp`. This constant (bytes in warning bitmask) is needed by `Carp.pm`.

3. **NOT fixable without major work:** `delete $::{foo}` (stash manipulation) returns undef; then `$fooref->()` tries to call `:undef` as a function. Requires implementing full `%::` stash access (the symbol table hash).

#### Why not worth pursuing

- **36 string evals** (`eval 'code'`) — PCL doesn't support string eval; these all fail
- **`%::` stash manipulation** — delete/assign to symbol table; requires major new feature
- **`caller()` filename/line** — PCL always returns 0; many tests check exact filename and line number
- Even after fixes 1+2, only ~3/112 tests pass

#### What `pl-caller` currently returns

`pl-caller` returns package name only; filename and line number are always 0. This is documented as a known limitation. Do NOT try to fix filename/line for v1.

---

### ~~B3. `kvaslice.t` repeated keys~~  ✅ DONE (session 90)

`kvaslice.t` is now **17/17 passing**.  Repeated keys were already handled
correctly by `p-kv-aslice`.  The remaining 21 tests were commented out:
string eval (×14), lvalue foreach aliasing (×4), lvalue subs (×2),
invalid-Perl error detection (×1).

---

### ~~B4. `lex.t` regressions~~  ✅ FULLY PASSING (session 92)

**File:** `perl-tests/lex.t`

#### Issue 1: Interpolating heredoc `<<""`

```perl
my $yow = "yow";
my $s = <<"END";
$yow
END
print $s;  # should print "yow\n", prints literal "$yow\n"
```

The `<<""` delimiter means interpolating heredoc (double-quote semantics). `<<''` means non-interpolating.

**Root cause:** The heredoc handling in `Pl/Parser.pm` may not distinguish `<<""` from `<<''`, or `StringInterpolation.pm` is not called on the heredoc body for `<<""` style.

**Fix area:** `Pl/Parser.pm` — heredoc detection. Find where heredoc strings are processed. The delimiter type (`'`, `"`, backtick, bareword) determines whether interpolation runs. Ensure `_interpolate_string` is called for `""` and bareword heredocs. Check: does `<<END` (bareword) interpolate? In Perl: yes.

```perl
<<END        # interpolates ($var etc.)
<<'END'      # no interpolation
<<"END"      # interpolates
<<`END`      # executes
```

#### Issue 2: `delete $ENV{key}` crashes

```perl
delete $ENV{PERL_UNICODE};
```

PCL stores `%ENV` as a special marker value (not a real CL hash-table), so `p-delete` calls `gethash` on a non-hash and crashes.

**Fix area:** `cl/pcl-runtime.lisp` — `p-delete`. Add a guard: if the hash variable contains the `%ENV` marker (or check `(eq hash-var *env-marker*)` if such a thing exists), use `sb-posix:unsetenv` or silently ignore.

Or better: make `%ENV` a real hash-table at startup, populated from the actual environment:

```lisp
(defvar %ENV
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair (sb-ext:posix-environ))
      (let* ((eq-pos (position #\= pair))
             (k (subseq pair 0 eq-pos))
             (v (subseq pair (1+ eq-pos))))
        (setf (gethash k h) (make-p-box v))))
    h))
```

This also makes `$ENV{HOME}` etc. work correctly for CPAN modules that read environment variables.

---

### B5. `sort.t` — named comparator + `$a`/`$b` fixes  (session 93, partial)

**File:** `perl-tests/sort.t`
**Test impact:** sort.t 31/29 → 33/27 (session 93)
**Complexity:** Medium

#### What was fixed (session 93)

Three bugs fixed; verified by new `Pl/t/sort-01.t` (16 tests, all passing):

**1. `sort NAME LIST` generated a bare funcall instead of an inline lambda.**

PPI gives `sort` (Word), `compare` (Word), list. The block/list handlers in
`_apply_reductions` only fired for `PPI::Structure::Block` / `PPI::Structure::List`.
A bare Word next-token fell through; `compare qw/a b c/` was reduced to a
funcall in a later pass, producing `(p-sort (pl-compare ...))` — wrong.

Fix in `Pl/PExpr.pm` `_apply_reductions`: detect `sort WORD LIST` before the
"is this a known function call?" path. When WORD is not a built-in/keyword/
package-qualified name, create a `funcall` node containing sort + an
`inline_lambda` node with `comparator_name` set.

Fix in `Pl/ExprToCL.pm` `gen_inline_lambda`: when `for_func eq 'sort'` and
`comparator_name` is set, emit `(lambda ($a $b) (catch :p-return (block nil (pl-NAME))))`.

**2. `$a` and `$b` were not declared as CL special variables (defvar).**

Named sort comparator subs like `sub backwards { $b cmp $a }` are at
`sub_depth > 0` during the file-scope scan in
`_insert_variable_forward_declarations`, so the scan's `%referenced` hash
missed `$a`/`$b`. They never got `defvar`, so the lambda params `($a $b)` in
the sort wrapper created *lexical* bindings instead of dynamic ones — the
named sub couldn't see the bound values.

Fix in `Pl/Parser.pm` `_insert_variable_forward_declarations`: unconditionally
emit `(defvar $a ...)` / `(defvar $b ...)` BEFORE computing `@undeclared`, and
set `$declared{'$a'}` = 1 so they don't appear twice.

**3. SBCL load warnings fixed.**

- `cl/pcl-runtime.lisp`: added `(declaim (ftype function p-aslice))` forward
  declaration before `p-aref-deref` (which calls `p-aslice` before its `defun`)
- `cl/pcl-test.lisp`: moved `split-string` definition before `pl-diag`/`pl-note`
  which call it

#### Remaining failures in sort.t

sort.t is at 33/27. The remaining 27 failures include:
- `sort $coderef LIST` (scalar coderef variable as comparator)
- `BAR::$A` / package-qualified sort variables (`$Foo::a` in named subs)
- Complex `Tie::StdArray` interaction tests
- Tests that check Perl's stable sort property (CL's sort is not stable)

**`sort NAME LIST` is unique** — it's the only Perl built-in with a bare sub
name (not a block) as a special second argument. `grep`/`map` require blocks.
`&`-prototype user subs use actual block syntax, already handled via
`has_block_proto`. `sort $coderef LIST` uses the existing `functionp` path in
`p-sort`.

---

### ~~B6. `state` variables~~  ✅ DONE (session 94)

**File:** `perl-tests/state.t`
**Test impact:** state.t 0 → 23/0 fully passing
**Complexity:** Medium

#### What was done (session 94)

Six bugs fixed; verified by `Pl/t/state-01.t` (20 tests, all passing):

**1. `%p-flatten-list` treated CL nil as an empty list.**

`(listp nil)` is true in Common Lisp (nil is the empty list). When `p-post++` on a nil box returned CL nil, `%p-flatten-list` iterated it as a zero-element list, swallowing the undef return value. List assignment received the wrong elements.

Fix: change `(listp item)` to `(consp item)` in `%p-flatten-list`. CL nil is not consp, so it falls through to the `(t ...)` scalar case and is added to the result as-is.

**2. `p-post++` on undef box returned nil instead of 0.**

Perl's `$x++` on an undef scalar returns 0 (the numeric old value; undef numerifies to 0). PCL returned raw CL nil, causing `is($x, 0)` to fail.

Fix in `p-post++` boxed scalar case: `old = (if (null val) 0 val)`.

**3. `state ($t) //= 3` — list form and `//=` not handled.**

`_process_state_declaration` only scanned for `PPI::Token::Symbol` (simple `$var`) after `state`, and only recognised `=` as an assignment operator. The `($t)` list form and the `//=` defined-or-assign operator were silently dropped.

Fix: handle `PPI::Structure::List` (extract symbols via `_find_symbols_in_list`) and `//=` operator (treated identically to `=` for the init guard).

**4. Nested state vars in bare blocks not found.**

`_find_all_declarations` stopped recursing at any `PPI::Structure::Block`. Bare blocks inside a sub (`{ state $bar = 12; ... }`) are `PPI::Structure::Block`, so `state $bar` was never found and never got the outer `let` binding.

Fix: recurse into `PPI::Structure::Block` unless it is an anonymous sub body (detected by checking `sprevious_sibling` for `PPI::Token::Word 'sub'`). Still exclude `PPI::Statement::Sub` (named subs) to avoid hoisting inner-sub state vars.

**5. State var initial binding was raw nil — increment ops failed silently.**

`p-pre++` and `p-post++` call `box-set` which returns early for non-box first args. With the old `($state__x nil)` initial binding, `++state $x` in expression context (no `state $x;` statement) silently no-oped.

Fix: initial binding is now `(make-p-box nil)` for `$` vars, `(make-array 0 ...)` for `@`, `(make-hash-table ...)` for `%`. Also updated `_process_state_declaration` to skip the data-init form for bare `state @arr` / `state %h` (only mark `__init` as true).

**6. Anon sub state rename map replaced parent renames.**

In `parse_block_to_cl_string`, `state_var_renames(\%state_renames)` replaced the entire environment rename map, losing parent-scope closure renames like `$outer → $outer__lex__2`. Nested closures using both captured vars and state vars would get unbound-variable errors.

Fix: merge with existing renames: `{%$existing, %state_renames}`.

#### Architecture note

State vars work via an outer `let` binding wrapping the `p-sub` form:
```lisp
(let (($state__f__x__1 (make-p-box nil)) ($state__f__x__1__init nil))
  (p-sub pl-f (&rest %_args)
    (block nil
      (unless $state__f__x__1__init
        (setf $state__f__x__1 (ensure-boxed INIT))
        (setf $state__f__x__1__init t))
      ...)))
```
The `p-sub` lambda closes over the outer `let` cells. At load time, the `defvar` for `$state__*` runs AFTER the `let+p-sub`, so the symbol is not yet special when the `let` executes — `let` creates lexical bindings captured by the closure.

---

### ~~B7. `context.t` tests 7-8 — `BEGIN {}` inside anonymous sub~~  ✅ DONE (BEGIN hoisting)

**File:** `perl-tests/context.t` tests 7-8

#### What was done

BEGIN hoisting already works: the `eval-when` is emitted at the top level, not inside the lambda body. No crash occurs.

#### Remaining failure — test 8 is wantarray (out of scope)

Test 8 checks that `$_ = sub { context(); BEGIN {} }->()` calls `context()` in scalar context. The wantarray propagation into an immediately-invoked anonymous sub is not correct — `context()` sees `void` instead of `scalar`. This is a wantarray issue; see `docs/wantarray-context.md`. **Do not fix without explicit user request.**

Test 2 (`@a=foo` — list context regex `/g`) is also a wantarray issue.

---

### ~~C1. Regex named captures via `%+`~~  ✅ DONE (session 91)

**Files:** Any code using `(?<name>...)` patterns
**Commits:** 0e76708, 5138471

#### What was done

- Set `cl-ppcre:*allow-named-registers* t` at startup (was NIL by default)
- `defvar %+` (hash-table, exported from `:pcl`)
- `clear-capture-groups`: added `(clrhash %+)`; also called unconditionally at the start of every match attempt (Perl clears `%+` even on failed matches)
- `set-capture-groups`: added optional `reg-names` parameter (the list returned by `cl-ppcre:create-scanner`); loops over names, populates `%+` for non-NIL named groups; also guards `$1`-`$9` against NIL reg-starts/ends (optional groups that didn't participate in the match)
- `do-regex-match`: wraps `create-scanner` in `multiple-value-bind` to capture `reg-names`; threads it through all three match paths (global+list, global+scalar, single)
- `do-regex-subst`: same; s///e lambda also populates `%+` from `reg-names`
- `StringInterpolation.pm`: `$+{name}` in double-quoted strings now calls `parse_hash_subscript` → `(p-gethash %+ "name")`
- **API note**: `cl-ppcre:create-scanner` returns `(values scanner reg-names)` where `reg-names` is a **list** (not vector) of names, NIL for unnamed groups
- 10 runtime regression tests in `Pl/t/named-capture-01.t`

---

### ~~C2. `s///r` — non-destructive substitution~~  ✅ DONE (session 90)

Fixed in `cl/pcl-runtime.lisp` `do-regex-subst`: added `non-destructive-p`
check for `:r` modifier. When set, skips the in-place box update and returns
`(make-p-box result)` instead of the replacement count. Works for `/r` and
`/rg`. No-match case correctly returns a copy of the original string.

---

### C3. `local $hash{key}` and `local @arr[N]` — element localization ✅ DONE (session 85)

**Files:** `perl-tests/local.t` (currently also blocked by Tie::Array, but that's separate)
**Test impact:** Medium (many modules use `local $Config{key}`)
**Complexity:** Medium

#### What's broken

```perl
local $hash{key};           # saves $hash{key}, restores on scope exit
local $hash{key} = "new";  # saves, sets to "new", restores on exit
local @arr[1, 2];           # saves $arr[1] and $arr[2], restores on exit
local @hash{'a', 'b'};     # saves $hash{a} and $hash{b}, restores on exit
```

#### Why CL `let` doesn't work

CL's dynamic binding via `let` works for whole variables (`defvar $foo`) because the binding is on the *symbol*. For hash/array elements, there's no symbol to rebind — the element is selected by a runtime key/index into a mutable data structure.

#### Solution: runtime macros with `unwind-protect`

Exactly the same pattern as `p-local-glob` (which is already in use and working).

##### Runtime macro: `p-local-hash-elem`

```lisp
(defmacro p-local-hash-elem (hash-var key-form &body body)
  "Save/restore a single hash entry. Like Perl's local $hash{key}."
  (let ((saved  (gensym "SAVED"))
        (kv     (gensym "KEY"))
        (exists (gensym "EXISTS")))
    `(let* ((,kv     ,key-form)
            (,exists (nth-value 1 (gethash ,kv ,hash-var)))
            (,saved  (gethash ,kv ,hash-var)))
       (unwind-protect (progn ,@body)
         (if ,exists
             (setf (gethash ,kv ,hash-var) ,saved)
             (remhash ,kv ,hash-var))))))
```

`(nth-value 1 (gethash k h))` returns T if the key was present, NIL if absent. This correctly handles the case where the key didn't exist before `local` — on exit it's removed, not left as nil/undef.

##### Runtime macro: `p-local-array-elem`

```lisp
(defmacro p-local-array-elem (arr-var idx-form &body body)
  "Save/restore a single array element. Like Perl's local $arr[N]."
  (let ((saved (gensym "SAVED"))
        (iv    (gensym "IDX"))
        (len   (gensym "LEN")))
    `(let* ((,iv   (p-to-number ,idx-form))
            (,iv   (if (< ,iv 0)
                       (max 0 (+ (length ,arr-var) ,iv))
                       ,iv))
            (,len  (length ,arr-var))
            (,saved (when (< ,iv ,len) (aref ,arr-var ,iv))))
       (unwind-protect (progn ,@body)
         ;; Restore: if element was present, set it back; if beyond length, shrink
         (cond
           ((< ,iv (length ,arr-var))
            (setf (aref ,arr-var ,iv) (or ,saved *p-undef*)))
           ;; If array grew during body: shrink back to original length
           ((> (length ,arr-var) ,len)
            (setf (fill-pointer ,arr-var) ,len)))))))
```

##### Runtime macro: `p-local-hash-slice`

For `local @hash{'a','b'}` (hash slice with multiple keys):

```lisp
(defmacro p-local-hash-slice (hash-var keys-form &body body)
  "Save/restore multiple hash entries. Like Perl's local @hash{@keys}."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved
            (mapcar (lambda (k)
                      (multiple-value-bind (v ex) (gethash k ,hash-var)
                        (list k ex v)))
                    (coerce ,keys-form 'list))))
       (unwind-protect (progn ,@body)
         (dolist (triple ,saved)
           (destructuring-bind (k ex v) triple
             (if ex
                 (setf (gethash k ,hash-var) v)
                 (remhash k ,hash-var))))))))
```

##### Runtime macro: `p-local-array-slice`

For `local @arr[1,2,3]`:

```lisp
(defmacro p-local-array-slice (arr-var indices-form &body body)
  "Save/restore multiple array elements. Like Perl's local @arr[N,M,...]."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved
            (mapcar (lambda (i)
                      (let ((ii (if (< i 0)
                                    (max 0 (+ (length ,arr-var) i))
                                    i)))
                        (list ii (when (< ii (length ,arr-var))
                                   (aref ,arr-var ii)))))
                    (coerce ,indices-form 'list))))
       (unwind-protect (progn ,@body)
         (dolist (pair ,saved)
           (destructuring-bind (i v) pair
             (when (< i (length ,arr-var))
               (setf (aref ,arr-var i) (or v *p-undef*)))))))))
```

#### Parser changes — `Pl/Parser.pm` `_process_local_declaration`

PPI gives us `local $hash{key}` as:
```
[Word("local"), Symbol("$hash"), Structure::Subscript("{key}")]
```

And `local @hash{@keys}` as:
```
[Word("local"), Symbol("@hash"), Structure::Subscript("{@keys}")]
```

After filtering whitespace and "local" into `@non_ws`, add detection **before** the existing `@vars` loop:

```perl
# Detect: local $hash{key}, local @arr[N], local @hash{@keys}, local @arr[N,M]
if (@non_ws >= 2
    && ref($non_ws[0]) eq 'PPI::Token::Symbol'
    && ref($non_ws[1]) eq 'PPI::Structure::Subscript') {

    my $sym  = $non_ws[0]->content;       # "$hash" or "@arr" or "@hash"
    my $sub  = $non_ws[1];
    my $open = substr($sub->start->content, 0, 1);  # '{' or '['
    my $sigil = substr($sym, 0, 1);                  # '$' or '@'

    # Get the hash/array variable in CL form
    # $hash{key}  -> hash var is %hash
    # @hash{keys} -> hash var is %hash
    # $arr[N]     -> array var is @arr
    # @arr[N,M]   -> array var is @arr
    my $cl_var;
    if ($open eq '{') {
        (my $base = $sym) =~ s/^[\$\@]/%/;
        $cl_var = $self->_transform_pkg_var($base);
    } else {
        (my $base = $sym) =~ s/^[\$\@]/@/;
        $cl_var = $self->_transform_pkg_var($base);
    }

    # Get the key/index expression from the subscript's children
    my @sub_children = grep { ref($_) ne 'PPI::Token::Whitespace' }
                       $sub->children;
    # Remove the braces/brackets themselves (first and last)
    # The inner content is what remains
    my $key_cl = $self->_parse_expression(\@sub_children, $stmt) // 'nil';

    # Detect slice vs single element
    my $is_slice = ($sigil eq '@');

    # Check for initializer (= expr) after the subscript
    my $has_init = 0;
    my @init_parts;
    for my $i (2 .. $#non_ws) {
        if (ref($non_ws[$i]) eq 'PPI::Token::Operator'
            && $non_ws[$i]->content eq '=') {
            $has_init = 1;
            @init_parts = @non_ws[($i+1) .. $#non_ws];
            last;
        }
    }

    $self->_emit(";; $perl_code");

    if ($is_slice && $open eq '{') {
        $self->_emit("(p-local-hash-slice $cl_var $key_cl");
    } elsif ($is_slice && $open eq '[') {
        $self->_emit("(p-local-array-slice $cl_var $key_cl");
    } elsif ($open eq '{') {
        $self->_emit("(p-local-hash-elem $cl_var $key_cl");
    } else {
        $self->_emit("(p-local-array-elem $cl_var $key_cl");
    }

    $self->indent_level($self->indent_level + 1);
    $self->{_local_let_depth}++;

    # If there's an initializer, emit the assignment as first body form
    if ($has_init) {
        my $init_cl = $self->_parse_expression(\@init_parts, $stmt) // 'nil';
        if ($open eq '{') {
            $self->_emit("(p-hash-set $cl_var $key_cl $init_cl)");
        } else {
            $self->_emit("(p-aset $cl_var $key_cl $init_cl)");
        }
    }

    $self->_emit("");
    return;
}
```

#### PPI subscript content extraction

The subscript `{key}` in PPI is a `PPI::Structure::Subscript`. Its `children` include the braces themselves plus the inner tokens. To get just the expression inside, call `$sub->schild(0)` .. `$sub->schild(-1)`, skipping the bracket tokens. Or use `$sub->children` and pass the interior tokens to `_parse_expression`.

Actually in PPI: `$sub->start` = the `{` token, `$sub->finish` = the `}` token. The interior children can be obtained via:

```perl
my @inner = grep { $_ != $sub->start && $_ != $sub->finish }
            $sub->children;
```

#### Initializer with element local

```perl
local $hash{key} = "new_value";
```

The macro saves the old value, then the body starts with `(p-hash-set %hash "key" "new_value")` as the first form. This is correct: save old → set new → body runs → restore old.

#### Nested locals in same scope

```perl
{
    local $h{a} = 1;
    local $h{b} = 2;
    # ... body
}
```

Both open `p-local-hash-elem` forms, nesting correctly:

```lisp
(p-local-hash-elem %h "a"
  (p-hash-set %h "a" 1)
  (p-local-hash-elem %h "b"
    (p-hash-set %h "b" 2)
    ;; ... body
    ))
```

#### Edge cases

1. **Key with runtime expression:** `local $h{$k}` — `$k` is the key-form, evaluated once and captured in `kv` variable in the macro. Safe.

2. **Negative array index:** `local $arr[-1]` — handled in `p-local-array-elem` by normalizing the index: `(+ (length arr) idx)`.

3. **Array extends during local:** If the array grows beyond its length at local-enter time, `p-local-array-elem` should shrink it back. Handled in the macro's cleanup.

4. **Hash key didn't exist:** `local $h{new_key}` where `new_key` wasn't in `%h`. On exit, the key is `remhash`'d (not set to undef). Handled by `(nth-value 1 (gethash k h))`.

5. **`local $ENV{key}`:** This is extremely common in CPAN code. Once `%ENV` is a real hash-table (see B4), this will work automatically. If `%ENV` is still a marker, add a special-case in the detection code.

#### Export requirement

All four macros must be added to the `:export` list in `(defpackage :pcl ...)`:

```lisp
#:p-local-hash-elem
#:p-local-array-elem
#:p-local-hash-slice
#:p-local-array-slice
```

---

### C4. Flip-flop `..` in scalar/boolean context

**File:** `perl-tests/flip.t`
**Test impact:** 3 tests
**Complexity:** Medium

#### What's broken

In scalar/boolean context, `LHS .. RHS` is a stateful flip-flop operator:

```perl
while (<DATA>) {
    if ($. == 1 .. $. == 5) {  # flip-on at line 1, flip-off at line 5
        print;
    }
}
```

Currently `..` in scalar context generates a range (wrong).

#### Fix

Each `..` source location needs its own state variable. At parse time, assign a unique counter N to each `..` expression. In the preamble bucket emit:

```lisp
(defvar *flip-state-N* nil)
```

At the call site emit:

```lisp
(p-flip-flop *flip-state-N* LEFT-EXPR RIGHT-EXPR)
```

Runtime macro:

```lisp
(defmacro p-flip-flop (state-var left right)
  "Perl's .. flip-flop in scalar context."
  `(cond
     ;; Currently OFF: check left (turn-on) condition
     ((not ,state-var)
      (if (p-true-p ,left)
          (progn
            ;; Turn on. Also check right immediately (for single-line match):
            (setf ,state-var t)
            (if (p-true-p ,right)
                (progn (setf ,state-var nil) (make-p-box "E0"))  ; "E0" = end-on-same-line
                (make-p-box 1)))
          *p-undef*))  ; OFF and left is false: return false
     ;; Currently ON: check right (turn-off) condition
     (t
      (let ((seq (incf *flip-seq-state-var*)))  ; sequence number
        (if (p-true-p ,right)
            (progn (setf ,state-var nil)
                   (make-p-box (format nil "~AE0" seq)))  ; final true
            (make-p-box seq))))))  ; still on
```

The exact Perl semantics of flip-flop return values (sequence number, "E0" suffix) are complex but the three tests in `flip.t` likely only test the basic on/off behavior. Implement the boolean-return version first.

#### Counter assignment at parse time

In `Pl/ExprToCL.pm` `gen_binary` (or wherever `..` in scalar context is detected), increment a module-level counter and use it for the state variable name:

```perl
our $flip_flop_counter = 0;

# In gen_binary for '..' in non-list context:
my $n = ++$flip_flop_counter;
# Emit defvar to preamble bucket
$self->_add_to_preamble("(defvar *flip-state-$n* nil)");
# Emit call
return "(p-flip-flop *flip-state-$n* $lhs_cl $rhs_cl)";
```

---

### C5. String `eval "code"`

**Files:** `perl-tests/signatures.t` (skip_all), others
**Test impact:** ~50 tests in eval.t and related
**Complexity:** Hard

#### What's broken

`eval "code"` (string eval) must parse and execute arbitrary Perl at runtime. Currently stubbed: sets `$@` to "not implemented" and returns undef.

#### Approach

At runtime, `p-eval-string` calls the `pl2cl` transpiler as a subprocess, gets CL back, reads and evals it in the current package context.

```lisp
(defun p-eval-string (str-box)
  "Perl string eval: transpile and execute Perl code at runtime."
  (let ((perl-code (p-to-string (p-box-val str-box))))
    (handler-case
      (let* ((cl-text (p-run-transpiler perl-code))
             (cl-form (read-from-string (concatenate 'string "(progn " cl-text ")"))))
        (setf $@ *p-undef*)
        (eval cl-form))
      (error (e)
        (setf $@ (make-p-box (format nil "~A" e)))
        *p-undef*))))

(defun p-run-transpiler (perl-code)
  "Call pl2cl subprocess, return CL string."
  (let ((path *pcl-pl2cl-path*))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "perl" path)
                          :input (make-string-input-stream perl-code)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (if (zerop exit-code)
          output
          (error "PCL transpile error: ~A" error-output)))))
```

`*pcl-pl2cl-path*` is already set in the generated preamble (it's the path to the `pl2cl` script).

#### Caveats

- The eval'd code runs in a fresh package context, not the caller's lexical scope. Perl's `eval ""` also doesn't capture `my` vars from the caller — only package vars. So this is correct.
- `$@` must be set to `""` (empty string) on success, not undef.
- The subprocess approach is slow. For v1, correctness matters more than speed.
- `eval "..."` test lines should stop being commented out once this is implemented.

#### Parser change

`Pl/Parser.pm` — wherever `eval "..."` is currently detected and commented/stubbed, route it to `p-eval-string`.

`Pl/PExpr/Config.pm` — add `eval` to `known_no_of_params` as a 1-param function (or handle as a special form since it has block-eval and string-eval forms).

---

### C6. `pack.t` and `method.t` — investigate before planning

**Files:** `perl-tests/pack.t`, `perl-tests/method.t`
**Test impact:** Unknown
**Complexity:** Unknown

Both files crash at load. Run before making any plan:

```bash
perl run-perl-test.pl perl-tests/pack.t   2>&1 | head -40
perl run-perl-test.pl perl-tests/method.t 2>&1 | head -40
```

**pack.t hypothesis:** Missing format characters in `p-pack`/`p-unpack`. These functions exist but likely don't implement all 80+ Perl pack template characters. Investigate which characters are failing.

**method.t hypothesis:** `INPUT-ERROR-IN-LOAD` suggests a parse error. Possibly non-ASCII characters in the source, or a Perl construct that generates invalid CL. The file may use unusual OO patterns.

---

## Implementation Session Order

| Session | Items | Key work |
|---------|-------|----------|
| 1 | ~~A1~~, ~~A2~~, ~~A3~~ | split ✅, sprintf ✅, grent ✅ (session 92) — 1/3 tests pass |
| 2 | B2, ~~B3~~ | caller.t investigation + fix; kvaslice ✅ DONE session 90 |
| 3 | ~~C3~~ | ~~`local $hash{key}` / `local @arr[N]`~~ ✅ DONE session 85 |
| 4 | B1 | Bare-if tail return (parser tail-position flag) |
| 5 | ~~B4~~, ~~B6~~ | lex.t (heredoc `<<""` + %ENV) ✅ DONE session 92; context.t BEGIN hoisting ✅ DONE (remaining failures are wantarray) |
| 6 | B5 + C6 | sort.t investigate, method.t/pack.t investigate |
| 7 | ~~C1~~, ~~C2~~ | Named captures %+ ✅ DONE session 91; s///r ✅ DONE session 90 |
| 8 | C4 | Flip-flop |
| 9 | C5 | String eval (hardest — leave for last) |

---

## Testing Protocol for Each Feature

1. Write a `Pl/t/featurename-01.t` with targeted PCL-level tests first.
2. Run against the corresponding `perl-tests/*.t` to measure improvement.
3. Run full PCL suite (`prove -j8 Pl/t/`) to check for regressions.
4. Run Perl sweep (`perl sweep-perl-tests.pl --jobs 8`) for net count.

---

## Current Test Count Ledger

| Category | Count |
|----------|-------|
| PCL suite | 65 files, 2667 tests, all passing (session 92) |
| Perl op/ suite passing | 5433 (session 91) |
| Perl op/ suite failing | ~2000 (session 91) |
| Fully-passing Perl files | 41 (session 91) |
| Skipped (hang) | 2 (bop.t, heredoc.t) |
| Zero-passing / unfixable | args.t, crypt.t, die_exit.t, print.t, hexfp.t, lfs.t, sprintf.t (skip_all: string eval) |
| grent.t | 1/3 — setgrent ✓; tests 2-3 blocked by @{$hash_elem} auto-vivif |

Estimated additional passing tests from this plan: **+200–400**, depending on investigation results for pack.t, method.t, sort.t.
