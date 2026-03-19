# PCL v1 Implementation Plan

**Written:** 2026-03-19
**Status:** 4869 passing / 962 failing in Perl op/ suite. 41 fully-passing files.
**Goal:** Push the highest-value fixable failures to zero while keeping all 2590 PCL tests green.

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

### A1. `split(/$x/, ...)` — regex variable interpolation

**File:** `perl-tests/split.t` test 73
**Test impact:** 1 test
**Complexity:** Easy

#### What's broken

`split(/$x/, $str)` compiles `/$x/` as a literal pattern matching the two-character string `$x`, instead of interpolating the value of `$x` as the pattern.

#### Root cause

In `Pl/ExprToCL.pm` `gen_leaf`, `PPI::Token::Regexp::Match` tokens are passed directly to `p-regex` as literal strings. No interpolation is done on the pattern body.

#### Fix area

`Pl/ExprToCL.pm` — `gen_leaf` where `$ref eq 'PPI::Token::Regexp::Match'`.

After extracting the pattern string from the token, check whether it contains `$varname` or `@arrayname` references. If so, run it through `Pl::StringInterpolation` (already used for `"..."` strings) to produce a CL string-building expression, then wrap in `(p-make-regex ...)` instead of `(p-regex "literal")`.

#### Generated code

```perl
split(/$x/, $str)
```

Currently generates (wrong):
```lisp
(p-split (p-regex "$x") $str ...)   ; matches literal "$x"
```

Should generate:
```lisp
(p-split (p-regex (concatenate 'string (p-to-string $x))) $str ...)
```

Or more precisely, whatever `_interpolate_string` produces for `"$x"` (which handles boxing/unboxing correctly).

#### Runtime side

`p-regex` already accepts a string and compiles it. No runtime changes needed — just pass the dynamically-built string.

#### Edge cases

- `/$x/i` — modifiers must be preserved; extract them before interpolation, reattach after.
- `/$x$y/` — multiple variables: `StringInterpolation` handles this.
- `/${\expr}/` — complex interpolation: same path.

---

### A2. `sprintf "%53.0f"` — trailing dot with precision 0

**File:** `perl-tests/sprintf.t` test (1 failure out of 2830)
**Test impact:** 1 test
**Complexity:** Trivial

#### What's broken

`sprintf("%53.0f", 0)` returns `"                                                    0."` (trailing dot) instead of `"                                                    0"`.

#### Root cause

In `cl/pcl-runtime.lisp` `p-sprintf`, the `%f` handler formats with CL's `format` directive `~,Nf` where N=0. When precision is 0, CL prints `0.` — it always emits the decimal point regardless of precision.

#### Fix area

`cl/pcl-runtime.lisp` — `p-sprintf` float case. After formatting, strip a trailing `.` when the format precision is 0 and the result ends with `.`.

```lisp
;; After producing the float string s:
(when (and (zerop precision) (char= (char s (1- (length s))) #\.))
  (setf s (subseq s 0 (1- (length s)))))
```

---

### A3. `grent.t` — stub POSIX user/group database functions

**File:** `perl-tests/grent.t`
**Test impact:** Unblocks entire file (count unknown — needs investigation)
**Complexity:** Easy–Medium

#### What's broken

`grent.t` calls `getgrent`, `setgrent`, `endgrent` (and maybe `getpwent`, `setpwent`, `endpwent`). These are undefined in PCL.

#### Fix area

`cl/pcl-runtime.lisp` — add stubs.
`Pl/PExpr/Config.pm` `%known_no_of_params` — register them.

SBCL has `sb-posix:getgrent` returning a group struct. We can expose these or return `undef` for a minimal stub.

#### Minimal stub approach

```lisp
(defun p-getgrent ()
  "Stub: return undef (no real group DB access)."
  *p-undef*)

(defun p-setgrent () *p-undef*)
(defun p-endgrent () *p-undef*)
```

#### Full implementation

Use `sb-posix:getgrent` which returns a `sb-posix:group` struct. Extract fields via `sb-posix:group-name`, `sb-posix:group-passwd`, `sb-posix:group-gid`, `sb-posix:group-mem`. Return as a CL vector (list context) or group name (scalar context), checking `*wantarray*`.

```lisp
(defun p-getgrent ()
  (handler-case
    (let ((g (sb-posix:getgrent)))
      (if *wantarray*
          (vector (make-p-box (sb-posix:group-name g))
                  (make-p-box (sb-posix:group-passwd g))
                  (make-p-box (sb-posix:group-gid g))
                  (make-p-box (sb-posix:group-mem g)))
          (make-p-box (sb-posix:group-name g))))
    (sb-posix:syscall-error () *p-undef*)))
```

Similarly for `getpwent`, `getpwnam`, `getpwuid`.

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

### B2. `caller.t` — UNBOUND-VARIABLE crash at startup

**File:** `perl-tests/caller.t`
**Test impact:** Unblocks entire file (count unknown)
**Complexity:** Medium

#### What's broken

`caller.t` crashes with `UNBOUND-VARIABLE` before running any tests. The variable name is unknown without investigation.

#### Likely root cause

`$Pkg::var` forward declaration issue: when code references `$Dog::VERSION` (or similar package-qualified variable) before a `package Dog` block appears, PCL emits `(defpackage :Dog ...)` but the `(defvar $VERSION ...)` runs in the wrong package context.

#### Investigation step

```bash
perl run-perl-test.pl perl-tests/caller.t 2>&1 | head -40
```

Look for the unbound variable name and which package it should be in.

#### Fix area

`Pl/ExprToCL.pm` `gen_leaf` for package-qualified variables (`$Pkg::var`). When emitting access to a cross-package variable, also emit a `(defvar Pkg::$var (make-p-box nil))` in the **preamble bucket**, guarded so it doesn't clobber existing bindings.

The preamble bucket trick: PCL already uses output buckets. Add a new entry in the preamble bucket for each `$Pkg::var` reference seen, using a `defvar` that only fires if the variable isn't already bound:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '|Dog|::$VERSION)
    (defvar |Dog|::$VERSION (make-p-box nil))))
```

Track which `$Pkg::var` combinations have already been emitted to avoid duplicates.

#### What `pl-caller` currently returns

`pl-caller` returns package name only; filename and line number are always 0. This is documented as a known limitation. Do NOT try to fix filename/line for v1.

---

### B3. `kvaslice.t` repeated keys

**File:** `perl-tests/kvaslice.t` tests 2-7
**Test impact:** ~6 tests
**Complexity:** Easy–Medium

#### What's broken

```perl
my @r = %arr{1, 1, 2};   # should give (1, $arr[1], 1, $arr[1], 2, $arr[2])
```

Repeated keys in `%arr{@keys}` should repeat in the output (key, value) pairs. Currently PCL deduplicates or returns wrong results.

#### Fix area

`cl/pcl-runtime.lisp` — `p-kvaslice-array` (or equivalent function for key-value array slices). Find the function, check whether it deduplicates. If it iterates over a hash or deduplicated structure, change it to iterate over the raw key list.

Also check test 3: "last element in scalar context" — `scalar(%arr{1,2})` should return the count of keys, or the last value.

---

### B4. `lex.t` regressions

**File:** `perl-tests/lex.t` (test 2 + crash at test 41)
**Test impact:** ~2 tests unblocked
**Complexity:** Medium

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

### B5. `sort.t` — TYPE-ERROR after Tie fix

**File:** `perl-tests/sort.t`
**Test impact:** Unknown — needs investigation
**Complexity:** Medium

#### What's broken

`sort.t` uses `Tie::StdArray`. The module now loads (Tie::Array hang was fixed in session 69), but a TYPE-ERROR occurs. Likely: `Tie::StdArray::SPLICE` calls `$self->PUSH(...)` etc., which dispatches to methods that expect tied array semantics. There may be a circular dispatch or a wrong type being passed.

#### Investigation step

```bash
perl run-perl-test.pl perl-tests/sort.t 2>&1 | head -60
```

Look for the exact TYPE-ERROR (what type was expected vs. what was received) and which Tie method triggered it.

---

### B6. `context.t` tests 7-8 — `BEGIN {}` inside anonymous sub

**File:** `perl-tests/context.t` tests 7-8
**Test impact:** 2 tests
**Complexity:** Medium

#### What's broken

```perl
$_ = sub { context(); BEGIN { } }->()
```

The `BEGIN {}` block inside the anonymous sub body is emitted as an `(eval-when ...)` form that ends up as an argument to `p-funcall-ref` instead of being hoisted out before the lambda definition. This causes a crash.

#### Root cause

In `Pl/Parser.pm`, `_process_begin_statement` unconditionally emits into the current block's code stream. When `in_subroutine > 0`, this puts the `eval-when` inside the lambda body, where it becomes an expression (not a top-level form).

#### Fix area

`Pl/Parser.pm` — `_process_element` or `_process_begin_statement`. When `in_subroutine > 0` and a `BEGIN {}` is encountered:

1. Process the BEGIN body into a temporary string.
2. Emit it into the **definitions bucket** (which runs at compile time) rather than into the current function body.
3. In the function body's code position, emit nothing (or a comment).

This matches what Perl does: `BEGIN` blocks run at compile time regardless of where they appear syntactically.

---

### C1. Regex named captures via `%+`

**Files:** Any code using `(?<name>...)` patterns
**Test impact:** Small (tests that check `%+`)
**Complexity:** Easy–Medium

#### What's broken

After a match like `"foo" =~ /(?<word>\w+)/`, Perl populates `%+` with `{word => "foo"}`. PCL sets `$1`, `$2`, etc. but does not populate `%+` or `%-`.

#### Fix area

`cl/pcl-runtime.lisp` — `do-regex-match` (or the equivalent function that runs matches and sets `$1` etc.).

`cl-ppcre:scan` with `:named-registers t` returns an additional value — a hash of named captures. After a successful match:

```lisp
;; After cl-ppcre:scan returns match:
(multiple-value-bind (start end reg-starts reg-ends named)
    (cl-ppcre:scan scanner subject :named-registers t)
  (when start
    ;; Set $1, $2, ... (already done)
    ;; Set %+:
    (clrhash %+)
    (when named
      (maphash (lambda (name idx)
                 (let ((rs (aref reg-starts idx))
                       (re (aref reg-ends   idx)))
                   (when rs
                     (setf (gethash name %+)
                           (make-p-box (subseq subject rs re))))))
               named))))
```

`%+` must be declared as a `defvar` in the pcl package and exported, just like `$1`, `$2`, etc.

#### Named register API note

`cl-ppcre:create-scanner` returns a named-register hash as its second return value (the register names mapped to indices). Check the CL-PPCRE docs for the exact API — it may be `:named-registers` in `scan` or it may come from the scanner object.

---

### C2. `s///r` — non-destructive substitution

**Files:** Any code using `s///r`
**Test impact:** Small
**Complexity:** Easy

#### What's broken

`my $new = $str =~ s/foo/bar/r` should leave `$str` unchanged and return the modified copy. Current behavior unknown — needs verification.

#### Fix area

`Pl/ExprToCL.pm` — regex codegen for `s///`. When the `/r` modifier is present:

1. Do not pass the source variable as a mutable reference.
2. Return the modified copy.

In the codegen, `s///` currently generates something like `(p-s-replace VAR pattern replacement flags)` which modifies VAR in place. With `/r`:

```lisp
;; Without /r (modifies in place, returns count):
(p-s-replace $str "foo" "bar" "")

;; With /r (returns copy, $str unchanged):
(p-s-replace-copy $str "foo" "bar" "")
```

`p-s-replace-copy` in the runtime:

```lisp
(defun p-s-replace-copy (box pattern replacement flags)
  "Non-destructive s///r — return modified copy without changing original."
  (let* ((str (p-to-string (p-box-val box)))
         (result (p-s-replace-string str pattern replacement flags)))
    (make-p-box result)))
```

where `p-s-replace-string` is the pure-string version of the substitution logic extracted from `p-s-replace`.

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
| 1 | A1, A2, A3 | split regex interpolation, sprintf dot, grent stubs |
| 2 | B2, B3 | caller.t investigation + fix, kvaslice repeated keys |
| 3 | C3 | `local $hash{key}` / `local @arr[N]` (runtime macros + parser) |
| 4 | B1 | Bare-if tail return (parser tail-position flag) |
| 5 | B4, B6 | lex.t (heredoc `<<""` + %ENV), context.t BEGIN hoisting |
| 6 | B5 + C6 | sort.t investigate, method.t/pack.t investigate |
| 7 | C1, C2 | Named captures %+, s///r |
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
| PCL suite | 60 files, 2590 tests, all passing |
| Perl op/ suite passing | 4869 |
| Perl op/ suite failing | 962 |
| Fully-passing Perl files | 41 |
| Skipped (hang) | 2 (bop.t, heredoc.t) |
| Zero-passing / unfixable | args.t, crypt.t, die_exit.t, print.t, hexfp.t, lfs.t |

Estimated additional passing tests from this plan: **+200–400**, depending on investigation results for pack.t, method.t, sort.t.
