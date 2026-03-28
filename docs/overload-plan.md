# PCL: `use overload` — What It Is and How to Implement It

**Written:** 2026-03-28
**Priority:** High — `use overload` is likely the single highest-ROI feature remaining.
**Estimated tests unblocked:** ~1100 (sprintf2.t 983, sort.t ~52, concat2.t 2, hashassign.t 10, others)

---

## What `use overload` Is

`use overload` lets a Perl class define what happens when its objects are used in operator
expressions.  Without it, `$obj + 1` just stringifies the object address; with it, the
class's `+` handler is called instead.

```perl
package MyNum;
use overload
    '+'   => \&add,       # $a + $b
    '""'  => \&stringify, # "$obj"  or  print $obj
    '0+'  => \&numify,    # $obj + 0  (force numeric)
    '<=>' => \&compare,   # sort, numeric comparison
    'cmp' => \&str_cmp;   # string comparison

sub new       { bless { val => $_[1] }, $_[0] }
sub add       { MyNum->new($_[0]{val} + $_[1]{val}) }
sub stringify { $_[0]{val} }          # called by print, sprintf %s, etc.
sub numify    { $_[0]{val} }          # called by arithmetic
sub compare   { $_[0]{val} <=> $_[1]{val} }
```

### Handler signature

Every overload handler receives **three arguments**: `($self, $other, $reversed)`.

- `$self`  — the object that has the overloading
- `$other` — the other operand (may be a plain number/string)
- `$reversed` — true when the object was the *right* operand (so Perl swapped the args)

```perl
# $obj + 1   → handler($obj, 1,    0)  # normal
# 1 + $obj   → handler($obj, 1,    1)  # reversed (Perl detected: left has no '+', right does)
```

Unary operators (neg, `!`, `~`) pass `undef` as `$other` and 0 as `$reversed`.

### The `fallback` option

```perl
use overload fallback => 1, '+' => \&add;
```

- `fallback => undef` (default) — use normal Perl coercion (stringify/numify) if op not defined
- `fallback => 1`    — autogenerate missing ops from available ones (`+=` from `+` etc.)
- `fallback => 0`    — die on any undefined op

For PCL v1, we implement **`fallback => undef`** (default) behaviour only: if an op isn't
registered, fall through to normal string/number coercion.  `fallback => 1` autogeneration
is a stretch goal.

### Most important operators (by test-suite frequency)

| Op key | Called when |
|--------|-------------|
| `""`   | Stringification: `"$obj"`, `print $obj`, `sprintf "%s"`, string concat |
| `0+`   | Numification: `$obj + 0`, arithmetic, `sprintf "%d/%f"` |
| `+` `-` `*` `/` `%` `**` | Arithmetic |
| `neg`  | Unary minus `-$obj` |
| `<=>` `cmp` | Numeric/string comparison, sort |
| `<` `>` `==` `!=` `<=` `>=` | Numeric comparison operators |
| `lt` `gt` `eq` `ne` `le` `ge` | String comparison operators |
| `.`    | String concatenation `$obj . "x"` |
| `bool` | Boolean context `if ($obj)` |
| `=`    | Copy on assignment (rarely needed) |
| `<<` `>>` `&` `\|` `^` `~` | Bitwise ops |

---

## Why It Looks Dangerous

The danger is real — here's what makes it hard:

1. **`stringify-value` / `to-string` are called everywhere.** Adding an overload check
   there adds overhead to every print, every comparison, every hash key lookup.

2. **Recursive overloading.** If the `""` handler calls `print`, which calls `stringify`,
   which calls the `""` handler again → infinite loop.  Perl guards against this with a
   re-entrancy flag per object.

3. **Reversed-operand logic.**  `1 + $obj` must swap args and set `$reversed=1`.  Getting
   this wrong silently produces wrong results.

4. **`fallback` interaction.** If `fallback => 1` and you defined `+` but not `+=`, Perl
   calls `+` for `+=`.  Getting fallback wrong breaks lots of things.

5. **Mutation operators.** `$obj += 1` modifies `$obj` in place only if the `+=` handler
   (or `+` with fallback) returns a new object AND that object is assigned back.  PCL's
   `+=` code path needs to handle this.

**Mitigation strategy:** guard every overload check behind a fast `(p-blessed-p v)` test.
The common case (plain number/string) never pays the dispatch cost.  The slow path
(blessed object) checks the table.

---

## Implementation Plan

### Step 1 — Overload registry in the runtime

Add to `cl/pcl-runtime.lisp` near the other package-level tables (around line 260):

```lisp
;;; Operator overload table: maps (pkg-name . op-string) -> handler box
(defvar *p-overload-table* (make-hash-table :test 'equal)
  "use overload registry: (cons pkg op) -> handler-function box")

;;; Fallback flags: maps pkg-name -> fallback value (nil / t / :undef)
(defvar *p-overload-fallback* (make-hash-table :test 'equal))
```

Add registration and lookup helpers:

```lisp
(defun p-register-overloads (pkg &rest pairs)
  "Called by generated code for `use overload OP => HANDLER, ...`"
  ;; pairs is a flat list: op1 fn1 op2 fn2 ...
  (loop for (op fn) on pairs by #'cddr
        for op-str = (to-string (unbox op))
        do (if (string= op-str "fallback")
               (setf (gethash pkg *p-overload-fallback*)
                     (let ((v (unbox fn)))
                       (cond ((null v) :undef)
                             ((p-true-p fn) t)
                             (t nil))))
               (setf (gethash (cons pkg op-str) *p-overload-table*) fn))))

(defun p-find-overload (val op-str)
  "Return the overload handler box for val's class and op, or nil."
  (when (p-box-p val)
    (let ((cls (p-get-class val)))
      (when cls
        (gethash (cons cls op-str) *p-overload-table*)))))

(defun p-call-overload (handler self other reversedp)
  "Invoke an overload handler: handler(self, other, reversed)"
  (let ((fn (unbox handler)))
    (cond
      ((functionp fn)
       (funcall fn self (or other *p-undef*) (if reversedp *p-true* *p-false*)))
      ((p-box-p fn)
       (p-method-call self (to-string (unbox fn))))
      (t (error "Bad overload handler: ~S" fn)))))
```

Export from `:pcl` package: `p-register-overloads`, `p-find-overload`, `p-call-overload`.

### Step 2 — Parse `use overload` in Parser.pm

In `Pl/Parser.pm`, `_process_use_statement`, add a case for `overload`:

```perl
if ($module eq 'overload') {
    # Collect the argument list as a flat expression
    my $args_cl = $self->_process_args_as_list($use_stmt);
    $self->_emit("(p-register-overloads (package-name *package*) $args_cl)");
    return;
}
```

The argument list `'+' => \&add, '""' => \&stringify` is just a flat list of alternating
string keys and code refs, which PCL already handles via its normal expression parser.
`\&add` generates `(make-p-box #'pl-add)` — exactly what `p-call-overload` expects.

### Step 3 — Stringification (highest impact)

Modify `stringify-value` in `cl/pcl-runtime.lisp`:

```lisp
(defun stringify-value (v)
  ;; Overload check first (only for blessed objects)
  (when (p-box-p v)
    (let ((handler (p-find-overload v "\"\"")))
      (when handler
        ;; Guard against re-entrant stringify (object's "" handler calls print etc.)
        ;; Use a simple flag on the object itself.
        (return-from stringify-value
          (let ((result (p-call-overload handler v nil nil)))
            (if (p-box-p result)
                (let ((*p-overload-table* *p-overload-table*))  ; preserve table
                  ;; Recurse with the result (which may itself be overloaded or plain)
                  (to-string (unbox result)))
                (to-string result)))))))
  ;; ... existing stringify-value body unchanged ...
  )
```

**Note:** re-entrancy is handled implicitly because the *result* of the `""` handler is
typically a plain string, not an overloaded object.  If the handler returns an overloaded
object, we recurse once — which is correct Perl behaviour.

### Step 4 — Numification

Modify `to-number` in `cl/pcl-runtime.lisp`:

```lisp
(defun to-number (v)
  (when (p-box-p v)
    (let ((handler (p-find-overload v "0+")))
      (when handler
        (return-from to-number
          (to-number (unbox (p-call-overload handler v nil nil)))))))
  ;; ... existing to-number body ...
  )
```

Note: `to-number` receives unboxed values in many call sites.  The guard `(p-box-p v)`
is safe because `p-box-p` on a non-box returns nil immediately.

### Step 5 — Arithmetic operators

Modify `p-+`, `p--`, `p-*`, `p-/`, `p-%`, `p-**` to dispatch overloads:

```lisp
(defun p-+ (a b)
  (let* ((ha (p-find-overload a "+"))
         (hb (when (null ha) (p-find-overload b "+"))))
    (cond
      (ha (p-call-overload ha a b nil))
      (hb (p-call-overload hb b a t))    ; reversed
      (t  (make-p-box (+ (to-number (unbox a)) (to-number (unbox b))))))))
```

The pattern is identical for all binary arithmetic ops — factor into a macro:

```lisp
(defmacro define-overloaded-binop (name op-str body-form)
  `(defun ,name (a b)
     (let* ((ha (p-find-overload a ,op-str))
            (hb (when (null ha) (p-find-overload b ,op-str))))
       (cond (ha (p-call-overload ha a b nil))
             (hb (p-call-overload hb b a t))
             (t  ,body-form)))))

(define-overloaded-binop p-+ "+" (make-p-box (+ (to-number (unbox a)) (to-number (unbox b)))))
(define-overloaded-binop p-- "-" (make-p-box (- (to-number (unbox a)) (to-number (unbox b)))))
;; etc.
```

### Step 6 — Comparison operators

Same pattern for `p-<=>` and `p-cmp`, which are the ones most needed by `sort`:

```lisp
(defun p-<=> (a b)
  (let ((ha (p-find-overload a "<=>")))
    (if ha
        (p-call-overload ha a b nil)
        (make-p-box (let ((na (to-number (unbox a)))
                          (nb (to-number (unbox b))))
                      (cond ((< na nb) -1) ((> na nb) 1) (t 0)))))))
```

Derived comparisons (`<`, `>`, `==`, etc.) can call `p-<=>` when `<=>` is overloaded
and the specific op is not:

```lisp
(defun p-== (a b)
  (let ((ha (p-find-overload a "=="))
        (hb (p-find-overload b "==")))
    (cond
      (ha (p-call-overload ha a b nil))
      (hb (p-call-overload hb b a t))
      ;; fallback: use <=> if available
      ((or (p-find-overload a "<=>") (p-find-overload b "<=>"))
       (p-true-p (make-p-box (zerop (unbox (p-<=> a b))))))
      (t (make-p-box (if (equalp (to-number (unbox a)) (to-number (unbox b))) 1 ""))))))
```

### Step 7 — String concatenation and boolean

```lisp
;; p-. (string concat)
(defun p-string-concat (a b)
  (let* ((ha (p-find-overload a "."))
         (hb (when (null ha) (p-find-overload b "."))))
    (if (or ha hb)
        (if ha (p-call-overload ha a b nil) (p-call-overload hb b a t))
        (make-p-box (concatenate 'string
                                 (to-string (unbox a))
                                 (to-string (unbox b)))))))

;; p-true-p (boolean context)
(defun p-true-p (v)
  (let ((handler (p-find-overload v "bool")))
    (if handler
        (not (null (unbox (p-call-overload handler v nil nil))))
        ;; ... existing p-true-p body ...
        )))
```

### Step 8 — `overload::StrVal`

Some code calls `overload::StrVal($obj)` to get the raw (unoverloaded) stringification
(e.g. the memory address form).  Add a stub:

```lisp
;; In ExprToCL.pm %RUNTIME_NAMES:
"overload::StrVal" => "p-overload-strval",

;; In pcl-runtime.lisp:
(defun p-overload-strval (obj)
  "Return the non-overloaded string value (address form) of obj."
  (make-p-box (format nil "~A=HASH(0x~X)" (p-get-class obj) (sxhash (unbox obj)))))
```

### Step 9 — `overloaded($obj)` query

```lisp
(defun p-overloaded (obj)
  "Return true if obj has any overloading registered."
  (when (p-box-p obj)
    (let ((cls (p-get-class obj)))
      (when cls
        ;; Check if any key in *p-overload-table* has this class
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (when (and (consp k) (equal (car k) cls))
                     (return-from p-overloaded *p-true*)))
                 *p-overload-table*))))
  *p-undef*)
```

---

## Files to Change

| File | Change |
|------|--------|
| `cl/pcl-runtime.lisp` | Add `*p-overload-table*`, `*p-overload-fallback*`, `p-register-overloads`, `p-find-overload`, `p-call-overload`, `p-overload-strval`, `p-overloaded`; modify `stringify-value`, `to-number`, `p-+` … `p-**`, `p-<=>`, `p-cmp`, comparison ops, `p-string-concat`, `p-true-p` |
| `Pl/Parser.pm` | Add `use overload` case in `_process_use_statement` |
| `Pl/PExpr/Config.pm` | Add `overloaded` to `known_no_of_params` |
| `Pl/ExprToCL.pm` | Add `overload::StrVal` → `p-overload-strval` in `%RUNTIME_NAMES` |

---

## Implementation Order

1. **Step 1** (registry) + **Step 2** (parser) — enables `use overload` to be parsed without crashing
2. **Step 3** (stringify) — single highest-impact change; fixes most `sprintf2.t` failures
3. **Step 4** (numify) — fixes arithmetic on overloaded objects
4. **Step 5** (arithmetic ops) — fixes `+`, `-`, `*` etc.
5. **Step 6** (comparison) — fixes `sort`, `<`, `>`, `==`
6. **Step 7** (concat + bool) — fixes string concat and boolean context
7. **Steps 8-9** (strval, overloaded()) — fixes a few tests that query overload state

After each step, run `prove -j8 Pl/t/` to confirm no regressions, then check sweep counts.

---

## Test File to Write First

Before starting, write `Pl/t/overload-01.t` with runtime tests:

```perl
# stringify overload
run_pl('package MyStr; use overload q("") => sub { "hello" };
        package main; my $o = bless {}, "MyStr"; print "$o\n";');
# → hello

# arithmetic overload
run_pl('package MyNum; use overload "+" => sub { $_[0]{v} + $_[1] };
        package main; my $o = bless {v=>5}, "MyNum"; print $o + 3, "\n";');
# → 8

# sort with <=> overload
run_pl('package MyNum; use overload "<=>" => sub { $_[0]{v} <=> $_[1]{v} };
        package main; my @a = map { bless {v=>$_}, "MyNum" } (3,1,2);
        my @s = sort { $a <=> $b } @a; print join(" ", map {$_->{v}} @s), "\n";');
# → 1 2 3
```

---

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| `stringify-value` performance regression | `p-box-p` + `p-get-class` are O(1); only blessed objects pay the hash-table lookup cost |
| Infinite `""` recursion | Handler returns plain string in practice; if recursive, CL stack overflow is caught by `p-eval-block` |
| Reversed-operand logic errors | Test with `1 + $obj` style tests in overload-01.t |
| `fallback` autogeneration | Defer `fallback => 1`; just implement `fallback => undef` (default) for now |
| `use overload` with method-name strings (`'+' => 'add'`) | `p-call-overload` already handles: if `fn` is a box containing a string, call `p-method-call self fn` |
