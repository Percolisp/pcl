---
name: Tie::Array pl-sub shadow infinite recursion bug
description: Root cause, attempted fix, and next approach for Tie::Array/reverse/local.t hang
type: project
---

## The Bug

`reverse.t`, `local.t`, `sort.t`, `kvaslice.t` all hang when loading `Tie::Array` or `Tie::Hash`.

**Root cause** (session 74): `pl-sub` uses `shadow` inside `eval-when` to prevent user methods from clobbering `pcl::` built-ins. But `shadow` affects symbol resolution of the lambda body compiled in the SAME eval-when form. So `sub PUSH { push(@$o, @_) }` in `Tie::StdArray` transpiles to `(pl-sub pl-PUSH ...)` where the body contains `(pl-push ...)`. After `shadow`, `pl-push` in the body resolves to `Tie::StdArray::PL-PUSH` (itself) → **infinite recursion**.

The same problem affects any user method named the same as a pcl built-in: `PUSH`, `SHIFT`, `POP`, `UNSHIFT`, etc.

**Evidence**: `/tmp/tie_array_cl.lisp` line 421-434 shows the transpiled `Tie::StdArray::PL-PUSH`:
```lisp
(pl-sub pl-PUSH (&rest %_args)
  (let ((@_ (pl-flatten-args %_args)))
    (block nil
      (let (($o (make-pl-box nil)))
        (pl-my-= $o (pl-shift @_))
        (pl-push (pl-cast-@ $o) @_)  ; <- resolves to self after shadow!
      ))))
```

Also: line 518 `(pl-push @ISA "Tie::Array")` — `pl-push` in package `Tie::StdArray` after shadow also calls the user method.

## Fix Attempted (DID NOT WORK)

Two-form `progn` approach: compile lambda BEFORE shadow executes, then do shadow in a separate eval-when form:
```lisp
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (symbol-function 'impl-sym) (lambda params body)))  ; Form 1: compile first
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (shadow ...) ...))                                          ; Form 2: shadow after
```

**Empirical result**: STILL infinite recursion in BOTH interpreted (`load`) and compiled (`compile-file` + load FASL) modes. The assumption that Form 1 compiles before Form 2's shadow affects resolution is FALSE in SBCL.

## Correct Next Approach: Qualify Built-in Calls in Generated Code

Since `shadow` affects resolution of any unqualified symbol in the same package, the fix is to make built-in calls **fully qualified** in generated CL code. Instead of:
```lisp
(pl-push (pl-cast-@ $o) @_)
```
Generate:
```lisp
(pcl:pl-push (pl-cast-@ $o) @_)
```

`pcl:pl-push` always refers to the PCL built-in regardless of what `shadow` has done in the current package.

**Where to implement**: `Pl/ExprToCL.pm`, `cl_name()` function. When generating a call to a known PCL built-in (all `pl-*` functions), prefix with `pcl:`. This way user packages can define `sub PUSH` → `pl-PUSH` locally without shadowing the runtime.

**Also needed**: The `(pl-push @ISA ...)` in `@ISA = ('Base')` codegen — that's emitted by Parser.pm, not ExprToCL.pm. Need to qualify those too.

**Why:** The `shadow` in `pl-sub` only affects the **current package's** symbol table. `pcl:pl-push` is a package-qualified reference and bypasses local shadowing entirely.

## bop.t hang (FIXED in session 74)

`(ash 4 2147483648)` makes SBCL compute a 2-billion-bit integer → hangs indefinitely.

**Fix**: Clamped shift counts at ±64 in `pl-<<` and `pl->>` in `pcl-runtime.lisp`:
```lisp
(if (>= (abs bv) 64) 0 (ash av bv))
```
This converts bop.t tests 9-15 from hanging to failing (wrong values for extreme shifts, which is acceptable for now). bop.t may still hang at other points (tie magic, `fresh_perl_is` calls) — not yet verified.

## Why: Ties to Multiple Failing Files
- `reverse.t`: Passes tests 1-12, then hangs on `use Tie::Array;` at line 53
- `local.t`: 866 lines, blocked by same Tie::Array import
- `sort.t`: Tie::Array import
- `kvaslice.t`: Tie::Array import

Fixing the `pl-sub` shadow issue would unblock all four files.

## How to Apply
Next session: implement the `pcl:` prefix approach in `ExprToCL.pm`'s `cl_name()`. Test with:
```bash
echo 'use Tie::Array; my @a; tie @a, "Tie::StdArray"; push @a, 1, 2, 3; print scalar @a;' | ./pl2cl | sbcl --noinform --load cl/pcl-runtime.lisp --load /dev/stdin
```
Expected output: `3`
