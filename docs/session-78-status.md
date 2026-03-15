# Session 78 Status — sprintf.t regression

## What works
- `for ([1,"a"], [2,"b"])` now iterates over arrayrefs correctly (the fix we made)
- PCL tests: 2510/2510 passing
- The extra `)` paren bug in `%p-flatten-for-list` was fixed

## What's broken
- `sprintf.t` is 0/566 (was 2829/2830 before this session)
- Symptom: values print as `SCALAR(0x...)` instead of strings

## Root cause hypothesis

**`p-array-init` double-boxes p-box arguments.**

When `p-array-init` receives a p-box (variable like `$template`), the
`add-element` fallthrough does `(make-p-box e)` — wrapping the p-box in
ANOTHER p-box. Later, `box-set` sees a "reference" (box-of-box) and
stores the outer box as-is. `to-string` then produces `SCALAR(0x...)`.

**Why didn't this break sprintf.t before?** Unknown. This has always been
the behavior of `p-array-init`. Something changed — likely the `gen_progn`
change (which generates `(if *wantarray* (vector ...) (progn ...))` for
multi-form comma expressions) is triggering in a code path inside sprintf.t
that it didn't before, changing what values get passed to `p-array-init`.

## What to try next session

1. **Isolate**: Does removing just the `gen_progn` change (restoring old
   `Pl/ExprToCL.pm` gen_progn) fix sprintf.t while keeping the runtime fix?

2. **Check**: What does sprintf.t's `push @tests, [...]` generate with old
   vs new gen_progn? Is there a comma expression somewhere that now wraps
   things differently?

3. **Fix `p-array-init` to unbox arguments**:
   ```lisp
   ;; Change the fallthrough in add-element from:
   (t (vector-push-extend (make-p-box e) result))
   ;; to:
   (t (vector-push-extend (make-p-box (unbox e)) result))
   ```
   This would stop double-boxing p-box arguments. **But** verify it doesn't
   break references (arrayrefs, hashrefs passed into `[...]`). Arrayrefs are
   p-boxes wrapping vectors — `(unbox arrayref)` = the vector, which then
   hits the `(vectorp e)` branch and gets SPREAD. That's wrong for
   `[$aref, ...]` — you'd lose the ref.

   Better fix for the fallthrough:
   ```lisp
   (t (let ((raw (if (p-box-p e) (p-box-value e) e)))
        (if (and (vectorp raw) (not (stringp raw)))
            ;; It's an arrayref — keep it as a ref (wrap vector in box)
            (vector-push-extend (make-p-box raw) result)
            ;; It's a scalar box — unbox and re-box (avoid double-boxing)
            (vector-push-extend (make-p-box raw) result))))
   ```
   Wait, these two branches do the same thing. The real fix:
   ```lisp
   ;; When e is a p-box: store e itself (it's a reference — arrayref, hashref, scalarref)
   ;; When e is a raw value: wrap in p-box
   (t (vector-push-extend (if (p-box-p e) e (make-p-box e)) result))
   ```
   This preserves arrayrefs and scalar refs as-is (correct), and wraps
   raw scalars in a new p-box (also correct). No double-boxing.

## Changed files in this session (uncommitted)

- `cl/pcl-runtime.lisp` — new `%p-flatten-for-list` (the foreach arrayref fix)
- `cl/pcl-test.lisp` — `test-to-scalar` handler-case wrapper
- `Pl/ExprToCL.pm` — `gen_progn` wraps @-sigil items with `(p-flatten ...)`
