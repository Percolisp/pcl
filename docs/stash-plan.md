# Plan: Symbol Table (Stash) Support — `%main::`, `$::{key}`, `delete $::{"pkg::"}`

## Context

Perl symbol tables (stashes) allow runtime manipulation of package namespaces.
Common patterns: `delete $::{"foo::"}` (remove a package), `delete $::{sub}`
(undefine a sub), `$::{key}` (look up a symbol), `*Foo:: = *Bar::` (alias packages).

PCL's current state is broken in two ways:

1. **`pl-stash` not exported** — `(pl-stash "main")` is generated in user packages
   but the symbol isn't in `:pcl`'s export list → immediate `UNDEFINED-FUNCTION` crash
2. **`pl-stash` is a stub** — returns a fresh empty hash, ignoring pkg-name entirely
3. **`*Foo:: = *Bar::` codegen is wrong** — generates `(pl-setf *Foo:: *Bar::)` which
   SBCL reads as package-qualified CL variables → `Package *MOVER does not exist`

Tests blocked: anonsub.t, bless.t (partly), caller.t, method.t (partly).

---

## Files to Modify

- `cl/pcl-runtime.lisp` — export, struct, stash-get, stash-delete, pl-gethash dispatch
- `Pl/ExprToCL.pm` — fix `*Pkg::` stash codegen (lines ~305-315)
- `Pl/t/transpile-test-03.t` or new file — regression tests

---

## Change 1: Export `pl-stash` (trivial)

In `cl/pcl-runtime.lisp` `defpackage :pcl` export list, add:
```lisp
#:pl-stash
```

---

## Change 2: Replace stub with proxy struct

Replace the `pl-stash` stub (lines 3301-3306) with:

```lisp
(defstruct (pl-stash-proxy (:constructor make-pl-stash-proxy (pkg-name)))
  pkg-name)

(defun pl-stash (pkg-name)
  "Return a stash proxy for the named Perl package."
  (make-pl-stash-proxy (to-string pkg-name)))
```

---

## Change 3: `stash-get` — reading `$::{key}`

```lisp
(defun stash-get (pkg-name key)
  "Return the typeglob for KEY in PKG-NAME's stash, or undef."
  (if (and (> (length key) 2) (string= "::" (subseq key (- (length key) 2))))
      ;; key ends in "::" — sub-package stash reference
      (make-pl-stash-proxy (subseq key 0 (- (length key) 2)))
      ;; symbol lookup: check for function binding (most common case)
      (let* ((upkey (string-upcase key))
             (pkg   (find-package (string-upcase pkg-name)))
             (fun-sym (and pkg (find-symbol (format nil "PL-~A" upkey) pkg))))
        (if (and fun-sym (fboundp fun-sym))
            ;; Return a typeglob-like box with the CODE slot set
            (let ((g (pl-make-typeglob pkg-name key)))
              (setf (pl-glob-code g) (symbol-function fun-sym))
              (make-pl-box g))
            *pl-undef*))))
```

---

## Change 4: `stash-delete` — `delete $::{key}` and `delete $::{"pkg::"}`

```lisp
(defun stash-delete (pkg-name key)
  "Delete KEY from PKG-NAME's stash. Handles 'subpkg::' and plain 'sym'."
  (if (and (> (length key) 2) (string= "::" (subseq key (- (length key) 2))))
      ;; delete $::{"foo::"} — remove a sub-package
      (let* ((subpkg-name (subseq key 0 (- (length key) 2)))
             (subpkg (find-package (string-upcase subpkg-name))))
        (when subpkg (delete-package subpkg))
        *pl-undef*)
      ;; delete $::{sym} — unintern all PCL variants of the symbol
      (let* ((upkey (string-upcase key))
             (pkg   (find-package (string-upcase pkg-name))))
        (when pkg
          (dolist (prefix (list (format nil "PL-~A" upkey) (format nil "$~A" upkey)
                                (format nil "@~A" upkey)  (format nil "%~A" upkey)))
            (let ((sym (find-symbol prefix pkg)))
              (when sym (unintern sym pkg)))))
        *pl-undef*)))
```

---

## Change 5: Dispatch in `pl-gethash` and `pl-delete`

**In `pl-gethash`** (around line 2840), add a new `cond` branch before the
`hash-table-p` fallthrough:

```lisp
((pl-stash-proxy-p h)
 (stash-get (pl-stash-proxy-pkg-name h) k))
```

**In `pl-delete`** (around line 3234), add a guard at the top:

```lisp
(when (pl-stash-proxy-p hash)
  (return-from pl-delete
    (stash-delete (pl-stash-proxy-pkg-name hash) (to-string key))))
```

---

## Change 6: Fix `*Pkg::` codegen in `ExprToCL.pm`

**Current** (lines ~305-315): handles `$Pkg::` and `%Pkg::` → `(pl-stash "Pkg")`.
Does NOT handle `*Pkg::` — falls through to generic symbol handling → `*Pkg::` → broken CL.

**Fix**: extend the regex to also catch `*` sigil:

```perl
if ($content =~ /^([\$\%\*])(.*)::$/) {
  my ($sigil, $pkg) = ($1, $2);
  $pkg = 'main' if $pkg eq '';
  $self->environment->add_referenced_package($pkg) if $self->environment;
  return "(pl-stash \"$pkg\")";
}
```

This makes `*Foo:: = *Bar::` generate:
```lisp
(pl-setf (pl-stash "Foo") (pl-stash "Bar"))
```

Then add a `pl-setf` dispatch for stash-to-stash assignment in `pcl-runtime.lisp`:
```lisp
;; (pl-setf (pl-stash "Foo") (pl-stash "Bar")) — package aliasing
((pl-stash-proxy-p place-val)
 (stash-copy (pl-stash-proxy-pkg-name place-val)
             (pl-stash-proxy-pkg-name (unbox value))))
```

And implement `stash-copy`:
```lisp
(defun stash-copy (dest-name src-name)
  "Copy all external symbols from SRC package into DEST package."
  (let ((src  (find-package (string-upcase src-name)))
        (dest (or (find-package (string-upcase dest-name))
                  (make-package (string-upcase dest-name) :use '(:cl :pcl)))))
    (when src
      (do-external-symbols (sym src)
        (shadowing-import sym dest))))
  *pl-undef*)
```

**Note:** `pl-setf` is a macro. The stash-proxy dispatch needs to be added as a
new case in the macro's expansion logic in `pcl-runtime.lisp`. Look for how
`pl-setf` handles `(pl-gethash ...)` — add a parallel case for `(pl-stash ...)`.

---

## Out of Scope

- `$::{key} = value` write (stash assignment by subscript) — skip for now; `(setf pl-gethash)` on a stash is a complex case. Most test usage is delete/read.
- `%main::` in list/hash context (enumerate all symbols) — skip
- `$::{z} = \undef` read-only constant trick — already documented in `docs/not-supported.md`
- Stash entries for non-function slots (scalar `$sym`, array `@sym`, hash `%sym`) — `stash-get` only returns the CODE slot for now; extend later if needed

---

## Verification

```bash
# 1. PCL suite — must stay green
prove -j8 Pl/t/

# 2. Targeted tests
perl sweep-perl-tests.pl --jobs 4 --timeout 60 anonsub.t caller.t method.t bless.t

# 3. Quick smoke tests
echo 'delete $::{main_func}; print "ok\n";' | ./pl2cl | sbcl --load /dev/stdin
echo 'delete $::{"Foo::"}; print "ok\n";' | ./pl2cl
echo '*Foo:: = *Bar::; print "ok\n";' | ./pl2cl
```

Expected improvements:
- anonsub.t: `delete $::{__ANON__}` no longer crashes
- caller.t: `delete $::{foo}` / `delete $::{"foo::"}` work
- method.t: `*Mover:: = *Mover2::` no longer crashes (package aliasing runs)
- bless.t: partially — the `delete $::{"_117941::"}` part works; the `package _117941`
  naming issue (starts with `_`) is a separate bug in `_assemble_output`'s `[A-Za-z]` scan
