# PCL Extensions

*(Rewritten 2026-08-25 to match the current tree; the 2026-05 version of
this file described an eager-load model that no longer exists.)*

PCL supports optional extension modules — CL files that implement Perl
built-ins too large or too specialised to live in `cl/pcl-runtime.lisp`
itself.  The current set:

| extension | file | source of truth | what it provides |
|---|---|---|---|
| `pcl-pack` | `cl/pcl-pack.lisp` | **transpiled** from `cl/pack-impl.pl` (Perl) + a hand-written appendix | `pack` / `unpack` |
| `pcl-mro` | `cl/pcl-mro.lisp` | **transpiled** from `lib/mro.pm` | the always-available `mro::` API (`get_linear_isa`, …) |
| `pcl-warnings` | `cl/pcl-warnings.lisp` | **transpiled** from `lib/warnings.pm` | the `warnings::` query/emit API (`enabled`, `warnif`, …) |
| `pcl-xs` | `cl/pcl-xs.lisp` | hand-written CL | the pclxs XS-bridge host side (`XSLoader::load` path) |

Three of the four are **written in Perl and compiled by PCL** — the checked-in
`.lisp` files are build artifacts (see "Regenerating", below).

## How extensions are loaded: lazily, via self-loading stubs

There are **no eager loads**.  Every public entry point of an extension has a
*self-loading stub* in `pcl-runtime.lisp`: the first call loads the
extension's `.lisp` file and then delegates to the real definition the load
just installed over the stub.  `p-pack`/`p-unpack` are hand-written stubs;
the `mro::`/`warnings::` families use the `%pcl-def-ext-stub` macro.

`p-load-extension NAME` does the actual work: it looks for `NAME.lisp` in
`*pcl-runtime-directory*` (the directory `pcl-runtime.lisp` was loaded from),
loads it once, and records it in `*pcl-loaded-extensions*` so later calls are
no-ops.  It returns `nil` (and the stub signals a clear error) when the file
is absent.

Two consequences of the lazy model:

* **Extensions are NOT baked into the saved runtime core.**  Every runner
  starts SBCL from a content-keyed saved core of `pcl-runtime.lisp` alone
  (`~/.pcl-cache/core/`, USER s439); extensions load from the tree at first
  use.  A program that never calls `pack` never pays for it.
* **An extension may install definitions and nothing else.**  It is `load`ed
  *into a running program*, so a PROGRAM preamble (the `@INC` reset, the
  `*pcl-pl2cl-path*` setup) would clobber that program's state — that was
  task #349's silent bug.  `pl2cl --extension` therefore emits no preamble,
  and `p-load-extension` **dies** (rule 12) on an artifact that carries one
  (`%pcl-check-extension-clean`).

## Regenerating the transpiled artifacts

The three transpiled artifacts are checked into the tree and stamped on line
1 with the `gen=` cache generation that built them.  **After any
emission-changing commit they must be regenerated**, or they keep running on
the old codegen — `Pl/t/artifact-staleness-01.t` (in the gate) compares each
stamp against `*pcl-cache-generation*` and fails the same session.

```bash
tools/rebuild-pack                                  # cl/pcl-pack.lisp (pack-impl.pl + appendix)
./pl2cl --extension lib/mro.pm      > cl/pcl-mro.lisp      && tools/tag-license cl/pcl-mro.lisp
./pl2cl --extension lib/warnings.pm > cl/pcl-warnings.lisp && tools/tag-license cl/pcl-warnings.lisp
```

(The license tag lands on line 2; the gen stamp stays line 1.
`Pl/t/license-tag-01.t` fails without the tag.)

## Adding a new extension

1. Implement it — in Perl under `lib/` (preferred; transpile with
   `pl2cl --extension`) or hand-written CL.  The file must be loadable into
   the `:pcl` package world (`(in-package :pcl)` for hand-written CL;
   transpiled output handles this itself).
2. Add self-loading stubs for the public entry points in
   `pcl-runtime.lisp` — one `%pcl-def-ext-stub` line per function (create
   the package first with `p-defpackage` if it is a new `Foo::` namespace).
3. Run `tools/tag-license` on any new file; keep the paren checker green
   (`sbcl --script tools/check-parens.lisp FILE.lisp`).

## Distribution

`tools/install-pcl` copies the whole runtime tree (including `cl/*.lisp`
extensions) in its repo-relative shape and builds the saved core at install
time, so the lazy loads find their files on the installed machine exactly as
in a checkout.

For a **standalone binary** (`sb-ext:save-lisp-and-die :executable t`), note
the lazy model: an extension is in the image only if something already
called into it (or you `(pcl::p-load-extension "pcl-pack")` explicitly)
before saving.  Load the extensions your program needs before the save, or
ship the `cl/` directory beside the binary so the stubs can find the files.

## See also

* `docs/shipped-modules.md` — how `use Foo` decides between a `lib/` pure-Perl
  shim (transpiled like user code) and CL-backed functionality; extensions
  are the engine behind the CL-backed side.
* `docs/xs-artifact-cache.md` / `docs/xs-shim-design.md` — the `pcl-xs`
  extension's own world.
