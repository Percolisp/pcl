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
  use, through the compiled-extension cache below.  A program that never
  calls `pack` never pays for it — not even the hash of a file it never
  loads, since `p-load-extension` does nothing at all until something asks.
* **An extension may install definitions and nothing else.**  It is `load`ed
  *into a running program*, so a PROGRAM preamble (the `@INC` reset, the
  `*pcl-pl2cl-path*` setup) would clobber that program's state — that was
  task #349's silent bug.  `pl2cl --extension` therefore emits no preamble,
  and `p-load-extension` **dies** (rule 12) on an artifact that carries one
  (`%pcl-check-extension-clean`).  The check reads the program's load state
  *after* the load, whichever form ran it, so a compiled extension smuggles
  nothing past it either — `tools/t/ext-fasl.t` has that row on both paths.

## The compiled-extension cache (task #1202)

An extension used to be `load`ed as **text**, so SBCL recompiled the whole
artifact on every run that reached it: `cl/pcl-pack.lisp` costs **4.26 s** to
load that way and **0.004 s** as a fasl (measured s1202).  And that cost was
not paid by `pack` users — `Sub::Quote`'s top-level code calls `pack("F",0)`
and Moo loads `Sub::Quote` for any `has`, so **every Moo class with one
attribute** paid the whole pack recompile, every run (task #1910).

So `p-load-extension` now goes through the **module fasl machinery**
(`docs/caching.md` §2): `%p-build-module-fasl` with its `*pcl-fasl-build*`
discipline, `%p-load-module-fasl`, the same temp + `rename(2)` publication,
the same `.failed` marker, the same 30-day prune.  One thing differs, and
only because an extension is not a transpile:

> **The key is the extension file's own BYTES**, plus `*pcl-runtime-identity*`
> (this runtime's source hash + this SBCL).  The entry is
> `<cache>/ext/<name>-<content stem>-<runtime identity>.fasl`.

A module entry is keyed by its *path* and validated against a dependency
manifest; an extension has no source to be out of date with, so there is no
validity question left to ask. **A stale extension fasl is not unlikely, it is
unreachable**: regenerate `cl/pcl-pack.lisp` and the next run computes a
different name and builds a new entry — and the superseded entry *for this
runtime* is deleted at that build (entries for a different runtime identity
are left alone; they belong to another tree and age out).

Notes:

* `--no-cache` / `PCL_NO_CACHE` does **not** turn this off, deliberately.
  That switch answers "is it the cache?" about a *transpile* of your code; an
  extension is a checked-in file compiled against this runtime and keyed by
  its bytes — which is exactly what the saved core is, and `--no-cache` does
  not disable that either (`PCL_NO_CORE` does).  **`PCL_NO_FASL_CACHE=1`** is
  the switch, and it turns off every fasl in the image at once.
* `PCL_FASL_DEBUG=1` names the path taken per extension: `FASL HIT`,
  `fasl-build`, or `TEXT`.
* Every failure — unreadable file, refused build, broken fasl — ends in the
  text load, so the worst case is the speed PCL had before this task.
* `pcl --cache-info` counts `ext/` as its own population; `pcl --clear-cache`
  removes it.

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
