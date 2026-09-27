# PCL extensions

An extension is a Common Lisp file that implements a Perl built-in too
large or too specialised to live in `cl/pcl-runtime.lisp` itself. It loads
into a running program the first time the program needs it. This page says
what the extensions are, how they load, and how to regenerate or add one.
There are five:

| extension | file | written in | what it provides |
|---|---|---|---|
| `pcl-pack` | `cl/pcl-pack.lisp` | Perl (`cl/pack-impl.pl`), compiled by PCL, plus a hand-written appendix | `pack` and `unpack` |
| `pcl-mro` | `cl/pcl-mro.lisp` | Perl (`lib/mro.pm`), compiled by PCL | the always-available `mro::` functions (`get_linear_isa` and the rest) |
| `pcl-warnings` | `cl/pcl-warnings.lisp` | Perl (`lib/warnings.pm`), compiled by PCL | the `warnings::` query and emit functions (`enabled`, `warnif` and the rest) |
| `pcl-xs` | `cl/pcl-xs.lisp` | hand-written Lisp | PCL's side of the experimental XS bridge (the `XSLoader::load` path) |
| `pcl-uniprops` | `cl/pcl-uniprops.lisp` | data, generated from perl's own Unicode tables by `tools/rebuild-uniprops` | the tables behind `\p{…}`, `\pX` and `\P{…}`; its one entry point is a self-loading stub, first called when a program compiles its first property |

Three of the five are **written in Perl and compiled by PCL**: the
checked-in `.lisp` files are build output, regenerated as described below.
A fourth, `cl/pcl-uniprops.lisp`, is generated too, but it is data from
perl's `Unicode::UCD`, not compiler output: it carries no `gen=` stamp (its
line 1 names the Unicode version and the perl that built it), and
`Pl/t/uniprops-01.t` regenerates it and compares the bytes.

## How extensions load: lazily, through stubs

Nothing is loaded eagerly. Every public entry point of an extension has a
*self-loading stub* in `pcl-runtime.lisp`: the first call loads the
extension's file, and then calls the real definition that the load
installed over the stub. `p-pack` and `p-unpack` are hand-written stubs;
the `mro::` and `warnings::` functions use the `%pcl-def-ext-stub` macro.

`p-load-extension NAME` does the work. It looks for `NAME.lisp` in the
directory `pcl-runtime.lisp` was loaded from (`*pcl-runtime-directory*`),
loads it once, and records it in `*pcl-loaded-extensions*`, so later calls
do nothing. It returns `nil`, and the stub signals a clear error, when the
file is missing.

Two consequences of loading lazily:

* **Extensions are not part of the saved runtime core.** Every run starts
  SBCL from a saved core of `pcl-runtime.lisp` alone (see
  [`caching.md`](caching.md) §1). An extension loads from the tree at first
  use, through its own compiled cache (below). A program that never calls
  `pack` never pays for it.
* **An extension may install definitions and nothing else.** It is loaded
  *into a running program*, so a program preamble (resetting `@INC`,
  setting the compiler path) would overwrite that program's state; for
  instance, `push @INC, "/tmp/mylib"; pack("N", 42)` would lose the push.
  `pl2cl --extension` therefore writes no preamble, and `p-load-extension`
  **dies**, naming the file, on an artifact that has one
  (`%pcl-check-extension-clean`). The check reads the program's state
  *after* the load, whichever way the file was loaded, so a compiled
  extension cannot get past it either; `tools/t/ext-fasl.t` tests both
  paths.

## The compiled-extension cache

Each extension is compiled once and cached under `~/.pcl-cache/ext/`, keyed
by the extension file's own bytes plus the runtime identity. Loading
`cl/pcl-pack.lisp` as text takes 4.26 seconds; loaded compiled, it takes
0.004 seconds (measured 2026-09-20). Because the key is the file's content,
a regenerated extension can never reach an old entry.
`PCL_NO_FASL_CACHE=1` turns the cache off (`--no-cache` does not), and
`PCL_FASL_DEBUG=1` shows which path each load took. Any failure falls back
to loading the text. The full description is in
[`caching.md`](caching.md) §4.

## Regenerating the transpiled artifacts

The three transpiled artifacts are checked into the tree, and line 1 of
each carries the cache generation (`gen=`) of the compiler that built it.
**After any change to the compiler's output they must be regenerated**, or
they keep running on the old compiler's code. The test
`Pl/t/artifact-staleness-01.t` compares each stamp with the current
generation (`*pcl-cache-generation*`) and fails until you do.

```bash
tools/rebuild-pack                                  # cl/pcl-pack.lisp (pack-impl.pl + appendix)
./pl2cl --extension lib/mro.pm      > cl/pcl-mro.lisp      && tools/tag-license cl/pcl-mro.lisp
./pl2cl --extension lib/warnings.pm > cl/pcl-warnings.lisp && tools/tag-license cl/pcl-warnings.lisp
tools/rebuild-uniprops                              # cl/pcl-uniprops.lisp: only when perl's Unicode version or the tool changes
```

The licence header lands on line 2, and the generation stamp stays on
line 1. `Pl/t/license-tag-01.t` fails without the header.

## Adding a new extension

1. Implement it, preferably in Perl under `lib/` (compile it with
   `pl2cl --extension`), or in hand-written Lisp. The file must load into
   the `:pcl` package: hand-written Lisp starts with `(in-package :pcl)`,
   and the transpiled output handles this itself.
2. Add self-loading stubs for the public entry points in
   `pcl-runtime.lisp`: one `%pcl-def-ext-stub` line per function (create
   the package first with `p-defpackage` if it is a new `Foo::` namespace).
3. Run `tools/tag-license` on any new file, and keep the parenthesis
   checker passing (`sbcl --script tools/check-parens.lisp FILE.lisp`).

## Distribution

`tools/install-pcl` copies the whole runtime tree, including the
`cl/*.lisp` extensions, in the same relative layout as the repository, and
builds the saved core at install time, so the lazy loads find their files
on the installed machine exactly as in a checkout.

A standalone binary made with `pl2cl --executable` does **not** embed the
extensions yet: it reads them from the PCL tree at run time, so it needs
that tree on the machine ([`single-binary-plan.md`](single-binary-plan.md)).
If you save an image yourself (`sb-ext:save-lisp-and-die :executable t`),
an extension is in the image only if something called into it before the
save; load the ones your program needs first (for example
`(pcl::p-load-extension "pcl-pack")`), or ship the `cl/` directory beside
the binary so the stubs can find the files.

## See also

* [`shipped-modules.md`](shipped-modules.md): how `use Foo` finds PCL's
  pure-Perl replacements in `lib/`, which are transpiled like user code.
* [`xs-artifact-cache.md`](xs-artifact-cache.md) and
  [`xs-shim-design.md`](xs-shim-design.md): the `pcl-xs` extension's own
  world.
