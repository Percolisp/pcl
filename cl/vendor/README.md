# `cl/vendor/` — third-party source, carried verbatim

Everything under this directory is **upstream code**.  It is not PCL's, it is
not tagged with PCL's license header, and **it is never edited here**.  A fix
belongs upstream; a new upstream version replaces the whole directory and
records the new stamp below.

## `cl-ppcre/`

| | |
|---|---|
| what | [cl-ppcre](https://edicl.github.io/cl-ppcre/), the Common Lisp regex engine PCL's `m//`, `s///` and `split` run on |
| version | `2.1.2` (what the `.asd` reports), taken from upstream **commit `a2ea581c23fdc184168423adbd4b4c1f48d42743`** (2025-06-06), which is master after the v2.1.2 tag |
| source | <https://github.com/edicl/cl-ppcre> — `https://github.com/edicl/cl-ppcre/archive/a2ea581.tar.gz` |
| retrieved | 2026-09-10 |
| license | BSD 2-clause, `cl-ppcre/LICENSE`, verbatim |
| author | Dr. Edmund Weitz |

**Why this commit and not the v2.1.2 tag.**  The tag's `optimize.lisp` and
`closures.lisp` differ from master; the master snapshot is what PCL has always
run against (Debian's `cl-ppcre` package ships exactly `a2ea581`, and that is
the copy every measurement, baseline and the `#1461` BMH self-test in
`cl/pcl-runtime.lisp` were taken with).  Vendoring the tag would have been a
silent library change riding a packaging commit.  Every file here is
byte-identical to that commit; only the file MODE was normalised (upstream
ships `charset.lisp` executable).

**What is here and what is not.**  Only the files the `cl-ppcre` ASDF system
loads — `cl-ppcre.asd` plus its 17 components — with `LICENSE` and `CHANGELOG`
for provenance.  The `cl-ppcre-unicode` system, the test suite and the HTML
docs are not carried: PCL loads `:cl-ppcre` and nothing else
(`asdf:already-loaded-systems` shows `asdf uiop asdf-package-system cl-ppcre`).

**How PCL finds it.**  `cl/pcl-runtime.lisp` pushes `<the runtime's own
directory>/vendor/cl-ppcre/` onto `asdf:*central-registry*` before its
`asdf:load-system :cl-ppcre`, so a machine with SBCL and nothing else can run
PCL.  If this directory is missing, ASDF's ordinary search (a distro package,
Quicklisp, a `~/.sbclrc`) is the fallback, and the error message says which of
the two was tried.  `tools/lib/PCLSbcl.pm` folds this directory's contents
into the saved core's cache key, so changing the vendored library invalidates
the core exactly as editing the runtime does.

**Upgrading.**  Replace the directory wholesale, update the stamp above,
re-run the gate, and re-read `%pcl-install-bmh-matcher`'s self-test output in
`cl/pcl-runtime.lisp` (`PCL: literal-prefix (BMH) regex scanning is OFF: …` on
stderr means the new version moved `cl-ppcre::create-bmh-matcher` and the
`#1461` optimization declined).
