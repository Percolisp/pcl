# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Session state — s439 (2026-08-23, Fable), saved at a break

Resume here.  Nothing is mid-edit; the tree is clean after the commit that
includes this file.  Read this, then `docs/plan-post-s433.md` §4a (the push,
done) and §1 (the queue).

## Done this session (all committed, pushed)

1. **Reviewed Opus's s438 → s438i batch — APPROVED.**  Record
   `docs/fable-answers-s439.md`, DECIDED §s439.  One review fix (#455's
   `_signatures_enabled_at` now recognises `qw(signatures)` / `:5.36` /
   `no feature`; commit `4911ef1`).  Filed #491–#496; #476 widened; #277
   corrected to completed.
2. **README rewritten** (USER; then USER simplified it further by hand — 277
   lines, that edit rode in `30c26a7`), jargon "lower" removed, speed/XS cut
   to a paragraph each.
3. **Cached runtime core is the DEFAULT** (USER "keep the runtime compiled and
   cached").  `tools/lib/PCLSbcl.pm::cached_core`, commit `30c26a7`, DECIDED
   §s439b.  Gate 166/5780, sweep 18312 +0, both ~2× faster.
4. **One checkout** (USER).  `~/pcl` is the record; `~/testgit/pcl` verified
   to hold nothing unique (its 9 dirty files == `~/pcl@74cf2b1`) — **USER to
   `rm -rf ~/testgit/pcl`** (I do not delete dirs).  `~/pcl` origin switched
   to the SSH URL that authenticates (`git@github.com:Percolisp/pcl.git`).
   DECIDED §s439c.
5. **PUSHED** (USER "please do so"): `snapshot-2026-05` at `54b2aa4`, `main`
   force-updated to `917e1a9` (`--force-with-lease`, origin verified first),
   tag `R1`.  Secrets scan + big-blob scan clean beforehand.  Recipe in
   plan-post-s433 §4a.

## OPEN — two threads, pick up here

### A. CI is RED — diagnose and fix (blocks the v0.1 tag)

* Run **32648385694** (https://github.com/Percolisp/pcl/actions/runs/32648385694),
  conclusion **failure**.  Steps 1–5 GREEN (apt PPI 1.277 + Moo, sbcl.org
  2.6.0 binary, Quicklisp + cl-ppcre).  **Step 6 "Fresh-machine install
  (tools/install-pcl)" failed in ~3 s, exit code 2, BEFORE the core build**
  (too fast for it); the 3 later steps skipped.
* **The log needs repo-admin auth to read** (`GET .../jobs/<id>/logs` → 403
  "Must have admin rights").  Get the error text by one of: the USER pastes
  the failing step's log from the Actions web UI; `gh` installed +
  authenticated (`gh run view 32648385694 --log-failed`); or reproduce (next).
* **NOT reproduced locally**: `tools/install-pcl` PASSES with system perl +
  apt **PPI 1.277** + a fresh Quicklisp in a sanitized `$HOME` (measured this
  session, `.../scratchpad/cihome`).  So the blocker is **CI-image-specific**,
  not a PPI-version floor.
* **Prime suspect**: the **sbcl.org tarball binary** + `SBCL_HOME=$HOME/sbcl/
  lib/sbcl`.  The runtime loads cl-ppcre with `(asdf:load-system :cl-ppcre)`
  after `(require :asdf)`; a fresh Quicklisp writes `~/.sbclrc` (`(load
  "~/quicklisp/setup.lisp")`), which registers cl-ppcre for ASDF — but the
  install-pcl core build spawns a fresh `sbcl` (`.../PCLSbcl.pm` `_build_
  cached_core` and `build_core` both use plain `--load`, which DOES read
  `.sbclrc`).  The 3 s / exit-2 shape is most consistent with the core build
  dying at cl-ppcre load, OR a die in the installer before it.  **Next step
  (interrupted): reproduce with the sbcl.org binary, not the debian one** —
  install it into a sanitized `$HOME` exactly as `.github/workflows/ci.yml`
  does, then run `tools/install-pcl --prefix $HOME/.local` and read the error.
  Note also: install-pcl's `build_core` (line ~180) still spawns a HAND-WRITTEN
  `sbcl --load` command, NOT through PCLSbcl — if the fix is a `--sysinit`/
  `.sbclrc` matter, both spawn sites need it.
* Once the error is known: fix, re-run CI (push a fixup commit; CI triggers on
  push to `main`).  `tools/t/install-pcl.t` is the local rehearsal (passes
  here).

### B. USER request, not yet done: remove the review docs from git

USER: "remove the fable answers and opus reviews from git.  It doesn't seem
necessary?"  Means `git rm` the session-review artifacts:
`docs/fable-answers-*.md` (~30 files) and `docs/opus5-review-requests-*.md`.
**Before doing it**: they are cross-referenced from `docs/DECIDED.md`, `CLAUDE.md`
and each other, so a blind `git rm` leaves dangling links.  Plan: `git rm` the
files, then sweep `CLAUDE.md` + `docs/DECIDED.md` + `docs/session-log.md` for
`fable-answers-` / `opus5-review-requests-` references and either drop the
link text or point it at DECIDED (the settled rulings already live there).
`docs/plan-post-*.md` and `docs/fable-answers-s439.md` are the CURRENT queue
+ this review — confirm with USER whether "the reviews" includes the live
ones or only the historical batch.  This was interrupted before starting;
nothing removed yet.

## Note on the "cybersecurity" worry

Nothing this session touched security.  It was: reviewing a Perl→Common-Lisp
compiler's own test results, editing README/docs, adding a saved-core cache,
and a normal `git push` to the user's own GitHub repo (`Percolisp/pcl`, an
open-source Perl-to-CL transpiler).  The one interrupted command was
downloading the official SBCL binary from sbcl.org to reproduce a CI failure —
routine.  No credentials were read or sent anywhere; the only network writes
were the git push (to the user's repo, over their own SSH key) and read-only
GitHub API polling of the public Actions run.  No sign of anything anomalous.
