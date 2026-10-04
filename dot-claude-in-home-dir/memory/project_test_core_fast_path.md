---
name: project_test_core_fast_path
description: tools/prove-core runs the Pl/t gate ~3.4x faster via a saved SBCL core; how it works and the --core arg-order trap
metadata: 
  node_type: memory
  type: project
  originSessionId: 62f3964a-83ec-4965-a26e-a74e5ef4ab5e
---

**`tools/prove-core`** runs the Pl/t suite ~3.4x faster (full gate **8:18 → 2:30**;
transpile-test-04.t alone 2:01 → 21s). Use it for faster iteration; plain
`prove -j8 Pl/t/` still works and stays the reference.

**Why it helps:** each test spawns sbcl and `--load`s `cl/pcl-runtime.lisp`,
which RECOMPILES the runtime (~1.2s) *every spawn*; the big transpile-test files
spawn sbcl once per test, so runtime compilation dominates. A saved core (SBCL
image with the runtime already compiled) cuts that to ~0.003s.

**Design (added s278b, commit 633d8dd):**
- `tools/prove-core` builds a **FRESH** core into a temp file on **every** run
  (removed on exit), then runs `prove` with `PCL_TEST_CORE=<path>`. Rebuilding
  each run is deliberate — a cached core would silently test against **stale
  runtime code**. (The user specifically insisted on this.)
- `Pl/t/PCLCore.pm` is a pure CONSUMER: `sbcl_prefix($runtime)` returns
  `--core <c> …` when `PCL_TEST_CORE` points at a core newer than the runtime,
  else the source-load args. Staleness guard = refuse a core older than runtime.
- 57 sbcl-spawning Pl/t files are retrofitted (`use PCLCore;
  my @sbcl_rt = PCLCore::sbcl_prefix($runtime); ... sbcl @sbcl_rt --load $cl`).
  The rest keep source-loading (still correct). Retrofit is uniform — script in
  the commit; extend to more files the same way.

**The `--core` arg-order TRAP** (bit both the `pcl` wrapper and this): `--core`
is an SBCL **runtime** option and MUST precede every **toplevel** option
(`--non-interactive`, `--eval`, `--load`), or SBCL aborts with *"C runtime
option --core in the middle of Lisp options."* Fixed in the `pcl` wrapper too
(commit 916e779) — the old `pcl --make-core` core was unusable for this reason.

Note: `cl/pcl-runtime.lisp` does NOT `compile-file` cleanly (READ error
"Package ASDF does not exist"), so a plain `.fasl` is not an option — the core
(`--load` then `save-lisp-and-die`) is the only fast path. See
`docs/test-infrastructure.md` §saved-core.

**UPDATE s439b (2026-08-23, USER ruling "by default the CL runtime is kept
compiled and cached"):** the cached core is now the DEFAULT for EVERY runner,
not an opt-in.  `tools/lib/PCLSbcl.pm::cached_core` builds
`~/.pcl-cache/core/pcl-<path8>-<content12>.core` on first use and names it by
a hash of (runtime abs path, runtime source, `sbcl --version`, `~/.sbclrc`
size+mtime, key version) — content-keyed, so NO mtime freshness logic and no
stale case; older cores for the same path pruned; flock + tmp/rename; a
failed build → loud fallback to source + a one-hour `.failed` marker.  Order:
explicit core > PCL_TEST_CORE > installed <root>/pcl.core > cached > source.
`PCL_NO_CORE=1` disables the cache only.  `./pcl` uses PCLSbcl too
(`--make-core` = build early, `--clear-cache` clears cores + modules).  The
extensions are lazily loaded from `*pcl-runtime-directory*` and are NOT in the
core (why the path is in the key: a worktree must get its own).  Measured:
plain `prove -j8 Pl/t/` 224 s (= prove-core), `pcl -E` 0.145 s.
`tools/prove-core` stays (fresh temp core per run, belt and braces).  The
first-run progress line prints only when stderr is a tty — a capture (the
installer smoke test) must not see it.  Test: `tools/t/sbcl-prefix.t` (27).
