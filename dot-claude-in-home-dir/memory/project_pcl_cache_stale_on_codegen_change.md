---
name: project_pcl_cache_stale_on_codegen_change
description: "~/.pcl-cache serves a stale transpiled require'd module after a codegen (Pl/) change — clear it when debugging"
metadata: 
  node_type: memory
  type: project
  originSessionId: b41ed61b-b72b-4fc8-87ad-039faadb42e6
---

`p-require`/`use` caches transpiled modules under `~/.pcl-cache/` (keyed on
source mtime/hash, not on the PCL compiler version). After you change codegen
(`Pl/*.pm`), a `require`d module whose **source** didn't change (e.g. perl's
`t/test.pl`, or any `lib/*.pm` shim) keeps serving the **old** transpiled
output from cache — so your fix appears not to work and tests crash/fail in the
required module's code, not yours.

**Why:** the cache validates on the module's source file, which is unchanged;
the compiler producing the cached `.lisp`/`.fasl` is not part of the key.

**How to apply:** when a codegen change affects code that flows through a
`require`d/`use`d module and the symptom is in that module, `rm -rf
~/.pcl-cache/*` before re-running. `PCL_NO_CACHE=1` (env) / `--no-cache` bypass
it for one run. Cost me a real debugging detour (a stale `test.pl` masked a
prototype-context codegen fix — `(p-scalar @_)` lingered after the source fix
landed). See `docs/perl-test-suite-survey.md` t/op section (commit 74de22e).

Especially relevant for the perl-test-suite survey (`tools/run-perl-suite.pl`):
those files `require './test.pl'`, which is cached.
