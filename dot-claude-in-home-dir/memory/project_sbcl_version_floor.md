---
name: project_sbcl_version_floor
description: "PCL's supported SBCL version floor (2.5.2+) and how it was decided"
metadata: 
  node_type: memory
  type: project
  originSessionId: ac932eb0-4a77-41cd-aef3-b89c2e716d88
---

PCL's documented SBCL floor is **2.5.2+** (in `README.md` deps line and
`CLAUDE.md` Dependencies). The floor is **empirical, not a guessed range**: the
full 3206-test gate has passed on **2.5.2** (the dev env until 2026-05-31) and
on **2.6.0** (current; `sbcl --version` → "SBCL 2.6.0.debian"). dpkg log:
`2026-05-31 upgrade sbcl:amd64 2:2.5.2-1 → 2:2.6.0-1`.

**Why:** the runtime uses SBCL-internal symbols (`sb-posix`/`sb-kernel`/`sb-ext`
/`sb-alien`/`sb-debug`/`sb-unix`/`sb-int`/`sb-sys` — ~40 distinct, but only ~5-10
genuinely version-fragile, mostly `sb-kernel` internals). We chose **option 1
(declare a tested floor)** over building a compat abstraction layer. The layer's
real payoff would be cross-Lisp portability (CCL/ECL), not SBCL-version-spanning
— defer it until that's a goal.

**How to apply:** to claim support for an older SBCL (e.g. Ubuntu 22.04 LTS ships
2.1.11, which reportedly "fails hard"), do NOT trust analysis — install that SBCL
and run `prove -j8 Pl/t/`. Brute-force empirical validation, same philosophy as
the Perl 5.20 floor. Update the floor docs only to a version the suite has
actually passed on.

Related: the transpiler emits exactly ONE SBCL-specific symbol into generated
code — overflow float literals (`1e9999`) → now `(p-double-inf)` /
`(p-double-inf t)`, a macro in `cl/pcl-runtime.lisp` Arithmetic section wrapping
`sb-ext:double-float-{positive,negative}-infinity` (the only place that symbol
appears in generated output). See [[project_session232_magic_and_useparent]].
