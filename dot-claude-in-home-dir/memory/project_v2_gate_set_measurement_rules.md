---
name: project_v2_gate_set_measurement_rules
description: How to run a PCL gate-SET compare over both populations without phantom or missed drift
metadata: 
  node_type: memory
  type: project
  originSessionId: bc2357db-6528-45d2-8073-ecbe571b77c1
  modified: 2026-08-09T09:30:54.488Z
---

The gate SET = every source in BOTH populations: perl's own `t/` (528 `.t`
across the 11 default dirs) + the 14-dist CPAN board (223 = `t/*.t` +
`lib/**.pm` + `t/lib/**.pm`).  Diff HEAD (a `git worktree`, never a copy) vs
the working tree, file by file, stdout AND stderr.

**Normalize THREE things or the result is noise:**

1. **The compiler's OWN ROOT** — the emitted preamble embeds it
   (`*pcl-pl2cl-path*`, the @INC pushes, `*p-core-inc-dirs*`; task #217).  A
   worktree lives elsewhere, so without folding both roots to one token EVERY
   file reads as changed (hit live, s372).
2. **Compiler line numbers** — `ROOT/(Pl|tools)/\S+ line \d+` → `line N`.  A
   `.pm` that gained lines otherwise shifts every warning (18 phantom hits,
   s370).
3. **The `;;; pcl: … gen=…` header** — a generation bump would otherwise report
   every file changed (194/523, s368).

**Cheap-first order** (ruled s371 §1): `tools/corpus-diff.pl` BEFORE a full
sweep — identical emission over `perl-tests/*.t` proves the sweep's `.t` half
cannot move, in minutes instead of an hour.

**When the edit provably cannot change emission except by DYING** (e.g. a
`// $default` → `// die` flip), the targeted measurement is a DIE-SCAN: run the
new compiler over both populations and grep stderr for the new message.  0 hits
= the population is empty.  Used for #274 (s372); flagged for ruling in
`docs/opus5-review-requests-s372.md` §2.
