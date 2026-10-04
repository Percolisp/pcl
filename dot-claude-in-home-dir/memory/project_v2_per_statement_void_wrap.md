---
name: project_v2_per_statement_void_wrap
description: "RESOLVED s288 (task #60): v2 sub-body :void regime shipped — one (let ((*wantarray* :void))) per body, wa_void_active suppresses per-statement binds, tail restores caller ctx at the LEAF. Keeps the design rationale + the compound-tail trap for future wantarray work."
metadata:
  node_type: memory
  type: project
  originSessionId: 9abe7e94-688c-4f93-a12b-976c0c74175d
---

**RESOLVED (s288, 2026-07-13, commit ecda6a9, gen v2-30).** The v2
compile-memory scaling bug (425 per-statement `(let ((*wantarray* :void)))`
wraps in substr.t's run_tests → SBCL heap exhaustion in the sweep's 1GB
dynamic space) is fixed by hoisting the regime, mirroring v1's
`wa_void_active` model.

**The shipped design (know this before touching wantarray again):**
- `Parser2::_lower_body_regime`: a multi-statement sub body (or single
  compound) is wrapped ONCE in `(let ((*wantarray* :void)) …)` and lowered
  with `local environment->{wa_void_active}=1`.  A single non-compound
  statement body (accessor) skips the regime — zero added binds.
- Three emitters skip their own :void bind under the flag: the seam's
  `ExprToCL::_ctx_wrap` (was already flag-aware), `ExprToCL2` native
  funcall bind, `_lower_stmt`'s narrowed g-match wrap.
- **Tail restore is LEAF-LEVEL** (`_restore_caller_wa` in `_lower_stmt`,
  fires when `$tail_ctx eq 'inherit'` && regime): wraps the innermost
  expression statement in `(let ((*wantarray* *pcl-caller-wantarray*)) …)`.
  **TRAP: never wrap a whole compound tail** — its non-tail inner
  statements must stay in the :void ambient; compounds thread `$tail_ctx`
  down to their branch leaves, which is where the restore lands.
- Explicit `return` needs nothing: the `p-return` macro evaluates its
  values under `*pcl-caller-wantarray*` itself; `p-sub` binds that at
  every entry (both calling conventions).  `p-wantarray` also reads it.
- Boundaries (do{}/eval{}/map-grep-sort/anon-sub bodies) are handled by
  v1's existing `local wa_void_active` resets in `parse_block_as_function`
  / `parse_block_to_cl_string` — the seam path flows through them.
- Perf: +2 dynamic binds per multi-statement sub call = noise (gcdrec
  bench row added to tools/bench-exec.pl: +0.7%); fib-class accessors
  unaffected via the single-statement carve-out.

Verified by full-sweep per-file parity vs HEAD + corpus-diff (35/111
files, all hunks regime-shaped) + a perl-vs-CL battery in
`Pl/t/transpile-test-04.t`.  Guard: `Pl/t/parser2-01.t` asserts exactly
ONE :void bind in the fib program.  ir-spec §4 documents the regime.

substr.t's remaining gate is ONLY `foreach over a magic-lvalue element
(substr/pos/vec)` now.  Related find while probing: task #64 — a bare
block as sub tail loses its value in BOTH pipelines (pre-existing).
See [[project_parser2_prototype]].
