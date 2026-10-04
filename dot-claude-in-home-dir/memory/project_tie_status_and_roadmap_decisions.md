---
name: project_tie_status_and_roadmap_decisions
description: "tie status (s247 probe): TIESCALAR works, TIEHASH/TIEARRAY proxies created but element ops unwired, TIEHANDLE absent. User decisions: DESTROY permanently not supported; XS only after compiler rewrite."
metadata: 
  node_type: memory
  type: project
  originSessionId: eb7a8489-7fdb-47ae-89ed-8a37f49263a9
---

# tie status + roadmap decisions (session 247, 2026-06-12)

## tie — what's wrong (probed, not guessed)
- **TIESCALAR WORKS** end-to-end (FETCH/STORE via the p-tie-proxy in the box;
  unbox→FETCH, box-set→STORE chokepoints; same hook reused by p-magic-cell).
- **TIEHASH/TIEARRAY: proxy IS created** (`p-tie` dispatches TIEHASH/TIEARRAY
  by container type, cl/pcl-runtime.lisp ~9737) **but element operations never
  consult it** — `$h{k}` on a tied hash returns nothing instead of FETCH.
  Missing: proxy arms in p-gethash/(setf p-gethash)/p-delete/p-exists/p-keys/
  p-each (FIRSTKEY/NEXTKEY) and p-aref family (FETCH/STORE/FETCHSIZE/PUSH/...).
  Same fix shape as the %ENV-MARKER%/stash arms — add dispatch, ~1 session per
  container kind.
- **TIEHANDLE: absent.**
- Known follow-ups after wiring: double-FETCH ordering (`$tied || $var`,
  sweep catalog), local.t hang via real Tie::Array.

## User decisions (2026-06-12, durable)
- **DESTROY will NOT be supported — permanent** (confirms the existing
  not-supported.md entry; don't propose GC finalizers). Tie tests needing
  DESTROY-on-untie stay skipped.
- **XS is far future** — only after the COMPILER REWRITE for better codegen
  (docs/type-flow-and-codegen-plan.md / codegen-rewrite-spec.md). Don't invest
  in XS bridging before that.

Related: [[reference_box_magic_hook]], [[project_moo_progress]].
