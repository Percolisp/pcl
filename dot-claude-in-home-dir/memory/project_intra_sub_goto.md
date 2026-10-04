---
name: project_intra_sub_goto
description: "goto LABEL: FULLY SHIPPED on v2 (s295b+c catch-wrap, ir-spec §6.4); v1-era partial status + Text::Balanced blockers now historical"
metadata: 
  node_type: memory
  type: project
  originSessionId: 79981349-5650-4ce1-9be0-01a9021c3a8e
  modified: 2026-07-18T06:21:35.285Z
---

**s295b+c (2026-07-18): `goto LABEL` is FULLY LOWERED on the v2 (default)
pipeline** — task #63 shipped.  Backward goto = lexical `(go :label)` into the
label's `(tagbody :label …)`; forward goto = `(catch :pcl-goto-LBL prefix…)` +
`(throw :pcl-goto-LBL nil)`, which also works from inside map/grep lambdas
(dynamic extent — the shape v1 crashes on).  **Normative porter spec:
`docs/ir-spec.md` §6.4** (regimes, composition rules, scope guard: prefix with
my/state/local decls is not wrapped → gate).  Computed `goto EXPR` stays
not-supported (`docs/not-supported.md` "Computed goto", scope note added).
De-gated array.t (167+15/195, beats v1).  Guards: transpile-test-01b.t
(lambda-goto — must NOT go in goto-label-01.t, whose harness drives v1
directly), parser2-01.t catch/throw assertions.

---

HISTORICAL (v1-era, session 263, `docs/intra-sub-goto-plan.md`): v1's
text-stream goto was partial (`_wrap_runtime_labels` paren-archaeology).  The
two Text::Balanced `_match_tagged` blockers (2-pass bucket splice discarded;
pass-dependent flat-vs-two-phase codegen) were v1-machinery bugs — moot for
files v2 lowers natively, still latent only if Text::Balanced itself gates to
v1 (untested).  The "refactor verdict" motivation is exactly what v2/E2
delivers.  Related: [[project_parser2_prototype]].
