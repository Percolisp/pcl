---
name: feedback-speed-over-readable-lisp
description: User decision 2026-07-02 — generated-code SPEED outranks readable Lisp; CLAUDE.md principle 2 amended
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 657169b8-1488-43d3-ad7f-5e5daa0e21a9
---

User (2026-07-02, codegen-rewrite discussion): making arbitrary code —
especially CPAN modules — run fast "is more important than readable Lisp."

**Why:** the rewrite's go/no-go bar is "generated code at least as fast as
native Perl"; readable output was a design nicety, not a requirement.

**How to apply:** never reject or water down a codegen/runtime speed
transform because the output gets uglier (mangled names, type decls
everywhere, fused loops, inline caches). Keep Perl-like naming only where it
costs nothing. CLAUDE.md principle 2 was amended in-repo to say this.
The speed menu lives in `docs/where-the-time-goes.md` §5; decision doc is
`docs/codegen-rewrite-review.md`. Related: [[project-codegen-rewrite-review]].
