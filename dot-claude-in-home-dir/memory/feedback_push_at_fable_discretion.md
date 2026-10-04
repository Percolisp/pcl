---
name: feedback-push-at-fable-discretion
description: USER s494 — pushing to origin is at Fable's discretion ("push when you think you should"); an earlier "don't push" was about one trivial README commit, not a standing hold
metadata:
  type: feedback
---

USER, mid-s494 (2026-09-21): "I just meant that it wasn't worth the trouble to push the little
README change - last session. :-) Please push when you think you should."

**Why:** s493 ended with "Don't push the README, we end the session", which I recorded as a
standing hold and then ASKED about again in s494 ("Hold it") — the USER meant only that one
small commit was not worth a push on its own.  I over-read a one-off remark into a rule.

**How to apply:** push finished, gated work without asking — it is the [[feedback_put_live_before_new_work]]
rule in practice.  Before a push: the bar that matches WHAT CHANGED (CLAUDE.md table); for a
docs / baselines / tools-test batch that is the gate files that read docs + the license and
hardcoded-path guards, not the full gate.  NEVER include the USER's uncommitted README.md edit
(check `git status` / `git diff` first; add files by name).  Read CI afterwards through the
public API.  A one-off "not now" from the USER is about THAT moment unless they say otherwise —
ask what was meant before writing it down as a standing rule.
