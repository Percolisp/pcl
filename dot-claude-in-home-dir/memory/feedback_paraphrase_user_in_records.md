---
name: feedback_paraphrase_user_in_records
description: In the published records (DECIDED, session-log, PAUSE notes, tasks, memory) PARAPHRASE the USER's decisions; quote their words only when the wording itself is the technical content
metadata:
  type: feedback
---

**Rule (USER, 2026-10-17, s518):** when a USER decision goes into a published record (`docs/DECIDED.md`, `docs/session-log.md`, the `briefs-and-rules-for-claude-subagents/` notes, the task store, memory -- all in the public repo), write the DECISION in plain words ("stays unsupported, not planned"); do not quote the USER's casual phrasing or jokes verbatim.  Quote exact words only when the wording carries technical meaning (a number, a name, a rule's precise boundary).

**Why:** the USER noticed a joking idiom of theirs quoted word for word in DECIDED and said they would have to joke less if that continues -- the records are published, and a quote there is a quote to the world.

**How to apply:** before committing a record that cites the USER, read the citation: if removing the quotation marks loses nothing technical, paraphrase.  Earlier verbatim quotes of casual phrasing may be paraphrased when touched.
