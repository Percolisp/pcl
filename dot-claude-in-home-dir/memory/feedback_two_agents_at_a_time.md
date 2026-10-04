---
name: feedback_two_agents_at_a_time
description: USER (2026-09-05, s470) — after the round-28 batch, run at most TWO Opus subjobs concurrently (was three); successors still launch immediately when one finishes, but only while fewer than two are running
metadata:
  type: feedback
---

USER, 2026-09-05 (s470), after a day of three-to-four concurrent agents: "After these,
please try to limit to two at a time. :-)"

**Why:** four concurrent Opus agents plus Fable's own sweeps hit the session rate limit
repeatedly (three cuts in one day, every agent resumed via SendMessage each time), and the
shared box made every bench number a load reading.

**How to apply:** the standing "launch the successor immediately when a subjob finishes"
rule stays, but the cap is TWO running execution agents (one perf agent at a time within
that).  When the four in flight at the time of the ruling (BP, BQ, BS, BR) finish, let the
count fall to one before launching the next brief from the pipeline.  Review/merge/sweep
work by Fable does not count against the two.  Supersedes the "max 3 execution agents"
figure in [[project_s421_opus_agents_inflight]] and the s452 interleaved-plan line.

**Update (USER 2026-09-26, s498):** after Fable asked "two at a time?" the USER first said "run 2
parallel jobs", then "Ah, run three subjobs at a time."  The cap is whatever the USER states at
the start of a session; the default when nothing is said stays TWO.  s498 ran three: s494p
(resumed), s497b, s498c.
