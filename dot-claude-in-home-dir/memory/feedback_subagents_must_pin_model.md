---
name: feedback_subagents_must_pin_model
description: "Agent-tool subagents INHERIT the parent's model (Fable!) unless an explicit model override is passed — round agents must be launched with model \"opus\" (or \"sonnet\" for prose/mechanical work)"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: b336d422-2ee9-424d-a82f-c1c6df955088
  modified: 2026-08-30T13:13:37.908Z
---

Round 12 (s454, 2026-08-30) was launched from a Fable session as three
`general-purpose` agents WITHOUT a `model` override.  A subagent with no
model pin inherits the PARENT's model, so all three "Opus agents" ran on
**Fable 5** and burned ~1.01M tokens = 19% of the USER's weekly Fable
allotment in ~30 minutes.  The USER stopped the round over it.

**Why:** the project's division of labor ("Fable plans/reviews, Opus
executes") lives in prose; the Agent tool only honors it through the
explicit `model` parameter.  Omitting it silently upgrades every agent to
the launcher's (premium, weekly-capped) model.

**How to apply:** EVERY Agent launch from a Fable session passes an explicit
`model`: `"opus"` for round execution agents, `"sonnet"` for prose/doc/
mechanical tasks (standing USER ruling).  Never rely on inheritance.  Also
prefer serial launches (one agent, review spend, then the next) after this
event unless the USER re-authorizes parallel.  Related: [[project_s421_opus_agents_inflight]].

**Update (USER 2026-09-29, s501): "Always use Opus 5.5, not Opus 5."**  Execution agents run on
**Opus 5.5 (`claude-opus-5-5`)**.  The Agent tool's `model: "opus"` alias is the only pin it offers, so
every brief makes the agent's FIRST ACTION writing its exact model id to
`$W/scratch/<label>/MODEL.txt`; Fable reads that file a minute after launch and stops any agent that
is not on Opus 5.5.  Where older memory/docs say "Opus 5 agents", read Opus 5.5.
