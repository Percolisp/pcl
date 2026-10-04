---
name: project_cpan_debugging_horizon
description: "Strategic state of the CPAN-module-driven debugging effort — expected duration, why, and approach"
metadata: 
  node_type: memory
  type: project
  originSessionId: eeab055f-63ef-4cda-95e9-edd8799a488c
---

As of session 240 (2026-06-09) the user expects the CPAN-module-driven debugging
phase to run **a few weeks longer than originally thought.** Driving `Moo`
end-to-end surfaced six *general, basic* bugs in one session (nested ternary w/o
parens, `CORE::` builtins, `package` in `BEGIN`, single-seg pkg casing in
`caller`, `${EXPR}` method, scheduled-block scoping) — none Moo-specific.

**Why this is the right driver, not a detour:** real modules act as broad fuzzers
that finally exercise language corners the copied `perl-tests/` suite skips (e.g.
`perl-tests/cond.t` never tests the `?:` operator at all). Each fix is a general
correctness fix that pays forward to every future module. Moo shares huge surface
with Moose / Type::Tiny / Test2, so its fixes compound.

**Two caution flags on the estimate:**
1. Parse/codegen bugs (most of session 240) are fast point fixes. But the
   **eval-lexical-capture / Moo self-bootstrap** family (MGC `assert_constructor`,
   Sub::Quote capture, CMM) is *semantic* — coderef identity, `weaken`,
   string-eval scoping — and needs design, not point fixes. Slower.
2. We find these reactively. The user's repeated instinct (flagged 240): the
   **test suite has coverage holes** — a *proactive* probe-generating pass over
   operator precedence/associativity, `CORE::` builtins, context, and OO dispatch
   would flush the cheap parse/codegen bugs faster than one-module-at-a-time, and
   leave permanent regression tests. Consider proposing this alongside Moo work.

See [[project_cpan_module_survey]], `docs/cpan-module-blockers.md`.
