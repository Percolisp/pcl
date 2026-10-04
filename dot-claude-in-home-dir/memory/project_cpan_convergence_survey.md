---
name: project_cpan_convergence_survey
description: "NEXT DIRECTION (user, s253c 2026-06-15): survey MORE CPAN modules to test whether problems CONVERGE to a finite shared set rather than each module finding a brand-new bug. User's fear = an infinite tail of problems. Moo (roles+modifiers) reduced to a handful of GENERAL bugs that generalized, which supports the convergence hope."
metadata: 
  node_type: memory
  type: project
  originSessionId: c70190c6-b5c1-4c9e-bad0-9177f33b61c0
---

# NEXT DIRECTION — CPAN convergence survey (user, session 253c, 2026-06-15)

> **UPDATE (s259, 2026-06-19): a concrete release-oriented plan now exists —
> `docs/cpan-release-plan.md`.** Decisions locked: release bar = a curated CPAN
> corpus (Moo ecosystem + breadth: Carp/Data::Dumper/JSON::PP/Test::Deep/PPI)
> passes its own `t/` suites; `pcl` runner is post-v1; DESTROY-via-GC = spike
> scope-exit finalizers. Phase order: 0 scoreboard (`tools/cpan-scoreboard.pl`),
> 1 per-statement `handler-case`, 2 buckets (builtin-name-collision FIRST →
> wantarray ctx → FASL double-exec → codegen node gaps), 3 DESTROY spike, 4
> fuzzer. Read `docs/cpan-release-plan.md` first, then the buckets below.

After Moo came down to a handful of GENERAL fixes (not Moo-specific hacks), the
user wants to **check more CPAN modules to see whether the problems CONVERGE** —
i.e. that new modules keep hitting the SAME finite set of underlying bugs, not an
unbounded stream of brand-new ones. The fear is "an infinite number of problems";
the evidence so far (roles + method modifiers reduced to ordering + eval-capture +
a few precedence/ref bugs that each helped beyond Moo) supports convergence.

## How to run the survey
- Pick a spread of pure-Perl (XS-free) CPAN modules — prefer widely-depended-on
  ones (the Moo/Moose ecosystem, Try::Tiny, Type::Tiny, Path::Tiny, JSON::PP,
  Data::Dumper-likes, etc.). Network/CPAN IS reachable (see
  [[project_network_and_cpan_available]]); cpanm works.
- For each: transpile + run a tiny exercise vs perl 5.40 (`./runpcl`), record the
  FIRST failure's root cause.
- **Bucket the root causes.** Convergence = the same buckets recur. Track which
  bucket each module lands in; a histogram tells us if the tail is finite.

## Known recurring buckets so far (the shared set to watch)
1. **Identifier collisions with CL builtins** — `package Car`→`car`, `has log`,
   `list`, etc. → SYMBOL-PACKAGE-LOCKED-ERROR. **Highest-impact open item**
   (a general name-mangling gap, distinct from s252 case-collision). Fixing this
   likely clears many modules at once → strong convergence signal.
2. Declaration ordering / compile-time stream (FIXED s253b).
3. eval-string capture + feature-probe-by-eval-die (FIXED s253b/c).
4. Module compile-load double-exec (worked around `*pcl-cache-fasl* nil`; perf).
5. Scalar SV-identity / sparse-array holes (documented not-supported).

## Why this matters
If module N's first bug is always in buckets 1–5, the work is BOUNDED: fix the
buckets, not the modules. If each module finds a NEW bucket, the tail is long.
The survey measures which world we're in. Start with the builtin-name-collision
fix (bucket 1) since it's the most likely shared blocker, then re-survey.

See [[project_moo_progress]], `docs/moo-status.md`, [[project_cpan_module_survey]].
