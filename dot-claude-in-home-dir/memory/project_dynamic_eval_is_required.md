---
name: project_dynamic_eval_is_required
description: eval $str_expression (dynamic string eval / load-config-then-eval) is a REQUIRED feature — never gate it as unsupported
metadata: 
  node_type: memory
  type: project
  originSessionId: 5de546c0-c36d-48a2-8086-3d89be0a5915
---

**User directive (2026-07-07):** `eval $str_expression;` — dynamic string eval,
the "load a config file and `eval` its contents" idiom — is unsafe but a **common
idiom that PCL MUST support.** It is NOT a candidate for a not-supported gate.

**Why:** real CPAN/config code reads a file and `eval`s it to populate variables;
refusing it blocks that whole class of programs. The user raised this twice,
firmly.

**How to apply:** dynamic string eval works TODAY (verified 2026-07-07, all v2
native, matches perl):
- eval reads an enclosing lexical: `my $x=5; eval q{$x+1}` → 6
- eval **writes back** an enclosing lexical: `my $x=1; eval q{$x=99}; $x` → 99
- eval sets package vars from a config string: `eval qq{\$Host="h";...}` → works
- eval builds a `%hash` from a config string → works

Mechanism = session-250 lexical capture (`docs/eval-lexical-capture.md`): v1's
`gen_funcall` emits `(p-eval STR (list (cons "$x" $x) …))` from the scoped
`_let_bound_vars`, so eval'd code reads/writes the caller's live `my` scope.

**The only v2-native boundary** (NOT a support gap — these still RUN, via v1
whole-file fallback which supports dynamic eval): the W10 spanning-rename used to
mangle a captured lexical (`$x__file__N`), invisible to eval-by-name → it gated
the file. Fixed for the file-UNIQUE case by unmangle-when-unique (commit 793563a,
[[feedback_dont_write_off_fixable]]): rename to the plain `$Pkg::name` global so
eval'd code resolving `$x` hits the same cell. A NON-unique spanning name in a
package block + dynamic eval still routes to v1 (works, just not native yet).
Never confuse "routes to v1" with "unsupported."
