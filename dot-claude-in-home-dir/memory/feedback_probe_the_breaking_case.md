---
name: feedback_probe_the_breaking_case
description: "When widening a parser rule, probe the case the rule would BREAK, not only the cases it fixes — the user caught a call→string regression this way (PCL, 2026-08-01)"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 1511008a-7c9e-41d8-9abe-ad6dadcc48fa
  modified: 2026-08-01T10:17:33.630Z
---

When widening a **classification rule** in the parser (bareword-vs-call,
operator-vs-string, declarator-vs-word), the probes that matter are the ones
the widened rule would now *capture wrongly* — not the ones it fixes.

**Why:** s316v, I widened PExpr's `_bareword_string` so any qualified
`Foo::Bar` with no known sub became a string, to make `tie %h, Tie::StdHash`
work. I probed the fixed cases and one safety case (a sub declared *later*,
which perl also strings) and reported it as safe. The user pushed back —
"Uhh, are you going to treat `Foo::Bar` as a string?!" — and the case I had
not probed was the one that broke: a sub declared *earlier*.

```perl
no strict;
package Foo; sub init { "CALLED" } package main;
my $a = Foo::init;    # perl: CALLED    my change: "Foo::init"
```

The guard I trusted (`has_prototype($sub_name)`) keys on the LITERAL declared
name: `sub Bar::direct` registers as `Bar::direct`, but `package Foo; sub
init` registers as `init`. So the "is it a known sub?" test answered *no* for
a sub that plainly exists, and real calls silently became strings — the same
failure class as [[project_parser2_prototype]]'s silent statement deletion.

**A probe agreeing is not the change being safe.** Two of the three failed
attempts had *all probes green*. The second was killed by `corpus-diff`:
the method invocant `Count::DATA->getline` silently lost its
`(p-resolve-invocant …)` wrapper — perl resolves class-vs-filehandle at
runtime, and hard-wiring the class deletes that. The probe printed the same
string either way because no filehandle was in play. For a codegen change,
**corpus-diff is the oracle, not the probe**: run it before believing
anything.

**How to apply:**
- Before widening a rule, write down what it now captures that it did not,
  and probe the *most ordinary* member of that set — not an exotic one.
- Distrust a "known symbol" oracle until you have checked what key it stores.
  Ask it about the shape you are about to gate on, in both spellings.
- A silent call→string / statement-drop conversion is never an acceptable
  risk to take on a hunch; prefer reusing the sibling mechanism for the
  narrow position (bless's class-name branch for `tie`) over a global rule.
- Reverting immediately and filing the finding beats defending the change.
- **Set an attempt budget and honour it.** I said "one more attempt, then
  revert regardless" and meant it; three variations each died to a different
  thing my model of the code did not contain. That is the signal that the
  path is not understood well enough to change safely — stop and write the
  findings up (task #142 carries all three failure modes), rather than
  learning the code by iterating on main during a release window.
