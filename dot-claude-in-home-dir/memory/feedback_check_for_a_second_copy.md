---
name: feedback-check-for-a-second-copy
description: "When behaviour is subtly wrong, look for a SECOND, weaker copy of the mechanism before debugging the one you found — three such bugs in one session"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 66d9b237-b64d-46e1-981b-5f3faca5611a
  modified: 2026-08-01T22:05:30.563Z
---

**s321 hit this three times in one session.** Each was a case where a correct
implementation already existed a few files away, and a second, weaker copy was
the thing actually running:

1. **#177** — `tools/run-perl-suite.pl` joined the two TAP streams by test
   NUMBER. The sweep's `sweep-diff.pl` had always keyed rows by DESCRIPTION.
   The number join blamed rows that pass.
2. **#179** — `pl-like`/`pl-unlike` in `cl/pcl-test.lisp` built their own
   scanner with raw `ppcre:create-scanner`, so `like`/`unlike` judged patterns
   by different rules than `=~` (which goes through `%pcl-build-scanner` and
   its workarounds).
3. **#182** — the `s///` replacement was built by a hand-rolled
   mini-interpolator instead of the real double-quoted-string parser. It was
   missing FOUR whole classes (subscripts, `@array`, `${digit}`, punctuation
   vars), two of them live in the shipped corpus.

**The tell:** the same construct works in one context and not another —
`"$h{$1}"` correct but `s/…/$h{$1}/` not; `=~` correct but `like` not. That
asymmetry means two implementations, not one buggy one.

**What to do:** before debugging the path you found, grep for a sibling that
does the same job (`create-scanner`, `interpolat`, the join/compare) and ask
which one the failing case actually uses. The fix is then almost always to
delete the copy and route through the original (CLAUDE.md 11), which is both
smaller and fixes cases nobody reported — #182's delegation closed three bugs
beyond the one filed.

**Corollary: read the emitted CL early.** For #182 two rounds of probing gave
a wrong theory (UTF-8 byte expansion); one look at
`(p-string-concat $h "{" $1 "}")` settled it instantly. Related:
[[feedback_ast_vs_string_matching]], [[feedback_reuse_dont_duplicate]].
