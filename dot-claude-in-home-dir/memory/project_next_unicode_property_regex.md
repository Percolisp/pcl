---
name: project_next_unicode_property_regex
description: "NEXT SESSION starts here — implement \\p{...} Unicode-property regex support"
metadata: 
  node_type: memory
  type: project
  originSessionId: ffb7102d-f287-4e69-9818-4032bc52d8d8
---

**Start the next session by implementing `\p{...}` / `\P{...}` Unicode-property
regex support.** User explicitly chose this as the next task (2026-06-23).

**Why:** convergent root cause — cl-ppcre treats `\p`/`\P` as literals, which
breaks Text::Tabs (`expand`/`unexpand` use `( () = /\PM/g )` for column width)
and Text::Wrap (wrap regex `\PM\pM*` → `die "This shouldn't happen"`). Also
closes the documented `\p{IsWord}` gap in `docs/not-supported.md`.

**Full ready-to-execute plan:** `docs/unicode-property-regex-plan.md`. Key
points: set `cl-ppcre:*property-resolver*` to a fn backed by
`sb-unicode:general-category`; cl-ppcre's resolver only fires for the braced
form, so also normalize shorthand `\pL`→`\p{L}` inside `perl-regex-to-ppcre`
(`cl/pcl-runtime.lisp`); cl-ppcre handles `\P` negation itself (resolver returns
only the positive predicate). New test `Pl/t/regex-unicode-prop-01.t`.

Builds on session 2026-06-23 (commit 62aaf7b) which fixed print-FH-vs-list,
`=~` no-match→'', and `pcl -M=imports`. See [[project_cpan_module_log]].
