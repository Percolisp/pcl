---
name: project_text_balanced_next
description: NEXT-SESSION start point — Text::Balanced remaining gap (intra-sub goto→tagbody) after the loop-body wantarray fix landed
metadata: 
  node_type: memory
  type: project
  originSessionId: e4ab34f1-bd28-4060-b0ac-524a24fa832a
---

**Start here next session (user said "start with the remaining question").**

Session 262 (commit `49f2c62`) made Text::Balanced's core `extract_*` work by fixing a general loop-body wantarray-context bug (loop-body statements now run void, not the sub's list context — was hanging `_match_bracketed`'s `while(pos<len){ m/\G.../gcs }` tokenizer). Gate 99/3528.

**REMAINING blocker for `extract_tagged`:** `Text::Balanced::_match_tagged` uses **intra-sub `goto LABEL`** — forward gotos to error/exit labels (`goto failed`/`matched`/`short`; labels `failed:`/`matched:`/`short:` near the sub's end). PCL only wraps labels in a CL `tagbody` at **top level** (`_wrap_runtime_labels`, see `docs/state-t-tagbody-goto.md`), NOT inside a sub body. Inside a sub the labels/`(go :X)` are emitted bare → "attempt to GO to nonexistent tag". The hard part: PCL's `my`→nested-`let` codegen vs tagbody's flat-tag requirement (tags must be at the tagbody's own top level, not nested in a `let`). In `_match_tagged` the `my`s are all before the labels and gotos are forward-only, so a tagbody wrapping the body-after-leading-lets is feasible. Plus a secondary `pl-croak` undefined (Carp `croak` not imported into the module's namespace).

**Also noted as lower-priority same-class gaps** (from the loop-body fix): bare blocks `{…}` still set tail_position too broadly (a context-sensitive op as a discarded bare-block tail); scalar `local $x = E if COND` (different branch than the s262 glob fix). Fix only if a real module hits them.

Repro: `./runpcl` a script with `use Text::Balanced qw(extract_tagged); extract_tagged(...)`. Module at `~/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/lib/Text/Balanced.pm`.
