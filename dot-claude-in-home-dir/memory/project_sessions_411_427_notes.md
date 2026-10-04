---
name: project_sessions_411_427_notes
description: "Compressed findings and traps from PCL sessions s411–s427 (the one-compiler + s420-fan-out era) — PPI clone/anchor, companion-suite attribution, B2 stale index, version-bundle scanner, filetest lexing/semantics; the full narratives are in docs/session-log.md and DECIDED"
metadata: 
  node_type: memory
  type: project
  originSessionId: c1aa8759-138a-41d3-bc3b-a6dd798a7179
  modified: 2026-08-22T13:38:53.447Z
---

Findings from s411–s427, moved out of MEMORY.md to keep the index under its
read limit (s428).  Each is settled and also recorded in `docs/DECIDED.md` /
the named doc / the task store; this file is the at-a-glance copy.  Newer
sessions' state lives on the MEMORY.md STATE/B3/QUEUE lines.

- **A CLONED PPI ELEMENT IS NOT AN ANCHOR** (#414, s420): `PPI::Node::DESTROY`
  walks the tree and empties every descendant hash, so a clone that is a NODE
  goes hollow the moment the caller returns and `content` reads undef.  Anchor
  anything you hand to the parser; a `…:?` in a leaf-emitter die IS that undef.

- **A COMPANION-SUITE "MOVE" IS NOT YOURS UNTIL A BASE WORKTREE SAYS SO** (s420:
  14 of 15 op/uni/mro movers were stale snapshot rows, verified by re-running
  each file on the base commit).  `baselines/perl-suite-run.tsv` drifts silently
  between snapshots — always A/B before attributing.

- **B2 (#343) WAS A STALE INDEX — FIXED s418 by recompute-at-use** (never cache
  a position across a scan that splices its own list).  Record in
  `docs/b2-stale-operand-ceiling-s417.md`; the reg_fold.t lesson: a stale-index
  disagreement was necessary but NOT sufficient for a divergence.  The probe was
  deleted WITH its subject — a structural fix beats a monitor.

- **A VERSION BUNDLE IS NEVER EVIDENCE FOR AN EXPERIMENTAL FEATURE on code that
  COMPILES** (s419: `use v5.38; class Foo;` is a perl SYNTAX ERROR); it stays
  fine at DROP sites.  Two keys, ONE scanner with a `$strict` flag.

- **PPI SPLITS A FILETEST AFTER A SCALAR/BLOCK FILEHANDLE** (`ppi-upstream-bugs.md`
  §22; also [[reference_ppi_lexing_traps]]): `print $fh -e $f` lexes as `-` +
  Word while `print STDERR -e $f` is correct.  ADJACENCY (`next_sibling`) is the
  discriminator and perl honours it too.  `$n -e $b` is a perl SYNTAX ERROR, so
  `-e` is never binary.

- **A FILETEST'S FALSE CARRIES INFORMATION** — perl: DEFINED `""` when the stat
  succeeded, `undef` only when it FAILED.  PCL answers undef for both (**#403**);
  do NOT assert definedness on a filetest until it closes.  Also filed: **#404**
  (perl stacks through PARENS), **#405** (`print $fh -3` writes to `$fh`),
  **#406** (a bareword ARG to a paren-less user sub call is emitted as a call and
  crashes).
