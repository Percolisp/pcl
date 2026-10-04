---
name: feedback_perl_i_slurp_truncates
description: "`perl -i -e 'my @l = <>; ... print ...'` TRUNCATES the file to zero bytes — -i restores STDOUT when ARGV is exhausted, so prints after the slurp go to the terminal"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 0cfb3270-d22a-4912-972f-cbf0f09c97d5
  modified: 2026-08-12T20:03:28.132Z
---

**`perl -i` combined with a SLURP (`my @l = <>;`) destroys the file.** `-i`
redirects STDOUT to the temp file only while an ARGV file is *open*; a list-context
`<>` reads every file and then closes ARGV, restoring the real STDOUT. Everything
printed afterwards goes to the terminal, the temp file is empty, and it is renamed
over the original. Measured live in s388 while resolving a merge conflict in
`docs/session-log.md` — 21,874 lines to 0 bytes.

**Why:** the guidance to always reach for Perl (CLAUDE.md) makes `perl -i` the
reflex for whole-file edits. It is safe for `-i -pe` / `-i -ne` (line at a time,
ARGV still open) and unsafe the moment you slurp to reorder or splice.

**How to apply:** for any whole-file rewrite, open the input explicitly, write to
a NEW path, verify it (line count, absence of conflict markers, expected headings),
and only then `cp` it into place — never let `-i` do it:

```perl
perl -e 'open my $in,"<",$f or die; my @l=<$in>; close $in;
         open my $out,">",$ARGV[0] or die; print $out @l[...]; close $out;' /tmp/merged
# verify /tmp/merged, THEN: cp /tmp/merged path
```

Recoverable inside a conflicted merge with `git checkout -m -- <file>` (restores
the conflict markers from the index). Related:
[[feedback_no_stash_when_stash_exists]] — same family: never let a one-shot
command overwrite a live file you have not first produced and checked elsewhere.

**The same hour, the same family, a second time (s388): a marker-splicing
one-liner silently dropped 12 rows from `Pl/t/transpile-test-10.t`, and the gate
still said PASS** — a `pop @h while $h[-1] !~ /done_testing/` walked off the end
of the wrong array. The tripwire was the gate's TEST COUNT: 5096 where 5109 was
expected. So when a merge or rewrite touches a TEST file, a green gate is not
the check — the COUNT is. Verify the rebuilt file against its pre-merge version
first (`diff <(git show main:PATH) rebuilt` must show only the intended hunk),
and check the count before trusting PASS.
