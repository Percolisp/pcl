---
name: project-test-angles-s493
description: "New WAYS OF RUNNING TESTS tried in s493 (USER ask) — the method (perl validates every mutation), each angle's measured yield, where the runners live, what is still to run"
metadata:
  type: project
---

USER (s492 end + s493): "try to find more obvious angles for finding bugs" / "look for new ways of running tests, to find problems. Present them to me after the session."  Fable's own work; fixes go to Opus batches.  Sibling of [[project-everyday-battery]].

**The method that made every angle cheap: PERL IS THE JUDGE OF THE TEST, not only of the answer.**  A mutated / expanded / extracted program is admitted only if perl's stdout + rc for it equal the original's (or `perl -c` accepts it) — nobody proves a transformation correct.

**Where:** `~/pcl-agent-scratch/s493/angles/FINDINGS.md` (every angle, numbers, probes) + `WRITEUP.md` (the presentation); runners `~/pcl-agent-scratch/s493/bin/{diffrun,mkvariants,modtests}.pl`; corpus `~/pcl-agent-scratch/s493/corpus/{orig,dp,dpp,dpx,dppx,sub,block,eval,pkg,strict}`; tasks #2090–#2100 (+ appended #1192, #1929).

**Yield, measured (2026-09-20, main 5316d666):**
- INDEX walk (perlfunc enumerated vs the runtime): 19 probes → 8 gaps (telldir, gethostby*, getservby*, setpriority, formline; `lc` mojibake on bytes without `use utf8`).  perlvar / perlop / perlre / perlrun NOT yet walked.
- ONE-LINERS (perl1line.txt): `pcl` lacks -n -p -l -a -F -i -0; with switches expanded 141/152 same; found `my ($a,@r) = <$fh>` reads one line.
- B::Deparse as a SPELLING MUTATOR: 21–25 of 77 right programs go wrong re-spelled; blockers `@{$x;}` + `"${$}"`; behind them non-leading `local` never restored.
- SCOPE WRAPS (sub/block/eval/pkg/strict): sub/block/pkg/strict solid (1 worse in 411); string eval: `local` on an unqualified package var is lost.
- perl's PODS as a corpus: 1,618 `perl -c`-valid blocks, 98.5 % compile clean; found PPI `0.5-1` mis-lex.
- COMPLEXITY-CLASS battery (N vs 4N): `.=` loop, `shift` drain, `unshift` QUADRATIC; recursion limit ~10k frames, untrappable.
- CORE MODULES' OWN t/ (2,373 files in the perl source tree `~/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/{cpan,dist,ext,lib}`, never run before): ten-dist sample 68.4 % of rows (Getopt::Long 100, Text::Tabs 10, Carp 15, File::Temp 25).
- NAME HYGIENE vs the runtime, generated: 27 of 36 exported p-NAMEs hijack a user sub of that name (`sub flatten`, `sub hash`).
- LOW yield: runtime stub census; CL package/operator names as Perl names (clean).
- Rosetta Code (655 perl-validated pure-computation programs, `angles/rosetta/valid/`): **478 of 655 identical (73.0 %), 77 of the 177 misses SILENT** — census round = task #2104 (not launched).

**How to apply:** `runpcl` takes NO script arguments — use `pcl` in any harness that passes args; never `pkill -f PATTERN` from a shell whose own command line contains PATTERN; dash's `ulimit` takes one option per call; always `| head`/`cut` a PCL stderr (SBCL backtraces are huge).
