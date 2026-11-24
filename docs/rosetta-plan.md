# The Rosetta plan -- the held-out sample (exit criterion 2)

**Written s543, 2026-11-24, by the reviewing session on the USER's word: "prepare
a plan for Rosetta; start with that seriously next session".  Task #2104.  This
file is the plan and its running state; the rulings are in `docs/DECIDED.md`
(s543, 2026-11-24), the subset's exit criteria in `docs/supported-subset.md`
section 5.**

## 1. Why this, why now

For months the project counted its own bug list and the list did not shrink:
every fix batch probes the neighbouring spellings against perl and files each
difference, so the count measures how hard we look, not how well programs run.
The USER's rulings of 2026-11-24 change the question from "how many
differences can we find" to **"does a program somebody else wrote run right"**:

* exit criterion 1 counts silent wrongs of ORDINARY code only (21 on the day;
  the 64 rare-spelling ones are listed known divergences);
* a neighbour found by a batch's own probes does not extend the batch unless
  it is ordinary;
* **the evidence for "ordinary" is a real program that holds the shape** --
  and the Rosetta corpus is 652 such programs that PCL was never tuned on.

So the corpus does two jobs: it is the measurement of criterion 2, and it is
the source of the next briefs -- a cause that breaks a Rosetta program is
ordinary by definition.

## 2. The corpus

* **What:** the Perl solutions of Rosetta Code (`github.com/acmeism/RosettaCodeData`,
  commit `1d475861d`, 2,239 Perl files), filtered STATICALLY to programs that
  touch no file, process, network, clock or chance, then admitted only if perl
  runs them to completion, deterministically, with output, in under 8 s (the
  s493 selection, `~/pcl-agent-scratch/s493/angles/rosetta/select.pl`): 655.
  Re-admitted on 2026-11-24 by `tools/everyday-smoke.pl --bless-expect` (three
  perl runs each, one from another path): **652** (three refused: two whose
  output depends on where the file is, one perl itself exits 255 on).
* **Where:** `~/pcl-agent-scratch/s543/rosetta/` -- `corpus/rosetta/NNNN-task.pl`
  with its `.expect` (perl 5.40.3's STDOUT) and `.rc`; `out/rosetta/` the
  last run's `.out` / `.err` / `.rc` per program; `run.sh` + `run.log` the run;
  `classify.pl` + `classes.tsv` the first-complaint classes; `refused/` the
  three programs left out.
* **How to run:** `tools/everyday-smoke.pl --corpus ~/pcl-agent-scratch/s543/rosetta/corpus --baseline none --jobs 2 --timeout 60 --outdir DIR`
  as ONE bounded `heavy.sh` leg (six minutes on the day).  It prints
  `EVERYDAY (corpus ...): N of 652 identical to perl`.
* **What it is NOT:** it has no I/O, no processes, no modules beyond the core
  ones a computation uses.  Files, pipes, sockets and CPAN modules are the
  everyday battery's and the board's.  A good Rosetta number says "the
  language core computes right", nothing more.
* **Licence:** Rosetta Code's text is GFDL 1.2.  **No program and no expected
  output is ever checked into this repository.**  Checked in are: the recipe
  that rebuilds the corpus, the cause table (program NAME -> verdict -> cause)
  and the history of the number.

## 3. Where it stands (measured 2026-11-24 on MAIN-READY-110 `26d0dac4`, v2-16000)

**547 of 652 identical to perl (83.9 %).**  On 2026-09-20 (v2-1660) the same
selection read 478 of 655 (73.0 %) with 77 silent.  Nobody worked on the
corpus in between: the 69 programs came from the general fixes.

The 105 that differ, by FIRST complaint (`classify.pl`; a class is where the
narrowing starts, not a cause):

| class | programs | what it is |
|---|---:|---|
| SILENT -- exit status as perl's, nothing on stderr, other output | **43** | the list is in `classes.tsv`; the first work |
| LOUD -- dies where perl does not | 28 | 5 are `try` / `e` / `INFINITY` "not exported" by `bigint` / `bignum` / POSIX; 2 are `List::Util` `max` / `min` undefined; 3 are raw Lisp conditions (an untidy error is a bug of its own); the rest one each |
| announced not-supported | 12 | 11 regex code blocks `(?{...})` (ruled), 1 `use bigrat` |
| statement dropped / refused | 11 | 2 are `given` / `when` (ruled); 2 a Unicode variable name; 7 are drops to narrow |
| module missing | 4 | `Unicode::Normalize` (XS) |
| other output with a PCL notice on stderr | 3 | `$SIG{FPE}`, `local` in an expression position, `Internals::SvREADONLY` |
| timeout at 60 s (perl under 8 s) | 2 | `Cycle-detection`, `Power-set` |
| heap / binding stack exhausted | 2 | |

18 of the 105 are outside the subset as it is written today (11 regex
code blocks, 2 `given` / `when`, 4 XS, 1 `bigrat`), which leaves **87 programs
that are ours**, 43 of them silent.  Those two numbers are the starting line.

## 4. The work, in order (next session)

**Step 0 -- before anything else:** merge whatever of s543l / s543m / s543n is
MERGE-READY (or resume them: `s543/PAUSE-s543.md`, last section), then re-run
the corpus once on that main.  The census starts from that run, not from
this file's table.

**Step 1 -- THE CENSUS (measurement; no fix).  Two agents, the classes split
between them:** agent A takes the 43 SILENT programs, agent B the 44 LOUD /
dropped / notice / timeout / heap ones.  For every program:

1. reduce it to the smallest reproducer that still differs, perl against PCL
   (the program's own text shrunk; never a guess from the diff);
2. grep the task store, `docs/DECIDED.md` and `docs/not-supported.md` BEFORE
   filing -- expect a third or more to be known tasks, several of them among
   the 64 marked `rare`;
3. one task per CAUSE, not per program: class word first, `triage`,
   `frequency: ordinary` with the program's name as the evidence; an existing
   `rare` task that a program holds is MOVED to `ordinary` with that name;
4. a cause outside the subset is keyed on its `docs/not-supported.md` id and
   not filed again.

The census's product is `baselines/rosetta-causes.tsv`: one row per differing
program -- name, class, the cause in the board's grammar (`#N`, `NS:<id>`,
`UNREDUCED #N`).  The bar for "done": every silent program has a cause; every
other one at least a class and a first reproducer; the cause table sums to the
run.  A COUNT is not a finding: each row was re-run for its cause.

**Step 2 -- the instrument (tooling, small, alongside step 1):**
`tools/rosetta-corpus` (the recipe: clone at the pinned commit, select, bless
-- so the corpus can be rebuilt on another machine); `everyday-smoke.pl`
takes the cause table as its `--baseline`, so a run reports NEW / FIXED
against it like the everyday battery does; a history file
`baselines/rosetta-history.tsv` (date, sha, generation, N of M) -- `--record`
refuses a non-default corpus today.

**Step 3 -- THE FIX BATCHES, by cause, largest first.**  Rank the causes by
the number of programs each one breaks; silent before dropped before loud
(the s538 order).  A brief takes a cluster of causes that share a mechanism,
exactly as today's briefs do, with two changes: the members are chosen by
PROGRAMS BROKEN, and every batch quotes `ROSETTA: N of 652` BEFORE -> AFTER
beside its `EVERYDAY:` line.  The probe discipline stays (perl -> base ->
tree, the shape the fix would break), but under the new rule a probe-found
neighbour is filed with its frequency and left unless it is ordinary.

**Step 4 -- beside it, one slot:** the ordinary silent wrongs that no Rosetta
program holds but the board or perl's documentation does (the 21 of the first
pass, less what s543l / s543m / s543n close).

**Step 5 -- the exit, and the truly held-out sample.**  When no Rosetta
program inside the subset is SILENTLY wrong and every other differing one
carries a cause, Rosetta has stopped being held out -- we will have tuned on
it.  Criterion 2 then asks for a FRESH sample, run ONCE: programs by other
authors that nobody here has looked at.  Proposed source, **for the USER to
confirm**: the Perl solutions of the Perl Weekly Challenge
(`github.com/manwar/perlweeklychallenge-club`: thousands of small programs by
hundreds of authors, many of which read arguments and files, so it covers
what Rosetta's filter threw out), selected by the same recipe.  It is fetched
only when step 3 is done, so that it stays unseen.  What it shows decides
whether the freeze ends: a silent wrong in it reopens criterion 1, not the
subset.

## 5. What would change this plan

* If the census finds that most of the 43 silent programs share a handful of
  causes, step 3 is two or three batches and the exit is near.  If it finds
  43 different causes, the tail is long and the USER decides how far down it
  the claim needs to go -- a number such as "95 % identical, no silent wrong
  in the subset" is a legitimate place to stop.
* The two timeouts and the heap cases are speed and memory, which the subset
  file puts outside the correctness claim; they are listed, sized, and not
  first.
* The 18 out-of-subset programs are the cheapest evidence there is for the
  USER's open boundary questions (`docs/supported-subset.md` section 6):
  eleven of them are regex code blocks.
