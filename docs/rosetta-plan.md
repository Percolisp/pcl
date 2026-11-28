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
  and the Rosetta corpus is 650 such programs (652 until s543o) that PCL was never tuned on.

So the corpus does two jobs: it is the measurement of criterion 2, and it is
the source of the next briefs -- a cause that breaks a Rosetta program is
ordinary by definition.

## 2. The corpus

* **What:** the Perl solutions of Rosetta Code (`github.com/acmeism/RosettaCodeData`,
  commit `1d475861d`, 2,239 Perl files), filtered STATICALLY to programs that
  touch no file, process, network, clock or chance, then admitted only if perl
  runs them to completion, deterministically, with output, in under 8 s (the
  s493 selection): 655.  Re-admitted on 2026-11-24 by
  `tools/everyday-smoke.pl --bless-expect` (three perl runs each, one from
  another path): 652 (three refused: two hash-order nondeterministic -- the
  bless said "depends on where the file is" because its third run was the one
  that differed -- and one perl itself exits 255 on under `LC_ALL=C`).  Since
  s543o both steps are one recipe, `tools/rosetta-corpus build`, and the corpus
  is **650**: two of the 652 were broken probes (section 3).
* **Where:** outside every checkout, in `PCLPaths::rosetta_dir` --
  `$PCL_ROSETTA_DIR` when set, else `~/.pcl-cache/rosetta` (beside the module
  cache; `pcl --clear-cache` leaves it alone, a hand `rm -rf ~/.pcl-cache/*`
  does not, and `build` makes it again).  It holds `corpus/rosetta/NNNN-task.pl`
  with its `.expect` (perl's STDOUT) and, for a non-zero exit, `.rc`;
  `refused/` what the admission refused, with `REASONS.tsv`; `work/` the clone;
  `BUILD-INFO` and `build.log`.  On the development box it holds s543o's
  verified build (2026-11-27).  Until s543o the corpus existed only as
  scratch files built by hand (`~/pcl-agent-scratch/s493/angles/rosetta/select.pl`,
  then `~/pcl-agent-scratch/s543/rosetta/`, which holds the 652 and so fails
  `verify` by the two broken probes of section 3).
* **The recipe -- `tools/rosetta-corpus`** (s543o; the rules in
  `tools/lib/PCLRosetta.pm`):
  `build` fetches the pinned commit (`1d475861d7141ef2fafff293d9339f56e4b0f5db`;
  a shallow, blob-less, sparse fetch of `/Task/*/Perl/*`), selects STATICALLY
  with the s493 word filter (verbatim; a candidate's number is its place among
  the candidates in sorted path order), and admits through
  `tools/everyday-smoke.pl --bless-expect --timeout 8` under `ulimit -v 4000000`
  (three perl runs, one a copy at another path from another cwd), refusing
  what printed nothing or more than 200,000 bytes, then re-runs every admitted
  program once, minutes later (the runner's `--verify-with-perl`): a DRIFT is an
  answer that holds the clock or chance, and is refused.  A REBUILD (the
  manifest describes the commit) decides two things the definition decided
  once: the programs the manifest names get the runner's 60 s, not 8 (the 8 s
  rule is a wall clock: 0478 runs 7.8 s here and timed out at load 15), and a
  program admitted here that the manifest does not name is refused with that
  reason (three URL programs use URI, installed here after s493; 0574 0896
  1107 1150 run 4.8-6.6 s on a quiet box, and the clock refused them at s493
  and at load 15) -- THE MANIFEST IS THE CORPUS.  `verify` compares a corpus
  with the MANIFEST `baselines/rosetta-corpus.tsv` (one row per program: name,
  sha256 of the `.pl`, sha256 of the `.expect`, the exit status -- names and
  digests only); `classify` reads a run's outdir and prints each differing
  program's verdict, line and CLASS (section 3's eight).
* **How to run:** `tools/rosetta-corpus run` -- as ONE bounded `heavy.sh` leg
  (3 min 48 s at `--jobs 2` on MAIN-READY-111).  It verifies the corpus against the
  manifest (and refuses to run one that differs), then runs
  `tools/everyday-smoke.pl` on it with `--label ROSETTA`, the cause table
  `baselines/rosetta-causes.tsv` as `--baseline`, `--history
  baselines/rosetta-history.tsv`, `--jobs 2 --timeout 60 --outdir <root>/.rosetta`;
  runner options after it win (`--record`, `--jobs`, a program).  The line a
  batch quotes, BEFORE -> AFTER beside `EVERYDAY:`, is the last one:
  `ROSETTA: N of 650 identical to perl (P %) -- <sha> gen <generation>`, and
  the buckets are the battery's (NEW = a regression; FIXED = the row leaves by
  edit).  `--record` on a clean tree appends a row to the history.
* **What it is NOT:** it has no I/O, no processes, no modules beyond the core
  ones a computation uses.  Files, pipes, sockets and CPAN modules are the
  everyday battery's and the board's.  A good Rosetta number says "the
  language core computes right", nothing more.
* **Licence:** Rosetta Code's text is GFDL 1.2.  **No program and no expected
  output is ever checked into this repository.**  Checked in are: the recipe
  that rebuilds the corpus, the manifest of names and digests, the cause table (program NAME -> verdict -> cause)
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

**s543o (2026-11-27): THE CORPUS IS 650.**  Rebuilding it found two of the
105 to be broken probes, not PCL's: `0359-Distinct-palindromes-within-decimal-numb`
prints one of two outputs, half and half (hash order; its five admission runs
agreed by chance), and `1442-Text-to-HTML`'s expected output holds the date of
its bless (its three runs fell inside one second) -- a SILENT row no fix could
ever clear.  Both left the manifest and the cause table by edit.  On
MAIN-READY-111 the corpus reads **547 of 650**; the starting line is **86
programs that are ours, 42 of them silent** (17 out of the subset: 10 regex
code blocks, 2 `given` / `when`, 4 XS, 1 `bigrat`), each a row of
`baselines/rosetta-causes.tsv` with its class.

## 4. The work, in order (next session)

**Step 0 -- before anything else:** merge whatever of s543l / s543m / s543n is
MERGE-READY (or resume them: `s543/PAUSE-s543.md`, last section), then re-run
the corpus once on that main.  The census starts from that run, not from
this file's table.

**Step 1 -- THE CENSUS (measurement; no fix).  Two agents, the classes split
between them:** agent A takes the 43 SILENT programs, agent B the 44 LOUD /
dropped / notice / timeout / heap ones (42 and 44 since s543o: 1442 left the
corpus).  For every program:

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
program -- name, verdict, first differing line, class, the cause in the
board's grammar (`#N`, `NS:<id>`, `UNREDUCED #N`).  It is ALSO the run's
baseline, so it has the runner's columns (name, verdict, line, cause) with the
class inserted before the cause (s543o; the format is in the file's header).
s543o seeds it: one row per differing program, its class from `classify`, its
cause `UNREDUCED #2104` (the 18 outside the subset keyed on their
not-supported id); a census agent edits the cause column row by row, and
`Pl/t/verdict-register-01.t` checks every key, the five fields, the class and
the name.  The bar for "done": every silent program has a cause; every
other one at least a class and a first reproducer; the cause table sums to the
run.  A COUNT is not a finding: each row was re-run for its cause.

**Step 2 -- the instrument (tooling, small, alongside step 1):**
`tools/rosetta-corpus` (the recipe: clone at the pinned commit, select, bless
-- so the corpus can be rebuilt on another machine); `everyday-smoke.pl`
takes the cause table as its `--baseline`, so a run reports NEW / FIXED
against it like the everyday battery does; a history file
`baselines/rosetta-history.tsv` (date, sha, generation, N of M) -- `--record`
refuses a non-default corpus today.  **DONE (s543o, 2026-11-27; the tool landed
in `25b73daf`)**: section 2 is the recipe as built; the cause table is seeded
(103 rows, the census edits its cause column); `tools/rosetta-corpus run
--record` writes the history.

**Step 3 -- THE FIX BATCHES, by cause, largest first.**  Rank the causes by
the number of programs each one breaks; silent before dropped before loud
(the s538 order).  A brief takes a cluster of causes that share a mechanism,
exactly as today's briefs do, with two changes: the members are chosen by
PROGRAMS BROKEN, and every batch quotes `ROSETTA: N of 650` BEFORE -> AFTER
beside its `EVERYDAY:` line.  The probe discipline stays (perl -> base ->
tree, the shape the fix would break), but under the new rule a probe-found
neighbour is filed with its frequency and left unless it is ordinary.

* batch 1 (s543t, 2026-11-28): overloading as the big-number classes use it
  and the bigint / bignum shims -- #5465 (a handler's scalar context), #5468
  (an array operand's count), #5467 (`int`), #2560 (= #932's sub half: the
  capture variables are scoped to the sub frame), #5500 (the import list),
  #5501 (the assignment keys; with #5544, the `=` copy constructor) --
  fourteen programs, **ROSETTA 547 -> 561 of 650**, buckets 0.
* batch 2 (s543w, 2026-11-28): the lexer's term position -- #650 (a word list
  written as a glob: PPI groups BJ BK), #5508 (an anonymous hash after an
  operator: BL), #5509 (`CONST*$var`: the prototype table), #1292 (a glob of
  three interpolation pieces): eight programs, **ROSETTA 561 -> 569 of 650**
  (547 -> 555 on its own base), buckets 0; the eight programs 0473
  1011 1364 0316 0422 0493 0556 0445 `same`; 0119 0649 0710 keyed on what they
  still hold (#5502, #5560, #5507).
* **batch 3 (s543u, 2026-11-28): regex escapes** -- `\K` #5560 (0107 0774 0818
  0838 0923 0925 0927 0933 1050 1298 1306 0266), `\G` in s/// #5461 + a list
  `\G//gc` keeps pos #5462 (0996 1297, and 0071: #5462 was its visible half,
  #5460 no longer shows in it), `\b{wb}` #5561 (1561), and 0649 (its last cause after s543w was `\K`): SEVENTEEN
  programs `same`; 0119 0710 keep their other cause (#5502, #5507): **ROSETTA 569 -> 586
  of 650** on top of batches 1 and 2, buckets 0 (the session log, s543u).

**Step 4 -- beside it, one slot:** the ordinary silent wrongs that no Rosetta
program holds but the board or perl's documentation does (the 21 of the first
pass, less what s543l / s543m / s543n close).

**Step 5 -- the exit, and the truly held-out sample.**  When no Rosetta
program inside the subset is SILENTLY wrong and every other differing one
carries a cause, Rosetta has stopped being held out -- we will have tuned on
it.  Criterion 2 then asks for a FRESH sample, run ONCE: programs by other
authors that nobody here has looked at.  The source, **accepted by the USER
(2026-11-24)**: the Perl solutions of the Perl Weekly Challenge
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

## 6. The census (step 1)

### 6a. The silent programs (s543p, 2026-11-27, CODE `496df8b1` = MAIN-READY-113)

**42 programs, 21 causes, 10 of them known before the census** (8 open tasks
and 2 not-supported ids); **0 FIXED-BY a merge** since the -111 run (all 42
re-run after s543m's merge: unchanged).  Every program was re-run, reduced to
a neutral reproducer that still differs, and its cause confirmed by a
perl-equivalent rewrite in a scratch copy that makes the program identical to
perl (two programs hold two causes: 0071, 0195).  No program is UNREDUCED.
Program names only (GFDL: no text, no output); the reproducers are in the
tasks.

| cause | class | programs | names | task | known before | IN / OUT | with |
|---|---|---:|---|---|---|---|---|
| `\K` (11) and `\b{wb}` (1) never match: escapes passed to cl-ppcre untranslated | SILENT | 12 | 0107 0774 0818 0838 0923 0925 0927 0933 1050 1298 1306 1561 | #5560 (`\K`, 11) #5561 (`\b{wb}`, 1); was `NS:ns-regex-escapes` | yes (ns) | **IN (ruled by the reviewing session 2026-11-28, DECIDED s543: the subset's "regexes within cl-ppcre's reach"; the ns row was an audit, not a ruling; flagged to the USER)** | #2381 #2440 #1494 |
| an overloaded operator's handler runs in the caller's (dynamic) LIST context: Math::BigInt/BigRat `/` returns quotient and remainder | SILENT | 3 | 0195 0567 1473 | #5465 | new | IN | #5468 #5467 #2560 |
| a slice as the operand of a modifying scalar op (`@h{LIST}++`, `undef @h{LIST}`, `@$r[$i] += x`) does nothing | SILENT | 3 | 0243 0808 0887 | #5466 | new | IN | #5464 #5469 |
| `\X` is base + combining marks, not UAX #29 (flags, ZWJ emoji, Hangul jamo) | SILENT | 3 | 1178 1342 1376 | #2381 | yes (moved to ordinary) | IN | ns-regex-escapes |
| integers are unbounded: no 64-bit wrap, no NV past 2**64, `%d` of a UV | SILENT | 3 | 0477 0672 1479 | `NS:ns-integers-unbounded` | yes (ns, by design #1513) | OUT (by design) | -- |
| `Math::BigRat->new("N/D")` is 1 | SILENT | 2 | 0195 0545 | #2560 | yes (ordinary) | IN | #5465 |
| `\G` in a substitution is a literal `G` | SILENT | 2 | 0071 0996 | #5461 | new | IN | #5462 (rare) #5460 |
| `int($obj)` ignores the class's `int` overload (BigInt becomes a plain number) | SILENT | 2 | 0351 1122 | #5467 | new (#1005 left `int` out) | IN | #5465 #5468 |
| `\N{NAME}` is not implemented (literal text) | SILENT | 2 | 0670 1436 | #1494 | yes (ordinary) | IN | ns-regex-escapes |
| a non-last comma operand in void / scalar context runs in LIST context | SILENT | 1 | 0071 | #5460 | new | IN | #5462 (rare) |
| a bare builtin (`length`) as an array subscript is autoquoted to index 0 | SILENT | 1 | 0129 | #5463 | new | IN | #246, #1285 |
| an ELEMENT `+=` a slice adds the slice's count | SILENT | 1 | 0190 | #5464 | new | IN | #5466 |
| printf float conversions drop the sign of `-0.0` | SILENT | 1 | 0442 | #1475 | yes (moved to ordinary; #2532 item 1 is the same) | IN | -- |
| POSIX bracket classes `[[:lower:]]` etc. are ASCII ranges | SILENT | 1 | 0641 | #2440 | yes (ordinary) | IN | ns-regex-escapes |
| `f { 'key' => ... }` with a no-prototype sub passes the block's value, not an anon hash | SILENT | 1 | 0903 | #1285 | yes (moved rare -> ordinary) | IN | #5463 |
| an ARRAY operand of an overloaded op is its first element, not its count (`use bigint`) | SILENT | 1 | 0904 | #5468 | new | IN | #5465 |
| `seek DATA, 0, 0` rewinds to the data section, not the source file | SILENT | 1 | 1118 | #1716 | yes (moved to ordinary) | IN | -- |
| a slice assignment from the same whole array reads it while writing | SILENT | 1 | 1198 | #5469 | new | IN | #5466 |
| without `use utf8`, a high byte after an interpolated `$name` is read into the name | SILENT | 1 | 1336 | #5470 | new | IN | -- |
| a no-initialiser `my` captured by a `my sub` keeps the first call's value | SILENT | 1 | 1432 | #2644 | yes (moved to ordinary) | IN | -- |
| `ref(\$s)` is SCALAR (perl REF) for a package `$s` holding a ref to a named aggregate | SILENT | 1 | 1480 | #5471 | new | IN | -- |

Found on the way and filed as `rare` (no program depends on it alone):
#5462, a list-context `\G` match ignores `/c` (0071 reaches it only through
#5460).  The fix batches this table suggests, largest first: **overload
dispatch** (#5465 #5468 #5467, with #2560: 7 programs), **the slice family**
(#5466 #5464 #5469: 5), **regex escapes and Unicode** (#2381 #2440 #1494 and
the `\K` / `\b{wb}` decision: 6 IN + 12 if `\K` comes in), **`\G` and
context** (#5461 #5460 #5462: 2), then the singles.

### 6b. The loud, dropped and resource programs (s543q, 2026-11-28, CODE `6a1301ed` = MAIN-READY-114)

**44 programs, 30 causes, 11 of them known before census B** (6 older open
tasks, 4 filed the same day by census A and the reviewing session, 1
not-supported id); **19 new tasks, #5500-#5518; 0 FIXED-BY a merge** (all 44
re-run on s543m's tree, the `local` / `%SIG` ones again after s543l's merge:
unchanged); **0 UNREDUCED**.  Every program was re-run, reduced to a neutral
reproducer, and its cause CONFIRMED by a perl-equivalent rewrite of that
spelling in a scratch copy that makes the program identical to perl; four
programs hold two or three causes (0119 0649 0710 1093), so the table counts
50 program-causes.  The two TIMEOUT programs are NOT slow: both are endless
loops of a correctness cause (#5514, #5516) -- no `SLOW:` task; the two
EXHAUSTED ones were sized through one bounded leg (#1929, #5518).  Six
programs end in a raw Lisp condition: that FORM is #1427's (and #2556's for
the two resource deaths), the cause is in the table.  Program names only
(GFDL); the reproducers are in the tasks.

| cause | class | programs | names | task | known before | IN / OUT | with |
|---|---|---:|---|---|---|---|---|
| a word list written as a glob, `<a b>`: after `use Module` it imports nothing (PPI: an operator; PCL's rebuild drops the spaces and makes a readline), after `[` a spaced `< a b >` drops the statement | LOUD 6, DROPPED 2 | 8 | 0119 0316 0422 0473 0649 0710 1011 1364 | #650 | yes (moved rare -> ordinary) | IN -- PPI: **needs the USER's word** (the PPI program is closed); the import half has a few-line PCL fix | #1292 #5508 |
| `use bigint try => LIB` / `lib =>` and `use bignum qw(e PI)`: the shims hand every import argument to Exporter and die | LOUD | 4 | 0033 0715 0998 0161 | #5500 | new | IN | s543t's batch |
| `\K` never matches (here the 2nd / 3rd cause of three LOUD programs, the only stdout cause of 0266) | LOUD, NOTICE | 4 | 0119 0266 0649 0710 | #5560 | yes (census A: 11 silent programs) | IN (ruled 2026-11-28) | s543u's batch |
| an ARRAY operand of an overloaded op is its first element (`@rows == 2` under bigint dies in objectify; Math::Complex `/ @_`) | LOUD, NOTICE | 2 | 0110 0466 | #5468 | yes (census A) | IN | s543t, #5501 |
| `builtin::indexed` is not implemented (the import skips it, the call dies) | LOUD | 2 | 0294 0119 | #5502 | new | IN | -- |
| lib/POSIX.pm lacks `getenv` and `INFINITY` (and the C99 constants / math functions) | LOUD | 2 | 0399 1049 | #5506 | new | IN | #5500 (a shim's import) |
| `sort $scalar OP ...` takes the scalar as the comparator: a drop (`=~`, `$_ > 0 ?`) or a raw `NIL is undefined` | DROPPED | 2 | 0841 1308 | #5510 | new | IN | #1427 |
| a non-ASCII lexical's WRITE sites emit an unescaped symbol (:invert reads it as another name): unbound, or an endless loop | LOUD, TIMEOUT | 2 | 1497 0313 | #5514 | new | IN | -- |
| a foreach range is materialized in the modifier form and with a `use constant` end: `for 1 .. Inf` dies out of memory | LOUD | 2 | 0557 1291 | #5517 | new | IN | #1427 (the raw die) |
| an `:lvalue` sub assigned to | DROPPED | 2 | 1498 1499 | `NS:ns-lvalue-subs` | yes (ns, deferred #930) | OUT | -- |
| `$a[shift]`: a builtin name as a subscript is autoquoted (1093's 2nd cause) | LOUD | 1 | 1093 | #5463 | yes (census A) | IN | #5511 |
| `\G` in a substitution is a literal G | LOUD | 1 | 1297 | #5461 | yes (census A) | IN | s543u's batch; #3414 (its notice) |
| a `state` variable of a SIGNATURE sub is invisible to a named sub nested in it (0710's 2nd cause) | LOUD | 1 | 0710 | #5507 | new | IN | -- |
| `pop @{ $h{k} \|\| [] }` / `shift @{ $a[-1] // [] }` fetch the element as an LVALUE: the key is created, an empty array dies | LOUD | 1 | 1093 | #5511 | new | IN | #165 (the inverse) |
| an overloaded ASSIGNMENT operator (`+=` `-=` `.=`) is never called | LOUD | 1 | 1253 | #5501 | new | IN | s543t, #5468 |
| nested paren groups on a list-assignment LHS are not flattened | LOUD | 1 | 0188 | #5503 | new | IN | -- |
| `map(sub { ... }, LIST)` (the EXPR form) runs the sub's body per element | LOUD | 1 | 0220 | #5504 | new | IN | #5513 (map's EXPR path) |
| `my $x, $y;` in a block whose named sub uses $x refuses the file ("possibly captured") | LOUD | 1 | 0306 | #5505 | new | IN | #318 #2058 |
| PPI lexes `{` after `not` / `!` as a block: the anon hash is dereferenced as a list | LOUD | 1 | 0493 | #5508 | new | IN -- PPI: needs the USER's word | #650 #286 |
| `CONST*$var` (no space) is lexed as a glob cast after a bareword: a drop (PCL's prototype table knows CONST) | DROPPED | 1 | 0556 | #5509 | new | IN | ppi-upstream-bugs §12 (`_repair_glob_multiply`) |
| a `local` in a TERNARY arm is refused (#3370's closed refusal) | DROPPED | 1 | 1259 | #5512 | new | IN | #3414 |
| `map split, LIST` emits a one-argument split: a raw arity condition | LOUD | 1 | 1359 | #5513 | new | IN | #5504, #1427 |
| `state` / `my` / `our` / `local` before `=>` in a paren-less argument list is dropped | LOUD | 1 | 1502 | #5515 | new | IN | -- |
| the value of `++vec(...)` / `vec(...) = V` is the untruncated number: the counter never carries (an endless redo) | TIMEOUT | 1 | 1053 | #5516 | new | IN | -- |
| `&$_(ARGS)` is dropped | DROPPED | 1 | 0333 | #4807 | yes | IN | -- |
| `(Bareword)[i]` in a list is called, not the string | LOUD | 1 | 0523 | #1594 | yes | IN | -- |
| a spaced `< $var >` glob emits a 3-argument `p-.` (a raw arity condition) | LOUD | 1 | 0445 | #1292 | yes | IN | #650, #1427 |
| `\N{name}` is literal text (and charnames' script list is not found) | NOTICE | 1 | 1495 | #1494 | yes | IN | census A (0670 1436) |
| recursion deeper than ~10,000 exhausts the binding stack (perl: 100,000 in 0.4 s) | EXHAUSTED | 1 | 0293 | #1929 | yes | IN (criterion 4) | #2556 (the form) |
| SBCL's fixed 1 GB heap; a foreach over `split //` costs 1.2-3x perl's memory (perl needs 2.1 GB here) | EXHAUSTED | 1 | 0478 | #5518 | new | IN (criterion 4) | #2556 (the form), #1590 |

**Frequency:** #650 moved rare -> ordinary (eight programs); #4807 #1594 #1292
#3414 #1427 #1929 #2556, which had no field (= ordinary), now name their
programs.  `tools/task-triage.pl` on the live store: 33 ordinary + 62 rare
(after census A) -> **41 ordinary + 61 rare** (#650 and the seven new silent
shapes: #5503 #5504 #5507 #5511 #5514 #5515 #5516).  **No duplicate with
census A**: its #5463 #5468 #5461 and the reviewing session's #5560 were
appended, not refiled.

**The fix batches this table suggests, largest first:** **the lexer's term
position** (#650 #5508 #5509, with #1292: 11 programs; #650 and #5508 need the
USER's word on the closed PPI program, but #650's six import programs need
only PCL's rebuild to keep the spaces); **overloading and the big-number
shims** (#5500 #5501 #5468, already in s543t: 7 programs); **regex escapes**
(#5560 #5461, already in s543u: 5); **list operators' first operand** (#5510
sort, #5504 / #5513 map's EXPR form, #5515 the paren-less list: 5 programs,
one parser area); **the shims' missing names** (#5502 builtin::indexed, #5506
POSIX: 4); then the singles, and the two resource tasks for the criterion-4
decision.
