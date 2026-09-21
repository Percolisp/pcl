# The everyday battery — `tools/everyday-smoke.pl`

**What it answers:** *how many ordinary Perl programs does PCL get exactly
right?*  One number, printed as the last line of every run:

```
EVERYDAY: 85 of 122 identical to perl (69.7 %) -- 98313c26 gen v2-1740
```

**Why it exists** (task #1994 → #2099; USER, s493: *"Yes, run on demand"*, and at
the end of s494: build it early and **steer by its number**, goal **> 90 %**).
The suites this project already runs — `perl-tests/`, perl's own `t/`, the CPAN
board — measure the language in isolation and in bulk, so they rank *exotic*
gaps first: that is what they contain.  They said 96.5 %.  When s491–s493 wrote
103 *ordinary* programs and compared them with perl byte for byte, 68 matched.
The gap between those two numbers is the finding, and this instrument is that
measurement made repeatable.

---

## Running it

```bash
tools/everyday-smoke.pl                  # the whole corpus, ~2 min cold, ~20 s warm
tools/everyday-smoke.pl modules          # one area
tools/everyday-smoke.pl everyday/idioms/local-scalar.pl     # one program
tools/everyday-smoke.pl --jobs 4 --timeout 120
tools/everyday-smoke.pl --record         # also append a row to the history file
```

A run needs **no perl** and no network: every program's expected STDOUT is
checked in beside it.  It exits **1** on NEW / UNEXPLAINED / STALE (see the
buckets), **2** on usage or a missing tree, **0** otherwise.

A run over less than the whole default corpus says so in the summary line —
`EVERYDAY (subset):` or `EVERYDAY (corpus <dir>):` — so a partial number can
never be quoted as *the* number.

Results live under `.everyday/` (gitignored): `<area>/<name>.{out,err,rc}` for
every program, and `work/<area>/<name>/` — the fresh empty directory the
program ran in, which is removed when the program matched and **kept when it
did not**, because what it wrote is usually the evidence.

## What the number is NOT

It is **not a compatibility percentage.**  The 122 programs were written *to
cover ground* — one idiom, one module, one perlfunc entry at a time — not
sampled from real code, and the denominator grows as programs are added.  It
answers exactly one question: *is ordinary Perl getting better, batch by
batch?*  Quoting "N of M" to a user as "PCL runs N % of Perl" would be false in
both directions: the corpus over-represents the seams where this project
already knows it is weak (modules, resources, the OS surface), and
under-represents plain computation, where PCL is strong.

`baselines/everyday-history.tsv` is the trend.  Every row carries **M** as well
as N, because a corpus that grows makes N alone meaningless.

## The buckets

Every run ends by comparing itself with `baselines/everyday-baseline.tsv` —
which lists **what is wrong**, exactly like `baselines/fail-baseline.tsv`.  A
program that matches perl has **no row**.

| bucket | what it means | what to do | exit |
|---|---|---|---|
| **NEW** | not `same`, and no row here | a regression, or a program added without its row | **1** |
| **FIXED** | a row whose program is now `same` | **edit the row out**, naming the batch that fixed it | 0 |
| **MOVED** | same row, different first-diff line (or verdict) | the first cause was fixed and a second shows, or something new broke earlier — **re-attribute** | 0 |
| **UNEXPLAINED** | a row with an empty cause | attribute it | **1** |
| **STALE** | a row naming a program that does not exist | delete it | **1** |

Rows leave **by edit**, never by re-blessing: there is deliberately **no
`--bless-baseline`**.  A baseline you can regenerate from a run is a baseline
that absorbs regressions silently — the same rule the sweep's fail baseline
follows.

Every row's cause is a **task number** or a `docs/not-supported.md` anchor, and
it must explain **the first differing line** — not the program in general.
Confirm it (run the task's reproducer, or read the diff) before writing it
down; a cause copied without looking is how a baseline starts lying.

## The verdicts

| verdict | meaning |
|---|---|
| `same` | STDOUT bytes and exit status both match |
| `DIFF` | STDOUT differs — the report names the first differing line |
| `RC` | STDOUT is identical, the exit status is not |
| `TIMEOUT` | the program was still running at `--timeout` (its whole process group is killed) |

**A program that dies loses its buffered STDOUT**, so a crash usually shows as
`DIFF` at line 1 even when the program got far.  When narrowing one, copy it to
a scratch file and add `$| = 1` — that is how #2287 was found.

## How a program is admitted

**A program is admitted only if perl's answer is a fact of the PROGRAM, not of
the RUN.**  `--bless-expect FILE…` runs it three times under perl — twice in
place and once as a **copy under a different absolute path with a different
cwd** — and refuses to write an expectation unless all three agree in STDOUT
bytes and exit status.  That catches `$0` and `__FILE__`, cwd-relative files,
the clock, `$$`, `rand`, and unsorted `keys` (perl randomizes its hash seed per
run, so three plain runs are a real hash-order test).  perl must also succeed:
a non-zero exit is refused unless the program declares `# expect-rc: N`.

Both sides get the **same world**, built by one sub (`child_env`) so they cannot
drift apart: a fresh empty cwd per program, `TZ=UTC LC_ALL=C LANG=C`, and
`PERL5LIB` / `PERL5OPT` / `PERL_HASH_SEED` / `PERL_PERTURB_KEYS` removed.

A program that fails admission is **repaired minimally, without changing what it
exercises** — sort the keys, print a property instead of an address, use a fixed
epoch instead of `time` — or it is not admitted.  `srand(42)` is *not* a repair:
perl's generator and PCL's differ by design, so print a property of the numbers
instead of the numbers.  **Record the PCL verdict before and after any repair**:
a repair that flips the verdict has changed the probe, and you should look
again.

## Adding a program

1. Write it under `everyday/<area>/<name>.pl` — one idea per file, so a
   whole-file refusal costs one program and not the battery.  Header comments:
   `# origin: …`, and, as needed, `# args: …`, `# needs: Module::Name`,
   `# timeout: S`, `# expect-rc: N`.
2. `tools/tag-license everyday/<area>/<name>.pl` — **before** deriving the
   expectation.  The tag moves line numbers, and a program that prints
   `__LINE__` would otherwise bake in the wrong one.
3. `tools/everyday-smoke.pl --bless-expect everyday/<area>/<name>.pl`.
4. `tools/everyday-smoke.pl <area>` — if it is not `same`, narrow the first
   differing line to a cause and add its baseline row in the same commit.
   Otherwise it will show up as **NEW** and fail the next run, which is the
   point.

The areas today: `idioms/` (42 — one everyday idiom each), `programs/` (33 —
small complete programs, including a log parser, a CSV munger, a directory
walker and a Moo class), `modules/` (28 — **one ordinary call** per core
module, because "it loads" is not "it works"), `index/` (19 — the perlfunc
index walk).

## The oracle, and drift

The checked-in expectations came from **perl 5.40.3**.  They are data, not
something a run recomputes, which is what makes a run reproducible — and what
makes it possible for them to go stale.

```bash
tools/everyday-smoke.pl --verify-with-perl
```

re-derives every expectation with the local perl and reports **DRIFT** (this
perl no longer prints what is checked in) and **NEEDS** (a `# needs:` module
this perl lacks, so nothing was verified).  Run it when adding programs and
after a perl upgrade.  A DRIFT row is **not** fixed by re-blessing on the spot:
find out whether the *program* or the *perl* changed, program by program.

## Another population, same tool

```bash
tools/everyday-smoke.pl --corpus /path/to/corpus --baseline none
```

is all it takes.  Third-party programs (Rosetta Code, task #2104) are **never
checked in**: the programs and their derived expectations live in a scratch
directory, `--bless-expect` derives them there, and `--record` refuses, because
a history row must identify what was measured.

## When to run it

Once per round, before a tag, and after any `lib/**` shim change — the module
surface is where this corpus is most sensitive.  Every batch report quotes the
`EVERYDAY:` line **before and after**; a **NEW** row is the reporting batch's to
fix or to explain.

## The test

`prove tools/t/everyday-smoke.t` — run directly, like `tools/t/tap-align.t`,
never in the `Pl/t` gate (the gate measures the transpiler; this measures a
measurement tool).  It drives the runner through a **fake `pcl`** that runs each
fixture program under perl and then misbehaves by name, so every verdict is
reachable in seconds without SBCL — and so the test does not depend on whichever
PCL bug happens to exist today.

## Not in this instrument (yet)

`--variants` (perl-validated mutations: B::Deparse re-spellings, the body
wrapped in a sub / a bare block / a string eval / another package — measured in
s493, see #2099 item 5), the complexity-class battery (#2098), one-liners as an
area (#2097), and "battery 3".  Each stays listed in #2099.
