# The IR conformance corpus

**A backend for PCL's IR is done when it answers every case here the way real
perl does.**

That sentence is borrowed, deliberately. `tools/pcl-conform` runs pclxs's
conformance corpus against PCL to decide whether PCL is a finished *XS host*;
this is the same idea one level up, for anyone writing a **JavaScript, C or
other backend** over PCL's IR (`docs/js-target-plan.md`,
`docs/c-target-notes.md`).

## What is here

    cases/NNN-<topic>.pl        a small Perl program, named by the semantics
                                it pins
    cases/NNN-<topic>.expected  perl 5.40.3's answer: stdout and exit code
    cases/NNN-<topic>.ir        the IR for that program, DATA form
                                (`pl2cl --emit-sexp`, docs/ir-spec.md §12b)
    cases/NNN-<topic>.rules     optional normalisations (below)
    known-fail.tsv              the cases PCL's OWN target still gets wrong,
                                each with the task that owns the bug

The cases were harvested from the probe files review sessions left behind —
each one was written to answer a real question about perl's behaviour, which
is why the corpus is dense in the places that are actually hard.

## How to measure your backend

```sh
tools/ir-conform --backend ./my-backend        # your backend over every case
tools/ir-conform --backend ./my-backend 3xx    # one topic (substring filter)
```

`--backend CMD` runs `CMD <case>.ir` once per case, in a fresh empty working
directory. Your command must **print the program's stdout on its stdout and
exit with the program's exit code**. Nothing else is compared:

* **stderr is never the oracle.** The interleaving of the two streams depends
  on buffering, so comparing merged streams would bless a flake. A case that
  wants to pin a diagnostic prints it to stdout itself.
* **The `.ir` file is the input**, so you never parse Perl. It is line
  oriented, 7-bit, and five reader rules wide — `docs/ir-spec.md` §12b has the
  grammar and a working reader in both Perl and JavaScript.

## The other two ways to run it

```sh
tools/ir-conform                # PCL's own CL target — the corpus's soundness proof
tools/ir-conform --oracle       # perl again — the corpus's self-test
tools/ir-conform --record       # re-record .expected and .ir
tools/ir-conform --list         # every case with its topic and rules
```

## Normalisation, and what is never blessed

A case whose perl output carries something that is not the semantics — a file
path in a die message, a heap address, hash order — is either **normalised** by
a named projection or **rejected** from the corpus. It is never blessed flaky.
The projections are named and closed, so a backend author implements the same
three in any language:

| `.rules` line | what it does |
|---|---|
| `normalise die-location` | ` at <path> line <n>[.]` → ` at FILE line N` |
| `normalise hex-address` | `0x` + 4 or more hex digits → `0xADDR` |
| `normalise sort-lines` | the output's lines, sorted (hash order) |

A case with no `.expected` **dies** rather than being skipped (CLAUDE.md rule
12): a claim nothing evaluates is not a pass.

## known-fail.tsv

PCL's own target passes 289 of 347 cases. The other 58 are PCL bugs the corpus
found, listed in `known-fail.tsv` with the task that owns each. Such a case
still runs; it prints `known` and does not fail the run. An **unlisted**
failure fails the run like a regression, and a listed case that starts
**passing** is reported `STALE` and fails the run — so a fix cannot land
without its row leaving the file. `--strict` counts them all as failures,
which is the number to watch shrink.

Rows leave `known-fail.tsv` **by edit, with the fix**, one at a time — never
by re-blessing a run.

## Where it runs

`Pl/t/ir-conform-01.t` is the gate row: the corpus's structural invariants
(every case has an oracle, every oracle's header matches its body, every
`.rules` line names a known normalisation, every known-fail row names a live
case and a task) plus an evenly spaced **20-case sample**, ~6 s. The full
corpus (~65 s at `--jobs 2`) is a WHAT-TO-RUN-WHEN entry: run it after a
`cl/` runtime change or a `Pl/` emission change.
