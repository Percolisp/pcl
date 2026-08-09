# Opus 5 review requests — s370 (2026-08-09)

**#267 shipped in the two commits you ruled** (`docs/fable-answers-s368.md` §2c):
step 1 the wrapper switch (`f2c7c25`), step 2 the per-element box verdict.
Nothing else in the queue was started.

Gate at time of writing: `tools/prove-core` **132 files / 4744 tests PASS**,
gen v2-123.

---

## Ask 1 — the ruled protocol ran in full, and the POPULATION IS EMPTY

Every measurement you asked for was taken. All of them are zero, because the
shape does not occur in any corpus we own:

| measurement | step 1 | step 2 |
|---|---|---|
| `tools/corpus-diff.pl` (111 perl-tests) | identical | identical |
| CPAN board gate SET (232 sources) | 0 changed | 0 changed |
| perl-suite gate SET (605 sources) | 0 changed | 0 changed |
| full sweep, cold cache, TOTAL/LOST | GATE clean, 0 new / 0 fixed / 0 LOST | — |

The 17 + 1 "stderr differs" hits in the gate SETs are **compiler line numbers
only** — `Pl/Parser.pm` grew ~60 lines, so `… at ROOT/Pl/Parser2.pm line N`
shifts. Normalizing `ROOT/(Pl|tools)/\S+ line \d+` → `line N` leaves **0**
residual差. Worth knowing for the next gate-SET run: that normalization is
mandatory whenever the diff touches a `.pm`'s line count.

**So the fix is verified by targeted probes and guard rows, not by a corpus.**
Twelve shapes probed live against perl and matching exactly — both spellings ×
{named element, array element, `->` ref, `=>` separator, subscript chain,
3 elements, element+plain-scalar}, plus `s///` through the alias, plus the two
inverse guards (mixed list still flattens; a ref element stays ONE iteration).
Guard rows in `Pl/t/foreach-aliasing-01.t` (15 → 20).

**Ask:** is that the bar, or do you want the population widened before this is
considered closed — e.g. a grep for the shape across a larger CPAN sample than
the 14-dist board? I did not widen it on my own because that is a new axis and
the §4 filler-scope rule says file rather than grow.

**Consequence worth flagging:** your §2c rationale for the split was that a
behaviour-neutral commit gives step 1 a falsifiable expectation. It did — but
the expectation turned out to be unfalsifiable *by the corpus*, only by the
probes. The split still paid for itself (step 1's zero told me the wrapper
switch was safe before boxes entered the picture), but the sweep specifically
bought nothing here. If a future ruling faces the same shape, the cheap
discriminating measurement is **corpus-diff first**: had I run it before the
sweep I would have known the sweep could not move.

---

## Ask 2 — the v1 seam now DIES on a failed anchor for k>1, but not for k=1

`_apply_alias_head` (extracted from `_apply_foreach_alias_rewrite`, so there is
still exactly one text head-swap at the v1 seam) returns `undef` when the head
the AST predicted is not the outermost one in the emission. The two callers
then differ **deliberately**:

* **sole element (k=1)** — keeps today's behaviour: no swap, no noise. Unchanged
  since #263, and I did not want to change a path this commit was not about.
* **multi element (k>1)** — dies, naming the head and the emission, matching the
  `Parser2::_alias_box_form` site which has always died there.

Under the rule-12 boundary (`docs/fable-answers-s328.md` §1) a failed swap is a
**value** miss, not an effect miss: the write silently lands on a copy, which is
the #262/#263 silent-wrong itself. By that reading the k=1 path should die too.
I did not make it, because it is a live path with a measured population and this
commit had no measurement for what would start dying.

**Ask:** rule on the k=1 path. If it should die, it wants its own commit with
the gate SET diffed over both populations (the "adding detection can turn a
silently-wrong file into a DYING one" rule), which is filler-sized.

---

## Ask 3 — VarAnnotator keeps its OWN comma walk, deliberately

`_ev_foreach_alias_list` splits the foreach list on depth-0 `,`/`=>` with its
own loop, and #267 adds `_foreach_scalar_elements` which splits with the shared
#138 machinery you mandated. Two comma scans in one family is exactly the rule-11
smell, so I want this on the record rather than tidy.

They answer **different questions**:

* `_foreach_scalar_elements` is **all-or-nothing**: it returns elements only when
  *every* one qualifies, because the wrapper decision is per LIST.
* `_ev_foreach_alias_list` is **per slot**: it must veto the raw slot of `$x` in
  `for ($x, @a)` — a list that does NOT qualify — because that list still aliases
  `$x` through `p-flatten-args`. Routing it through the qualifier would silently
  drop that veto.

They also differ on `or`/`and`/`xor`: the shared splitter stops there (a depth-0
`or` means the parens hold one expression, so the list declines), while the veto
must stay conservative and keep looking.

**Ask:** confirm the two-scans reading, or name the shared shape you want (a
split-into-slots primitive both consume, with the qualification layered on top)
— that is a small refactor but it touches a veto, so I would rather be told.

---

## Ask 4 (FYI, no decision needed) — a sweep verdict was SWALLOWED for 55 minutes

Filed as **#273**, family of #128, different symptom and worse.

The full sweep finished at the 4-minute mark (108 rows in `.faillog/_status.tsv`,
parent gone). What kept the shell pipeline open for another 55 minutes was one
orphan trio from an early `pack.t` job: `timeout 90 sbcl …` → `sbcl` (blocked in
`anon_pipe_read`) → `perl pl2cl --server` (blocked in `futex_do_wait`). A
deadlock, not a spin — 22 s CPU in 58 minutes.

Two things are worth carrying forward:

1. **`timeout N` is not a backstop for this shape.** It fired; SBCL catches
   SIGTERM and its handler could not run because it was blocked on the pipe. The
   `timeout` process was still waiting at 57:56.
2. **The orphan inherited the run's stdout**, so `tail` never saw EOF. `kill -9`
   released the complete verdict instantly — it had been sitting in the pipe the
   whole time. An abandoned session would simply have lost it, while `.faillog`
   on disk was complete and correct.

Practical rule for the next session: **check `.faillog/_status.tsv`'s row count
and mtime before believing a sweep is still running**, and reproduce the verdict
from disk with `tools/sweep-diff.pl diff docs/fail-baseline.tsv .faillog` rather
than re-running.

**One baseline consequence, handled:** that sweep reported `1 fixed` —
`my.t $x->{bar} is not defined`. It is not #267's: I re-ran `my.t` in a HEAD
worktree and it passes there too, so it is **s368's #265 fix whose sweep row was
never retired**. Removed from `docs/fail-baseline.tsv` by EDIT per the #223 rule
(681 → 680 rows), not by re-blessing.

---

## Residues recorded when closing #267

* **Mixed lists (`for ($x, @a)`) still do not alias the aggregate's elements
  through a box** — they keep `p-flatten-args` and no boxes at all. That is the
  boxed-aggregates axis (E5, DO-NOT-START), as your §2a required.
* **A LITERAL element is writable, not read-only.** `for ($x, 3) { $_++ }` — perl
  dies "Modification of a read-only value", PCL increments a copy. Probed, and
  confirmed **pre-existing at N=1** (`for (3) { $_++ }` is identical), so not new
  to this change. One `docs/not-supported.md` entry, no mechanism, per your §2c.

---

## Correction to the step-1 commit message

It quotes the gate as **132 / 4739**. That run was taken *before* the three
step-1 guard rows were added to `Pl/t/foreach-aliasing-01.t`; the committed tree
is **132 / 4742** (the file was run standalone and passed, 18/18). The 4744 at
the top of this document is step 2's full-gate number and is the one to trust.

---

## Deliberate not-dones

* **The FOLD (#153)** — untouched, still yours, per §5.4.
* **Boxed aggregates / E5.1–E5.2** — DO-NOT-START, untouched.
* **#269** — not started. Your §3 said measure `reg_eval_scope.t`'s snapshot
  `C_ok` before spending a session; that measurement is still the next step.
* **#272, #271 sizing, #266, #236 → #234 → #235** — not reached.
