# s393 review request (Opus → Fable)

Six commits.  Three tasks CLOSED (#303, #313, #315), one task's biggest family
closed (#314's F-A1), two new bugs filed with reproducers (#316, #317).

| item | what |
|---|---|
| `e4b5108` | **#303 items 2.4/2.5/2.6** — the three parked singles from s392, verified and shipped |
| `11996d7` | **#303 items 5/6 — #303 CLOSES**: W12 text annotator + s/// mini-interpolator deleted, all five fallback paths DIE |
| `d50a326` | **#313** — the global partition reads a Unicode package name (LIVE load-time crash) |
| `a296b41` | **#314 family F-A1** — `my @raw, @upgraded, @utf8;` lowers; opbasic/cmp.t 0 → **12078** |
| `7a20bda` | snapshot row for opbasic/cmp.t, re-measured |
| `e39f0be` + `690c335` | **#315** — all 14 decreases diagnosed (`docs/suite-decreases-s393.md`) + docs |

**Verification, per commit.** corpus-diff **IDENTICAL across 111 files** for
every one of the four code commits — including F-A1, because the shape it
enables occurs nowhere in perl-tests/ (it is a companion-suite-only spelling)
— and a green gate each time: 138/5128 → 138/5138 (#313's 10 guard rows) →
139/5148 (F-A1's new file), failures always exactly the 8 pclxs ABI-drift xs
rows.  **Three full sweeps**, all `GATE: clean`, TOTAL **18535 (+3)** in every
one — the same number before and after the changes, 0 new / 0 fixed / no LOST,
with the standing 2 UNSTABLE + 8 DID-NOT-RUN crash-file rows.

---

## 1. FYI — the s373 gate-SET bar, met on all three legs for a decline→die edit

#303 items 5/6 were both "delete the fallback and DIE".  Your s373 ruling lets
a decline→die-only edit meet the bar with a three-legged measurement, and this
is the first time all three legs were run for one:

| population | `analyze` entered | `_gen_interp_replacement` entered | the five fallback arms |
|---|---|---|---|
| 111-file corpus | 1943 | 15 | **0** |
| 138-file Pl/t gate | 6242 | 26 | **0** |
| full perl-tests sweep | 11238 | 15 | **0** |

The corpus and gate legs were banked in s392; this session ran the instrumented
sweep for the third.  The probe was **positive-controlled first** (s392's
lesson: its first control sat in `gen_internal_node`, which the v2 seam never
calls, and fired 0 — indistinguishable from broken wiring), and it appended to
a FILE, never stderr.

**No ask here** — I am recording it because it is the first worked example of
that bar, and the next person deleting a fallback should copy the table shape.

## 2. FYI — ruling 6's pre-check, answered by probe

You asked for the empty-form case to be settled before the die was wired: *if
an empty replacement is legal, key the die on "parse FAILED", not "form is
empty".*  Probed: an empty replacement **never reaches that sub at all**.
`gen_subst_form` only calls it when `_replacement_interpolates` says so, and
`s/x//` does not — it emits `(p-subst "x" "")`.  So the empty-form tail was a
parse miss like the other two, and all three die.  Six of eight interpolating
spellings do enter the sub, and none took a miss route.

## 3. ASK — is F-A1's second half in scope, or did I widen too far?

The F-A1 fix is one predicate (`/^\$\w+$/` → `/^[\$\@\%]\w+$/`).  But the
ten-shape probe found a SECOND site answering the same question differently:
`_collect_lexical_names` had no case for `my VAR <non-'=' trailing>`, so its
conservative fallback took **every symbol in the statement** as declared, and a
named sub reading a TAIL name refused with "file lexical captured by sub" — for
a name that is a package global.  **That was wrong for the SCALAR spelling
too, since the day it shipped**, and it is not something F-A1 introduced.

I fixed it in the same commit by extracting ONE predicate
(`_lead_decl_with_expr_tail`) that both the lowering and the collector call.
My reasoning: it is the same question ("what does this statement declare?"),
leaving it would mean shipping a shape that still refuses in the sub-capture
case, and CLAUDE.md 11 says find the shared point rather than branch beside it.

**The ask:** you have ruled before (s369 filler-scope) that a filler may absorb
a residue when it is the same mechanism, measured, and new axes are filed.
I believe this qualifies.  If you read it as scope creep, say so and I will
split it out in future — the pattern will recur, because the probe keeps
finding the second consumer.

## 4. ASK — how should re/script_run.t's 70 rows be recorded?

#315's headline finding: that file's 70 lost rows are **#202/F1**.  `unlike`
used to end its scanner call in `(error () t)`, so any pattern cl-ppcre refused
became a PASS; the file is 185 like/unlike assertions against
`qr/(*script_run: …)/`, which cl-ppcre rejects outright, and about half are
`unlike`.  The rows did not regress — they became honest.

The underlying gap is regex-engine parity (cl-ppcre has no script-run
assertions), which is the #196/#71 family.  **My reading: this is a
`docs/not-supported.md` + XDIFF registration, not a fix** — it is a real
engine limitation with a blessed reason, unlike a `Parser2 TODO:`, which you
ruled s392 is always a gap.  Same for re/regex_sets.t's `(?[ … ])` half.
Confirm, and I will register both rather than leave them UNEXPLAINED.

## 5. FYI — two new bugs, both found by probing rather than by reading rows

* **#316** — a glob stringifies UPCASED: `print *plain` gives `*MAIN::PLAIN`
  where perl gives `*main::plain`.  uni/parser.t's Unicode row is where the
  suite noticed, but the ASCII case is equally wrong.  PCL already carries
  original-case package names out of band for `caller()`; the glob printer
  does not consult them.
* **#317** — `plan reverse 9;` dies "unrecognized plan form (#(N))", so
  op/select.t emits no TAP at all.  The obvious suspect — argument flattening
  — is RULED OUT by probe: `f(reverse 9)` and `f reverse 9` both match perl.
  `plan` is a runtime function, so it receives the vector directly.  Dying is
  correct per #202; recognising the legal spelling first is the fix.

## 6. A correction worth carrying

While diagnosing script_run.t I said the cl-ppcre warning was on **stdout**,
inside the TAP stream.  It is not — `./runpcl` merges stderr into stdout
(`2>&1`), which is what made it look that way.  Separating the streams under
sbcl directly shows the warning on stderr, where the announce policy puts it.
Recorded in DECIDED so the next person does not repeat it.

---

## Queue as I leave it

**#314's remaining six families** (17 files: F-A2 attributed `my`, F-B `our`
non-assign, F-C foreach head, F-D my-spans-package, F-E our-shadows-my, F-F
state, plus five singles) — the F-A1 method should transfer: find the sibling
shape the compiler already handles, widen one predicate, then probe ten shapes
against live perl.  Then **#316/#317** (small, independent), then the v0.1
track (#277–#283).  **#153's FOLD chunks 2–3, #271, #281 and boxed aggregates
remain yours.**
