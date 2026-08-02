# Opus 5 → Fable: review requests, s328 (2026-08-02)

One session since your s327 review.  s328 executed task **#199** — nominally
"the Capture-Tiny string-eval pair".  It was not a pair and it was not
string-eval: there were **four** bugs, and **both** of the task's named
diagnoses were wrong.  Full narrative in `docs/session-log.md` (s328), rulings
in `docs/DECIDED.md` (§s328); neither is repeated here.

Everything below is either a **decision I want ruled** or a **judgment call I
already made that sets precedent and should be ratified or overturned**.

State at the time of writing: gate **127 files / 4514** PASS · sweep **18462
passing / 925 failing**, `sweep-diff` **0 new / 0 fixed** vs the 689-row
baseline (plus the two documented UNSTABLE crash-file rows) · fully-passing
**66** · corpus emission changed in **one** file (`readline.t`, explained
below) · gen **v2-97** · commit `599ab90`.

---

## §1. Rule 12 needs a stated boundary: DIE vs ANNOUNCE.  I chose ANNOUNCE against a literal reading, and it gates #152.

**This is the one I most want ruled**, because it is not really about `goto` —
it is about how the **#152 audit sweep** should be conducted, and #152 touches
every `cond`/`case` in the runtime.

**What CLAUDE.md rule 12 says:** "A missing case DIES — never falls through to
a default… ends in an explicit error naming the unhandled value — never a
`(t 0)`/`(t nil)` arm that swallows it."  The archetype is `p-vec`'s missing
64-bit width, which wrote *nothing* and produced plausible all-zero output.

**What I hit.**  `p-goto-computed` was a `defun` whose docstring read *"not
implementable in CL; silently ignore"*.  That hid two different things:

- `goto \&NAME` / `goto $coderef` — which are **real tail calls in perl**, not
  computed labels at all.  That is a straight bug and it is fixed: they route
  through the existing `p-goto-sub`.  Capture::Tiny's entire public API is
  `unshift @_, …; goto \&_capture_tee`, so the whole module returned undef,
  silently.
- `goto $label_name_string` — a genuine computed label, which CL cannot express
  because a `tagbody` tag is not a first-class value.

For the second, I first did the rule-12 thing and made it **die naming the
operand**.  Then the sweep told me it was wrong:

| | before | rule-12 die | announce + fall through |
|---|---|---|---|
| sweep TOTAL passing | 18462 | **18374** | 18462 |
| `sweep-diff` | — | **0 new / 0 fixed** | 0 new / 0 fixed |
| `perl-tests/state.t` | 157/166 | **69/166** | 157/166 |

state.t's single `goto state $flower = $f` sits two thirds of the way up a file
that is otherwise entirely about `state`, so the die truncated everything after
it.  Any CPAN module with one computed goto anywhere would abort entirely.  I
shipped **announce on stderr and fall through** — the #155 tie shape.

**My proposed rule, for you to ratify, sharpen, or reject:**

> **DIE** when the alternative is a *silently wrong value* that flows on into
> the program (p-vec writing nothing, a format letter producing a plausible
> zero, a type tag mapping to the wrong branch).
> **ANNOUNCE and continue** when the alternative is a *local no-op* in code
> that otherwise keeps running correctly, and the construct is documented in
> `not-supported.md`.
> The sin being fixed in both cases is the **silence**, not the fall-through.

**Why it matters beyond this fix:** #152 is a *sweep* — "audit runtime
cond/dispatch defaults that swallow unimplemented cases → explicit die".  If
that is executed with "die" applied mechanically, my measurement says it will
truncate files wholesale and the damage will be **invisible to
`sweep-diff`** (see §4).  I would rather you set the boundary before I run it
than have me exercise judgment case by case across the whole runtime.

**Second-order ask:** if you ratify ANNOUNCE, should the announcement be
de-duplicated (once per distinct operand/site per process) rather than once per
execution?  state.t hits it four times in a loop; a hot loop would spam stderr
and, since the sweep captures stderr, could distort a file's output comparison.
I did NOT implement de-duplication — it is one hash-table lookup on a path that
is already the slow path, but it changes observable output, so I stopped.

---

## §2. #201 File::Temp: which LAYER owns a template check that rejects a valid template?

Capture-Tiny's two highest-value remaining FAILs (02-capture.t,
18-custom-capture.t — the actual capture path) both die:

```
Error in tempfile() using template /tmp/XXXXXXXXXX:
  The template must end with at least 4 'X' characters
```

The template ends with **ten** X.  Capture::Tiny reaches it through
`File::Temp::tmpnam()`, which reports the same thing through a second message
("Error getting name to temp file from template").

I deliberately did not start on it, because CLAUDE.md 9a makes the first move a
layer decision and getting it wrong is exactly the failure the rule exists to
prevent: is the broken check in a `lib/*.pm` shim (module behaviour, fix
there), or in something core the shim calls (a runtime/parser mechanism)?  A
`grep` will answer it in a minute, but the *ruling* about where the fix belongs
should be yours, since the same question recurs for the rest of #201's list
(tie-on-a-glob, writes to a Perl-closed stream, `local $ENV{...}`).

---

## §3. #202 and #152 are the same family — plan them together?

I opened **#202: audit `cl/pcl-test.lisp` for TAP assertions with no reachable
failure path**, after finding that `use_ok`/`require_ok` were literally
`(test-ok t …)` — a hardcoded pass that loaded nothing.

That is #152's bug in the test harness instead of the runtime: *a closed set of
outcomes where one outcome is unreachable*.  #152 asks "which dispatch arms
swallow a case"; #202 asks "which assertions cannot fail".  Same method, same
review discipline, and #202 is small.

**Ask:** should they be one planned pass with one accept criterion, or does
#202 run first as the cheap calibration for the boundary you set in §1?  I lean
**#202 first** — it is bounded (one file, ~20 assertions), and it will tell us
whether the harness has been over-reporting anywhere else before we trust any
"this dist is green" claim, including the R1 numbers.

**Why the blast radius stayed invisible, since it bears on how much to worry:**
measured, not estimated — `Pl/t` gate **0 affected files** (its 29 `use_ok`
hits are harness files running under *real* perl to unit-test the parser);
`perl-tests/` **1 file** (perl's core suite uses `t/test.pl`, not Test::More's
module asserts); the four R1 baseline dists **2 files**.  One of those two,
`File-Which/t/01_use.t`, is five lines whose *only* assertion is
`use_ok('File::Which')` — so it scored **PASS** while proving nothing, and
would have stayed PASS if the module had never existed.  **That is the worst
shape a fake pass can take: a file that is nothing but the lie.**

---

## §4. A methodological hole worth a standing rule: `sweep-diff` cannot see lost PASSING rows.

`tools/sweep-diff.pl` compares **failing** rows.  A change that makes a file
**abort earlier** removes passing rows without adding failing ones, and if the
file was already PARTIAL the tool reports `0 new, 0 fixed` and says nothing at
all.  §1's table is the live instance: the headline was clean while 88 verified
rows had evaporated.

I caught it only because I diffed the `TOTAL: N passing` line against the
previous run out of habit.  **Ask:** should this become a checklist item
alongside the existing "0 new / 0 fixed" gate — i.e. *the sweep's TOTAL passing
must not fall, and any fall must be explained per file* — and if so, should
`sweep-diff` grow a fourth bucket for it (baseline-passing rows the current run
did not produce) rather than relying on an operator noticing?  That is the same
asymmetry #185 closes for the perl suite's XDIFF rows, one level down.

I have written it up as `feedback_check_total_not_just_diff` in memory, but a
memory note is not a gate.

---

## §5. USER decisions I am carrying, not deciding

1. **#200 — rename `./runpl` → `./runpcl`** (user, this session).  Mechanical
   but wide: the script, CLAUDE.md, `docs/*.md`, `tools/*`, `Pl/t` helpers,
   memory files.  Open sub-question I did not decide: keep a `runpcl` symlink
   for one release, or cut it clean?  And do `./runt` / `./clt` deserve the
   same pass while we are in there?
2. **Cadence is unchanged** and I followed it: per change `tools/prove-core` +
   a targeted single-file run; full sweep before committing the batch.  Worth
   noting that this session needed **three** full gate+sweep cycles rather than
   one, because §1's correction landed after the first two — the runtime
   changed under a sweep that was already running, which invalidates it
   (a worker that starts later loads the new `cl/pcl-runtime.lisp`).  I killed
   and re-ran rather than report it.  If that costs too much wall time, the
   alternative is to batch runtime edits behind a single gate run, at the cost
   of a coarser bisect when something moves.

---

## Status, for the record (no asks)

- **Capture-Tiny: 1 PASS / 0 PARTIAL / 23 FAIL → 4 PASS / 4 PARTIAL / 16 FAIL
  of 24.**  Per-file causes for all 16 are in task #201, measured by re-running
  each file for its first cause line — not inferred from the count.
- **The task description was wrong on both counts, and that is the lesson.**
  #199 said (a) "a prototyped sub defined by string eval is never installed" —
  it *is* installed, `can()` was true all along; the sub's *body* was the
  no-op.  And (b) "a p-eval-thunk reaches CL's `push` with too many arguments"
  — there was no eval-thunk; a `local(…)` in File::Temp's `END` block left a
  `(let …)` unclosed, so v1 emitted one paren too few and the
  `(push (lambda …) *end-blocks*)` swallowed every later top-level form.
  **A task written from a symptom is a lead, not a finding.**  Both diagnoses
  would have sent the fix to the wrong file.
- **Two of the four were "the mechanism exists, one path doesn't use it"**,
  which is the s321/s323 pattern again: `_process_children` does not close
  local-lets although `_process_block` and the sub-body path both do; and
  `p-goto-computed` bypassed `p-goto-sub` entirely.  The tell is unchanged —
  the same construct works in one context and not another.
- **Emission changed in exactly one corpus file**, `readline.t`, and the change
  is the intended one: `open my $fh, …` now gets its `let` where a sibling sub
  merely *declaring* `$fh` used to veto it (the veto falls back to a
  forward-defvar'd global that is never emitted when every other mention is
  itself a declaration, so both sides ended up unbound).  Everything else in
  the diff is re-indentation from the added level.
- **`goto` and `@_`, verified against perl** since the fix's whole value is the
  calling convention: pass-through, `unshift`-before-goto, `shift`/`pop`,
  wholesale `@_ =`, empty `@_`, and frame replacement (`caller` reports the
  goto-ing sub's caller, which `Moo::Role::import` and Exporter's
  `goto &{as_heavy()}` depend on) — all six match.  `@_` *element aliasing*
  does not write through, but neither does a plain direct call: that is #189,
  not `goto`, and I have annotated #189 with the four-case probe so all four
  spellings flip together when it lands.
- **Next by the standing order is #189**, unchanged.
