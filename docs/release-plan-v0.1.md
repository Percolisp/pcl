# Release plan — v0.1, the first public release (s375c, 2026-08-09)

USER-initiated (2026-08-09).  Two USER rulings anchor it:

1. **The runtime (and the saved core) is COMPILED AT INSTALL** on the target
   machine — the same model the XS bridge already uses: compile happens at
   install time, like perl, never at first use.  Task #277.
2. **The IR gets a pre-release pass**: look at the generated code and
   optimize/clarify it with MACROS, and update `docs/ir-spec.md` to match —
   the scoped, pulled-forward version of Target B (#75 remains the full
   flag-day).  Task #281.

## The track, in order

| phase | what | tasks | owner |
|---|---|---|---|
| 0 | prerequisites already in flight: FOLD chunks 2–3; Opus filler queue | #153; #275 → #276 → #238 → #239 → #237 | Fable; Opus |
| 1 | it works on someone else's machine: relocatable/regenerated artifacts → installer (deps check, artifact regen, **runtime+core compiled at install**, smoke gate) → path sweep to zero → server-leak fix | #217 → #277, #278, #128 | Opus |
| 2 | IR pass: re-measure the `generated-cl-ir-review.md` friction list against current emission, introduce the macro vocabulary where it costs no speed, update `ir-spec.md` normatively; absorbs #218/#219 | #281 (after #153) | Fable design, Opus arms |
| 3 | neatness: root junk + 29 loose planning docs + .gitignore; README/STATUS/CHANGELOG | #279, #280 | Opus |
| 4 | confidence: the big bug hunt (ruled s360 as the E5 exit gate — the public release is what it was deferred for) | per docs/bug-review-s359.md §4 | both |
| 5 | gate + infrastructure: fresh-machine container install from README alone, gate green; then CI | #282 → #283, tag v0.1 | Opus |

Rough sizing: 5–8 sessions after phase 0 completes, most of it
Opus-executable; #281's vocabulary design and the bug-hunt triage are the
Fable-shaped parts.

## Ground rules for the track

- **#281 is EMISSION-CHANGING**: full verification (corpus-diff explained
  per file, gate, sweep TOTAL/LOST), generation bump, artifact regeneration.
  Everything else in the track must be emission-NEUTRAL and is verified by
  the gate + the #278 grep invariant.
- **Speed still wins** (CLAUDE.md §2): a macro that expands to today's form
  is free; one that changes the expansion needs the bench first.
- The release tag precondition is **#282 (fresh-machine green) + the phase-4
  bug hunt**, not a date.

## Decisions that are the USER's (open)

1. **Public name** — "PCL" collides with Portable Common Loops and
   *Practical Common Lisp*; the `percolisp` GitHub org already exists.
   A rename is cheap before release, expensive after.
2. ~~**Publish the process docs?**~~ **RULED by the USER, s401 (2026-08-15):
   it is open source — the process docs (session-log, DECIDED.md, the
   fable-answers/review-request series) stay in the public repo AS-IS under
   `docs/`; no archiving, no pruning.**  #279 is now pure mechanics.
3. ~~**LICENSE body**~~ **RULED by the USER, s401: "License — same as Perl.  Tag
   all code files."**  The dual Artistic-1.0-Perl OR GPL-1.0-or-later text
   stands; every PCL code file now carries the tag (`tools/tag-license`,
   `tools/lib/PCLLicense.pm`, gate row `Pl/t/license-tag-01.t`); files from
   the Perl distribution / CPAN are NOT tagged (user: "Don't tag code files
   straight from the Perl distro!") — perl-tests/, cpan-tests/, and the two
   lib/ carry-overs are excluded by name with reasons.
4. **pclxs bundling** — recommendation: release PCL first, mention pclxs as
   the experimental XS sibling; its GitHub push stays the user's deferred
   call (#92).
5. **Hosting/remote** — the repo currently has no remote at all.
