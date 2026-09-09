# The publish plan (s480, Fable, 2026-09-09) — making Percolisp look interesting

**USER 2026-09-09:** "I am going to clean up the .md files so they are simpler and
then publish pointers to PCL, hopefully to drum up some interest."  And:
"Percolisp will be an experiment, until we can run DBI etc."  This plan is
**first on the next session's queue**; the bug-fix queue (`docs/plan-post-s473.md`,
s473t4 → s473t3 → #1592 → #1117) resumes behind it.

## 0. Framing — what the pitch is allowed to say

* **Percolisp is an experiment until DBI runs.**  That sentence goes into the
  README's maturity line verbatim, and "DBI runs" becomes the milestone that
  ends the experiment on the roadmap (§1 P7 says what DBI needs).  An honest
  "experiment" with numbers travels further than a "product" with adjectives.
* **Enthusiasm is the goal; numbers are the method.**  Every claim on the first
  screen carries a measured number and a link to how it was measured.  Nothing
  is reordered, hidden or rounded up: the moo-objs 30× stays on the table next
  to the collatz 0.18×.
* **The public name is Percolisp.**  `pcl` is the command.  "PCL" alone collides
  with the Point Cloud Library and HP's printer language in every search.
* **Three audiences, one headline each** (used in the README's first screen,
  the release notes and the posts):
  * Perl people: *your script, unchanged — loops 3–5× faster than perl, and
    you can read the compiled output.*
  * Lisp people: *Perl 5 with CPAN running inside SBCL.*
  * Compiler people: *a documented IR with facts-licensed optimizations,
    written in Perl, that compiles Perl.*

## 1. Members, in order (owner · size · bar)

| # | member | owner | size | bar |
|---|---|---|---|---|
| P1 | **The install wall.** (a) Vendor cl-ppcre under `cl/vendor/cl-ppcre/` (BSD-2; ~100 KB of Lisp) and push that directory onto `asdf:*central-registry*` before the `asdf:load-system` in `cl/pcl-runtime.lisp` line 42, so a machine needs SBCL and nothing else; keep the Quicklisp path as the fallback when the vendored copy is absent.  (b) `tools/install-matrix/deps.sh` and the README quick start lose the Quicklisp step.  (c) A container image: `tools/install-matrix/` already builds ubuntu:24.04 + deps; publish it (`percolisp/pcl`) and give the README a `docker run` one-liner that runs the demo.  (d) The README says "tested on Ubuntu 24.04 with exactly these commands" and gives the SBCL 2.5.2 binary download line, instead of "newer than most distributions ship". | Opus agent (label **s481a**, IDs 1597–1606) | M | fresh ubuntu:24.04 container runs the quick start with the README's commands verbatim; `prove tools/t/install-container.t` green; `Pl/t/core-deps-01.t` still green; the gate; `docs/caching.md` + `docs/pcl-commands.md` say where cl-ppcre comes from |
| P2 | **The README's first screen.**  In order: one sentence + one number ("Perl 5 compiled to native code through SBCL; loops 3–5× faster than perl; 84 of 183 CPAN test files pass unchanged"), the hedge ("an experiment until DBI runs"), a **four-line Perl / four-line Lisp side by side** (the "output you can read" claim made visible), the quick start (post-P1), then the rest as it is.  The speed table gains **two whole-program rows** (`json-rt`, `textproc` from `tools/bench-exec.pl`, measured on a quiet box per the s479 pad-probe rule) because "what about a real program" is every reader's first question.  Two sentences that are currently oblique become plain: a compiled program still needs perl installed for string `eval`; there is no standalone binary yet. | **Fable writes**, USER edits (USER: "I found the prompt to make you write a good README") | S | a reader who has never seen PCL can say in 30 s what it is, what is faster, what is slower, and how to try it; every number links to its measurement |
| P3 | **Repo metadata, license files, release, contributing.**  GitHub description → one sentence (today it leads with "a PPI addition"); topics `perl perl5 common-lisp sbcl compiler transpiler`; homepage.  `LICENSE-Artistic` + `LICENSE-GPL` beside `LICENSE` so GitHub classifies the license (today: NOASSERTION).  A GitHub Release for v0.1.0 with the CHANGELOG excerpt.  A short `CONTRIBUTING.md`: how to run the gate, where bugs go, what a good bug report carries (the perl-vs-pcl probe).  Delete the empty untracked `memory/` at the repo root. | files: the s481a agent (same batch); GitHub settings: **USER** (repo admin) | S | the repo page shows a license badge, topics, a release and a description that matches the README |
| P4 | **The docs split.**  `docs/` holds 156 files plus 28 in `history/`; a visitor sees session logs and review rulings.  Target: `docs/README.md` = an index of the **user-facing eight** — `STATUS.md`, `pcl-commands.md`, `caching.md`, `not-supported.md`, `ir-spec.md`, `extensions.md`, `shipped-modules.md`, `xs-artifact-cache.md` — and everything else under `docs/dev/` (plans, rulings, reviews, session-log, DECIDED, runbooks) with `docs/history/` untouched.  `CLAUDE.md` at the root is 60 KB of process; keep a short one that `@docs/dev/…`-imports the bulk (Claude Code honours `@path` imports), so sessions keep their rules and visitors see a page.  Every path that moves is fixed in the tools that read it (`grep -rn 'docs/' tools/ Pl/t/ CLAUDE.md` first — the staleness and license gates read `docs/`). | **USER** (doing it); the plan supplies the list above; an Opus agent fixes the tool paths afterwards if the USER asks | M | `prove -j8 Pl/t/` green after the move (no-hardcoded-paths, license-tag, artifact-staleness); a visitor who clicks `docs/` sees one index and eight pages |
| P5 | **The announcement.**  One post (Sonnet drafts, Fable reviews, USER signs): the story is the hook — a Perl compiler largely written with Claude over 400+ sessions, differential fuzzing against perl as the bug finder, `pack` written in Perl and compiled by the compiler itself, and the IR.  Per-audience openers from §0.  Channels: blogs.perl.org + r/perl + Perl Weekly (Perl headline); r/lisp + Planet Lisp (Lisp headline); Hacker News with the compiler headline.  Goes out **after P1–P3** — a pointer that lands on the Quicklisp step is a pointer wasted. | Sonnet prose → Fable review → USER | S | the post makes no claim the README does not make with a number |
| P6 | **A 30-second recording.**  `pcl script.pl` beside the generated Lisp, as an asciinema or GIF linked from the first screen.  Cheaper than any paragraph for "you can read the output". | USER or the s481a agent | S | under 40 s; the Lisp shown is real `pl2cl` output |
| P7 | **The DBI milestone (what ends the experiment).**  DBI is XS, so it needs pclxs's remaining optional group (`io`, task #117), DBD::SQLite's XS through `tools/pcl-xs-install`, and the DESTROY question (`docs/xs-abi5-and-destroy.md`: an unimplemented destructor leaks every statement handle).  Not scheduled here — **sized** into one task so the roadmap can say what "DBI runs" costs, and the README roadmap says "the experiment ends when DBI runs". | Fable sizes (task), USER decides when | task only | the task names the three blockers with their tasks and an estimate |

## 2. Order and slots for the next session

1. **Launch s481a (P1 + P3 files)** into a slot on the first turn, pinned
   Opus, fresh worktree, generation bump only if emission changes (it will
   not; a runtime `asdf` registry push is not emission).  The second slot
   takes **s473t4** (the bug-fix queue continues; USER: bug fixes now).
2. **Fable writes P2** while they run — the README is a prose deliverable
   with numbers Fable already holds; the two whole-program rows are measured
   on the quiet box **before** the agents load it (first ten minutes of the
   session, `uptime` beside every number).
3. **USER does P4 and the GitHub settings** in parallel; the agent fixes tool
   paths afterwards if asked.
4. **P5 after P1–P3 are live** (merged, pushed, CI green, the container image
   pulled once from a clean machine).
5. P6 and P7 as fillers.

## 3. What NOT to do

* Do not reorder or trim the speed table; add rows, never remove.
* Do not describe anything as "works" without the population number behind
  it (`docs/STATUS.md` is the source; the README quotes it).
* Do not put the Claude authorship anywhere but where it is (Background) —
  factual, once, not a headline and not hidden.
* Do not change `pcl`, `pl2cl`, `runpcl` names; the public NAME changes, the
  commands do not.
* Do not launch P5 before the install wall is gone.
