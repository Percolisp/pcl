# a2775 (s510, LOW PRIORITY, ANALYSIS ONLY) — the list-context bind around built-in calls that never read their context: is it safe to stop emitting it, how much does it buy, and what exactly would the change be?

You are an EXECUTION agent (Opus 5.5) on the PCL project (Perl -> Common Lisp transpiler).  This job
produces a REPORT and a recommendation.  **It does not change the compiler or the runtime: nothing under
`Pl/`, `cl/`, `lib/`, `tools/` or `baselines/` is committed.**  The USER asked for it (2026-10-06):
"create a job for doing careful analysis of this before doing it — nicer and more readable CL is a good
thing.  Don't make it highly prioritized."  The reviewing session (Fable) rules on your report; the
change itself, if it is made, is a later batch.

Read first, through main's checkout `/home/bernt/pcl`:
1. `briefs-and-rules-for-claude-subagents/s510/SHARED-BOX.md` — FIRST ACTION (your model id into
   `$W/scratch/a2775/MODEL.txt`), the lock script for heavy legs, the rules at its end.  You were launched
   WITH worktree isolation: `$W` is your launch directory (check `git log --oneline -1`; if it is not
   main's head, `git rebase main` before anything else).  You are the LOWEST priority on the box: never
   queue more than one heavy leg at a time, and take none while `HEAVY.holder` names a `bench`.
2. `briefs-and-rules-for-claude-subagents/s473/COMMON.md` (the rulebook).
3. Task `~/.claude/tasks/pcl/2775.json` — the finding, the first measurement and the four open points.
4. `docs/ir-spec.md` on the `*wantarray*` context protocol, `docs/generated-cl-ir-review.md` §3.5, and
   `Pl/ExprToCL.pm`: `%WANTARRAY_SENSITIVE` (~line 222, read its comment and INVARIANT) and the funcall
   context rule that ends in `return $ctx == LIST_CTX ? Pl::CLForm::ctx_bind('t', $call) : $call;`
   (~line 3001), including the `join` paragraph just above it.

Your task IDs: 2800–2809 (file a pre-existing bug you find; do not fix it).

## The subject
`print rand 5, sin 4, rand sin 1, 2, 3;` compiles to
`(p-print (p-list-ctx (p-rand 5)) (p-list-ctx (p-sin 4)) (p-list-ctx (p-rand (p-sin 1))) 2 3)`.
`(p-list-ctx X)` is `(let ((*wantarray* t)) X)`.  Built-ins whose answer depends on context are in
`%WANTARRAY_SENSITIVE` and are bound in BOTH contexts.  Every OTHER built-in is bound in list context
only, by the fallback at the end of the rule — and in a scalar slot it is emitted bare.  If the table
is complete, the fallback is redundant.  `join`'s call-wide bind (same function) is documented there as
redundant for correctness since #2004 and kept only to avoid churn: it belongs to the same question.

## What the report must answer — each with its evidence on disk under `$W/scratch/a2775/`
**A. Is the table complete?**  List EVERY place the runtime reads `*wantarray*` (and
`*pcl-caller-wantarray*`): `cl/pcl-runtime.lisp`, the compiler-built artifacts (`cl/pcl-pack.lisp`,
`cl/pcl-mro.lisp`, `cl/pcl-warnings.lisp`), `cl/pcl-xs.lisp`, macros included (a macro that expands to a
read counts for every function using it).  For each reader: which Perl built-in(s) reach it, by which
emitted head, and whether that head is (1) in the table, (2) a non-funcall node with its own wrapper
(readline, glob, sort / map / grep, `do FILE`, …), (3) a user-sub call path (always bound), or (4) NONE
of these — a built-in that passes today only because of the fallback.  Class (4) is the finding: name
each, with a probe against perl (list slot, scalar slot, and called from inside a sub that was itself
called in the other context).

**B. Who runs USER code from inside a built-in outside the table?**  An overload handler (`abs $obj`,
`"$obj"` inside `join` / `sprintf` / `lc`, `==`), a tie method (FETCH / STORE / PRINT / READLINE), a
`sort` comparator named by a variable, `sprintf('%s', $obj)`, `local $SIG{__WARN__}` / `__DIE__`
handlers, a `DESTROY`-like callback, `AUTOLOAD`, `import`.  **And a REPLACED built-in** (task #2779:
`use subs`, an import list, `BEGIN { *CORE::GLOBAL::rand = sub { wantarray ? … : … } }`): the replacement is
user code reached through the built-in's own call form, so in a LIST slot the fallback bind is what gives it
list context today — probe all four override spellings in list, scalar and void slots, with and without the bind
(`~/pcl-agent-scratch/s510/review/foy/i-wantarray.pl`, `j-subs-wantarray.pl` are a start).  For each: what does `wantarray` answer
inside that user code in perl (list slot / scalar slot / void), in PCL today, and in PCL with the
fallback gone?  Today's scalar-slot answer is already "whatever the enclosing sub was called in", so a
difference that exists today in the scalar slot is PRE-EXISTING — file it once, as one task, and say
whether removing the fallback widens it.

**C. How much of the emitted code is it?**  Count, over the bench board (`tools/bench-exec.pl`'s
programs), the `everyday/` corpus, `lib/**/*.pm` and the 111-file corpus (`tools/corpus-diff.pl`'s
population): all context wrappers by head; the fallback binds by built-in name (top 30); `join`'s
bind; wrappers per 100 emitted lines before → after.  Decide the built-in / user-sub split from the
compiler's own facts (`%RUNTIME_NAMES`), not by eye — a counter in YOUR scratch copy of the compiler,
switched by an environment variable, is fine and stays uncommitted.

**D. What does a trial removal do?**  In a scratch extraction of main (`git archive HEAD | tar -x -C
$W/scratch/a2775/trial`; its own cache; NEVER committed), delete the fallback (and, as a second variant,
`join`'s bind as well; if A found class-(4) built-ins, a third variant with those added to the table).
Then, each heavy one through `heavy.sh a2775 leg …`, ONE at a time:
- `tools/corpus-diff.pl`: how many files change, and is every diff ONLY a lost wrapper?  (Normalise and
  prove it mechanically: re-insert nothing, strip the wrapper from the BASE emission with a script and
  compare — say how many files are then byte-identical and show every one that is not.)
- the gate (`tools/prove-core`): every failing row, classified — a transpile-SHAPE row that asserts the
  wrapper text (count them: they are the test churn the change would carry) vs a BEHAVIOUR row (each one
  is a class-(4) or class-B finding: explain it).
- the sweep (`perl tools/sweep-perl-tests.pl --jobs 4`): NEW / FIXED / LOST and the TOTAL line.
- `tools/everyday-smoke.pl --jobs 2`: the EVERYDAY line and every bucket.
- the companion `--all --quick --jobs 4` ONLY if the gate and the sweep are clean of behaviour rows.
A trial needs a generation bump in the scratch copy (stale cached module transpiles otherwise) and the
three artifacts regenerated there.

**E. What does it buy at run time?**  Task #2775 has one hand-made loop (0.651 → ~0.62 s, about 1.5 ns
per wrap).  Take the bench rows where C says the fallback is dense (at least five; plus `fibret`,
`methret`, `intloop` as controls where it should be absent), base vs trial, interleaved, K=5, twice,
as `heavy.sh a2775 bench …` — only when no other label is waiting for the lock.  State the control
band.  A gain inside the band is reported as "no measurable gain", not rounded up.

**F. What does it buy the reader?**  Three emitted files before / after (one bench program, one
everyday program, one `lib/` module): the changed lines side by side, and the wrapper count.

## The report
`docs/ctx-bind-fallback-analysis-s510.md`, committed ALONE on your branch (docs only) — plain language,
conclusion first: (1) the verdict in three lines — remove / remove after fixing N things / do not
remove; (2) the exact change that would be made (the lines, the table additions if any, the test rows
that assert the wrapper text, the bar from CLAUDE.md's WHAT TO RUN WHEN, the generation bump and the
artifacts); (3) sections A–F with their tables, each naming its evidence file under
`$W/scratch/a2775/`; (4) what you could NOT establish, said plainly.  Append a ten-line summary and the
doc's path to task #2775 (through `~/.claude/tasks/pcl/2775.json`; `JSON::PP->new->utf8`, `:raw`).
Then `REPORT-READY: <sha>` as the FIRST line of `$W/scratch/a2775/STOP.md`; if you have a SendMessage
tool, one line to `main`: `a2775 REPORT-READY <sha>`.

Keep `$W/scratch/a2775/STOP.md` current after every step (a resume recipe at its top): this job is the
first to be cut when the session ends.

## Final report (SHORT)
STOP.md's first line; your model id; the verdict; the class-(4) built-ins (or "none"); the counts from
C in two lines; the trial's gate / sweep / everyday result in three lines; the measured gain with its
control band; tasks filed; anything you could NOT do.
