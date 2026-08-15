# Review request — s399 (Opus 5), for Fable

Five commits, the s397 queue worked in order (items 1–6).  Everything is
measured; the parts worth your time are the three places where **the premise a
queue item was filed under turned out to be wrong**, and one thing nobody had
ever counted.

Gate **142 files / 5275 tests** (failures exactly the 13 pclxs xs rows the user
said to ignore).  Full sweep **GATE clean**, TOTAL passing **18516** — down 24
by design, see §4.  `tools/corpus-diff.pl` identical at every step.  Cache
generation v2-147 → **v2-148**.

| commit | item |
|---|---|
| `7af2a97` s399a | #331 artifacts regenerated + a staleness GATE |
| `bf3fe69` s399b | #332 `\(@a) = LIST` silent wrong |
| `f8ffd56` s399c | ASK-1 internals section — and op/const-optree.t NOT registered |
| `6f04839` s399d | #323 the three warning helpers made honest |
| `c754abc` s399e | #314 F-D scalar half |

Sized, not started: **#340** (op/try.t), **#341** (op/lexsub.t), **#342**
(base/lex.t).  Filed: **#337** (`my sub` scoping), **#338** (F-D container
residue), **#339**, **#343** (the drop census).

**Two questions need you before anything else moves**, both in the same area:
**§7** — where the drop/decline signal belongs (#339; I tried a fix and
reverted it, and the revert is the interesting part) — and **§6** — whether the
census belongs in Option B phase 2's acceptance set.  §6 also carries a smaller
call (should a file-level non-lvalue assignment die like perl, or keep dropping
one statement).

---

## 1. #331 — there are THREE checked-in artifacts, and the drift is now a gate

The task named two; the test I wrote to stop the drift recurring found
`cl/pcl-warnings.lisp` (from `lib/warnings.pm`), also eleven generations stale.
It found it because the file **discovers** artifacts by their line-1 `gen=`
stamp instead of listing them, and carries a COUNT row so a header-format
change cannot make it pass vacuously.  That count row is the whole reason the
third one is not still stale.

Consequence, deliberate and worth your ruling: **an emission-changing commit
that bumps `*pcl-cache-generation*` must now regenerate the artifacts in the
same commit**, or `Pl/t/artifact-staleness-01.t` goes red.  That is what
CLAUDE.md already asked for and nothing enforced.  The cost is one
`tools/rebuild-pack` (~40 s) plus, when pack's emission really moves, its
pack.t verification.  I think that is the right trade; say if you disagree.

## 2. #332 — the fix is the sibling, and the RHS context was the hidden half

`\(@a) = LIST`, `\my(@x) = …`, `\(my @x) = …` all reached `p-setf` as the
RVALUE form `(p-list-scalar (p-refgen-list @a))` — a list of fresh refs, not a
`\`-cast place, so the alias arm never saw it and the array stayed EMPTY at
exit 0.  Recognised on the FORM (the one shape the three spellings share) and
lowered to `(p-backslash-list @a)`, so it flows through the SAME dispatch as
`\$x = REF`; the runtime arm resolves referents through the same
`p-alias-scalar-target` the scalar arm uses.

The half the task did not mention: **the right-hand side had been generated in
SCALAR context** (`(progn (p-backslash $x) (p-backslash $y))`, the comma
operator), so even with the place fixed a call on the right would have been
wrong.  It is now LIST context — probed with `\(@i) = refs();`.

Fifteen shapes byte-identical to perl.  Rule 12: any other `\(…)` target dies
at transpile, matching perl, which rejects `\(%h) =` and `\(@$ref) =` at
compile time.

## 3. ASK-1 — the section is in, but **op/const-optree.t must NOT register**

The class section exists as ruled ("Readouts of perl's own internals: `B::`
optree inspection, `re::optimization`, `XS::APItest`", `grep -c` over
`perl-suite-expected.tsv` IS the population, "what would lift it: nothing").

The ruling also authorised registering op/const-optree.t on the premise that
**every** one of its 62 diverging rows is such a readout.  The per-row read the
bar demands says:

| rows | what they are | class |
|---|---|---|
| 53 | `… is/is not inlinable` (28), `… has (no) :method attribute` (25) | this section |
| 5 | `… now throws exception (RT 134138)` — perl REJECTS `sub () { $x }` when `$x` is modified elsewhere; PCL compiles it | §Error compatibility for invalid Perl input |
| **4** | `retval of my sub …` | **a real fix target** |

The four are task #337: PCL compiles `my sub x () { 8 }` as a PACKAGE sub, so
two same-named lexical subs in different scopes clobber each other and every
`\&x` resolves to the last one —

    my $g1 = sub { my sub x () { 8 } \&x };
    my $g2 = sub { my sub x () { 3 } \&x };
        perl: 8 3        PCL: 3 3        silent

In ISOLATION PCL is right, which is why it took a file with two of them to
show.  All-or-nothing keeps the file UNEXPLAINED; the section records the split
so re-registration after #337 needs no re-derivation.  I ran `--bless-rows` and
reverted it with the row.

**The general point for future rulings:** a registration authorised in the
abstract still has to survive its per-row read, and this one is the second time
that read has changed the answer (s393's `unlike` was the first).

## 4. #323 — 24 rows went red on purpose, and **#221's trigger has fired**

The three helpers ran the code and `pass()`ed unconditionally.  Replaced by the
real t/test.pl bodies on top of `capture_warnings`.  Every row that flipped has
ONE cause: PCL emits no warnings-gated diagnostic.

    perl-tests/assignwarn.t  116/0 → 96/20    Use of uninitialized value
    perl-tests/hashassign.t  309/0 → 305/4    odd elements / even-sized list
    perl-tests/time.t        72/0 unchanged   (its rows expect NO warning —
                                               they pass HONESTLY now)

Task #221 (a minimal `use warnings` model) was UNSCHEDULED pending "the first
test family or CPAN module whose failure is *warning not emitted*".  **This is
that family**, and with the companion suite it is 121 rows (assignwarn 20,
hashassign 4, numify 11, utf8decode 86).  I did not schedule anything — the
standing rule (default-off diagnostics stay ABSENT, never unconditional) is
untouched, and there is now a not-supported section with #221 as owner.  Your
call whether the trigger firing changes #221's priority.

Two measurement notes:

- **The companion population is CLOSED**: eight files call these helpers
  anywhere under perl's `t/`, and all eight were run.  No `--all` needed.
- **op/assignwarn.t's row set is nondeterministic** and is registered
  `*rows-unstable*` on measurement, not assumption: the file iterates
  `keys %should_warn`, so both sides emit rows in per-process random order and
  the description-multiset pairing gave 81 then 73 diverging rows in two
  consecutive runs.  The COUNTS are stable, so the file still registers XDIFF.
- op/utf8decode.t (644/42 → 620/90) stays UNEXPLAINED: 86 are this entry, 4 are
  a pre-existing divergence of another kind (verified in a worktree at
  `f8ffd56`).  Its row count now MATCHES perl's 710, which retires the
  snapshot's "numbering is offset for 592 rows" note — the old stub emitted one
  row where perl emits two.

Baselines edited ROW BY ROW with a header note; `sweep-diff` clean against the
edited pair.

## 5. #314 F-D — the scalar half shipped; the container half was diagnosed wrong

`my ($fetch, $store) = (0, 0);` spanning a package boundary refused with
`sdecls=0 dc=1` — the declaration COUNTED but no statement was found to rename.
perl declares each name of a list form exactly as the single form does, and the
pass renames ONE symbol inside the declaring statement either way, so the two
facts are merged for that pass in one accessor (`scalar_decl` + the `$`-sigil
entries of `mlist_decl`, which already recorded exactly this).  The promotion
pass keeps reading them separately — merging there processes a list decl twice.

Measured at the s372 bar with a **GATE-SET scan of 638 files** (111 perl-tests
+ 527 of perl's t/, first stderr line normalized, ~2.5 min per population):
FOUR rows differ and only four.

**The s395 diagnosis of the container half was imprecise** — it is not "the
facts scan does not record container declarations" (`container_decl` and its
own span loop have existed since s305).  It is two things (#338): op/svleak.t
declares `my @a` three times, so the container loop's file-uniqueness rule
refuses; and a container name inside a list decl (`my ($x, @rest)`) needs the
DECL LOWERING, which knows only the no-init single-container shape.  The first
may be lifted by direction D (a `let` of a symbol-macro name is legal and
shadows it — the comment that justifies the rule predates that), but that is
your call, and it is worth ZERO rows.

Small thing in the same commit: the checker's die now names the CANONICAL
variable (`my-lexical 'a' (canon @a) …`).  The bare name hid the one fact that
says which loop must handle it.

## 6. The thing nobody had counted: **72 files carry a silently dropped statement**

Chasing a stderr line the gate-set scan kept printing led to
`(progn ;; PARSE ERROR: … nil)` — the #138 family.  Census in
`docs/parse-error-drop-census-s399.tsv` (task #343), taken at `c754abc`:

    72 files, 379 drops    9 perl-tests (12 drops) · 63 companion (367) · 0 lib
    56 files  "Bug. Fell through. Missing case: ["
    21        "Handle single node of unknown type"
     5        a ruled "PCL:" refusal swallowed into a drop (deliberate, §below)
     1        Can't locate object method "add_node" — an INTERNAL error, hidden

**A drop is not cosmetic.**  perl-tests/bless.t's single drop is

    is ref $untied, "main", 'blessing through tied refs' or diag $@;

— an assertion that never runs and appears in no count, in a file the sweep
reports as passing.  The dominant cause minimises to

    f ref $u, "m", "d" or g "fb";    perl: f( m d) g(fb)    PCL: (nothing)
    f ref $u or g "fb";              perl: f() g(fb)        PCL: (nothing)
  INVERSE, both fine today:
    f $u, "m", "d" or g "fb";        f(ref $u, "m") or g "fb";

i.e. **parenless call × named-unary first argument × a following low-precedence
`or`/`and`** — which is `Test::More`'s `is … or diag $@`.

I did not touch it: that is the `$end_pars` region
`pexpr-term-parsing-review.md` says not to patch in place.  **My ask: put this
reproducer, with #259 (a `(;$;)`-prototype call drops) and #335 (a list slice
heading a postfix-if drops), into Option B phase 2's acceptance set, and make
the census phase 2's metric** — re-run `tools/drop-census.pl`, the number falls, every
remainder explained.  Two cheaper pieces that need no parser work are on the
task: the `add_node` internal error, and a GATE (a per-file drop count vs a
blessed census) that would have caught the s398 `(LIST)[i]{k}` drop
automatically.

**One design question for you.**  The 5 "PCL:" refusals that appear as drops
are DELIBERATE: `_shape_expr_error` re-raises every `PCL:` error except
"Can't modify non-lvalue subroutine call in assignment" outside eval mode (the
eval half must propagate so the eval returns undef, like perl).  perl refuses
to compile the whole FILE for the non-eval case; PCL drops the one statement
and runs on.  33 of the 379 drops are that.  Keep, or make it die?

## 7. ASK — the stderr line that is right about the wrong thing (#339)

**This is the ask I most want answered, because it decides what "loud" means
for the whole drop family, and I could not settle it from the rules.**

`Pl/PExpr.pm` (~808) ends its single-node dispatch with a `warn` and then a
`die`:

    warn "Handle single node of unknown type: ref='" . ref($e1) . "'\n";
    die  "Handle single node of unknown type. Dump:\n" . dump($e1);

**Measured (s399 gate-set scan, 638 files):** 25 of the 527 companion-suite
files print that line, and every one of them compiles — rc 0, correct-looking
CL.  t/op/glob.t prints it twice and emits 294 lines.  The ref values seen are
`PPI::Token::Operator`, `PPI::Token::Cast` and `''`.

So my first reading was "error-shaped noise for a routine decline" — the term
walker declines bare words and prefix ops BY DESIGN, the caller catches the die
and takes another route, and **two callers have already had to work around this
very line**: `Pl/VarAnnotator.pm` (~695) and `Pl/Parser2.pm` (~8415) both wrap
their analysis parses in `local $SIG{__WARN__} = sub { }`, with comments naming
this warn and the rationale "any real problem repeats in the actual lowering"
plus "test helpers merge pl2cl's stderr into the generated CL".  Two
independent workarounds for one line is normally the tell that the fix belongs
at the line.

**I deleted the warn, and reverted it — because the reading was wrong.**  The
emission diff showed why: in t/op/glob.t that decline is not handled at all, it
becomes

    (progn ;; PARSE ERROR: Handle single node of unknown type. Dump:
     nil)

i.e. the statement `ok <~>, '~ works';` is DELETED (the `<~>` glob).  The same
message therefore covers two opposite events: a decline the compiler recovers
from (silent would be right) and a statement that vanished (loud is the whole
point — rule 12's "the sin is the silence").  Deleting the warn would have
removed the only runtime-visible signal of the second.

**The question: where does the signal belong, and what is it?**  What I would
do, if you agree:

- **(b) move the announcement from the DECLINE site to the DROP site.**  Say
  nothing at `PExpr.pm`'s die — a decline is not an event — and announce ONCE
  at `Pl/Parser.pm`'s `_shape_expr_error` / the `(progn ;; PARSE ERROR …)`
  emitter, which is the place that actually knows a statement is being
  replaced by nil, and the only place that can name the FILE, the LINE and the
  source text.  That turns 25 files of undifferentiated noise into 72 files ×
  N precise "statement dropped at F line L: <text>" lines — which is also
  exactly the signal `tools/drop-census.pl` reconstructs after the fact, and
  what a per-file drop gate (§6) would key on.

The alternatives I considered and why I did not pick them: **(a) leave it** —
the noise is real (it made this session's gate-set scan harder to read, and it
is why two callers silence warns), and the message is attached to the wrong
event; **(c) make the drop a hard error** — that is the separate design call
below, and it should not ride in on a diagnostics change.

Cost/risk if you say yes: it changes stderr for many files, so the bar is a
`tools/gate-set-scan.pl` diff over both populations plus the gate — I have the
tool now.  It does NOT change emission (the die text lands in the PARSE ERROR
comment; keeping that text identical keeps corpus-diff clean, which the aborted
attempt confirmed).  One open sub-question I could not answer from the data:
the `ref=''` case (a node with no class, in comp/final_line_num.t, op/closure.t,
re/pat.t, re/pat_re_eval.t) may be a repair-pass artefact rather than a
legitimate decline — worth one probe before or after, either way.

## 8. Also filed

- **#341** corrects #314's note on op/lexsub.t: it does NOT die on "Negative
  repeat count does nothing" — that is a warning printed ten times, from a
  NEGATIVE `indent_level` in `Pl/Parser.pm:8908` (its own small bug).  The
  killer is the capture refusal.
- **#342** records that base/lex.t's blocking shape is a rule-12 violation in
  isolation: pl2cl exits 0 and emits `(p-subst … (lambda () nil) :e)`, so the
  s///e replacement silently becomes nil.

## 9. Queue as I leave it

Remaining from the s397 list: the **v0.1 track** (#277–#280, #282–#283) — I did
not start it, because it is release engineering with user-facing decisions
(installer shape, repo hygiene, CI) rather than mechanical work.  Fillers
unstarted: #330, #321, #322, #324, #326, #328, plus this session's #337–#343.

---

## Addendum (after the queue): #324 was the harness, and the run that proves it is unfinished

Three more commits after §9 was written.

- `07069ab` — `corpus-diff.pl` prints the SILENT-DROP count on both sides.
  Free (it already has both emissions) and it is the cheap half of §6's gate
  idea: the perl-tests corpus is covered on the tool run that every emission
  change already does.
- `275540a` — **#324 is not `(?{ CODE })`.**  `tools/run-perl-suite.pl` was
  the only runner without `--control-stack-size 512`, so the companion suite
  measured PCL on SBCL's 2 MB default — 256× smaller than the gate, the sweep
  and `./runpcl` — and four files died `control-stack-exhausted` there and
  nowhere else.  s395's probes "did not reproduce" because they ran through
  `./runpcl`, which has the flag.  +37 C_ok: re/pat_rt_report.t 2431/39 →
  2454/56 (it now runs to the end), op/utf8cache.t 0/2 → 2/0,
  re/pat_psycho.t 0/11 → 11/0, re/speed.t 0/0 → 1/0.

**What is NOT verified, and it is the next session's first step:** the `--all`
confirmation run was stopped by the user at **391 of 521 files**.  Those 391
differ from the snapshot in exactly ONE row — `mro/package_aliases_utf8.t`, the
registered `*rows-unstable*` file at its known serial value — so nothing is
known to have moved, but re/, run/ and uni/ (~130 files) have not been looked
at with the flag on.  The snapshot header says so in place.

**Two decisions this hands you.**  (1) re/pat_psycho.t and re/speed.t no longer
crash — they now RUN the pathological patterns they exist to time, and PCL is
slow on them (>400 s for pat_psycho under `--jobs 1`).  Buying their ~12 rows
with a `docs/perl-suite-timeouts.tsv` allowance costs minutes of every full
companion run; I left their rows carrying the old crash signature rather than
make that call silently.  (2) **#344** — four runners hand-write their own sbcl
command line and this is the second drift (the sweep's `--load` of the test
library was the first).  `PCLCore::sbcl_prefix` is already the helper; the work
is moving it where the other three can use it, with byte-identical command
lines as the acceptance.

**One process note for the cadence rule.**  This session ran the full
perl-tests sweep four times — once per code commit — where the standing rule is
"every 3rd–5th change".  Each was defensible under a standing rule of its own
(a `cl/` change, a runtime change, a baseline-moving change, a name-resolution
change), but it is most of the session's wall time, and if you want the rule to
win over those, say so and I will batch.

---

## ASK (user, at session end) — look at the test suites: optimize WHAT WE RUN WHEN

> "that one hour sweep can't be run every half hour" — and the cadence rule
> ("full sweep + suite only every 3rd–5th change") is the only thing written
> down today, so each session re-derives the rest.  The user asked for this to
> be looked at as a whole.  Sizing task is **#345**; the design call is yours.

**The portfolio, with what each one costs and — the part that matters — what
it is BLIND to.**  Measured this session unless noted.

| measurement | cost | sees | blind to |
|---|---|---|---|
| `tools/prove-core` (gate) | ~4 min | 142 files / 5275 rows of PCL's own tests, transpile + runtime | everything not in `Pl/t/` |
| `tools/corpus-diff.pl` | ~2 min | BYTE emission over the 111 perl-tests files vs a ref | runtime behaviour; `cl/`+`lib/` (loaded, not transpiled); anything outside perl-tests |
| full perl-tests sweep | ~10 min | 108 files / 18516 rows, runtime, both baselines, module transpiles | perl's own suite; transpile-only regressions in files it does not run |
| companion `--all` | **30–60 min** | 521 files of perl's own t/ | — (it is the widest, and the most expensive) |
| `tools/gate-set-scan.pl` (new) | 2.5 min/population | the transpile VERDICT of 638 files across BOTH populations | runtime — a file that compiles differently but still runs looks identical |
| `tools/drop-census.pl` (new) | 3.5 min | silently dropped statements in the emitted CL of 658 files | everything else |
| `tools/pcl-conform` | minutes | XS conformance, 398 cases | the language |

**Three things this session showed about that table.**

1. **Two of my four full sweeps were predicted-null and found nothing.**  Both
   followed an IDENTICAL `corpus-diff` with no `cl/`, `lib/` or
   `perl-tests/t/` change and no name-resolution change — i.e. the standing
   rule "corpus-diff's corpus IS the sweep's input set" already implies the
   sweep cannot move.  ~20 minutes.  The rule exists as a POSITIVE ("run
   corpus-diff first"); what is missing is the NEGATIVE ("…and then do not run
   the sweep").
2. **Half the companion hour is the known-bad tail** — the six `re/regexp*.t`
   hang files (#326) each burn the full per-file timeout and produce nothing;
   `re/pat_psycho.t` and `re/speed.t` now add ~10 min for ~12 rows.  #345
   proposes a `--quick` that skips them while still REPORTING them as not-run.
3. **The cheapest gate is often a new one, not a bigger old one.**  Eleven
   generations of artifact drift were invisible to every measurement above;
   what caught it was a three-grep `Pl/t/` row costing zero seconds.  Same
   shape available for the drop census (#343) and, per #344, for the four
   runners' divergent sbcl command lines — which is the class of bug NOTHING
   in the table can see, because each runner defines its own reality.

**What I would ask you to rule on:** a decision table keyed on WHAT CHANGED
(`Pl/` vs `cl/` vs `lib/` vs `perl-tests/t/` vs `tools/`) rather than on a
change COUNT, since the count rule is what both over- and under-fires; whether
`--quick` is the right shape for the companion run; and whether any of the
three new tools (gate-set scan, drop census, the corpus-diff drop counter)
should become standing per-change steps or stay on-demand.  My instinct is
that only the drop counter earns per-change status — it is already free — but
that is exactly the kind of call worth having ruled once instead of re-argued
per session.
