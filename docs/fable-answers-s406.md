# Fable answers — the s404 + s405 + s406 batch (s407, 2026-08-16)

Three review requests were pending, not two: `opus5-review-requests-s404.md`
(§9's five asks + the #351 substitution in §10) was never answered either —
s403 ruled s402, and the s406 deferral named only s405.  All three are ruled
here as ONE batch, in the order they were written.  Verdict first, then the
independent verification, then the two regressions the review found and fixed,
then the asks.

## 0. Verdict

**All three sessions APPROVED as shipped**, with two review fixes landed in
this session and one filed narrowing:

* **s404** (`00b150f` #345, `a82acbc` #353, `d1ecafc` #349, `987c78a` #350,
  `a46aa3f` #354+#351, `e588c56` #342-1, `df10b95` #346-harness): approved.
  The #351 "not a term" substitution is RATIFIED (§4.6) with one amendment
  the review found the hard way (§2).
* **s405** (`8609e38` #358, `97c938f` #340, `a90b481` #277, `b0fb372` #347):
  approved.  The try/catch model is confirmed normative in ir-spec §6.3 (§4.7);
  the #347 judgment call (§7.3 of the request) is UPHELD.
* **s406** (`5376d99` #348, `d5d88c8` #355, `e7d16bf` #128, `32094da` #361):
  approved.  #361's `x` repair carried a REGRESSION on a shape its 17 probes
  did not include; fixed here (§2).  **#362 is CLOSED** — its real cause was
  not the one the task named (§3).

## 1. Independent verification

| measurement | this session |
|---|---|
| Gate `tools/prove-core`, cold, at `32cf956` | **147 files / 5355 rows**, the only failures the 13 pclxs xs rows (4+5+4) — matches the claim to the row |
| `tools/corpus-diff.pl` after the review fixes | **identical across 111 files**, silent drops 12 unchanged |
| `tools/emission-ab.pl --ref HEAD` over `lib/` | **18/18 SAME** |
| `tools/emission-ab.pl --ref HEAD` over perl's own t/ (556 files, the sizing population of #351/#354) | **556/556 SAME, 0 RCDIFF** |
| Full sweep after the runtime change (§3) | see §3.3 |
| try/catch: six shapes beyond the 25 oracle rows (implicit sub value, list value, loop `next`, `die` inside catch, `$@` sequencing across two constructs, `map { try … }`) | **identical to perl 5.40.3** — except the two spellings in §5.1/§5.2, which are not p-try defects |
| #347's six shapes | reproduced as reported (4 match, 2 registered "will not stay shared") |
| #358 standalone (`open FH,"<&=5"; <FH>`) | dies EBADF, returns |
| #348 `pl-which_perl` under `PCLPERL` / `PCL_FRESH_PERL=real` | as reported |
| #362's five-row table + the Sub::Defer shape | reproduced BEFORE the fix, identical to perl AFTER (§3) |

## 2. REGRESSIONS found by the review — one family, fixed here

The three token-stream repairs (`_repair_glob_multiply` #354, `_repair_word_match`
#351, `_repair_word_x_call` #361) all answer perl's operator-vs-term question,
and all three were blind to the same term: **a Word after `->` is a METHOD NAME,
and a method call ends a term.**  Probed against perl 5.40.3 at `32cf956`:

| shape | perl | PCL at 32cf956 | cause |
|---|---|---|---|
| `$o->name x 3` | `ababab` | **crash** — `main::pl-x` undefined: repaired to `$o->name + x(3)` | REGRESSION of #361 (`32094da`): the emission at `b9224ba` was the correct `(p-str-x (method-call) 3)` |
| `$o->w / $o->h / 2` | `2` | **dropped whole** (`print $o->w m/ $o->h / 2`) | REGRESSION of #351 (`a46aa3f`): compiled to nested `p-/` at `a46aa3f^` |
| `$o->w*w()` | `60` | dropped whole | PRE-EXISTING hole in #354's `_ends_term` — PPI lexes `*w` as a glob after the method-name Word, and the repair did not count that Word as ending a term |

None of the three shapes occurs in any of the four populations (corpus-diff
identical, lib 18/18, perl's t/ 556/556 — before AND after the fix), which is
exactly why the sizing scans could not see them; `$obj->method x N` and
`$a->w / $b->h` are ordinary CPAN spellings.  This is the standing rule
"**widening a parser rule: probe the case it would BREAK**" — the 17 `x`
probes and the 28-site population scan were all "does the repair fire where it
should", none was "where must it NOT fire" over the term forms.

**Fix (this session, `Pl/Parser2.pm`):** one predicate, `_is_method_name_word`
(Word whose previous significant token is `->`), consulted by `_ends_term`
(#354) and skipped-on by the two Word repairs (#351, #361).  And the `x`
repair now ALSO requires that **the document DECLARES a sub named `x` at all**
(`_document_declares_sub`, from the same token walk that already collects the
term words — one scan, two sets): perl reads `WORD x N` as a call to `x` only
where a sub `x` can be meant; with no `sub x` in the document valid perl can only
mean repetition, whatever WORD is.  That positive condition would have made the
`->name x 3` regression impossible on its own; the method-name rule is still
needed for a document that does declare `sub x`.  Guards: two rows in
`Pl/t/bareword-call-01.t` (all three shapes + `Foo->new->name x 2`, and the
no-`sub x` inverse).  Emission over all populations is unchanged by the fix
(§1); the shapes above are identical to perl now.

**Residue, accepted and recorded (not fixed):** an IMPORTED lowercase
`()`-prototype sub is invisible to the token scan, so `use Math::Trig; print pi
/ 2 + pi / 4` is now repaired to a match and dropped (announced) — before #351
it compiled as division of the STRING `"pi"` (0), i.e. it was silently wrong
already, and `pi / 2` alone still is (§5.4).  Rare (Math::Trig's `pi` is the
one common case); the honest fix is the #266 bareword classifier at the print/
operator sites, task #365.

## 3. #362 CLOSED — the cause was not `\&NAME` identity

The task said "`\&NAME` builds a NEW reference every evaluation, named subs
only".  It does not: `p-backslash-sub` returns `(symbol-function sym)` — the
same object every time — and #362's own exact reproducer answered `same` at
`32cf956` when both sides were plain boxes.  The discriminating measurement the
task did not take was the EMISSION of its probe:

    (p-my-= $cr1 (p-backslash-sub 'pl-f))                               ; a box
    (let (($cr2 (%pcl-to-number-strict (p-backslash-sub 'pl-f) "$cr2")))  ; a RAW NUMBER

Type flow freezes `$cr2` to a raw-numeric slot because its only use is `==`;
`$cr1` is also CALLED, so it stays a box.  Then `0+$cr1` = the object address
(box-nv has a `functionp` arm) and `0+$cr2` = **0** — `%to-number-raw`, the raw
value's numifier, had no `functionp` arm and fell through to `(t 0)`.  A code
ref is the ONE reference kind that is a raw function rather than a wrapper box
(`\$x`, `\@a`, `\%h` are boxes and never reach the raw path), which is why the
task measured "named subs only" and "anon coderefs compare equal": both sides
of the anon probe were boxes.  So: two numifiers for one value, one of them a
rule-12 `(t 0)` swallow — the [[feedback_check_for_a_second_copy]] shape.

**Fix (this session, `cl/pcl-runtime.lisp`):** `((functionp val) (object-address
val))` in `%to-number-raw`, next to the regex arm that already does the same.
Guard: `Pl/t/ref-identity-01.t` t21/t22 (the frozen-slot shape; the address is
non-zero).  All of #362's probes (e1, g1, the exact reproducer, redefinition,
`\&{"f"}`/`\&$n`, alias, qualified) are identical to perl now.  The Moo
`%DEFERRED` wall was NEVER this bug — Sub::Defer's install-then-lookup shape
already matched perl before the fix, as the task's own discriminating
measurement showed; `project_coderef_identity_blocker` is re-measured by the
board (task #208), not by this task.

### 3.3 The sweep (runtime changed, so it is owed)

Run on the working tree with both fixes in place: **GATE clean, 0 new / 0
fixed, TOTAL passing 18517 = baseline, DROPS 12 = census**, the standing 2
UNSTABLE + 8 unverified (crash-file noise).  As expected — the arm fires only
for a code ref numified through a raw slot.  Gen v2-150 → v2-151 (`b1bbdcd`),
the three artifacts regenerated (stamp-only diffs), pack.t 5636/89, 0 new.

## 4. The asks, ruled

### 4.1 s404 §9

1. **`re/speed.t` NOT registered — approved.**  The registry means "give it
   the time and it finishes"; a file measured to return the same 1 row at
   90/300/900 s is a hang, and registering it would make the registry lie.
   `re/pat_psycho.t`'s 450 s allowance from a real completion is right.
   s400 §7.4's premise was false for one of its two files: the measurement
   wins, no re-ruling needed.
2. **NOT-RUN rather than run-with-a-smaller-budget — RATIFIED.**  A truncated
   TAP stream is a different measurement, not a cheaper one; `--quick`'s bar
   ("identical verdicts for every file that runs in both") is the right one and
   NOT-RUN counting as UNEXPLAINED keeps the hole countable.
3. **`--extension` as its own flag — approved.**  The one difference from
   `--module` (diagnostics stay on) is exactly the s402 warm-cache rule: a
   developer building an artifact must see the drop.  Keep the three modes.
4. **#350 shipped rather than reported — approved, and the process point is
   answered:** "FIX = A MEASUREMENT" meant do not patch BLIND.  The
   measurement was taken first (52/657 files, one shape, nothing depending on
   the hoist), the probe matched perl, and the flip is simpler + faster +
   correct — the s379 sign-off rule.  Same session is fine when the measurement
   is in the commit.
5. **The rewritten `parser2-01.t` expectation — checked, all four conjuncts
   hold** (perl-probed; the diff is exactly the require moving; the edit
   strengthens — source order AND `unlike p-eval-always`; the runtime guard
   row is in `use-require-01.t` in the same commit).

### 4.6 s404 §10 — the #351 substitution ("not a term" instead of #266's classifier)

**RATIFIED, with the §2 amendment.**  Perl's own rule IS the negative — for a
non-term word `/` is a syntax error, not division — so under principle 9 "not
a term" is the whole test, and the classifier the s403 answer named cannot run
at repair time (no environment yet).  What the substitution missed is that
"term" is not only a property of the WORD but of its POSITION: a Word after `->`
is a term whatever its spelling.  Fixed (§2).  The imported-lowercase-`()`-sub
residue is accepted and filed (§2, #365).

**The `<op/*>` guard** and the population scan that found it are the model:
the 28-site scan asked "where does the repair fire", the review asked "where
must it not" — both are required for a token-stream repair from now on (write
the second list into the commit).

### 4.7 s405 §8

1. **The `$@` / `finally` model belongs in ir-spec §6.3 — CONFIRMED normative.**
   Re-probed here beyond the 25 rows: `$@` is `""` inside try AND catch, the
   pre-try value again inside finally and after (a following construct sees the
   intervening `eval`'s error, then clears it again inside its own try);
   `sub f { try { 5 } catch ($e) { 6 } }` returns 5 (implicit value); list
   context `try { (1,2,3) }` gives 3 elements; `next` inside try inside a loop
   runs finally then continues; `map { try { $_*2 } catch ($e) { 0 } }` works.
   All identical to perl 5.40.3.  Two things the model does NOT cover, neither
   a p-try defect: §5.1 and §5.2 below.
2. **op/try.t stays DIFF — approved.**  Register XDIFF only when every cause is
   a registered non-support; three of these four are live tasks.
3. **#360's layer — RULED (c′), and widened.**  A feature-enabling CORE pragma
   is LANGUAGE, not module behaviour: `feature`, `experimental` and the
   `use vN` bundles are perl's own, and the compiler already knows `strict`,
   `warnings`, `utf8` by name for the same reason — 9a's smell test is about
   CPAN modules.  But the mechanism is NOT a source rewrite of `use
   experimental` into `use feature`: **PPI has the hook** —
   `PPI::Document->new(\$src, custom_feature_include_cb => sub {…})` lets the
   host decide which parse features an include statement enables (PPI 1.291
   `Statement/Include.pm feature_mods`: it consults the callback FIRST).  So
   `_ppi_parse` (the ONE construction site) passes a callback answering from a
   small table of perl's core spellings: `use feature LIST` (PPI already),
   `use experimental LIST` (PPI only knows `signatures` there), and the version
   bundles — **`use v5.40` enables `try`** (PPI's own hack knows only
   `signatures` for ≥ 5.035), which the review found is a whole-statement DROP
   today (§5.1).  Cause (2) — `experimental.pm` dying on `for values %h` — gets
   the thin `lib/experimental.pm` shim (module layer, `import` = feature on +
   warnings off, delete-when: `for values %h` aliases, the E5 axis).  Task #360
   updated with both halves and the `use v5.40` shape; **string-eval
   inheritance of the feature is its own piece, task #364** (§5.2).
4. **#359 (`$^F`) — behind the release; the fd-3 hole ANNOUNCES.**  An fd the
   program marked inheritable that cannot be handed through is an effect
   (s329 boundary), so: `PCL: fd 3 cannot be inherited across exec` on stderr,
   with a `not-supported.md` entry, when `$^F >= 3` and a spawn happens.  Not a
   filler — the ~8 `run-program` sites make it a session.
5. / 6. **Installer shape — stand as shipped**: `$HOME/.local` default,
   `$PREFIX/lib/pcl` + `$PREFIX/bin` wrappers, `<root>/pcl.core` found by
   pattern.  #282's README describes them; a single-directory layout is not
   worth a second code path.

Also from the s405 request, ruled without being asked:
* **§7.3's judgment call (die → registered divergence for the "will not stay
  shared" family) — UPHELD**, for the reasons given: the die protected no
  coherent invariant (the family already diverged where it did not die), it
  cost whole files, and a wrong value here is a failing TAP row, never silent.
  The residue is one family with #337's shape 10 (§4.8 ask 4).
* **§5.3 (a sixth repair walk) — no measurement needed**: each repair reparses
  only when it fires, and the token walk is O(tokens) next to PExpr.  When a
  seventh arrives, fold the `$doc->tokens` walks into ONE pass that dispatches
  to the repairs — a note, not a task.
* **§1.5 (a spinning child outlives the run)** — the runners kill only the
  direct child on TIMEOUT; a grandchild spawned by `pclperl-for-tests` is
  orphaned and burns a core.  Filed as **#367**: run each file in its own
  process group and kill the GROUP on timeout (`setsid` + `kill(-$pgid)`), in
  both runners.  Same family as #273.
* **§1.3's protocol note** — carried into the hang protocol memory: for a hang
  take `/proc/<pid>/status` (State R vs S) WITH the fd list.

### 4.8 s406 §7

1. **io/crlf_through.t reading OK — (a), approved.**  #139 owns `:crlf`;
   io/crlf.t still measures it; the snapshot header carries the note.  One
   file measuring a gap is enough — the rule is that the gap is COUNTED
   somewhere, not that every file exhibits it.
2. **The runner does the serial re-run — YES, filed as #366.**  A companion
   file whose verdict or counts differ from the snapshot is re-run ALONE at
   the end of the run (the #215 shape the sweep has for LOST); both values are
   reported and the serial one is the verdict.  Cap the re-run set (say 40
   files) so a broken tree does not double the wall time, and print the cap
   when it bites.  It is a runners-row change: verdicts compared file-by-file
   before/after, `PCL_SHOW_SBCL=1` unchanged.
3. **Scratch `local::lib` of dev-only modules — inside the standing
   permission** (a dist fetch into the session scratchpad is not a system
   install).  The recipe is in `docs/test-debugging-runbook.md` now (§"Leak
   hunting"), because the negatives-only hour is exactly what it removes.
4. **#337 split — CONFIRMED.**  The rename half (shapes 1/2/3/12) is session
   F, `_rename_*`-family, region from the declaration to the end of the
   enclosing block, STOPPING at a sibling redeclaration (#296-B2); the sweep is
   its gate.  Shape 10 is the SAME residue as #347's registered family (a
   hoisted named sub cannot be a fresh closure per iteration; the fix is a
   promotion-decision change — a per-call cell for the hoisted sub) — record
   it ON that family in `not-supported.md`, size it separately, do not promise
   it with the rename.  Shapes 4/5: register (principle 9).
6. **#362 — CLOSED here (§3).**  It does not jump ahead of #337; there is
   nothing left to schedule.  What the task teaches: a "SILENT WRONG" task must
   carry the EMISSION of its reproducer — the `%pcl-to-number-strict` line was
   the whole diagnosis, and no amount of probing at the perl level could name
   it.  Added to the "suspect X" rule in DECIDED.
7. **The ALL-CAPS asymmetry — stands, and it is not two answers to one
   question.**  "Is this word a declared term" has ONE predicate
   (`_word_is_declared_term`).  What differs per repair is a SECOND question,
   "what is an UNDECLARED ALL-CAPS word most likely to be at this position":
   before `/` an imported constant (division), before `x` a filehandle (call).
   Those are legitimately different priors.  With the `sub x` positive condition
   the `x` side now barely depends on the prior at all.
8. Noted.  #348 landed correctly under §2c's standing.

## 5. Findings of the review, filed (all pre-existing unless marked)

### 5.1 `use v5.40; try {…} catch ($e) {…}` — whole statement DROPPED (announced, rc 0)

The :5.40 bundle enables `try`; PPI's version hack knows only `signatures`, so
the construct lexes as one swallowing statement and everything up to the next
`;` vanishes — including the statement after the catch block.  Same mechanism
as #360's cause (1); folded into #360 (the `custom_feature_include_cb` table
answers version bundles too).  Note `use v5.36` does NOT enable try (perl's
bundle does not include it until 5.40) — the table must say so.

### 5.2 A DROP inside a string eval is SILENT — even with `PCL_DROP_ANNOUNCE=all`

`p-ensure-transpiler` starts `pl2cl --server` with `:error nil`, so the
transpile-time announcement never reaches anyone (the emitter's own comment
records this as "a file-mode diagnostic in practice").  Measured: `eval q{ f
ref $u, "m" or g "fb"; 7 }` (the #343 reproducer) returns 7 with `$@` empty and
the call gone; `eval q{ try {…} catch ($e) {…} $v }` returns undef with `$@`
empty (the eval'd text carries no pragma, so PPI swallows the whole body).

**RULING: in eval-string mode a drop DIES** — a perl-shaped compile error that
lands in `$@`, exactly what the ruled `PCL: unsupported in string eval:`
refusals already do.  Perl's contract for `eval STRING` is "what does not
compile sets `$@`"; the reader of stderr does not exist there; and this is the
announce→DIE step Option B phase 2 ends with (§6.4 of the s400 answers), taken
early only for the path where announcing is impossible.  Task **#363**: die at
the two PARSE-ERROR emitters when `eval_mode`, message with the fixed prefix;
population first (the s373 three-leg bar: a one-sweep instrumented count of
eval-mode drops, TOTAL/LOST, gate-set scan not needed since file mode is
untouched).  **String-eval feature inheritance is #364**: the eval site's
`presumed_features` (PPI answers it per element) rides the server request next
to `eval_captures`, keys the eval cache (s387 rule), and reaches
`PPI::Document->new(…, feature_mods => …)` in `_ppi_parse`.

### 5.3 anon-sub `__SUB__` returns a NO-OP lambda (value-producing, rule 12)

`pl-__SUB__` is a stub returning `(lambda (&rest) nil)`; a factorial through
`__SUB__` in an anon sub prints 0.  Documented PARTIAL in `not-supported.md`,
but the s329 boundary says a missing case whose VALUE flows onward DIES.  Task
**#368**: make the runtime stub die perl-shaped ("PCL: `__SUB__` inside an
anonymous sub is not supported"), measure op/current_sub.t before/after (the
one companion file that reaches it), keep the named-sub rewrite.  Found because
#128's leak guard pointed lib/ authors at `__SUB__` — the guard's `lib/` scope
was wrong in both directions (a closure cycle is not a leak under SBCL, and the
remedy is a no-op there) and is removed in this session's fix commit.

### 5.4 An IMPORTED `()`-prototype sub is a STRING in operator/list positions

`use Math::Trig; print pi, "\n"` prints `pi`; `2 * pi` is 0; `pi + 1` is 3.14
(parsed as `pi(+1)`); `(pi, 1)` is `pi 1` — while `my $y = pi` and `foo(pi)`
are right.  Perl: 3.14 everywhere.  Same site as #356 (`print PI / 2`), which
#361 fixed for DECLARED constants only; the imported case needs the environment
(the module's prototypes ARE extracted — `my $y = pi` proves it) consulted at
the operator-loop term reading, i.e. the #266 classifier at one more site.
Task **#365**, with the probe table.  Silent-wrong today.

## 6. Housekeeping done this session

`docs/DECIDED.md` s407 section; `docs/session-log.md` s407; `docs/plan-post-s400.md`
§2d (the next Opus queue); CLAUDE.md pointer 2e replaced; `docs/test-debugging-runbook.md`
"Leak hunting"; tasks #362 (closed, cause corrected), #360 (widened), #337 (split
confirmed), #359 (ruling), #363–#368 filed; the two review fixes committed with
their guards.
