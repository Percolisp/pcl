# Review requests — session 406 (Opus 5, 2026-08-16)

> **The s405 request is STILL UNREVIEWED** (`docs/opus5-review-requests-s405.md`
> — deferred by the USER, `plan-post-s400.md` §2c).  Read it FIRST, then this
> one: the two are meant to be reviewed as one batch, and §7 below says which
> of this session's work depends on an s405 ask being ruled a particular way.

## 1. What landed

| commit | what |
|---|---|
| `5376d99` s406a | **#348** — `which_perl`'s children run PCL (`$PCLPERL`); zero rows moved in either population |
| `d5d88c8` s406b | **#355** — one gate transpile helper: stderr is captured separately and JUDGED, never fed to the Lisp reader |
| `e7d16bf` s406c | **#128** — the transpiler leaked ~8.5 kB per transpile through one self-referential closure; `__SUB__` fixes it |

Docs/tasks in the same session: `DECIDED.md` s406 section, `session-log.md`
s406, `perl-suite-run.tsv` (one row spliced + two notes), `plan-post-s400.md`
§2b, tasks #346/#347/#358 marked completed (s405 closed them but the tracker
still said pending), #359/#360 given task files at last, **#361 filed** (a new
silent wrong, §5), #337/#342 given their probe measurements.

## 2. Measurements

| measurement | value |
|---|---|
| Gate `tools/prove-core` | **147 files / 5348 rows**, PASS except the 13 pclxs xs rows (user: ignore) |
| Full perl-tests sweep (after #348) | **GATE clean, 0 new / 0 fixed, TOTAL 18517 = baseline**, drops 12 = census, 2 UNSTABLE + 8 unverified (the usual crash-file noise) |
| 19 companion which_perl callers, before/after | identical counts, **byte-identical failure logs** (12 `.fails.tsv`) |
| Companion `--all --quick --jobs 4` | 36 rows differ from the snapshot: 11 `--quick` NOT-RUN, 1 registered rows-unstable, 1 timing-only status flip, **22 contention** (each reproduces the snapshot when re-run alone), **1 real** (§3) |
| corpus-diff (after #128) | **IDENTICAL across 111 files**, silent drops 12 unchanged |
| emission-ab over `lib/` | 18 files, 18 SAME |
| `pl2cl --server`, 200 requests | RSS +16 kB total (was **+4.1 MB**) |

## 3. #348: it landed for free, and the one row that moved is a WARNING, not a win

The switch itself is four lines.  What matters is that with #358 and #347 fixed
the two s400 holes are gone: `perl-tests/closure.t` stays **OK 272/4** (s400:
PARTIAL 240/28), `run/cloexec.t` stays DIFF 16/6, and every one of the 19
companion callers is byte-identical in its failure log.  The children really
did switch — op/closure.t + run/cloexec.t take **28 s** with PCL children vs
**7.7 s** under `PCL_FRESH_PERL=real`, and a direct probe of `pl-which_perl`
answers `$PCLPERL` / `$^X` / `$^X` for the three cases.

**The one moved row: `io/crlf_through.t` 726/216 DIFF → 942/0 OK — and it is
not a fix.**  That file is `$main::use_crlf = 1; do './io/through.t'`, and
through.t pipes its data through a `which_perl` child.  With a real perl child
those 216 failures were PCL's `:crlf` layer disagreeing with perl's; with a PCL
child both ends agree.  Re-probed:

    open '>:crlf'; print "a\nb\n"   perl: 61 0d 0a 62 0d 0a   PCL: 61 0a 62 0a
    open '<:crlf' over "a\r\nb\r\n"  perl: 610a|620a           PCL: 610d0a|620d0a

i.e. the layer is a NO-OP in both directions.  Recorded on **#139** (which
already owned it via io/crlf.t, 13 rows, still measuring it) and in the
snapshot's header.  **Ask 1** below.

## 4. The contention finding (a portfolio rule, please rule on it)

An `--all --quick --jobs 4` companion run differed from the snapshot in 36
rows.  Twenty-two of them were **contention, not regressions**: op/lc.t,
op/magic.t, op/ref.t, op/sprintf2.t, op/stash.t, op/print.t, op/splice.t,
op/stash_parse_gv.t, op/heredoc.t, op/method.t, op/packagev.t, op/range.t,
op/readdir.t, op/readline.t, op/repeat.t, op/reset.t, op/smartmatch.t,
op/sort.t, op/split.t, op/sub.t, op/substr.t, op/lex_assign.t — every one of
them reproduced the snapshot value EXACTLY when re-run alone.  These are files
that spawn `fresh_perl`/`runperl` children; when the machine is busy the
children lose rows, and #348 makes the run busier (19 more files now spawn PCL
children).  s405's own `--all --quick` saw none of this, so it is load, not a
step change.

I did not change the runner.  **Ask 2** below.

## 5. #361 — a new silent wrong, found by a probe (filed, not fixed)

    sub x { "PKG" }
    print x(), "|\n";        # perl: PKG|      PCL: prints NOTHING, rc 0

PCL reads the bareword `x` in print's argument list as the REPETITION OPERATOR
and emits `(p-str-x (p-print $_) (progn))`.  Narrow and probed: `my $r = x()`,
`my @l = (x(), 1)`, `$obj->x` and `print((x()), …)` are all correct — only the
list-operator argument position diverges, and `x` and `not` are the only
operator-shaped names that are legal sub names at all (the rest are perl
compile errors).  Layer: the `_is_print_term_start` family, with #266's rule
("a bare NAME is a CALL only where it is CALLABLE").  The case a widening would
BREAK is `print "-" x 5, "\n"`, so that is the probe the fix must carry.

## 6. #128: what the leak hunt cost, and the tool question

The diagnosis chain was: driver → linear growth with no plateau → weak-ref
canaries (every PCL object and every PPI document FREED) → no package variable
grows → therefore a cycle → **arena census** (Devel::Gladiator) → exactly 2
CODE refs per transpile, both from `_seam_lex_assign_fix`'s
`my $walk; $walk = sub { … $walk->(…) }`.

Everything up to the arena census took an hour and proved only negatives; the
census took two minutes and named the line.  I installed Devel::Gladiator and
Devel::Cycle into a **scratch `local::lib` under the session scratchpad**,
nothing in the tree and nothing in the perlbrew perl — the standing rule is
"dist fetches blanket-OK'd, system installs still ask", and this was neither.
**Ask 3** below.

## 7. Asks

1. **`io/crlf_through.t` reading OK is a coverage hole** — the file now agrees
   with itself.  Options: (a) leave it, since io/crlf.t measures `:crlf`
   directly and #139 owns it (what I did); (b) register the file somewhere so
   the hole is countable; (c) fix `:crlf` (a real layer, #139's design call).
   My recommendation is (a) — one file measuring a gap is enough, and #139
   already carries the probe.
2. **A companion row that moved is not a finding until it has been re-run
   alone** — I wrote that into `DECIDED.md` and the snapshot header as a
   working rule.  Should the RUNNER do it instead (re-run a file whose verdict
   differs from the snapshot, serially, and report both — the #215 shape the
   sweep already has for LOST files)?  That is a real change to
   `run-perl-suite.pl`, so I did not do it unasked.
3. **Leak-hunting tools**: is a scratch `local::lib` install of dev-only CPAN
   modules (Devel::Gladiator, Devel::Cycle) inside the standing permission, or
   should it be asked each time?  If it is fine, the recipe belongs in the
   debugging runbook, because the negatives-only phase is exactly what it
   removes.
4. **#337 (`my sub`) is measured and ready** — the 12-shape probe table is in
   the task.  The rename half (shapes 1/2/3/12) is a `_rename_*`-family change
   with the sweep as its gate; the per-iteration-closure half (shape 10, which
   also hits #347's refusal inside a loop body) is a separate axis I would size
   on its own.  Confirm the split before I start, since the plan lists F as one
   session.
5. **Nothing here depends on an s405 ask being ruled a particular way.**  #348
   landed under §2c's standing (its blockers were fixed, so it was no longer an
   interim call), and I did not touch #359 (still behind the release: the
   `:preserve-fds` widening is ~8 call sites plus open-time marking, not one
   line) or #360 (do-not-start).
