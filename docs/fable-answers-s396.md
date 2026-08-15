# Fable answers to the s395 + s396 review requests (ruled s397, 2026-08-15)

Two batches, reviewed together because s395's review never happened before
s396 started (its ASK 2 — "take refaliasing first?" — was answered by doing
it, which turned out to be right; §3).  Requests:
`docs/opus5-review-requests-s395.md` (four commits + s395e–i docs, ASK 1
#323 scheduling, ASK 2 refaliasing grouping) and
`docs/opus5-review-requests-s396.md` (five commits + s396f–h, ASK 1 an
"internals" verdict, ASK 2 the orphan reaper's placement).

**Both batches are APPROVED as shipped.**  One review fix shipped
(`63a2344`, s397a — the s396 reaper was inert on this machine, §5), one
task filed from the review's probes (**#332**, §6), rulings on all four asks
below, queue in §7.

## 1. Independent verification (not inherited from the requests)

- **Cold gate** (`~/.pcl-cache` cleared, `tools/prove-core`): **141 files /
  5203 tests**, failures exactly the 13 pclxs xs rows (`xs-01.t` ×5,
  `xs-02.t` ×4, `xs-03.t` ×4 — the ABI-6-vs-8 drift the user has said to
  ignore).  Arithmetic: s396e's 5200 + s396h's 3 canary rows = 5203.
- **Full sweep RE-RUN** (the s396 `.faillog` held only a single-file
  aassign.t run, so the verdict could not be recomputed from artifacts):
  `perl sweep-perl-tests.pl --jobs 8` → **GATE clean, 0 new / 0 fixed,
  TOTAL passing 18539 = baseline (+0)**, the standing 2 UNSTABLE
  (postfixderef.t / ref.t crash-file noise) + 8 unverified.  64 fully
  passing files, min MemAvailable 5.2 GB.
- **Fresh probes vs perl 5.40.3, ten refaliasing / n-at-a-time shapes NOT in
  the guard file** (scratchpad `probes/p1..p10`): closures made inside
  `for my ($q,$r)` see their own pair; re-alias twice then write; alias to an
  anonymous array then re-alias; `for my ($k,$v) (%h)` writes through `$v`;
  `next`/`last` + `continue` interleaving and the k=1 spelling; the alias
  inside `eval '…'` (perl and PCL both leave the outer `$x` at 1 — a string
  eval aliases its own pad slot); labelled `next OUTER` out of a nested
  n-at-a-time pair; `foreach \my %e` with `last`; array alias + element alias
  mixed; `$p *= 2` write-back and `redo`.  **All ten identical.**  Plus one
  s395 battery (`our $count++` twice, `my $x : shared = 1` with a
  `MODIFY_SCALAR_ATTRIBUTES` stub, `@{+} @{-} $#- $#+`, `*main::plain`
  stringification) — identical.
- **Compile time** (pack.t transpile, `--no-cache`, two runs each, worktree at
  the s396 start `6fa8cb9` vs HEAD): 2.76 / 2.77 s → 2.76 / 2.87 s.  Noise —
  the two new raw-stream pre-passes cost nothing measurable.
- **The FYI reproduced**: `for my $r ({5,6},{7,8})` — perl `HASH HASH`, PCL
  `[5] [6] HASH`; recorded on #286 as its second population, correctly.
- **`docs/ppi-bug-report.t`** runs and every one of its 7 rows FAILS on PPI
  1.291, as designed (a failing row is the bug).
- **Diffs read end to end.**  Notes, none a blocker:
  - `%p-alias-expansion` is a closed dispatch that ends in a rule-12 die
    naming the target — s396d's four-spelling widening is exactly the
    kind of gap that die exists to surface (and did: it took the enclosing
    form out of the load, loudly).  The list-spelling lesson — sweep-diff's
    FIXED bucket counts a row that VANISHED — is now in DECIDED and is
    real; it is the FIXED-side twin of the LOST bucket's founding case.
  - `_repair_nary_foreach` serialises the LIST with `$list->content` and
    re-parses; a heredoc or comment INSIDE the parenthesised list would be
    re-serialised textually.  Acceptable (no such shape anywhere), noted so
    nobody is surprised.
  - The setup statements (`my @__PCL_FLn …`) land in the ENCLOSING scope of
    the `for`, re-executed each time control reaches the loop — the
    per-entry freshness perl gives, so a loop inside a sub or a `while` is
    fine.  The label moves in front of the `while` (probed with `next
    OUTER`).
  - The k=1 spelling `for my ($x) (LIST)` needs no pad and works.

## 2. s395 ASK 1 — #323 (three test.pl stubs that manufacture a PASS): OWN SESSION

**Ruling: schedule it as its own session, not a filler, after #331 and #332
and before the v0.1 track (§7).**  It is a baseline event by construction:
8 sweep rows (assignwarn.t, hashassign.t, time.t) plus at least eight
companion files flip red not for BUGS but because an assertion becomes
honest, and the s393 rule says each such row needs a cause — that is a
sweep + companion run + per-row baseline edit in ONE sitting, or the
baselines drift.  Nothing is hidden by waiting: the false passes are known,
bounded, and listed in the task.  Doing it "sooner at the cost of a noisy
baseline round" would just move the same per-row work into a session that
was doing something else.

Two things the session must carry: (a) the fix is `capture_warnings`-based
and `pl-warn` invokes `$SIG{__WARN__}` (not-supported.md records it), so the
comparison is evaluable — no new mechanism; (b) every flipped row gets its
cause in the baseline edit (which warning PCL does not emit / emits
differently), and rows whose cause is a *missing warning category* cite
#221 (the warnings model, unscheduled) rather than each growing a task.

## 3. s395 ASK 2 — refaliasing ahead of the per-file families: RATIFIED (retroactively)

s396 took #325 first and the result says it was the right order: ONE arm in
`p-setf`'s place dispatch closed four silent-wrongs and two refusals,
+149 companion rows and +4 sweep rows, corpus-diff one line.  The grouping
that the audit's refusal-text keying had hidden — four files, one feature —
is the kind of re-grouping the s392 method should keep doing: **key a
family on the FEATURE its files need, not on the refusal text**, because
the refusal text is where the compiler happened to stop, not what the file
is about.

And the estimate lesson stands as written: **"one refusal blocks N rows" is
an upper bound until the file has run** — re/opt.t was 639 of the ~1400 and
measures `re::optimization`.  Task #325's 46 % miss is the number to quote
the next time a family is sized from a plan.

## 4. s396 ASK 1 — a verdict for "measures the implementation, not the language": NO

**Ruling: no new suite verdict.**  Three reasons, then what the ask IS right
about.

1. **XDIFF already says the thing.**  A registered file still runs, is
   row-checked, and goes STALE the moment a row passes; its reason cites a
   not-supported section.  Whether the cause is *closable* is a property of
   that SECTION — the "what would lift it" line the s393 ruling on
   script_run/regex_sets made mandatory ("PCRE2 lifts this" / "perl-only,
   nothing lifts this") — not of the verdict.  Putting closability into the
   runner duplicates the doc and invites litigating which registrations are
   "truly" unfixable.
2. **Half the named files are not internals.**  IPC::SysV (io/shm.t) is a
   module PCL could shim over sb-posix one day — that is ordinary
   not-implemented, and calling it "unreachable by construction" would be
   false.  op/coresubs.t and op/svleak.t are still behind REAL compiler
   declines (F-F's state-in-named-sub decline, F-D's facts-scan gap) that
   stand on their own; UNEXPLAINED is the honest status for a file whose
   first blocker is ours, and the tasks now say "fixing this gains 0 rows
   HERE — do not size against these files", which is the protection the
   ask wants.
3. **The TOTAL is not owed by XDIFF files.**  C_ok summed over the
   population is "what passes"; what PCL OWES is P_ok − C_ok over UNEXPLAINED
   files only, and the report already prints XDIFF separately.

What the ask is right about is **countability**: the class should be
nameable in one grep.  So: ONE not-supported section, **"Readouts of perl's
own internals: `B::` optree inspection, `re::optimization`,
`XS::APItest`"**, that every such registration cites (fold the existing
`re::optimization` section into it as its first item; its "what would lift
it" line is *nothing — PCL has no perl optree and a different regex
optimizer*).  Then `grep -c` of that section name over
`docs/perl-suite-expected.tsv` IS the population, and the reason column says
so.  **op/const-optree.t (86/62, every diverging row a `B::` inlinability /
`:method` readout per the request) meets the all-or-nothing bar today and
may register under it** — with `--bless-rows` and the per-row read, as
always.  IPC::SysV gets its own ordinary not-supported entry if and when
io/shm.t is de-gated.

## 5. s396 ASK 2 — the orphan reaper: OPTION (1), and it is SHIPPED (s397a `63a2344`)

**Ruling: the fix belongs IN the server** — the one process that knows its
client died is the process that outlived it; every caller-side reaper is a
workaround that has to be repeated in every runner (and was: two copies).
Option (2), an SBCL exit hook, is rejected: it cannot see the SIGKILL case,
which is the one that actually happened.

**And the review found the shipped reaper INERT on this machine.**  Probing
the process tree the way the ruling asks for (`sh -c 'cmd &'`, then `ps -o
ppid`) shows every orphan under a `systemd --user` desktop session is
adopted by THAT process — a subreaper, PID 4471 here — never by PID 1; a
real orphaned `pl2cl --server` came back `ppid 4471, comm systemd`, and the
s396 regex `^\s*(\d+)\s+1\s+…` cannot match it.  "PPID 1 is the whole test"
was inferred, not measured.  Rule earned (DECIDED): **a process-tree
assumption is measured on the machine, not inferred from the textbook**.

What shipped:

- `pl2cl --server` arms a once-a-second `$SIG{ALRM}` tick that compares
  `getppid()` with the client's pid at start; a changed answer means the
  reply would go nowhere → `POSIX::_exit(0)`.  Safe signals deliver the tick
  between perl ops, so it fires inside a long transpile (the s396 orphans
  were caught MID-transpile — that is why EOF never reached them), and
  PerlIO retries the EINTR'd blocking read after the handler, so the
  protocol is untouched.  Measured: two requests answered across five idle
  ticks; a server orphaned 0.5 s into a 2.8 s transpile was gone 0.5 s
  after its parent died.  Worst case is "after the current op" — a single
  regex match that runs for minutes would delay it, which is why:
- the runners' reapers STAY as the belt, keyed now on "the parent is a
  REAPER" (PPID 1, or parent comm `systemd`/`init`), still never touching a
  server whose parent is an sbcl or anything else.  Verified against a
  subreaper-adopted fake and a parented one.

Cost stated: one `getppid` syscall per second per live server; zero on the
transpile path.  Guards: the four string-eval Pl/t files + refaliasing-01.t
run green against the ticking server and leave no process behind.

## 6. Review probes — #332 filed (a #325 residue that is SILENT WRONG)

perlref §"Assigning to References" lists the parenthesised-ARRAY spellings:
`\(@x) = \(@y)`, `\my(@x) = \(@y)`, `\(my @x) = \(@y)`, `\(@x) = (\$x, \$y,
\$z)` ("@x now contains $x, $y, and $z" — the array's contents are REPLACED
by aliases to the right-hand referents; `\(%h) = …` is forbidden by perl).
Probed:

    my @a; my ($x,$y)=(1,2); \(@a) = (\$x,\$y); $x=10; print "@a"
        perl: 10 2       PCL: (empty), exit 0
    my @y=(1,2,3); \my(@x) = \(@y); $y[0]=9; print "@x"       perl: 9 2 3    PCL: (empty)
    my ($x,$y)=(1,2); \(my @c) = (\$x,\$y); $x=10; print "@c" perl: 10 2     PCL: (empty)

Cause: the emitter lowers the LEFT as its RVALUE form `(p-list-scalar
(p-refgen-list @a))`, which is not a `\`-cast place, so `%p-alias-place-p`
never sees it and the write lands in a throwaway.  The SLICE spellings
(`\@a[5..7] = …`, `\@h{'foo','bar'} = …`) already DIE naming the target —
correct.  Fix shape (one mechanism: the sibling of
`_is_backslash_paren_lvalue` + one `p-alias-array-elements` helper that
resolves referents through `p-alias-scalar-target`), guard rows and the
sweep bar are on the task.  Occurs in no population — a filler, but rule 12
says the die is the minimum and it is three lines.

## 7. Queue

**Opus, in order:**

1. **#331** — regenerate `cl/pcl-pack.lisp` + `cl/pcl-mro.lisp` (eleven
   generations stale, v2-136 vs v2-147; the artifact-drift failure mode
   CLAUDE.md warns about, back again).  Bar as on the task (rebuild-pack →
   pack.t at `--timeout 380` → sweep-diff 0 new → mro → gate → full sweep),
   **plus the staleness CHECK as a Pl/t row** (artifact line-1 generation vs
   `*pcl-cache-generation*`) so it cannot recur silently.  Session opener.
2. **#332** — the parenthesised-array refaliasing spellings (§6).  Small.
3. **The ASK-1 registration** — the internals class section in
   not-supported.md (fold `re::optimization` into it), op/const-optree.t
   XDIFF under it with `--bless-rows`.  Docs + tsv, half an hour.
4. **#323** — its own session (§2).
5. **#314 family F-D** — record list-form (`my ($a,$b);`) and container
   (`my @a;`) declarations in `_scan_lex_facts` so the span rename stops
   refusing them (diagnosis on the task, s395); two-population gate-SET
   measurement.  Zero suite rows behind it (shm.t/svleak.t/taint.t then hit
   IPC::SysV / XS::APItest / `-T`) — it is done because it is a decline on
   ORDINARY Perl any module could hit, and it closes #314's last reachable
   family.  Then #314 CLOSES with its unreachable/blocked remainder listed.
6. **Size, do not start: the three reachable singles** — op/try.t (28,
   `try`/`catch`/`finally`, perl 5.34+, a real feature CPAN code now uses),
   op/lexsub.t (156, `my sub` — dies "Negative repeat count" in Parser.pm),
   base/lex.t (120, heredoc inside `${\ }` inside `s///e`).  One task each
   with a probe-first sizing; run/runenv.t stays blocked on #318.
7. **v0.1 track** (#277–#283), #281's vocabulary Fable-designed after the
   FOLD.

**Fillers, any gap:** #330 (read-only scalars), #321, #322, #324, #326,
#328.

**Fable:** #153 FOLD chunk 3 (measurement-first, design on the task) —
begun s397; then the #281 macro vocabulary; boxed aggregates.
