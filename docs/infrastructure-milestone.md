# The critical-infrastructure milestone — measured

*Task #1607, measured in session s481b (2026-09-10) on main `f330e885`
(generation v2-1220).  WORK IN PROGRESS — group A partially measured.*

## 0. The rule

**Perl 5.40.3 is the oracle and ROWS are the measure.**  A module "runs" when
its OWN `t/` produces, under PCL, the verdicts real perl produces on the same
machine, file by file and row by row.  PASS/PARTIAL/FAIL labels are not the
measure (a file can be labelled PASS on two of perl's twenty rows); the ok /
not-ok counts are.

Every row below is a command that was run, with its log under
`scratch/s481b/` of the measuring worktree:

* transpile — every `lib/**/*.pm` of the dist through `pl2cl --module`, counting
  files, hard errors, `PCL: statement dropped` announcements and ruled refusals;
* PCL's own `t/` — `tools/cpan-scoreboard.pl --jobs 4 --timeout 120`, which is
  the same runner the 14-dist CPAN board uses;
* perl beside it — the same files, the same `@INC` rule, CWD = the dist root.

A file that produces no rows **under perl too** (an author test, a missing
optional dependency, a network test) is `PERL-SKIP` and is never counted as a
PCL failure; those files are named per dist.

**Threshold for "runs":** every `t/` file that perl does not skip produces
under PCL the same ok / not-ok counts perl produces.  Nothing weaker — a module
a deployment depends on is not "running" at 90 % of its own assertions.

## 1. The list, module by module

(filled in as the measurement proceeds)

## 2. The count

(pending)

## 3. Blockers, ranked

(pending)
