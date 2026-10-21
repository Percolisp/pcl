---
name: project_cpan_board_widening_s515
description: USER decision 2026-10-11 -- the CPAN board widens after s515a/s515b with hygiene first, then five dists chosen by use (Text-CSV, Path-Tiny, Getopt-Long, JSON-PP, HTTP-Tiny), each with a one-time row census; Type-Tiny later
metadata:
  type: project
---

**USER (2026-10-11, s515): "Please add them when reasonable."**  The 14-dist CPAN board (`~/pcl-agent-scratch/s513/board.sh`, dists under `~/.cpan/build`, blessed rows `baselines/cpan-board14-fails.tsv`) widens as the next correctness brief after s515a and s515b land (s515c):

1. **Hygiene first**: #2950 (the blessed baseline is stale: 40 NEW / 74 FIXED rows never edited -- the #223 recipe, row by row with causes, `# taken-at:` stamp); Scalar-List-Utils' 38 XS-only t/ files (all FAIL, a third of the board) leave the board or are marked XS-only so the count stops lying.
2. **Five dists by USE, not size**: Text-CSV (cpan-tests/modules/Text-CSV-2.04, already measured 769/58), Path-Tiny (fetch), Getopt-Long + JSON-PP + HTTP-Tiny (core: perl-5.40.3's `cpan/<dist>/t/` in the perlbrew build tree).  Type-Tiny (closures / overload / string eval stress) the round after.
3. **Each dist joins with a ONE-TIME row census by cause**, blessed row by row, never from a run.

**Why:** the four module-load blockers (Encode s513i, Digest + Storable s514a, File::Glob s514e) are gone; the population was limited by what could LOAD (Text-CSV 0 -> 769 the moment Encode loaded).  **What any bigger dist keeps rediscovering:** the two parked USER rulings #2370 (scope-exit DESTROY) and #233 (caller fidelity) -- raise them when the widening's census shows them again.

**How to apply:** write `s515/s515c-prompt.md` from this; the board's dist list lives in `board.sh` (scratch) -- the brief must also update whatever the repo treats as canonical (`tools/cpan-scoreboard.pl` takes any dist dir; `--no-dist-lib` for XS-stubbed dists).  Related: [[project_cpan_board_state]], [[project_network_and_cpan_available]].

**▶ REMINDER OWED TO THE USER (set s524, 2026-10-22 02:1x, the USER: "after a session or two, get more CPAN modules ... please remind me when it is time").**  TRIGGER: at the START of the session after the next one (s526), or earlier once the three board levers have landed -- #3650 (pack U0, in flight s524), #3493 (an expression-embedded `my` reusing an enclosing lexical; 4,000 IO-Compress rows) and the `use bytes` regex shape behind Text-CSV 70_rt (19,999 rows) -- Fable SAYS, unprompted, in its opening message: "it is time to get more CPAN modules" and proposes the list: the unrun items of THIS memory's list first (Getopt-Long; Path-Tiny deeper; HTTP-Tiny beyond its 5 rows), then new dists chosen by use.  Reason (s524 measurement): the board's cheap rows are picked -- 1,512 blessed rows left, ~38 % behind the #233 / #1361 deferral, the rest medium mechanisms -- and a NEW dist's first run has been the reliable source of simple bugs.  See [[project_cpan_board_state]].
