# `baselines/` — the blessed measurement baselines

Data the runners compare a run against, curated row by row (a row leaves a
baseline by EDIT with its cause in the file's header notes, never by
re-blessing a whole file from a run — CLAUDE.md "Test Status").  Moved here
from `docs/` in s440 so that `docs/` stays browsable (USER).  Every file is
TAB-separated and may contain NUL bytes: read with `grep -a` or perl.

| file | what | reader |
|---|---|---|
| `fail-baseline.tsv` | the blessed FAILING rows of `perl-tests/*.t` (the sweep); SIX columns since s465 — the last is the CAUSE (#993) | `tools/sweep-diff.pl` |
| `pass-baseline.tsv` | per-file pass counts of the sweep (the LOST bucket) | `tools/sweep-diff.pl` |
| `parse-error-drop-census-s399.tsv` | the #138 drop census over six populations (the DROPS gate) | `tools/drop-census.pl`, `tools/sweep-diff.pl`, `tools/run-perl-suite.pl` |
| `row-shortfall.tsv` | rows the PLAN promised and PCL never produced, both populations, with a cause (the SHORTFALL gate, #993) | `tools/sweep-diff.pl`, `tools/run-perl-suite.pl` |
| `perl-suite-fails.tsv` | the blessed DIVERGING ROWS of perl's own `t/` — the companion's `fail-baseline.tsv` (#993) | `tools/run-perl-suite.pl` |
| `perl-suite-notrun-stamps.tsv` | when each never-run companion file was last measured (#993) | `tools/run-perl-suite.pl` |
| `perl-suite-run.tsv` | per-file verdict snapshot of perl's own `t/` (the companion suite) | `tools/run-perl-suite.pl` |
| `perl-suite-expected.tsv`, `perl-suite-expected-rows.tsv` | registered expected divergences / diverging rows of the companion suite | `tools/run-perl-suite.pl` |
| `perl-suite-fixture.tsv`, `perl-suite-timeouts.tsv` | fixture files and per-file timeout allowances of the companion suite | `tools/run-perl-suite.pl` |
| `cpan-scoreboard.tsv`, `cpan-widen-scoreboard.tsv` | the CPAN board verdicts | `tools/cpan-scoreboard.pl` |
| `cpan-board14-*.tsv`, `cpan-widen-causes-s323.tsv`, `perl-suite-run-s357.tsv`, `perl-suite-run-s365.tsv` | dated snapshots kept for attribution | (history) |
