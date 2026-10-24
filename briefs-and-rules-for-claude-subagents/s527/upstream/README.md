# The upstream package for PCL's PPI branch (s527a, 2026-10-24)

The branch `pcl` = upstream PPI master `455bfa73` + 23 commits.  The fork
`Percolisp/PPI` did not exist (or could not be read) when s527a finished, and
`gh` is unauthenticated on the dev box, so the branch travels here:

- `0001-*.patch` .. `0023-*.patch` -- `git format-patch master..pcl`; apply
  on upstream master with `git am *.patch`.
- `pcl-branch.bundle` -- the same commits as a git bundle:
  `git fetch pcl-branch.bundle pcl:pcl` in a clone of upstream PPI, then push
  the branch to the fork.
- `PR-A.md` .. `PR-I.md` -- one text per PR group: title, the perl rule with a
  perl one-liner, PPI's tokens before and after (`dump-<letter>.txt`), the rows
  of t/ppi_bugs_pcl.t it fixes, its commits, what it depends on.

Every commit leaves PPI's own suite passing (`prove -l -r t`, with
AUTHOR_TESTING=1 too: no warnings).  Commit 0001 adds `t/ppi_bugs_pcl.t` (PCL's
docs/ppi-bug-report.t in PPI's style, 77 rows in TODO blocks); each PR is "TODO
tests, then the fix that removes the markers", PPI's house rule.  On the branch
33 of the 91 rows pass (14 on master).  Groups E and F carry two pairs each:
the second pair of E widens the term test to builtins (`print ~~$y`), the
second pair of F keeps `print $fh .5` the number perl reads.

PCL vendors the branch as `vendor/PPI` (refresh: `tools/vendor-ppi CLONE pcl`).
