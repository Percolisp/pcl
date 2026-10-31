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

## s527b (2026-10-25 .. 10-31): the branch is now 57 commits, head ded727c3

`0001-*.patch` .. `0057-*.patch` and `pcl-branch.bundle` are regenerated from
the whole branch (`git format-patch master..pcl`).  The new PR groups, each
"TODO tests, then the fix", PPI's suite green after every commit:

- `PR-J.md` term-end rule (a method name, a postfix ++/--, a subscript's }, a
  builtin with no operand end a term; a { after a block's } opens the next
  block) -- §55 §33;  `PR-K.md` a { opening the document is a block -- §38;
- `PR-E3.md` / `PR-D2.md` -- the third pair of group E (~~ after a block's }),
  the second and third pairs of group D (every punctuation array/hash, @^);
- `PR-YAML.md` -- YAML::PP loaded lazily (#3924);  `PR-L.md` ->symbol of
  *$x{SLOT} -- §27;  `PR-M.md` indented here-doc indentation -- §30;
- `PR-N.md` a - after print $fh / a statement block starts a term -- §22 §35;
  `PR-O.md` a / after a statement block starts a match -- §56 (new);
- `PR-P.md` globs named by punctuation, digits, a caret letter -- §26;
- `PR-Q.md` $$$a -- §1, which IS upstream PR #428 (credited, widened to $${});
- `PR-R.md` a nameless parameter only inside a signature -- §32;
- `PR-S.md` a label only at a statement's start; declaration attributes -- §7 §8 (a second pair, s530: a TYPED declaration, `my Dog $spot : attr`).

The group letters are assigned by `tools/vendor-ppi` in branch order; s527c's
groups follow S.
