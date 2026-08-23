# `Pl/t/shapes/` — the SHAPES corpus (task #496, s440)

Deliberately-awkward Perl files that exist only to exercise grammar **no real
population contains**.  Five changes in a row (#453, #365, #454, #455, #435)
were emission-identical over all four measured populations (corpus-diff 111
files, lib 22, cpan 402, perl t/ 604) and were guarded only by rows plus an
inverse run on a worktree — the s371 rule working as intended, and also five
real bugs living in shapes no population had.  This directory makes those
shapes a *population*:

* `tools/corpus-diff.pl` transpiles every `*.pl` here with both compilers
  after the corpus verdict and prints **its own line** (`shapes: N files
  identical` / `SHAPES: k of N differ ...`), never mixed into the 111-file
  count; a shapes diff fails the run like a corpus diff (a file subset on the
  command line skips the shapes — a subset run is a probe).
* `tools/emission-ab.pl --shapes` adds them to any A/B.

**What it is not:** a second test suite.  An A/B asserts only "unchanged"; a
*row* (in `Pl/t/*.t`) asserts perl's answer.  The files here are lifted from
guard files and must not duplicate their rows.

**Admission rule:** a shape that moved a fix in some session and occurs in no
population.  One file per family, one block-scoped `package SNN;` per lifted
snippet (the packages keep same-named subs apart; they also exposed #497 and
#498 on the corpus's first run — which is the corpus doing its job).  Every
file is valid perl; run it from the repository root (`imported-term.pl` uses
`use lib "Pl/t/shapes/lib"`, a literal path relative to the compiler's tree,
which is how both tools invoke `pl2cl`).

| file | family | seed |
|---|---|---|
| `operand-grammar.pl` | a user sub whose prototype makes it a named unary (#453) | `Pl/t/user-unary-01.t` |
| `sub-heads.pl` | signatures vs file lexicals, the feature region's own line, bundles (#454, #455) | `Pl/t/sig-param-shadow-01.t` |
| `imported-term.pl` | an imported `()`-prototype sub is a TERM (#365), `@EXPORT` from a variable | `Pl/t/imported-term-01.t` (+ `lib/T438/Konst.pm`) |
| `punct-arrays-glob.pl` | punctuation arrays (#415, #451), `<~>`, the glob word model (#450) | `Pl/t/punct-array-glob-01.t` |
| `bareword-handles.pl` | package-qualified bareword handles (#452), punctuation-array interpolation | `Pl/t/punct-array-glob-01.t` |
| `interpolation-nonascii.pl` | non-ASCII identifiers and their ASCII twins (#418, #422.2, #435) | `Pl/t/utf8-source-01.t` |

Known divergences the corpus currently carries on purpose: none at the moment.
#497 (`sub-heads.pl`: the same signature sub name in two packages) and #498
(`punct-arrays-glob.pl`: `@?` written in one package, read in another) were
both found here and fixed the same session (s440c, s440d).
