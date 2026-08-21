# Fable review of s420 (2026-08-22, s421) — verdict, findings, rulings

Opus's s420 batch (`00028e7`: #414, #413, #412, #410, +#416, #417; census
135 → 106) filed no review-request doc, so this file is the review itself.
Everything below was re-measured in this session, not read off the commit.

## 0. Verdict: s420 APPROVED as shipped

| leg | s420 claim | s421 re-measurement |
|---|---|---|
| gate, COLD (`~/.pcl-cache` cleared) | 155 files / 5614 rows, failures = the 13 pclxs xs rows | **155 / 5614**, `Wstat` lines = xs-01 (5), xs-02 (4), xs-03 (4) — exactly the 13; 226 s wall |
| full perl-tests sweep (own gate) | GATE clean, TOTAL 18363 → 18364 | **GATE clean, TOTAL 18364 (+0)**, drops 7 = census, 6 UNSTABLE = the usual above-abort-point noise (postfixderef/ref/yadayada, all rows = baselines) |
| drop census (`tools/drop-census.pl`, 8 jobs) | 33 files / 106 drops | **33 / 106, sorted-row diff vs the blessed TSV EMPTY** |
| probes vs perl 5.40.3 | ten #414 shapes, nine #416 shapes, #417 pair, #413 three shapes | 8 probe files (below): every shape the commit names matches perl; the divergences found are all PRE-EXISTING (A/B on a `47e0750` worktree) and are filed |

Per-task probe results (perl = PCL unless stated):

* **#414** — `"$x[$b[1]]"`, `"$_[$_[1]]"` (the #390 reproducer), `"$x[$i[$i[1]]]"`,
  `"$aoa[$i[1]][$i[0]]"`, `"$hh{a}{b}[$i[1]]"`, `"$x[$h{k}]$x[-1]"`,
  `"@x[$i[0],$i[1]]"`, `"$x[$i[0]+1]"`, `"$g{$ks[$i[0]]}"`, `"$r->[$i[2]]"` — all
  identical.  **#390 is the same bug and is CLOSED** (it was still `pending`).
* **#413** — `sub main::end(&)` from `package End` + `end { … }`; `Foo::g { "D" }`
  (qualified CALL of a `(&)` sub); `A::f 1, 2` with `sub A::f($)` → `A:12`;
  `\@`-proto `k @arr`; `()`-proto `q2 + 1` → 43 — identical.
* **#412 / #417** — `scalar(@{^CAPTURE})`, `$#{^CAPTURE}`, `${^CAPTURE}[1]`,
  `%{^CAPTURE}` / `%{^CAPTURE_ALL}` keys, `$#+ $#-` for `/(a)(x)?(y)?/` (3 1),
  `@+`/`@-` element-wise incl. the present undef, a failed match leaving the
  array alone — identical.
* **#416** — `s///` no-match `<>`, `+0`, defined, false; `/g` count; `/r`;
  `tr///` 0; list-context `("")`; a summed loop; `$q = ($w =~ s/a/A/g); $q*2`
  under the default `PCL_OPT` (raw-numeric takes `""` without a type error) —
  identical.
* **#410** — `$Ｘ{a}`, `$Ｖ[0]`, `"$Ｘ{a} $Ｖ[1] @Ｖ[0,1]"`, `$ｄ{k}{x}[0]`,
  `$ｒ->{a}`/`$$ｒ{a}`/`${$ｒ}{a}`, `keys %Ｘ`, `if ($Ｓ) {`, `@Ｘ{qw(a)}`,
  `&$ｆ(2)`/`&{$ｆ}(3)`, `$Ｓ ? {a=>1}->{a} : 0`, `map`/`map-hash` bodies —
  identical.  Two residues (below).

## 1. Findings from the review probes (all pre-existing; all filed)

1. **#420 (NEW, silent wrong)** — a DEREF spelling with a trailing subscript
   inside a dq string: `"$$r[1]"` → `ARRAY(0x1)[1]`, `"$$h{b}"` →
   `HASH(0x2){b}`, `"${$r}[1]"`, `"@$r[0,1]"`, `"@{$r}[1]"`, `"$$h{a}[1]"`,
   `"${$h}{a}[0]"` all leave the subscript as literal text (emission
   `(p-string-concat (p-cast-$ $r) "[1]")`).  Identical at `47e0750`.
   Corpus cost measured: perl-tests 0, lib 0, cpan-tests 2 (Carp eval text),
   perl's t/ 8 (mro/package_aliases*.t description strings) — near zero, but
   `"$$self{name}"` is a common old CPAN idiom.  Route: #388 consumer-3 port
   (the s379 standing rule is "no new scanner fixes"); acceptance set in the
   task.
2. **#421 (NEW)** — the prototype table is keyed by the BARE name, so
   `sub A::f($)` and `sub B::f(&@)` collide (last wins): `package A; print
   f 1, 2` → perl `A:12`, PCL `A:1`; in the other declaration order the
   block-form call is DROPPED.  Identical at `47e0750`; #413 made the
   convention explicit, it did not create it.  Within-one-file collisions in
   all four corpora: only op/lexsub.t (a lexsub) and comp/redef.t (same-package
   redefinition, perl-equivalent).  Filler; fix shape = (package, bare) key
   with bare fallback.
3. **#418 WIDENED** — the NFKC + `:invert` reader mechanism collides VARIABLES
   too: `our %Ｘ=(a=>1); our %X=(a=>2); our $Ｘ=5; our $X=6; print $Ｘ{a},
   $X{a}, $Ｘ, $X` → perl `1256`, PCL `2266` (`%Ｘ` and `%X` are ONE symbol;
   the backtrace prints it `|%x|`).  Fix rule stays one rule: pipe-quote any
   emitted symbol carrying a non-ASCII character.
4. **#422 (NEW umbrella)** — (1) `"@{^CAPTURE}"` / `"@{^CAPTURE}[0]"` inside a
   string DROP (the `$#` and `${…}[1]` spellings and all list forms work);
   (2) `$Ｘ {a}` (whitespace before the subscript after a repaired non-ASCII
   symbol) is read as scalar + block and crashes unbound — `_reclass_subscripts_after`
   stops at the Whitespace sibling; ASCII `$x {a}` works; (3) **PPI's LEXER
   fails the whole document** on a non-ASCII foreach loop variable
   (`for my $Ｉ (1,2) { 1 }` → "Lexer failed: Illegal state in 'foreach'
   compound statement"; `for (my $Ｉ=0;…)`, `while (my $Ｉ = …)`, `my $Ｉ = 1`
   lex fine) — logged as `ppi-upstream-bugs.md` §23 addendum + a
   `ppi-bug-report.t` row; no token-level workaround exists.  All three: 0
   corpus rows.
5. **#423 (NEW — the op/gv.t 50/47 → 49/48 mover, ATTRIBUTED)**.  s420 verified
   it PRE-EXISTING on a `47e0750` worktree and spliced it without a cause.
   Three-way probe (HEAD / `47e0750` post-s419d / `98159c7` pre-s419d):
   `$a = *main::foo; $a =~ s/^\*//; print ref(\$a), "|$a"` is `SCALAR|main::foo`
   (= perl) pre-s419d and `REF|GLOB(0x1)` from s419d on; `tr///` on a glob
   value likewise (`*MAIN::QUX` → `GLOB(0X4)`).  Cause: s419d (#119/#402) moved
   `do-regex-subst`/tr from `(to-string (unbox string-box))` to
   `(to-string string-box)` so the `""` overload + tie FETCH run — and
   `box-sv`'s typeglob branch prints every box holding a typeglob as
   `GLOB(0x…)` ("Blessed typeglob ref"), i.e. PCL's box model does not
   distinguish a glob VALUE (`$a = *FOO`, perl `*main::foo`, `ref(\$a)` GLOB)
   from a glob REF (`$a = \*FOO`, perl `GLOB(0x…)`).  The unboxed path had the
   value spelling, the box path has the ref spelling; s419d made s/// agree
   with print/interpolation (which already said `GLOB(0x1)` at all three
   trees).  A consistent answer — but a real regression for one test row, and
   a pre-existing model gap.  Task #423 carries the measurement and the two
   fix shapes; perl-tests has no gv.t, so only the companion can see it.

## 2. Rulings (standing; also in DECIDED s421)

* **"PRE-EXISTING" is a verdict about WHEN, not WHY.**  A companion mover
  verified pre-existing on a base worktree is spliced into
  `docs/perl-suite-run.tsv` only WITH its cause (a task or a commit), or with
  the words "cause unknown, measurement: …" naming the next cheap step.  s420's
  op/gv.t splice had neither; the cause was one three-way probe away and it is
  a regression from the session before.
* **A `cl/**` change to a COERCION or STRINGIFICATION path runs the op/
  companion leg before it ships** — the CLAUDE.md table already says "the dirs
  the change touches"; s419d read that as "re/ for a regex change" and op/ is
  where the glob rows live.  The sweep cannot stand in: perl-tests has no gv.t.
* **s420's bare-name prototype keying stands** (perl applies the prototype on a
  qualified call; the collision is pre-existing and measured rare → #421).
* **The s420 splice of every other companion mover is ACCEPTED** (each was
  re-run on the base worktree; the verdicts are consistent with the sessions
  named for them).

## 3. Where the probes live

`/tmp/claude-1000/-home-bernt-pcl/5ff320ed-4793-4a3a-9135-43cc25707814/scratchpad/probes/`
(p1–p8; each task's reproducer is copied into its task text, which is the
durable copy).  The plan that follows from this review is
`docs/plan-post-s420.md`.
