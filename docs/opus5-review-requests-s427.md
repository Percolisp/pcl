# Review requests — session 427 (Opus 5, 2026-08-22)

Agent F of the s425 fan-out: **session O3, the fillers** of
`docs/plan-post-s420.md` §1 item 5 — **#442, #422.2, #421, #415**.  Four
commits, one per item, each with its own probe table against perl 5.40.3 and
its own guard rows.  Generation **v2-173**; branch
`worktree-agent-a1ef8cda8527aca2b`, rebased onto `dbef93c` (main after s425c, i.e. after both sibling
branches merged),
fast-forward mergeable.

§1–§4 are the items in commit order; §5 the whole-batch measurements; §6 what
was FILED and not fixed; §7 what was skipped and why; §8 the asks.

Two of the four turned out to be **one-member fixes of a family with a latent
crash next door**, which is the shape the s366 filler rule is meant to find:
`@#` had a forward declaration and `@?` did not (so `$?[1]` killed a file with
no PPI bug involved at all), and the glob-metacharacter blacklist hid `<~>`,
`<foo.txt>` and `<<>>` behind one wrong test.

---

## §1  #442 — `chr(N)` above U+10FFFF kept two answers, chosen by an optimizer

**Commit `416fa09`.**  The task said: probe BOTH paths against the s318 §11
ruling, then make ONE place decide.

### The measurement (nine shapes, three trees)

| | perl | PCL default | PCL_OPT=none |
|---|---|---|---|
| `ord(chr(0x4000000))` | 67108864 | 67108864 | 67108864 |
| `length(chr(0x4000000))` | 1 | 1 | 1 |
| `my $c = chr(0x4000000); ord($c)` | 67108864 | **65533** | 67108864 |
| `length($c)` | 1 | 1 | 1 |
| `my $d = chr(0x110000); ord($d)` | 1114112 | **65533** | 1114112 |
| `my @a = (chr(0x4000000)); ord($a[0])` | 67108864 | 67108864 | 67108864 |
| `my %h = (k => chr(…)); ord($h{k})` | 67108864 | 67108864 | 67108864 |
| `ord(chr(-1))` | 65533 | 65533 | 65533 |
| `ord(chr(0x10FFFF))` | 1114111 | 1114111 | 1114111 |

**The general-form compiler was already right on all nine.**  Only the DEFAULT
emission diverged, and only where the scalar took the `raw-string` slot
verdict.  So the bug belongs to the optimization, not to the ruling — and it is
exactly what the optimization registry forbids: *`PCL_OPT=none` is the
general-form compiler; its emission must RUN identically*.

### The fix, and where "one place" is

`%pcl-to-string-strict` — the raw slot's eager freeze — called `to-string` on
the `p-superchar` payload, pre-empting a decision the general path makes later.
It now passes a superchar through (`%pcl-superchar-payload`), so the U+FFFD
collapse happens at `to-string`, which is where the boxed path makes it.  Every
consumer of a raw-string slot goes through a `p-*` runtime function that
coerces, so nothing else moves.

The predicate looks through a plain box but deliberately **not** through
`unbox`'s tie/magic dispatch: a tie proxy is never a superchar, and running
FETCH there would run it twice for one write.

**The first cut missed the box entirely** and `my $d = $c` still answered
65533.  Found by the 15-shape probe below, not by the reproducer in the task.

### The 15-shape probe (after the fix)

`length`, `ord`, interpolation, `uc`, `eq` self, `eq "\x{FFFD}"`, hash key,
concat length, `substr`, boolean, `index`, **copy** (`my $d = $c`), `.=`
append, `split //`, `sprintf "%s"` — **the two emissions are IDENTICAL on all
15**, and match perl on every row where the character itself is not consumed.
The four that still differ from perl (interpolation, `uc`, `substr`, `eq
"\x{FFFD}"`) are the blessed U+FFFD gap and are now identical in both regimes.

`docs/not-supported.md` corrected: s422 recorded "once the value has been
assigned it is 65533" — that was the bug, not the rule.  Guard
`Pl/t/wide-codepoint-01.t` rows 8–11 (11 rows, 10 s), including the
both-emissions row that IS the property.

---

## §2  #422.2 — a space before a repaired non-ASCII symbol's subscript

**Commit `11b1715`.**  `_reclass_subscripts_after` (the #410 repair) walked
with `next_sibling`, so ONE whitespace token stopped it and the lexer's
bareword-derived guess stood.  The cost is not a drop, it is SILENT WRONG:

```
print $Ｘ {a}   →  (p-print :fh |$x| "a")     a block-form FILEHANDLE spec
print $Ｖ [1]   →  (p-print :fh |$v| (make-p-box (p-array-init 1)))
```

`snext_sibling` is the fix, and the reason it is the right one is that **PPI's
own lexer steps over whitespace there** — the ASCII `$h {a}` and
`@h {qw(a b)}` dump as `PPI::Structure::Subscript` under 1.291.  This pass only
ever re-decides what PPI decided from a bareword it should never have seen, so
mirroring PPI is the whole specification.

Probed identical to perl after: `$Ｘ {a}`, `$Ｖ [1]`, `@Ｘ {qw(a b)}`,
`$ｒ ->{a}`; and the shapes it must NOT touch — `if ($Ｖ [0]) { … }`,
`map { … } @Ｖ`, `foreach (@Ｖ) { }`, a bare block, an anon sub, a `sort { }`
comparator, an anonymous hash in a list, a non-ASCII sub definition.  The one
shape that cannot be probed is `for my $Ｉ (…) { }`: PPI's LEXER fails the whole
document (#422 item 3, already logged as §23's addendum) — unreachable from a
token repair.

Guard `Pl/t/utf8-source-01.t` row 8, beside the #410 row it extends.

---

## §3  #421 — a prototype belongs to the package that DECLARES the sub

**Commit `8d7abe8`.**  Both halves probed vs perl and A/B'd on a `753ecab`
worktree:

| | perl | before | after |
|---|---|---|---|
| `{A: sub f($)} {B: sub f(&@)}` `package A; print f 1, 2` | `A:12` | **`A:1`** (silent wrong) | `A:12` |
| `{C: sub h(&@)} {D: sub h($)}` `print C::h { "y" } 7, 8` | `C:y:7 8` | **DROPPED** | `C:y:7 8` |

perl's rule is the sub it RESOLVES at compile time, so the fix is at the
`Pl::Environment` seam where #413 already normalizes qualified names in both
directions: a second table `pkg_prototypes` (`{bare => {pkg => info}}`), filled
from the declaring package — the qualifier when the name carries one, else the
package the pre-scan already computes for `add_declared_sub`, else the current
package.

**The lookup is deliberately narrow:** the per-package table is consulted ONLY
when a bare name has more than one declaring package.  With 0 or 1 the flat
table IS that entry, so every non-colliding program takes exactly the path it
took before — which is why corpus-diff is IDENTICAL and the lib A/B is
SAME=22.  The flat table stays the fallback when the current package has no
declaration of its own, because that is how an IMPORTED prototype reaches a
call site.  Both tables travel through `merge()` and `clone()`.

Eight-shape probe vs perl, all identical: imported `first { } @n`, a qualified
call from a package that has its own `f`, unqualified resolution in the current
package, a qualified DECLARATION (#413's shape), same-package redefinition
(last wins), a third-package call, an unrelated two-arg prototype.  The only
diff in the whole run is perl's two compile-time warnings ("Prototype
mismatch", "Subroutine redefined"), which PCL does not emit (#221).

Guard `Pl/t/prototype-01.t`, 9 rows beside #413's — including the inverse
guard that a single-package prototype still reaches an outside call site by
BOTH spellings, and a table-shape row that both declaring packages are kept.

---

## §4  #415 — `@?` is a variable and `<~>` is a glob

**Commit `6c6b3f8`.**  Two of the nine census singles, the two the task had
already measured.  Both were one-member fixes of a family.

### `@?` (t/re/subst.t:346)

perl lets any punctuation character name a global — probed on 5.40.3, every
one except `@+`/`@-` (read-only) and `@{` (a syntax error).  PPI has Magic
tokens only for the arrays perl documents, so `@?` lexes as `Cast('@') +
Operator('?')` and the statement DROPPED.  The merge is unambiguous: in valid
perl a `@` cast is only ever followed by `$`, `{` or an identifier, so a cast
abutting an operator is a parse of no legal program.  Logged as
`docs/ppi-upstream-bugs.md` §24 with two failing rows in `docs/ppi-bug-report.t`
(now 29 rows; both new ones fail on 1.291, which is what a bug row must do).

**The other half needed no PPI bug at all.**  `$?[1]` is element 1 of `@?` and
has always lowered to `(p-aref @? 1)` through the machinery that serves
`$#[0]`, but only `@#` was ever forward-declared — so a file containing
`$?[…]` died at LOAD with an unbound variable, a crash nothing in the census
could see.  Parser2's punctuation bucket declares the whole family now.

**The boundary is the CL SYMBOL SPELLING, not perl's grammar:**
`? ! . / ~ ^ & % = < >` are CL constituents, so `@?` reads bare exactly as the
synthesized `@#` does.  `@,` `@;` `@|` `@'` `@"` `@(` are CL macro or escape
characters and need a pipe-quoted symbol — #418's territory — so they keep
dropping loudly and are filed as **#449**.

### `<~>` (t/op/glob.t:110)

perlop states the readline rule as a WHITELIST: *"If what's within the angle
brackets is neither a filehandle nor a simple scalar variable containing a
filehandle name, typeglob, or typeglob reference, it is interpreted as a
filename pattern to be globbed."*  PCL had the inverse — a blacklist of glob
metacharacters — so `<~>` emitted `(p-readline ~)`, an unbound CL symbol that
killed the whole file, and so did `<foo.txt>`.

Three things the measurements found that the rule alone does not give you:

1. **corpus-diff caught a regression the probe table could not.**  Parser2's
   rename passes rewrite the symbol INSIDE the token, so perl-tests/scalar.t's
   `<$fh>` arrives as `<$main::fh__file__0>` once `$fh` is promoted.  The
   scalar test has to allow a package qualifier; perl reads `<$main::fh>` as a
   readline too (probed).  My first cut turned that one statement into a glob.
2. After a list-operator Word, PPI lexes `<~>` as three operators (§14), and
   `_fix_ppi_glob_after_block` did not rebuild it because `~` was not in its
   glob-metacharacter class.  It IS one (bsd_glob expands a leading tilde) and
   it is LESS ambiguous than the `*` already in that class.
3. **The COMPANION A/B caught `<<>>`** — perl 5.22's double diamond, which PPI
   hands over as a Readline token whose inner text is `<>`.  The old rule
   crashed on an unbound `<>` symbol (io/argv.t's own failure note in the
   snapshot), and a naive whitelist would have globbed the string "<>" and
   returned nothing — trading a crash for a SILENT wrong.  It is named
   explicitly and lowers like `<>`: io/argv.t **23/30 → 27/26**.

Finally `p-glob` expands the leading tilde the way bsd_glob does (`~`,
`~/rest`, `~user`; an unknown user and a non-leading tilde are left alone,
which is perl's answer), or the row would run and still fail.

~40 probe shapes vs perl, including every inverse guard the change is one
character away from: `@$r`, `@{$r}`, `@{[…]}`, a ternary after an array, `%`
modulus; `$a < ~$b`, `~0`, `f() < 9`, `<=>`; `<$fh>`, `<FH>`, a `while` loop,
`<DATA>`.

4. **A fourth finding came from the REBASE**, after s423/#418 merged: a handle
   NAME is a perl IDENTIFIER, and under `use utf8` that means UNICODE word
   characters.  The whitelist's first cut was `[A-Za-z_]\w*`, so `<ＦＨ>`
   read as a filename pattern and globbed SILENTLY — and #418's own
   bareword-filehandle guard row is what caught it.  Both spellings are
   `[^\W\d]\w*` now, and the discriminating pair is guarded here: `<ＦＨ>`
   is a readline, `<Ｆ.txt>` is a pattern, same letters.

Guard `Pl/t/punct-array-glob-01.t` (13 rows, 13 s — new file:
`misc-fixes-02.t`, the usual home for a PPI workaround, measures **362 s**
under plain prove and CLAUDE.md §6 says not to grow it).

---

## §5  Measurements (whole batch)

**Gate (COLD, `tools/prove-core`, `PCLXS_DIR` exported): 157 files / 5661
rows.**  Failures = the 13 pclxs xs rows and nothing else (xs-01 5, xs-02 4,
xs-03 4).  Arithmetic from main's 156/5639: +1 file
(`punct-array-glob-01.t`), +22 rows (3 wide-codepoint, 1 utf8-source, 9
prototype, 9→11 punct-array-glob).

**Full perl-tests sweep (`--jobs 3`): GATE clean.**

```
TOTAL: 18365 passing, 906 failing across 108 files
TOTAL dropped statements: census 7, current 7 (+0)
TOTAL passing: baseline 18365, current 18365 (+0)
summary: 0 new, 0 fixed, 7 unstable (crash-file noise), 10 unverified
GATE: clean
```

Neither baseline was edited: nothing in `perl-tests/` moved.

**corpus-diff: IDENTICAL across 111 files, silent drops 7 unchanged** — run
after each of the four commits (it is what caught the scalar.t regression in
§4).  **`emission-ab --ref 753ecab` over the 22 lib shims: SAME=22, DIFF=0**,
also after each commit.

**Gate-SET scan, BOTH populations, 638 files × 2, vs a 753ecab worktree** —
mandatory here because three of the four items widen a classifier (the
prototype lookup, the punctuation repair, the readline/glob rule).  The diff is
**exactly two lines**:

```
t/op/glob.t   drop "ok <~>, '~ works';"           ->  OK
t/re/subst.t  drop "ok( ! @?, '…' );"             ->  (its other drop, unchanged)
```

**Drop census 33 files / 106 drops → 32 / 104**, edited row by row with the
cause (`t/op/glob.t` row REMOVED, `t/re/subst.t` 3 → 2); header total
recomputed.

**Companion leg — 18 files** covering every dir the batch touches (`op/ re/
io/ comp/ uni/ mro/`), run A/B against the base worktree.  The A/B diff is
three rows:

| file | base | s427 | cause |
|---|---|---|---|
| `op/glob.t` | 13/4, 1 drop | **14/4, 0 drops** | #415, `<~>` |
| `io/argv.t` | 23/30, note `unbound:<>` | **27/26**, no note | #415, `<<>>` |
| `re/subst.t` | 0/0, 3 drops | 0/0, **2 drops** | #415, `@?` (the file crashes before TAP for an unrelated `p-typeglob` reason, on both trees) |

Everything else byte-identical, `op/chr.t` included (its 38/7 XDIFF is the
blessed `use bytes` + >U+10FFFF pair and #442 does not touch either: those
rows are `chr` in a LITERAL comparison, not an assigned `ord`).

`docs/perl-suite-run.tsv` spliced with causes.  A third row moved and is
**PRE-EXISTING**: `uni/gv.t` 41/40 → 53/28 — the BASE worktree measures 53/28
too, so the snapshot row is stale.  Recorded as such with the next measurement
named (an A/B across the s422/#419 and s424/#423 merges, the two sessions
between that row's blessing and 753ecab).

Generation **v2-173**; all three artifacts regenerated and byte-identical
apart from the gen stamp, as a batch with an IDENTICAL corpus-diff must be.

**Generation: v2-173, not the assigned v2-171.**  Agent B (#418) set v2-171 in
its own tree and ran its measurements under it; since `~/.pcl-cache` is shared
and keyed by that string, a module transpile cached under v2-171 could have
come from either compiler.  On Fable's instruction this branch moved to a
fresh string and RE-RAN both the cold gate and the full sweep under it (the
numbers in this section are the v2-173 runs; the identical v2-171 runs are in
the session log).  The three artifacts were regenerated at v2-173 and their
bodies are byte-identical to the v2-171 build — only the stamp moved.

---

## §6  Found and FILED, not fixed

* **#449** — punctuation-named globals whose CL symbol spelling needs pipe
  quoting (`@,` `@;` `@|` `@'` `@"` `@(`) still DROP, and the `%X` hash family
  is untouched (PPI does not even give a Cast there: `%?` is
  `Operator('%') + Operator('?')`, so the "a cast cannot abut an operator"
  licence does not apply).  0 corpus rows; loud, not silent.  Should be taken
  WITH #418, since it is the same pipe-quoting rule.
* **#450** — `glob(PATTERN)` with no wildcard returns EMPTY where perl returns
  the pattern itself (`glob("/nope-xyz")`, `glob("/home/")`, `glob("x~y")`,
  `glob("~nosuchuser")`).  PRE-EXISTING: byte-identical on the base worktree,
  so it is not the tilde expansion.  A `cl/` fix, so the full sweep is its bar.

---

## §7  Skipped, with what was measured

The other five #415 singles, each probed:

* **op/utftaint.t:18** — does not reproduce isolated (the task already said so;
  re-confirmed).  The trigger is contextual and unlocated; locating it is a
  hunt, not a filler.
* **op/filetest.t:161** (`sub _ { … }` then `-f _`) — #403-family, and my
  instructions said to take #403 only if a row depended on it.  No row here
  does.
* **op/utf8cache.t:70** (`substr $x, 0, 1, = "\x{100}"`) and
  **op/sub_lval.t:1065** (`{; @119797 }`) — both still DROP loudly.  They are
  term-grammar shapes that lead into `_reduce_term`/the `$end_pars` region,
  which is B3's and which `docs/pexpr-term-parsing-review.md` forbids adding
  guards to.  Worth recording: `{; @119797 }` swallows the FOLLOWING statement
  into its drop (`{; @119797 } print "B\n";` is one dropped statement), so its
  real cost is larger than one row.
* **op/sub_lval.t:970** (`continue { … }` in an rvalue-return context) — the
  lvalue file's torture row; the file is 33 lvalue-exempt drops already.

**#403 itself** was not taken (no row in this batch depends on it).

---

## §8  Asks

1. **#442's answer moved PCL TOWARD perl, and that is an expectation rewrite
   of a row that named the divergence.**  `Pl/t/wide-codepoint-01.t`'s old row
   6 asserted `1:65533` for `my $s = chr(0x4000000); length($s), ord($s)`, with
   a comment saying the divergence was the blessed ruling and that
   `not-supported.md` must change in the same commit if the row ever did.  It
   now asserts the perl oracle (67108864).  The four s376 conjuncts are met
   (perl-probed text; the diff is exactly the divergence; the edit
   STRENGTHENS — a fixed expectation became an oracle comparison; guard rows in
   the same commit).  **Confirming that reading is the ask** — specifically
   that "the blessed answer is U+FFFD" is about the CHARACTER, and the box
   keeping its NUMBER through assignment is not a re-litigation of s318 §11.
   `not-supported.md` now says exactly that.

2. **#421's narrowness — is "consulted only on a collision" the right shape, or
   should the per-package table be authoritative always?**  I chose the narrow
   form deliberately: it makes the change provably inert everywhere the corpus
   can see (0 collisions in four populations), and it keeps the import path on
   the flat table where it has always been.  The cost is that a call site in a
   package that has NO declaration of its own still gets the last-registered
   entry when two others collide — perl would die "Undefined subroutine" there,
   so no working program can tell.  If the queue ever wants the strict form,
   the seam is one line in `_proto_entry`.

3. **Not an ask, a note for whoever merges:** §4's `<<>>` case was found by the
   COMPANION A/B and by nothing else — the probe table, corpus-diff, the
   gate-SET scan and the sweep were all silent about it, because the file's row
   counts were the only place it showed.  The s421 ruling that a `cl/`
   coercion change runs the companion leg earned its keep here on a `Pl/`
   change: the leg is worth running for any rule that reclassifies a TOKEN
   family, not only for coercions.

---

## §9  RUN LEDGER — exactly what was run, on which tree, under which generation

Written at the session's forced end.  **All four items are DONE, committed and
guarded; the branch is fast-forward mergeable onto `dbef93c`.**  What is
incomplete is the FINAL re-verification pass on the final tree — read this
table before trusting any number above.

| run | tree | generation | result |
|---|---|---|---|
| cold gate | rebased onto `463a8f8` (after s426) | **v2-171** | 157 files / **5665 rows**, failures = the 13 pclxs xs rows only |
| full sweep `--jobs 3` | same | **v2-171** | GATE clean, TOTAL 18365 (+0), drops 7 = census, 0 new / 0 fixed |
| cold gate | same tree | **v2-173** | 157 / 5665, same 13 xs rows — the collision re-run Fable asked for |
| full sweep `--jobs 3` | same tree | **v2-173** | GATE clean, TOTAL 18365 (+0), drops 7 = census — identical to the v2-171 run bucket for bucket |
| cold gate | **final tree** (rebased onto `dbef93c`, after s423/#418) | v2-173 | **157 files / 5684 rows**, failures = the 13 pclxs xs rows only |
| corpus-diff | final tree (rebased onto `dbef93c`, after s423/#418) | v2-173 | **IDENTICAL across 111 files**, silent drops 7 unchanged |
| gate-SET scan, both populations (638 × 2) | final tree vs a `dbef93c` worktree | v2-173 | **exactly two verdicts move**: t/op/glob.t drop → OK, t/re/subst.t loses its `@?` drop |
| companion leg, 18 files | final tree vs a `dbef93c` worktree | v2-173 | **exactly three rows move**: op/glob.t 13/4 → 14/4, io/argv.t 23/30 → 27/26, re/subst.t drops 3 → 2 |
| `emission-ab` over the 22 lib shims | after every commit | both | SAME=22, DIFF=0 |
| the three artifacts | final tree | v2-173 | regenerated; **bodies byte-identical**, only the stamp moved |
| guard files, run directly on the final tree | final tree | v2-173 | `punct-array-glob-01.t` 13/13, `utf8-source-01.t` 25/25, `wide-codepoint-01.t` 11/11, `prototype-01.t` 137/137 |

**THE ONE THING NOT RUN: the full perl-tests sweep on the FINAL tree** (the
one rebased onto `dbef93c`).  The cold gate on that tree DID complete — 157 /
5684, only the 13 xs rows — and the sweep was next when the session ended.
What changed between the swept tree and the final one is the rebase onto
s423/#418 plus ONE code line: the readline/glob whitelist widened from
`[A-Za-z_]\w*` to `[^\W\d]\w*`, with its two guard rows.  That line was
measured on the final tree by everything short of a full run — corpus-diff
IDENTICAL over 111 files, the gate-SET scan over both populations showing only
the two intended verdicts, the 18-file companion A/B showing only the three
intended rows, and the four guard files green including #418's own
bareword-filehandle row, which is what motivated the widening.  **A reviewer
should run `perl sweep-perl-tests.pl --jobs 3` on the merged tree before
blessing the numbers**; expected GATE clean, TOTAL 18365 (+0), drops 7.

Raw logs of every run above are in the worktree's `.s427/` directory
(`gate*.log`, `sweep*.log`, `comp-*.tsv`, `gs-*.tsv`), which is untracked on
purpose — it is evidence, not repo content.
