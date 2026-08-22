# Review requests — session 426 (Opus 5, 2026-08-22)

One item: **`docs/plan-post-s420.md` §1 item 4 — #388 consumer 3**
(`Pl/PExpr/StringInterpolation.pm` onto `Pl::InterpScan`'s `scan_one`
events) with **#420**, **#422 item 1** and the **#390/#414 shape** as the
acceptance set.  One commit.

The s366 escape hatch (a local #420 fix reusing the `$name[` continuation)
was **not** taken: the port landed, and the port IS the fix.

§1 is what the port is; §2 the second half nobody knew was there (the
emitter); §3 the measurements; §4 the two pre-existing bugs filed and not
fixed; §5 the asks.

---

## §1  The port — one dispatcher, three lowering arms

`parse_interpolated_variable` was 334 lines of hand-rolled scanning: `\G`
name grabs with their own `::` continuation loop, a `$#` branch with three
sub-branches, a `$::` branch, a punctuation-magic branch, an `@$`/`@{`/`@-`
branch, an arrow-chain walker, and `_parse_subscript_chain`'s group counter
— feeding four builders (`parse_braced_expression`,
`parse_array_braced_interpolation`, `parse_array_subscript`,
`parse_hash_subscript`) that between them carried the six-line
brace-depth walk **nine times** and one exact 36-line duplicate.  That is
the #1 family of `docs/dup-census-worklist-s411.md` and the standing rule
`docs/var-handling-review-s379.md` §8's whole target.

It is now:

```perl
my $ev = Pl::InterpScan::scan_one($content, $pos);
return (undef, $pos) unless $ev;
... dispatch on ($ev->{sigil}, $ev->{form}, $ev->{chain}) ...
```

**1216 → 664 lines.**  What stayed is exactly what `docs/interp-scan.md`
step 3 said would stay: the node BUILDING and the case-mod/literal outer
loop in `parse_interpolated_string`.

The lowering is three arms, deliberately not a fourth table:

1. **Leaf tokens** — `$name` / `${name}` → Symbol, `$1` / `$$` → Symbol,
   `$!` / `$^X` / `${1}` → Magic, `$#name` / `$#{name}` / `$#-` →
   ArrayIndex, `@name` → Symbol, `@{name}` / `@-` / `@$ref` →
   `array_str_interp`.  *Which PPI token* is the only thing left to decide
   here, and each choice reproduces what the deleted branch produced.
2. **The element/slice accessor** — exactly one bracket group, no arrow, on
   a NAME: `a_acc` / `h_acc` / `slice_a_acc` / `slice_h_acc`, one builder
   for all four (`_interp_element`) instead of two 130-line functions.
3. **The reference's own SOURCE TEXT through the ordinary expression
   pipeline** — every deref base, every braced EXPRESSION, a second group,
   an explicit arrow.  This is the move `_parse_postfix_deref` and consumer
   1's `_compile_ref_text_form` already make, and it is **the fix**: the old
   scanner had no way to continue past a deref, so `"$$r[1]"` emitted
   `(p-string-concat (p-cast-$ $r) "[1]")` and printed `ARRAY(0x…)[1]`.

Two scanner facts were **probed on perl 5.40.3 and added to
`Pl/InterpScan.pm`** while wiring, because the scanner follows perl:

* **A braced EXPRESSION does not close the reference; a braced NAME does.**
  The module said "braces CLOSE the reference" flatly, probed on the NAME
  form only.  Measured: `"${x}[0]"` is `$x` then literal (NAME, closes);
  `"${$r}[1]"` is 20, `"@{$r}[1]"` is 20, `"@{$hr}{'a','b'}"` is a hash
  slice, `"@{[1,2]}[0]"` is 1, `"${ $r }[1]"` is 20 with blanks, and
  `"${\ $x}[0]"` dies *"Not an ARRAY reference"* — i.e. it **took** the
  group rather than leaving it literal.  `"${$rr}->[1]"` binds the explicit
  arrow too.  So `_scan_braced_dollar`'s and `_scan_snail`'s expression arms
  now call `_scan_chain` (the `@` side with `$max = 1`: a bracket after a
  slice is a perl compile error).
* **`@{^NAME}` is the magic ARRAY of that name** — the symmetric twin of the
  `${^NAME}` arm the module already had.  Without it the whole reference
  fell to form `expr` and the consumer tried to parse `^CAPTURE` as an
  expression: `"@{^CAPTURE}"` was a whole-statement **DROP** ("Bug. Fell
  through. Missing case: []").  That is #422 item 1.  Probed: like every
  braced NAME it closes — `"@{^CAPTURE}[0]"` prints `a b[0]`.

and two arms the consumer had but the scanner did not: **`$#-` / `$#+`**
(#417 shipped them in `StringInterpolation` only, so the port would have lost
them) and **a braced PUNCTUATION name** — `@{+}` is `@+`, the rule
`Pl::PExpr::braced_punct_magic_name` states and the token pre-pass
`_fold_braced_punct_magic` applies to the same spelling in code.  Caret and
punctuation are one predicate now (`_braced_magic_name`), asked by both
sigils.

**The Pl/t gate is what found that last one, and it is worth recording how.**
The first full gate run failed `Pl/t/match-vars-01.t` on the #355
stderr-aware helper's row — *"transpile DROPPED a statement"* — because
`"@{+}"` had no scanner arm and `+` went to the expression path to be parsed
alone.  That is the exact shape #314 cost `re/pat_rt_report.t` its whole
2513 rows.  Probed after the fix: braces close for punctuation too
(`"@{+}[1]"` is `4 3 4[1]`, `"${+}[1]"` is `b[1]`, `"%{+}"` unchanged,
`qr/A@{+}B/` still builds).  Its reach was then measured directly: **5 files
in the four populations carry a `${X}` / `@{X}` punctuation body, and all 5
are byte-identical to main** — the only behaviour it changes is the dq-string
spelling, which no corpus file uses.

**Divergences 3, 4, 5, 6 and 7 of `docs/interp-scan.md`'s table are closed
by the port** — `$$r[0]`/`@$r[0]`, `@-[0]` slices, `${ x }` with blanks,
`$12abc` (perl reads `${12} . "abc"`), `$::a::b` (perl reads the full
name).  Each has a probe row in §3.

---

## §2  The half the task did not know about: a slice through a reference
##     was silent-wrong in ORDINARY CODE

With the subscript finally reaching the parser, `"@$r[0,1]"` compiled to
`(p-join |$"| (p-cast-@ (p-aslice $r 0 1)))` — correct emission — and
printed **nothing**.  The same expression in plain code printed nothing too:

```perl
my @s = @$r[0,1];        # PCL: ""    perl: "10 20"
```

**A slice whose CONTAINER is a scalar is a slice through a reference**, and
all four slice emitters handed the raw box to the slice runtime, which then
indexed the REFERENCE.  The element sibling has always dereferenced
(`$$r[1]` lowers to `p-aref-deref`); the slice family never did.  What it
cost depended on how many box layers the ref carried, which is why it looked
like it worked — probed vs perl 5.40.3, 18 rows:

| shape | PCL before | perl |
|---|---|---|
| `my $ao=[7,8,9]; @$ao[1]` | `8` | `8` — right, by luck (one box layer) |
| `my $r=\@named; @$r[0,1]` | `""` | `10 20` — **SILENT WRONG** |
| `@{$r}[0,1]` | `""` | `10 20` — **SILENT WRONG** |
| `my $hr=\%named; @$hr{'a','b'}` | DIED | `HA HB` |
| `@{$hr}{'a','b'}` | DIED | `HA HB` |
| `%$hr{'a'}` | DIED | `a=HA` |
| `@$r[0,1] = (1,2)` | no-op | assigns — **SILENT WRONG** |
| `@$hr{'c','d'} = (3,4)` | DIED | assigns |
| `delete @$hr{'a'}` | DIED | `HA` |

Fix: **one helper**, `_slice_container_form` (`Pl/ExprToCL.pm`), used by
`gen_array_slice_form`, `gen_hash_slice_form`, `gen_kv_hash_slice_form`,
`gen_kv_array_slice_form` and `_elem_container_key`'s slice arm.  It casts
the container with `p-cast-@` / `p-cast-%` unless the child node is a bare
`Symbol`/`Magic` naming the aggregate (`@a`, `%h`, `@-`, `@{name}`) — a
scalar in container position can only be a reference.  `p-cast-@` /
`p-cast-%` peel whichever layering is there and are the identity on a
container that is already the aggregate, and they carry the `#163` referent
check, so a wrong-kind referent dies as perl does.

It **replaced** `gen_kv_array_slice_form`'s `my $arr_form = $arr =~ /^\$/ ?
['unbox', $arr] : $arr` — the same rule written shape-blind: right for a
single-boxed anon ref, one box layer short of `\@named`.  That is the
"second copy" the rule-11 smell test names, and it is now one.

All 18 rows match perl.

---

## §3  Measurements

### The ten-shape probe (56 rows), perl 5.40.3 vs PCL

`probe-interp.pl`: the five #420 spellings and their compounds,
`"@{^CAPTURE}"`, the #390/#414 nested-subscript set, the chain/postfix
shapes, and 25 inverse guards.  **Before: 15 divergences (2 of them
whole-statement DROPS, 1 a run-time CRASH).  After: 0 of 56.**

| # | shape | perl | PCL before | PCL after |
|---|---|---|---|---|
| 01 | `"$$r[1]"` | `20` | `ARRAY(0xA)[1]` | `20` |
| 02 | `"x $$r[1] y"` | `x 20 y` | `x ARRAY(0xA)[1] y` | ✓ |
| 03 | `"$$hr{b}"` | `9` | `HASH(0xA){b}` | ✓ |
| 04 | `"${$r}[1]"` | `20` | `ARRAY(0xA)[1]` | ✓ |
| 05 | `"@$r[0,1]"` | `10 20` | `10 20 30[0,1]` | ✓ |
| 06 | `"@{$r}[1]"` | `20` | `10 20 30[1]` | ✓ |
| 07 | `"$$hr{a}[1]"` | `6` | `HASH(0xA){a}[1]` | ✓ |
| 08 | `"${$hr}{a}[0]"` | `5` | `HASH(0xA){a}[0]` | ✓ |
| 09 | `"$$r[$i[1]]"` | `20` | `ARRAY(0xA)[1]` | ✓ |
| 10 | `"@{$hr}{'a','b'}"` | `ARRAY(0xA) 9` | **CRASH** (`Not an ARRAY reference`, load aborted) | ✓ |
| 11 | `"$$r[1]$$hr{b}"` | `209` | `ARRAY(0xA)[1]HASH(0xA){b}` | ✓ |
| 12 | `"@{^CAPTURE}"` | `a b` | **DROP** | ✓ |
| 13 | `"@{^CAPTURE}[0]"` | `a b[0]` | **DROP** | ✓ |
| 38 | `"[@$r[9]]"` | `[]` | `[10 20 30[9]]` | ✓ |
| 53 | `"@-[1] @+[1]"` | `1 3` | `@-[1] @+[1]` | ✓ |

Unchanged and re-verified (the rows that could have broken): 14–37 and
39–56 — `"${^CAPTURE}[1]"`, `"$#{^CAPTURE}"`, the whole #390/#414 nested
set (`"$x[$i[0]]"`, `"$x[$h{k}]"`, `"$x[!$i[0]]"`, `"$_[$_[1]]"` in a sub,
`"@ops[$i[0],@{$cur[0]},2]"`), `"$h2{a}{b}[1]"`, `"$hr->{a}[0]"`,
`"@{[ 2*3 ]}"`, `"${\ (2*3) }"`, `"$obj->method"` (not a call),
`"${name}text"`, `"$name {a}"` vs `"$name{a}"`, `"a@ b"`, `"a@.b"`,
`"user\@host.com"`, `"$$sr"`, `"@$r"`, `"$$sr\[1]"`, `"$$sr [1]"`,
`"$$sr\{b}"`, `"$$sr@x"`, `"\$x[1]"`, `"\@x"`, `"$$"`, `"$#x $#$r $#{$r}"`,
`"@x[1,2]"`, `"@h{k,b}"`, `"$h{-f}"`, `"$x[-1]"`, `"$::name"`,
`"@{x}[0]"`, `"@{y}"`, `"@y[0]"`, `"@- @+"`, `"$+[1] $-[1]"`,
`"\U$name\E done"`.

Two companion probe files: `probe-braces.pl` (24 rows, the brace/arrow
continuation rules) and `probe-arrow.pl` (9 rows, explicit `->` after a
deref).  Their only remaining perl-vs-PCL differences are **die-message
text/line** and the **#443 leniency** below — and in every one of those
rows the interpolation now answers exactly what the equivalent CODE
answers, which is the port's contract.

### Emission

* `tools/corpus-diff.pl`: **6 of 111 files** — `hash.t`, `kvaslice.t`,
  `kvhslice.t`, `postfixderef.t`, `ref.t`, `split.t`.  Every hunk in all
  six is the §2 fix and nothing else: `(p-hslice $h …)` →
  `(p-hslice (p-cast-% $h) …)`, `(p-aslice $name1 …)` →
  `(p-aslice (p-cast-@ $name1) …)`, `(p-kv-aslice (unbox $a) …)` →
  `(p-kv-aslice (p-cast-@ $a) …)`.  **Silent drops 7, unchanged.**
  Measured separately — `Pl/ExprToCL.pm` restored from main in the working
  tree, `tools/corpus-diff.pl 463a8f8`, then `git checkout --` — and the
  answer is *"emission identical to 463a8f8 across 111 files"*: **the
  interpolation port on its own moves nothing in the corpus.**  That is a
  run, not an inference from the hunks.
* `tools/emission-ab.pl --ref 753ecab` over **perl's own `t/` (605 files) +
  `lib/**` (22) + the cpan board (402) = 1029 files**: **21 DIFF, 0 RCDIFF**,
  and every one is one of the two fixes —
  `mro/package_aliases.t` + `mro/package_aliases_utf8.t` are **#420** in the
  wild (`"$$_{code}"` / `"$$_{name}"` in the TAP description strings →
  `(p-gethash-deref $_ "code")`), the other 19 are the §2 slice deref
  (`io/bom.t`, `op/avhv.t`, `op/hash.t`, `op/kvaslice.t`, `op/kvhslice.t`,
  `op/postfixderef.t`, `op/ref.t`, `op/split.t`, `uni/fold.t`,
  `lib/Math/BigInt/Calc.pm`, `Carp.pm`, `Test2::API::Context`,
  `Test2::Util::HashBase`, `Test::Builder`, `Test::More`,
  `Test::Tester::Capture`, `Text::CSV_PP.pm`, `Text-CSV/t/76_magic.t`,
  `TestML::Tiny`).
  **How the number was reached** (three runs, because two deltas landed after
  the first): the full 1029 A/B gave 22, of which `op/sub_lval.t` was the
  digit-named-array wrapper described below; after that fix its reach was
  re-measured over **the 17 files in the four populations carrying an
  `@<digit>` shape** — 15 SAME, and the only 2 DIFF are `postfixderef.t`
  (both copies), which is the slice fix.  The braced-punctuation delta was
  measured the same way over **the 5 files carrying a `${X}`/`@{X}`
  punctuation body — all 5 SAME**.  Each delta measured against the
  population it can reach.

### Gate / sweep

* Gate COLD (`tools/prove-core`, `PCLXS_DIR=$HOME/pclxs`): **156 files /
  5641 rows**, and the ONLY failures are the 13 pclxs xs rows (xs-01 5,
  xs-02 4, xs-03 4) — pclxs is under separate work.  main is 156 / 5639;
  the two extra rows are this session's guards.
* Full sweep (`perl sweep-perl-tests.pl --jobs 3`, runs its own gate):
  **GATE clean.**  TOTAL passing **18365**, baseline 18365, **+0**.
  Drops **7, census 7, +0**.  `0 new, 0 fixed`; 7 UNSTABLE and 10
  DID-NOT-RUN rows, all in files already PARTIAL, which the tool labels as
  crash-file noise rather than regressions.  **No baseline was edited** —
  nothing moved.
* The generation is **v2-170** and the three checked-in artifacts
  (`cl/pcl-pack.lisp` via `tools/rebuild-pack`, `cl/pcl-mro.lisp` and
  `cl/pcl-warnings.lisp` via `pl2cl --extension` + `tools/tag-license`) were
  regenerated in the same commit; `Pl/t/artifact-staleness-01.t` is green.

---

## §4  Filed, not fixed (both PRE-EXISTING, both found by the probes)

* **#443 — PCL is lenient where perl is fatal on a wrong-kind deref.**
  `${$aref}` hands the referent back (so it reads as the array's length)
  where perl dies "Not a SCALAR reference"; `$$scalarref_to_aref[1]`
  answers undef where perl dies "Not an ARRAY reference".  **Identical in
  the code path and the interpolation path** — they agree with each other
  and both diverge from perl, which is why it is not this session's.
  `p-cast-@`/`p-cast-%` DO carry the `#163` referent check; `p-cast-$` and
  the `p-*-deref` element accessors do not.  It is the reason
  `"$$r->[1]"` now prints `20` where perl dies: the port made the string
  agree with the code, and the code is the lenient one.
* **#444 — `"${ \"a\" }"` emits UNREADABLE CL** and loses the whole file to
  an SBCL read error.  The `@{ EXPR }` arm unescapes the block text; the
  `${ EXPR }` arm never has.  The task carries the discriminating
  measurement that makes it not-a-one-liner: a blanket `unescape_string`
  breaks `"${\ (2*3) }"`, whose backslash is the reference operator.

---

## §5  Asks

**Ask 1 — was §2 in scope?**  The emitter half was not in the task and it
changes emission for a code shape (six corpus files).  I took it because
#420's own acceptance table has `"@$r[0,1]"` → `10 20` in it, so the item
could not close without it, and because leaving `gen_kv_array_slice_form`'s
`unbox` next to a new `p-cast-@` would have been the second copy rule 11
forbids.  If the ruling is that it should have been filed and the #420 rows
left failing, say so and I will split it.

**Ask 2 — the `@{ EXPR }` / `${ EXPR }` unescape asymmetry.**  I preserved
it byte-for-byte rather than unifying it, and filed #444 with the
measurement.  Preserving a known-wrong asymmetry inside a port whose whole
point is de-duplication is arguable both ways; I chose "no unmeasured
semantic change inside a structural port".  Confirm or reverse.

**Ask 3 — consumer 2.**  `#388`/`#237` stay open for the Parser2 rename
machinery, which is also the compile-time half (`_interp_names` 1.65 s
exclusive in a 50 s sample).  It is the last consumer; does it want its own
session on the queue now, or does it wait behind B3?

**Ask 4 — a scanner fact worth a `docs/ir-spec.md` line?**  "A braced NAME
closes an interpolated reference, a braced EXPRESSION does not" is a perl
semantic PCL now depends on in two consumers.  It is in
`Pl/InterpScan.pm`'s comment and `docs/interp-scan.md`; the CLAUDE.md rule
says load-bearing semantics go in `ir-spec.md` and a code comment must never
be the only copy.  `interp-scan.md` is arguably that second copy — confirm,
or I add an `ir-spec` line.
