# PPI bugs/limitations hit by PCL — candidates to report upstream

PPI is the Perl parser PCL relies on (`PPI::Document`). A handful of PCL's
"not-supported" items are actually PPI tokenizer bugs, not PCL design choices.
This file collects the ones worth filing upstream, with **minimal repros** and the
**PPI version tested**, so they can be sent to the PPI maintainers.

- Upstream: <https://github.com/Perl-Critic/PPI> (issues), or rt.cpan.org / PPI.
- **PPI version tested: 1.291** (perl 5.40.3). Re-run the repros before filing in
  case a newer PPI already fixed one.

> Scope note: NOT every PCL parse quirk is a PPI bug. Counter-example —
> `$$ref->()` mis-association is **PCL's**, not PPI's: PPI emits a faithful flat
> token stream (`Cast '$'`, `Symbol '$r'`, `Op '->'`, `List '()'`) and leaves
> operator precedence to the consumer; PCL's PExpr orders it wrong. Only put
> things here that PPI itself tokenizes/structures incorrectly.

---

## 1. `$$$ref` (triple dereference) tokenized as `$$` + `$ref`  [CONFIRMED 1.291]

**Perl:** `$$$ref` means `${${$ref}}` — a triple scalar dereference.

**PPI:** tokenizes it as the special PID variable `$$` (`PPI::Token::Magic`)
followed by `$ref` (`PPI::Token::Symbol`):

```perl
use PPI;
my $d = PPI::Document->new(\'$$$ref');
$_->isa('PPI::Token') && printf "%-22s %s\n", ref($_), $_->content
    for @{ $d->find(sub { $_[1]->significant }) || [] };
# PPI::Token::Magic        $$
# PPI::Token::Symbol        $ref
```

**Expected:** a Cast/Symbol structure equivalent to `${ ${ $ref } }` (e.g.
`Cast '$'`, `Cast '$'`, `Symbol '$ref'`), the way `$$ref` already yields
`Cast '$'` + `Symbol '$ref'`.

**Impact:** any triple (or deeper) sigil deref is unparseable. PCL documents the
`${$$ref}` block form as the workaround (`docs/not-supported.md` →
"Triple … dereference without braces").

**WORKED AROUND IN PCL (s390, #305) — the workaround note above is retired.**
The mis-lex is positional and therefore repairable: `$$` is the PID only when
it is NOT directly followed by another deref sigil, a scalar, or a brace
block.  `Pl::PExpr::_split_pid_magic_cast_run` is one pre-pass over the token
list (beside `_default_filetest_operand`, before any term machinery) that
rewrites such a `Magic('$$')` into two `Cast('$')`; source ADJACENCY is read
from PPI's own sibling links, not from the whitespace-filtered `@$e`, so
`$$ $x` is left alone.  Everything downstream then sees the ordinary cast run
it already understands.

Note the mis-lex is not uniform, which is why one repair covers all depths:
`$$rr` lexes CORRECTLY (Cast+Symbol), `$$$rr` gives Magic+Symbol, `$$$$rr`
gives Magic+Cast+Symbol — PPI takes `$$` greedily and then resumes normally.

Until s390 this was a **silent statement drop**: the stray Magic matched no
case, the "Missing case" die degraded to a `;; PARSE ERROR` comment, and
`print "x=", $$$rrr->{k}, "\n";` printed NOTHING AT ALL — not even the
literal prefix.  Three such statements were being dropped inside
`perl-tests/ref.t`.  Guard rows: `Pl/t/transpile-test-10.t` (`#305` × 3,
including the inverse that bare `$$` is still the PID).

---

## 2. C99 hex-float literal `0x1.8p+1` split into 5 tokens  [CONFIRMED 1.291]

**Perl 5.22+:** `0x1.8p+1` is a single numeric literal (== 3.0).

**PPI:** tokenizes it as five separate tokens:

```perl
use PPI;
my $d = PPI::Document->new(\'my $x = 0x1.8p+1;');
printf "%-26s %s\n", ref($_), $_->content
    for @{ $d->find(sub { $_[1]->significant }) || [] };
# ...
# PPI::Token::Number::Hex    0x1
# PPI::Token::Number::Float  .8
# PPI::Token::Word           p
# PPI::Token::Operator       +
# PPI::Token::Number         1
```

**Expected:** one `PPI::Token::Number::*` (a dedicated hex-float number token, as
Perl itself tokenizes it).

**Impact:** hex-float literals are uninterpretable from the token stream. PCL
skips `perl-tests/hexfp.t` for this reason (`docs/not-supported.md` → "Hex
floating-point literals").

---

## 3. `< EXPR >` comparison chain misread as a glob/readline  [FIXED in PPI]

**Reported earlier** (PCL `docs/ppi-glob-disambiguation.md`): a chain like
`$x->[0] < $x->[1] > $x->[2]` was misread by PPI as a `<...>` readline/glob,
silently dropping/garbling the statement.

**Status on PPI 1.291:** FIXED upstream — it tokenizes correctly now (zero
`PPI::Token::QuoteLike::Readline`). Do NOT file. It is **not** in
`docs/ppi-bug-report.t`. PCL keeps its own regression guard for it in
`Pl/t/misc-fixes-02.t` ("chained < > comparison is not misparsed as a
glob/readline") so we notice if either PPI or PCL regresses.

---

## 4. `7%-3` tokenized as the magic hash `%-` (modulo lost)  [CONFIRMED 1.291]

**Perl:** `7%-3` is `7 % -3` → `-2`. `%+`/`%-` are the named-capture magic
*hashes*; they only appear in term position, never right after a term.

**PPI:** with no space, `%-` (and `%+`) tokenizes as `PPI::Token::Magic`, losing
the `%` operator:

```
PPI::Token::Number  7        # PPI gives the spaced form 7 % -3 correctly:
PPI::Token::Magic   %-       #   Number 7 / Operator % / Number -3
PPI::Token::Number  3
```

**Repro + ready-to-send report:** `docs/ppi-bug-modulo-magic.md`. Failing test is
Bug 3 in `docs/ppi-bug-report.t`.

**Impact:** any `term%-operand` / `term%+operand` is a PCL PARSE ERROR. PCL works
around it in `Pl/Parser.pm` (`_fix_modulo_magic`): a `%-`/`%+` Magic token that
directly follows a term is re-split into `% -`/`% +` on the PPI tree, then
re-parsed (so strings/regexes are untouched). Regression guard in
`Pl/t/misc-fixes-02.t`.

---

## 5. `{ LITERAL , ... }` (anon hash, comma-separated) misclassified as a Block  [CONFIRMED 1.291]

**Perl:** in term context, a leading `{` whose first element is a *string or
number literal* followed by `,` is an anonymous-hash constructor, exactly like the
`=>` form.  `eval "{ 'a' , 'foo' }"` returns a HASH ref (`{a => 'foo'}`); `{ 1, 2 }`
likewise.  (Barewords `{ foo, 1 }` and variables `{ $x, 1 }` stay code blocks —
Perl only promotes a *literal* first element.)

**PPI:** only `=>` triggers Constructor classification.  With a comma it returns a
`PPI::Statement::Compound` wrapping a `PPI::Structure::Block`, i.e. a code block:

```perl
use PPI;
for my $src ("{ 'a' , 'foo' }", "{ 'a' => 'foo' }") {
  my ($c) = PPI::Document->new(\$src)->schildren;
  printf "%-18s -> %s :: %s\n", $src, ref($c), join(",", map ref, $c->schildren);
}
# { 'a' , 'foo' }   -> PPI::Statement::Compound :: PPI::Structure::Block
# { 'a' => 'foo' }  -> PPI::Statement          :: PPI::Structure::Constructor
```

**Expected:** both should be `PPI::Structure::Constructor` (anon hash), since Perl
treats `{ LITERAL , ... }` and `{ LITERAL => ... }` identically in term context.

**Impact:** `eval "{ 'a', 'b' }"`-style hash construction (and any rvalue
`{ LITERAL, ... }` PPI happens to flatten to a Compound) was generated as a bare
block.  Surfaced via Perl's own `t/comp/term.t` tests 15/17/19.  PCL works around
it in `Pl/Parser.pm` (`_bare_block_is_anon_hash`, used from
`_process_compound_statement`): a leading-`{` Compound whose body is a single
expression statement starting with a `Quote`/`Number` literal followed by `,`/`=>`
is re-routed to the anon-hash constructor codegen.  Deliberately narrower than the
map/grep block detector (`_block_is_hash_constructor`), where `{ 'a', $_ }` is a
genuine code block.  Regression guard in `Pl/t/transpile-test-04.t`.

---

## 6. `for ${*$f} (LIST) { }` — LEXER DIES: "Illegal state in 'for' compound statement"  [CONFIRMED 1.291]

Minimal repro (found s358, task #253 — kills `t/op/for.t` line 767 whole-file):

```perl
no strict 'refs';
my $f = "v";
for ${*$f} (5,11,33) { print }     # valid perl; runs, printing 5,11,33
```

`PPI::Document->new` returns undef with `Lexer failed: Illegal state in
'for' compound statement`.  This is not a mis-tokenization but a hard LEXER
failure: the foreach loop-variable slot accepts `$x` / `my $x` but not a
block-deref lvalue `${*$f}` (a glob-deref used as the loop alias — the perl
suite's low-refcnt-package-var / assert-SEGV regression test).  Plain
`for $x (…)` is fine; `foreach ${*$f} (…)` fails identically.

PCL-side status: **no workaround** — the whole file fails to transpile, and
that is the honest report.  `t/op/for.t`'s rows in the s323e suite snapshot
were an ACCIDENT of `--lenient-ppi` truncation (the same class as the #228
`lex.t` registration); the flag was retired by ruling (§5a.4, s356), so the
file now reads TRANSPILE-FAIL.  It is NOT a flip loss and NOT a PCL
regression (task #253, `docs/fable-answers-s357.md`); it joins the #254 §4
residue registration with this cause.  Ready to file upstream as **bug 4
in `docs/ppi-bug-report.t`** (self-contained Test::More file, all four
rows FAIL on 1.291 = the bugs).  A CANARY row in `Pl/t/misc-fixes-02.t`
asserts the current broken state — when a PPI upgrade fixes the lexer,
that row FAILS, which is the signal to drop this section and un-register
op/for.t.

---

## 7. `(sub :lvalue { … })` — an anon sub's ATTRIBUTE at expression start becomes a LABEL  [CONFIRMED 1.291]

Minimal repro (found s365, task #268 — `t/op/sub_lval.t`):

```perl
my @a = (sub :lvalue { 1 });          # perl: one code ref
for my $s (sub :lvalue { 1 }) { }     # perl: one iteration
```

Token dump — the SAME text tokenizes two different ways depending on
position:

```
my $f = sub :lvalue { 7 };        # mid-expression: CORRECT
   Word(sub)  Operator(:)  Attribute(lvalue)  Block

my @a = (sub :lvalue { 1 });      # at expression START: WRONG
   Label(sub :)  Word(lvalue)  Block

my @a = (sub :lvalue :method {}); # chained attributes chain as Labels
   Label(sub :)  Label(lvalue :)  Word(method)  Block

for my $s (sub :lvalue {1}, 2) {} # inside a `for` list, WORSE:
   Structure::For
     Statement::Compound [ Label(sub :) ]        <- its own STATEMENT
     Statement           [ Word(lvalue) Block Operator(,) Number(2) ]
```

Expected: `Word(sub) Operator(:) Attribute(lvalue)` in every position — a
`sub` keyword cannot be a label name, so `sub :` is never a label.  The
lexer's statement-start heuristic ("Word `:`" = label) fires before the
`sub` keyword is considered.  In the `for` case the mis-lex also makes PPI
call the loop parens a `PPI::Structure::For` (a C-style for header), so the
loop VARIABLE and the list end up in structurally different shapes than a
plain `for my $x (LIST)`.

PCL-side workaround: `Pl::Parser2::_normalize_anon_sub_attrs` (a
document-level pass beside the other PPI repairs) merges the split
statements back together, drops the attribute run, and re-blesses the
`Label('sub :')` into a plain `Word('sub')`; the `Structure::For`
re-bless sits in `_lower_compound` and keys on "a C-style `for` never has a
loop VARIABLE before its parens".  Before it, the expression fell through to
`Bug. Fell through. Missing case: [` and the whole statement was replaced by
a PARSE ERROR comment — silently dropping code.  Guard rows:
`Pl/t/transpile-test-09.t` (`anon sub with attributes in expression
position (#268)`), which also holds the inverse guards (a REAL loop label
must still lower as a label).

### 7b. …and if the prototype text ends in `$`, the `$)` eats the paren too  [CONFIRMED 1.291]

Found s367 (task #270) by adversarial probing of the 7 repair.  INSIDE the
mis-lexed run, `prototype($)`'s closing paren is tokenized as the **magic
variable `$)`**, so the attribute's paren group never closes there — PPI
closes it on the SUB's own closing paren instead and swallows the block:

```perl
my @l = (sub :prototype($) { 42 });   # perl: 42.  PCL (before the fix): NOTHING, exit 0.

   Label(sub :)  Word(prototype)
   Structure::List ( … )              <- closed by the OUTER `)`
     Magic($))                        <- ate the prototype's own `)`
     Structure::Subscript { 42 }      <- the sub's BLOCK, swallowed
   … and the enclosing List is left UNFINISHED (`( ... ???`)
```

Every prototype whose text ends in `$` hits it — `($)`, `(;$)`, `($;$)`,
`($$;$)`, … including the single most common prototype there is.  `($$)`,
`(\@)`, `($_)` and friends lex correctly and take the 7 path unchanged
(verified live).  `(;$)` mangles one layer further again: its `;` becomes a
`Statement::Null`, which is INSIGNIFICANT, so any `schildren`-based walk
misses it.

Because the stolen `)` belongs to the enclosing structure, no local tree
edit restores a well-formed tree (and inside a `for` LIST the damage spreads
across sibling statements).  So `Pl::Parser2::_repair_swallowing_prototypes`
runs BEFORE all the 7 tree surgery and does not read the tree at all: it
walks the RAW TOKEN STREAM from each `sub :` Label and, when the run spells
`:[attr:]*prototype(` … `$)`, blanks exactly those tokens.  The document
text then reads `sub { … }` as if the attribute had never been written, and
one reparse (`_reparse_doc`, shared with the state prepass) yields the tree
PPI would have built for the plain anon sub.  Dropping the prototype is
effect-only and ANNOUNCES on stderr, per the s329 boundary.

The silent half is closed too: 7's repair used to `next` when its run did
not end at a Block — the deliberate "don't guess" guard — after which the
statement was dropped with a PARSE-ERROR comment at exit 0.  A `sub :` Label
is only ever produced by this mis-lex, so a run that does NOT end at a Block
is known-mangled input, and it now **dies naming the shape**.  Guard rows:
`Pl/t/transpile-test-09.t` (`anon sub :prototype ending in $ at expression
start (#270)`), with `($$)`/`(\@)` and a NAMED `sub f :prototype($)` as the
inverse guards.

---

## 8. A variable declaration's ATTRIBUTE is not a `Token::Attribute`  [CONFIRMED 1.291]

Minimal repro (found s395, task #314 family F-A2 — `t/op/attrs.t`, `t/uni/attrs.t`):

```perl
my $x : shared = 1;      # valid perl; prints 1
```

```
# PPI::Document->new(\'my $x : shared = 1;')->tokens
# PPI::Token::Word          my
# PPI::Token::Symbol        $x
# PPI::Token::Operator      :          <-- expected PPI::Token::Attribute 'shared'
# PPI::Token::Word          shared     <--
# PPI::Token::Operator      =
# PPI::Token::Number        1
# PPI::Token::Structure     ;
# (statement class: PPI::Statement::Variable)
```

PPI *does* produce a `PPI::Token::Attribute` for the same syntax on a **sub**
(`sub f : lvalue {…}`), but inside a `PPI::Statement::Variable` the attribute
run comes back as a bare `Operator ':'` followed by ordinary `Word`s (plus an
argument `List` for `: Foo(bar)`). So there is no way to tell a declaration's
attribute from an unrelated `?:` fragment except by position.

Why it matters beyond tidiness: the shape is indistinguishable from a
declaration followed by a trailing expression, so a consumer that supports
`my $x <tail>` will silently take `: shared = 1` as that tail. In PCL that made
`my $x : shared = 1; print $x` **print nothing** — a silent wrong that had been
live since the scalar branch shipped, found only by probing the family.

PCL-side workaround: `_strip_decl_decorations` (Pl/Parser2.pm) deletes the
attribute run in ONE document pre-pass, before any decl-shape matcher runs, and
ANNOUNCES the drop (rule 12, effect-only). Unblocked `t/op/attrs.t` 0 → 28 and
`t/uni/attrs.t` 0 → 8; the remainder is the attribute PROTOCOL (task #322).

---

## 9. `${ PUNCTUATION }` is a variable, but lexes as Cast + Block  [CONFIRMED 1.291]

Minimal repro (found s395 — `t/re/pat_rt_report.t`, 2513 rows behind it):

```perl
my @plus = @{+};        # valid perl: @{+} IS the magic array @+
print "@{+}";           # same in a string, and in a pattern: qr/A@{+}B/
```

```
# @{+}    -> Cast '@'  Structure '{'  Operator '+'  Structure '}'
# @{foo}  -> Cast '@'  Structure '{'  Word     'foo'  Structure '}'   (same shape)
# ${!}    -> Cast '$'  Structure '{'  Operator '!'  Structure '}'
# $#-     -> PPI::Token::Magic '$#-'                 (NOT an ArrayIndex)
```

Perl's `${ NAME }` accepts a punctuation name, so `@{+}` is `@+`, `${!}` is
`$!`, `%{+}` is `%+`. PPI folds the **identifier** spelling (`@{foo}` → `@foo`)
and the **caret** spelling itself, but leaves the punctuation ones as a deref
Cast over a Block containing a lone `Operator`. A deref block holding exactly
one Operator token can never be an expression, so this is unambiguous and PPI
could fold it the same way it folds the others.

Second half, same family: `$#foo` lexes as `PPI::Token::ArrayIndex`, but `$#-`
and `$#+` come back as a single `PPI::Token::Magic` — so a consumer keyed on
ArrayIndex sees no case at all and emits the name as a bare symbol.

In PCL this was a **silent empty list** in plain code and a die in a pattern.
Workaround: one decision function (`Pl::PExpr::braced_punct_magic_name`) asked
by the token pre-pass and by StringInterpolation's `@{…}` scanner — a pure
re-tokenization, no new emission case. `t/re/pat_rt_report.t` 0 → 2431.

---

## 10. `for` accepts only `[my] $scalar` as its loop variable — and mis-lexes the rest of the FILE  [CONFIRMED 1.291]

Minimal repros (found s396, tasks #327 and #329):

```perl
use feature 'refaliasing', 'declared_refs';
for \my %e (@l) { A() }          # valid perl 5.22+: %e aliases each element
for my ($q, $r) (@l) { A() }     # valid perl 5.36+: two at a time
```

```
# for \my %e (@l) { A() } print "x";
#   PPI::Statement::Compound   for
#   PPI::Statement             \my %e (@l) { A() } print "x";
#
# for my ($q, $r) (@l) { A() } print "x";
#   PPI::Statement::Compound   for my
#   PPI::Statement             ($q, $r) (@l) { A() } print "x";
#
# for my $q (@l) { A() } print "x";            <-- the shape PPI does handle
#   PPI::Statement::Compound   for my $q (@l) { A() }
#   PPI::Statement             print "x";
```

The loop-variable slot only accepts `$scalar` or `my $scalar`. A `\`-cast
(refaliasing), a non-scalar (`%e`, `@a`), and the 5.36 parenthesized
n-at-a-time list all fail it — and unlike §6 this is **not** a clean lexer
error: the `Compound` statement silently keeps only the keyword, and the entire
rest of the construct **plus every following statement up to the next `;`** is
swallowed into one flat sibling `PPI::Statement`. A consumer therefore sees a
`for` with no list and no block, and loses unrelated code with it.

Related: §6 is the same slot rejecting a block-deref lvalue `${*$f}`, where it
dies loudly instead. The two together suggest the loop-variable slot is the
thing to widen upstream.

PCL-side workaround: repair the RAW TOKEN STREAM and reparse (the §7b pattern —
no tree edit can work, the enclosing structure is unfinished). Both shapes are
re-spelled into constructs PPI does lex:

```perl
for \my %e (@l) { BODY }      =>  for my $tmp (@l) { \my %e = $tmp; BODY }
for my ($q,$r) (@l) { BODY }  =>  my @L = map \$_, (@l); my $I = 0;
                                  while ($I < @L) { \my $q = $L[$I];
                                                    \my $r = $L[$I+1]; BODY }
                                  continue { $I += 2 }
```

`_repair_alias_foreach` / `_repair_nary_foreach` (Pl/Parser2.pm). Unblocked
`t/op/const-optree.t` 0 → 86 and `t/op/for-many.t` 0 → 63. Guard rows in
`Pl/t/refaliasing-01.t`.

---

## 11. `/PATTERN/` after a paren-less WORD is tokenized as division  [CONFIRMED 1.291]

**Perl:** after a bareword that is not a known unary operator, the tokenizer
expects a TERM, so `/` starts a match against `$_`:

```perl
sub ok { print "ok(@_)\n" }
$_ = "aa";
ok /a/, "desc";        # perl: ok(1 desc)
print /a/, "\n";       # perl: 1        <- a CORE list operator, same shape
```

**PPI:** correct after `grep`, `return`, `(` and `=` — and wrong after any
other Word, including core list operators:

```
ok /x/, 1;      Word(ok)     Operator(/) Word(x) Operator(/) Operator(,) Number(1)
print /x/, 1;   Word(print)  Operator(/) Word(x) Operator(/) Operator(,) Number(1)
grep /x/, @a;   Word(grep)   Regexp::Match(/x/)  Operator(,) Symbol(@a)      <- right
return /x/;     Word(return) Regexp::Match(/x/)                              <- right
ok(/x/);        Word(ok) Structure(() Regexp::Match(/x/) Structure())        <- right
```

Nothing downstream can recover it: the pattern's own text has been
re-tokenized *as code* (`/$qr/` becomes two divisions around a Symbol), so the
match and the whole argument list are lost. The rule PPI already implements
for `grep`/`return` is the general one — a `/` in TERM position (after a Word
that is not a term itself) starts a match.

**Repro + failing row:** Bug 8 in `docs/ppi-bug-report.t`.

**Impact on PCL: SILENT WRONG, two flavours (task #351).**  The statement is
dropped whole — `ok /$qr/, "desc";` produces no call at all (`t/re/pat_re_eval.t`
line 1114, `t/re/pat.t`) — and with a modifier letter it compiles to real
DIVISION: `ok /foo/x, "d";` emits `(P-/ (P-/ …) "x")` and dies at run time with
`division-by-zero`.

**PCL workaround (s404):** `_repair_word_match` in `Pl/Parser2.pm` rewrites the
opening `/` to `m/` on the raw token stream and reparses.  Its condition is
perl's, and it is a NEGATIVE, which is what makes it safe: perl reads `/` after
a bareword as division ONLY when the word is a TERM (a constant, a
`()`-prototyped sub, or a 0-ary builtin) — for anything else perl does not fall
back to division, it is a **syntax error** (measured: `ok /foo/` with `ok`
undeclared, and with `ok` declared BELOW, are both syntax errors).  PCL assumes
valid Perl (principle 9), so "not a term" is exactly the test.  Measured over
both populations: 28 `WORD /` sites, of which the repair fires on `ok`, `while`
and `when` and must NOT fire on `map { … } <op/*>`, where PPI derails a GLOB
into `< Word / * >` — hence the `<` guard.  Removes 11 dropped statements.

---

## 12. `)*name` — a `*` after a term is lexed as a GLOB, not multiplication  [CONFIRMED 1.291]

**Perl:** a `*` where a term has just ENDED is multiplication; a glob can only
start where a TERM can.

```perl
my ($s, $k) = (0, "ab");
$s += length($k)*length($k);   # perl: 4
```

**PPI:** after a token that ends a term, `*name` written with no space becomes
one `PPI::Token::Symbol` (a typeglob):

```
$s += length($k)*length($k);   Word(length) List Symbol(*length) List     <- wrong
$s = $x*length($k);            Symbol($x)   Symbol(*length) List          <- wrong
$s = $a[0]*foo();              Structure(]) Symbol(*foo)                  <- wrong
$s = $h{x}*foo();              Structure(}) Symbol(*foo)                  <- wrong
$s = "3"*length($k);           Quote("3")   Symbol(*length)               <- wrong
$s = 2*length($k);             Number(2) Operator(*) Word(length)         <- RIGHT
$s = length($k) * length($k);  … Operator(*) Word(length)                 <- RIGHT (space)
sub f { 1 } *bar = \&f;        Structure(}) Symbol(*bar)                  <- RIGHT (a real glob)
```

A NUMBER on the left, or a single space, lexes correctly — which is what makes
this so easy to miss.

**Repro + failing row:** Bug 9 in `docs/ppi-bug-report.t`.

**Impact on PCL: SILENT WRONG (task #354).**  The statement reaches PExpr as
`Word List Symbol List`, a shape it has no case for, so the WHOLE STATEMENT is
dropped (#138 family).  `Data::Dump` line 325 is exactly this, in any program
that uses it.  **Workaround (s404):** `_repair_glob_multiply` splits the token
back into `*` + word when the previous significant token ends a term.  `}` is
counted only when it closes a SUBSCRIPT — the block-closing case above is a
real glob, and the tree is what tells them apart.

---

## 13. Tokenization of a trailing `__END__`/`__DATA__` section depends on `$/`  [CONFIRMED 1.291]

**Perl:** `$/` is an input-record separator for *reading*; it has nothing to do
with how source text is parsed.

**PPI:** with `$/` undefined (slurp mode — extremely common in code that has
just read the source with `local $/`), a document whose LAST line is `__END__`
or `__DATA__` comes back with ONE EXTRA newline in the section:

```perl
my $src = "# c\n__END__\n";
{ local $/; PPI::Document->new(\$src)->serialize }   # 13 bytes — an extra "\n"
PPI::Document->new(\$src)->serialize                 # 12 bytes — correct
```

so `serialize` is not the identity and the DATA section gains a line that the
file does not contain.

**Repro + failing row:** Bug 10 in `docs/ppi-bug-report.t`.

**Impact on PCL: SILENT WRONG, fixed at both ends (s404).**  `pl2cl`'s stdin
branch had a bare `local $/;` that stayed in effect across the parse, so every
program transpiled through `pl2cl < file` got an extra empty line in its
`<DATA>` handle (this is how `tools/emission-ab.pl` feeds files, which is where
it surfaced).  The slurp is now scoped, and `_ppi_parse` — the one place either
pipeline turns source into a document — trims trailing whitespace the parse
invented, so the result no longer depends on the caller's `$/` at all.

---

## 14. `<FH>` / `<glob>` after a list operator or a block is lexed as `<` … `>`  [CONFIRMED 1.291]

**Perl:** `<…>` in TERM position is a readline/glob, wherever that position
comes from — after `sort`, after `print`, after a `map`/`grep` block.

**PPI:** correct after `=` and after a comma, wrong after a list-operator Word
or a closing brace it took for the end of a term:

```
my @s = <op/*>;               QuoteLike::Readline(<op/*>)              <- right
join ",", <STDIN>;            QuoteLike::Readline(<STDIN>)             <- right
sort <STDIN>;                 Word(sort) Operator(<) Word(STDIN) Operator(>)   <- WRONG
print <STDIN>;                Word(print) Operator(<) …                <- WRONG
my @l = sort <$fh>;           … Operator(<) Symbol($fh) Operator(>)    <- WRONG
map { $h{$_}++ } <op/*>;      … Structure(}) Operator(<) Word(op) Operator(/) Operator(*) Operator(>)  <- WRONG
my @s = grep { 1 } <op/*>;    same                                     <- WRONG
```

This is the same operator-vs-term expectation error as §11 and §12, in the
third direction: a TERM position PPI reads as an operator one.  (§3 was the
INVERSE — comparison chains read as globs — and was fixed upstream in 1.291;
this direction was not.)

**Repro + failing row:** Bug 11 in `docs/ppi-bug-report.t`.

**Impact on PCL: none today, because two workarounds cover it — but they are
load-bearing and undocumented until now (logged s404).**
`Pl::PExpr::_fix_ppi_glob_after_block` scans the token run for `< … >` that
looks like a glob/readline and rebuilds the single
`PPI::Token::QuoteLike::Readline` token; it is why `sort <$fh>`, `print <$fh>`
and `map { … } <op/*>` all transpile correctly (probed against perl, s404).
The second is a NEGATIVE dependency: `_repair_word_match` (§11) must NOT treat
the `/` inside a derailed `<op/*>` run as a match, so it skips a `/` whose
Word is preceded by `<` — i.e. this bug's damage is what that guard is keyed
on.  If PPI is fixed, both come out together.

---

## 15. `)` followed by `-1` — the operator is swallowed into a negative NUMBER  [CONFIRMED 1.291]

**Perl:** `(1+2)-1` is subtraction; a `-` where a term has just ended cannot
start a literal.

**PPI:**

```
my $x = (1+2)-1;   … Structure()) Number(-1) Structure(;)     <- WRONG
my $x = (1+2) - 1; … Structure()) Operator(-) Number(1)        <- right (space)
```

so the subtraction disappears and the consumer sees two adjacent terms.  Same
operator-vs-term family as §11/§12/§14: after `)` a term has ENDED, so `-` is
an operator.

**Repro + failing row:** Bug 12 in `docs/ppi-bug-report.t`.

**Impact on PCL: none today** — `Pl::PExpr::_fix_ppi_negative_number_bug`
splits such a Number back into `Operator(-)` + the positive number when the
previous token ends a term.  Logged s404 (the workaround predates the log).

---

## 16. perl 5.40's `^^` (logical XOR) is tokenized as two `^` operators  [CONFIRMED 1.291]

**Perl 5.40:** `$a ^^ $b` is the low-precedence-style logical XOR operator.

**PPI:** `Operator(^) Operator(^)` — two bitwise XORs, which is a different
expression entirely (and, applied to two operands, a parse the consumer cannot
tell from `$a ^ (^$b)`).

**Repro + failing row:** Bug 13 in `docs/ppi-bug-report.t`.

**Impact on PCL: none today** — `Pl::PExpr::_fix_ppi_logical_xor_bug` merges the
adjacent pair back into one `^^` token.  Logged s404 (the workaround predates
the log).

---

## 17. A SUBSCRIPT after a deref or a KV slice is structured as something else  [CONFIRMED 1.291]

**Perl:** `${$r}[0]` is element 0 of `@$r`, and `%h{...}` (5.20+) is a key/value
hash slice — in both, the bracketed part SUBSCRIPTS what precedes it.

**PPI:** it builds a different structure class for each:

```
my $v = ${$r}[0];        Structure::Block({$r})  Structure::Constructor([0])   <- WRONG
my %kv = %h{qw(a b)};    Symbol(%h)              Structure::Block({qw(a b)})   <- WRONG
my @s  = @h{qw(a b)};    Symbol(@h)              Structure::Subscript({…})     <- right
```

`Constructor` means "an anonymous arrayref literal" and `Block` means "a code
block" — so a consumer that trusts the structure class reads `${$r}[0]` as a
deref followed by a fresh `[0]` arrayref, and the KV slice as a hash followed by
a block.

**Repro + failing row:** Bug 14 in `docs/ppi-bug-report.t`.

**Impact on PCL: none today** — `Pl::PExpr` re-blesses both shapes at the sites
noted around `_parse_subscript_ix` (the braced-deref case) and the `%h{…}` /
`%a[…]` KV-slice cases.  Logged s404 (the workarounds predate the log).

---

## Possibly FIXED upstream — verify before trusting

* **`word :` in a ternary lexed as a Label** — `Pl::PExpr::_fix_ppi_ternary_label_bug`
  exists for it, but on 1.291 six spellings (`$c ? foo : bar` at statement
  start, in a list, in a call, ALL-CAPS, with parens, without spaces) all lex
  CORRECTLY.  Either the trigger is narrower than the comment says or PPI fixed
  it; task #357 is to find the shape or drop the workaround, with a canary
  either way.

---

## How to add to this list

When PCL hits a parse problem, first check whether **PPI** mis-tokenizes it
(dump `PPI::Document->new(\$src)` and inspect the token stream) vs. whether PCL's
PExpr just interprets a correct token stream wrongly. Only the former belongs
here. Include: minimal repro, the PPI token dump, expected tokens, PPI version,
and the PCL-side workaround/affected tests.
