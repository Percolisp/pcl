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

---

## How to add to this list

When PCL hits a parse problem, first check whether **PPI** mis-tokenizes it
(dump `PPI::Document->new(\$src)` and inspect the token stream) vs. whether PCL's
PExpr just interprets a correct token stream wrongly. Only the former belongs
here. Include: minimal repro, the PPI token dump, expected tokens, PPI version,
and the PCL-side workaround/affected tests.
