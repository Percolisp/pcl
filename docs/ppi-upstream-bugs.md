# PPI bugs PCL works around — the upstream report

PPI is the Perl parser PCL is built on (`PPI::Document`).  Some of what looks
like a PCL limitation is really a PPI tokenizer or lexer bug, and this file is
where each one is written down: a **minimal repro**, the actual token dump
against the expected one, the **PPI version tested**, and the workaround PCL
carries.  It is meant to be sendable to the PPI maintainers as it stands.

* Upstream: <https://github.com/Perl-Critic/PPI> (issues), or rt.cpan.org / PPI.
* **PPI version tested: 1.291** (perl 5.40.3).  Re-run a repro before filing it —
  a newer PPI may have fixed it.
* `docs/ppi-bug-report.t` is the same list as a runnable `Test::More` file:
  every row FAILS on the current PPI, so a passing row means the bug is gone.

> **Scope.**  Not every PCL parse problem is a PPI bug.  Counter-example:
> `$$ref->()` mis-association is *PCL's* — PPI emits a faithful flat token
> stream (`Cast '$'`, `Symbol '$r'`, `Op '->'`, `List '()'`) and leaves
> precedence to the consumer; PCL's expression parser orders it wrongly.  Only
> things PPI itself tokenizes or structures incorrectly belong here.

## The list

| # | what PPI gets wrong | status |
|---|---|---|
| [1](#1-ref-triple-dereference-tokenized-as---ref--confirmed-1291) | `$$$ref` (triple dereference) tokenized as `$$` + `$ref` | CONFIRMED 1.291 |
| [2](#2-c99-hex-float-literal-0x18p1-split-into-5-tokens--confirmed-1291) | C99 hex-float literal `0x1.8p+1` split into 5 tokens | CONFIRMED 1.291 |
| [3](#3--expr--comparison-chain-misread-as-a-globreadline--fixed-in-ppi) | `< EXPR >` comparison chain misread as a glob/readline | FIXED in PPI |
| [4](#4-7-3-tokenized-as-the-magic-hash---modulo-lost--confirmed-1291) | `7%-3` tokenized as the magic hash `%-` (modulo lost) | CONFIRMED 1.291 |
| [5](#5--literal----anon-hash-comma-separated-misclassified-as-a-block--confirmed-1291) | `{ LITERAL , ... }` (anon hash, comma-separated) misclassified as a Block | CONFIRMED 1.291 |
| [6](#6-for-f-list----lexer-dies-illegal-state-in-for-compound-statement--confirmed-1291) | `for ${*$f} (LIST) { }` — LEXER DIES: "Illegal state in 'for' compound statement" | CONFIRMED 1.291 |
| [7](#7-sub-lvalue-----an-anon-subs-attribute-at-expression-start-becomes-a-label--confirmed-1291) | `(sub :lvalue { … })` — an anon sub's ATTRIBUTE at expression start becomes a LABEL | CONFIRMED 1.291 |
| [8](#8-a-variable-declarations-attribute-is-not-a-tokenattribute--confirmed-1291) | A variable declaration's ATTRIBUTE is not a `Token::Attribute` | CONFIRMED 1.291 |
| [9](#9--punctuation--is-a-variable-but-lexes-as-cast--block--confirmed-1291) | `${ PUNCTUATION }` is a variable, but lexes as Cast + Block | CONFIRMED 1.291 |
| [10](#10-for-accepts-only-my-scalar-as-its-loop-variable--and-mis-lexes-the-rest-of-the-file--confirmed-1291) | `for` accepts only `[my] $scalar` as its loop variable — and mis-lexes the rest of the FILE | CONFIRMED 1.291 |
| [11](#11-pattern-after-a-paren-less-word-is-tokenized-as-division--confirmed-1291) | `/PATTERN/` after a paren-less WORD is tokenized as division | CONFIRMED 1.291 |
| [12](#12-name--a--after-a-term-is-lexed-as-a-glob-not-multiplication--confirmed-1291) | `)*name` — a `*` after a term is lexed as a GLOB, not multiplication | CONFIRMED 1.291 |
| [13](#13-tokenization-of-a-trailing-__end____data__-section-depends-on---confirmed-1291) | Tokenization of a trailing `__END__`/`__DATA__` section depends on `$/` | CONFIRMED 1.291 |
| [14](#14-fh--glob-after-a-list-operator-or-a-block-is-lexed-as-----confirmed-1291) | `<FH>` / `<glob>` after a list operator or a block is lexed as `<` … `>` | CONFIRMED 1.291 |
| [15](#15--followed-by--1--the-operator-is-swallowed-into-a-negative-number--confirmed-1291) | `)` followed by `-1` — the operator is swallowed into a negative NUMBER | CONFIRMED 1.291 |
| [16](#16-perl-540s--logical-xor-is-tokenized-as-two--operators--confirmed-1291) | perl 5.40's `^^` (logical XOR) is tokenized as two `^` operators | CONFIRMED 1.291 |
| [17](#17-a-subscript-after-a-deref-or-a-kv-slice-is-structured-as-something-else--confirmed-1291) | A SUBSCRIPT after a deref or a KV slice is structured as something else | CONFIRMED 1.291 |
| [18](#18-finally----is-not-part-of-the-try-statement--and-eats-the-next-one--confirmed-1291) | `finally { … }` is not part of the `try` statement — and eats the next one | CONFIRMED 1.291 |
| [19](#19-a-call-to-a-sub-named-x-after-a-list-operator-is-lexed-as-the-repetition-operator--confirmed-1291) | A call to a sub named `x` after a list operator is lexed as the repetition operator | CONFIRMED 1.291 |
| [20](#20-two-of-perls-three-ways-to-enable-try-are-not-recognised-so-the-construct-mis-lexes--confirmed-1291) | Two of perl's three ways to enable `try` are not recognised, so the construct mis-lexes | CONFIRMED 1.291 |
| [21](#21-a-term-initial--is-lexed-as-the-smart-match-operator--confirmed-1291) | A term-initial `~~` is lexed as the smart-match operator | CONFIRMED 1.291 |
| [22](#22-a-filetest-after-a-scalar-filehandle-is-split-into----word--confirmed-1291) | A filetest after a SCALAR filehandle is split into `-` + WORD | CONFIRMED 1.291 |
| [23](#23-a--scalar-with-a-non-ascii-name-is-split-into-cast--word--but--are-not--confirmed-1291) | A `$` scalar with a NON-ASCII name is split into Cast + Word — but `@`/`%`/`*`/`&` are not | CONFIRMED 1.291 |
| [24](#24-a-punctuation-named-array--is-split-into-cast--operator--confirmed-1291) | A PUNCTUATION-named array `@?` is split into Cast + Operator | CONFIRMED 1.291 |
| [25](#25--name-after-a-token-that-ends-a-term-is-lexed-as-one-negative-bareword-word--confirmed-1291) | `-name` after a token that ENDS A TERM is lexed as one negative-bareword Word | CONFIRMED 1.291 |
| [26](#26-a-glob-whose-name-is-punctuation-or-a-digit-run-is-split-into-two-operators--confirmed-1291) | A glob whose NAME is punctuation or a digit run is split into two operators | CONFIRMED 1.291 |

Also below: [possibly fixed upstream](#possibly-fixed-upstream--verify-before-trusting) (verify before trusting), and [how to add to this list](#how-to-add-to-this-list).

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

### 1b. `$${EXPR}` — the same mis-lex, plus a STRUCTURE error on the braces  [CONFIRMED 1.291]

**Perl:** `$${$ref}` is `${ ${$ref} }`, and `$$ {$ref}` — with a space — is
something else entirely (an element of the hash `%$`), so the two spellings
must not be conflated.  Probed on 5.40.3: the adjacent form prints the
double-deref value, the spaced form prints undef.

**PPI:** the adjacent form gives `Magic '$$'` as in bug 1, and then structures
the braces as a **`PPI::Structure::Subscript`** — a hash key — because they
follow what it believes is a variable.  The correct structure is the one PPI
already produces for `${$ref}`: `Cast '$'` + `PPI::Structure::Block`.

```perl
use PPI;
my $d = PPI::Document->new(\'$${$ref}');
print ref($_), "  [", $_->content, "]\n" for $d->schild(0)->schildren;
# PPI::Token::Magic          [$$]
# PPI::Structure::Subscript  [{$ref}]
#
# vs.  PPI::Document->new(\'${$ref}')  ->
# PPI::Token::Cast           [$]
# PPI::Structure::Block      [{$ref}]
```

The child statement differs too: the Subscript wraps a
`PPI::Statement::Expression`, the Block a plain `PPI::Statement`.

**Impact:** repairing only the `Magic` (bug 1's fix) is not enough — a consumer
is then left with `Cast, Cast, Subscript` and a subscript that has no base in
front of it.

**WORKED AROUND IN PCL (s441b, #463 item 1):** the same pre-pass,
`Pl::PExpr::_split_pid_magic_cast_run`, now also re-blesses that
`Structure::Subscript` to `Structure::Block` (and its
`Statement::Expression` child to `Statement`) — exactly the shape `${$ref}`
arrives in — whenever it is the token that made the `$$` two casts.  The
adjacency test that already guards the split keeps the spaced form out, which
is what perl does.  Guard rows: `Pl/t/prefix-incr-deref-01.t` (`#463(1)`, with
the PID and `$$h{k}` inverses).  Unblocked: `t/op/gv.t:911-912` and
`t/uni/gv.t:805-806` (a tie class whose `FETCH`/`STORE` are `$${$_[0]}`), four
dropped statements.

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

### 11a. The mis-lex takes THREE shapes, and the s404 workaround only saw one  (task #872, s458)

The repair asks one sanity question before firing — *is there a closing `/`?* —
and it asked it by scanning forward for a bare `Operator(/)`, stopping at the
first `Structure(;)`.  Both halves are wrong, because **everything between the
two delimiters has been re-tokenized as code**, so the pattern's own text
supplies `;`s and eats `/`s:

```
1 while /b(?{$n++; $n++})c/g;         # the `;` is INSIDE a (?{…}) code block
  Word(while) Operator(/) Word(b) Structure(() Operator(?) Structure({)
  Symbol($n) Operator(++) Structure(;)   <-- scan stopped here, repair declined
  … Structure(}) Structure()) Word(c) Operator(/) Word(g) Structure(;)

1 while /(a+b?)x/g;                   # after a (…) group PPI is in TERM position
  Word(while) Operator(/) Structure<(a+b?)> Operator(x)
  Regexp::Match</g;\nprint "next\n";>    <-- the closing / STARTED a match that
                                            swallowed the rest of the FILE

push @p, $1 while /([a-c])\//g;       # an escaped delimiter
  … Structure()) Cast(\) Operator(//) Word(g) Structure(;)
                          ^^ the closing / merged with the modifier-less / into
                             the defined-or operator
```

Note the second: PPI emits **both** an `Operator(/)` and a `Regexp::Match`, so
the statement it builds is not merely wrong, it is unbounded — one file-length
token.  The third shows the delimiter merging into a LONGER operator (`//`,
and by the same route `/=`, `//=`).

**PCL (s458, `Pl::Parser2::_match_close_after`):** the forward scan now tracks
`(`/`{`/`[` nesting and honours a `;` only at the statement's own depth, and
accepts as the close any `Operator`- or `Regexp`-class token whose content
BEGINS with `/`.  It answers yes/no, never *where*, so a `/` that is pattern
text rather than the true close is still the right answer.  Closes 5 census
drops in `t/re/pat.t` and `t/re/pat_advanced.t` — the `1 while /…(?{…})…/g`
counting loop — and the escaped-delimiter shape probed above.

### 11b. The damage CASCADES to the next statement, and MANUFACTURES tokens  (task #931, s461ar)

§11a noted in passing that after a `(…)` group PPI is back in term position and
the closing delimiter starts a match of its own.  That is not a property of the
group — it is the general shape, and it is what makes this bug *unbounded*: the
tokenizer passes the closing `/` and is in term position again, so the **next**
`/` in the file opens a `Regexp::Match` that runs to the one after it.  Two
adjacent `ok /…/` statements therefore collapse into ONE, with the second `ok`
buried *inside a token*:

```perl
sub ok { 1 }
$_ = 'aaabccc';
 ok /a+b?c+/, "one";
 ok /a*b?c*/, "two";
print "done\n";
```

perl: five statements, both calls run.  PPI 1.291 (`->schildren`): **three**,
the last of which is the whole rest of the file.  The token stream:

```
Word(ok) Operator(/) Word(a) Operator(+) Word(b) Operator(?) Word(c) Operator(+)
Regexp::Match</, "one";\n ok /a>          <-- the SECOND `ok` is inside this token
Symbol(*b)                                <-- manufactured: see below
Operator(?) Word(c) Operator(*)
Regexp::Match</, "two";\nprint "done\n";\n>
```

Two consequences a consumer has to plan for, and neither is visible from one
statement:

1. **A repair cannot be a single pass.**  The second `ok` is not a `Word` token
   at all until the first repair has been applied and the document reparsed, so
   a one-pass rewrite fixes the first of a run and loses the rest.
2. **The pattern text is tokenized as code, and some of it becomes tokens that
   appear nowhere in the source.**  The `*b` of `a*b?c*` above is a
   `PPI::Token::Symbol` — a typeglob — and `$_` and `*b` are the only Symbols
   the document has.  Any *other* repair that runs over this region is reading
   the inside of a regular expression: a consumer that repairs bug 12
   (`)*name` lexed as a glob) will faithfully rewrite `*b` to `* b` and splice
   a space into the middle of the pattern.  The result still compiles, matches
   something else, and says nothing — the worst outcome available.

**Repro + failing rows:** Bug 8b in `docs/ppi-bug-report.t` (statement count,
manufactured symbol).

**PCL workaround (s461ar):** `_repair_word_match` is now a bounded driver over
`_repair_word_match_pass`, iterating to a fixpoint (each round repairs at least
one `/` or stops, and a file's `/` characters are finite) — the same two-sub
shape `_repair_glob_pattern_cascade` was given in s449 for §14's cascade.  And
it MOVED to run second in `Pl::Parser2::parse`'s repair block, right after that
one and before `_repair_glob_multiply`: a repair whose damage is unbounded must
run before the repairs that read the damaged region, never beside them.
Removes the `t/re/pat.t:106` census drop, which had swallowed three statements
including a `$_ = 'aaaccc'` reset.

### 11c. A pattern that STARTS with a quote-like letter swallows the closing `/` into a QUOTE token  (task #940, s462at)

§11b's manufactured tokens are the general case; this is its worst spelling,
because the token that is manufactured is a **quote-like operator** and it
takes the closing delimiter — and the rest of the file — inside itself.

```perl
sub ok { print(($_[0] ? "ok" : "not ok"), " - $_[1]\n") }
$_ = "zzz";
ok /q*/, "four";
```

perl prints `ok - four`: the pattern `q*` (a literal `q`, zero or more) matches
the empty string in `"zzz"`.  PPI 1.291, after reading the `/` as division, is
in TERM position and reads `q*…*` as a `Quote::Literal` — running to the next
`*` or, as here, to end of file:

```
Word(ok) Operator(/) Quote::Literal<q*/, "four";\n>
```

There is **no closing-delimiter token at all**: it is characters 2-3 of the
quote's content.  A consumer scanning the token stream for the match's closing
`/` finds nothing and declines the repair.

Every quote-like letter derails this way, and the class of the manufactured
token follows the letter — probed one by one on 1.291:

| source        | manufactured token                            |
|---------------|-----------------------------------------------|
| `ok /q*/, …`  | `Quote::Literal <q*/, "four";\n>`             |
| `ok /qq*/, …` | `Quote::Interpolate`                          |
| `ok /m*/, …`  | `Regexp::Match <m*/, "two";\n>`               |
| `ok /s*/, …`  | `Regexp::Substitute`                          |
| `ok /y*/, …`  | `Regexp::Transliterate`                       |
| `ok /tr*/, …` | `Regexp::Transliterate`                       |
| `ok /x*/, …`  | *(nothing — `x` is not a quote-like letter)*  |

**Repro + failing rows:** Bug 8c in `docs/ppi-bug-report.t`.

**PCL workaround (s462at):** `_match_close_after` also answers yes when the
token IMMEDIATELY after the mis-lexed `/` is a quote-like token spelled with an
operator letter and a non-word delimiter and containing a `/`
(`_manufactured_quote_close`).  **Strictly the next token**, and that is
measured, not tidiness: `ok /bcd|xyz/, qq [… /…/];` (`t/re/pat.t:113`) ends in
a description string that contains `/`, so a scan of the whole statement also
"finds a close" after the real closing delimiter — whose preceding token is the
Word `xyz` — and repairs that one too, giving `m/bcd|xyzmmmm/`, one `m` per
fixpoint round.  perl's own lexer licenses the repair wherever it fires:
`sub w {8} print w / 2, " a/b\n";` is a syntax error in perl (`Unknown regexp
modifier "/b"`), i.e. perl reads that `/` as a match as well.

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

### 14b. THE CASCADE — a `/` after the mis-read `>` becomes a MATCH  (task #479)

The claim above that the impact is "none today" was true only of the three
tokens the rebuild reaches.  Once `>` has been taken for an operator, the
tokenizer is in OPERATOR-just-seen state, so the **next `/` is read as the
start of a match** — and PExpr's rebuild cannot undo that, because by then the
`/` and everything it swallowed are ONE token:

```
my $so = $ok ? <$f> // "" : "";
  Operator(?) Operator(<) Symbol($f) Operator(>) Regexp::Match(//) Quote::Double("") …
                                                 ^^^^^^^^^^^^^^^^^ should be Operator(//)

my $so = $ok ? <$f> / 2 : "";
  Operator(?) Operator(<) Symbol($f) Operator(>) Regexp::Match(/ 2 : "";)
                                                 ^^^ the REST OF THE STATEMENT
```

perl reads both as one would expect (`<$f>` is a term, so `/` and `//` are
operators):

```
$ printf 'hello\n' > /tmp/x
$ perl -e 'open(my $f,"<","/tmp/x"); my $s = 1 ? <$f> // "" : ""; print $s'
hello
```

**Impact on PCL: a DROPPED STATEMENT (#138 family) — and it was in our own
harness.** `perl-tests/t/test.pl`'s `runperl_and_capture` had two of them, so
every one of the 108 sweep files re-transpiled a file with two dropped
statements; it was the first thing the #472 child-drop instrument found.  The
`/ 2` spelling is worse: the swallowed text takes the rest of the statement
with it, so the drop message quotes code from three lines further down.

**Workaround (s446k):** `Pl::Parser2::_repair_readline_cascade`.  The cascade
cannot be undone token by token, so the repair works at SOURCE level like
`_rewrite_state_prepass`: it spells the diamond as the call perlop says it is
(`<$f>` → `readline($f)`) and reparses, after which PPI tokenizes the whole
statement correctly.  It fires only where the cascade actually happened — the
`<` must be in term position (`_ends_term`), the body must be a readline body
(a simple scalar or a bareword handle; `<>` is left alone) and a `Regexp` token
must follow the `>`.  `readline(...)` rather than `(<$f>)` because parenthesing
would create perl's `print (...) interpreted as function` gotcha for
`print <$f> // ""`.

**Repro + failing rows:** Bugs 13 and 14 in `docs/ppi-bug-report.t`.  Guard:
`Pl/t/readline-ternary-01.t` (the shape occurs in ZERO files of all four
in-repo populations, so those rows are the only guard there can be).

### 14c. THE CASCADE with a GLOB PATTERN — the `>` is swallowed too  (task #563)

§14b is the cascade when the body is a *readline* body, where the `>` still
stands as an operator and only the token after it is mis-lexed.  When the body
is a **glob pattern**, the `/` that starts the match is *inside* the diamond, so
the `>` goes with it — and the match then runs on to the next `/`, which is
normally lines away:

```
my @f = sort <./nope-*-xyz>;
  Word(sort) Operator(<) Operator(.) Regexp::Match(/nope-*-xyz>;)
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^ the `>` is in here

my @f = sort <./a-*>; print "y"; print "z";
  Word(sort) Operator(<) Operator(.) Regexp::Match(/a-*>; print "y"; print "z";)
```

and it does not stop at end of line: `sort <./a-*>;` followed by three more
statements is FOUR tokens, the last of them a Regexp holding all three.  perl
reads the diamond as a glob and the rest of the file as itself:

```
$ perl -e 'my @f = sort <./nope-*-xyz>; print "n=", scalar(@f), "\n"'
n=0
```

The trigger is the *first* character of the pattern: `.` and `~` are operators
to PPI, so a term is expected when the `/` arrives.  A pattern whose `/` count
happens to close the match survives (`<./x/*>`, `<./a/b/*>`), and one with no
leading `.` never derails at all (`<op/*.t>`) — which is why this went unnoticed
while §14's rebuild carried the family.

**Impact on PCL: an UNBOUNDED dropped statement (#138 family).**
`Pl::PExpr::_fix_ppi_glob_after_block` cannot reach it, because its rebuild
needs the `<` … `>` run to still be a run, and `_repair_readline_cascade`'s
body test is perlop's readline whitelist.

**Workaround (s449s):** `Pl::Parser2::_repair_glob_pattern_cascade`, which runs
FIRST of the repair block precisely because this damage is the widest.  It
rewrites the diamond into the `glob("PATTERN")` perlop says it is (`<…>` and
`glob` interpolate identically, so only a `"` needs escaping) and reparses.
Its conditions: the `<` is in TERM position, the first `>` is found *inside* a
`PPI::Token::Regexp` (when the `>` survived, the run was not derailed and the
existing rebuild owns it), and the pattern is one contiguous word.

**What it cost to make the term test honest** — two real gaps in PCL's
`_ends_term` oracle, both found by looking for the case the repair would
break, and both shared by the other repairs in this family: `$#a` is a
`PPI::Token::ArrayIndex`, not a Symbol, and a **deref block's** `}` is a
`PPI::Structure::Block`, not a Subscript (that second one was already known —
§12's repair works around it with a whitelist).  Without them
`$#a<3&&$s=~/a>b/` and `${$x}<3&&$s=~/a>b/`, both valid Perl, transpiled to
`$#aglob("3&&$s=~/a")b/`.

**Repro + failing rows:** Bugs 15 and 16 in `docs/ppi-bug-report.t`.  Guard:
`Pl/t/readline-ternary-01.t` (like §14b, ZERO sites in every population —
emission-ab over 921 files was SAME 921 — so the rows are the only guard).

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

## 18. `finally { … }` is not part of the `try` statement — and eats the next one  [CONFIRMED 1.291]

**Perl:** `try BLOCK catch (VAR) BLOCK finally BLOCK` (5.34's `use feature
'try'`, finally since 5.36) is ONE statement, and it self-terminates: no `;` is
needed after the closing brace, exactly like `if`/`while`.

**PPI:** with `use feature 'try'` in scope PPI knows the construct half way.  It
builds a `PPI::Statement::Compound` for `try {…} catch (VAR) {…}` and then stops
— `finally {…}` is left out, and because the orphan statement it starts is not
terminated, that statement SWALLOWS everything up to the next `;`:

```perl
use feature 'try';
try { foo(); } catch ($e) { bar($e); } finally { baz(); }
is($x, 1, 'desc');
```

```
PPI::Statement::Include     use feature 'try';
PPI::Statement::Compound    try { foo(); } catch ($e) { bar($e); }
PPI::Statement              finally { baz(); } is($x, 1, 'desc');   <- WRONG, two statements
```

Expected: one `PPI::Statement::Compound` holding try/catch/finally, then
`is($x, 1, 'desc');` as its own statement.

Two neighbouring facts, both measured on 1.291:

* **Without the pragma** the whole construct is one plain `PPI::Statement` that
  swallows the rest — but perl does not compile that file either, so PCL leaves
  it alone (principle 9: assume valid Perl input).
* **`use experimental 'try'`** — the other way real code enables the feature —
  does NOT switch PPI's `try` support on, so it lexes like the no-pragma case.

**PPI version tested:** 1.291.

**Repro + failing row:** Bug 15 in `docs/ppi-bug-report.t`.

**Impact on PCL: a whole statement disappeared.**  The assertion after a
`finally` block was swallowed into it and never ran — in `t/op/try.t` that is
several rows.  `Pl::Parser2::_repair_try_finally` terminates the orphan where
perl does (a `;` on the finally block's closing brace) and `_lower_block` joins
the `finally {…};` that is left back onto its `try` — the same route the
unlabeled `continue` block already takes.  Canary:
`Pl/t/misc-fixes-02.t`; end-to-end guard: `Pl/t/try-catch-01.t`.

## 19. A call to a sub named `x` after a list operator is lexed as the repetition operator  [CONFIRMED 1.291]

**Perl:** `x` is both the repetition operator and a legal sub name, and perl
decides by asking whether a complete TERM precedes it.  After a list operator
there is none, so the word is the call:

```perl
sub x { "PKG" }
print x(), "|\n";        # perl prints: PKG|
print x, "|\n";          # perl prints: PKG|   (no parens, same reading)
print STDOUT x(), "|\n"; # perl prints: PKG|   (a handle is not an operand either)
```

**PPI:** any Word before `x` counts as a term, so the operator reading wins:

```
PPI::Token::Word           [print]
PPI::Token::Operator       [x]        <- WRONG, should be Word
PPI::Token::Structure      [(]
PPI::Token::Structure      [)]
PPI::Token::Operator       [,]
PPI::Token::Quote::Double  ["|\n"]
```

Expected: `Word(print) Word(x) Structure(() Structure()) Operator(,) …` — which
is what PPI produces in every position where it expects a term, e.g. after `=`
(`my $r = x();`), after a comma, or after the `+` disambiguator
(`print +x(), …`).

**PPI version tested:** 1.291.

**Repro + failing row:** Bug 16 in `docs/ppi-bug-report.t`.

**Impact on PCL: SILENT WRONG, rc 0 (task #361).**  `print x(), "|\n"` compiled
to `(p-str-x (p-print $_) (progn))` — the print of `$_` repeated zero times.
Nothing was printed and nothing was announced.  `Pl::Parser2::_repair_word_x_call`
inserts perl's own disambiguator (a unary `+`, a documented no-op that PCL
already emits as a plain call) when the Word before `x` is not a DECLARED term —
the same condition `_repair_word_match` uses for `/PATTERN/` (§11), with the
ALL-CAPS shortcut dropped, because an ALL-CAPS word before `x` is a filehandle
rather than a constant.  Guard rows: `Pl/t/bareword-call-01.t`; canary:
`Pl/t/misc-fixes-02.t`.

### 19b. …and the same `x` STARTING a statement  [CONFIRMED 1.291, found s456ag]

A second position, the same misreading, and here there is no operator reading
available at all: `x` is INFIX, so a statement cannot begin with it.  PPI does
begin one with it whenever the previous token is a sub definition's closing
brace — and a bare `x` right after `sub x {…}` is precisely how such a sub is
called with neither parens nor arguments:

```perl
sub x { print "PKG\n" }   x         # perl prints PKG
my sub x { print "LEX\n" } x        # perl prints LEX  (t/op/lexsub.t writes this)
```

```
# PPI::Document->new(\'my sub x { 1 } x')->tokens
# PPI::Token::Word       my
# PPI::Token::Word       sub
# PPI::Token::Word       x
# PPI::Structure::Block  { 1 }
# PPI::Token::Operator   x     <-- WRONG: expected Word (a call)
```

A `;` after the sub's brace makes PPI lex the next `x` as a Word, which is the
control.

**Impact on PCL:** the statement was DROPPED ("Handle single node of unknown
type") — `t/op/lexsub.t` lines 448 and 836 are the `state sub` and `my sub`
spellings of it, one census drop each.  The same `_repair_word_x_call` handles
it, with a second arm whose discriminator is PPI's own TREE rather than the
previous token: the `x` must be the FIRST significant child of its Statement.
That is what keeps the genuine operator out — in `do { "a" } x 3` and
`map { … } x 3` the `x` sits in the MIDDLE of its statement, and those are
exactly the shapes a "previous token does not end a term" test would have
broken (`_ends_term` answers 0 for a `do`/`map` block's brace, correctly for
its other callers).  Guard rows: `Pl/t/method-name-word-01.t`.

---

## 20. Two of perl's three ways to enable `try` are not recognised, so the construct mis-lexes  [CONFIRMED 1.291]

**Perl:** three spellings enable 5.34's try/catch, and they are equivalent —
`use feature 'try'`, `use experimental 'try'` (which *is* `feature->import` plus
a warnings unimport), and the version bundles `use v5.40` / `use 5.040` (perl's
`:5.39` and `:5.40` bundles contain `try`; `:5.36` does not).

**PPI:** `PPI::Statement::Include::feature_mods` knows only the first.  For the
other two the lexer never switches `try` on, so the construct lexes like the
no-pragma case — ONE unterminated statement that swallows what follows:

```perl
use v5.40;
try { die "boom\n" } catch ($e) { print "caught: $e" }
print "after\n";
```

```
PPI::Statement::Include     use v5.40;
PPI::Statement              try { … } catch ($e) { … } print "after\n";   <- WRONG
```

Expected: a `PPI::Statement::Compound` for the try/catch, then `print "after\n";`
as its own statement — which is exactly what PPI produces for the same file
with `use feature 'try';` on the first line.

Two separate defects behind it, both in `feature_mods`:

* **the version hack.**  Its comment says installing a future `feature.pm` is
  impossible, so it hard-codes `signatures` for `>= 5.035` and stops.  No bundle
  ever enables `try`.
* **the `experimental` branch** answers `{ signatures => … }` and nothing else —
  so `use experimental 'try'` not only fails to enable `try`, it returns
  `signatures => 0` and *disables* signatures.  It also ignores `use` vs `no`,
  so `no experimental 'signatures'` reads as an enable.

**PPI version tested:** 1.291.

**Repro + failing rows:** Bug 17 in `docs/ppi-bug-report.t`.

**Impact on PCL: a whole statement disappeared** — `use v5.40; try {…}` was an
announced DROP at rc 0, and `use experimental 'try'` did not compile at all.
PPI has the hook for it (`custom_feature_include_cb`, consulted BEFORE the
built-in logic), so the fix is a table rather than a source rewrite:
`Pl::Parser::_pcl_feature_include_cb` answers all three spellings and the `no`
forms, with the bundle thresholds taken from perl's own `%feature::feature_bundle`.
Guard: `Pl/t/feature-pragma-01.t` (which also re-derives the thresholds from the
running perl, so a perl that changes them fails a row).

---

## 21. A term-initial `~~` is lexed as the smart-match operator  [CONFIRMED 1.291]

**Perl:** `~~` is the smart match only where an OPERATOR may stand.  Where a
TERM is expected it is two complements, `~(~$x)` — the classic "numify /
truncate to integer" idiom, which perl-tests/bop.t and t/op/bop.t both assert:

```perl
my $y = 3;
print ~~$y, "\n";     # perl prints: 3
is(~~$y, 3);          # bop.t:196
is(~~$y, "c");        # bop.t:285, on a string
```

**PPI:** one `~~` token, always the operator:

```
PPI::Token::Word           [is]
PPI::Token::Structure      [(]
PPI::Token::Operator       [~~]      <- WRONG: two Operator [~] are meant here
PPI::Token::Symbol         [$y]
PPI::Token::Operator       [,]
PPI::Token::Number         [3]
PPI::Token::Structure      [)]
PPI::Token::Structure      [;]
```

Expected: `… Structure(() Operator(~) Operator(~) Symbol($y) …`, which is what
PPI produces for `~ ~$y` (with a space) and for every other prefix operator in
that position.  The decision is the same one PPI already makes correctly for
`x` (§19) and `/` (§11): does a complete TERM precede the token?

**PPI version tested:** 1.291.

**Repro + failing row:** Bug 20 in `docs/ppi-bug-report.t`.

**Impact on PCL (task #370): the statement was DROPPED WHOLE.**  The main loop
saw a binary operator with no left operand ("Fell through. Missing case"), so
two rows of bop.t vanished in each population — silently, until the drop
announcement (#339) started naming them.  `Pl::Parser2::_repair_term_initial_complement`
splits a `~~` whose previous significant token does not satisfy `_ends_term`
into two `~` tokens, and reparses; an INFIX `~~` is left alone, and PCL then
refuses it perl-shaped (smart match was removed in perl 5.42 — task #371,
`docs/not-supported.md`).  Guard rows + canary: `Pl/t/misc-fixes-02.t`.


## 22. A filetest after a SCALAR filehandle is split into `-` + WORD  [CONFIRMED 1.291]

**Perl:** in `print FILEHANDLE LIST` the handle may be a bareword, a scalar or
a block, and a leading `-X` in the LIST is one filetest operator in all three.
`perl -MO=Deparse` agrees, and it distinguishes the spaced form:

```perl
print $fh -e $f;      # deparse: print $fh -e $f       (one filetest)
print $fh - e $f;     # deparse: print $fh -(e($f))    (minus, then a call)
```

Adjacency is the discriminator, and perl honours it.

**PPI:** the BAREWORD handle lexes correctly and the SCALAR/BLOCK handle does
not — the same `-e`, two answers:

```
# PPI::Document->new(\'print STDERR -e $f;')->tokens   -- CORRECT
# PPI::Token::Word          print
# PPI::Token::Word          STDERR
# PPI::Token::Operator      -e         <-- one filetest operator
# PPI::Token::Symbol        $f

# PPI::Document->new(\'print $fh -e $f;')->tokens      -- WRONG
# PPI::Token::Word          print
# PPI::Token::Symbol        $fh
# PPI::Token::Operator      -          <-- expected Operator '-e'
# PPI::Token::Word          e          <--
# PPI::Token::Symbol        $f
```

`print {$x} -e $f;` splits the same way.  The rule PPI appears to apply is
"after a scalar variable a `-` must be binary minus", which is wrong here
because the scalar is a FILEHANDLE, not a term — and, notably, `-e` cannot be
a binary operator anywhere: `$n -e $b` is a perl SYNTAX ERROR (`syntax error
near "$n -e "`), so there is no competing reading to protect.  PPI already
makes exactly this term-or-not decision correctly for `x`, for `/PATTERN/`
and for `~~` after a bareword handle.

**Impact on PCL (task #372): silent-wrong, then a crash.**  PCL lowered
`print $fh -e $f` to `(p-print (p-- $fh (pl-e $f)))` — a subtraction of a call
to a sub named `e`, printed to STDOUT instead of to `$fh` — which dies at load
with "The function main::pl-e is undefined" when no such sub exists.
`Pl::PExpr::_fuse_print_filehandle_filetest` fuses the two tokens back into
the `-X` Operator when they are ADJACENT (`next_sibling`, not
`snext_sibling`), the head word is `print`/`printf`/`say`, the next element is
a scalar or block filehandle, and the letter is in PExpr's `prefix` table — so
the spaced form keeps its correct reading.  Guard rows:
`Pl/t/filetest-stack-01.t`.

---

## 23. A `$` scalar with a NON-ASCII name is split into Cast + Word — but `@`/`%`/`*`/`&` are not  [CONFIRMED 1.291]

Perl has allowed unicode identifiers since 5.8 (`use utf8`), and PPI reads them
correctly for every sigil except `$`:

```perl
perl -MPPI -CS -e 'use utf8; for my $c (q{$Ｘ}, q{%Ｘ}, q{@Ｘ}, q{*Ｘ}, q{&Ｘ}) {
  my $d = PPI::Document->new(\$c);
  print "$c => ", join(" ", map { ref($_) . "[" . $_->content . "]" } $d->tokens), "\n" }'

$Ｘ => PPI::Token::Cast[$] PPI::Token::Word[Ｘ]        <-- WRONG
%Ｘ => PPI::Token::Symbol[%Ｘ]
@Ｘ => PPI::Token::Symbol[@Ｘ]
*Ｘ => PPI::Token::Symbol[*Ｘ]
&Ｘ => PPI::Token::Symbol[&Ｘ]
```

perl reads all five as variables (`$Ｘ = 1; print $Ｘ` prints 1).

**The line.**  `PPI/Token/Unknown.pm`, the `$` branch of
`__TOKENIZER__on_char`:

```perl
} elsif ( $c eq '$' ) {
        ...
        if ( $char =~ /[a-z_]/i ) {            # <-- ASCII-only
                # Symbol
                $t->{class} = $t->{token}->set_class( 'Symbol' );
```

Every sibling branch in the same function (`*`, `%`, `&`, `@`) tests
`/[\w:]/`, which matches a unicode word character, and
`PPI::Token::Symbol::__TOKENIZER__on_char` then consumes the rest of the name
with `m/\G([\w:\']+)/gc` — also unicode-clean.  So the name only has to survive
the first character test; `$` is the one sigil that does not use it.  (This is
also why `$$Ｘ` works: the inner symbol is created by the `^\$\$\w` branch of
`PPI::Token::Magic`, not by this test.)

The proposed fix is to make the `$` branch match its siblings — `/[\w:]/`, with
the existing "not after a Number" guard the others carry.

**Second-order damage: the LEXER has already chosen.**  Merging the two tokens
back into one Symbol is not enough, because by then the structure that follows
was built against the bareword:

```
$Ｘ{a}   ⇒  Symbol($Ｘ)  PPI::Structure::Block        { a }        <-- expected Subscript
$Ｖ[0]   ⇒  Symbol($Ｖ)  PPI::Structure::Constructor  [ 0 ]        <-- expected Subscript
$Ｘ->{a} ⇒  Symbol($Ｘ)  Operator(->)  PPI::Structure::Subscript   <-- correct
```

so the enclosing statement is not a subscripted variable at all.

**Impact on PCL (task #410): 21 dropped statements** across `t/uni/gv.t`,
`t/uni/stash.t`, `t/uni/caller.t`, `t/uni/method.t`, `t/uni/readline.t`,
`t/mro/basic_utf8.t` and `t/mro/package_aliases_utf8.t` — every one of them a
`$NONASCII{…}` hash/stash element or a `$NONASCII[…]` array element.
`Pl::Parser::_merge_unicode_symbols` (which already merged the tokens) now also
re-classes the postfix chain that follows a repaired symbol: the `{…}`/`[…]`
containers become `PPI::Structure::Subscript` and their inner `PPI::Statement`
an `Expression`, which is what the lexer would have chosen had the symbol been
whole.  Text and tokens are untouched; an explicit `->` is stepped over because
PPI already got that one right.  Guard row: `Pl/t/utf8-source-01.t`.

**Addendum (s421): the LEXER fails the WHOLE document on a non-ASCII foreach
loop variable.**  `for my $Ｉ (1,2) { 1 }`, `foreach my $Ｉ (…)`, `for $Ｉ (…)`,
`for my $Ｉ(…)` and `for my $Ｉ (@a)` all return undef from
`PPI::Document->new` with `errstr` "Lexer failed: Illegal state in 'foreach'
compound statement" (1.291), while `for (my $Ｉ=0; $Ｉ<2; $Ｉ++)`, `while (my $Ｉ =
shift)` and `my $Ｉ = 1;` lex fine — the foreach-slot state machine expects a
Symbol where the `$` split has put a Cast.  PCL cannot repair this at the
token level (there are no tokens); it dies "PPI failed to tokenize".  Corpus
cost: 0 files (only t/lib/warnings/pad, a data file).  Task #422 item 3;
row in `docs/ppi-bug-report.t`.

---

## 24. A PUNCTUATION-named array `@?` is split into Cast + Operator  [CONFIRMED 1.291]

perl lets any punctuation character name a global, and real code uses it —
`t/re/subst.t:346` is `ok( ! @?, 'parsing of split subst with comment' );`.
PPI has `PPI::Token::Magic` entries for the arrays perl documents (`@-`, `@+`,
`@*`, `@_`, `@ARGV`, …) and its `Symbol` name regex is word-bounded, so every
other punctuation name falls through to the `@` CAST branch and the character
after it is tokenized on its own:

```perl
perl -MPPI -e 'for my $c (q{@?}, q{@!}, q{@.}, q{@-}, q{@+}) {
  my $d = PPI::Document->new(\"my $n = scalar($c);");
  print "$c => ", join(" ", map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                            grep { $_->significant } $d->tokens), "\n" }'

@? => ... Cast[@] Operator[?] ...      <-- WRONG
@! => ... Cast[@] Operator[!] ...      <-- WRONG
@. => ... Cast[@] Operator[.] ...      <-- WRONG
@- => ... Magic[@-] ...                (correct — it is in %MAGIC)
@+ => ... Magic[@+] ...                (correct — it is in %MAGIC)
```

perl accepts all of them: `@? = (1,2); print scalar(@?)` prints 2, and so does
every punctuation character except `@+`/`@-` (read-only) and `@{` (a syntax
error).  Probed on perl 5.40.3.

**Why it is a bug and not a missing feature.**  A `@` Cast is only ever
followed by `$`, `{` or an identifier in valid perl, so `Cast + Operator` is
not a parse of anything — the token stream describes no legal program.  The
same holds for the `%` sigil, where PPI does not even produce a Cast: `%?`
comes out as `Operator[%] Operator[?]`, i.e. a modulus.

The proposed fix is the one `%MAGIC` already implies: after the sigil, accept
a single punctuation character as the whole name (which is exactly perl's own
rule in `toke.c`'s `scan_ident`), rather than enumerating the documented
names.

**Impact on PCL (task #415): one dropped statement** (`t/re/subst.t:346`) plus
a LATENT CRASH the same family caused without any PPI bug at all — `$?[1]` is
element 1 of `@?`, and PCL lowers it to `(p-aref @? 1)` through the machinery
that also serves `$#[0]`/`@#`, but only `@#` was ever forward-declared, so a
file containing `$?[…]` died at load with an unbound variable.
`Pl::Parser::_merge_punct_array_symbols` merges the two tokens back into one
`PPI::Token::Symbol`, and Parser2's punctuation forward-declaration bucket now
covers the whole family instead of `@#` alone.  Guard rows:
`Pl/t/punct-array-glob-01.t`.

### 24b. The `%` twin, and why it needed a POSITION rule  (tasks #550 + #449, s449s)

The array repair above was first limited to the punctuation characters that are
CL symbol CONSTITUENTS (`? ! . / ~ ^ & % = < >`); the rest (`@,`, `@|`, `@@`,
`@\`) were left dropping as task #449 because a bare `@,` is not a CL symbol at
all — `,` ends a token, `|` opens a quoted one — so the emitted file failed to
READ.  That is now `Pl::CLForm::needs_pipes`' second arm: a name carrying a
CL-unsafe character is pipe-quoted, exactly as a non-ASCII one is (#418).  It
cannot collide with the "ASCII stays bare" half, because no perl identifier,
package name or bareword can contain those characters — the only names it
reaches are punctuation variables, which carry no letters.

**The HASH twin is a different repair, and PPI's own output says why:**

```
my @k = keys %?;    Word(keys) Operator(%) Operator(?)     <- no Cast at all
%? = (a => 1);      Operator(%) Operator(?) Operator(=) …
@? = (1, 2);        Cast(@)     Operator(?) …
```

An `@` Cast can only ever be a sigil, so the array repair decides on ADJACENCY
alone.  `%` is also the MODULO operator, and PPI hands the sigil over as a bare
`Operator` when it reads it that way, so the hash repair needs perl's rule for
which one it is — and perl's rule is POSITION, `%` opening a hash where a TERM
is expected.  It is not the obvious answer (probed 5.40.3):

```
$ perl -e 'sub f { 7 } print f % 3, "\n"'
7$                       # `%3, "\n"` is the HASH %3, passed to f
$ perl -e 'sub f { 7 } print f() % 3, "\n"'
1                        # after `)` a term has ended: modulo
```

**Workaround (s449s):** `Pl::Parser2::_repair_punct_hash_name` (plus its Cast
sibling for `%%` / `%*`), keyed on `_ends_term` and the declared-term test for
a Word — the same pair §14c's repair uses for the term-position `<`.  The
CHARACTER SET is the array repair's (`Pl::Parser::punct_container_chars`): one
set, two arms, this file's half supplying the position.  The rewrite is
TEXTUAL, into perl's own BLOCK spelling of a name — `%?` → `%{?}` — for two
reasons: a repair in Parser2's chain is undone by the next repair's
`_reparse_doc` unless it changes the TEXT (measured), and perlref's `%{ NAME }`
is the VARIABLE, so the block form is legal under `use strict 'refs'` where the
symbolic `%{'?'}` would die.

What still drops is decided by PPI, not by CL, and each was measured one
character at a time: `" ' \` #` (the token PPI built SWALLOWED the rest of the
line), `$` (the deref spelling; PPI hands `$;` over as Magic), `( ) [ ] ;` (a
`Token::Structure` the lexer has already built a Structure around), `%/` (PPI
derails it into a Regexp) and `%@` / `%\` (PCL drops the `%{@}` target).  `@:`
and `%:` are a different bug — PPI makes those a Symbol, so they REACH emission
and fail there.  All of it is task #653.

---

## 25. `-name` after a token that ENDS A TERM is lexed as one negative-bareword Word  [CONFIRMED 1.291]

The third sibling of §12 (`)*name` read as a glob) and §15 (`)-1`), and the one
that was still unrepaired when the s435 announce→DIE flip made it fatal.

**Perl:** after a `)` a term has ENDED, so `-` is binary minus.  The
negative-bareword string form (`-foo` ⇒ `"-foo"`) can only start where a TERM
can.  `perl -MO=Deparse` on the reproducer gives
`length('abc') - length('a')`.

```perl
my $z = length("abc")-length("a");   # perl: 2
```

**PPI:** the `-` is glued to the following identifier and the pair becomes one
`PPI::Token::Word`:

```
# PPI::Document->new(\'my $z = length("abc")-length("a");')->tokens  -- WRONG
# PPI::Token::Word          length
# PPI::Token::Structure     (
# PPI::Token::Quote::Double "abc"
# PPI::Token::Structure     )
# PPI::Token::Word          -length     <-- expected Operator('-') Word('length')
# PPI::Token::Structure     (
# PPI::Token::Quote::Double "a"
# PPI::Token::Structure     )
```

A SPACE fixes it (`) - length(`), and a NUMBER on the right is already handled
(`)-1` is §15).  PPI makes the same operator-vs-term decision correctly for
`x` after a list operator and for `/PATTERN/` after a bareword, so the machinery
exists — this position is simply not asking it.

**Impact on PCL (task #457): silent-wrong, then an unloadable module.**  PExpr
has no case for `Word(-length) List`, so the WHOLE STATEMENT was dropped (#138
family).  Measured over five populations: **zero** sites in `lib/` (22 files),
`cpan-tests/modules` (402), `perl-tests` (111) and perl's own `t/` (604), and
**two** in one board dist — `Text-Balanced-2.07-0/lib/Text/Balanced.pm` lines
118 and 397:

```perl
$escs .= substr($escs,-1) x (length($dels)-length($escs));   # line 118
$closetagpos = pos($$textref)-length($1);                    # line 397
```

Line 118 is inside `gen_delimited_pat`, which the module's own top level CALLS
at line 308 — so once a dropped statement started DYING when reached (s435),
`use Text::Balanced` died and the dist went from **958 passing rows to zero**.
Before the flip the same bug was silent: `$escs` kept its initial `'\\'` and
the generated pattern was quietly wrong.

`Pl::Parser2::_repair_minus_word` splits the token back into `-` + WORD on the
raw stream when the previous significant token `_ends_term`, and reparses —
the same predicate and the same shape as `_repair_glob_multiply`.  The
condition is a NEGATIVE, which is what makes it safe: `(-f => 4)`,
`foo(-bar)`, `$h{-key}` and `1, -bar` all follow `(`, `{` or `,`, none of which
ends a term (all probed against perl).  Guard rows: `Pl/t/minus-word-01.t`.

---

## 26. A glob whose NAME is punctuation or a digit run is split into two operators  [CONFIRMED 1.291]

**Perl:** a glob is named by whatever names a variable, and punctuation and
digits name variables (`$-`, `$!`, `$1`), so `*-`, `*!` and `*1` are globs.
Real perl test code writes them:

```
*X = *-;            t/re/reg_namedcapture.t:18   makes %X the named-capture hash
*Y = *!;            t/re/reg_namedcapture.t:25   makes %Y the errno hash
local *a = *1;      t/re/subst.t:951             makes $a the first capture
local *1 = sub {…}; t/op/method.t:38             installs main::1
```

**PPI:** `PPI::Token::Symbol`'s name is word-bounded, so only `*word` is a
Symbol.  The rest come out as two ordinary tokens:

```
our %X; *X = *-;      Symbol(*X) Operator(=) Operator(*) Operator(-)
local *a = *1;        Word(local) Symbol(*a) Operator(=) Operator(*) Number(1)
*^R = *g;             Operator(*) Operator(^) Word(R) Operator(=) Symbol(*g)
```

This is the same class as §24 (`@?` → Cast + Operator), one sigil over — but
NOT the same repair, because `*` is also multiplication and a glob pattern's
metacharacter, so a `Cast` never appears and adjacency cannot decide it.

**Impact on PCL: the statement was DROPPED** (#138 family), with the compiler's
own message `Got op '-', not postfix.  But there is nothing after it??` for the
punctuation forms and `Bug. Fell through. Missing case: []` for the digits.

**Workaround (s446k, task #463):** `Pl::Parser2::_repair_punct_glob_name`
rewrites the name into the symbolic spelling the compiler already lowers —
`*-` → `*{'-'}` — and reparses.  The two are the same glob in perl even inside
a package (probed: both reach the forced-`main` punctuation globals).  Its
condition is a WHITELIST of the positions where a `*` can OPEN a glob name
(statement/list start, after `=`, `,` or `return`), not `_ends_term`'s
negative, because a false positive turns working multiplication into a glob:
measured over all four populations, 1329 files hold 23 term-position `*` sites
and only 8 are globs — the rest are glob PATTERNS inside a `<…>` run PPI
derailed (§14), regex bodies in a file PPI mis-lexed whole, and `@{$h} * (…)`,
where `_ends_term` itself wrongly says the term has not ended because a deref
block's `}` is not a subscript's.

**The two remaining spellings, covered s448o (task #562)** — same PPI bug, two
more token shapes, repaired in the same walk:

```
*^R = *g;             Operator(*) Operator(^) Word(R) Operator(=) Symbol(*g)
tie my $v=>'m', *];   … Operator(,) Operator(*) Structure(])
```

* **`*^R` is three tokens, and the name is NOT `^`.**  perl's caret convention
  means the glob named chr(18) — the one `$^R` reads — so `*{'^'}R` would have
  been a silent wrong.  The repair emits the CONTROL CHARACTER, `*{'<chr18>'}`,
  which is what perl means by it (probed: `*^R = *g` aliases `$^R` to `$g`;
  `${"\cR"}` IS `$^R` while `${"^R"}` is a different variable; `"" . *^R` is
  `*main::` followed by chr(18)).  Only a Word of ONE upper-case letter is
  taken: PPI hands the whole following word over as one token, so `*^Rfoo` is
  `Word("Rfoo")` — `*^R` followed by `foo`, not a glob name — and a longer Word
  keeps dropping loudly.
* **`*]` needs a Structure-aware arm, and PPI itself supplies the
  discriminator.**  A bracket that closes a real subscript, list or constructor
  is the `finish` of a `PPI::Structure`; an orphan is parked in a
  `PPI::Statement::UnmatchedBrace`.  Measured over subscripts, slices, anon
  constructors, list slices and `($x)[0]`, that test separates them exactly.  A
  `(*)` PROTOTYPE never reaches the arm at all — PPI lexes it as one
  `Token::Prototype`.  Only CLOSERS (`]`, `)`, `}`) are in the set: `*{` is the
  deref-block spelling and must never be claimed, an opener is never orphaned
  so the test could not protect it, and deleting a `;` would take the statement
  terminator with it.

### 26b. A SIGIL-named glob is two CASTS, not two operators  (task #564, s449s)

The third token shape, and the one the two arms above could not reach, because
neither token is an Operator:

```
local *@;             Word(local) Cast(*) Cast(@)          perl-tests/local.t:828
*@ = *x;              Cast(*) Cast(@) Operator(=) Symbol(*x)
my $z = *@;           … Operator(=) Cast(*) Cast(@)
```

`$@`, `$$`, `$%` and `$&` are variables, so `*@`, `*$`, `*%` and `*&` are the
globs that hold them — but `@`/`$`/`%`/`&` are also sigils, so PPI classifies
each as a `Token::Cast` rather than an Operator, and the `*` in front of one
becomes a Cast too.

**Impact on PCL: the statement was LOST, silently** — `local *@;` fell through
every branch of `_process_local_declaration` to a bare `return`.

**Workaround (s449s):** the same walk, one more arm.  Its extra condition is
the token AFTER the second Cast: a real deref cast applies to *something* (a
Symbol, a `{` block, another Cast), while a glob name is followed by whatever
ends or continues the statement — so `@$r`, `%$h`, `@{$r}` and `&$cr` cannot be
claimed.  The lowering the repaired `*{'@'}` reaches is `p-local-glob-dynamic`
(task #564), which is why the `local` position could only be whitelisted once
that existed.

**Repro + failing rows:** Bugs 15 and 16 in `docs/ppi-bug-report.t`.  Guard:
`Pl/t/punct-glob-name-01.t`.

---

## 27. `->symbol` answers `%x` for the `$x` in `*$x{SLOT}` — the `*` cast is missing from its "cast trumps braces" set  [CONFIRMED 1.291]

**Scope note:** this is not a mis-TOKENIZATION — PPI's token stream for
`*$x{SCALAR}` is right (`Cast(*) Symbol($x) Structure::Subscript({SCALAR})`).
It is PPI's own documented ANALYSIS method giving a wrong answer for valid
Perl, and the fix upstream is one token, so it is worth reporting here rather
than filing as a PCL-only task.

**Perl:** `*$x{SCALAR}` is the glob-slot syntax — `*{$x}{SCALAR}` — whose
operand is the **scalar** `$x`.  The braces are the slot name, not a hash
subscript.  Real code writes it:

```
*$_{HASH}         lib/5.40.3/Carp.pm:34, 122, 124   (five sites in one core module)
${*$_{SCALAR}}    lib/5.40.3/Carp.pm:127
*$fh{IO}          t/op/filetest.t:313, t/op/stat.t:485
*$handleref{NAME} t/op/gv.t:1100
${*$a{SCALAR}}    t/uni/parser.t:75, 76
```

`*$x[0]{…}` is not valid Perl at all (`syntax error … near "$a["`), so braces
after a `*` cast are never an element access.

**PPI:** `PPI::Token::Symbol::symbol` documents itself as returning "the ACTUAL
symbol this token refers to" and resolves `$foo{…}` to `%foo` — correctly
skipping that resolution when a cast trumps the braces.  Its cast set omits
`*`:

```perl
my %cast_which_trumps_braces = map { $_ => 1 } qw{ $ @ % };   # PPI/Token/Symbol.pm
```

Measured on 1.291 (`$doc->find('PPI::Token::Symbol')`, `->symbol` per token):

| source           | `->symbol` | perl means |
|------------------|-----------|------------|
| `$x = *$a{SCALAR};` | `%a`  | `$a`  ← **wrong** |
| `$x = *$a{ARRAY};`  | `%a`  | `$a`  ← **wrong** |
| `$x = $$a{k};`      | `$a`  | `$a`  (cast `$` trumps) |
| `$x = @$a{'k','j'};`| `$a`  | `$a`  (cast `@` trumps) |
| `$x = $a{k};`       | `%a`  | `%a`  |
| `$x = ${$a}{k};`    | `$a`  | `$a`  |

**Fix upstream:** add `*` to `%cast_which_trumps_braces`.  (`&` already returns
early, one line above.)

**Impact on PCL: two independent failures, one of them a SILENT WRONG** (task
#663).  `->symbol` is the one question ~40 sites in the compiler ask a Symbol
token — "which variable is this".  (a) The `my $a`/`my $b` exception rename
skips a token whose `->symbol` is a different variable, so `*$a{SCALAR}` kept
reading the never-assigned package global `$a`: the whole top-level form died
`Can't use an undefined value as a symbol reference` and `t/uni/parser.t` lost
35 rows of coverage (18/5 with one aborted form → 28/30, 58 rows).  (b) With a
MAGIC symbol it reached the expression compiler: `*$_{HASH}` emitted
`(p-dynamic-typeglob (p-gethash %_ "HASH"))` — a typeglob of the hash element
`$_{HASH}` of a phantom `%_` — where perl reads the glob's HASH slot.  That one
is live in core `Carp.pm`.

**Workaround (s451w):** `Pl::Parser::_brace_glob_slot_symbol`, one pass in
`_ppi_parse`'s repair group, wraps the Symbol's TEXT in braces so the reparse
yields `*{$x}{SLOT}` — the spelling PCL's machinery already consumes,
emission-identical, and a Symbol PPI can no longer mis-canonicalise because the
subscript is no longer its next sibling.  It keys on `Token::Cast` eq `*`, and
PPI itself separates the dangerous case: in `8 *$Config{sizesize}` (a real line
in `perl-tests/vec.t`) the `*` is an `Operator`, not a `Cast` — verified after a
call, a subscript, a hash element, a string, a number and a paren.

**Repro + failing rows:** Bug 27 in `docs/ppi-bug-report.t`.  Guard:
`Pl/t/glob-slot-operand-01.t`.

---

## 28. `$x.2` — a `.` followed by a digit after a term is lexed as a Number::Float, so the concatenation OPERATOR disappears  [CONFIRMED 1.291]

The fourth sibling of §12 (`)*name`), §15 (`)-1`) and §25 (`)-name`): the same
operator-vs-term decision, in the one position PPI still gets wrong.

**Perl:** `perlop` and `toke.c` agree — a number may START at `.` only where a
TERM is expected.  After a complete term `.` is the concatenation operator, so
`$_.2` is `$_ . 2`.  `perl -MO=Deparse -e 'my $x = $_.2'` prints
`my $x = $_ . 2;`.

```perl
local $_ = "a";  my $x = $_.2;   # perl: x is "a2"
my $y  = "b";    my $z = $y.5;   # perl: z is "b5"
```

**PPI:** there is no `.` operator in the stream at all — the dot is absorbed
into the number:

```
# PPI::Document->new(\'my $x = $_.2;')->tokens  -- WRONG
# PPI::Token::Word          my
# PPI::Token::Symbol        $x
# PPI::Token::Operator      =
# PPI::Token::Magic         $_
# PPI::Token::Number::Float .2        <-- expected Operator('.') Number('2')
# PPI::Token::Structure     ;
```

A space (`$_ . 2`) or a non-digit (`$_."2"`) makes PPI lex it correctly — the
bug needs the digit adjacent to the dot.  Every term-ending token on the left
triggers it: `$x.2`, `$a[1].3`, `$h{k}.4`, `f().6`, `$#a.7`, `"r".8`,
`$r->[0].1`.

**Impact on PCL (task #480):** the term walker sees SYMBOL NUMBER juxtaposed
with no operator between them and DROPS the whole statement ("Bug. Fell
through. Missing case: ['Token::Magic<\$_>','Token::Number::Float<.2>']") — the
#138 family, fatal since the s435 announce→DIE flip.  Found by the s438 cpan-`t`
census population in `Text-CSV-2.04/t/78_fragment.t:101`, the only site in the
six census populations.

`Pl::Parser2::_repair_dot_number` splits the token back into `.` + NUMBER on
the raw stream when the previous significant token `_ends_term`, and reparses —
the same predicate and shape as `_repair_minus_word`.  The condition is the
family's NEGATIVE, which is what makes it safe: `= .5`, `return .5`,
`(a => .5)`, `[.5]`, `f(.5)`, `1, .5` and `-.5` all follow a token that does
not end a term (all probed against perl).  ONE position needs an exception the
shape oracle cannot see — a `print`/`printf`/`say` FILEHANDLE is shaped like a
term but STARTS the argument list, so `print $fh .5` writes the number `0.5`
(probed); `Pl::Parser2::_is_print_filehandle_slot` is that exception.  Guard
rows: `Pl/t/minus-word-01.t`.

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
