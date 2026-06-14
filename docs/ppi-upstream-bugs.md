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

## 3. `< EXPR >` comparison chain misread as a glob/readline  [NEEDS RE-CONFIRM]

**Reported earlier** (PCL `docs/ppi-glob-disambiguation.md`): a chain like
`$x->[0] < $x->[1] > $x->[2]` was misread by PPI as a `<...>` readline/glob,
silently dropping/garbling the statement.

**Status on PPI 1.291:** could NOT reproduce — the following all tokenize
correctly (zero `PPI::Token::QuoteLike::Readline`):

```perl
my $r = $x->[0] < $x->[1] > $x->[2];
print $a < $b > $c;
my $s = join(",", "x", $_->[0] < $_->[1] > $_->[2], "y");
```

Likely fixed in a PPI release after the one PCL's doc was written against. **Do
NOT file** until a current-PPI repro is found; if PCL still trips on a specific
form, capture that exact source first.

---

## How to add to this list

When PCL hits a parse problem, first check whether **PPI** mis-tokenizes it
(dump `PPI::Document->new(\$src)` and inspect the token stream) vs. whether PCL's
PExpr just interprets a correct token stream wrongly. Only the former belongs
here. Include: minimal repro, the PPI token dump, expected tokens, PPI version,
and the PCL-side workaround/affected tests.
