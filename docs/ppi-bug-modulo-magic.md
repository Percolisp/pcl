# PPI bug: `7%-3` mis-tokenized as the magic hash `%-`

**PPI 1.291, perl 5.40.3**

`7%-3` (modulo with a negative operand, no spaces) is tokenized as the magic
hash `%-`, losing the `%` operator. `%+` (`7%+3`) has the same problem.

```perl
use PPI;
my $d = PPI::Document->new(\'7%-3');
printf "%-20s %s\n", ref($_), $_->content for grep { $_->significant } $d->tokens;
```

Actual vs. expected (the latter is what PPI already gives for the spaced `7 % -3`):

```
PPI::Token::Number  7        PPI::Token::Number    7
PPI::Token::Magic   %-   →    PPI::Token::Operator  %
PPI::Token::Number  3        PPI::Token::Number    -3
```

`%-`/`%+` are *hashes*: they only appear in term position, never right after a
term. So when `%` follows a term it must be the modulo operator. Perl agrees:
`perl -e 'print 7%-3'` → `-2`.

### Failing test

```perl
use Test::More tests => 1;
use PPI;
my @sig = map { ref . '=' . $_->content }
          grep { $_->significant } PPI::Document->new(\'7%-3')->tokens;
ok !grep(/Magic=\%-$/, @sig), '7%-3 is "7 % -3", not the magic hash %-'
    or diag "got: @sig";
```
