#          -*-Mode: CPerl -*-

# #153 / E5.0 step 1: unit tests for _term_extent / _reduce_term — the ONE
# term-grammar walker (Option B phase 1, docs/pexpr-term-parsing-review.md).
#
#   term := cast* primary postfix*
#
# The walker returns the inclusive end index of the term starting at a given
# index, or undef when it cannot bound the term confidently (bare words,
# prefix operators, method calls with args).  undef is a first-class answer:
# call sites keep their legacy derivation for those shapes.
#
# These tests pin the extent per the grammar, straight off PPI token arrays —
# no transpile, no SBCL, so the file is fast.

use v5.32;
use strict;
use warnings;

use lib ".";

use PPI;
use Test::More;

BEGIN { use_ok('Pl::PExpr') };

my $o = Pl::PExpr->new(e => []);

# Tokenize CODE and return the significant children of its first statement,
# with a trailing ';' stripped — the same shape the parse loop works on.
# GOTCHA: the PPI::Document must be kept ALIVE — its recursive DESTROY tears
# down the token tree, leaving the returned tokens hollow (content() undef).
my @keep_docs;
sub toks {
  my ($code) = @_;
  my $doc = PPI::Document->new(\$code) or die "PPI failed on: $code";
  push @keep_docs, $doc;
  my ($stmt) = grep { $_->isa('PPI::Statement') } $doc->children;
  die "no statement in: $code" if !$stmt;
  my @sig = grep { $_->significant } $stmt->children;
  pop @sig if @sig && ref($sig[-1]) eq 'PPI::Token::Structure'
           && $sig[-1]->content eq ';';
  return \@sig;
}

# Assert the term starting at index START of CODE's token array ends at WANT
# (inclusive index), or that the walker declines (WANT undef).
sub extent_is {
  my ($code, $start, $want, $desc) = @_;
  my $e   = toks($code);
  my $got = $o->_term_extent($e, $start);
  is($got, $want, $desc // sprintf("%s @%d -> %s",
                                   $code, $start, $want // 'undef'));
}

# --- simple primaries ---------------------------------------------------
extent_is('$x;',           0, 0);
extent_is('$x + $y;',      0, 0, 'term stops before binary op');
extent_is('42;',           0, 0);
extent_is('3.14;',         0, 0);
extent_is('"str";',        0, 0);
extent_is("'str';",        0, 0);
extent_is('qw(a b);',      0, 0);
extent_is('qr/x/;',        0, 0);
# At statement level PPI tokenizes <STDIN> as three Operator/Word tokens, so
# the walker declines; in expression position it is one QuoteLike::Readline.
extent_is('<STDIN>;',          0, undef, 'statement-level <STDIN>: declines');
extent_is('my $l = <STDIN>;',  3, 3,     'readline in expr position is a term');
extent_is('$_;',           0, 0, 'magic var');
extent_is('$#foo;',        0, 0, 'arraylen');

# --- subscript chains ---------------------------------------------------
extent_is('$h{k};',            0, 1);
extent_is('$a[0];',            0, 1);
extent_is('$h{a}{b}[0];',      0, 3);
extent_is('$h{a}->{b}->[0];',  0, 5);
extent_is('$r->{k};',          0, 2);
extent_is('$r->[0]{x};',       0, 3);
extent_is('$h{k} + 1;',        0, 1, 'chain stops before op');

# --- casts / derefs -----------------------------------------------------
extent_is('@$list;',       0, 1);
extent_is('$$h{k};',       0, 2);
extent_is("\@\$h{'a','b'};", 0, 2, 'hash-ref slice');
extent_is('\$x;',          0, 1, 'ref-of-scalar');
extent_is('\@a;',          0, 1, 'ref-of-array');
extent_is('\&foo;',        0, 1, 'ref-of-code');
extent_is('${$r};',        0, 1, 'block deref');
# PPI spells the slice group after a block deref as a Constructor, not a
# Subscript; the walker declines rather than stop inside the term.
extent_is('@{$r}[0];',     0, undef, 'cast-block deref + slice group: declines');
extent_is('$#{$r};',       0, 1, 'arraylen of block deref');
extent_is('$#$r;',         0, 1, 'arraylen of scalar deref');

# --- postfix deref / slices ---------------------------------------------
extent_is('$hr->%*;',      0, 2, 'postfix hash deref');
extent_is('$ar->@*;',      0, 2, 'postfix array deref');
extent_is('$ar->@[0,1];',  0, 3, 'postfix array slice');
extent_is('$hr->%{a};',    0, 3, 'postfix kv slice');

# --- KV / % slices ------------------------------------------------------
extent_is('%h{a,b};',      0, 1, 'kv hash slice');
extent_is('%a[0,1];',      0, 1, 'kv array slice');

# --- constructors / blocks / lists as primaries -------------------------
extent_is('[1,2];',            0, 0);
extent_is('[1,2]->[0];',       0, 2);
extent_is('my $z = {a=>1}->{a};', 3, 5, 'anon hash + arrow subscript');
extent_is('(1,2);',            0, 0, 'paren list is one term');

# --- word calls ---------------------------------------------------------
extent_is('foo(1);',       0, 1, 'word + parens is a call term');
extent_is('foo(1)->[0];',  0, 3, 'call + arrow subscript');

# --- method calls -------------------------------------------------------
extent_is('$obj->method;',      0, 2, 'no-args method');
extent_is('$obj->method(1);',   0, undef, 'method WITH args: decline');
extent_is('$obj->$m(1);',       0, undef, 'dynamic method with args: decline');
extent_is('$obj->method->{k};', 0, 4, 'method then subscript');

# --- declines (undef is the answer, not an error) -----------------------
extent_is('foo;',          0, undef, 'bare word: not our call');
extent_is('foo 1;',        0, undef, 'list operator: not our call');
extent_is('!$x;',          0, undef, 'prefix op: not our call');
extent_is('-e $f;',        0, undef, 'filetest: not our call');
extent_is('+3;',           0, undef, 'unary plus: not our call');

# --- limit handling -----------------------------------------------------
{
  my $e = toks('$h{k};');
  is($o->_term_extent($e, 0, 0), undef, 'chain crossing limit declines');
  is($o->_term_extent($e, 5),    undef, 'start past end declines');
}

# --- _reduce_term round trip --------------------------------------------
{
  my $e = toks('$h{a}{b} + 1;');
  my ($id, $next) = $o->_reduce_term($e, 0);
  ok(defined $id,  '_reduce_term returns a node id');
  is($next, 3,     '_reduce_term returns index after the term');
  is(scalar(@$e), 5, 'input token array is NOT mutated');
}
{
  my $e = toks('foo;');
  my @r = $o->_reduce_term($e, 0);
  is(scalar(@r), 0, '_reduce_term declines when extent declines');
}

done_testing();
