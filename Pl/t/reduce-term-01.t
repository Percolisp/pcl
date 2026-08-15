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
# Subscript (only because it follows a `}` rather than a symbol).  It is a
# SLICE on the deref, so the walker takes it as one postfix group — and stops:
# a slice yields a list, nothing postfixes it.  (#153 step 4a.)
extent_is('@{$r}[0];',     0, 2, 'cast-block deref + slice group');
extent_is('%{$h}{a};',     0, 2, 'cast-block deref + kv slice group');
extent_is('${$r}[0];',     0, 2, 'scalar-cast block deref + group');
extent_is('@{$r}[0] + 1;', 0, 2, 'slice group stops before op');
# INVERSE: the group rule needs a CAST — a bare block/constructor primary
# followed by another group is not a slice on anything.
extent_is('{a=>1}[0];',    0, 0, 'no cast: group does not attach');
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
# #153 step 4a: an arg List directly after a method NAME is part of the call,
# so the walker consumes it and keeps walking the chain (it used to decline
# rather than stop in the middle of a method call).
extent_is('$obj->method(1);',   0, 3, 'method WITH args');
extent_is('$obj->$m(1);',       0, 3, 'dynamic method with args');
extent_is('$obj->m(1)->[0];',   0, 5, 'method args then subscript');
extent_is('$obj->m(1)->n(2);',  0, 6, 'chained method calls with args');
extent_is('$obj->m(1) + 2;',    0, 3, 'method call stops before op');
extent_is('$obj->method->{k};', 0, 4, 'method then subscript');
# INVERSE: a List after a SUBSCRIPT is not an argument list — `$h{k}(1)` is a
# code-ref call, which the term grammar does not claim.
extent_is('$h{k}(1);',          0, 1, 'list after subscript is not method args');

# --- #153 FOLD chunk 3 widenings W1–W5 ---------------------------------
# handle_subcalls runs BEFORE the fold, so at fold time `m(1)` is already ONE
# funcall node: model that state by splicing a synthetic node wrapper in.
sub with_node {
  my ($code, $at, $len, $type) = @_;
  my $e = toks($code);
  splice @$e, $at, $len, $o->make_subtree_item(0, $type);
  return $e;
}
# W1: `-> <funcall>` is a chain step.
{
  my $e = with_node('$o->m(1) + 1;', 2, 2, 'funcall');   # $o -> <funcall> + 1
  is($o->_term_extent($e, 0), 2, 'W1: -> <funcall> is a chain step');
  $e = with_node('$o->m(1)->{k}->n(2);', 2, 2, 'funcall'); # $o -> <fc> -> {k} -> n (2)
  is($o->_term_extent($e, 0), 7, 'W1: chain continues past -> <funcall>');
  $e = with_node('$o->m(1);', 2, 2, 'funcall');
  is($o->_term_extent($e, 0), 2, 'W1: whole-array method call');
}
# W2: `-> ( args )` — a coderef call — raw List or the <tree_val> node.
extent_is('$code->(1);',            0, 2, 'W2: coderef call, raw List');
extent_is('$h{k}->(1)->[0];',       0, 5, 'W2: coderef call then subscript');
extent_is('$code->(1) + 1;',        0, 2, 'W2: coderef call stops before op');
{
  my $e = with_node('$code->(1) + 1;', 2, 1, 'tree_val');
  is($o->_term_extent($e, 0), 2, 'W2: coderef call, <tree_val> args');
}
# W3: `-> $#*` postfix last-index.
extent_is('$ar->$#*;',              0, 2, 'W3: postfix $#*');
extent_is('$ar->$#* + 1;',          0, 2, 'W3: postfix $#* stops before op');
# W4: a `[..]` Constructor directly after a List / qw() primary is a LIST
# SLICE group; the chain continues after it.
extent_is('(1,2,3)[1];',            0, 1, 'W4: list slice');
extent_is('(f())[0]->{k};',         0, 3, 'W4: list slice then chain');
extent_is('qw(a b)[1];',            0, 1, 'W4: qw list slice');
extent_is('(1,2)[0] + 1;',          0, 1, 'W4: list slice stops before op');
# INVERSE: perl rejects `f()[0]` and `$o->m()[0]` — a Constructor after a
# call/method-call is NOT taken (the walker stops before it).
extent_is('foo()[0];',              0, 1, 'W4 inverse: no slice group after a call');
extent_is('$o->m()[0];',            0, 3, 'W4 inverse: no slice group after a method call');
extent_is('(1,2){0};',              0, 0, 'W4 inverse: only [..] is a list slice');
# W5: a WORD directly followed by an arrow is a self-bounded primary.
extent_is('Foo->new;',              0, 2, 'W5: class method call');
extent_is('Foo::Bar->new(1);',      0, 3, 'W5: qualified class + args');
extent_is('shift->m;',              0, 2, 'W5: shift->m');
extent_is('__PACKAGE__->m(1)->[0];', 0, 5, 'W5: __PACKAGE__ chain');
extent_is('foo->[0];',              0, 2, 'W5: word arrow subscript');
extent_is('Foo->new + 1;',          0, 2, 'W5: stops before op');
extent_is('Foo::->new;',            0, 2, 'W5: Foo:: spelling');
# INVERSE: a word NOT directly followed by an arrow is still declined.
extent_is('Foo => 1;',              0, undef, 'W5 inverse: fat comma is not an arrow');
extent_is('foo $x->m;',             0, undef, 'W5 inverse: word then term is a list op');
# W6: a glob slot `*name{SLOT}` (Symbol + Block) is one postfix group and the
# chain continues.
extent_is('*STDOUT{IO};',            0, 1, 'W6: glob slot');
extent_is('*STDOUT{IO}->autoflush(1);', 0, 4, 'W6: glob slot then method chain');
extent_is('*x{CODE} + 1;',           0, 1, 'W6: glob slot stops before op');
extent_is('\*STDOUT{IO};',           0, 2, 'W6: ref of a glob slot');
extent_is('*x + 1;',                 0, 0, 'W6 inverse: bare glob, no group');
# W7: `-> ${ EXPR }` is a computed-method step, and its `( args )` belong to it.
extent_is('$o->${\ "m"};',           0, 3, 'W7: computed method');
extent_is('$o->${\ "m"}(1);',        0, 4, 'W7: computed method with args');
extent_is('$o->${\ "m"}(1)->[0];',   0, 6, 'W7: computed method args then chain');
extent_is('Foo->${\ "m"} + 1;',      0, 3, 'W7: on a class word, stops before op');
# W8: PPI labels the leading `(…)` of a postfix-if condition a Condition;
# it is a paren primary — chain and list-slice rules apply.
{
  my $doc = PPI::Document->new(\'return 1 if (my $t = $r)->[0] eq "x";');
  push @keep_docs, $doc;
  my ($stmt) = grep { $_->isa('PPI::Statement') } $doc->children;
  my @sig = grep { $_->significant } $stmt->children;
  my ($ci) = grep { ref($sig[$_]) eq 'PPI::Structure::Condition' } 0 .. $#sig;
  ok(defined $ci, 'W8: PPI gives a Structure::Condition after postfix if');
  my @e = @sig[$ci .. $#sig - 1];   # drop the trailing ';'
  is($o->_term_extent(\@e, 0), 2, 'W8: Condition primary + arrow subscript');
  $doc = PPI::Document->new(\'print 1 if (f())[1] == 2;');
  push @keep_docs, $doc;
  ($stmt) = grep { $_->isa('PPI::Statement') } $doc->children;
  @sig = grep { $_->significant } $stmt->children;
  ($ci) = grep { ref($sig[$_]) eq 'PPI::Structure::Condition' } 0 .. $#sig;
  @e = @sig[$ci .. $#sig - 1];
  is($o->_term_extent(\@e, 0), 1, 'W8: Condition + Constructor is a list slice');
}
# W9: after a list-slice group, PPI labels a further ARROW-LESS `[j]` group a
# Constructor by predecessor — it is a subscript on the slice; the arrow chain
# continues after it.  (The `{k}` twin arrives as a Block and is re-labelled a
# Subscript by the retag pre-pass — its rows live with that pass.)
extent_is('([1,2])[0][1];',         0, 2, 'W9: array subscript after a list slice');
extent_is('(f())[0][1]->{x};',      0, 4, 'W9: then the arrow chain continues');
extent_is('([1,2])[0][1] + 1;',     0, 2, 'W9: stops before op');
extent_is('(1,2) [1];',             0, 1, 'W9: whitespace before the slice group is not significant');
{
  my $e = toks('my $x = ([1,2])[0][1] + 1;');
  splice @$e, 0, 3;                    # the RHS: List [0] [1] + 1
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W9 fold: list slice + array subscript folds: [node + 1]');
}

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

# --- _fold_terms (#153 FOLD chunk 1) ------------------------------------
# The pre-pass reduces embedded postfix-bearing terms in place.  These rows
# pin the guards that make it SAFE, not just the folds that make it work.
{
  my $e = toks('$h{k} + $a[0];');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'both embedded terms folded: [node + node]');
  ok($o->is_internal_node_type($e->[0]), 'left term is a node');
  ok($o->is_internal_node_type($e->[2]), 'right term is a node');
}
{
  # Whole-array guard: a term that IS the whole expression is left to the
  # legacy machinery — _reduce_term parses through this same function, so
  # folding it would recurse forever.
  my $e = toks('$h{a}{b};');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'whole-array term is NOT folded (recursion guard)');
}
{
  # Cast-start guard: `$$r[0]` must fold FROM THE CAST or not at all.  The
  # whole-array guard blocks the cast start here, and the Symbol at index 1
  # must not fold alone — that would re-bind the subscript as `${ $r[0] }`.
  my $e = toks('$$r[0];');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'tail of a cast-led whole-array term is not folded');
}
{
  # Embedded cast-led term folds from the cast, one node, correct extent.
  my $e = toks('$$r[0] + 1;');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'cast-led term folds from the cast: [node + 1]');
  ok($o->is_internal_node_type($e->[0]), 'folded cast-led term is a node');
}
{
  # Arrow-start guard: a dynamic method name is the middle of a chain.
  my $e = toks('$o->$m + 1;');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'dynamic-method chain folds as one term');
}
{
  # Word-led terms are not folded (indirect object / list operators), but a
  # postfix-bearing ARGUMENT after the word is.
  my $e = toks('foo $x[0];');
  $o->_fold_terms($e);
  is(scalar(@$e), 2, 'word stays raw, its postfix-bearing arg folds');
  is(ref($e->[0]), 'PPI::Token::Word', 'the word itself is untouched');
}
{
  # Combining-rule guard: a raw Block after the term (indirect method args
  # `$o->SUPER::m{@a}`) blocks the fold — the legacy loop reads the pair.
  my $e = toks('$o->SUPER::m{@a};');
  $o->_fold_terms($e);
  is(scalar(@$e), 4, 'term followed by a raw Block is not folded');
}

{
  # #153 chunk 3 (W5): the fold's start set — a WORD directly followed by an
  # arrow, a quoted string, a qw() list; and (W4) a List whose slice group
  # follows.  Each embedded term folds to ONE node; a word not followed by an
  # arrow (fat comma) stays raw.
  my $e = toks('Foo->new(1) + 1;');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W5 fold: Class->new(args) folds: [node + 1]');
  ok($o->is_internal_node_type($e->[0]), 'W5 fold: the folded term is a node');
  $e = toks('shift->m + 1;');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W5 fold: shift->m folds');
  $e = toks('"Class"->new + 1;');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W5 fold: quoted-string invocant folds');
  $e = toks('qw(a b)[1] . "x";');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W4/W5 fold: qw list slice folds');
  $e = toks('(1,2)[0] + 1;');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W4 fold: list slice folds from the List');
  $e = toks('Foo => $h{k};');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W5 inverse: fat-comma word stays raw, its value folds');
  is(ref($e->[0]), 'PPI::Token::Word', 'W5 inverse: the word itself is untouched');
  $e = toks('$o->SUPER::new + 1;');
  $o->_fold_terms($e);
  is(scalar(@$e), 3, 'W5: SUPER::new after an arrow is mid-chain, folds from $o');
}

done_testing();
