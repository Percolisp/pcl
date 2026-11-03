#!/usr/bin/env perl
# stmt-classify-01.t -- Pl::Stmt, THE statement classifier (s533a, C1 of the
# parser program #3694): one row per kind and per shape reader, pure unit rows
# (PPI only, no SBCL).  A statement is parsed with the vendored PPI and its
# record asserted -- kind + the text of the spans the lowering reads.  The
# shapes that were wrong-prone in history are here on purpose (a postfix
# modifier with a below-assignment tail, `return … if …`, a chained `my`, a
# label on a loop, `package NAME BLOCK`, a signature, the refused `format`).
# Inverse row: a statement class with no kind DIES (rule 12) -- a missing kind
# is never a silent default arm.
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use lib "$RealBin/../..";
use Pl::VendorPPI;   # PPI, from vendor/PPI/lib (s527a)
use Pl::Stmt;

# The first (or the Nth) top-level statement of SRC.
sub st {
  my ($src, $n) = @_;
  my $d = PPI::Document->new(\$src) or die "PPI failed on: $src\n";
  my @s = grep { $_->isa('PPI::Statement') } $d->schildren;
  my $s = $s[ $n // 0 ] or die "no statement $n in: $src\n";
  our %KEEP; $KEEP{$s} = $d;   # a PPI element dies with its document
  return $s;
}
sub txt { my ($t) = @_; return join ' ', map { $_->content } @$t }
sub kind { Pl::Stmt::classify(st(@_))->{kind} }

# ---- kinds ------------------------------------------------------------------
is kind('my $x = f() or die;'),        'decl',      'my-decl with a low-prec tail is a decl';
is kind('our $x = 1;'),                'decl',      'our is a decl';
is kind('local $h{k} = 1;'),           'local',     'local ELEM = … is local';
is kind('delete local $h{k};'),        'local',     'delete local is local';
is kind('my ($v) = delete local $h{k};'), 'local',  'my (…) = delete local … is local';
is kind('BEGIN { 1 }'),                'scheduled', 'BEGIN is scheduled';
is kind('use strict;'),                'use',       'use is use';
is kind('no warnings;'),               'use',       'no is use';
is kind('require Foo;'),               'use',       'require is use';
is kind("1;\n__END__\nx\n", 1),        'end',       '__END__ is end';
is kind('sub f { 1 }'),                'sub',       'a named sub is sub';
is kind('my $f = sub { 1 };'),         'decl',      'an anon sub in an init stays the decl';
is kind('sub { 1 };'),                 'expr',      'an unnamed sub statement is expr';
is kind('return $x if $y;'),           'return',    'return is return';
is kind('next if $x;'),                'loopctl',   'next is loopctl';
is kind('last LOOP;'),                 'loopctl',   'last LABEL is loopctl';
is kind('if ($x) { 1 }'),              'compound',  'if is compound';
is kind('{ 1 }'),                      'compound',  'a bare block is compound';
is kind('$x = 1;'),                    'expr',      'an assignment is expr';
is kind('foo();'),                     'expr',      'a call is expr';
my $r = Pl::Stmt::classify(st('$x = 1;'));
is $r->{plain}, 1, 'a plain statement is plain (the embedded-my arm)';
is_deeply [@{ Pl::Stmt::classify(st('given ($x) { 1 }')) }{qw(kind plain)}], ['expr', 0], 'a given statement is expr, not plain';

# label kinds
$r = Pl::Stmt::classify(st("AGAIN:\n\$x++;"));
is_deeply [@$r{qw(kind label)}], ['label', 'AGAIN'], 'a standalone label';
is Pl::Stmt::standalone_label(st("AGAIN:\n\$x++;")), 'AGAIN', 'standalone_label reads it';
$r = Pl::Stmt::classify(st('L1: for my $i (1..3) { 1 }'));
is_deeply [@$r{qw(kind label)}], ['compound', 'L1'], 'a label on a loop is the compound, label kept';
is Pl::Stmt::standalone_label(st('L1: for my $i (1..3) { 1 }')), undef, '... and not a standalone label';

# package
$r = Pl::Stmt::classify(st('package Foo { 1 }'));
is_deeply [@$r{qw(kind name version)}], ['package', 'Foo', undef], 'package NAME BLOCK';
ok $r->{block}, '... carries its block';
$r = Pl::Stmt::classify(st('package Bar 1.2;'));
is_deeply [@$r{qw(name version)}, defined $r->{block} ? 1 : 0], ['Bar', '1.2', 0], 'package NAME VERSION;';

# sub with a signature / a prototype / a forward declaration
$r = Pl::Stmt::classify(st("use feature 'signatures';\nsub f (\$x, \$y = 2) { \$x }", 1));
is_deeply [@$r{qw(kind name proto_or_sig)}], ['sub', 'f', '($x, $y = 2)'], 'a sub with a signature';
$r = Pl::Stmt::classify(st('sub g ($$) { 1 }'));
is $r->{proto_or_sig}, '$$', 'a sub with a prototype';
$r = Pl::Stmt::classify(st('sub h;'));
ok !$r->{block}, 'a forward declaration has no block';

# refused: the REFUSAL fact (a `format` statement, a `defer` block, ...)
$r = Pl::Stmt::classify(st('format STDOUT ='), refusal => 'format/write report formatting is not supported');
is_deeply [@$r{qw(kind why)}], ['refused', 'format/write report formatting is not supported'],
  'a refusal fact makes the kind refused (rule 11: format is routed to the refusal)';

# ---- decl_shape ---------------------------------------------------------------
sub dshape { Pl::Stmt::decl_shape(st(@_)) }
my $d = dshape('my $x = f() or die;');
is_deeply [$d->{decl}, $d->{name}, txt($d->{init}), txt($d->{tail})],
  ['my-scalar', '$x', 'f () or die', 'or die'], 'my $x = f() or die: init + its below-assignment tail';
$d = dshape('my $x = 5;');
is_deeply [$d->{decl}, txt($d->{init}), $d->{tail}], ['my-scalar', '5', undef], 'my $x = 5 has no tail';
$d = dshape('my $x if $c;');
is_deeply [$d->{decl}, $d->{init}, txt($d->{declmod})], ['my-scalar', undef, '$c'], 'my $x if COND';
$d = dshape('my $a = my $b = 0;');
is_deeply [$d->{decl}, $d->{name}, txt($d->{init})], ['my-scalar', '$a', 'my $b = 0'], 'a chained my: the head declares $a';
is dshape('state $n = 0;')->{decl}, 'state', 'state';
is dshape('state $n = 0;')->{name}, '$n', 'state name';
is dshape('our @a = (1);')->{decl}, 'our', 'our';
is_deeply [@{ dshape('my $aa, $bb;') }{qw(decl name)}], ['my-lead', '$aa'], 'my VAR, … (lead)';
is dshape('my ();')->{decl}, 'my-empty', 'my ()';
is dshape('my (undef) = f();')->{decl}, 'my-noname', 'my (undef) = RHS';
is dshape('my @a = (1, 2);')->{decl}, 'my-list', 'my @a = LIST';
is dshape('my ($p, %q);')->{decl}, 'my-list', 'my (LIST)';
my @k = Pl::Stmt::strip_semi(st('my @a = (1, 2);')->schildren);
is_deeply Pl::Stmt::decl_list_view(\@k), { eq => 2, container => 1, paren => 0, names => $k[1] }, 'decl_list_view of my @a = …';
@k = Pl::Stmt::strip_semi(st('my ($p) = my $q = my @r = 7;')->schildren);
my ($names, $rest) = Pl::Stmt::decl_chain([@k[3 .. $#k]]);
is_deeply [$names, txt($rest)], [['$q', '@r'], '7'], 'decl_chain reads the chained links';

# ---- compound_shape -----------------------------------------------------------
sub cshape { Pl::Stmt::compound_shape(st(@_)) }
my $c = cshape('if ($x) { 1 } elsif ($y) { 2 } else { 3 }');
is_deeply [$c->{ckind}, map { $_->{kw} } @{ $c->{clauses} }], ['if', 'if', 'elsif', 'else'], 'if/elsif/else clauses';
is cshape('unless ($x) { 1 }')->{ckind}, 'if', 'unless is the if chain';
$c = cshape('while (my $l = <F>) { 1 }');
is_deeply [$c->{ckind}, $c->{kw}, $c->{cond}->content], ['while', 'while', '(my $l = <F>)'], 'while (COND)';
is cshape('until ($x) { 1 }')->{kw}, 'until', 'until';
$c = cshape('for (my $i = 0; $i < 3; $i++) { 1 }');
is_deeply [$c->{ckind}, $c->{nsections}, map { $_->content } @$c{qw(init cond step)}],
  ['for-c', 3, 'my $i = 0;', '$i < 3;', '$i++'], 'for(;;) sections';
$c = cshape('for (;;) { 1 }');
is_deeply [$c->{ckind}, $c->{init}, $c->{cond}, $c->{step}], ['for-c', undef, undef, undef], 'for(;;) empty sections';
$c = cshape('L1: for my $i (1..3) { 1 }');
is_deeply [@$c{qw(ckind label name loop_my cast)}], ['foreach', 'L1', '$i', 1, 0], 'label + foreach my';
$c = cshape('foreach $x (@a) { 1 }');
is_deeply [@$c{qw(ckind name loop_my)}, txt($c->{list_parts})], ['foreach', '$x', 0, '@a'], 'foreach VAR (LIST)';
$c = cshape('for (@a) { 1 }');
is_deeply [@$c{qw(ckind name)}, $c->{var}], ['foreach', '$_', undef], 'for (LIST): the topic';
$c = cshape('{ 1 } continue { 2 }');
is_deeply [$c->{ckind}, $c->{cont}->content, $c->{cont_missing}], ['block', '{ 2 }', 0], 'a bare block with continue';
$c = cshape("use feature 'try'; no warnings;\ntry { 1 } catch (\$e) { 2 } finally { 3 }", 2);
is_deeply [@$c{qw(ckind catch_var has_catch)}, scalar @{ $c->{blocks} }, $c->{fin_block}->content],
  ['try', '$e', 1, 2, '{ 3 }'], 'try / catch (VAR) / finally';
is cshape('LOOP: { 1 }')->{label}, 'LOOP', 'a labelled bare block keeps the label';

# ---- break_shape --------------------------------------------------------------
my $b = Pl::Stmt::break_shape(st('return $x if $y;'));
is_deeply [$b->{kw}, txt($b->{expr}), $b->{mod}, txt($b->{cond}), $b->{top_comma}],
  ['return', '$x', 'if', '$y', 0], 'return EXPR if COND';
$b = Pl::Stmt::break_shape(st('return if $y;'));
is_deeply [txt($b->{expr}), $b->{mod}], ['', 'if'], 'return if COND: empty expr';
is Pl::Stmt::break_shape(st('return (1, 2);'))->{top_comma}, 1, 'return (LIST) has a top-level comma';
is Pl::Stmt::break_shape(st('return f(1, 2);'))->{top_comma}, 0, 'return f(LIST) has none';
$b = Pl::Stmt::break_shape(st('next LINE unless $ok;'));
is_deeply [$b->{kw}, txt($b->{expr}), $b->{mod}], ['next', 'next LINE', 'unless'], 'next LABEL unless COND';

# ---- expr_shape ---------------------------------------------------------------
sub xshape { Pl::Stmt::expr_shape(st(@_)) }
my $x = xshape('$x = $y + 1;');
is_deeply [@{ $x->{assign} }{qw(name op)}, txt($x->{assign}{rhs})], ['$x', '=', '$y + 1'], '$x = RHS';
is xshape('$x .= "a";')->{assign}{op}, '.=', '$x OP= RHS';
is xshape('$x = f() or die;')->{assign}, undef, 'a below-assignment tail keeps the generic path';
is xshape('$x = 1 if $c;')->{assign}, undef, 'a modifier keeps the generic path';
is_deeply xshape('$n++;')->{incdec}, { name => '$n', op => '++', post => 1 }, '$n++';
is_deeply xshape('--$n;')->{incdec}, { name => '$n', op => '--', post => 0 }, '--$n';
is xshape('@a++;')->{incdec}, undef, 'only a plain scalar is an incdec';
is xshape('foo;')->{bareword}->content, 'foo', 'a bare word statement';
is xshape('...;')->{yada}, 1, 'the yada statement';

# ---- rule 12: a statement class with no kind DIES ------------------------------
{
  package PPI::Statement::S533aBogus;
  our @ISA = ('PPI::Statement');
}
my $bogus = st('1;');
bless $bogus, 'PPI::Statement::S533aBogus';
eval { Pl::Stmt::classify($bogus) };
like $@, qr/^Pl::Stmt: no statement kind for a PPI::Statement::S533aBogus/,
  'an unknown statement class dies naming it (no silent default arm)';

done_testing;
