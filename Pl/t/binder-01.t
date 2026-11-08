#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# binder-01.t -- the BINDER (Pl::Binder, perl's pad model; s525q, production
# since s533b; docs/binder-dual-run-s525.md).
#
# Each program is transpiled by ./pl2cl under PCL_BINDER_CHECK=die through
# `tools/binder-check.pl --dump`: the transpile must not die (a binder error
# dies in that mode) and the ref -> binding listing must give perl's answer
# for each shape (pad.c intro_my / pad_findlex; toke.c S_pending_ident).  The
# last row is RULE 12 for the shadow question every rename pass asks
# (_refers_to_decl, 1c port 3): a declaration the answering table does not
# hold DIES naming it.  (Until s535a that row inverted the binder's answer at
# the last dual-run hook, _ref_shadowed, which left with the port.)  PPI +
# perl only -- no SBCL: a fast file.
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);
use lib "$RealBin/../..";

my $root = "$RealBin/../..";
my $dir  = tempdir(CLEANUP => 1);
my $n    = 0;

sub dump_of {
  my ($src) = @_;
  my $f = "$dir/p" . (++$n) . '.pl';
  open my $fh, '>', $f or die "$f: $!"; print $fh $src; close $fh;
  my $out = `$^X $root/tools/binder-check.pl --dump $f 2>&1`;
  return ($out, $? >> 8);
}

# One listing line: "LINE:COL  TOKEN  CANON  VIA -> KIND NAME decl=L:C #ID"
sub binds {
  my ($out, $pos, $canon, $kind, $name, $decl) = @_;
  my $re = qr/^\Q$pos\E\s+\S.*?\s\Q$canon\E\s+\S+\s+-> \Q$kind\E\s+\Q$name\E\s+decl=\Q$decl\E\s/m;
  return $out =~ $re;
}

my $prog = <<'PERL';
my $x = 1;
{ my $x = $x + 1; print "$x\n"; }
my $y = 1; my $y = 2; print $y;
for (my $i = 0; $i < 2; $i++) { print $i } print $i;
my $v; for $v (1, 2) { print $v }
for $w (1) { print $w }
use feature 'signatures'; no warnings;
sub f ($p, $q = $p) { $p + $q }
our $o = 3; package Q; print $o, $u, $ENV{HOME}, $Z::q; package main;
my @a = (1); my $k = 0; print "$a[$k]\n";
my $cap = 5; sub g { $cap }
PERL
my ($out, $rc) = dump_of($prog);
is($rc, 0, 'PCL_BINDER_CHECK=die: the binder builds on the shapes program') or diag $out;
ok(binds($out, '2:11', '$x', 'my', '$x', '1:4'),  'an initializer sees the OUTER binding (intro_my: visible from the next statement)');
ok(binds($out, '2:25', '$x', 'my', '$x', '2:6'),  'the next statement sees the inner `my`');
ok(binds($out, '3:29', '$y', 'my', '$y', '3:15'), 'a same-scope redeclaration opens a new binding');
ok(binds($out, '4:39', '$i', 'my', '$i', '4:9'),  'a loop-HEAD `my` binds in the head and body');
ok(binds($out, '4:50', '$i', 'global', '$main::i', '-'), 'the loop-head `my` ends with the loop');
ok(binds($out, '5:12', '$v', 'my', '$v', '5:4'),  '`foreach $v` with a lexical $v in scope aliases the LEXICAL');
ok(binds($out, '6:5',  '$w', 'global', '$main::w', '-'), '`foreach $w` with no lexical is the global');
ok(binds($out, '8:17', '$p', 'param', '$p', '8:8'), 'a later default sees an earlier signature parameter');
ok(binds($out, '8:28', '$q', 'param', '$q', '8:12'), 'a signature parameter binds in the body');
ok(binds($out, '9:30', '$o', 'our', '$main::o', '9:5'), 'an `our` alias is the DECLARING package across a `package` statement (#475)');
ok(binds($out, '9:34', '$u', 'global', '$Q::u', '-'), 'an undeclared name is the global of the POSITIONAL package');
ok(binds($out, '9:38', '%ENV', 'special', '%main::ENV', '-'), '%ENV is forced into main:: (special)');
ok(binds($out, '9:50', '$Z::q', 'global', '$Z::q', '-'), 'a qualified name is never a lexical (#3080)');
ok(binds($out, '10:31', '$k', 'my', '$k', '10:17') && binds($out, '10:31', '@a', 'my', '@a', '10:4'),
   'an interpolated "$a[$k]" binds both names');
ok(binds($out, '11:22', '$cap', 'my', '$cap', '11:4'), 'a named sub reads (captures) the outer `my`');

# RULE 12 (1c port 3, s535a): the rename passes' shadow question -- "is this
# token a use of the variable that declarator makes?" -- asked about a
# declaration the answering binder does not hold (here: a use in ANOTHER
# document) DIES naming it, never a silent "not a use" that would leave the
# token un-renamed.  In-process, from a real binder.
{
  require Pl::VendorPPI;
  require Pl::Parser2;
  require Pl::Binder;
  my $src = "my \$q = 1; print \$q;\n";
  my $doc = PPI::Document->new(\$src);
  my $parser = bless {}, 'Pl::Parser2';
  $parser->{_binder} = Pl::Binder->new($doc, parser => $parser)->bind;
  my ($decl) = grep { $_->content eq '$q' } @{ $doc->find('PPI::Token::Symbol') };
  my $osrc = "print \$q;\n";
  my $other = PPI::Document->new(\$osrc);
  my ($use) = @{ $other->find('PPI::Token::Symbol') };
  my $err = eval { $parser->_refers_to_decl($use, '$q', $decl); 1 } ? '' : $@;
  like($err, qr/^PCL internal: the binder holds no record of `\$q` \(asked about \$q, declared by `\$q` at line 1\)/,
       'rule 12: the shadow question dies on a declaration the answering table does not hold');
}

done_testing();
