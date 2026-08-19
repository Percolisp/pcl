#          -*-Mode: CPerl -*-
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ruled-refusal-01.t — Option B phase 2 Track A (task #371): the five families
# that stop being SILENT DROPS and become perl-shaped REFUSALS.
#
# The classifier runs only where a statement was already lost, so the rows
# below come in pairs: the shape that must now REFUSE, and the sibling shape
# that must NOT — because the two directions are not symmetrical.  A missed
# refusal leaves today's behaviour; a false one kills a whole file.
#
# The `~~` arm is the one that asks nothing itself: since #370 the infix/prefix
# question is answered ONCE, upstream, by
# Parser2::_repair_term_initial_complement (a term-initial `~~` is two
# complements — `is(~~$y, 3)` in perl-tests/bop.t, 507 rows — and is split
# before the parse).  Its guard rows and the PPI canary are in
# Pl/t/misc-fixes-02.t.
#
# Most rows call the classifier directly on PPI tokens: no transpile, no SBCL.
# Three end-to-end rows check that the refusal actually reaches the user (file
# mode) and that a NON-ruled drop still only drops.

use v5.32;
use strict;
use warnings;

use lib ".";

use PPI;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

BEGIN { use_ok('Pl::Parser') };

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";

my $p = Pl::Parser->new();

# The document must be kept ALIVE: its recursive DESTROY hollows out the
# tokens the classifier is still holding (the reduce-term-01.t gotcha).
my @keep_docs;

# The statement the announcer would have handed the classifier: CODE's LAST
# top-level statement (so a row can put `use feature 'class';` in front of the
# statement under test and have the feature be in scope).
sub stmt_of {
  my ($code) = @_;
  my $doc = PPI::Document->new(\$code) or die "PPI failed on: $code";
  push @keep_docs, $doc;
  my @st = grep { $_->isa('PPI::Statement') } $doc->children;
  die "no statement in: $code" if !@st;
  return $st[-1];
}

sub refuses {
  my ($code, $re, $desc) = @_;
  my $got = $p->_ruled_refusal_for_drop([stmt_of($code)]);
  like($got // '(no refusal)', $re, $desc);
}

sub keeps_dropping {
  my ($code, $desc) = @_;
  my $got = $p->_ruled_refusal_for_drop([stmt_of($code)]);
  is($got, undef, $desc);
}

# --- given / when / default (removed in perl 5.42) -----------------------
refuses('given ($x) { when (1) { print "a" } }', qr/^given\/when .*not supported/,
        'given BLOCK refuses');
refuses('when (1) { print "a" }', qr/^given\/when/, 'when BLOCK refuses');
refuses('default { print "b" }',  qr/^given\/when/, 'default BLOCK refuses');
refuses('CORE::given ($x) { }',   qr/^given\/when/, 'CORE::given refuses');
keeps_dropping('my $g = given_up($x);', 'a sub whose name merely starts with given');
keeps_dropping('$h{when} = 1;',         'a hash key named when');

# --- perl 5.38 class / field / method / ADJUST ---------------------------
refuses('class Foo { field $x; }', qr/^feature 'class'/, 'class BLOCK refuses');
refuses("use feature 'class';\nfield \$x;", qr/^feature 'class'/,
        'field refuses when the feature is in scope');
refuses("class Foo;\nmethod m { 1 }", qr/^feature 'class'/,
        'method refuses inside a file that declares a class');
refuses("use v5.38;\nADJUST { 1 }", qr/^feature 'class'/,
        'ADJUST refuses under use v5.38');
keeps_dropping('method $obj "a", "b";',
               'a Moose-style/indirect `method` in a file with no class');
keeps_dropping('field $x;', 'a `field` sub call in a file with no class');

# --- format / write ------------------------------------------------------
refuses("format one = \n", qr/^format\/write/, 'format NAME = refuses');
keeps_dropping('$h{format} = 1;', 'a hash key named format');

# --- defer (perl 5.36) ---------------------------------------------------
refuses('defer { print "x" }', qr/^defer blocks/, 'defer BLOCK refuses');
keeps_dropping('my $f = first { $_ > 1 } @a;',
               'a (&@)-prototype block call is not a defer block');

# --- smart match, and the double complement that is NOT one --------------
refuses('my $r = $x ~~ @y;', qr/^smart match/, 'infix ~~ after a symbol refuses');
refuses('my $g = join q{-}, (@a, (/X/ ~~ @b));', qr/^smart match/,
        'infix ~~ after a match refuses');
# A PREFIX `~~` never reaches this classifier at all: since #370,
# Parser2::_repair_term_initial_complement has already split it into two `~`
# complements, so `is(~~$y, 3)` COMPILES (guard rows + the PPI canary are in
# Pl/t/misc-fixes-02.t).  That is why the arm above asks nothing beyond "is
# there a `~~` token" — one predicate, upstream, instead of a second one here.
refuses('my $r = $x ~~ $y;', qr/^smart match/, 'infix ~~ between two scalars refuses');

# --- the deliberate exclusion (Track A table, measured out — task #399) ---
keeps_dropping('$foo = doit $object "FOO";',
               'indirect object syntax is NOT refused here');

# --- end to end: the refusal reaches the user, and only for these families
SKIP: {
  skip "pl2cl not found", 3 unless -x $pl2cl;

  my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh "my \$x = 1;\ngiven (\$x) { when (1) { print \"a\" } }\n";
  close $fh;
  my (undef, $err, $rc) = PCLCore::transpile_raw("$pl2cl $pl");
  isnt($rc, 0, 'pl2cl exits nonzero on a ruled refusal');
  like($err, qr/^PCL: given\/when \(feature 'switch'\) is not supported/m,
       'the refusal text is perl-shaped and names the feature');
  like($err, qr/, at \Q$pl\E line 2$/m, 'the refusal names file and line');
}

# A drop that is NOT one of the ruled families still only drops: pl2cl exits 0
# and announces.  This is the boundary the whole change rests on.
SKIP: {
  skip "pl2cl not found", 2 unless -x $pl2cl;

  my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh "my \$foo;\nmy \$object = bless {}, 'C';\n\$foo = doit \$object \"FOO\";\n";
  close $fh;
  my (undef, $err, $rc) = PCLCore::transpile_raw("$pl2cl $pl");
  is($rc, 0, 'a non-ruled drop leaves pl2cl exiting 0');
  like($err, qr/^PCL: statement dropped at .* line 3:/m,
       'and it is still announced as a drop');
}


# The repair must NOT fire when the left side is a term that is not a Symbol:
# a match, a heredoc, `…`/qx…, <FH>.  This is end-to-end on purpose — the unit
# rows above never run the repair, and the drop census is what caught the
# regression (#370's first cut read `/X/ ~~ @a` as term-initial and split
# perl's smart match into two complements, silently, in t/op/smartmatch.t).
SKIP: {
  skip "pl2cl not found", 2 unless -x $pl2cl;

  my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh "my \@a = ('x');\nfor ('x') { my \$r = (/x/ ~~ \@a); print \$r }\n";
  close $fh;
  my (undef, $err, $rc) = PCLCore::transpile_raw("$pl2cl $pl");
  isnt($rc, 0, 'a match on the left keeps `~~` INFIX (refused, not split)');
  like($err, qr/^PCL: smart match/m, 'and the refusal is the smart-match one');
}
done_testing();
