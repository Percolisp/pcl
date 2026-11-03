#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# requalify-binder-01.t -- the BLOCK-PACKAGE REQUALIFIER asks the BINDER
# (s533b, parser program step 1c port 1; docs/binder-dual-run-s525.md §6
# item 1).
#
# A `package X;` statement inside a block (a sub body, a bare block) re-homes
# every bare package variable for the rest of the block, which a nested
# `(in-package :X)` cannot express (in-package is READ-time), so the
# requalifier rewrites the region's bare names to qualified spellings.  It used
# to decide "lexical or global?" with its own scope walk (`_binding_at` and
# four helpers); it now asks `Pl::Binder::requalify_home`, perl's pad model:
#   my | state | param | lexsub | outer -> leave the token
#   our -> the DECLARING package; global -> the POSITIONAL package;
#   special (%ENV, $STDOUT, ...) -> main.
# The legacy walk had three SILENT WRONGS the binder's dual-run found (s525q,
# #3800 #3801 #3804) and three more the port's D0 measurement and its probes
# named (#4423, #4424 x2).  Rows 1 and 3-8 FAIL on the base 4fb60297
# (inverse-verified); row 2 is #3800's bare-block twin, which the base gets
# right by accident (the block is split into sections there).  #3801's
# bare-block twin is NOT here: at file level the chained `my $F` crosses a
# section and the SPAN pass, which still reads only a statement's first
# declarator, mis-homes it (F=0 on the base and after the port; the span pass
# is port 3, docs/binder-dual-run-s525.md §6) -- task #4425.  Every expected text is
# perl 5.40.3's own output.  Row 12 is rule 12: a token the binder holds no
# record for DIES naming it -- never a silent "leave".

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 12;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── 1-2. #3800: a `my $x` inside an EARLIER anon sub of the same list is not
#    in scope in a LATER one (pad_leavemy at the first body's end); the later
#    sub's `$x` is the global of the region's package.
is(run_cl(<<'PL'), "c=1\nmain=u P=5\n",
sub run {
    my $c = 0;
    package P;
    my @t = (sub { my $x = 1; $x }, sub { $x = 5; $c++ });
    $t[1]->();
    package main;
    print "c=$c\n";
}
run();
print "main=", $main::x // 'u', " P=", $P::x // 'u', "\n";
PL
   '#3800: a `my` in an earlier sibling anon sub does not bind a later one (sub body)');

is(run_cl(<<'PL'), "c=1\nmain=u P=5\n",
{
    my $c = 0;
    package P;
    my @t = (sub { my $x = 1; $x }, sub { $x = 5; $c++ });
    $t[1]->();
    package main;
    print "c=$c\n";
}
print "main=", $main::x // 'u', " P=", $P::x // 'u', "\n";
PL
   '#3800: the bare-block twin');

# ── 3-4. #3801: a CHAINED declaration `my $S = my $F = 0` declares both
#    names; a named sub after the switch reads the LEXICAL $F.
is(run_cl(<<'PL'), "F=2 S=2 T::F=u\n",
sub run {
    my $S = my $F = 0;
    package T;
    sub g { ++$F; ++$S }
    package main;
    T::g(); T::g();
    print "F=$F S=$S T::F=", $T::F // 'u', "\n";
}
run();
PL
   '#3801: the second `my` of a chained declaration is a lexical (sub body)');

is(run_cl(<<'PL'), "s=012 Q::i=u\nt=01 R::j=u\n",
sub run {
    package Q;
    my $s = "";
    for (my $i = 0; $i < 3; $i++) { $s .= $i }
    print "s=$s Q::i=", $Q::i // "u", "\n";
}
run();
{ package R; my $t = ""; for (my $j = 0; $j < 2; $j++) { $t .= $j } print "t=$t R::j=", $R::j // "u", "\n" }
PL
   '#4423: a C-style `for (my $i ...)` binds its condition and step too (sub body + bare block)');

# ── 5-6. #3804: `tie my($x), CLASS` after the switch ties the LEXICAL.
is(run_cl(<<'PL'), "x=Human pre=1\nglobal untied\n",
sub run {
    my $pre = 1;
    package HumanTie;
    sub TIESCALAR { bless {} }
    sub FETCH { "Human" }
    tie my($x), "HumanTie";
    print "x=$x pre=$pre\n";
    print defined(tied $HumanTie::x) ? "global tied\n" : "global untied\n";
}
run();
PL
   '#3804: `tie my($x), CLASS` declares a lexical (sub body)');

is(run_cl(<<'PL'), "x=Human pre=1\nglobal untied\n",
{
    my $pre = 1;
    package HumanTie;
    sub TIESCALAR { bless {} }
    sub FETCH { "Human" }
    tie my($x), "HumanTie";
    print "x=$x pre=$pre\n";
    print defined(tied $HumanTie::x) ? "global tied\n" : "global untied\n";
}
PL
   '#3804: the bare-block twin');

# ── 7. a `package NAME { }` block INSIDE a switched region homes its globals
#    to NAME (the positional package), not to the region's package.
is(run_cl(<<'PL'), "U2::y=u2y U::y=u\n",
sub run {
    package U;
    package U2 { $y = "u2y" }
    print "U2::y=", $U2::y // 'u', " U::y=", $U::y // 'u', "\n";
}
run();
PL
   'a block-form package inside a switched region: its globals are ITS package\'s');

# ── 8. a name perl forces into main:: (gv.c S_gv_is_in_main) in ANY sigil:
#    `$STDOUT` after a switch is $main::STDOUT.
is(run_cl(<<'PL'), "so=ms env=ok\n",
sub run {
    package U;
    $main::STDOUT = "ms";
    @main::ENV = ("ok");
    print "so=$STDOUT env=$ENV[0]\n";
}
run();
PL
   'a forced-main name of another sigil ($STDOUT, @ENV) stays in main:: after a switch');

# ── 9-11. CONTROLS (right on the base and after the port): the `package NAME
#    { }` block twins of #3801 / #3804, and an `our` declared before the
#    switch keeps its declaring package (the in-block spelling of #475).
is(run_cl(<<'PL'), "F=2 S=2 T::F=u\n",
sub run {
    my $S = my $F = 0;
    package T { sub g { ++$F; ++$S } }
    T::g(); T::g();
    print "F=$F S=$S T::F=", $T::F // 'u', "\n";
}
run();
PL
   'control: #3801 in a `package NAME { }` block');

is(run_cl(<<'PL'), "x=Human pre=1\nglobal untied\n",
sub run {
    my $pre = 1;
    package HumanTie {
        sub TIESCALAR { bless {} }
        sub FETCH { "Human" }
        tie my($x), "HumanTie";
        print "x=$x pre=$pre\n";
        print defined(tied $HumanTie::x) ? "global tied\n" : "global untied\n";
    }
}
run();
PL
   'control: #3804 in a `package NAME { }` block');

is(run_cl(<<'PL'), "1|x|\no1=m1+ o2=q2+ Q::g=qg\n",
{ our @o; package A; push @o, "x"; package main; print scalar(@o), "|@o|\n"; }
sub run {
    our $o1 = "m1";
    package Q;
    our $o2 = "q2";
    $o1 .= "+"; $o2 .= "+"; $g = "qg";
    package main;
    print "o1=$main::o1 o2=$Q::o2 Q::g=$Q::g\n";
}
run();
PL
   'control: an `our` alias keeps its DECLARING package across a switch (#475 in-block)');

# ── 12. RULE 12: a token the binder holds no record for (here: asked about a
#    name the token does not carry) DIES naming it.  Built in-process from a
#    real binder; nothing in the production path reads a test switch.
{
    require Pl::VendorPPI;
    require Pl::Parser2;
    require Pl::Binder;
    my $src = "sub r { package P; \$z = 1; }\n";
    my $doc = PPI::Document->new(\$src);
    my $b = Pl::Binder->new($doc)->bind;
    my $parser = bless { _binder => $b }, 'Pl::Parser2';
    my ($tok) = grep { $_->content eq '$z' } @{ $doc->find('PPI::Token::Symbol') };
    my $err = eval { $parser->_requalify_home($tok, '$q'); 1 } ? '' : $@;
    like($err, qr/^PCL internal: the binder holds no record of \$q .*`\$z`/,
         'rule 12: the requalifier dies on a token the binder does not record');
}
