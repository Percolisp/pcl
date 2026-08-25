#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# punct-glob-name-01.t — task #463 items 3-5: a glob whose NAME is punctuation,
# a digit run, or hidden behind perl's `;` block disambiguator.
#
# perl names a glob with any punctuation character or digit run, exactly as it
# names a scalar, and real code writes them:
#
#     *X = *-;             t/re/reg_namedcapture.t:18  — %X becomes %-
#     local *a = *1;       t/re/subst.t:951            — $a becomes $1
#     local *1 = sub {…};  t/op/method.t:38            — the numeric glob as an LVALUE
#     *{;undef} = 3;       t/op/gv.t:1020              — asserts the DIE message
#
# PPI's Symbol regex is word-bounded, so it hands `*-` over as two ordinary
# operator tokens (docs/ppi-upstream-bugs.md §26) and every one of these was
# DROPPED — "Got op '-', not postfix.  But there is nothing after it??".
# Pl::Parser2::_repair_punct_glob_name rewrites the name into the symbolic
# spelling `*{'-'}` the compiler already lowers; the `;` family needed no lexer
# fix at all, only Pl::Parser2::_normalize_null_statements, because
# PPI::Statement::Null is INSIGNIFICANT and the walker was the only thing that
# could see it.
#
# THE NEGATIVES ARE THE POINT.  The repair's condition is a WHITELIST of the
# positions where a `*` can OPEN a glob name (statement/list start, after `=`,
# `,` or `return`), not `_ends_term`'s negative, because a false positive here
# turns working multiplication into a glob and kills the file, while a miss
# costs only today's drop.  The measured
# term-position `*` sites in the four populations that must NOT be touched are
# all here: multiplication after a deref block (`${$r}{k}*2`, where `_ends_term`
# itself says the term has not ended), after a subscript, after a call, and a
# glob PATTERN.  Every expectation is the live `perl` answer.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 8;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the bug ---------------------------------------------------------------

both_agree(<<'PL', 'a PUNCTUATION glob name: *X = *- (t/re/reg_namedcapture.t:18)');
no strict 'refs';
our %X; *X = *-;
'X' =~ /(?<X>X)/;
print "aliased=", (defined $X{X} ? "yes" : "no"), "\n";
PL

both_agree(<<'PL', 'a DIGIT glob name: local *a = *1 (t/re/subst.t:951)');
no strict 'refs';
"hello" =~ /(ell)/;
our $a;
{ local *a = *1; print "a=[$a]\n"; }
PL

both_agree(<<'PL', "perl's `;` block disambiguator, both sigils (t/op/gv.t:1020)");
no strict 'refs';
our $a1 = "A"; our @z = (7,8);
my $x = ${;"a1"};  print "1=[$x]\n";
my @y = @{;"z"};   print "2=[@y]\n";
# The same Null statement in a BARE block — t/op/sub_lval.t:1066, which is
# where the census counted this one.
sub bare { {; @z } }
my @b = bare(); print "3=[@b]\n";
PL

# The `;` does more than force the BLOCK reading: inside one a lone bareword is
# an EXPRESSION where a plain deref reads it as the NAME.  Deleting the `;` and
# stopping there emitted the glob literally named "undef".
both_agree(<<'PL', 'a bareword inside a FORCED block is a call, not a name');
no strict 'refs';
sub foo { "bar" }
our $bar = 5; our $foo = 9;
print "A=[", ${foo}, "]\n";
print "B=[", ${;foo}, "]\n";
print "C=[", ${ foo() }, "]\n";
print "D=[", *{;foo}, "]\n";
print "E=[", *{foo}, "]\n";
PL

# The die message is compared without its " at FILE line N." tail: PCL's
# location for a runtime die is its own (`(eval 0) line 0`), a general
# divergence and not what this row is about.
{
    my $code = <<'PL';
no strict 'refs';
my $ok = eval { *{;undef} = 3; 1 };
my $e = $ok ? "no-die" : $@;
$e =~ s/ at .*//s;
print "err=[$e]\n";
PL
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "*{;undef} = 3 dies as a symbol reference (perl: "
                    . ($perl =~ s/\n/\\n/gr) . ")");
}

# A digit glob as an ASSIGNMENT TARGET, in the spelling the repair does cover:
# at statement start.  (`local *1 = sub {…}`, t/op/method.t:38, is deliberately
# NOT covered — `local *{EXPR}` loses the statement silently, task #564 — so it
# keeps dropping loudly until that closes.)
both_agree(<<'PL', 'a DIGIT glob as an assignment TARGET installs the sub');
no strict 'refs';
my $one = "1";
*1 = sub { 123 };
print "res=[", &$one(), "]\n";
PL

# ---- the negatives ---------------------------------------------------------

both_agree(<<'PL', 'negative: every multiplication the whitelist must not claim');
my $n = 3; print "1=[", $n*1, "]\n";
my @a = (1,2,3); my %h = (k=>4);
print "2=[", $h{k}*2, "]\n";
print "3=[", scalar(@a)*2, "]\n";
print "4=[", (2)*-1, "]\n";
my $r = {k=>5}; print "5=[", ${$r}{k}*2, "]\n";
print "6=[", 2*!0, "]\n";
sub sz { 7 } print "7=[", sz()*3, "]\n";
my $hr = {n=>6}; print "8=[", @{[1,2]}*3, "]\n";
PL

both_agree(<<'PL', 'negative: a glob PATTERN and the ordinary glob spellings');
no strict 'refs';
my @g = sort glob("./nope-*-xyz");
print "1=[", scalar(@g), "]\n";
our $g2 = "GEE"; our $p2; *p2 = *g2; print "2=[$p2]\n";
*{"n2"} = \$g2; print "3=[", ${"n2"}, "]\n";
my @f = sort <*.no-such-suffix>;
print "4=[", scalar(@f), "]\n";
PL
