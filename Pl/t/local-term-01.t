#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# A `local` TERM that is not the first term of its statement (task #3370,
# RULED s518; docs/ir-spec.md "local"): a void comma list splits into one
# statement per term under the condition evaluated once, a local that is the
# first thing its statement evaluates hoists before it, anything else REFUSES
# at its statement.  Plus #3411: `local *STDOUT` in another package is main's.
# Every row but the last compares against perl (5.38-safe: core only).

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

sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code, $raw) = @_;    # $raw: a row that MEANS a refusal reads stderr itself
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my ($cl_code, $err) = $raw ? PCLCore::transpile_raw(qq{$pl2cl $pl_file})
                                 : (PCLCore::transpile(qq{$pl2cl $pl_file}), q{});
    $$raw = $err if ref $raw;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_lt {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# 1. #3370 the scalar spelling: the local happens at its term, restored at
#    the block's end; the condition is evaluated once.
test_lt('comma list: local term localizes and restores', <<'PERL');
our $g = 1;
sub f { print "g=$g\n" }
{ my %l; $l{o}++, local($g), $g = 2 if 1; f(); print "o=$l{o}\n"; }
f();
PERL

# 2. a false modifier: nothing runs, nothing is localized.
test_lt('comma list: false modifier localizes nothing', <<'PERL');
our $g = 1;
{ my $n = 0; $n++, local($g), $g = 3 if 0; print "g=$g n=$n\n"; }
print "g=$g\n";
PERL

# 3. the condition is evaluated ONCE, before the local changes what it reads.
test_lt('comma list: condition evaluated once, first', <<'PERL');
our $g = 1; my $c = 0;
sub cond { $c++; $g == 1 }
{ my $n = 0; $n++, local $g = 4, $n++ if cond(); print "g=$g n=$n c=$c\n"; }
{ my $n = 0; $n++, local $g = 6, $n++ unless !cond(); print "g=$g n=$n c=$c\n"; }
print "g=$g\n";
PERL

# 4. the glob spelling (Capture::Tiny 344) in a non-main package, under a tie
#    whose OPEN dies: the local hides the tie, the reopen works (#3411: the
#    standard handles are main's from every package).
test_lt('comma list: local(*STDOUT) under a tie, in package Foo', <<'PERL');
package TE;
sub TIEHANDLE { my $x = ''; bless \$x }
sub OPEN { die "unimplemented OPEN\n" }
sub PRINT { my $s = shift; $$s .= join '', @_; 1 }
package Foo;
tie *STDOUT, 'TE';
my %l;
{
  $l{o}++, local(*STDOUT), open(STDOUT, ">&=1") if tied *STDOUT;
  print STDERR "inside=", (tied(*STDOUT) ? 1 : 0), "\n";
  open(STDOUT, ">", "/dev/null") or die "reopen: $!";
}
print STDERR "after=", (tied(*STDOUT) ? 1 : 0), " o=$l{o}\n";
PERL

# 5. #3411 alone: a leading `local *STDOUT` in package Foo is main's STDOUT.
test_lt('local *STDOUT in package Foo localizes main::STDOUT', <<'PERL');
package TE;
sub TIEHANDLE { my $x = ''; bless \$x }
sub PRINT { my $s = shift; $$s .= join '', @_; 1 }
package Foo;
tie *STDOUT, 'TE';
{ local *STDOUT; print STDERR "a=", (tied(*STDOUT) ? 1 : 0), "\n"; }
{ local(*STDOUT) if 1; print STDERR "b=", (tied(*STDOUT) ? 1 : 0), "\n"; }
print STDERR "after=", (tied(*STDOUT) ? 1 : 0), "\n";
PERL

# 6. a local that is the first thing its statement evaluates: an argument,
#    an assignment's right side, a parenthesised right side under a modifier.
test_lt('local term first evaluated: argument / my init / modifier', <<'PERL');
our @x = ([1], [2]);
sub is_ { print "is $_[0] $_[1]\n" }
{ is_(local $x[0][0] = 19, 19); print "in $x[0][0]\n"; }
print "out $x[0][0]\n";
{ my $a = local $/ = 7; print "a=$a /=$/\n"; }
print "/=", ($/ eq "\n" ? "nl" : $/), "\n";
our $z = 1;
{ my $o = (local $z = 5) if 1; print "z=$z o=$o\n"; }
print "z=$z\n";
our $r = [5];
{ is_(local +($r//0)->[0] = 9, 9); print "in $r->[0]\n"; }
print "out $r->[0]\n";
our $q = 1;
{ if (local $q = 2) { print "q=$q\n" } print "q=$q\n"; }
print "q=$q\n";
PERL

# 7. an inner local of the SAME operand under a leading one changes nothing.
test_lt('local @b = local(@b) = LIST', <<'PERL');
our @bee = (1);
{ local @bee = local(@bee) = qw(foo bar); print "@bee\n"; }
print "@bee\n";
PERL

# 8. a local no lowering reaches is a REFUSAL at its statement, not a silent
#    read (perl: "1 9 1"; PCL refuses perl-shaped, trappable).
{
    my $code = <<'PERL';
our @x = ([1]);
my @r = eval { @{[1, local $x[0][0] = 9]} };
print "err=", ($@ =~ /PCL: local in this expression position is not supported/ ? 1 : 0), "\n";
print "x=$x[0][0]\n";
PERL
    my $err;
    is(run_cl($code, \$err), "err=1\nx=1\n", 'unreached local term refuses at its statement');
    like($err, qr/^PCL: refused statement at .* line 2: .*local in this expression position/m,
         '... and the transpile announces the refusal');
}

done_testing();
