#!/usr/bin/env perl
# pos-01.t: pos() function and /g iterative match tests

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^caught .*\n//gm;
    $out =~ s/^compilation unit.*\n//gm;
    $out =~ s/^\s*Undefined.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 8;

# Basic /g pos tracking
is run_cl(<<'END'), "2\n", '/g match sets pos';
my $x = 'banana'; $x =~ /.a/g; print pos($x), "\n";
END

# /gc keeps pos on failed match
is run_cl(<<'END'), "2\n", '/gc keeps pos on failure';
my $x = 'banana'; $x =~ /.a/g; $x =~ /.z/gc; print pos($x), "\n";
END

# Second /g continues from stored pos
is run_cl(<<'END'), "4\n", 'second /g continues from pos';
my $x = 'banana'; $x =~ /.a/g; $x =~ /.a/g; print pos($x), "\n";
END

# Assigning to the variable resets pos
is run_cl(<<'END'), "4\n", 'assign resets pos, then new match works';
my $x = 'banana'; $x =~ /.a/g; $x =~ /.a/g;
$x = '123 56'; $x =~ / /g; print pos($x), "\n";
END

# pos returns undef when no /g in progress
is run_cl(<<'END'), "undef\n", 'pos returns undef with no active match';
my $x = 'hello';
print defined(pos($x)) ? pos($x) : "undef", "\n";
END

# Failed /g (no /c) resets pos to undef
is run_cl(<<'END'), "undef\n", 'failed /g resets pos to undef';
my $x = 'banana'; $x =~ /.a/g; $x =~ /NOMATCH/g;
print defined(pos($x)) ? pos($x) : "undef", "\n";
END

# while /g loop
is run_cl(<<'END'), "2\n4\n6\n", 'while /g loop iterates correctly';
my $x = 'banana';
while ($x =~ /.a/g) { print pos($x), "\n"; }
END

# /g in list context returns all matches
is run_cl(<<'END'), "ba:na:na\n", '/g list context returns all matches';
my $x = 'banana';
my @m = ($x =~ /.a/g);
print join(':', @m), "\n";
END
