#!/usr/bin/env perl
# braced-bareword-deref-01.t — ${name}/@{name}/%{name}/$#{name} with a bare
# identifier are the *variable* (the lexical, or the package var if none), NOT a
# symbolic ref.  Perl: `${name}` == `$name`, `@{arr}` == `@arr`,
# `$#{arr}` == `$#arr`, `${name}[0]` == `$name[0]`, `${h}{k}` == `$h{k}`.
#
# The bug: PCL autoquoted the bareword inside the braces and emitted a
# package-only symbolic deref ((p-cast-$ "name") / (p-array-last-index "arr")),
# which never sees a lexical.  Fix: collapse Cast+Block(bareword) into the plain
# variable token in both the expression parser and the string-interpolation
# parser.  Root cause behind the paragraph_mode.t stall (sub-local lexicals).

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

plan tests => 15;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ── expression context (not inside a string) ─────────────────────────────────
test_cl('${name} is the lexical $name',
    'my $name = 42; my $x = ${name}; print "$x\n";', "42\n");

test_cl('$#{arr} is $#arr (lexical last index)',
    'my @arr = (1,2,3); my $n = $#{arr}; print "$n\n";', "2\n");

test_cl('@{arr} is @arr (lexical array)',
    'my @arr = (1,2,3); my @d = @{arr}; print "@d\n";', "1 2 3\n");

test_cl('${name}[1] is $name[1] (element of @name)',
    'my @name = (10,20,30); my $z = ${name}[1]; print "$z\n";', "20\n");

test_cl('${h}{k} is $h{k} (hash element)',
    'my %h = (a => 99); my $y = ${h}{a}; print "$y\n";', "99\n");

test_cl('%{h} is %h (lexical hash)',
    'my %h = (a => 1, b => 2); my @kv = sort %{h}; print "@kv\n";',
    "1 2 a b\n");

# ── inside string interpolation ──────────────────────────────────────────────
test_cl('"${name}" interpolates the lexical',
    'my $name = 5; print "scalar=${name}\n";', "scalar=5\n");

test_cl('"@{arr}" interpolates the lexical array',
    'my @arr = (1,2,3); print "deref=@{arr}\n";', "deref=1 2 3\n");

test_cl('"$#{arr}" interpolates the lexical last index',
    'my @arr = (1,2,3); print "lastidx=$#{arr}\n";', "lastidx=2\n");

# ── genuine symbolic refs / expression derefs still work ─────────────────────
test_cl('${$ref} still derefs an expression',
    'my $y = 7; my $r = \$y; print ${$r}, "\n";', "7\n");

test_cl('@{$ref} still derefs an arrayref',
    'my @a = (1,2); my $ar = \@a; print "@{$ar}\n";', "1 2\n");

test_cl('${"name"} is a symbolic ref to the package var',
    'no strict; our $foo = 7; my $n = "foo"; print ${$n}, " ", ${"foo"}, "\n";',
    "7 7\n");

# ── $#{$ref} / $#$ref expression-deref of last index, inside a string ─────────
# (Were producing the bogus "$#{ARRAY(0x..)}" before the string-interp fix.)
test_cl('"$#{$ref}" interpolates last index of the referenced array',
    'my @a = (1,2,3); my $ar = \@a; print "li=$#{$ar}\n";', "li=2\n");

test_cl('"$#$ref" (sigil form) interpolates last index',
    'my @a = (1,2,3); my $ar = \@a; print "li=$#$ar\n";', "li=2\n");

test_cl('"$#{$h->{list}}" nested deref last index',
    'my $h = { list => [1,2,3,4] }; print "li=$#{$h->{list}}\n";', "li=3\n");
