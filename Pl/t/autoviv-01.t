#!/usr/bin/env perl
# autoviv-01.t - Tests for hash autovivification

use v5.30;
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../..";
use Test::More;

# Load test utilities from transpile-test-01.t pattern
use Pl::Parser;

# Skip if SBCL not available
my $sbcl_version = `sbcl --version 2>/dev/null`;
plan skip_all => "SBCL not available" unless $sbcl_version;

plan tests => 38;

# Helper to run Perl code
sub run_perl {
    my ($code) = @_;
    my $full = q{use v5.30; use strict; use warnings; } . $code;
    my $result = `perl -e '$full' 2>&1`;
    chomp $result;
    return $result;
}

# Helper to run Common Lisp code
sub run_cl {
    my ($lisp_code) = @_;
    my $lisp_file = "/tmp/pcl-autoviv-$$.lisp";
    open my $fh, ">", $lisp_file or die "Can't write $lisp_file: $!";
    print $fh $lisp_code;
    close $fh;

    my $result = `sbcl --noinform --non-interactive --load "$FindBin::Bin/../../cl/pcl-runtime.lisp" --load "$lisp_file" 2>&1`;
    unlink $lisp_file;

    # Filter out SBCL noise
    my @lines = split /\n/, $result;
    my @output;
    for my $line (@lines) {
        next if $line =~ /^;|caught WARNING|undefined variable|compilation unit|file:/;
        next if $line =~ /^\s*$/;
        next if $line eq "PCL Runtime loaded";
        push @output, $line;
    }
    return join("\n", @output);
}

# Helper to transpile and compare
sub test_transpile {
    my ($name, $perl_code) = @_;

    my $parser = Pl::Parser->new(code => $perl_code);
    my @cl = $parser->parse();
    my $cl_code = join("\n", @cl);

    my $perl_out = run_perl($perl_code);
    my $cl_out = run_cl($cl_code);

    is($cl_out, $perl_out, $name) or do {
        diag "Perl output: $perl_out";
        diag "CL output: $cl_out";
        diag "Generated CL:\n$cl_code";
    };
}

diag "";
diag "-------- Basic autovivification:";

test_transpile("two-level hash autovivification", '
my %h = ();
$h{a}{b} = 42;
print $h{a}{b};
');

test_transpile("three-level hash autovivification", '
my %h = ();
$h{x}{y}{z} = 99;
print $h{x}{y}{z};
');

test_transpile("multiple keys at same level", '
my %h = ();
$h{a}{b} = 1;
$h{a}{c} = 2;
print $h{a}{b}, "-", $h{a}{c};
');

test_transpile("multiple branches", '
my %h = ();
$h{a}{x} = 10;
$h{b}{y} = 20;
print $h{a}{x}, "-", $h{b}{y};
');

diag "";
diag "-------- Empty hash initialization:";

test_transpile("empty hash init", '
my %h = ();
$h{key} = "value";
print $h{key};
');

test_transpile("hash with initial values then autoviv", '
my %h = (top => "level");
$h{nested}{key} = "deep";
print $h{top}, "-", $h{nested}{key};
');

diag "";
diag "-------- Reading autovivified values:";

test_transpile("read after autoviv write", '
my %h = ();
$h{a}{b}{c} = "found";
my $val = $h{a}{b}{c};
print $val;
');

test_transpile("check intermediate hash exists", '
my %h = ();
$h{a}{b} = 1;
print ref($h{a}) eq "HASH" ? "yes" : "no";
');

diag "";
diag "-------- Edge cases:";

test_transpile("overwrite autovivified value", '
my %h = ();
$h{a}{b} = 1;
$h{a}{b} = 2;
print $h{a}{b};
');

test_transpile("numeric keys", '
my %h = ();
$h{1}{2}{3} = "num";
print $h{1}{2}{3};
');

diag "";
diag "-------- Hash initialization with multiple pairs:";

test_transpile("hash init with multiple pairs", '
my %h = (a => 1, b => 2, c => 3);
print $h{a}, "-", $h{b}, "-", $h{c};
');

test_transpile("hash init multiple pairs then autoviv", '
my %h = (x => 10, y => 20);
$h{z}{deep} = 30;
print $h{x}, "-", $h{y}, "-", $h{z}{deep};
');

diag "";
diag "-------- Mixed hash/array autovivification:";

test_transpile("hash then array autoviv", '
my %h = ();
$h{foo}{bar}[5] = "five";
print $h{foo}{bar}[5];
');

test_transpile("deeper mixed nesting", '
my %h = ();
$h{a}[0]{b}[1] = "nested";
print $h{a}[0]{b}[1];
');

diag "";
diag "-------- Reading non-existent paths (should be undef, no error):";

test_transpile("read missing path returns undef", '
my %h = ();
$h{foo}{bar}[5] = 1;
my $val = $h{other}{path};
print defined($val) ? "defined" : "undef";
');

test_transpile("read different branch returns undef", '
my %h = ();
$h{foo}{bar}[5] = 1;
my $val = $h{bar}{foo}[1];
print defined($val) ? "defined" : "undef";
');

diag "";
diag "-------- Autovivification through undef refs:";

test_transpile("hashref autoviv: undef scalar -> hashref on assignment", '
my $ref;
$ref->{key} = "val";
print $ref->{key};
');

test_transpile("arrayref autoviv: undef scalar -> arrayref on assignment", '
my $ref;
$ref->[0] = "elem";
print $ref->[0];
');

test_transpile("nested ref autoviv: undef -> hashref -> arrayref", '
my $ref;
$ref->{A}[0] = "nested";
print $ref->{A}[0];
');

test_transpile("nested ref autoviv: write then read", '
my $ref;
$ref->{x}[2] = 99;
print defined($ref->{x}[2]) ? "ok" : "undef";
');

test_transpile("nested hash ref with two keys", '
my $ref;
$ref->{a} = 1;
$ref->{b} = 2;
print $ref->{a}, "-", $ref->{b};
');

test_transpile("array of hashrefs via autoviv", '
my $ref;
$ref->[0]{name} = "alice";
$ref->[1]{name} = "bob";
print $ref->[0]{name}, "-", $ref->[1]{name};
');

diag "";
diag "-------- Copying autovivified nested refs to scalars (regression: box-set must not scalarize a ref):";

# An autovivified hash element holds a hash REFERENCE, not a bare %hash.
# Copying it to a scalar must preserve the ref, not collapse to the key-count.
test_transpile("copy autovivified nested hashref to scalar", '
my %h;
$h{a}{b} = 1;
$h{a}{c} = 2;
my $r = $h{a};
print ref($r), " ", scalar(keys %$r);
');

test_transpile("copy autovivified nested arrayref to scalar", '
my %h;
$h{a}[0] = 10;
$h{a}[1] = 20;
my $r = $h{a};
print ref($r), " ", scalar(@$r);
');

test_transpile("copy nested arrayref-in-array to scalar", '
my @a;
$a[0][0] = 1;
$a[0][1] = 2;
my $r = $a[0];
print ref($r), " ", scalar(@$r);
');

test_transpile("copy nested hashref-in-array to scalar", '
my @a;
$a[0]{x} = 1;
$a[0]{y} = 2;
my $r = $a[0];
print ref($r), " ", scalar(keys %$r);
');

test_transpile("autovivified slot stringifies as a HASH ref", '
my %h;
$h{a}{b} = 1;
my $r = $h{a};
print( ($r =~ /^HASH\(0x[0-9a-f]+\)$/) ? "isref" : "notref" );
');

test_transpile("ref through fully-qualified package hash element", '
$Pkg::data{k}{inner} = 5;
my $r = $Pkg::data{k};
print ref($r), " ", $r->{inner};
');

diag "";
diag "-------- Initializing with hash/array constants and storing in containers:";

test_transpile("store anon hashref constant in hash element", '
my %h;
$h{cfg} = { x => 1, y => 2 };
my $r = $h{cfg};
print ref($r), " ", $r->{x} + $r->{y};
');

test_transpile("store anon arrayref constant in array element", '
my @a;
$a[0] = [ 10, 20, 30 ];
my $r = $a[0];
print ref($r), " ", $r->[0] + $r->[1] + $r->[2];
');

test_transpile("array of hashrefs (list of constants)", '
my @recs = ( { n => "a" }, { n => "b" }, { n => "c" } );
my $r = $recs[1];
print ref($r), " ", $r->{n};
');

test_transpile("hash of arrayrefs (constants)", '
my %h = ( evens => [2,4,6], odds => [1,3,5] );
my $r = $h{odds};
print ref($r), " ", join(",", @$r);
');

test_transpile("nested arrayref constant, copy inner to scalar", '
my $aoa = [ [1,2], [3,4] ];
my $row = $aoa->[1];
print ref($row), " ", $row->[0], $row->[1];
');

test_transpile("nested hashref constant, copy inner to scalar", '
my $h = { a => { deep => 9 } };
my $inner = $h->{a};
print ref($inner), " ", $inner->{deep};
');

test_transpile("push hashrefs into array then copy out", '
my @a;
push @a, { id => $_ } for 1..3;
my $r = $a[2];
print ref($r), " ", $r->{id};
');

test_transpile("array element is a copied autovivified hashref, then mutate", '
my %src;
$src{a}{b} = 1;
my @dst;
$dst[0] = $src{a};
$dst[0]{c} = 2;
print join(",", map { "$_=$src{a}{$_}" } sort keys %{$src{a}});
');

test_transpile("scalar copy of bare hash IS the key count (not a regression)", '
my %h = (a => 1, b => 2, c => 3);
my $n = %h;
print( ($n =~ m{^3(/\d+)?$}) ? "count-ok" : "got:$n" );
');

test_transpile("scalar copy of bare array IS the element count", '
my @a = (10, 20, 30, 40);
my $n = @a;
print $n;
');

done_testing();
