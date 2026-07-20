#!/usr/bin/env perl
# raw-verdict-01.t - B-regime scan-licensed freeze verdicts (task #62,
# docs/raw-numeric-verdict.md): raw-numeric / raw-string slots for variables
# whose writes are unproven but whose USES all license one family, with every
# native write routed through the strict freeze coercers.
#
# Transpile checks pin the verdict (wrapper present/absent); runtime checks
# pin value fidelity vs perl for the trap cases ("0.0" truthiness, ref
# stable-ID identity, aggregate scalar-context collapse).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;
use Pl::Parser2;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 24;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ---- verdict shape checks (transpile only) --------------------------------

# The bench shape: element-seeded loop bound, all uses numeric → raw-numeric.
my $cl = Pl::Parser2->parse_code(
  q{my %h=(k=>5); my $n = $h{k}; my $s=0; for (my $i=0; $i<$n; $i++) { $s+=$i } print "$s\n";});
like($cl, qr/\(\$n \(%pcl-to-number-strict /, 'element-seeded numeric bound: B-num freeze');

# All-string uses (interpolation, length, bool) → raw-string; bool licenses str.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>"x"); my $m = $h{k}; if ($m) { print "got $m\n"; } print length($m),"\n";});
like($cl, qr/\(\$m \(%pcl-to-string-strict /, 'string/bool uses: B-str freeze');

# Boolean context DISQUALIFIES raw-numeric ("0.0"/"00"/" " are true strings
# that numify false) — a bool + num mix stays boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>"0.0"); my $n = $h{k}; print "T\n" if $n; print $n+1,"\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'bool+num mixed uses stay boxed (the "0.0" trap)');

# defined() is a call arg → opaque → boxed (freeze would make undef defined).
$cl = Pl::Parser2->parse_code(
  q{my %h; my $n = $h{k}; print defined($n)?"d":"u"; print $n+1,"\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'defined() use disqualifies');

# Dereference → opaque → boxed (freeze would break the ref).
$cl = Pl::Parser2->parse_code(q{my %h=(k=>[1]); my $r = $h{k}; print $r->[0],"\n";});
like($cl, qr/\(\$r \(make-p-box nil\)\)/, 'deref use disqualifies');

# Range endpoint is TYPE-SENSITIVE (magical string range) → opaque → boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>3); my $n = $h{k}; for my $i (1..$n) { print $i } print "\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'range endpoint use disqualifies');

# & | ^ are TYPE-SENSITIVE (string bitwise on two strings) → opaque → boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>6); my $n = $h{k}; print $n & 3, "\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'bitwise & use disqualifies');

# Unary minus is TYPE-SENSITIVE (-"abc" eq "-abc") → opaque → boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>3); my $n = $h{k}; print -$n, "\n"; print $n+1,"\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'unary minus use disqualifies');

# A read hidden in a regex PATTERN interpolates (stringify) — licenses B-str,
# disqualifies B-num even when every visible use is numeric.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>3); my $n = $h{k}; print $n+1,"\n"; print "y\n" if "x3" =~ /x$n/;});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'regex-pattern interpolation blocks B-num');

# `use overload` anywhere in the file disables both B-verdicts.
$cl = Pl::Parser2->parse_code(
  q{package O; use overload '""' => sub {"o"}; package main; my %h=(k=>5); my $n=$h{k}; print $n+1,"\n";});
unlike($cl, qr/%pcl-to-number-strict/, 'use overload in file disables freezes');

# Sub params never freeze (caller-bound initial value).
$cl = Pl::Parser2->parse_code(
  q{sub f { my ($n) = @_; return $n + 1; } print f(2), "\n";});
unlike($cl, qr/%pcl-to-\w+-strict \$n/, 'sub params never freeze');

# ---- runtime fidelity ------------------------------------------------------

test_cl('B-num loop bound computes identically',
    'my %h=(k=>4); my $n=$h{k}; my $s=0; for (my $i=0; $i<$n; $i++){ $s+=$i } print "$s\n";',
    "6\n");

test_cl('B-str prints the exact element string',
    'my %h=(k=>"0.0"); my $m=$h{k}; print "T\n" if $m; print "[$m]\n";',
    "T\n[0.0]\n");

test_cl('ref stable-ID: frozen key matches a later live read',
    'my %h=(k=>{x=>1}); my $r=$h{k}; my %seen; $seen{$r}=1;
     my $r2=$h{k}; print exists $seen{$r2} ? "same" : "diff", "\n";',
    "same\n");

test_cl('aggregate scalar-context collapse inside the freeze',
    'our @a; my $n = @a = split(/,/, "a,b,c"); print "$n\n";',
    "3\n");

test_cl('undef freeze: numeric slot sees 0, like perl at first numeric use',
    'my %h; my $n=$h{nope}; print $n+1,"\n";',
    "1\n");

# ---- S1 str-buffer (fill-pointer append) ----------------------------------

# Accumulator with only `.=` writes and transient uses → buffer.
$cl = Pl::Parser2->parse_code(
  q{my $s = ""; for (my $i=0; $i<10; $i++) { $s .= "ab"; } print "$s\n";});
like($cl, qr/\(\$s \(%pcl-str-buffer ""\)\)/, 'S1: accumulator init becomes a buffer');
like($cl, qr/\(%pcl-str-append \$s "ab"\)/,   'S1: .= appends in place');

# A bare-copy alias escape (opaque use) blocks the buffer (the alias must
# not observe later in-place appends).
$cl = Pl::Parser2->parse_code(
  q{my $s=""; $s .= "a"; my $t = $s; print "$t$s\n";});
unlike($cl, qr/%pcl-str-buffer/, 'S1: alias escape blocks buffer');

# A hash-key use is RETAINED by the table → blocks buffer (still B-str ok).
$cl = Pl::Parser2->parse_code(
  q{my $s=""; $s .= "ab"; my %h; $h{$s}=1; print "$s\n";});
unlike($cl, qr/%pcl-str-buffer/, 'S1: hash-key use blocks buffer');

# Any non-.= compound write (x=) blocks buffer.
$cl = Pl::Parser2->parse_code(q{my $s="x"; $s .= "y"; $s x= 2; print "$s\n";});
unlike($cl, qr/%pcl-str-buffer/, 'S1: x= write blocks buffer');

# foreach range var never buffers (bound by the loop macro, not an init).
$cl = Pl::Parser2->parse_code(q{for my $i (1..3) { $i .= "x"; print "$i\n"; }});
unlike($cl, qr/%pcl-str-buffer/, 'S1: foreach range var never buffers');

test_cl('S1 runtime: append loop matches perl',
    'my $s = ""; for (my $i=0; $i<5; $i++) { $s .= "ab"; }
     print "$s\n"; print length($s), "\n"; print "T\n" if $s;',
    "ababababab\n10\nT\n");

test_cl('S1 runtime: self-append is safe',
    'my $s = "ab"; $s .= $s; $s .= $s; print "$s\n"; print "eq\n" if $s eq "abababab";',
    "abababab\neq\n");
