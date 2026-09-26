#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# deref-vivify-01.t — task #2341: a NEVER-WRITTEN `my $x;` that is
# DEREFERENCED is a vivification target, so it must have a PLACE (a box).
#
# The raw-slot verdict (Pl::VarAnnotator, Kind-A `raw-slot`) used to give
# `my $u;` a raw `:scalar` slot holding the VALUE undef, and a deref of a raw
# undef has nowhere to put the array it creates: `my $u; push @$u, 1` DIED
# ("Type of arg 1 to push must be array"), and `$u->[0]` / `for (@$u)` /
# `keys %$u` / slices / `pop` left $u undef where perl makes it a reference.
# The rule (VarAnnotator's `deref-of-unwritten` reason): a scalar with ZERO
# writes and at least one deref is boxed.  A WRITTEN scalar keeps its verdict
# (the s473b trap: boxing every chain root cost an accessor loop +97 %), so
# the breaking cases below — an accessor parameter, a never-dereferenced
# `my $x;`, a counter, a string buffer — must stay raw.
#
# The run rows are one program, each line an independent probe (each probe is
# its own sub, i.e. its own VarAnnotator region); expected = perl 5.40.3's
# output of the same source, recorded at authoring time.
#
# Also guarded here, found on the way (all runtime): ref() of a box holding
# CL NIL answered ARRAY (`(listp nil)`), `$$u = 1` wrote into $u itself
# instead of vivifying a SCALAR ref, an rvalue element read / `$#$u` did not
# vivify its box, and a RAW undef reached p-cast-@'s fallback, which answered
# undef — ONE element to every list consumer.

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub cl_of { PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

sub run_cl {
    my ($code) = @_;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh cl_of($code);
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---- the verdict: which slots are boxed (transpile only, no SBCL) ---------

like(cl_of('my $u; push @$u, 1; print scalar(@$u), "\n";'),
     qr/\(\$u :box /, 'never-written + push @$u: boxed');
like(cl_of('my $x; my $e = $x->[0]; print ref($x), "\n";'),
     qr/\(\$x :box /, 'never-written + $x->[0] read: boxed');
like(cl_of('my $s; my @sl = @$s[0,1]; print ref($s), "\n";'),
     qr/\(\$s :box /, 'never-written + slice base: boxed');
like(cl_of('my $u; my $t = "[@$u]"; print $t, "\n";'),
     qr/\(\$u :box /, 'never-written + "@$u" interpolation: boxed');
like(cl_of('my $u; my $f = sub { push @$u, 1 }; $f->(); print scalar(@$u), "\n";'),
     qr/\(\$u :box /, 'never-written + deref inside a closure body: boxed');
# BREAKING cases: must stay raw.
like(cl_of('my $x; print defined $x ? 1 : 0, "\n";'),
     qr/\(\$x :scalar /, 'never dereferenced: stays raw');
like(cl_of('sub acc { my $s = shift; return $s->{x} } print acc({x => 5}), "\n";'),
     qr/p-raw-params \(\(\$s :scalar\)\)/, 'accessor parameter (a write): stays raw');
like(cl_of('my %h = (k => 1); my $k; my $v = $h{$k // "k"}; print "$v\n";'),
     qr/\(\$k :scalar /, 'never-written used only as a hash KEY: stays raw');
like(cl_of('my @l = qw(a b); my $acc = ""; for my $w (@l) { $acc .= $w } print "$acc\n";'),
     qr/\(\$acc :str-buffer /, 'string buffer: unchanged');
like(cl_of(q{sub g { my $s = shift; my @k = keys %$s; return scalar(@k) } print g({a => 1}), "\n";}),
     qr/p-raw-params \(\(\$s :scalar\)\)/, 'a WRITTEN scalar (a parameter) that is dereferenced keeps its verdict');

# ---- the run rows ----------------------------------------------------------

my $prog = <<'PERL';
sub r01 { my $u; push @$u, 1, 2; print "r01 ", scalar(@$u), "\n" }
sub r02 { my $u; unshift @$u, 1, 2; print "r02 ", scalar(@$u), "\n" }
sub r03 { my $u; splice @$u, 0, 0, 1; print "r03 ", scalar(@$u), "\n" }
sub r04 { use strict; my $x; my $e = $x->[0]; print "r04 ", ref($x), "\n" }
sub r05 { use strict; my $y; for (@$y) { } print "r05 ", ref($y), "\n" }
sub r06 { use strict; my $z; my @k = keys %$z; print "r06 ", ref($z), "\n" }
sub r07 { use strict; my $s; my @sl = @$s[0,1]; print "r07 ", ref($s), "\n" }
sub r08 { use strict; my $p; my $v = pop @$p; print "r08 ", ref($p), "\n" }
sub r09 { my $d; my @e = @$d; print "r09 ", scalar(@e), "\n" }
sub r10 { my $z; my @v = values %$z; my @e = each %$z; print "r10 ", ref($z), "\n" }
sub r11 { my $u; my $n = $#$u; print "r11 $n ", ref($u), "\n" }
sub r12 { my $u; my $r = \@$u; print "r12 ", ref($u), " ", ($r == $u ? "same" : "diff"), "\n" }
sub r13f { scalar @_ }
sub r13 { my $u; my $n = r13f(@$u); print "r13 $n ", ref($u), "\n" }
sub r14 { my $u; my @m = map { $_ } @$u; print "r14 ", ref($u), "\n" }
sub r15 { my $u; my @g = grep { $_ } @$u; print "r15 ", ref($u), "\n" }
sub r16 { my $u; my @s = sort @$u; print "r16 ", scalar(@s), "\n" }
sub r17 { my $u; @$u = (1, 2); print "r17 ", scalar(@$u), "\n" }
sub r18 { my $u; %$u = (a => 1); print "r18 ", join(",", keys %$u), "\n" }
sub r19 { my $u; $$u = 1; print "r19 ", ref($u), " $$u\n" }
sub r20 { my $u; $u->[0]{k} = 1; print "r20 ", ref($u), " ", ref($u->[0]), "\n" }
sub r21 { my ($p, $q); push @$p, 1; push @{$q}, 2; print "r21 ", scalar(@$p), scalar(@$q), "\n" }
sub r22 { my $r; my $t = $r; push @$r, 1; print "r22 ", scalar(@$r), "\n" }
sub r23 { for my $i (1 .. 2) { my $acc; push @$acc, $i; print "r23 ", scalar(@$acc), "\n" } }
sub r24f { my $l; push @$l, @_; $l }
sub r24 { print "r24 ", scalar(@{ r24f(1, 2, 3) }), "\n" }
sub r25 { my $u; my $f = sub { push @$u, 1 }; $f->(); $f->(); print "r25 ", scalar(@$u), "\n" }
sub r26 { my $u; { push @$u, 1 } print "r26 ", scalar(@$u), "\n" }
sub r27 { my $u; eval q{ push @$u, 1 }; print "r27 ", scalar(@$u), "\n" }
sub r28 { my $u; push @$u, 1 for 1 .. 3; print "r28 ", scalar(@$u), "\n" }
sub r30 { my $u; my $s = "[$u->[0]]"; print "r30 $s ", ref($u), "\n" }
sub r33 { my $u; $u->{a}{b} = 1; print "r33 ", ref($u), " ", ref($u->{a}), "\n" }
sub r34 { my $u; my $e = exists $u->{a}{b}; print "r34 ", ref($u), " ", ref($u->{a}), "\n" }
sub r35 { my $u; my $e = $u->{a}{b}; print "r35 ", ref($u), " ", ref($u->{a}), "\n" }
sub r36 { my $u; my $v = shift @$u; print "r36 ", ref($u), "\n" }
sub r37 { my $u; my @h = @$u{qw(a b)}; print "r37 ", ref($u), "\n" }
sub r38 { my $u; my $v = $$u[1]; print "r38 ", ref($u), "\n" }
sub r39 { my $u; my $v = ${$u}{k}; print "r39 ", ref($u), "\n" }
sub r40 { my $u; my @s = $u->@*; print "r40 ", scalar(@s), "\n" }
sub r41 { my $w; my $r = \$w; print "r41 [", ref($w), "]\n" }
sub r42 { my @e = @{ undef() }; my %h = %{ undef() }; print "r42 ", scalar(@e), scalar(keys %h), "\n" }
sub b01 { my $n = 0; $n++ for 1 .. 10; print "b01 $n\n" }
sub b02f { my $s = shift; $s->{x} }
sub b02 { print "b02 ", b02f({x => 5}), "\n" }
sub b03 { my $x; print "b03 ", (defined $x ? 1 : 0), "\n" }
sub b04 { my @l = qw(a b c); my $acc = ""; $acc .= $_ for @l; print "b04 $acc\n" }
for my $r (qw(r01 r02 r03 r04 r05 r06 r07 r08 r09 r10 r11 r12 r13 r14 r15 r16
              r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r30 r33 r34
              r35 r36 r37 r38 r39 r40 r41 r42 b01 b02 b03 b04)) {
  eval { no strict 'refs'; &$r(); 1 } or print "$r DIED\n";
}
PERL

# perl 5.40.3's output of $prog, line for line.
my @want = split /\n/, <<'WANT';
r01 2
r02 2
r03 1
r04 ARRAY
r05 ARRAY
r06 HASH
r07 ARRAY
r08 ARRAY
r09 0
r10 HASH
r11 -1 ARRAY
r12 ARRAY same
r13 0 ARRAY
r14 ARRAY
r15 ARRAY
r16 0
r17 2
r18 a
r19 SCALAR 1
r20 ARRAY HASH
r21 11
r22 1
r23 1
r23 1
r24 3
r25 2
r26 1
r27 1
r28 3
r30 [] ARRAY
r33 HASH HASH
r34 HASH HASH
r35 HASH HASH
r36 ARRAY
r37 HASH
r38 ARRAY
r39 HASH
r40 0
r41 []
r42 00
b01 10
b02 5
b03 0
b04 abc
WANT

my @got = split /\n/, run_cl($prog);
for my $i (0 .. $#want) {
    my ($tag) = $want[$i] =~ /^(\S+)/;
    is($got[$i] // '<missing>', $want[$i], "run: $tag");
}
is(scalar(@got), scalar(@want), 'run: no extra output lines');

# ---- no-strict RVALUE deref of an undef BOX -----------------------------------
# perl never vivifies on these (it reads the symbolic `@{""}`).  The rvalue
# site carries `:rvalue` (#2103), so the box is left undef; what is still
# wrong is the SCALAR count of that empty symbolic array: perl says undef.
is(run_cl('sub r29 { my $u; my $s = "[@$u]"; print "r29 $s ", ref($u), "\n" } r29();'),
   "r29 [] \n", 'no-strict rvalue "@$undef": no vivification');
TODO: {
    local $TODO = '#2408: a no-strict scalar(@$undef) is 0, perl says undef';
    is(run_cl('sub r31 { my $u; my $n = @$u; print "r31 [$n]\n" } r31();'),
       "r31 []\n", 'no-strict scalar(@$undef): undef');
}

done_testing();
