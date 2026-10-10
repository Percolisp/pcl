#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# raw-slot-arg-01.t - a REFERENCE argument bound into a p-raw-params slot
# (task #3081).  A raw slot holds a raw value or a CONTAINER box; a ref value
# whose referent is a box (`\$x`, `\\[1]`, `$$v` of a ref-to-ref) is bound in a
# fresh container by %p-param-copy, else `$$v` read one level too deep:
# `d3(\\\\[2])` printed \\'2' where perl prints \\\\A.  Every row is perl's
# own output (run here), and the same programs under PCL_OPT=-raw-slot (the
# general @_ binding) must agree too.

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

plan tests => 6;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($pl_file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# One program, three answers: perl, PCL, PCL with the raw-slot lever off.
sub both_ways {
    my ($name, $code) = @_;
    my $pl_file = write_pl($code);
    my $want = `$^X $pl_file 2>&1`;
    is(run_cl($pl_file), $want, "$name (perl: " . ($want =~ s/\n/\\n/gr) . ")");
    local $ENV{PCL_OPT} = '-raw-slot';
    is(run_cl($pl_file), $want, "$name, PCL_OPT=-raw-slot");
}

both_ways("#3081 recursion on \$\$v: both depths, tail and non-tail, element and other-callee", <<'PL');
sub d3 { my ($v) = @_; return "'$v'" if !ref $v; return 'A' if ref $v eq 'ARRAY'; return '\\' . d3($$v) }
print d3(\\[1]), "\n"; print d3(\\\\[2]), "\n"; print d3(\\\\\\[3]), "\n";
sub e3 { my ($v) = @_; return "'$v'" if !ref $v; return 'A' if ref $v eq 'ARRAY'; my $s = '\\' . e3($$v); return $s }
print e3(\\\\[2]), "\n";
sub g { my ($v) = @_; ref $v ? 'R' . g($$v) : "'$v'" } print g(\\\\5), "\n";
my @a = (\\\\[7]); print d3($a[0]), "\n";
sub h { my ($v) = @_; return ref $v ? 'H' : "'$v'" } sub k { my ($v) = @_; return '\\' . h($$v) } print k(\\[1]), "\n";
sub m4 { my ($v) = @_; return "'$v'" if !ref $v; return 'A' if ref $v eq 'ARRAY'; return '\\' . m4(${$v}) }
print m4(\\\\[2]), "\n";
sub c1 { my ($v) = @_; $v = 9; return $v } my $x = 3; my $rx = \$x; c1($$rx); print "x=$x\n";
my @n = (4); sub el { my ($v) = @_; $v + 1 } print el($n[0]), " $n[0]\n";
PL

both_ways("#3081 a literal ref-to-ref argument: ref(\$v) and ref(\$\$v) at every level", <<'PL');
sub t { my ($v) = @_; print ref($v), " ", ref($$v), "\n"; }
my $x = 5; t(\$x); t(\\$x); my $r = \\$x; t($r); t($$r); t(\\[1]);
sub u { my ($v) = @_; print ref($v), "|"; ref $v eq "REF" ? u($$v) : print "\n" }
u(\\\\$x); my $q=\\\\$x; u($q);
sub w { my ($v) = @_; print ref($v), "|"; if (ref $v eq "REF") { my $z = $$v; w($z) } else { print "\n" } }
w(\\\\$x);
PL

both_ways("#3081 a scalar-ref parameter still aliases its referent and keeps its identity", <<'PL');
use Scalar::Util qw(refaddr reftype blessed);
my $x = 5; my $rx = \$x;
sub w1 { my ($r) = @_; $$r = 7; return } w1(\$x); print "x=$x\n";
sub same { my ($r) = @_; return ($r == $rx ? "eq" : "ne") . " " . (refaddr($r) == refaddr($rx) ? "addr" : "noaddr") }
print same(\$x), "\n";
sub st { my ($r) = @_; return "$r" =~ /^SCALAR\(0x/ ? "str-ok" : "str-bad" } print st(\$x), "\n";
my $o = bless \(my $s = 3), 'Obj';
sub bl { my ($r) = @_; return ref($r) . " " . $$r . " " . (blessed($r) // 'u') }
print bl($o), " ", bl(bless \(my $t = 4), 'P'), "\n";
sub rt { my ($r) = @_; reftype($r) } print rt(\$x), " ", rt(\\$x), " ", rt(\[1]), " ", rt(\sub {1}), "\n";
sub inc { my ($r) = @_; $$r++ } inc(\$x); print "x=$x\n";
sub keep { my ($r) = @_; return $r } my $k = keep(\$x); $$k = 11; print "x=$x\n";
sub two { my ($a, $b) = @_; $$a . $$$b } print two(\"p", \\"q"), "\n";
sub cpy { my ($r) = @_; my $c = $r; $$c = 99 } cpy(\$x); print "x=$x\n";
my @l = (1,2); sub ar { my ($r) = @_; push @$r, 3; scalar @$r } print ar(\@l), " @l\n";
sub gl { my ($r) = @_; ref $r } print gl(\*STDOUT), "\n";
PL
