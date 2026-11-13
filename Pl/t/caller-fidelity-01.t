#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# caller-fidelity-01.t -- s539d, task #233 tier 1 + #1361: what `caller`
# answers, read from the call-frame stack (cl/pcl-runtime.lisp, "THE
# CALL-FRAME STACK"; docs/caller-implementation.md).
#
#   * the FIELD SHAPES -- `caller' with no argument is three fields, caller(N)
#     eleven, the sub name / hasargs / wantarray fields, the empty list past
#     the outermost frame (every build);
#   * an ANONYMOUS sub whose body reads `caller' is a frame, named by
#     set_subname when it was renamed (every build);
#   * `# line N "FILE"' moves __LINE__ / __FILE__ / a die's location (every
#     build);
#   * the FILE and LINE of a frame's call site are perl's under
#     PCL_OPT=line-track -- the location registers are written only there
#     (the default build answers `-' line 0, docs/not-supported.md
#     ns-caller-fidelity) -- and Carp's messages built from them;
#   * the emission rows: the default build writes no register, and only an
#     anonymous sub that reads a frame carries :frame (its cost, +3.5 ns per
#     call, is in p-anon-sub's docstring).
#
# Every expectation is the live `perl' 5.40.3 answer (probed s539d); a
# program that needs its own file name compares against $0.

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

plan tests => 10;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub transpile { my ($code, $env) = @_; PCLCore::transpile(($env // '') . "$pl2cl " . write_pl($code)) }

# PCL_OPT reaches the RUN as well as the transpile (a module the run loads is
# transpiled by a child pl2cl).  lt => 1 is the line-track build.
sub run_cl {
    my ($code, %opt) = @_;
    my $env = $opt{lt} ? 'PCL_OPT=line-track ' : '';
    my $cl_code = transpile($code, $env);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `$env sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# -- 1. The field shapes (every build). ---------------------------------------
is(run_cl(<<'PL'), <<'OUT', 'caller: 3 fields without EXPR, 11 for caller(N), sub name / hasargs / wantarray, empty past the top');
sub show { join "|", map { defined $_ ? $_ : 'U' } @_ }
sub f0 { my @c = caller; print "a ", scalar(@c), " $c[0]\n"; my $p = caller; print "b $p\n" }
f0();
package Q; main::f0(); package main;
sub f2 { my @c = caller(0); print "c ", scalar(@c), " ", show(@c[0,3,4,5,6,7]), "\n" }
f2(); my @l = f2(); my $s = f2();
sub f3 { my @c = caller(1); print "d ", show(@c[0,3]), "\n" } sub f4 { f3() } f4();
sub top { my @c = caller(5); print "e ", scalar(@c), "\n"; my @n = caller(-1); print "f ", scalar(@n), "\n" } top();
my @t = caller; print "g ", scalar(@t), "\n";
sub g1 { goto &f2 } g1();
sub rec { my $n = shift; return rec($n - 1) if $n; my $d = 0; $d++ while caller($d); $d } print "h ", rec(300), "\n";
PL
a 3 main
b main
a 3 Q
b Q
c 11 main|main::f2|1|U|U|U
c 11 main|main::f2|1|1|U|U
c 11 main|main::f2|1||U|U
d main|main::f4
e 0
f 0
g 0
c 11 main|main::f2|1|U|U|U
h 301
OUT

# -- 2. An anonymous sub that reads caller is a frame (every build). ---------
is(run_cl(<<'PL'), <<'OUT', 'an anonymous sub reading caller is a frame: PKG::__ANON__, a set_subname name, its context');
use Sub::Util qw(set_subname);
my $an = sub { my @c = caller(0); print "a $c[0] $c[3] $c[4]\n" }; $an->();
package P; my $pa = sub { (caller(0))[3] }; package main; print "b ", $pa->(), "\n";
my $r = set_subname "Foo::bar" => sub { (caller(0))[3] }; print "c ", $r->(), "\n";
sub named { my $x = sub { my @c = caller(1); $c[3] }; $x->() } print "d ", named(), "\n";
my $plain = sub { $_[0] + 1 }; print "e ", $plain->(1), "\n";
my @w = map { my $k = $_; sub { my @c = caller(0); $c[5] ? "L$k" : "S$k" } } 1 .. 2;
my @lst = $w[0]->(); my $sc = $w[1]->(); print "f @lst $sc\n";
PL
a main main::__ANON__ 1
b P::__ANON__
c Foo::bar
d main::named
e 2
f L1 S2
OUT

# -- 3. `# line' directives (every build). ------------------------------------
is(run_cl(<<'PL'), <<'OUT', '# line N "FILE" moves __LINE__, __FILE__ and die / warn locations, in a string eval too');
print "a ", __LINE__, " ", (__FILE__ eq $0 ? "F" : __FILE__), "\n";
# line 100 "foo.pl"
print "b ", __LINE__, " ", __FILE__, "\n";
eval { die "x" }; print "c $@";
#line 200
print "d ", __LINE__, " ", __FILE__, "\n";
my $r = eval "\n# line 50 \"tpl.tt\"\ndie 'z'"; print "e $@";
# line 7 "bar.pl"
eval { die "y" }; print "f $@";
local $SIG{__WARN__} = sub { print "g $_[0]" }; warn "w";
PL
a 1 F
b 100 foo.pl
c x at foo.pl line 101.
d 200 foo.pl
e z at tpl.tt line 50.
f y at bar.pl line 7.
g w at bar.pl line 8.
OUT

# -- 4. File and line of the call site, line-track build. ---------------------
my $where = <<'PL';
sub fl { my ($f, $l) = @_; ($f eq $0 ? "F" : $f) . " $l" }
sub f0 { my @c = caller; print "a ", fl(@c[1,2]), "\n" }
f0();
sub f2 { my @c = caller(0); print "b ", fl(@c[1,2]), " $c[3]\n" }
f2();
sub g1 { goto &f2 } g1();
eval 'f2(); 1' or die;
my $an = sub { my @c = caller(0); print "c ", fl(@c[1,2]), " $c[3]\n" };
$an->();
sub f3 { my @c = caller(1); print "d ", fl(@c[1,2]), " $c[3]\n" } sub f4 { f3() }
f4();
# line 40 "foo.pl"
f0();
PL
is(run_cl($where, lt => 1), <<'OUT', 'line-track: caller FILE and LINE are the call site, through goto &sub, a string eval, an anon sub, a # line');
a F 3
b F 5 main::f2
b F 6 main::f2
b (eval 1) 1 main::f2
c F 9 main::__ANON__
d F 11 main::f4
a foo.pl 40
OUT

# ... and the default build answers the registers as they stand: no location
# was recorded, so `-' line 0 -- `(eval N)' from a string eval's code.
is(run_cl($where), <<'OUT', 'default build: no location recorded -- `-` line 0, `(eval N)` from a string eval');
a - 0
b - 0 main::f2
b - 0 main::f2
b (eval 1) 0 main::f2
c - 0 main::__ANON__
d - 0 main::f4
a - 0
OUT

# -- 5. Carp builds perl's messages from caller (#1361), line-track build. ---
my $carp = <<'PL';
use Carp qw(croak carp confess);
package M; use Carp;
sub boom { croak "bad arg" }
sub boomnl { croak "nl\n" }
sub inner { croak "inner" } sub outer { inner() }
sub conf { confess "conf" }
sub obj { croak { code => 7 } }
sub warnit { carp "careful" }
package N; our @ISA = ('M');
sub viaisa { M::boom() }
package P;
sub deep3 { M::boom() } sub deep2 { deep3() } sub deep1 { deep2() }
package Q; our @CARP_NOT = ('M');
sub notme { M::boom() }
package main;
sub fix { my $m = shift; $m =~ s/\Q$0\E/F/g; $m }
eval { M::boom() }; print "1: ", fix($@);
eval { M::boomnl() }; print "2: ", fix($@);
eval { M::outer() }; print "3: ", fix($@);
eval { N::viaisa() }; print "4: ", fix($@);
eval { P::deep1() }; print "5: ", fix($@);
eval { Q::notme() }; print "6: ", fix($@);
eval { M::obj() }; print "7: ", ref($@), " $@->{code}\n";
eval { M::conf() }; my @l = split /\n/, $@; print "8: ", fix($l[0]), "\n";
local $SIG{__WARN__} = sub { print "9: ", fix($_[0]) };
M::warnit();
eval { local $Carp::CarpLevel = 1; P::deep1() }; print "10: ", fix($@);
PL
is(run_cl($carp, lt => 1), <<'OUT', 'line-track: croak / carp / confess report the first caller outside the package (@ISA, @CARP_NOT, CarpLevel)');
1: bad arg at F line 17.
2: nl
 at F line 18.
3: inner at F line 19.
4: bad arg at F line 20.
5: bad arg at F line 12.
6: bad arg at F line 22.
7: HASH 7
8: conf at F line 6.
9: careful at F line 26.
10: bad arg at F line 27.
OUT

# ... and in the default build every message still has perl's SHAPE, the
# ` at FILE line N.' tail a test matches (IO-Compress's `\s+at').
{
    my @rows = split /\n/, run_cl($carp);
    my @shaped = grep { /^\d+: (?:bad arg|inner|conf|careful) at \S+ line \d+\.$/ } @rows;
    is(scalar(@shaped), 8, 'default build: croak / carp / confess messages end in ` at FILE line N.`')
        or diag(join "\n", @rows);
}

# -- 6. The emission. -----------------------------------------------------------
{
    my $cl = transpile('my $f = sub { $_[0] + 1 }; my $g = sub { (caller(0))[3] }; sub h { 1 } print $f->(1), $g->(), h(), "\n";');
    my @anon = $cl =~ /\(p-anon-sub "main"(\s+:frame)?/g;
    is_deeply([map { defined $_ ? 'frame' : 'plain' } @anon], ['plain', 'frame'],
              'only the anonymous sub that reads caller carries :frame');
    unlike($cl, qr/\(p-(?:line|file) /, 'the default build writes no location register');
    like(transpile("# line 9 \"x.pl\"\nmy \$z = __LINE__ . __FILE__;\n"), qr/9 "x\.pl"|"x\.pl"/,
         '__FILE__ / __LINE__ after a directive are its constants');
}
