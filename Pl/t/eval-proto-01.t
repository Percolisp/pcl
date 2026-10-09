#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# eval-proto-01.t -- task #2870: a string eval is parsed with the PROTOTYPES of
# the subs visible at the eval SITE.
#
# perl compiles an eval's text against the live stash, so a `sub un ($)` of the
# program -- declared, glob-assigned, imported, made by an earlier eval -- makes
# `un 1, 2` un(1), 2 inside it.  PCL compiles the text in the pl2cl server,
# which saw only the text, the package, the capture names and the features:
# every such sub was an unknown word there (silent wrong for `($)` and `()`, a
# loud drop for `(&@)`).  The eval request now carries the visible `NAME=PROTO`
# pairs (the runtime's %p-eval-visible-protos) and they join the eval CACHE key
# (ir-spec §6).
#
# Every row compares against perl's STDOUT (warnings off).  No 5.40 syntax:
# CI's perl is 5.38.

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

plan tests => 21;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "no warnings;\n$code";
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
    my $file = write_pl($code);
    my $perl = `perl $file 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the eval sees the program's prototypes (the s511 probe set) ---------

both_agree('sub un3 ($) { "u:$_[0]" } print eval(q{join "|", un3 1, 2}), "\n"; print "err: $@\n" if $@;',
           'a `($)` sub declared in the FILE parses inside the eval (p16)');

both_agree('*un2 = sub ($) { "u:$_[0]" }; print eval(q{join "|", un2 1, 2}), "\n"; print "err: $@\n" if $@;',
           'a glob-assigned `($)` sub (p13)');

both_agree('eval q{sub un ($) { "u:$_[0]" }}; print eval(q{join "|", un 1, 2}), "\n"; print "err: $@\n" if $@;',
           'a `($)` sub an EARLIER eval defined (p12)');

both_agree('eval q{sub blk2 (&@) { my $c = shift; join ",", map { $c->($_) } @_ }};
            print eval(q{blk2 { $_[0] * 2 } 1, 2, 3}), "\n"; print "err: $@\n" if $@;',
           'an eval-defined `(&@)` sub takes a block in a later eval (p11)');

both_agree('eval q{sub K () { 10 }}; print eval(q{K + 1}), "\n"; print "err: $@\n" if $@;',
           'an eval-defined `()` sub is a TERM in a later eval (p15)');

both_agree('use constant K2 => 10; print eval(q{K2 + 1}), "\n"; print "err: $@\n" if $@;',
           'a `use constant` of the file is a TERM inside the eval (p20, #2768)');

both_agree('use List::Util qw(first max); print eval(q{first { $_ > 1 } 1, 2, 3}), "\n";
            print "err: $@\n" if $@; print eval(q{max 1, 7, 3}), "\n";',
           'an IMPORTED `(&@)` sub takes a block inside the eval (p19)');

both_agree('require List::Util; List::Util->import("first");
            print eval(q{first { $_ > 1 } 1, 2, 3}), "\n"; print "err: $@\n" if $@;',
           'imported at RUN time, before the eval (p18)');

both_agree('use Scalar::Util qw(blessed); my $o = bless {}, "K";
            my $r = eval q{blessed $o && $o->isa("K") ? "yes" : "no"};
            print defined $r ? $r : "undef", "\n"; print "err: $@\n" if $@;',
           'Scalar::Util::blessed is `($)` inside the eval (p25)');

both_agree('*h = sub ($) { 7 }; my $n = eval q{my @r = (h 1, 2); scalar @r}; print "$n\n";',
           'a RUN-TIME glob assignment governs a LATER eval (s512 p07)');

# ---- the cache key, a qualified name, another package ---------------------

both_agree('*g = sub { "L" }; my $t = q{my @r = (g 1, 2); scalar @r};
            print eval($t), "\n"; *g = sub ($) { "S" }; print eval($t), "\n";',
           'the SAME text under two prototypes compiles twice (the eval cache key)');

both_agree('package Other; sub f ($) { "o:$_[0]" } package main;
            print eval(q{join "|", Other::f 1, 2}), "\n";',
           'a QUALIFIED call reads the named package\'s prototype');

both_agree('package Y; sub f ($) { "y:$_[0]" } package X; sub f { "x:@_" }
            print eval(q{join "|", f 1, 2}), "\n";',
           'the eval\'s OWN package decides: X::f has no prototype');

# ---- what the eval defines stays in the eval (unchanged) -----------------

both_agree('sub bar; eval q{sub bar ($) { "b:@_" }}; print bar 1, 2; print "\n";',
           'a prototype defined INSIDE an eval does not change the FILE\'s parse (p9)');

both_agree('print eval(q{use List::Util qw(max); max 1, 5, 3}), "\n"; print "err: $@\n" if $@;',
           'what the eval text itself `use`s is read (p14)');

# ---- the file level: Scalar::Util's shim carries the real prototypes -----

both_agree('use Scalar::Util qw(blessed); my $o = bless {}, "K";
            my $r = blessed $o && $o->isa("K") ? "yes" : "no"; print "$r\n";
            print prototype(\&blessed), "\n";',
           'blessed is `($)` in the program too (it was skipped by name)');

# ---- found on the way: a `($)` slot keeps a DUALVAR -----------------------

both_agree('use Scalar::Util qw(dualvar isdual); sub mk { dualvar(0, "abc") }
            sub idp ($) { isdual($_[0]) ? 1 : 0 } my $d = dualvar(1, "a");
            sub s1 ($) { $_[0] = "w"; 1 } s1(scalar($d));
            print isdual(dualvar(0, "abc")) ? 1 : 0, idp(mk()), " d=$d\n";',
           'scalar() of a dualvar is the dualvar: isdual through a `($)` slot, and it aliases');

# ---- the WIRE (s513d): both ends of the eval protocol agree, or say so ---
#
# The final bench of s513d HUNG for 68 minutes: tools/bench-exec.pl's runtime
# A/B loaded the tree's program (whose preamble names the TREE's pl2cl) into
# the BASE runtime, which sent the six-field request of before #2870 to a
# seven-field server.  The server waited for a line end inside the eval TEXT,
# the runtime for the answer.  Every request now begins with the wire tag and
# every status line carries it (*p-eval-wire* / $EVAL_WIRE); a mismatch is an
# error at once.  The texts below carry a per-run token, so the eval disk
# cache cannot answer them and each one really crosses the wire.

my $tok = "t$$" . time;

both_agree("package Nope; my \$v = eval q{my \$t = '$tok'; 6 * 7}; print \"\$v\\n\"; print \"err: \$@\\n\" if \$@;",
           'an eval from a package with NO prototyped subs crosses the wire (an empty pairs line is a line)');

{
    my $subs = join '', map { sprintf 'sub a_rather_long_prototyped_name_%03d ($) { "p%d:$_[0]" } ', $_, $_ } 1 .. 300;
    both_agree("$subs print eval(q{my \$t = '$tok'; join '|', a_rather_long_prototyped_name_123 1, 2}), \"\\n\"; print \"err: \$@\\n\" if \$@;",
               'a pairs line of several KB (300 prototyped subs) crosses the wire');
}

# The server's end: a request from a runtime of ANOTHER version (main's six
# fields, untagged) is answered at once with an error naming both -- measured
# without SBCL, as the old runtime would see it.
{
    use IPC::Open2;
    my $pid = open2(my $out, my $in, $^X, $pl2cl, '--server');
    binmode $in, ':utf8'; binmode $out, ':utf8';
    my $code = '1 + 1';
    print $in join("\n", 'main', '', '', '', length $code), "\n", $code;
    $in->flush;
    my ($st, $body) = ('(no answer in 30 s)', '');
    eval {
        local $SIG{ALRM} = sub { die "alarm\n" };
        alarm 30;
        $st = <$out>; my $n = <$out>; chomp($st, $n); read($out, $body, $n);
        alarm 0;
    };
    kill 'TERM', $pid; waitpid $pid, 0;
    ok($st =~ /^err PCL-EVAL-WIRE / && $body =~ /different PCL trees/,
       "the server answers an untagged (other-version) request with an error at once (got '$st')");
}

# The runtime's end: an eval server that answers WITHOUT the tag (an older
# pl2cl) makes the eval fail with $@ naming the mismatch -- never a wait.
{
    my ($sfh, $fake) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $sfh 'my $l = <STDIN>; $| = 1; print "ok\n1\n1"; sleep 60;', "\n";
    close $sfh;
    my $cl = PCLCore::transpile("$pl2cl " . write_pl(
        "my \$r = eval q{my \$t = '$tok'; 1 + 1}; print defined \$r ? \"r=\$r\\n\" : \"died: \$@\";"));
    $cl =~ s{\(setf pcl::\*pcl-pl2cl-path\* #P"[^"]*"\)}{(setf pcl::*pcl-pl2cl-path* #P"$fake")}
        or die "no pl2cl-path line in the preamble";
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl;
    close $cl_fh;
    my $output = `timeout 120 sbcl @sbcl_rt --load $cl_file 2>&1`;
    like($output, qr/died: PCL: the eval server .* answered "ok"/,
         'the runtime refuses an untagged answer: $@ names the mismatch');
}
