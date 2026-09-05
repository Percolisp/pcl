#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# census-bugs-01.t — the bugs the s470bm IR censuses found (tasks #1173-#1179).
#
# Each block names its task and asserts the PERL answer, probed against perl
# 5.40.3 and quoted in the block's comment.  These are RUN rows: every one of
# them is a value the program consumes, which is why the bugs were silent.

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

plan tests => 14;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── #1179: `use parent`'s -norequire is a FLAG, never a superclass ─────────
# perl 5.40.3 on all three spellings: @ISA is (Foo), ->isa("-norequire") false.
# parent.pm strips it only in FIRST position, so the qw() spelling and the
# comma spelling must agree — they used to not.

test_cl('#1179 qw(-norequire Foo): @ISA holds only the class', <<'PL', "ISA=[Foo]\n");
package Foo; sub hi { "hi" }
package Bar; use parent qw( -norequire Foo );
package main;
print "ISA=[@Bar::ISA]\n";
PL

test_cl('#1179 qw(-norequire Foo): ->isa("-norequire") is false', <<'PL', "foo=1 nore=0\n");
package Foo; sub hi { "hi" }
package Bar; use parent qw( -norequire Foo );
package main;
printf "foo=%d nore=%d\n", (Bar->isa("Foo") ? 1 : 0), (Bar->isa("-norequire") ? 1 : 0);
PL

test_cl('#1179 comma spelling still right (the half that worked)', <<'PL', "ISA=[Foo Baz]\n");
package Foo; sub hi { "hi" }
package Baz; sub hi2 { "h2" }
package Bar; use parent -norequire, 'Foo', 'Baz';
package main;
print "ISA=[@Bar::ISA]\n";
PL

test_cl('#1179 qw() with two classes after the flag', <<'PL', "ISA=[Foo Baz]\n");
package Foo; sub hi { "hi" }
package Baz; sub hi2 { "h2" }
package Bar; use parent qw(-norequire Foo Baz);
package main;
print "ISA=[@Bar::ISA]\n";
PL

# The flag is honoured in FIRST position only — parent.pm's own rule
# (`if (@_ and $_[0] eq '-norequire') { shift }`).  perl DIES on this input
# (it goes looking for Foo.pm), so PCL is only required not to strip.
test_cl('#1179 a LATER -norequire is an ordinary list element', <<'PL', "ISA=[Foo -norequire]\n");
package Foo; sub hi { "hi" }
package Bar; use parent qw(Foo -norequire);
package main;
print "ISA=[@Bar::ISA]\n";
PL

# The parenthesised list spelling reached @ISA at all only after the flatten.
test_cl('#1179 -norequire with a parenthesised class list', <<'PL', "ISA=[Foo]\n");
package Foo; sub hi { "hi" }
package Bar; use parent -norequire, ('Foo');
package main;
print "ISA=[@Bar::ISA]\n";
PL

# ── #1178: an inline `my` inside a CALL ARGUMENT ──────────────────────────
# `f((open my $fh, ...), ...)` used to emit a call to the non-existent op
# `p-my`, and the whole emitted file died at load.  Both spellings are here:
# the plain one was always right and must stay right.

test_cl('#1178 open my $fh inside a call argument', <<'PL', "take[call-arg]=1\n");
sub take { my ($v, $d) = @_; print "take[$d]=", ($v ? 1 : 0), "\n" }
take((open my $fh, "<", "/etc/hostname"), "call-arg");
PL

test_cl('#1178 the plain spelling is unchanged', <<'PL', "plain=1\n");
my $ok = open my $fh, "<", "/etc/hostname";
print "plain=", ($ok ? 1 : 0), "\n";
PL

test_cl('#1178 the declared handle really is the opened one', <<'PL', "n=1\n");
sub take { my ($v) = @_; return $v }
my $r = take((open my $fh, "<", "/etc/hostname"));
my @l = <$fh>;
print "n=", scalar(@l), "\n";
PL

# ── #1178: `open ..., undef` is perl's ANONYMOUS TEMPORARY FILE ───────────
# perl 5.40.3: every FILE mode succeeds; `<` is read-only, and every write
# mode reads back after a seek (the temp file is O_RDWR whatever the mode).

test_cl('#1178 open +> undef opens an anonymous temp file', <<'PL', "ok=1\n");
my $ok = open(my $fh, "+>", undef);
print "ok=", ($ok ? 1 : 0), "\n";
PL

test_cl('#1178 the anonymous temp file round-trips a write', <<'PL', "read=[hello]\n");
open(my $fh, "+>", undef) or die "no";
print $fh "hello\n";
seek($fh, 0, 0);
my $l = <$fh>; chomp $l;
print "read=[$l]\n";
PL

test_cl('#1178 a > anonymous temp file reads back too (perl: O_RDWR)', <<'PL', "read=[hi]\n");
open(my $fh, ">", undef) or die "no";
print $fh "hi\n";
seek($fh, 0, 0);
my $l = <$fh>; chomp $l;
print "read=[$l]\n";
PL

test_cl('#1178 a < anonymous temp file is read-only and empty', <<'PL', "print=0 defined=0\n");
open(my $fh, "<", undef) or die "no";
my $p = print($fh "x\n") ? 1 : 0;
seek($fh, 0, 0);
my $l = <$fh>;
printf "print=%d defined=%d\n", $p, (defined $l ? 1 : 0);
PL

my $tap_banner = "# PCL Test library loaded\n";
test_cl('#1178 the task reproducer: ok((open my $fh, "+>", undef))', <<'PL', $tap_banner . "1..1\nok 1 - opened\n");
use Test::More tests => 1;
ok((open my $fh, "+>", undef), "opened");
PL
