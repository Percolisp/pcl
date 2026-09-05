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

plan tests => 6;

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

