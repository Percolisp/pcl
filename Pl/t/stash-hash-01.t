#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# stash-hash-01.t - a package stash read and written AS A HASH, inside the
# bounded design s522 ruled (docs/not-supported.md "Live symbol-table hashes"):
# every delete spelling that removes a sub goes through the one write-through
# path (#3511), the snapshot lists variables and filehandles too (#1510), a
# non-sub element's VALUE is its typeglob (perl's value; core Exporter reads
# `${$pkg . "::"}{EXPORT_FAIL}`), and DELETING a non-sub symbol removes its glob from the package
# (#3691, s523d: compiled names keep the old cells, run-time lookups miss).  Expected lines are perl 5.40.3's output (probed s522b)
# (rows 9-10 probed s523d).  Inverse: on the aff112d7 base every row fails
# (a,b,keep / the reader death on `@Pkg::{...}` / empty / the subs-only view
# / a-def / no glob value / the delete answers).

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
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    ref $expected ? like($got, $expected, $name) : is($got, $expected, $name);
}

# Through ./runpcl: the program is loaded FORM BY FORM, as a user runs it.
sub test_pcl {
    my ($name, $code, $expected) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $got = `$project_root/runpcl $pl_file 2>&1`;
    is($got, $expected, $name);
}

my $subs = 'package Local::Class; sub a {1} sub b {2} sub keep {3} package main; my @imports = qw(a b); ';
my $left = 'print join(",", sort grep { defined &{"Local::Class::$_"} } qw(a b keep)), "\n";';

test_cl('#3511: delete @{"Pkg::"}{LIST} removes the subs (was a silent no-op)',
    $subs . 'delete @{"Local::Class::"}{@imports}; ' . $left,
    "keep\n");

test_cl('#3511: delete @Pkg::{LIST} transpiles (was a reader death) and removes the subs',
    $subs . 'delete @Local::Class::{@imports}; ' . $left,
    "keep\n");

test_cl('#1510: a package holding only @ISA has a non-empty stash',
    'package Foo; our @ISA = ("Bar"); package main; print scalar(keys %{"Foo::"}) ? "has" : "empty", "\n";',
    "has\n");

test_cl('#1510: keys of a stash list subs, package variables and filehandles',
    'package Foo; our $x = 1; our @y = (1); our %h = (a=>1); sub f {1} open FH, "<", "/dev/null";'
    . ' package Bar; sub g {1} package Baz; our @ISA = ("Bar"); package main;'
    . ' for my $p (qw(Foo Bar Baz)) { no strict "refs"; print "$p: ", join(",", sort keys %{"${p}::"}), "\n" }',
    "Foo: FH,f,h,x,y\nBar: g\nBaz: ISA\n");

test_cl('#1510: exists on a stash element answers for @ISA and a missing name',
    'package Foo; sub f {1} package Baz; our @ISA = ("Foo"); package main;'
    . ' print exists $Foo::{ISA} ? "e":"ne", exists $Baz::{ISA} ? "e":"ne", exists $Foo::{nope} ? "e":"ne", "\n";',
    "neene\n");

test_cl('#3511: delete $Pkg::{name} undefines the sub for defined, *{}{CODE} and method calls',
    'package Local::Class; sub a {1} package main; delete $Local::Class::{a};'
    . ' print defined &{"Local::Class::a"} ? "a-def" : "a-gone", " ", (defined *{"Local::Class::a"}{CODE} ? "code" : "nocode"), "\n";'
    . ' print eval { Local::Class->a; 1 } ? "ran\n" : "died\n";',
    "a-gone nocode\ndied\n");

test_cl('a stash slice READ of subs still answers',
    'package Q; sub b {2} sub keep {3} package main; my @x = @Q::{qw(b keep)}; print scalar(@x), "\n";',
    "2\n");

test_cl('the VALUE of a non-CODE stash element is its typeglob',
    'package Foo; our $x = 7; our @y = (1,2); package main; my $g = $Foo::{x};'
    . ' print "$g|", ref(\$Foo::{x}), "|", (defined $Foo::{y} ? 1 : 0), "|", scalar(@{*{$Foo::{y}}{ARRAY}}), "\n";',
    "*Foo::x|GLOB|1|2\n");

# #3691 (s523d): the glob LEAVES the package -- the stash view, exists and every
# run-time lookup by NAME (symbolic, the @ISA readers for isa / methods) miss
# it, while compiled $x / \$x / @ISA keep the old cells (perl's pinned glob),
# and delete returns the glob; nothing on stderr.  Expected = perl 5.40.3.
# Inverse: the 2534d7d0 base prints e / k / def / isa 1 and announces.
test_cl('#3691: DELETING a non-CODE symbol removes the glob from the stash; compiled names keep the old cell', <<'EOF',
no strict 'refs';
our $x = 1; my $r = \$x;
print exists $::{x} ? "e\n" : "ne\n";
my $g = delete $::{x};
print exists $::{x} ? "e\n" : "ne\n";
print((grep { $_ eq "x" } keys %main::) ? "k\n" : "nk\n");
print ${"main::x"} // "undef", "\n";
print "$x $$r\n";
print "g=", (defined $g ? "$g" : "undef"), "\n";
@Blength::ISA = 'Bladd'; @Bladd::ISA = ();
print Blength->isa("Bladd") ? 1 : 0, "\n";
delete $Blength::{ISA};
print Blength->isa("Bladd") ? 1 : 0, " ", scalar(@Blength::ISA), "\n";
our @y = (1, 2); my $gy = delete $Foo2::{nope}; print defined $gy ? "d\n" : "u\n";
print "ok\n";
EOF
    "e\nne\nnk\nundef\n1 1\ng=*main::x\n1\n0 1\nu\nok\n");

# The FIRST #3691 version UNINTERNED the symbols, and a top-level statement read
# AFTER the delete -- ./runpcl and the script cache load a program form by
# form -- interned a fresh UNBOUND symbol: a raw CL "The variable $x is
# unbound" (Fable's merge probe p523d/d3).  These rows run through ./runpcl.
# Inverse: that tree (0fe1038e) dies on both; the base announces.
test_pcl('#3691: top-level reads AFTER a delete (form-at-a-time loading) see the old cell; the view and isa miss it', <<'EOF',
our $x = 1; our @y = (1, 2); sub r { $x }
delete $main::{x};
print exists $main::{x} ? "exists" : "gone", "\n";
print defined ${"main::x"} ? "def" : "undef", "\n";
print "r=", r(), " top=", $x, "\n";
print join(",", grep { /^[xy]$/ } sort keys %main::), "\n";
my $g = delete $main::{y}; print ref(\$g), " $g\n";
print "@y\n";
print exists $main::{y} ? "exists" : "gone", "\n";
package P; sub meth { "P::meth" } package Q; our @ISA = ('P'); package main;
print Q->meth, "\n";
delete $Q::{ISA};
print eval { Q->meth } // "no-method", "\n";
print exists $Q::{ISA} ? "exists" : "gone", " ", scalar(@Q::ISA), "\n";
print "end\n";
EOF
    "gone\nundef\nr=1 top=1\ny\nGLOB *main::y\n1 2\ngone\nP::meth\nno-method\ngone 1\nend\n");

test_pcl('#3691: a delete inside a sub, then top-level reads', <<'EOF',
our $x = 1; sub r { $x }
sub go { delete $main::{x}; print "r=", r(), "\n"; print "in-sub=", $x, "\n"; }
go();
print "top=", $x, "\n";
print exists $main::{x} ? "exists" : "gone", "\n";
print "end\n";
EOF
    "r=1\nin-sub=1\ntop=1\ngone\nend\n");

# The residue (docs/not-supported.md "Live symbol-table hashes"): a SYMBOLIC
# access after the delete vivifies a NEW variable in perl; PCL un-marks the
# name and shares the old cell, so the compiled $x / @a see the write.
TODO: {
    local $TODO = '#3691 residue: a symbolic access after a stash delete shares the old cell (perl: a new variable)';
    test_pcl('#3691: a symbolic write after the delete is a NEW variable', <<'EOF',
our $x = 1; our @a = (1, 2);
delete $main::{x}; delete $main::{a};
${"main::x"} = 2; print "$x ${'main::x'}\n";
print scalar(@{"main::a"}), "\n";
push @{"main::a"}, 9; print "@a | @{'main::a'}\n";
EOF
        "1 2\n0\n1 2 | 9\n");
}

# #3321: a stash ALIAS `*A:: = *B::` redirects RUN-TIME name resolution (bless,
# method calls, isa, can) to B's stash; names bound at COMPILE time (r4's `sub
# later`, r6's `$A0::v`) stay with A's own stash, as in perl.  Program = Fable's
# s522 merge probe b3-stash-alias.pl; expected = perl 5.40.3.  Inverse: the
# aff112d7 base prints r1 `0 none`, r2 A0, r3 cannot, r5 none, r7 none.
test_cl('#3321: a stash alias redirects bless / methods / isa / can; compile-time names stay', <<'EOF',
package B0; sub hi { "hi" } package A0; *A0:: = *B0::; package main; my $o = bless {}, "A0";
print "r1 ", ($o->isa("B0") ? 1 : 0), " ", (eval { $o->hi } // "none"), "\n";
print "r2 ", ref($o), "\n";
print "r3 ", (A0->can("hi") ? "can" : "cannot"), " ", (A0->isa("A0") ? 1 : 0), "\n";
package A0; sub later { "later" } package main; print "r4 ", (eval { B0->later } // "none"), "\n";
package B0; our @ISA = ("Base0"); package Base0; sub bz { "bz" } package main; print "r5 ", (eval { A0->bz } // "none"), "\n";
$A0::v = 1; print "r6 ", ($B0::v // "undef"), "\n";
package JSON::PP::Boolean; sub new { bless {}, shift } sub val { "v" } package main; *Dummy::True:: = *JSON::PP::Boolean::; print "r7 ", (eval { Dummy::True->new->val } // "none"), "\n";
EOF
    "r1 1 hi\nr2 B0\nr3 can 1\nr4 none\nr5 bz\nr6 undef\nr7 v\n");

# #3321 + perl t/op/method.t "SUPER inside moved package": SUPER is relative to
# the package the code was COMPILED in, so code compiled under `package Mover`
# after `*Mover:: = *Mover2::` keeps its own @ISA for SUPER, while bless still
# names the alias target.  Expected = perl 5.40.3.  Inverse: the 8038fac6 base
# prints r2 Mover (the alias ignored); the pre-fix s522b tree printed r1 undef.
test_cl('#3321: SUPER inside an aliased package walks the @ISA its own code wrote', <<'EOF',
BEGIN { *Mover:: = *Mover2:: }
package Mover; no strict; @ISA = 'door'; sub door::dohtem { 'dohtem' }
print "r1 ", (eval { Mover->SUPER::dohtem } // "undef"), "\n";
package main; print "r2 ", ref(bless {}, "Mover"), "\n";
EOF
    "r1 dohtem\nr2 Mover2\n");
