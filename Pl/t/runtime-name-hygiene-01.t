#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# runtime-name-hygiene-01.t — task #2100: a user sub named like one of the
# RUNTIME's own internal operators must not be hijacked at its call site.
#
# `Pl::ExprToCL`'s `%RUNTIME_NAMES` is NOT perl's builtin list: beside `print`
# and `reverse` it holds the runtime's own vocabulary — `flatten`, `hash`,
# `aref`, `aslice`, `box`, `setf`, `let`, `regex`, `subst`, `typeglob`, `cwd`,
# `reftype`, `weaken`, … — and a PLAIN call lowered to `p-NAME` on that table
# alone, so the user's sub was never reached:
#
#     sub hash { "user:@_" }  print hash(1,2);   # printed "12"  (SILENT)
#     sub flatten { ... }     flatten(1,2);      # CL program error
#     sub let { ... }         let(1);            # failed inside a MACROEXPANSION,
#                                                # taking the whole file with it
#
# 27 of the 36 such names failed; the method call and the `&NAME(...)` form
# were right all along, which is what located the bug in `cl_name`'s plain-call
# arm.  `sub flatten`, `sub hash`, `sub box`, `sub regex`, `sub exception`,
# `sub cwd` are everyday names.
#
# THE RULE: a name the program DECLARES is the user's, UNLESS it is a perl
# KEYWORD — perl calls the builtin for `sub reverse {…}; reverse(…)` unless it
# was imported or predeclared with `use subs` (#1870/#1992 own that half).
# "Is it a keyword" is MEASURED, not listed: `prototype("CORE::NAME")` DIES for
# a non-keyword, so the die is the discriminator
# (`Pl::PExpr::Config::is_core_keyword`).  The `CORE::NAME` spelling names the
# builtin unconditionally and is carried through as a separate flag — without
# it, lib/Cwd.pm's `sub cwd { CORE::cwd() }` called ITSELF and exhausted the
# binding stack.
#
# THE NAMES ARE DERIVED AT TEST TIME from the runtime's own export list, so a
# runtime operator added tomorrow is covered the day it is added.  All of them
# go into ONE program (one SBCL launch) and the whole output is compared with
# perl's.

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

plan tests => 4;

# ---- the names, derived from the TABLE THAT DECIDES ------------------------
# `%RUNTIME_NAMES` in Pl/ExprToCL.pm is what routes a plain call to `p-NAME`,
# so the set is read from there at test time: every entry that is a legal Perl
# identifier and that perl does NOT know as a keyword.  A runtime operator
# added to that table tomorrow joins this test the day it is added.
my @names;
{
    open my $fh, '<', "$project_root/Pl/ExprToCL.pm" or die "open ExprToCL: $!";
    my $in_table = 0;
    my $body     = '';
    while (my $l = <$fh>) {
        if (!$in_table) { $in_table = 1 if $l =~ /^my \%RUNTIME_NAMES\s*=\s*map .* qw\(/; next }
        last if $l =~ /^\);/;
        $body .= $l;
    }
    close $fh;
    $body =~ s/#.*//g;
    my %seen;
    for my $n (split ' ', $body) {
        next if $n !~ /\A[A-Za-z_][A-Za-z_0-9]*\z/;             # not an identifier
        next if $seen{$n}++;
        next if eval { my $p = prototype("CORE::$n"); 1 };      # a perl keyword
        push @names, $n;
    }
}
@names = sort @names;

cmp_ok(scalar(@names), '>=', 20,
       '%RUNTIME_NAMES holds at least 20 non-keyword identifiers to test (got '
     . scalar(@names) . ')');

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

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
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, $desc);
}

# ---- ONE program, every name, three call spellings each -------------------
my $prog = join('', map { "sub $_ { return \"user:\@_\" }\n" } @names)
         . join('', map { "print \"$_=\", $_(1,2), \"|\", main->$_(3), \"|\", &$_(4), \"\\n\";\n" }
                    @names);
both_agree($prog, 'a user sub of a runtime-internal name wins at every call spelling');

# ---- the cases the rule must NOT break ------------------------------------
both_agree(<<'P', 'a perl KEYWORD still wins over a user sub of the same name');
sub reverse { return "MINE" }
print "1 ", CORE::reverse("ab"), " ", &reverse("x"), "\n";
my $r = [1,2];
print "2 ", ref($r), "\n";
package Obj; sub new { bless {}, shift } sub hi { "hi" }
package main;
my $o = Obj->new;
print "3 ", ($o->can("hi") ? "can" : "cannot"), " ", ($o->isa("Obj") ? "isa" : "not"), "\n";
print "4 ", $o->can("hi")->($o), "\n";
P

# lib/Cwd.pm is `sub cwd { CORE::cwd() }`, and Scalar::Util's exports collide
# with the runtime's names too — an imported sub must still reach its shim.
both_agree(<<'P', 'an IMPORTED sub of a runtime name still reaches its shim');
use Cwd qw(cwd getcwd abs_path);
use Scalar::Util qw(reftype blessed looks_like_number);
print "1 ", (cwd() =~ m{/} ? "ok" : "BAD"), " ", (getcwd() =~ m{/} ? "ok" : "BAD"), "\n";
print "2 ", (abs_path(".") =~ m{/} ? "ok" : "BAD"), "\n";
my $r = [1,2];
print "3 ", reftype($r), " ", (blessed($r) // "undef"), "\n";
print "4 ", looks_like_number("3.5") ? 1 : 0, "\n";
P
