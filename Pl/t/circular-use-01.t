#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# circular-use-01.t - a module reached again while it is still LOADING is
# skipped as a LOAD and still IMPORTS (task #1998).
#
# perl's `use` is `require` + `import`, and `require` is skipped when the file
# is already in %INC -- which perl sets BEFORE running the file's body.  So a
# circular `use` takes the ordinary already-loaded path: no second load, and
# `import` called on whatever the half-loaded module has defined SO FAR.  PCL
# returned early instead, importing nothing and warning on stderr about a
# state perl treats as ordinary -- which is why `use IO::Socket;` died
# "Undefined subroutine &IO::Socket::UNIX::AF_UNIX called" and took
# IO::Socket::INET and HTTP::Tiny down with it.
#
# The fixture is a pair of modules written here, never a module NAME: the fact
# under test is the mechanism (CLAUDE.md 9a).  Every expectation is perl
# 5.40.3's own output, and because run_cl merges stderr, a returning warning
# would fail these rows too.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

plan tests => 5;

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

sub write_mod {
    my ($dir, $name, $body) = @_;
    open my $fh, '>', "$dir/$name.pm" or die "$dir/$name.pm: $!";
    print $fh $body;
    close $fh;
}

my $DIR = tempdir(CLEANUP => 1);

# A CUSTOM `sub import` is defined at COMPILE time, so a circular `use` finds
# it and it really imports.  CircB's own `hello` comes from exactly that call.
write_mod($DIR, 'CircA', <<'PM');
package CircA;
require CircB;
sub import { my $c = caller; no strict 'refs';
             *{"${c}::hello"} = sub { "hello from CircA" } }
1;
PM
write_mod($DIR, 'CircB', <<'PM');
package CircB;
use CircA;
sub greet { defined &hello ? hello() : "NO-IMPORT" }
1;
PM

test_cl('a circular `use` still calls a custom import',
    qq{use lib "$DIR";\nuse CircA;\nprint CircB::greet(), "\\n";\n}
  . qq{print hello(), "\\n";\n},
    "hello from CircA\nhello from CircA\n");

# An EXPORTER import whose `our \@EXPORT = ...` sits BELOW the require imports
# NOTHING when it is reached circularly -- and does so SILENTLY, as perl does.
write_mod($DIR, 'ExpA', <<'PM');
package ExpA;
use Exporter;
our @ISA = ("Exporter");
require ExpB;
our @EXPORT = ("esym");
sub esym { "ExpA-sym" }
1;
PM
write_mod($DIR, 'ExpB', <<'PM');
package ExpB;
use ExpA;
our $seen = defined(&esym) ? "IMPORTED" : "NOT-IMPORTED";
1;
PM

test_cl('a circular Exporter import sees only what is defined so far',
    qq{use lib "$DIR";\nuse ExpA;\n}
  . qq{print "\$ExpB::seen\\n";\nprint esym(), "\\n";\n},
    "NOT-IMPORTED\nExpA-sym\n");

# `require` is load-only in perl, circular or not: no import call at all.
write_mod($DIR, 'ReqA', <<'PM');
package ReqA;
require ReqB;
sub import { my $c = caller; no strict 'refs';
             *{"${c}::rmark"} = sub { "IMPORTED" } }
1;
PM
write_mod($DIR, 'ReqB', <<'PM');
package ReqB;
require ReqA;
our $seen = defined(&rmark) ? "IMPORTED" : "NOT-IMPORTED";
1;
PM

test_cl('a circular `require` does NOT import',
    qq{use lib "$DIR";\nrequire ReqA;\nprint "\$ReqB::seen\\n";\n},
    "NOT-IMPORTED\n");

# The import list reaches a half-loaded module's import exactly as perl's does.
write_mod($DIR, 'ArgA', <<'PM');
package ArgA;
require ArgB;
sub import { my (undef, @a) = @_; my $c = caller; no strict 'refs';
             *{"${c}::args"} = sub { join ",", @a } }
1;
PM
write_mod($DIR, 'ArgB', <<'PM');
package ArgB;
use ArgA qw(x y);
sub what { args() }
1;
PM

test_cl('the import LIST reaches a circular import',
    qq{use lib "$DIR";\nuse ArgA qw(p q);\nprint ArgB::what(), "|", args(), "\\n";\n},
    "x,y|p,q\n");

# #2105 (closed s543c; fixed by an unknown earlier change, verified on
# 64353611): modules that set up Exporter INSIDE a BEGIN block -- the
# spelling that makes a circular import work in perl, because @EXPORT is
# ready before the other module's `use` runs -- used to die at load with a
# CL read error.  Expected text probed against perl 5.40.3.  This row runs
# the program through `runpcl` (the product's driver, = `pcl`): the bare
# `pl2cl FILE` + `sbcl --load` path the helpers above use still loses the
# circular import here ("Undefined subroutine &BegB::a_fn"), measured s543c
# and recorded in #2105.
write_mod($DIR, 'BegA', <<'PM');
package BegA;
BEGIN { require Exporter; our @ISA = ("Exporter"); our @EXPORT = ("a_fn"); }
use BegB;
sub a_fn { "a" }
sub a_calls_b { b_fn() }
1;
PM
write_mod($DIR, 'BegB', <<'PM');
package BegB;
BEGIN { require Exporter; our @ISA = ("Exporter"); our @EXPORT = ("b_fn"); }
use BegA;
sub b_fn { "b" }
sub b_calls_a { a_fn() }
1;
PM

{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh qq{use lib "$DIR";\nuse BegA;\nprint a_fn(), " ", BegA::a_calls_b(), " ", BegB::b_calls_a(), "\\n";\n};
    close $fh;
    is(scalar `$project_root/runpcl $pl_file 2>&1`, "a b a\n",
       '#2105 Exporter set up in BEGIN, used circularly: both imports arrive (runpcl)');
}
