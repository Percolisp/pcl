#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# anon-hash-term-01.t -- PPI §50 (task #3742, s527e): where perl expects a
# term, a `{` is an ANONYMOUS HASH.  After die / warn / exists / delete
# (toke.c XTERM) always; after map / grep (XREF) when the look-ahead says so:
# an immediate `}`, or a word, number or string followed by `=>` -- or by a
# comma, unless the word starts with a lower-case letter.  PCL's PPI decides
# it now (Lexer _curly / _curly_hash_lookahead); before, PPI said Block and
# PCL's `_block_is_hash_constructor` (which STAYS) caught only `WORD =>`, so
# `map { Foo, $_ }, LIST`, `map {}, LIST` and `map { "k" => $_ }, LIST`
# lost their hashes (row 1 FAILS on the base; row 2 passed there through
# `_block_is_hash_constructor`).  The negatives -- a map BLOCK
# that merely looks like a pair -- are row 3.

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

plan tests => 3;

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
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- 1. map / grep: the look-ahead says HASH ------------------------------

both_agree(<<'PL', 'PPI §50: map/grep { Word, ... } / {} / { "str" => ... } followed by a comma build hashes');
my @m = map { Foo, $_ }, 1; print join(",", %{$m[0]}), "\n";
my @e = map {}, 1, 2; print scalar(@e), ref $e[0], "\n";
my @q = map { "k" => $_ }, 3; print $q[0]{k}, "\n";
my @w = map { lc => $_ }, 4; print $w[0]{lc}, "\n";
my @r = map({a => $_}, 1, 2); print scalar(@r), " ", $r[1]{a}, "\n";
my @g = grep {a => 1}, 1, 2; print scalar(@g), "\n";
PL

# ---- 2. XTERM: die / warn / exists / delete -------------------------------

both_agree(<<'PL', 'PPI §50: die / warn / exists / delete { ... } take an anonymous hash');
print exists {k=>1}->{k}, "|", delete({k=>5}->{k}), "\n";
eval { die { code => 42 } }; print $@->{code}, "\n";
local $SIG{__WARN__} = sub { print ref($_[0]) || "str", "\n" }; warn { a => 1 };
PL

# ---- 3. the negatives: a map / grep / sort BLOCK stays a block ------------

both_agree(<<'PL', 'PPI §50: { $_ => 1 } / {; "x" => 1 } / { lc($_) => 1 } / { +"$_" => 1 } stay map blocks');
my @t = map { $_ => 1 } 1, 2; print scalar(@t), "\n";
my @u = map {; "x$_" => 1 } 1, 2; print scalar(@u), "\n";
my @v = map { lc($_) => 1 } "A"; print "@v\n";
my %h = map { +"$_" => 1 } qw(a b); print join(",", sort keys %h), "\n";
my @f = grep { $_ > 1 } 1, 2, 3; print "@f\n";
my @s = sort { $a <=> $b } 3, 1; print "@s\n";
PL
