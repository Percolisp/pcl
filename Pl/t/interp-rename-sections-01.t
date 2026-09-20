#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# interp-rename-sections-01.t — a rename inside an interpolating "full" quote
# must not truncate its body (task #1921, s473t6c).
#
# A PPI full-quote token (qq{} qq[] qx{} qr{} m{} s{}{} <$fh>) keeps its body
# as a SECTION RECORD — {position, size} offsets into its own content — and
# every body reader slices the content with it.  `set_content` replaces the
# text and leaves that record alone, so when Parser2's interpolation rewriter
# lengthens a name ($v -> $v__file__0, the identity promotion a sub forces)
# the body was SILENTLY CUT at the ORIGINAL length:
#
#   qq{A "q{x $v y}"}          ->  A "q{x            (tail gone)
#   s{x $v y}{got $v ok}       ->  le__0 y}{3 y      (mangled, both sections)
#   qx{echo "a{$v}b"}          ->  (empty)
#
# The rename only happens when the name actually moves, which needs BOTH an
# inner-block declaration of the same name (so there is a region to rename)
# and a sub reading the file-level one (so it is identity-promoted) — that is
# why the fixture carries both.  t/op/exec.t is where this was found: its
# `system qq{$Perl -le "print q{ok $tnum - ...}"}` line lost its tail.

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

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

plan tests => 18;

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

# ONE program, one SBCL launch: every quote class that carries a section
# record, all under the same rename.
my $prog = <<'PROG';
my $out;
{ my $v = 1; $out = qq{A "q{x $v y}"}; }
my $v = 3;
print "QQ=", qq{A "q{x $v y}"}, "\n";
print "QQBR=", qq[A [x $v y] z], "\n";
my $s = "x 3 y";
$s =~ s{x $v y}{got $v ok};
print "SUBST=$s\n";
print "M=", ("x 3 y" =~ m{x $v y} ? "yes" : "no"), "\n";
my $qr = qr{x $v y};
print "QR=", ("x 3 y" =~ $qr ? "yes" : "no"), "\n";
my $qx = qx{echo "a{$v}b"};
chomp $qx;
print "QX=$qx\n";
print "PLAIN=", "p $v q", "\n";
sub g { return $v }
print "G=", g(), "\n";
PROG

my %got = map { /^([A-Z]+)=(.*)\z/s ? ($1 => $2) : () } split /\n/, run_cl($prog);

# The three shapes that were SILENTLY WRONG before the fix.
is($got{QQ},    'A "q{x 3 y}"', 'qq{} with a nested brace keeps its tail after a rename');
is($got{QQBR},  'A [x 3 y] z',  'qq[] with a nested bracket keeps its tail after a rename');
is($got{SUBST}, 'got 3 ok',     's{}{} rewrites BOTH sections, pattern and replacement');
is($got{QX},    'a{3}b',        'qx{} keeps its command body after a rename');

# The shapes that already worked — the net that says the repair did not move
# anything else (probe the case a change would BREAK).
is($got{M},     'yes',          'm{} still matches after a rename');
is($got{QR},    'yes',          'qr{} still matches after a rename');
is($got{PLAIN}, 'p 3 q',        'a plain "…" (no section record) is unaffected');
is($got{G},     '3',            'the promoted lexical itself still reads');

# ── #1919: TWO ADJACENT occurrences of the SAME renamed name ────────────────
#
# `_interp_fixer`'s escape guard used to CONSUME the character to the left of
# the sigil — `((?:^|[^\\])(?:\\\\)*)` — so under /g the second of two
# adjacent occurrences had its left context eaten by the first match and was
# never seen: `"A[$v$v]"` printed `A[7]`, silently, and both sides of an s///
# lost one the same way.  The guard is now the zero-width `(?<!\\)((?:\\\\)*)`,
# which says the same thing (an EVEN number of backslashes before the sigil).
#
# ONE program, one SBCL launch.  Rows 9-11 are the shapes that must NOT move:
# a SEPARATED pair, an ESCAPED sigil, and an escaped BACKSLASH before a real
# one.  Every expectation is perl 5.40.3's own output.
my $adj = <<'PROG';
{ my $v = 1; my @a = (9); print "inner $v @a\n"; }
my $v = 7;
my @a = (1, 2);
sub f {
    print "TWO=[$v$v]\n";
    print "THREE=[$v$v$v]\n";
    print "ARRAY=[@a@a]\n";
    print "LAST=[$#a$#a]\n";
    my $s = "xx";
    (my $r = $s) =~ s/x/[$v$v]/;
    print "REPL=$r\n";
    print "PAT=", ("7x7" =~ /$v$v/ ? "match" : "no-match"), "\n";
    print "PAT2=", ("77x" =~ /$v$v/ ? "match" : "no-match"), "\n";
    print "SEP=[$v-$v]\n";
    print "ESC=[\$v$v]\n";
    print "BSL=[\\$v]\n";
}
f();
PROG

my %adj = map { /^([A-Z0-9]+)=(.*)\z/s ? ($1 => $2) : () } split /\n/, run_cl($adj);

is($adj{TWO},   '[77]',     'two ADJACENT occurrences of a renamed name both rewrite');
is($adj{THREE}, '[777]',    'three adjacent occurrences all rewrite');
is($adj{ARRAY}, '[1 21 2]', 'adjacent @array interpolations both rewrite');
is($adj{LAST},  '[11]',     'adjacent $#array interpolations both rewrite');
is($adj{REPL},  '[77]x',    'an s/// REPLACEMENT keeps both adjacent occurrences');
is($adj{PAT},   'no-match', 'an s///-style PATTERN keeps both, so 7x7 does NOT match');
is($adj{PAT2},  'match',    '... and 77x does');
is($adj{SEP},   '[7-7]',    'a SEPARATED pair is unchanged');
is($adj{ESC},   '[$v7]',    'an ESCAPED sigil is still not interpolated');
is($adj{BSL},   '[\7]',     'a literal backslash before a real occurrence is kept');
