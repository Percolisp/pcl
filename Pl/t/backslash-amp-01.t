#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# backslash-amp-01.t: `\` over a `&`-MENTION is the sub slot, per ELEMENT
# (task #1681).
#
# `\&foo` had the intercept; `\(&foo)` did not — so the parenthesised spelling
# lowered its element through the ordinary `&`-prefix CALL path, CALLED foo
# with the caller's @_, and took a reference to the RESULT.  t/comp/proto.t's
# `a_sub \(&tmp_sub_1)` then handed a SCALAR ref to a `(&)` prototype, and
# `&{$_[0]}` died "Undefined subroutine &main::1" — one aborted top-level form
# that cost the file 155 of its 216 rows.
#
# The discriminator is perl's own and it is the ARGUMENT PARENS, not the
# parens around the list: `\(&foo)` is a CODE ref, `\(&foo())` is a SCALAR ref
# to the call's value.  Every expectation below was taken from perl 5.40.3
# running the same program.
#
# The INVERSE rows are the point of the file: an element that is NOT a
# mention (a call, a scalar, an aggregate, a range, a slice) must keep the
# lowering it had, and the bare spellings `\&foo` / `\&$cr` / `\&{$cr}` must
# not move.

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

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan skip_all => "sbcl not found"  if ! `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 16;

# One SBCL launch for the whole family: each line prints one answer, and
# @CALLS records whether a mention was (wrongly) called.
my $prog = <<'EOF';
our @CALLS;
sub foo { push @CALLS, 'foo'; 1 }
sub bar { push @CALLS, 'bar'; 2 }
sub Other::baz { push @CALLS, 'baz'; 3 }
my $cr = \&foo;
my $x = 5; my @arr = (7, 8); my %h = (k => 9);

# --- a mention inside \( … ) is the sub slot, and nothing is called
my $s = \(&foo);
print "t1:", ref($s), ":@CALLS\n"; @CALLS = ();
my @a = \(&foo, &bar);
print "t2:", scalar(@a), ":", join(",", map { ref } @a), ":@CALLS\n"; @CALLS = ();
my @b = \(&foo, $x);
print "t3:", join(",", map { ref } @b), ":@CALLS\n"; @CALLS = ();
my $c = \(&$cr);
print "t4:", ref($c), ":@CALLS\n"; @CALLS = ();
my $d = \(&{$cr});
print "t5:", ref($d), ":@CALLS\n"; @CALLS = ();
my $q = \(&Other::baz);
print "t6:", ref($q), ":@CALLS\n"; @CALLS = ();

# --- the ref is the SUB: calling through it runs it once
print "t7:", $s->(), ":@CALLS\n"; @CALLS = ();

# --- the shape that aborted t/comp/proto.t: a (&) prototype fed \(&NAME)
sub a_sub (&) { return ref($_[0]) . "/" . &{$_[0]} }
sub tmp { 42 }
print "t8:", a_sub(\(&tmp)), "\n";

# --- INVERSE: ARGUMENT PARENS make it a call, so the ref is to the VALUE
my $f1 = \(&foo());
print "t9:", ref($f1), ":@CALLS\n"; @CALLS = ();
my $f2 = \(&$cr());
print "t10:", ref($f2), ":@CALLS\n"; @CALLS = ();

# --- INVERSE: the bare spellings are unchanged
print "t11:", ref(\&foo), ":", ref(\&$cr), ":", ref(\&{$cr}), ":@CALLS\n"; @CALLS = ();

# --- INVERSE: non-mention elements keep their lowering
my @o = \($x, @arr, %h);
print "t12:", join(",", map { ref } @o), "\n";
my @m = \(&foo, 1 .. 2);
print "t13:", scalar(@m), ":", join(",", map { ref } @m), ":@CALLS\n"; @CALLS = ();
my @sl = \(&foo, @arr[0, 1]);
print "t14:", scalar(@sl), ":", join(",", map { ref } @sl), ":@CALLS\n"; @CALLS = ();

# --- the FOURTH member of the prototype ref-slot table: a `\&` slot takes
#     the sub slot of a `&`-MENTION, exactly as `\&NAME` does
sub gc (\&) { return ref($_[0]) }
print "t15:", gc(&foo), ":@CALLS\n"; @CALLS = ();
# --- INVERSE: the three slots that already worked are unmoved
sub ga (\@) { return ref($_[0]) . scalar(@{$_[0]}) }
sub gh (\%) { return ref($_[0]) }
sub gs (\$) { return ref($_[0]) }
print "t17:", ga(@arr), ":", gh(%h), ":", gs($x), "\n";
EOF

my $out = run_cl($prog);
my %got = map { /^(t\d+):(.*)$/ ? ($1, $2) : () } split /\n/, $out;

# perl 5.40.3's answers, measured.
my @expect = (
    [ t1  => 'CODE:',                    '\\(&NAME) is a CODE ref and calls nothing' ],
    [ t2  => '2:CODE,CODE:',             '\\(&NAME, &NAME) is two CODE refs' ],
    [ t3  => 'CODE,SCALAR:',             '\\(&NAME, $x) mixes a sub slot and a scalar ref' ],
    [ t4  => 'CODE:',                    '\\(&$cr) is the coderef itself' ],
    [ t5  => 'CODE:',                    '\\(&{$cr}) is the coderef itself' ],
    [ t6  => 'CODE:',                    '\\(&Pkg::NAME) is a CODE ref' ],
    [ t7  => '1:foo',                    'the ref calls the sub exactly once' ],
    [ t8  => 'CODE/42',                  'a (&) prototype fed \\(&NAME) receives a CODE ref' ],
    [ t9  => 'SCALAR:foo',               'INVERSE: \\(&NAME()) is a ref to the call VALUE' ],
    [ t10 => 'SCALAR:foo',               'INVERSE: \\(&$cr()) is a ref to the call VALUE' ],
    [ t11 => 'CODE:CODE:CODE:',          'INVERSE: the bare \\&NAME spellings are unmoved' ],
    [ t12 => 'SCALAR,ARRAY,HASH',        'INVERSE: non-mention elements keep their lowering' ],
    [ t13 => '3:CODE,SCALAR,SCALAR:',    'INVERSE: a range element still spreads' ],
    [ t14 => '3:CODE,SCALAR,SCALAR:',    'INVERSE: a slice element still spreads' ],
    [ t15 => 'CODE:',                    'a (\\&) prototype slot takes the sub slot of &NAME' ],
    [ t17 => 'ARRAY2:HASH:SCALAR',       'INVERSE: the \\@ / \\% / \\$ slots are unmoved' ],
);

for my $e (@expect) {
    my ($key, $want, $desc) = @$e;
    is($got{$key} // "(missing)\n--- output ---\n$out", $want, "$key: $desc");
}
