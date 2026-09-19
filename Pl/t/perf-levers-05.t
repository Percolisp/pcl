#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-05.t — the round-34 levers (s473x,
# docs/plan-speed-and-ir-s470.md §A.4, docs/faster-codegen-suggestions.md
# §0.2o), guarded the way perf-levers-04.t guards round 31's: both levers are
# RUNTIME-only, `pl2cl`'s output is byte-identical with and without them
# (corpus-diff IDENTICAL over 111), so a transpile grep can say nothing.  What
# can be asserted is (a) the MECHANISM — the named helpers exist, which is what
# FAILS on a pre-s473x tree — and (b) that every shape either lever can be
# handed still answers perl 5.40.3's answer.
#
#   p-flatten-args (task #1517 half two): a whole-array argument used to be
#     spread one element at a time — one hairy-vector read of the source and
#     one vector-push-extend into the result PER ELEMENT.  sb-sprof put those
#     two at ~70 % of the `feargs` row and the element PROMOTION the code is
#     really about at 0.2 %.  It is now ONE grow and ONE `replace` through
#     %p-vec-data's simple-vectors (%p-flatten-vector-into / %p-flatten-grow /
#     %p-flatten-run), followed by a patch pass that touches only the slots
#     which are not already boxes.  %p-flatten-vector-slow keeps the old
#     spread for a vector that is not the plain shape.  NO new per-array fact
#     was needed, and nothing happens at all for a SCALAR argument — which is
#     what killed #883's pre-sizing arm.
#   parse-perl-number (%p-plain-integer-string): the commonest string a Perl
#     program numifies is an ASCII integer, and it used to take the whole
#     general body — three whole-string allocations (a left trim, a
#     `string-downcase`, a right trim) made only to test for inf/nan, a run of
#     subseq+string= probes, and finally `read-from-string`, the full CL
#     READER.  An exactly-signed-digits string is now a digit loop; everything
#     else answers NIL and takes the general body unchanged.
#
# EVERY EXPECTATION BELOW IS PERL 5.40.3's OWN OUTPUT, probed by running the
# same program under perl (scratch/s473x/guard-sem.pl in the s473x worktree).
# The ANSWER rows pass on a pre-s473x tree as well — they are the correctness
# NET for a pure speed lever (the s473v rule); the MECHANISM rows are the ones
# that fail there.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;
plan tests => 78;

sub run_pl {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# THE MECHANISM — read out of the loaded runtime itself.  These are the rows
# that FAIL on a pre-s473x tree.  %p-vec-data is the CONTROL: it predates this
# batch and is fbound on both trees, so a probe that answered NIL for
# everything could not pass silently.
# ─────────────────────────────────────────────────────────────────────────────
my ($mfh, $mfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $mfh <<'LISP';
(format t "bulk ~a~%"      (and (fboundp 'pcl::%p-flatten-vector-into) t))
(format t "grow ~a~%"      (and (fboundp 'pcl::%p-flatten-grow) t))
(format t "run ~a~%"       (and (fboundp 'pcl::%p-flatten-run) t))
(format t "slow ~a~%"      (and (fboundp 'pcl::%p-flatten-vector-slow) t))
(format t "plainint ~a~%"  (and (fboundp 'pcl::%p-plain-integer-string) t))
(format t "control ~a~%"   (and (fboundp 'pcl::%p-vec-data) t))
;; The fast integer path's OWN answers: the shape it takes, and the shapes it
;; DECLINES (NIL) so the general body keeps them.
(dolist (s (list "42" "-42" "+42" "0" "-0" "007" "" "-" " 12" "12 " "3.14"
                 "1e3" "3rd" "abc" "1_000" "1 2" "12345678901234567890"))
  (format t "pi[~a] ~a~%" s (pcl::%p-plain-integer-string s)))
LISP
close $mfh;
my $mech = `sbcl @sbcl_rt --load $mfile 2>&1`;

like($mech, qr/^bulk T$/mi,
     '%p-flatten-vector-into exists: a whole-array argument is copied in bulk');
like($mech, qr/^grow T$/mi,
     '%p-flatten-grow exists: the result grows ONCE per array argument');
like($mech, qr/^run T$/mi,
     '%p-flatten-run exists: the replace + patch pass over one run');
like($mech, qr/^slow T$/mi,
     '%p-flatten-vector-slow exists: the old spread is kept for odd vectors');
like($mech, qr/^plainint T$/mi,
     '%p-plain-integer-string exists: the reader-free integer path');
like($mech, qr/^control T$/mi,
     'control: %p-vec-data is fbound on every tree, so the probe itself works');

like($mech, qr/^pi\[42\] 42$/mi,        'fast path: "42" is 42');
like($mech, qr/^pi\[-42\] -42$/mi,      'fast path: "-42" is -42');
like($mech, qr/^pi\[\+42\] 42$/mi,      'fast path: "+42" is 42');
like($mech, qr/^pi\[0\] 0$/mi,          'fast path: "0" is 0, not a decline');
like($mech, qr/^pi\[-0\] 0$/mi,         'fast path: "-0" is 0');
like($mech, qr/^pi\[007\] 7$/mi,        'fast path: leading zeros');
like($mech, qr/^pi\[12345678901234567890\] 12345678901234567890$/mi,
     'fast path: a bignum-sized run of digits is exact, as read-from-string was');
like($mech, qr/^pi\[\] NIL$/mi,         'declines: the empty string');
like($mech, qr/^pi\[-\] NIL$/mi,        'declines: a lone sign');
like($mech, qr/^pi\[ 12\] NIL$/mi,      'declines: a leading blank (the trim path owns it)');
like($mech, qr/^pi\[12 \] NIL$/mi,      'declines: a trailing blank');
like($mech, qr/^pi\[3\.14\] NIL$/mi,    'declines: a decimal point');
like($mech, qr/^pi\[1e3\] NIL$/mi,      'declines: an exponent');
like($mech, qr/^pi\[3rd\] NIL$/mi,      'declines: trailing junk');
like($mech, qr/^pi\[abc\] NIL$/mi,      'declines: no digits at all');
like($mech, qr/^pi\[1_000\] NIL$/mi,    'declines: an underscore');
like($mech, qr/^pi\[1 2\] NIL$/mi,
     'declines: an embedded blank — the extent scan owns that one');

# ─────────────────────────────────────────────────────────────────────────────
# THE ANSWERS — perl 5.40.3's, for every shape either lever can be handed.
# ─────────────────────────────────────────────────────────────────────────────
my $num = run_pl(<<'PERL');
no warnings;
printf "n-int %s\n",      "42" + 0;
printf "n-neg %s\n",      "-42" + 0;
printf "n-plus %s\n",     "+42" + 0;
printf "n-zero %s\n",     "0" + 0;
printf "n-negzero %s\n",  "-0" + 0;
printf "n-lead0 %s\n",    "007" + 0;
printf "n-empty %s\n",    "" + 0;
printf "n-sign %s\n",     "-" + 0;
printf "n-space %s\n",    "  12  " + 0;
printf "n-tail %s\n",     "3rd" + 0;
printf "n-float %s\n",    "3.14" + 0;
printf "n-floatx %s\n",   "3.14foo" + 0;
printf "n-exp %s\n",      "1e3" + 0;
printf "n-expneg %s\n",   "2.5e-3" + 0;
printf "n-hexstr %s\n",   "0x10" + 0;
printf "n-inf %s\n",      ("inf" + 0 > 1e300 ? "INF" : "no");
printf "n-nan %s\n",      ("nan" + 0 == "nan" + 0 ? "no" : "NAN");
printf "n-alpha %s\n",    "abc" + 0;
printf "n-embed %s\n",    "1 2" + 0;
printf "n-inner %s\n",    "12a34" + 0;
printf "n-dot %s\n",      "." + 0;
printf "n-undersc %s\n",  "1_000" + 0;
printf "n-nl %s\n",       "12\n" + 0;
printf "n-cmp %s\n",      ("10" == 10 ? "eq" : "ne");
printf "n-sort %s\n",     join(",", sort { $a <=> $b } ("10", "9", "-2", "100"));
PERL

my @num_rows = (
    ['n-int 42',        'an ASCII integer string numifies to itself'],
    ['n-neg -42',       'a negative integer string'],
    ['n-plus 42',       'a leading + is dropped'],
    ['n-zero 0',        '"0" is 0'],
    ['n-negzero 0',     '"-0" is 0'],
    ['n-lead0 7',       'leading zeros are not octal'],
    ['n-empty 0',       'the empty string is 0'],
    ['n-sign 0',        'a lone sign is 0'],
    ['n-space 12',      'surrounding blanks are trimmed'],
    ['n-tail 3',        'the leading numeric portion wins ("3rd")'],
    ['n-float 3.14',    'a decimal still parses'],
    ['n-floatx 3.14',   'a decimal with trailing junk'],
    ['n-exp 1000',      'scientific notation still parses'],
    ['n-expneg 0.0025', 'a negative exponent'],
    ['n-hexstr 0',      '"0x10" is 0 in perl, not 16'],
    ['n-inf INF',       '"inf" is still infinity'],
    ['n-nan NAN',       '"nan" is still a NaN'],
    ['n-alpha 0',       'a non-numeric string is 0'],
    ['n-embed 1',       '"1 2" stops at the blank'],
    ['n-inner 12',      '"12a34" stops at the letter'],
    ['n-dot 0',         'a lone dot is 0'],
    ['n-undersc 1',     'an underscore ends the number'],
    ['n-nl 12',         'a trailing newline'],
    ['n-cmp eq',        'numeric == against an integer string'],
    ['n-sort -2,9,10,100', 'a numeric sort of integer STRINGS'],
);
for my $r (@num_rows) {
    my ($line, $desc) = @$r;
    my $q = quotemeta $line;
    like($num, qr/^$q$/m, "numify: $desc");
}

my $flat = run_pl(<<'PERL');
no warnings;
sub cnt  { scalar @_ }
sub jn   { join("|", map { defined $_ ? $_ : "u" } @_) }
sub wr   { $_[0] = "W"; $_[2] = "X" if @_ > 2; return scalar @_ }
sub ex   { join ",", map { exists $_[$_] ? 1 : 0 } 0 .. $#_ }
my @a = (1, 2, 3);
printf "f-cnt %s\n",   cnt(@a);
printf "f-join %s\n",  jn(@a);
printf "f-mix %s\n",   jn(0, @a, 9);
printf "f-two %s\n",   jn(@a, @a);
my @e = ();
printf "f-empty %s\n", cnt(@e) . "/" . cnt(@e, @a);
my @w = (1, 2, 3);
printf "f-write %s\n", wr(@w) . " " . jn(@w);
my @h; $h[3] = "d";
printf "f-hole %s\n",  cnt(@h) . " " . jn(@h) . " " . ex(@h);
sub fill { $_[1] = "V"; return scalar @_ }
my @h2; $h2[3] = "d";
printf "f-holewr %s\n", fill(@h2) . " " . jn(@h2) . " " . (exists $h2[1] ? 1 : 0);
my %hh = (k => "v");
printf "f-hash %s\n",  jn(sort keys %hh) . " " . cnt(%hh);
my @big = (1 .. 100);
printf "f-big %s\n",   cnt(@big) . " " . jn($big[0], $big[99]);
my @grown = (1 .. 5);
sub grow { push @grown, 99; return scalar @_ }
printf "f-grow %s\n",  grow(@grown) . " " . scalar(@grown);
my @s = ("a", 2, 3.5, undef, [7]);
printf "f-kinds %s\n", join(",", map { !defined($_) ? "u" : ref($_) ? ref($_) : $_ } @s);
sub second { return $_[1] }
printf "f-twice %s\n", second(@a) . second(@a);
my @al = (1, 2, 3);
sub alias_all { $_++ for @_; return scalar @_ }
printf "f-alias %s\n", alias_all(@al) . " " . jn(@al);
my @nest = (1, 2);
sub outer2 { return inner2(@_) }
sub inner2 { $_[0] = "I"; return scalar @_ }
printf "f-nest %s\n", outer2(@nest) . " " . jn(@nest);
PERL

my @flat_rows = (
    ['f-cnt 3',                 'a whole array flattens to its elements'],
    ['f-join 1|2|3',            'in order'],
    ['f-mix 0|1|2|3|9',         'a scalar before and after the array'],
    ['f-two 1|2|3|1|2|3',       'the SAME array twice in one call'],
    ['f-empty 0/3',             'an empty array contributes nothing'],
    ['f-write 3 W|2|X',         '$_[0] and $_[2] write THROUGH into the array'],
    ['f-hole 4 u|u|u|d 0,0,0,1','holes read undef and stay non-exists'],
    ['f-holewr 4 u|V|u|d 1',    'a write through a hole vivifies the source slot'],
    ['f-hash k 2',              'a hash argument spreads to key/value pairs'],
    ['f-big 100 1|100',         'a 100-element array: count and both ends'],
    ['f-grow 5 6',              'the callee may push to the array it was handed'],
    ['f-kinds a,2,3.5,u,ARRAY', 'mixed strings, numbers, undef and a ref'],
    ['f-twice 22',              'the same array flattened twice answers alike'],
    ['f-alias 3 2|3|4',         'foreach over @_ aliases every element'],
    ['f-nest 2 I|2',            '@_ passed on to a second sub still writes through'],
);
for my $r (@flat_rows) {
    my ($line, $desc) = @$r;
    my $q = quotemeta $line;
    like($flat, qr/^$q$/m, "flatten: $desc");
}

# ─────────────────────────────────────────────────────────────────────────────
# #1918 — NUMIFICATION READS ASCII DIGITS ONLY.
#
# The round-34 fast path (%p-plain-integer-string, above) tests
# `(<= 0 (- code 48) 9)` and is right; the GENERAL BODY it falls through to
# used CL's `digit-char-p`, which SBCL answers for every Unicode Nd character.
# So `"\x{661}\x{662}" + 0` was 12 where perl says 0, and `"12\x{663}" + 0`
# was 123 where perl says 12 — perl's grok_number reads ASCII digits and stops.
# Worse in two of the shapes: the extent scan TOOK the non-ASCII digit and the
# CL reader then refused the substring, so `"12.\x{663}"` and `"1e\x{663}"`
# came back 0 where perl gives 12 and 1.
#
# ONE predicate, %p-ascii-digit-p, used by the extent scan of parse-perl-number
# and by looks-like-number (the same grok_number question).  The REGEX side is
# a DIFFERENT question and is untouched: perl's `\d` DOES match Unicode digits
# without /a, and PCL's does not — measured here, filed as #1972.
#
# EVERY EXPECTATION IS PERL 5.40.3's OWN OUTPUT
# (scratch/s491a/probes/p1918.{pl,perl.out}).
{
    my ($nfh, $nfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $nfh <<'LISP';
(format t "asciidigit ~a~%" (and (fboundp 'pcl::%p-ascii-digit-p) t))
(format t "control ~a~%"    (and (fboundp 'pcl::parse-perl-number) t))
LISP
    close $nfh;
    my $mech = `sbcl @sbcl_rt --load $nfile 2>&1`;
    like($mech, qr/^asciidigit T$/mi,
         '#1918 mechanism: the ASCII-only digit predicate exists');
    like($mech, qr/^control T$/mi,
         '#1918 control: parse-perl-number is fbound on both trees');
}

{
    my $out = run_pl(<<'PERL');
no warnings;
my @cases = ("\x{ff11}\x{ff12}", "\x{661}\x{662}", "12\x{663}", "\x{663}12",
             "1\x{660}2", "12.\x{663}", "1e\x{663}", "\x{1D7CE}",
             "12", " 12 ", "-12", "3.5", "1e3");
my $i = 0;
for my $c (@cases) {
    $i++;
    my $inc = $c; $inc++;
    printf "n%02d %s %s %s %s %s\n", $i, $c + 0, $c * 1, int($c), -$c, $inc;
}
PERL
    my @rows = (
        ['n01 0 0 0 0 1',                'FULLWIDTH ONE TWO numifies to 0'],
        ['n02 0 0 0 0 1',                'ARABIC-INDIC ONE TWO numifies to 0'],
        ['n03 12 12 12 -12 13',          'an ASCII run stops at the first non-ASCII digit'],
        ['n04 0 0 0 0 1',                'a LEADING non-ASCII digit numifies to 0'],
        ['n05 1 1 1 -1 2',               'a non-ASCII digit INSIDE an ASCII run ends it'],
        ['n06 12 12 12 -12 13',          'the decimal part stops at a non-ASCII digit'],
        ['n07 1 1 1 -1 2',               'an exponent of non-ASCII digits is not an exponent'],
        ['n08 0 0 0 0 1',                'a non-BMP mathematical digit numifies to 0'],
        ['n09 12 12 12 -12 13',          'the ASCII control is unmoved'],
        ['n10 12 12 12 -12 13',          '... with surrounding blanks'],
        ['n11 -12 -12 -12 12 -11',       '... signed'],
        ['n12 3.5 3.5 3 -3.5 4.5',       '... a float'],
        ['n13 1000 1000 1000 -1000 1001','... an exponent'],
    );
    for my $r (@rows) {
        my ($line, $desc) = @$r;
        my $q = quotemeta $line;
        like($out, qr/^$q$/m, "#1918 numify: $desc");
    }
}
