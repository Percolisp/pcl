#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# lvalue-root-01.t — a write to a container ELEMENT writes the element; the
# subscript is a READ (task #995, s466bd).
#
# `Pl::VarAnnotator`'s tree walk records a boxing event for every lvalue it
# cannot prove is a plain scalar.  Until s466bd only the `=` arm asked WHERE
# the write lands: `++`/`--`, the compound assigns, `=~`, the mutating
# builtins and `\` all marked the WHOLE operand subtree, so `$h{$k}++`,
# `$h{$k} .= "x"` and `chomp $h{$k}` recorded a write against the hash KEY
# (and `$a[$i]++` against the index) and boxed a variable perl only reads.
# On the arrhash bench program that one false positive was 15.4 %
# %make-p-box + 8.1 % box-set of the run.
#
# Every write arm now asks ONE marker, `_tw_mark_lvalue`, which walks the
# access chain down to its ROOT: a plain %h/@a root means no scalar is
# written at all, a SCALAR root ($r->{A}) is written back by
# autovivification and must stay boxed, and anything else keeps the whole
# subtree marked.  The rows below are one shape each — the raw slot where
# perl only reads, the box where a box is right.
#
# Each row asserts the declaration's CLASS, which #1035 step 1 put on the
# binding itself: `(p-let ((NAME CLASS INIT)) …)`.  `:scalar` IS the claim
# here (a raw unboxed slot), `:box` is its negation, so a row cannot pass by
# matching a shape that lost its verdict.
#
# INVERSE GUARD (measured on a MAIN worktree, 9bee19b): rows 1-4, 6-12 and
# 14 — every row that asserts a subscript variable is a RAW slot, the two
# `unlike` rows included — FAIL there (the variable is `:box (make-p-box
# nil)` + `p-my-=`).  The other eleven pass on both sides: they are the "a
# box IS right" half (row 5 is the `=` arm, which already had the rule) plus
# the PCL_OPT rows, and they are what stops the fix from being widened into
# a wrong one.  The base must be a tree that already has `p-let` (#1035,
# f330e5f): on anything older every row fails for the WRONG reason — the
# spelling, not the verdict.
#
# The last section (rows 24-30) is task #1056, the same table's other half:
# which access spellings CLASSIFY their subscript's use.  Its inverse guard is
# c80b1a0, where rows 24, 26, 27, 28 and 29 fail (the subscript variable is
# `:box (make-p-box nil)`) while 25 and 30 — the two negatives — pass.
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
my $pl2cl = "$project_root/pl2cl";
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan tests => 30;

# Transpile one snippet.  Every case wraps its shape in a counting loop so
# the subscript variable is a fresh block `my` — the raw-slot candidate the
# task is about.
sub cl_of {
    my ($src, %env) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "$src\n";
    close $fh;
    local @ENV{keys %env} = values %env;
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    $cl =~ s/\s+/ /g;
    return $cl;
}

# --- the subscript is a READ: a raw let slot, no box ----------------------

my $inc = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k}++; }');
like($inc, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-post\+\+ \(p-gethash-box %h \$k\)\)\)/,
     'hash-incdec: $h{$k}++ leaves $k a raw slot');
unlike($inc, qr/\(\$k :box \(make-p-box nil\)\)/,
     'hash-incdec: no box allocated for the key');

my $ainc = cl_of('my @a; for my $n (1..3) { my $i = 0 + $n; $a[$i]++; }');
like($ainc, qr/\(p-let \(\(\$i :scalar \(p-\+ 0 \$n\)\)\) \(p-post\+\+ \(p-aref-box \@a \$i\)\)\)/,
     'array-incdec: $a[$i]++ leaves the INDEX a raw slot');

my $cat = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k} .= "x"; }');
like($cat, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-\.= \(p-gethash %h \$k\) "x"\)\)/,
     'hash-concat: a compound assign to an element reads the key');

my $asn = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k} = 1; }');
like($asn, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(setf \(p-gethash %h \$k\) 1\)\)/,
     'hash-assign: the `=` arm keeps the rule it always had');

my $sub = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k} =~ s/a/b/; }');
like($sub, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-=~ \(p-gethash-box %h \$k\)/,
     'hash-subst: a substitution TARGET is an element write, the key a read');

my $chp = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; chomp $h{$k}; }');
like($chp, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-chomp \(p-gethash-box %h \$k\)\)\)/,
     'hash-chomp: a mutating builtin writes the element, not the key');

my $nst = cl_of('my %h; for my $n (1..3) { my $k = "k".$n; my $j = "j".$n; $h{$k}{$j}++; }');
like($nst, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-let \(\(\$j :scalar \(p-\. "j" \$n\)\)\)/,
     'nested: $h{$k}{$j}++ over a plain %h root writes NO scalar');
unlike($nst, qr/\(\$[kj] :box \(make-p-box nil\)\)/,
       'nested: neither key is boxed');

my $slc = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; @h{$k} = (1); }');
like($slc, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-setf \(p-hslice %h \$k\)/,
     'slice: a hash-slice LHS reads its keys');

my $rte = cl_of('my %h; my @o; for my $n (1..3) { my $k = "k".$n; push @o, \$h{$k}; }');
like($rte, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-push \@o \(p-backslash \(p-gethash-box %h \$k\)\)\)\)/,
     'ref-to-elem: \\$h{$k} refs the ELEMENT — the key stays raw');

my $wrt = cl_of('sub bump { $_[0] .= "+" } my %h;'
              . ' for my $n (1..3) { my $k = "k" . $n; bump($h{$k}); }');
like($wrt, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-void-ctx \(pl-bump \(p-gethash-argbox %h \$k\)\)\)\)/,
     'writer-arg: an element passed to an @_-writing sub reads the key');

# --- a box IS right ------------------------------------------------------

my $der = cl_of('my $r; for my $n (1..3) { my $k = "k" . $n; $r->{$k}++; }');
like($der, qr/\(p-let \(\(\$r :box \(make-p-box nil\)\)\)/,
     'deref-root: the SCALAR root of a deref chain stays boxed (autoviv writes back)');
like($der, qr/\(p-let \(\(\$k :scalar \(p-\. "k" \$n\)\)\) \(p-post\+\+ \(p-gethash-deref-box \$r \$k\)\)\)/,
     'deref-root: … and its key is still only read');

my $emb = cl_of('my $z; for my $n (1..3) { my $x = 0 + $n; $z = ++($x = 5); }');
like($emb, qr/\(p-let \(\(\$x :box \(make-p-box nil\)\)\)/,
     'incdec-of-assign: ++($x = 5) really writes $x → boxed');

my $dsc = cl_of('my $v = 1; for my $n (1..3) { my $r = \$v; $$r++; }');
like($dsc, qr/\(p-let \(\(\$r :box \(make-p-box nil\)\)\)/,
     'deref-scalar: $$r++ writes through $r → boxed');

my $rtk = cl_of('my @o; for my $n (1..3) { my $k = "k" . $n; push @o, \$k; }');
like($rtk, qr/\(p-let \(\(\$k :box \(make-p-box nil\)\)\)/,
     'ref-taken: \\$k on the variable ITSELF still boxes it');

my $chs = cl_of('my %h; for my $n (1..3) { my $s = "s" . $n; chomp $s; $h{$s}++; }');
like($chs, qr/\(p-let \(\(\$s :box \(make-p-box nil\)\)\)/,
     'chomp-scalar: chomp $s writes $s → boxed (the marking still fires)');

my $sbs = cl_of('my %h; for my $n (1..3) { my $s = "s" . $n; $s =~ s/a/b/; $h{$s}++; }');
like($sbs, qr/\(p-let \(\(\$s :box \(make-p-box nil\)\)\)/,
     'subst-scalar: $s =~ s/// writes $s → boxed');

my $wra = cl_of('sub bump { $_[0] .= "+" } my %h;'
              . ' for my $n (1..3) { my $s = "s" . $n; bump($s); $h{$s}++; }');
like($wra, qr/\(p-let \(\(\$s :box \(make-p-box nil\)\)\)/,
     'writer-arg-scalar: a plain scalar passed to an @_-writing sub stays boxed');

# --- the registry still owns the transform -------------------------------

my $none = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k}++; }',
                 PCL_OPT => 'none');
like($none, qr/\(p-let \(\(\$k :box \(make-p-box nil\)\)\) \(p-my-= \$k \(p-\. "k" \$n\)\)/,
     'PCL_OPT=none: the key is boxed — the general form is unchanged');

my $noslot = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k}++; }',
                   PCL_OPT => '-raw-slot');
like($noslot, qr/\(p-let \(\(\$k :box \(make-p-box nil\)\)\) \(p-my-= \$k \(p-\. "k" \$n\)\)/,
     '-raw-slot: the un-boxed key is exactly the raw-slot transform');

my $ainc_none = cl_of('my @a; for my $n (1..3) { my $i = 0 + $n; $a[$i]++; }',
                      PCL_OPT => 'none');
like($ainc_none, qr/\(p-let \(\(\$i :box \(make-p-box nil\)\)\)/,
     'PCL_OPT=none: the array index is boxed too');

# --- ALL EIGHT access spellings classify their subscript's USE (task #1056) --
#
# %ACCESS_NODE is the one table that knows the [BASE, SUBSCRIPT...] shape, but
# only h_acc/a_acc used to say what the subscript POSITION means; the six deref
# and slice types fell through to the generic "opaque" tail.  `$h{$k}` and
# `$r->{$k}` stringify the key identically, yet only the first could give the
# slot the B-regime string freeze, and only `$a[$i]` the numeric one.  The
# rows below are one spelling each, on a slot whose write shape is UNPROVEN
# (`g()`) — which is exactly the case the B regime exists for.  `:str` /
# `:num` IS the claim; PCL_OPT=-raw-numeric is its negation.
my $ug = 'sub g { $_[0] } ';
my $dref = cl_of($ug . 'my $r = {}; my $t = 0;'
                . ' for my $n (1..3) { my $k = g($n); $t += $r->{$k}; }');
like($dref, qr/\(\$k :str \(%pcl-to-string-strict /,
     'h_ref_acc: a key read through ->{} is a STRINGIFY use');
like($dref, qr/\(\$r :box \(make-p-box nil\)\)/,
     'h_ref_acc: … while the deref ROOT stays opaque and boxed');

my $hsl = cl_of($ug . 'my $r = {}; '
              . 'for my $n (1..3) { my $k = g($n); my @v = @$r{$k, "z"}; }');
like($hsl, qr/\(\$k :str \(%pcl-to-string-strict /,
     'slice_h_acc: @$r{…} keys are stringify uses');

my $kvh = cl_of($ug . 'my $r = {}; '
              . 'for my $n (1..3) { my $k = g($n); my %w = %$r{$k}; }');
like($kvh, qr/\(\$k :str \(%pcl-to-string-strict /,
     'kv_slice_h_acc: %$r{…} keys are stringify uses');

my $aref = cl_of($ug . 'my $ar = []; my $t = 0;'
               . ' for my $n (1..3) { my $i = g($n); $t += $ar->[$i]; }');
like($aref, qr/\(\$i :num \(%pcl-to-number-strict /,
     'a_ref_acc: an index read through ->[] is a NUMIFY use');

my $asl = cl_of($ug . 'my $ar = []; '
              . 'for my $n (1..3) { my $i = g($n); my @u = @$ar[$i, 0]; }');
like($asl, qr/\(\$i :num \(%pcl-to-number-strict /,
     'slice_a_acc: @$ar[…] indices are numify uses');

my $dref_nb = cl_of($ug . 'my $r = {}; my $t = 0;'
                  . ' for my $n (1..3) { my $k = g($n); $t += $r->{$k}; }',
                    PCL_OPT => '-raw-numeric');
like($dref_nb, qr/\(\$k :box \(make-p-box nil\)\)/,
     '-raw-numeric: the freeze IS the transform — the key is boxed again');
