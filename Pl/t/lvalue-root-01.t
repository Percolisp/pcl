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
# INVERSE GUARD (measured on a 57848f3 worktree): rows 1-4, 6-12 and 14 —
# every row that asserts a subscript variable is a RAW slot — FAIL there
# (the variable is `(make-p-box nil)` + `p-my-=`).  The other eleven pass on
# both sides: they are the "a box IS right" half (row 5 is the `=` arm,
# which already had the rule) plus the PCL_OPT rows, and they are what
# stops the fix from being widened into a wrong one.
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
plan tests => 23;

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
like($inc, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-post\+\+ \(p-gethash-box %h \$k\)\)\)/,
     'hash-incdec: $h{$k}++ leaves $k a raw slot');
unlike($inc, qr/\(let \(\(\$k \(make-p-box nil\)\)\)/,
     'hash-incdec: no box allocated for the key');

my $ainc = cl_of('my @a; for my $n (1..3) { my $i = 0 + $n; $a[$i]++; }');
like($ainc, qr/\(let \(\(\$i \(p-\+ 0 \$n\)\)\) \(p-post\+\+ \(p-aref-box \@a \$i\)\)\)/,
     'array-incdec: $a[$i]++ leaves the INDEX a raw slot');

my $cat = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k} .= "x"; }');
like($cat, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-\.= \(p-gethash %h \$k\) "x"\)\)/,
     'hash-concat: a compound assign to an element reads the key');

my $asn = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k} = 1; }');
like($asn, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(setf \(p-gethash %h \$k\) 1\)\)/,
     'hash-assign: the `=` arm keeps the rule it always had');

my $sub = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k} =~ s/a/b/; }');
like($sub, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-=~ \(p-gethash-box %h \$k\)/,
     'hash-subst: a substitution TARGET is an element write, the key a read');

my $chp = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; chomp $h{$k}; }');
like($chp, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-chomp \(p-gethash-box %h \$k\)\)\)/,
     'hash-chomp: a mutating builtin writes the element, not the key');

my $nst = cl_of('my %h; for my $n (1..3) { my $k = "k".$n; my $j = "j".$n; $h{$k}{$j}++; }');
like($nst, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(let \(\(\$j \(p-\. "j" \$n\)\)\)/,
     'nested: $h{$k}{$j}++ over a plain %h root writes NO scalar');
unlike($nst, qr/\(let \(\(\$[kj] \(make-p-box nil\)\)\)/,
       'nested: neither key is boxed');

my $slc = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; @h{$k} = (1); }');
like($slc, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-setf \(p-hslice %h \$k\)/,
     'slice: a hash-slice LHS reads its keys');

my $rte = cl_of('my %h; my @o; for my $n (1..3) { my $k = "k".$n; push @o, \$h{$k}; }');
like($rte, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-push \@o \(p-backslash \(p-gethash-box %h \$k\)\)\)\)/,
     'ref-to-elem: \\$h{$k} refs the ELEMENT — the key stays raw');

my $wrt = cl_of('sub bump { $_[0] .= "+" } my %h;'
              . ' for my $n (1..3) { my $k = "k" . $n; bump($h{$k}); }');
like($wrt, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-void-ctx \(pl-bump \(p-gethash-argbox %h \$k\)\)\)\)/,
     'writer-arg: an element passed to an @_-writing sub reads the key');

# --- a box IS right ------------------------------------------------------

my $der = cl_of('my $r; for my $n (1..3) { my $k = "k" . $n; $r->{$k}++; }');
like($der, qr/\(let \(\(\$r \(make-p-box nil\)\)\)/,
     'deref-root: the SCALAR root of a deref chain stays boxed (autoviv writes back)');
like($der, qr/\(let \(\(\$k \(p-\. "k" \$n\)\)\) \(p-post\+\+ \(p-gethash-deref-box \$r \$k\)\)\)/,
     'deref-root: … and its key is still only read');

my $emb = cl_of('my $z; for my $n (1..3) { my $x = 0 + $n; $z = ++($x = 5); }');
like($emb, qr/\(let \(\(\$x \(make-p-box nil\)\)\)/,
     'incdec-of-assign: ++($x = 5) really writes $x → boxed');

my $dsc = cl_of('my $v = 1; for my $n (1..3) { my $r = \$v; $$r++; }');
like($dsc, qr/\(let \(\(\$r \(make-p-box nil\)\)\)/,
     'deref-scalar: $$r++ writes through $r → boxed');

my $rtk = cl_of('my @o; for my $n (1..3) { my $k = "k" . $n; push @o, \$k; }');
like($rtk, qr/\(let \(\(\$k \(make-p-box nil\)\)\)/,
     'ref-taken: \\$k on the variable ITSELF still boxes it');

my $chs = cl_of('my %h; for my $n (1..3) { my $s = "s" . $n; chomp $s; $h{$s}++; }');
like($chs, qr/\(let \(\(\$s \(make-p-box nil\)\)\)/,
     'chomp-scalar: chomp $s writes $s → boxed (the marking still fires)');

my $sbs = cl_of('my %h; for my $n (1..3) { my $s = "s" . $n; $s =~ s/a/b/; $h{$s}++; }');
like($sbs, qr/\(let \(\(\$s \(make-p-box nil\)\)\)/,
     'subst-scalar: $s =~ s/// writes $s → boxed');

my $wra = cl_of('sub bump { $_[0] .= "+" } my %h;'
              . ' for my $n (1..3) { my $s = "s" . $n; bump($s); $h{$s}++; }');
like($wra, qr/\(let \(\(\$s \(make-p-box nil\)\)\)/,
     'writer-arg-scalar: a plain scalar passed to an @_-writing sub stays boxed');

# --- the registry still owns the transform -------------------------------

my $none = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k}++; }',
                 PCL_OPT => 'none');
like($none, qr/\(let \(\(\$k \(make-p-box nil\)\)\) \(p-my-= \$k \(p-\. "k" \$n\)\)/,
     'PCL_OPT=none: the key is boxed — the general form is unchanged');

my $noslot = cl_of('my %h; for my $n (1..3) { my $k = "k" . $n; $h{$k}++; }',
                   PCL_OPT => '-raw-slot');
like($noslot, qr/\(let \(\(\$k \(make-p-box nil\)\)\) \(p-my-= \$k \(p-\. "k" \$n\)\)/,
     '-raw-slot: the un-boxed key is exactly the raw-slot transform');

my $ainc_none = cl_of('my @a; for my $n (1..3) { my $i = 0 + $n; $a[$i]++; }',
                      PCL_OPT => 'none');
like($ainc_none, qr/\(let \(\(\$i \(make-p-box nil\)\)\)/,
     'PCL_OPT=none: the array index is boxed too');
