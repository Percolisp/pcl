#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser2;
use FindBin;
use lib "$FindBin::Bin";
use PCLCore;

# The sbcl command line comes from the ONE builder every runner shares
# (tools/lib/PCLSbcl.pm via PCLCore::sbcl_prefix, task #344): the saved core
# with the runtime already compiled in, and the 512 MB control stack.  This
# file used to spell `sbcl --noinform --non-interactive --load
# cl/pcl-runtime.lisp` itself, which recompiles the whole runtime on EVERY row
# -- 2.89 CPU-s a row against 0.007 s from the core (measured s473u, #1544) --
# and ran on the default 2 MB stack, which is exactly the drift #344 exists to
# stop.
my @sbcl_rt = PCLCore::sbcl_prefix("$FindBin::Bin/../../cl/pcl-runtime.lisp");

sub run_pl {
    my $code = shift;
        my $cl_code = Pl::Parser2->parse_code($code);

    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;

    my $output = `sbcl @sbcl_rt --load "$filename" 2>&1`;
    unlink $filename;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s+//;

    return $output;
}

plan tests => 36;

# ─── Group A: (setf p-aref) intermediate slots must be nil, not boxes ───
# When @a=('a','b','c') and we assign $a[4], slot 3 should !exist.

{
    my $out = run_pl(q{
our @a = ('a','b','c');
$a[4] = 'd';
say exists $a[3] ? "exists" : "not-exists";
say defined $a[3] ? "defined" : "undef";
say scalar(@a);
});
    is($out, "not-exists\nundef\n5\n", 'A1: intermediate slot not-exists/undef after extension');
}

# ─── Group B: local($a[N]) = $a[N] — RHS evaluated before fresh box ───

{
    my $out = run_pl(q{
our @a = ('a','b','c');
{
    local($a[1]) = 'X';
    local($a[2]) = $a[2];
    say $a[1];
    say $a[2];
}
say $a[1];
say $a[2];
});
    is($out, "X\nc\nb\nc\n", 'B1: local($a[N])=$a[N] copies old value, restores on exit');
}

# ─── Group C: undef @a inside local scope — restore re-extends array ───

{
    my $out = run_pl(q{
our @a = ('a','b','c');
{
    local($a[1]) = 'X';
    local($a[2]) = $a[2];
    undef @a;
}
say defined $a[0] ? $a[0] : "undef";
say $a[1];
say $a[2];
});
    is($out, "undef\nb\nc\n", 'C1: elements restored after undef @a inside local scope');
}

# ─── Group D: non-local body elements survive restore ───
# local($a[5])='z'; $a[4]='y' (independent) must NOT be removed on restore.

{
    my $out = run_pl(q{
our @a = ('a','b','c');
{
    local($a[5]) = 'z';
    $a[4] = 'y';
}
say scalar(@a);
say $a[4];
say exists $a[5] ? "exists" : "not-exists";
});
    is($out, "5\ny\nnot-exists\n", 'D1: non-local body element survives; local element removed');
}

# Slice version: local(@a[4,6])=('x','z'); $a[5]='y' (independent)
{
    my $out = run_pl(q{
our @a = ('a','b','c');
{
    local(@a[4,6]) = ('x','z');
    $a[5] = 'y';
}
say scalar(@a);
say defined $a[4] ? $a[4] : "undef";
say $a[5];
say exists $a[6] ? "exists" : "not-exists";
});
    is($out, "6\nundef\ny\nnot-exists\n", 'D2: slice local: non-local a[5] survives, local a[4]/a[6] removed');
}

# ─── Group E: delete local $a[N] for non-existent N doesn't change scalar() ───

{
    my $out = run_pl(q{
our @a = ('a','b','c');
{
    delete local $a[888];
    say scalar(@a);
    say exists $a[888] ? "exists" : "not-exists";
}
say scalar(@a);
});
    is($out, "3\nnot-exists\n3\n", 'E1: delete local non-existent keeps scalar() unchanged');
}

# ─── Group F: local @arr = @arr — init RHS must not be ignored ───

{
    my $out = run_pl(q{
our @a = (1,2,3);
my $d = "@a";
{
    local @a = @a;
    say "@a" eq $d ? "same" : "different";
}
say "@a" eq $d ? "restored" : "changed";
});
    is($out, "same\nrestored\n", 'F1: local @a = @a preserves elements inside scope');
}

# ─── Group G: my $c = delete local $a[N] — must wrap in p-local-array-elem ───

{
    my $out = run_pl(q{
our @a = ('a','b','c');
{
    my $c = delete local $a[1];
    say $c;
    say exists $a[1] ? "exists" : "not-exists";
}
say $a[1];
});
    is($out, "b\nnot-exists\nb\n", 'G1: my $c = delete local $a[N]: returns val, restores on exit');
}

{
    my $out = run_pl(q{
our @a = ('a','b','c',undef,'d');
{
    my ($x,$y) = delete local @a[1,4];
    say $x;
    say $y;
    say exists $a[1] ? "exists" : "not-exists";
    say exists $a[4] ? "exists" : "not-exists";
}
say $a[1];
say $a[4];
});
    is($out, "b\nd\nnot-exists\nnot-exists\nb\nd\n",
       'G2: my ($x,$y) = delete local @a[N,M]: returns vals, restores on exit');
}

# ─── Group H: my $c = delete local $h{key} — must wrap in p-local-hash-elem ───

{
    my $out = run_pl(q{
our %h = (a=>1, b=>2, c=>3);
{
    my $c = delete local $h{b};
    say $c;
    say exists $h{b} ? "exists" : "not-exists";
}
say $h{b};
});
    is($out, "2\nnot-exists\n2\n", 'H1: my $c = delete local $h{k}: returns val, restores on exit');
}

{
    my $out = run_pl(q{
our %h = (a=>1, b=>2, c=>3, d=>4);
{
    my ($x,$y) = delete local @h{qw/b d/};
    say $x;
    say $y;
    say exists $h{b} ? "exists" : "not-exists";
    say exists $h{d} ? "exists" : "not-exists";
}
say $h{b};
say $h{d};
});
    is($out, "2\n4\nnot-exists\nnot-exists\n2\n4\n",
       'H2: my ($x,$y) = delete local @h{k1,k2}: returns vals, restores on exit');
}

# ─── Integration: combined scenario from perl-tests/local.t ───

{
    my $out = run_pl(q{
our @a = ('a','b','c');
$a[4] = 'd';
{
    delete local $a[1];
    say exists $a[3] ? "exists" : "not-exists";
    say $a[4];
    {
        delete local $a[888];
        say scalar(@a);
        my ($dd,$zzz) = delete local @a[4,999];
        say scalar(@a);
        say $dd;
        say defined $zzz ? $zzz : "undef";
        my $cc = delete local $a[2];
        say scalar(@a);
        say $cc;
    }
}
say scalar(@a);
say $a[1];
say $a[2];
say $a[4];
});
    is($out, "not-exists\nd\n5\n3\nd\nundef\n1\nc\n5\nb\nc\nd\n",
       'I1: combined delete local array scenario matches perl-tests/local.t');
}

{
    my $out = run_pl(q{
our %h = (a=>1, b=>2, c=>3, d=>4);
{
    delete local $h{b};
    {
        delete local $h{yyy};
        my ($dd,$zzz) = delete local @h{qw/d zzz/};
        my $cc = delete local $h{c};
        say $dd;
        say defined $zzz ? $zzz : "undef";
        say $cc;
        $h{yyy} = 888;
        $h{zzz} = 999;
    }
}
say scalar(keys %h);
say $h{b};
say $h{c};
say $h{d};
say exists $h{yyy} ? "exists" : "not-exists";
say exists $h{zzz} ? "exists" : "not-exists";
});
    is($out, "4\nundef\n3\n4\n2\n3\n4\nnot-exists\nnot-exists\n",
       'I2: combined delete local hash scenario matches perl-tests/local.t');
}

# ─── Per-fix verification tests ───────────────────────────────────────────────

# Fix A: setf-p-aref fills intermediate slots with nil
{
    my $out = run_pl(q{
our @a;
$a[0] = 'a';
$a[3] = 'd';
say exists $a[1] ? "exists" : "not-exists";
say exists $a[2] ? "exists" : "not-exists";
say defined $a[1] ? "defined" : "undef";
say scalar(@a);
});
    is($out, "not-exists\nnot-exists\nundef\n4\n",
       'Fix-A: sparse assignment creates non-existing intermediate slots');
}

# Fix B: local($h{k}) = $h{k} RHS before fresh box
{
    my $out = run_pl(q{
our %h = (a=>1, b=>2);
{
    local($h{a}) = $h{a};
    say $h{a};
}
say $h{a};
});
    is($out, "1\n1\n", 'Fix-B: local($h{k}) = $h{k} copies old value');
}

# Fix C: local $hash{key} = EXPR where RHS depends on other local
{
    my $out = run_pl(q{
our %h = (a=>10, b=>20);
{
    local $h{a} = $h{b};
    say $h{a};
}
say $h{a};
});
    is($out, "20\n10\n", 'Fix-C: local $h{a} = $h{b} reads b correctly');
}

# Fix D: local @hash = %hash
{
    my $out = run_pl(q{
our %h = (a=>1, b=>2);
my $orig = join(",", map { "$_=$h{$_}" } sort keys %h);
{
    local %h = %h;
    say join(",", map { "$_=$h{$_}" } sort keys %h) eq $orig ? "same" : "diff";
}
say join(",", map { "$_=$h{$_}" } sort keys %h) eq $orig ? "same" : "diff";
});
    is($out, "same\nsame\n", 'Fix-D: local %h = %h preserves and restores hash');
}

# Fix E: p-local-array-elem restore with orig-len trimming
{
    my $out = run_pl(q{
our @a = (1,2,3);
{
    local $a[5] = 99;
    $a[4] = 77;
}
say scalar(@a);
say $a[4];
say exists $a[5] ? "exists" : "not-exists";
});
    is($out, "5\n77\nnot-exists\n", 'Fix-E: only local element removed; body-assigned element survives');
}

# Fix F: delete local does not affect other elements
{
    my $out = run_pl(q{
our @a = ('a','b','c',undef,'d');
{
    delete local $a[1];
    $a[3] = 'e';
}
say $a[1];
say $a[3];
say scalar(@a);
});
    is($out, "b\ne\n5\n", 'Fix-F: delete local restores deleted; body-assigned survives');
}

# Fix G: my $c = delete local scalar and restore
{
    my $out = run_pl(q{
our @a = (10,20,30);
{
    my $val = delete local $a[1];
    say $val;
    say scalar(@a);
}
say $a[1];
say scalar(@a);
});
    is($out, "20\n3\n20\n3\n", 'Fix-G: delete local scalar returns value, restores element');
}

# Fix H: delete local slice and restore
{
    my $out = run_pl(q{
our %h = (x=>10, y=>20, z=>30);
{
    my ($a,$b) = delete local @h{qw/x z/};
    say $a;
    say $b;
    say scalar(keys %h);
}
say $h{x};
say $h{z};
say scalar(keys %h);
});
    is($out, "10\n30\n1\n10\n30\n3\n", 'Fix-H: delete local hash slice returns values, restores');
}

# Fix I: delete local $h{k} for non-existent key — removed on exit
{
    my $out = run_pl(q{
our %h = (a=>1);
{
    delete local $h{nosuchkey};
    $h{nosuchkey} = 999;
}
say exists $h{nosuchkey} ? "exists" : "not-exists";
});
    is($out, "not-exists\n", 'Fix-I: delete local non-existent key — new val removed on exit');
}

# Fix J: local @a = @a then modify in scope; outer unchanged
{
    my $out = run_pl(q{
our @a = (1,2,3);
{
    local @a = @a;
    push @a, 99;
    say scalar(@a);
}
say scalar(@a);
});
    is($out, "4\n3\n", 'Fix-J: local @a = @a makes independent copy; outer unchanged after scope');
}

# Fix K: exists check inside delete local $a[N] block
{
    my $out = run_pl(q{
our @a = ('a','b','c');
$a[4] = 'd';
{
    delete local $a[1];
    say exists $a[3] ? "exists" : "not-exists";
}
});
    is($out, "not-exists\n", 'Fix-K: exists $a[3] false inside delete local $a[1] block (no box fill)');
}

# RT #7411: local $#a = N truncates the array inside the block.
# Perl localizes the array-length magic; the truncation takes effect immediately.
# (Perl does NOT restore on scope exit — that is a known Perl limitation, RT #7411,
# marked local $::TODO in perl-tests/local.t — so we only assert the in-block effect.)
{
    my $out = run_pl(q{
my @a = (1..5);
{
    local $#a = 2;
    say "$#a [@a]";
}
});
    is($out, "2 [1 2 3]\n", 'RT #7411: local $#a = 2 shortens array inside block');
}

# local $#a = N can also extend the array (with undef tail) inside the block.
{
    my $out = run_pl(q{
my @a = (1,2,3);
{
    local $#a = 4;
    say scalar(@a);
}
});
    is($out, "5\n", 'RT #7411: local $#a = 4 extends array length inside block');
}

# ─── Group I: delete local on an ARROW-DEREF element ($ref->{k} / $ref->[N]) ──
# Was silently dropping the `local` (no save/restore) so the element never
# restored on scope exit (local.t 119/120).  Both `my $x = delete local …` and
# the standalone form must wrap in p-local-{hash,array}-elem on the referent.
{
    my $out = run_pl(q{
my $a = { b => 1 };
{
    my $bb = delete local $a->{b};
    say "$bb ", scalar(keys %$a);
    $a->{d} = 3;
}
say scalar(keys %$a), " ", $a->{b}, " ", $a->{d};
});
    is($out, "1 0\n2 1 3\n",
       'I1: my $x = delete local $ref->{k} restores the hash elem on exit');
}

{
    my $out = run_pl(q{
my $a = [10, 20, 30];
{
    my $x = delete local $a->[1];
    say "$x ", join(",", map { defined $_ ? $_ : "u" } @$a);
}
say join(",", map { defined $_ ? $_ : "u" } @$a);
});
    is($out, "20 10,u,30\n10,20,30\n",
       'I2: my $x = delete local $ref->[N] restores the array elem on exit');
}

{
    my $out = run_pl(q{
my $a = { b => 1, c => 2 };
{
    delete local $a->{b};
    say join(",", sort keys %$a);
}
say join(",", sort keys %$a);
});
    is($out, "c\nb,c\n",
       'I3: standalone delete local $ref->{k} restores the hash elem on exit');
}

# ─── Group J: plain `local $ref->{k} = v` (no delete) on an arrow-deref ───────
# Was mis-binding the scalar $ref itself to the RHS (a runtime crash); must wrap
# the referent's element in p-local-hash-elem-init / p-local-array-elem-init.
{
    my $out = run_pl(q{
my $a = { b => 1 };
{
    local $a->{b} = 99;
    say $a->{b};
}
say $a->{b};
});
    is($out, "99\n1\n",
       'J1: local $ref->{k} = v installs then restores the hash elem');
}

{
    my $out = run_pl(q{
my $a = [10, 20, 30];
{
    local $a->[1] = 99;
    say $a->[1];
}
say $a->[1];
});
    is($out, "99\n20\n",
       'J2: local $ref->[N] = v installs then restores the array elem');
}

# ─── Group K: conditional `local X = V if/unless COND` (statement modifier) ───
# Previously the modifier leaked into the RHS parse and died ("Missing case").
# True cond: localize+assign then restore.  False cond: value unchanged.
{
    my $out = run_pl(q{
our %h = (k => 7);
{ local $h{k} = 99 if 1; say "in=$h{k}"; }
say "out=$h{k}";
});
    is($out, "in=99\nout=7\n", 'K1: local $h{k}=V if TRUE installs then restores');
}
{
    my $out = run_pl(q{
our %h = (k => 7);
{ local $h{k} = 99 if 0; say "in=$h{k}"; }
say "out=$h{k}";
});
    is($out, "in=7\nout=7\n", 'K2: local $h{k}=V if FALSE leaves value unchanged');
}
{
    my $out = run_pl(q{
our $x = 3;
{ local $x = 9 unless 0; say "in=$x"; }
say "out=$x";
});
    is($out, "in=9\nout=3\n", 'K3: local $x=V unless FALSE installs then restores');
}
{
    my $out = run_pl(q{
our @y = (1, 2);
{ local @y = (8, 9) if 0; say "in=@y"; }
say "out=@y";
});
    is($out, "in=1 2\nout=1 2\n", 'K4: local @y=LIST if FALSE leaves array unchanged');
}

# ─── Group L: the key of a LOCAL hash element is the element's key (#4062) ───
# `local $h{@a}` keys on @a's COUNT, as `$h{@a}` does (#3865): the v1 seam
# compiled the subscript as a free expression and keyed on the array itself.
# Expected text probed under perl 5.40.3 (scratch/s530c/pr/loc1.pl).
{
    my $out = run_pl(q{
my @a = (1,2); our %h = (2 => "two", 1 => "one"); our @b = (10, 11, 12, 13);
{ local $h{@a} = "loc"; print "1 $h{2}\n"; } print "2 $h{2}\n";
{ local $h{@a}; print "3 ", (defined $h{2} ? $h{2} : "undef"), "\n"; } print "4 $h{2}\n";
{ local $b[@a] = 99; print "5 @b\n"; } print "6 @b\n";
my $r = \@a;
{ local $h{@$r} = "lr"; print "9 $h{2}\n"; } print "10 $h{2}\n";
{ local @h{@a} = ("x", "y"); print "11 $h{1} $h{2}\n"; } print "12 $h{1} $h{2}\n";
{ local ($h{@a}, $b[0]) = ("p", "q"); print "13 $h{2} $b[0]\n"; } print "14 $h{2} $b[0]\n";
our $hr = \%h; { local $hr->{@a} = "dr"; print "15 $h{2}\n"; } print "16 $h{2}\n";
});
    is($out, "1 loc\n2 two\n3 undef\n4 two\n5 10 11 99 13\n6 10 11 12 13\n9 lr\n10 two\n"
           . "11 x y\n12 one two\n13 p q\n14 two 10\n15 dr\n16 two\n",
       'L1: local $h{@a} / $hr->{@a} / ($h{@a}, ...) key on the COUNT; slices still spread');
}
