#!/usr/bin/env perl

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser;

sub run_pl {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    my $cl_code = $parser->parse();

    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;

    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s+//;

    return $output;
}

plan tests => 24;

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
