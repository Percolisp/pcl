#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Regression: `local` through a *symbolic* reference (a string naming a package
# variable) saves/restores that package variable; `local` through a *hard*
# reference dies with "Can't localize through a reference".
# Covers local ${...}, $$x, @{...}, @$x, %{...}, %$x  (op/localref.t).
#
# HARNESS NOTE (s446l, task #525).  `Pl::Parser2->parse_code` emits a bare
# `(in-package :pcl)` and NOT the `(p-defpackage :main) (in-package :main)`
# preamble `pl2cl` writes, so the program's globals used to be interned in the
# PCL package rather than in `main` — a package layout no real PCL program ever
# has.  That did not matter until the symbolic-ref resolver started answering
# perl's question ("an unqualified name resolves in the PERL-level current
# package", which is `main` here) instead of reading the CL reader's
# `*package*`: the direct `$aa` and the symbolic `${"aa"}` then named two
# different variables and `local` restored the one nobody read.  Through
# `./pl2cl` — every real program — the two always agreed, and still do (probed
# vs perl: this file's whole scalar/array/hash snippet is byte-identical).
# `run_pl` therefore switches to `main` before the program, which is what the
# real pipeline does.  The assertions below are UNCHANGED.

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser2;

sub run_pl {
    my $code = shift;
    my $cl_code = Pl::Parser2->parse_code($code);
    # Run the program in `main`, as pl2cl's preamble does (see HARNESS NOTE).
    $cl_code =~ s/\A\Q(in-package :pcl)\E/(in-package :pcl)\n(p-defpackage :main)\n(in-package :main)/
        or die "run_pl: parse_code output no longer starts with (in-package :pcl)";

    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;

    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^\s+//;
    return $output;
}

plan tests => 9;

# ── scalar: local ${aa} / ${"aa"} / ${$x} / $$x all localize $aa ──
{
    my $out = run_pl(q{
$aa = 1;
{ local ${aa};   $aa = 3; print "in1=$aa\n"; }
print "out1=$aa\n";
{ local ${"aa"}; $aa = 4; print "in2=$aa\n"; }
print "out2=$aa\n";
$x = "aa";
{ local ${$x};   $aa = 5; print "in3=$aa\n"; }
print "out3=$aa\n";
{ local $$x;     $aa = 7; print "in4=$aa\n"; }
print "out4=$aa\n";
});
    is($out, "in1=3\nout1=1\nin2=4\nout2=1\nin3=5\nout3=1\nin4=7\nout4=1\n",
       'local on symbolic scalar deref saves/restores $aa');
}

# ── array: local @{$x} / @$x localize @aa ──
{
    my $out = run_pl(q{
@aa = ('a','b');
$x = "aa";
{ local @{$x}; @aa = ('i','j'); print "in=@aa\n"; }
print "out=@aa\n";
{ local @$x;   @aa = ('m','n'); print "in=@aa\n"; }
print "out=@aa\n";
});
    is($out, "in=i j\nout=a b\nin=m n\nout=a b\n",
       'local on symbolic array deref saves/restores @aa');
}

# ── hash: local %{$x} / %$x localize %aa ──
{
    my $out = run_pl(q{
%aa = (a=>'b');
$x = "aa";
{ local %{$x}; %aa = (i=>'j'); print "in=$aa{i}\n"; }
print "out=$aa{a}\n";
{ local %$x;   %aa = (m=>'n'); print "in=$aa{m}\n"; }
print "out=$aa{a}\n";
});
    is($out, "in=j\nout=b\nin=n\nout=b\n",
       'local on symbolic hash deref saves/restores %aa');
}

# ── hard reference: local through a real ref is fatal ──
{
    my $out = run_pl(q{
$aa = 1;
$x = \$aa;
eval { local $$x; };
print $@ =~ /Can't localize through a reference/ ? "scalar-die\n" : "scalar-NODIE\n";
$x = \@aa;
eval { local @$x; };
print $@ =~ /Can't localize through a reference/ ? "array-die\n" : "array-NODIE\n";
$x = \%aa;
eval { local %$x; };
print $@ =~ /Can't localize through a reference/ ? "hash-die\n" : "hash-NODIE\n";
});
    is($out, "scalar-die\narray-die\nhash-die\n",
       'local through a hard reference dies');
}

# ── ${\$aa} and ${\'x'} (anon hard refs) also die ──
{
    my $out = run_pl(q{
$aa = 1;
eval { local ${\$aa}; };
print $@ =~ /Can't localize through a reference/ ? "ok1\n" : "no1\n";
eval { local @{[]}; };
print $@ =~ /Can't localize through a reference/ ? "ok2\n" : "no2\n";
});
    is($out, "ok1\nok2\n", 'local through anonymous hard refs dies');
}

# ── transpile-level: the macro is emitted (not silently dropped) ──
{
        my $cl = Pl::Parser2->parse_code('$aa=1; { local ${aa}; $aa=3; }');
    like($cl, qr/p-local-deref-scalar/, 'local ${aa} emits p-local-deref-scalar');
}
{
        my $cl = Pl::Parser2->parse_code('@aa=(1); { local @{$x}; }');
    like($cl, qr/p-local-deref-array/, 'local @{$x} emits p-local-deref-array');
}
{
        my $cl = Pl::Parser2->parse_code('%aa=(); { local %$x; }');
    like($cl, qr/p-local-deref-hash/, 'local %$x emits p-local-deref-hash');
}

# ── #1260 / #1243 (c): the INITIALIZER of a symbolic `local` ─────────────────
# `local ${'main::g'} = 9` used to localize and then DROP the assignment: this
# branch returned before it ever looked at the `=`, so the read inside the
# scope saw undef where perl sees 9 — silently, on every sigil.  The whole
# family in one program; every line is perl 5.40.3, probed s473a
# (scratch/s473a/probes/m3guard.pl), and q1-q9 + q13 all differ on the
# 41ca2496 extraction.  The SUBSCRIPTED spelling `local ${main::gh}{a} = 7`
# is NOT covered and is still wrong twice over (it localizes the scalar
# $main::gh and drops the init) — task #1341.
{
    my $out = run_pl(q{
$g = 1; @ga = (1,2,3); %gh = (a=>1); $c = 1; $evals = 0;
{ local ${'main::g'} = 9;  print "q1=$g\n"; } print "r1=$g\n";
{ local ${"main::g"} = 8;  print "q2=$g\n"; } print "r2=$g\n";
{ local ${'g'} = 7;        print "q3=$g\n"; } print "r3=$g\n";
{ local @{'main::ga'} = (4,5); print "q4=@ga\n"; } print "r4=@ga\n";
{ local %{'main::gh'} = (b=>2); print "q5=", join(",", map {"$_=$gh{$_}"} sort keys %gh), "\n"; }
print "r5=", join(",", map {"$_=$gh{$_}"} sort keys %gh), "\n";
$n = 'main::g'; $an = 'main::ga';
{ local ${$n} = 6; print "q6=$g\n"; } print "r6=$g\n";
{ local $$n = 5;   print "q7=$g\n"; } print "r7=$g\n";
{ local @{$an} = (7,8); print "q8=@ga\n"; } print "r8=@ga\n";
{ local ${'main::g'} = ${'main::g'} + 1; print "q9=$g\n"; } print "r9=$g\n";
{ local ${'main::g'} = 5 if $c;  print "q10=$g\n"; } print "r10=$g\n";
{ local ${'main::g'} = 5 if !$c; print "q11=$g\n"; } print "r11=$g\n";
sub side { $evals++; return 4 }
{ local ${'main::g'} = side() if !$c; print "q12=$g evals=$evals\n"; }
sub nm { $evals++; return 'main::g' }
{ local ${ nm() } = 3; print "q13=$g evals=$evals\n"; } print "r13=$g evals=$evals\n";
{ local ${'main::g'}; print "q14=", (defined $g ? $g : "undef"), "\n"; } print "r14=$g\n";
});
    is($out, join("", map {; "$_\n" }
                  'q1=9',  'r1=1',
                  'q2=8',  'r2=1',
                  'q3=7',  'r3=1',
                  'q4=4 5', 'r4=1 2 3',
                  'q5=b=2', 'r5=a=1',
                  'q6=6',  'r6=1',
                  'q7=5',  'r7=1',
                  'q8=7 8', 'r8=1 2 3',
                  'q9=2',  'r9=1',           # the RHS reads the OLD value
                  'q10=5', 'r10=1',          # a TRUE statement modifier
                  'q11=1', 'r11=1',          # a FALSE one localizes nothing
                  'q12=1 evals=0',           # …and never evaluates the RHS
                  'q13=3 evals=1', 'r13=1 evals=1',  # the NAME runs once
                  'q14=undef', 'r14=1'),    # bare: no init, still restored
       'a symbolic local ASSIGNS its initializer, on every sigil (#1260)');
}
