#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ref-identity-01.t: a reference's printed TYPE and ADDRESS are properties of
# the referent, not of the storage path it took (task #163).
#
# PCL represents `\$x` as a fresh wrapper box around $x's box.  Both the
# stringifier and `==` used to read the WRAPPER — so two `\$x` printed two
# different addresses and compared unequal — and box-sv guessed which level it
# was at by counting boxes, so the SAME reference printed SCALAR through a
# variable (`my $r = \$x; print $r`) and REF straight into print, into an
# array element, or into a raw sub parameter.  One rule now answers both:
# %p-ref-referent, keyed on the wrapper's is-ref flag, which is what p-ref
# already used for its LVALUE/REF arms.
#
# The INVERSE guards matter as much as the positives: widening `\$aref` to REF
# must not turn `\@a` or an element holding an array wrapper into REF too.

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

plan tests => 47;

# One SBCL launch for the whole family: each line prints one answer.
# Every expectation below was taken from real perl running the same program.
my $prog = <<'EOF';
sub ty { my $s = "$_[0]"; $s =~ s/\(0x[0-9a-f]+\)//; return $s }
my %h = (k => 1); my @a = (10, 20); my $x = 5;

# --- identity: two `\` of the same thing are the same reference
print "t1:", (\$x == \$x ? "EQ" : "NE"), "\n";
print "t2:", (\$h{k} == \$h{k} ? "EQ" : "NE"), "\n";
print "t3:", (\$a[0] == \$a[0] ? "EQ" : "NE"), "\n";
print "t4:", ("".\$x eq "".\$x ? "SAME" : "DIFF"), "\n";
print "t5:", (\$x == \$a[0] ? "COLLIDE" : "DISTINCT"), "\n";

# --- printed type does not depend on how the ref reached the printer
my $r = \$x;
print "t6:",  ty($r), "\n";          # through a variable
print "t7:",  ty(\$x), "\n";         # straight into a sub argument (raw slot)
print "t8:",  ty(\$h{k}), "\n";      # element referent
print "t9:",  ty(\5), "\n";          # \literal: a fresh box, still SCALAR
my @refs = (\$x); my %hr = (s => \$x);
print "t10:", ty($refs[0]), "\n";    # array element
print "t11:", ty($hr{s}), "\n";      # hash value

# --- SCALAR vs REF is decided by what the referent CURRENTLY holds
my $lit = \1; my $rr = \$lit;
print "t12:", ty($rr), "\n";         # referent holds a ref -> REF
$lit = 5;
print "t13:", ty($rr), "\n";         # ...now holds a plain scalar -> SCALAR
my $aref = \@a; my $href = \%h;
print "t14:", ref(\$aref), ":", ref(\$href), "\n";

# --- INVERSE: aggregate refs are untouched by the widening above
print "t15:", ty(\@a), ":", ty(\%h), ":", ty(\&ty), "\n";
print "t16:", ref($aref), ":", ref($href), "\n";
my @agg = (\@a, \%h);
print "t17:", ref($agg[0]), ":", ref($agg[1]), "\n";
print "t18:", (\@a == \@a ? "EQ" : "NE"), "\n";

# --- a ref to a plain SCALAR is not a container: perl's fatal, not a silent
#     empty list and not an SBCL type error (#154's two shapes)
my $z = 7; my $sref = \$z;
my @errs;
for my $t (sub { my @c = @$sref; 1 },
           sub { my %c = %$sref; 1 },
           sub { $sref->{k} },
           sub { $sref->[0] }) {
    push @errs, (eval { $t->(); 'NO-DIE' } // ($@ =~ /^(Not an? \w+ reference)/ ? $1 : "OTHER: $@"));
}
print "t19:", join("|", @errs), "\n";
print "t20:", $z, "\n";

# --- \&f identity through a RAW numeric slot (task #362, s407): $c2's only
#     use is `==`, so type flow freezes it to a raw number at the write; the
#     raw numifier had no arm for a function and answered 0, while $c1 (also
#     CALLED, so it stays a box) numified to the address — "diff".  Both
#     spellings must agree, and match perl's "same".
sub cf { 1 } my $c1 = \&cf; my $c2 = \&cf; my $cv = $c1->();
print "t21:", ($c1 == $c2 ? "EQ" : "NE"), "\n";
print "t22:", (0+$c2 > 0 ? "ADDR" : "ZERO"), "\n";

# --- the GLOB member of the same family (task #423, s424).  A glob VALUE
#     (`$g = *foo`) is not a reference at all — perl makes the SV a GV — while
#     a glob REF (`\*foo`) is; PCL tells them apart by the SAME is-ref flag
#     this file's rule is built on, and box-nv already read it (t25: the bare
#     glob numifies to 0 while printing GLOB(0x1) — the word and the number
#     disagreed).  Every copy path a scalar takes must carry the flag, or a
#     glob ref degrades into a glob value on the way (t28-t31).
sub foo { 42 }
my $g = *main::foo; my $gr = \*main::foo;
print "t23:", (ref($g) eq "" ? "NOTREF" : "ref=".ref($g)), ":", ref(\$g), "\n";
print "t24:", ty($g), "\n";
print "t25:", (0+$g), ":", (0+$gr > 0 ? "ADDR" : "ZERO"), "\n";
print "t26:", ref($gr), ":", ty($gr), "\n";
print "t27:", ref(\$gr), "\n";
my @ga = ($gr, $g); my %gh = (r => $gr, v => $g);
print "t28:", ref($ga[0]), ":", (ref($ga[1]) eq "" ? "NOTREF" : ref($ga[1])), "\n";
print "t29:", ref($gh{r}), ":", (ref($gh{v}) eq "" ? "NOTREF" : ref($gh{v})), "\n";
sub thru { my $x = shift; return ref($x) eq "" ? "NOTREF" : ref($x) }
print "t30:", thru($gr), ":", thru($g), "\n";
print "t31:", ty($ga[0]), ":", ty($ga[1]), "\n";
our $og = \*main::foo;
print "t32:", ref($og), ":", ty($og), "\n";
my $bg = bless \*main::foo, "D";
print "t33:", ref($bg), ":", ty($bg), "\n";
print "t34:", ty(*main::foo), ":", (ref(*main::foo) eq "" ? "NOTREF" : ref(*main::foo)), "\n";
my $sg = *main::foo; $sg =~ s/^\*//;
print "t35:", $sg, ":", ref(\$sg), "\n";
my $tg = *main::qux; $tg =~ tr/a-z/A-Z/;
print "t36:", $tg, "\n";
# INVERSE, and the case the first cut of the #423 fix BROKE: carrying the flag
# must not carry the BOX — a list assignment snapshots its RHS, so preserving
# the source box collapses ($g1,$g2) = ($g2,$g1) into one glob.
sub bar { 43 }
my $s1 = *main::foo; my $s2 = *main::bar; ($s1, $s2) = ($s2, $s1);
print "t37:", ty($s1), ":", ty($s2), "\n";
my $q1 = \*main::foo; my $q2 = \*main::bar; ($q1, $q2) = ($q2, $q1);
print "t38:", ("$q1" eq "$q2" ? "COLLAPSED" : "distinct"), ":", ref($q1), ":", ref($q2), "\n";
my $m1 = *main::foo; my $m2 = \*main::bar; ($m1, $m2) = ($m2, $m1);
print "t39:", ref($m1), ":", (ref($m2) eq "" ? "NOTREF" : ref($m2)), ":", ty($m2), "\n";

# --- \&$name (task #517): the SYMBOLIC spelling of \&NAME is the same
#     late-bound CODE ref, not a SCALAR ref to p-get-coderef's NIL.  Every
#     line here was taken from perl 5.40.3.
{ package L; sub AUTOLOAD { our $AUTOLOAD; return "AUTO($AUTOLOAD)" } }
my $bl = "L::gone";
print "t40:", ref(\&$bl), ":", ref(\&{"L::gone"}), ":", ref(\&L::gone), "\n";
print "t41:", (\&$bl)->(7), ":", (\&L::gone)->(7), "\n";
my $ln = "M::later"; my $lref = \&$ln;
print "t42:", ref($lref), "\n";
{ package M; sub later { "BODY(@_)" } }
print "t43:", $lref->(9), "\n";                 # body defined AFTER the ref
my $none = "NoSuchPkg42::x"; my $nref = \&$none;
print "t44:", ref($nref), ":",
      (eval { $nref->(); 1 } ? "LIVED"
        : ($@ =~ /Undefined subroutine &NoSuchPkg42::x called/ ? "msg-ok" : "msg=[$@]")), "\n";
# INVERSE: a value that already IS code, or a glob, keeps its old answer, and
# `defined &$name` still distinguishes a body from a name.
my $anon = sub { "ANON(@_)" }; my $ac = \&$anon;
print "t45:", ref($ac), ":", $ac->(1), ":", ($ac == $anon ? "same" : "differ"), "\n";
my $gref = \*main::foo; my $gc = \&$gref;
print "t46:", ref($gc), ":", $gc->(), "\n";
print "t47:", (defined(&$bl) ? "def" : "undef"), ":",
              (defined(&$ln) ? "def" : "undef"), "\n";
EOF

my $out = run_cl($prog);

like $out, qr/^t1:EQ$/m,
    '\\$x == \\$x — identity is the referent, not the fresh wrapper';
like $out, qr/^t2:EQ$/m,
    '\\$h{k} taken twice is one reference (the element box is stable)';
like $out, qr/^t3:EQ$/m,
    '\\$a[0] taken twice is one reference';
like $out, qr/^t4:SAME$/m,
    'and the printed address matches, not just the numeric compare';
like $out, qr/^t5:DISTINCT$/m,
    'INVERSE: refs to different scalars stay distinct';

like $out, qr/^t6:SCALAR$/m, 'SCALAR through a variable';
like $out, qr/^t7:SCALAR$/m, 'SCALAR straight into a raw sub parameter';
like $out, qr/^t8:SCALAR$/m, 'SCALAR for a hash-element referent';
like $out, qr/^t9:SCALAR$/m, '\\5 is SCALAR (its own fresh box), not REF';
like $out, qr/^t10:SCALAR$/m, 'SCALAR out of an array element';
like $out, qr/^t11:SCALAR$/m, 'SCALAR out of a hash value';

like $out, qr/^t12:REF$/m,
    'ref-to-ref prints REF while the referent holds a reference';
like $out, qr/^t13:SCALAR$/m,
    '...and becomes SCALAR once the referent holds a plain value (perl decides dynamically)';
like $out, qr/^t14:REF:REF$/m,
    'ref(\\$aref) is REF — the referent is the SCALAR, not the array it points to';

like $out, qr/^t15:ARRAY:HASH:CODE$/m,
    'INVERSE: \\@a / \\%h / \\&f still print their own kinds';
like $out, qr/^t16:ARRAY:HASH$/m,
    'INVERSE: the aggregate refs themselves are unchanged';
like $out, qr/^t17:ARRAY:HASH$/m,
    'INVERSE: an element holding an aggregate wrapper is ARRAY/HASH, not REF';
like $out, qr/^t18:EQ$/m,
    'INVERSE: \\@a == \\@a — aggregate identity is the container';

like $out, qr/^t19:Not an ARRAY reference\|Not a HASH reference\|Not a HASH reference\|Not an ARRAY reference$/m,
    'a scalar ref used as a container dies with perl\'s message on all four paths';
like $out, qr/^t20:7$/m,
    'INVERSE: the scalar behind that ref is untouched by the failed derefs';
like $out, qr/^t21:EQ$/m,
    '\\&f == \\&f when one side is a raw-numeric slot (#362: the raw numifier answered 0)';
like $out, qr/^t22:ADDR$/m,
    'a code ref in a raw slot numifies to its address, not 0';

like $out, qr/^t23:NOTREF:GLOB$/m,
    'a glob VALUE is not a reference — ref($g) is "" and \\$g is a GLOB ref (#423)';
like $out, qr/^\Qt24:*main::foo\E$/m,
    'a glob value stringifies to its perl spelling, not GLOB(0x…)';
like $out, qr/^t25:0:ADDR$/m,
    'and the NUMBER agrees with the word: bare glob 0, glob ref its address';
like $out, qr/^t26:GLOB:GLOB$/m,
    'INVERSE: a glob REF still says GLOB and prints GLOB(0x…)';
like $out, qr/^t27:REF$/m,
    'INVERSE: a ref to a scalar holding a glob REF is REF, not GLOB';
like $out, qr/^t28:GLOB:NOTREF$/m,
    'the flag survives an array element (a copy that dropped it degraded the ref)';
like $out, qr/^t29:GLOB:NOTREF$/m,
    'the flag survives a hash value';
like $out, qr/^t30:GLOB:NOTREF$/m,
    'the flag survives @_ and shift (%p-flatten-list snapshotted the raw glob)';
like $out, qr/^\Qt31:GLOB:*main::foo\E$/m,
    'and the two print their own spellings out of an array element';
like $out, qr/^t32:GLOB:GLOB$/m,
    'a glob ref in a PACKAGE variable (box-in-box store) prints GLOB(0x…), not SCALAR(0x…)';
like $out, qr/^t33:D:D=GLOB$/m,
    'INVERSE: bless \\*foo keeps D=GLOB(0x…)';
like $out, qr/^\Qt34:*main::foo:NOTREF\E$/m,
    'a RAW typeglob is a glob VALUE by the same convention (#316)';
like $out, qr/^t35:main::foo:SCALAR$/m,
    's/// on a glob value matches its VALUE spelling (op/gv.t; the s419d regression)';
like $out, qr/^\Qt36:*MAIN::QUX\E$/m,
    'tr/// likewise rewrites the value spelling, not GLOB(0X…)';
like $out, qr/^\Qt37:*main::bar:*main::foo\E$/m,
    'INVERSE: ($g1,$g2) = ($g2,$g1) SWAPS two glob values (carrying the flag must not carry the box)';
like $out, qr/^t38:distinct:GLOB:GLOB$/m,
    'INVERSE: and swapping two glob REFS keeps them distinct';
like $out, qr/^\Qt39:GLOB:NOTREF:*main::foo\E$/m,
    'INVERSE: a mixed swap moves the ref-ness with the value, not with the slot';

like $out, qr/^t40:CODE:CODE:CODE$/m,
    '\\&$name on a body-less name is CODE, like \\&NAME and \\&{"name"} (#517)';
like $out, qr/^\Qt41:AUTO(L::gone):AUTO(L::gone)\E$/m,
    '...and calling it reaches the package AUTOLOAD with the FULL name (it was empty)';
like $out, qr/^t42:CODE$/m,
    '\\&$name taken before the body exists is CODE';
like $out, qr/^\Qt43:BODY(9)\E$/m,
    '...and is LATE-BOUND: the body defined afterwards is the one that runs';
like $out, qr/^t44:CODE:msg-ok$/m,
    'a name in a package that does not exist is still CODE, and dies perl\'s death when called';
like $out, qr/^\Qt45:CODE:ANON(1):same\E$/m,
    'INVERSE: \\&$coderef is that same coderef';
like $out, qr/^t46:CODE:42$/m,
    'INVERSE: \\&$globref still reaches the glob\'s CODE slot';
like $out, qr/^t47:undef:def$/m,
    'INVERSE: `defined &$name` still tells a body from a bare name';
