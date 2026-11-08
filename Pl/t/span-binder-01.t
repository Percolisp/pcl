#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# span-binder-01.t -- the SPAN PASS asks the BINDER (s535a, parser program
# step 1c port 3; docs/binder-dual-run-s525.md §6 item 3), and the
# package-region residues fixed beside it (#4421 #4440 #4422 #475).
#
# v2 lowers each top-level `package NAME;` section as its own CL form, so a
# my-lexical used in a LATER section than its declaration must become a
# package cell.  The question "which lexicals do that" was a bare-name text
# pre-filter plus a sibling-walk shadow test (_ref_shadowed); it is now
# Pl::Binder::spans ("does binding B have a ref in segment j"), and the rename
# rewrites exactly B's uses (Pl::Binder::refers_to_decl).  Rows 1-9 FAIL on
# the base 06a4b8ba (inverse-verified on its extraction): a silently wrong
# value or a loud refusal of valid perl.  Rows 10-15 are the package-region
# residues (failing on the base too).  Rows 16-19 are shapes the old code got
# right and the port must keep; rows 20-21 (#4546, the promoter's rewrite)
# fail on the base too; row 22 is rule 12.  Every expected text is
# perl 5.40.3's own output.

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
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 35;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
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

# ── 1-3. #4425: a CHAINED declarator `my $S = my $F = 0` -- the old pass read
#    only the statement's FIRST declarator, so `$F` was never promoted and the
#    named sub in the other section incremented a different variable (F=0).
is(run_cl(<<'PL'), "F=2 S=2\n", '#4425: a chained `my` in a flattened bare block is promoted');
{
    my $S = my $F = 0;
    package T;
    sub g { ++$F; ++$S }
    package main;
    T::g(); T::g();
    print "F=$F S=$S\n";
}
PL
is(run_cl(<<'PL'), "F=2 S=2\n", '#4425: ... and at file level');
my $S = my $F = 0;
package T;
sub g { ++$F; ++$S }
package main;
T::g(); T::g();
print "F=$F S=$S\n";
PL
is(run_cl(<<'PL'), "F=2 S=2 ev=2/2 top=2\n", '#4425: ... and a string eval naming the chained one reaches its cell');
my $S = my $F = 0;
package T;
sub g { ++$F; ++$S }
package main;
T::g(); T::g();
sub ev { eval '"$F/$S"' }
print "F=$F S=$S ev=", ev(), " top=", eval('$F'), "\n";
PL

# ── 4. #3803's loud face: a compound-HEAD `for my $e` in a later section beside
#    a file `my $e` (and a `%e`) -- the old sibling walk could not scope the
#    loop variable, so the rename refused and the CHECKER killed the file.
is(run_cl(<<'PL'), "loop 1\nloop 2\nafter file hash\nmain file\n", '#3803: a loop-head `my` in a later section is its own variable');
my $e = "file";
my %e = (k => "hash");
package Foo;
for my $e (1, 2) { print "loop $e\n" }
print "after $e $e{k}\n";
package main;
print "main $e\n";
PL

# ── 5. an EXPRESSION-EMBEDDED file `my` (`open my $fh, ...`, a `my` in a
#    call's argument list) used in a later section -- the old pass saw only
#    `my` statements, so the later section read an EMPTY package variable.
is(run_cl(<<'PL'), "read: L1\nok=ok\n", 'an embedded file `my` used in a later section is promoted');
my $data = "L1\n";
open my $fh, '<', \$data or die;
sub mk { "ok" }
ok_(my $tag = mk());
sub ok_ { 1 }
package Foo;
my $l = <$fh>;
print "read: $l";
print "ok=$tag\n";
PL

# ── 6. a later section's TEXT run by a string eval in a named sub (the
#    string-eval rule of port 2): the eval sees the file lexical.
is(run_cl(<<'PL'), "6\n", 'an eval text in a later section reaches a file lexical');
my $x = 3;
sub run { eval $_[0] }
package Foo;
print main::run('$x * 2'), "\n";
PL

# ── 7. a use only inside a HEREDOC body after a flattened package block
#    (Test-Simple t/Legacy/fail-more.t's END block) -- the old text pre-filter
#    read statement CONTENT, which has no heredoc body, so the name was never
#    renamed and the checker refused the file.
is(run_cl(<<'PL'), "failed 38 tests of 38.\n", 'a heredoc-only use in a later section is a use');
my $Total = 38;
{ package Foo; sub new { 1 } }
END {
    print <<ERR;
failed $Total tests of $Total.
ERR
}
PL

# ── 8. `our $y` in a later section MASKS the file `my $y` for the rest of the
#    file: the old pass counted the masked uses as the lexical's (#470's
#    family) and the later section read "lex"; the alias's own uses take the
#    declaring package (#475's rewrite at a section boundary).
is(run_cl(<<'PL'), "foo pkg\nmain pkg pkg\n", 'an `our` in a later section masks the file `my`');
my $y = "lex";
package Foo;
our $y = "pkg";
print "foo $y\n";
package main;
print "main $y $Foo::y\n";
PL

# ── 9. chained my / our in each position, read in a later section.
is(run_cl(<<'PL'), "3 3 4 4 5 5\n4 5\n", 'chained my / our declarators, read across a section');
my $a1 = my $b1 = 3;
our $c1 = my $d1 = 4;
my $e1 = our $f1 = 5;
package Foo;
print "$a1 $b1 $c1 $d1 $e1 $f1\n";
package main;
print "$main::c1 $main::f1\n";
PL

# ── 10-12. #4421: a `package NAME { BLOCK }` lowered IN PLACE (in a sub, an
#    `if` block, a string eval, another package block) homes its bare globals
#    to NAME, as a `package NAME;` region does.  The base wrote main::.
is(run_cl(<<'PL'), "main=u P=5 Po=1\n", '#4421: a package block inside a sub body');
sub run { package P { $x = 5; our $o = 1; } }
run();
print "main=", $main::x // "u", " P=", $P::x // "u", " Po=", $P::o // "u", "\n";
PL
is(run_cl(<<'PL'), "main=u Q=6\nR=2 3 main=0\n", '#4421: ... a flattened bare block, and an `if` block in a sub');
{ package Q { $y = 6; } }
print "main=", $main::y // "u", " Q=", $Q::y // "u", "\n";
sub s2 { if (1) { package R { @z = (1, 2); $h{k} = 3 } } }
s2();
print "R=", scalar(@R::z), " ", $R::h{k} // "u", " main=", scalar(@main::z), "\n";
PL
is(run_cl(<<'PL'), "EV=7 3 main=u\nB=1 A=u\noo=main-o+ g2=2 mg2=u\n", '#4421: ... a string eval, a nested package block, an outer `our`');
eval 'package EV { $ez = 7; @ea = (1, 2, 3) } 1' or die $@;
print "EV=", $EV::ez // "u", " ", scalar(@EV::ea), " main=", $main::ez // "u", "\n";
package A { package B { $nb = 1 } }
print "B=", $B::nb // "u", " A=", $A::nb // "u", "\n";
our $oo = "main-o";
sub f { package P2 { $oo .= "+"; $g2 = 2 } }
f();
print "oo=$oo g2=", $P2::g2 // "u", " mg2=", $main::g2 // "u", "\n";
PL

# ── 13-14. #4440 + #4422: an `our` DECLARATOR whose statement's lowering does
#    not qualify it -- a `for our $x` loop head, `local our $x`, an `our` in an
#    expression -- in a sub after `package NAME;` is spelled in its declaring
#    package, as its uses already were.  The base aliased / localized main::.
is(run_cl(<<'PL'), "o=7\np=8\nq=9\ni=1\ni=2\nk=0 k=1 |2|u\n", '#4440: `for our` in a sub region');
sub f { package Z; for our $o (7) { print "o=$o\n" } } f();
sub g { package Y; for our $p (8) { print "p=", $p, "\n" } } g();
{ package X; for our $q (9) { print "q=$q\n" } }
sub u { package U; for our $i (1 .. 2) { main::inner() } } sub inner { print "i=", $U::i // "u", "\n" } u();
sub v { package V; for (our $k = 0; $k < 2; $k++) { print "k=$k " } print "|$V::k|", $main::k // "u", "\n" } v();
PL
is(run_cl(<<'PL'), "L|L|u\nL|L\nL|L\n4 5|4 5|0\n5|5|u\n1 1|1|u\n", '#4422: `local our` (scalar, array) and an expression `our` in a region');
sub r { package S; local our $lo = "L"; print "$lo|", $S::lo // "u", "|", $main::lo // "u", "\n" } r();
{ package S2; local our $lo2 = "L"; print "$lo2|", $S2::lo2 // "u", "\n" }
package S3; local our $lo3 = "L"; print "$lo3|", $S3::lo3 // "u", "\n";
package main;
sub lx2 { package X2; local our @la = (4, 5); print "@la|@X2::la|", scalar(@main::la), "\n" } lx2();
sub q4 { package S4; print our $e = 5, "|", $S4::e, "|", $main::e // "u", "\n" } q4();
sub w { package W; my $x = our $y = 1; print "$x $y|$W::y|", $main::y // "u", "\n" } w();
PL

# ── 15-16. #475: a FILE-level `our` alias read in a later section of another
#    package is the declaring package's variable.
is(run_cl(<<'PL'), "mA\n1\n1\n1|x|\n", '#475: a file-level `our` across a top-level package switch');
our $t = "m"; package A; $t .= "A"; package main; print "$t\n";
our @o = (1); package A; print scalar(@o), "\n"; package main; print scalar(@o), "\n";
{ our @o2; package A; push @o2, "x"; package main; print scalar(@o2), "|@o2|\n"; }
PL
is(run_cl(<<'PL'), "3|3|u\n1 2|1|u\n", '#475 control: a plain `our` statement in a sub region (its lowering qualifies it)');
sub r { package X; our $z = 3; print "$z|$X::z|", $main::z // "u", "\n" } r();
sub t { package T; our ($a1, @b1) = (1, 2); print "$a1 @b1|$T::a1|", $main::a1 // "u", "\n" } t();
PL

# ── 17-19. what the old pass got right and the port must keep.
is(run_cl(<<'PL'), "10\n7 7\nfoo sees 1\nfoo own 2\nmain 2\n", 'a named sub in a later section; a same-name `my` in a later section');
my $x = 10;
package Foo;
sub get { $x }
sub set { $x = $_[0] }
package main;
print Foo::get(), "\n";
Foo::set(7);
print "$x ", Foo::get(), "\n";
my $w = 1;
package Bar;
print "foo sees $w\n";
my $w = 2;
print "foo own $w\n";
package main;
print "main $w\n";
PL
is(run_cl(<<'PL'), "in block inner\nbar outer inner\nf(A,A) FILE\n42\n43\n10\n", 'a package block\'s own `my`; a signature param (#454); evals and closures across a section');
use feature 'signatures'; no warnings;
my $x = "outer";
package Foo { my $x = "inner"; sub fx { $x } print "in block $x\n"; }
package Bar;
print "bar $x ", Foo::fx(), "\n";
package main;
my $p = "FILE";
package Sig;
sub f ($p, $q = $p) { "f($p,$q)" }
print f("A"), " $p\n";
package main;
my $n = 42;
package Ev;
print eval('$n'), "\n";
my $c = '$n + 1';
print eval($c), "\n";
package main;
my $m = 5;
package Cl;
my $fn = sub { my $g = sub { $m * 2 }; $g->() };
print $fn->(), "\n";
PL
is(run_cl(<<'PL'), "inner\nouter\n", 'the flatten refusal still fires: a block `my` shadowing a file lexical used after it');
my $y = "outer";
{ package T; sub tf { "t" } package main; my $y = "inner"; print "$y\n"; }
print "$y\n";
PL

# ── 20-21. the capture PROMOTER's rewrite asks the same binder question: a
#    C-style `for (my $i ...)` in a sub beside a captured file `my $i` keeps its
#    own `$i` -- the old sibling walk could not see the loop-head declaration,
#    so the loop's condition and step were renamed to the file cell (#4546;
#    core Math::Complex::root clobbered the cached `i`).
is(run_cl(<<'PL'), "loop 0 loop 1 loop 2 cap=5\n", '#4546: a C-style for `my` in a sub is not the promoted file lexical');
my $i = 5;
sub cap { $i }
sub lp { my @o; for (my $i = 0; $i < 3; $i++) { push @o, "loop $i" } "@o" }
print lp(), " cap=", cap(), "\n";
PL
# (like, not is: Math::Complex installs $SIG{FPE}, which the runtime announces on stderr)
like(run_cl(<<'PL'), qr/^i=i n=4$/m, '#4546: Math::Complex::root leaves i alone');
use Math::Complex;
my @r = root(16, 4);
print "i=", i, " n=", scalar(@r), "\n";
PL

# ── 22. RULE 12: after a rename, a use of the binding the rewrite did not
#    reach DIES naming it (_span_check_rewritten) -- never a later section
#    silently reading another package's variable.  In-process, from a real
#    binder and the segments the pass would build.
{
    require Pl::VendorPPI;
    require Pl::Parser2;
    require Pl::Binder;
    my $src = "my \$x = 1;\npackage Foo;\nprint \$x;\n";
    my $doc = PPI::Document->new(\$src);
    my $parser = bless {}, 'Pl::Parser2';
    my $bd = Pl::Binder->new($doc, parser => $parser)->bind;
    $parser->{_binder} = $bd;
    my @st = $doc->schildren;
    my $segs = [ { pkg => 'main', stmts => [ $st[0] ] }, { pkg => 'Foo', stmts => [ $st[2] ] } ];
    my ($decl) = grep { $_->content eq '$x' } @{ $st[0]->find('PPI::Token::Symbol') };
    my $b = $bd->decl_binding($decl);
    local $parser->{_span_ranges} = [ map { $bd->ord_range($_->{stmts}[0], $_->{stmts}[-1]) } @$segs ];
    my $err = eval { $parser->_span_check_rewritten($segs, $bd, $b, 'x', 'x__file__0', 0, 0); 1 } ? '' : $@;
    like($err, qr/^PCL internal: the span rename of \$x left a use unrenamed at line 3: \$x/,
         'rule 12: a spanning use the rewrite missed dies naming it');
}

# ── 23-35 (s536e): file lexicals across package sections.  Rows 23-26
#    #4547 (an EXCEPTION-partition name -- `my $a` / `my $b` -- promoted under
#    its own name was the proclaimed special, and the exception rename then
#    moved the declaration off it: the later section read `main::$a`, empty);
#    27-29 #4541 (a file-level `state` cell is a global of the DECLARING
#    package -- read in another package it was another variable); 30-32 #4543
#    (a container inside a LIST `my` was never offered to the span renamer:
#    refused "spans a package boundary"); 33-35 #4545 (a default inside a
#    one-token signature, Token::Prototype #455, is code in the token's TEXT:
#    the span pass refused, the capture promoter refused, an `our` alias was
#    not requalified there).  Every row but 26 (a control) fails on the base
#    c8886b61; every expected text is perl 5.40.3's own output.
is(run_cl(<<'PL'), "4 a=y\ny\n", '#4547: a file `my $a` read in a later section (and by an eval there)');
my $a = "y";
package Q;
print "4 a=$a\n";
print eval('$a'), "\n";
PL
is(run_cl(<<'PL'), "f=AB A B\nAx f=AxB\nm Ax B\n", '#4547: `my $a` / `my $b` read and written by a later section and its sub, back in main');
my $a = "A"; my $b = "B";
package Q;
sub f { "f=$a$b" }
print f(), " $a $b\n";
$a .= "x"; print "$a ", f(), "\n";
package main;
print "m $a $b\n";
PL
is(run_cl(<<'PL'), "1 b=x\nx\n", '#4547: a bare block holding a block-form `package P { }`, a named sub reading `$b`');
{ my $b = "x"; package P { } print "1 b=$b\n"; sub h { $b } }
print h(), "\n";
PL
is(run_cl(<<'PL'), "1 2 3 | 7 7 7 | 7\n", '#4547: a later section\'s sort block reads the file `my $a`, not sort\'s (control: an exception-rename blocker kept the base right here)');
my $a = 7;
package Q;
my @seen;
my @s = sort { push @seen, $a; $Q::a <=> $Q::b } (3, 1, 2);
print "@s | @seen[0 .. 2] | $a\n";
PL
is(run_cl(<<'PL'), "st=1\ng=2\ng=3\nst=3\n", '#4541: a file `state` read and incremented by a later section\'s sub, twice');
use feature "state"; state $st = 1;
package Foo;
print "st=$st\n";
sub g { $st++; "g=$st" }
print g(), "\n"; print g(), "\n";
print "st=$st\n";
PL
is(run_cl(<<'PL'), "n=11\nn=12\nn=13\nin=5\nc=7\n", '#4541: a `state` read after an in-block `package` and from a `package NAME { }` block');
use feature "state";
for my $i (1 .. 3) { state $n = 10; $n++; package Foo; print "n=$n\n"; }
state $c = 5;
package Bar { sub inc { $c++ } print "in=$c\n"; }
Bar::inc(); Bar::inc();
print "c=$c\n";
PL
is(run_cl(<<'PL'), "r=bz bz\n123\n", '#4541: an in-block and a section package write it; a named sub\'s state across an in-sub `package` (control)');
use feature "state";
state $st;
{ package Bar; $st .= "b"; }
package Baz;
$st .= "z";
sub r { "r=$st" }
package main;
print Baz::r(), " $st\n";
sub mk { state $k = 0; package Q; return ++$k }
print mk(), mk(), mk(), "\n";
PL
is(run_cl(<<'PL'), "u=1\nv=2 3 4 2\n", '#4543: `my ($u, @v)` -- the list\'s container read and written in a later section');
my ($u, @v) = (1, 2, 3); print "u=$u\n";
package Foo;
push @v, 4; print "v=@v $#v\n";
PL
is(run_cl(<<'PL'), "a2,b3 r=1 p=7 q=1\n", '#4543: `my ($r, %s)` beside an `our ($p, @q)` list, the hash written in a later section');
our ($p, @q) = (7, 8, 9);
my ($r, %s) = (1, a => 2);
package Foo;
$s{b} = 3; @q = (1);
print join(",", map {"$_$s{$_}"} sort keys %s), " r=$r p=$p q=@q\n";
PL
is(run_cl(<<'PL'), "3 1 5 | 2\n2 6 3\n", '#4543: `my (@a, @b)` read by a later section\'s sub; a loop pushes, a slice reads');
my (@a, @b);
@a = (1); @b = (2);
package Foo;
sub f { push @a, 5; scalar(@a) + scalar(@b) }
print f(), " @a | @b\n";
my ($n, @w) = (0);
for (1 .. 3) { push @w, $_ * 2 }
print "@w[0,2] ", scalar(@w), "\n";
PL
is(run_cl(<<'PL'), "f=FILE 2 <FILE> \$p\nf=x 9 s t\n", '#4545: one-token signature defaults read file lexicals across a section (a single-quoted text is not a use)');
my $p = "FILE"; my @v = (1, 2);
package Foo;
use feature 'signatures'; no warnings; sub f ($q = $p, $n = scalar(@v), $s = "<$p>", $t = '$p') { "f=$q $n $s $t" }
print f(), "\n"; print f("x", 9, "s", "t"), "\n";
PL
is(run_cl(<<'PL'), "f=FILE FILE|f=A A|f=A B\nf=K\n", '#4545: a parameter named like the lexical -- its own default reads the file lexical, a later one the parameter; a hash element default');
my $p = "FILE"; my %h = (k => "K");
package Foo;
use feature 'signatures'; no warnings; sub f ($p = $p, $q = $p) { "f=$p $q" }
print f(), "|", f("A"), "|", f("A", "B"), "\n";
use feature 'signatures'; no warnings; sub g ($q = $h{k}) { "f=$q" }
print g(), "\n";
PL
is(run_cl(<<'PL'), "f=FILE\nf=m\n", '#4545: the capture promoter\'s mangle and the `our` requalifier rewrite a one-token default too');
my $p = "FILE"; { my $p = "inner"; }
use feature 'signatures'; no warnings; sub f ($q = $p) { "f=$q" }
print f(), "\n";
our $t = "m";
package A;
use feature 'signatures'; no warnings; sub f ($x = $t) { "f=$x" }
print f(), "\n";
PL
