#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# punct-glob-name-01.t — task #463 items 3-5: a glob whose NAME is punctuation,
# a digit run, or hidden behind perl's `;` block disambiguator.
#
# perl names a glob with any punctuation character or digit run, exactly as it
# names a scalar, and real code writes them:
#
#     *X = *-;             t/re/reg_namedcapture.t:18  — %X becomes %-
#     local *a = *1;       t/re/subst.t:951            — $a becomes $1
#     local *1 = sub {…};  t/op/method.t:38            — the numeric glob as an LVALUE
#     *{;undef} = 3;       t/op/gv.t:1020              — asserts the DIE message
#
# PPI's Symbol regex is word-bounded, so it hands `*-` over as two ordinary
# operator tokens (docs/ppi-upstream-bugs.md §26) and every one of these was
# DROPPED — "Got op '-', not postfix.  But there is nothing after it??".
# Pl::Parser2::_repair_punct_glob_name rewrites the name into the symbolic
# spelling `*{'-'}` the compiler already lowers; the `;` family needed no lexer
# fix at all, only Pl::Parser2::_normalize_null_statements, because
# PPI::Statement::Null is INSIGNIFICANT and the walker was the only thing that
# could see it.
#
# THE NEGATIVES ARE THE POINT.  The repair's condition is a WHITELIST of the
# positions where a `*` can OPEN a glob name (statement/list start, after `=`,
# `,` or `return`), not `_ends_term`'s negative, because a false positive here
# turns working multiplication into a glob and kills the file, while a miss
# costs only today's drop.  The measured
# term-position `*` sites in the four populations that must NOT be touched are
# all here: multiplication after a deref block (`${$r}{k}*2`, where `_ends_term`
# itself says the term has not ended), after a subscript, after a call, and a
# glob PATTERN.  Every expectation is the live `perl` answer.

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

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 26;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the bug ---------------------------------------------------------------

both_agree(<<'PL', 'a PUNCTUATION glob name: *X = *- (t/re/reg_namedcapture.t:18)');
no strict 'refs';
our %X; *X = *-;
'X' =~ /(?<X>X)/;
print "aliased=", (defined $X{X} ? "yes" : "no"), "\n";
PL

both_agree(<<'PL', 'a DIGIT glob name: local *a = *1 (t/re/subst.t:951)');
no strict 'refs';
"hello" =~ /(ell)/;
our $a;
{ local *a = *1; print "a=[$a]\n"; }
PL

both_agree(<<'PL', "perl's `;` block disambiguator, both sigils (t/op/gv.t:1020)");
no strict 'refs';
our $a1 = "A"; our @z = (7,8);
my $x = ${;"a1"};  print "1=[$x]\n";
my @y = @{;"z"};   print "2=[@y]\n";
# The same Null statement in a BARE block — t/op/sub_lval.t:1066, which is
# where the census counted this one.
sub bare { {; @z } }
my @b = bare(); print "3=[@b]\n";
PL

# The `;` does more than force the BLOCK reading: inside one a lone bareword is
# an EXPRESSION where a plain deref reads it as the NAME.  Deleting the `;` and
# stopping there emitted the glob literally named "undef".
both_agree(<<'PL', 'a bareword inside a FORCED block is a call, not a name');
no strict 'refs';
sub foo { "bar" }
our $bar = 5; our $foo = 9;
print "A=[", ${foo}, "]\n";
print "B=[", ${;foo}, "]\n";
print "C=[", ${ foo() }, "]\n";
print "D=[", *{;foo}, "]\n";
print "E=[", *{foo}, "]\n";
PL

# The die message is compared without its " at FILE line N." tail: PCL's
# location for a runtime die is its own (`(eval 0) line 0`), a general
# divergence and not what this row is about.
{
    my $code = <<'PL';
no strict 'refs';
my $ok = eval { *{;undef} = 3; 1 };
my $e = $ok ? "no-die" : $@;
$e =~ s/ at .*//s;
print "err=[$e]\n";
PL
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "*{;undef} = 3 dies as a symbol reference (perl: "
                    . ($perl =~ s/\n/\\n/gr) . ")");
}

# A digit glob as an ASSIGNMENT TARGET, in the spelling the repair covers:
# at statement start.
both_agree(<<'PL', 'a DIGIT glob as an assignment TARGET installs the sub');
no strict 'refs';
my $one = "1";
*1 = sub { 123 };
print "res=[", &$one(), "]\n";
PL

# ---- #564: `local` joins the whitelist, because the target now LOWERS -------
#
# `local *1 = sub {…}` (t/op/method.t:38) repairs into `local *{'1'} = sub {…}`,
# and until #564 that Cast + Block shape fell through _process_local_declaration
# and the statement VANISHED — no announcement, no `;; PARSE ERROR`, no census
# row.  So the repair was held back rather than trade a loud drop for a silent
# one.  p-local-glob-dynamic is the lowering; `local` is in the whitelist; these
# rows are both halves.

both_agree(<<'PL', 'the #564 shape: local *{EXPR} = sub {…} installs and RESTORES');
no strict 'refs';
my $one = "1";
{ local *{"1"} = sub { 123 }; print "in=[", &$one(), "]\n"; }
print "after=[", (defined &{"1"} ? "def" : "undef"), "]\n";
PL

both_agree(<<'PL', 'the repaired spelling: local *1 = sub {…} (t/op/method.t:38)');
no strict 'refs';
my $one = "1";
{ local *1 = sub { 123 }; print "in=[", &$one(), "]\n"; }
print "after=[", (defined &{"1"} ? "def" : "undef"), "]\n";
PL

# A SIGIL character names a glob too, and PPI hands BOTH tokens over as CASTS
# there — the third spelling of §26.  perl-tests/local.t:828 is this line.
both_agree(<<'PL', 'a SIGIL glob name: local *@ localizes $@ (perl-tests/local.t:828)');
eval { die "boom\n" };
print "err=[$@]";
{ local *@; print " in=[$@]"; eval { 1 }; }
print " out=[$@]\n";
PL

both_agree(<<'PL', 'a SIGIL glob name as an assignment TARGET: *@ = *x');
no strict 'refs';
our $x = "XV";
*@ = *x;
print "at=[$@]\n";
PL

# The name is an EXPRESSION, evaluated exactly once and in the package in
# effect — the same resolver the assignment path uses.
both_agree(<<'PL', 'the name is an expression, evaluated ONCE');
no strict 'refs';
my $c = 0;
sub nm { $c++; "zz" }
{ local *{nm()} = sub { 9 }; print "in=[", &{"zz"}(), "][c=$c]\n"; }
print "after=[", (defined &{"zz"} ? "def" : "undef"), "][c=$c]\n";
PL

both_agree(<<'PL', 'a QUALIFIED run-time name, and the other three slots');
no strict 'refs';
our @zz = (1,2); our %zz = (a=>1); our $zz = "S";
my $n = "zz"; my @new = (7,8);
{ local *{$n} = \@new; print "arr-in=[@zz]\n"; }
print "arr-out=[@zz]\n";
{ local *{"main::zz"}; print "cleared=[@zz][", (defined $zz ? $zz : "undef"), "]\n"; }
print "restored=[@zz][$zz]\n";
PL

# The deprecated conditional idiom, in the run-time-named spelling: perl does
# not localize at all when the condition is false.
both_agree(<<'PL', 'local *{EXPR} = RHS if COND, both ways');
no strict 'refs';
our $q = "Q"; my $n = "q";
for my $c (1, 0) {
  { local *{$n} = \"Z" if $c; print "c=$c in=[$q]\n"; }
  print "c=$c out=[$q]\n";
}
PL

# `local *$x` where $x holds a GLOB REF, the shape Moo's _install_coderef
# writes — and the shape op/gv.t:918 ([perl #77926]) reaches through a tie.
# It used to bind the SCALAR $g: a silent wrong, not a drop.
both_agree(<<'PL', 'local *$x through a GLOB REF localizes the glob, not the scalar');
no strict 'refs';
our $vv = "V";
my $g = \*vv;
{ local *{$g} = \"NEW"; print "in=[$vv]\n"; }
print "out=[$vv]\n";
PL

# ---- #564 negatives: the deref casts the `*` arm must not claim ------------

both_agree(<<'PL', 'negative: ordinary @/%/& derefs are not glob names');
my $r = [1,2]; print "1=[@$r]\n";
my $h = {a=>1}; print "2=[", join(",", %$h), "]\n";
my @c = @{$r}; print "3=[@c]\n";
sub cf { 42 } my $cr = \&cf; print "4=[", &$cr(), "]\n";
my $n = 3; print "5=[", $n*2, "]\n";
PL

# ---- the negatives ---------------------------------------------------------

both_agree(<<'PL', 'negative: every multiplication the whitelist must not claim');
my $n = 3; print "1=[", $n*1, "]\n";
my @a = (1,2,3); my %h = (k=>4);
print "2=[", $h{k}*2, "]\n";
print "3=[", scalar(@a)*2, "]\n";
print "4=[", (2)*-1, "]\n";
my $r = {k=>5}; print "5=[", ${$r}{k}*2, "]\n";
print "6=[", 2*!0, "]\n";
sub sz { 7 } print "7=[", sz()*3, "]\n";
my $hr = {n=>6}; print "8=[", @{[1,2]}*3, "]\n";
PL

both_agree(<<'PL', 'negative: a glob PATTERN and the ordinary glob spellings');
no strict 'refs';
my @g = sort glob("./nope-*-xyz");
print "1=[", scalar(@g), "]\n";
our $g2 = "GEE"; our $p2; *p2 = *g2; print "2=[$p2]\n";
*{"n2"} = \$g2; print "3=[", ${"n2"}, "]\n";
my @f = sort <*.no-such-suffix>;
print "4=[", scalar(@f), "]\n";
PL

# ---- task #562: the two spellings the s446k repair deliberately left alone --
#
# `*^R` (t/re/pat.t:1715) is NOT the glob named `^`.  perl's caret convention
# means the glob named chr(18) — the one `$^R` reads — so `*{'^'}R` would have
# been a silent wrong, and the repair spells the CONTROL CHARACTER instead.
# PCL spells the same variable `$^R` (a pipe-quoted CL symbol, task #412), and
# the two spellings meet in the runtime at %p-slot-name.  Probed: `${"\cR"}` IS
# `$^R` in perl, while `${"^R"}` is a different variable, and `"" . *^R` is
# `*main::` + chr(18).

both_agree(<<'PL', 'a CARET glob name: *^R aliases the caret variable (t/re/pat.t:1715)');
no strict 'refs';
no warnings 'once';
our $caretsrc = "SCALARVAL"; our @caretsrc = (1,2,3);
$^R = "before";
print "1=[$^R]\n";
*^R = *caretsrc;
print "2=[$^R]\n";
$^R = "written-through";
print "3=[$caretsrc]\n";
print "4=[@{\"\cR\"}]\n";
PL

# The control-character name is the ONE name, whichever way it is written, and
# it is forced to main from inside a package exactly as perl forces it.
both_agree(<<'PL', 'a caret glob is the same variable as its control-character name');
no strict 'refs';
no warnings 'once';
our $csrc = "FROMPKG";
package Foo;
*^R = *main::csrc;
package main;
print "1=[$^R]\n";
print "2=[", ${"\cR"}, "]\n";
$^R = "back";
print "3=[$csrc]\n";
PL

# `*]` (t/op/tie_fetch_count.t:189).  PPI hands `]` over as a Token::Structure,
# so it needs its own arm — and that arm must not claim a bracket that is
# CLOSING something.  The negatives below are the shapes it must leave alone.
both_agree(<<'PL', 'a CLOSING-BRACKET glob name: *] is the glob holding $]');
package main;
sub TIESCALAR { my $p = shift; my $v = shift; bless \$v, $p }
sub FETCH { my $s = shift; return $$s }
no strict 'refs';
{
    tie my $var4 => 'main', *];
    my $g = $var4;
    print "1=[$g]\n";
}
my $bkt = *];
print "2=[$bkt]\n";
print "3=", ((${*{']'}{SCALAR}} == $]) ? 1 : 0), "\n";
PL

both_agree(<<'PL', 'negative: every closing bracket that IS closing something');
my @arr = (10,20,30);
print "1=[", $arr[1], "] 2=[", join(",", @arr[0,2]), "]\n";
my $ref = [1,2];
print "3=[", scalar(@$ref), "] 4=[", (sort { $a <=> $b } 5,3)[0], "]\n";
my %h = (k => 7);
print "5=[", $h{k}, "] 6=[", $h{k}*2, "]\n";
sub proto (*) { return "proto:$_[0]" }
print "7=[", proto("z"), "]\n";
my $hr = {n=>6}; print "8=[", ${$hr}{n}*3, "]\n";
PL

# ---- #602: `*A = *B` REPLACES A's glob, it does not merge into it ----------
#
# perl's glob-to-glob assignment makes A another name for B's entry, so a slot
# B does NOT have is a slot A no longer has.  PCL copied slot by slot behind a
# `boundp` guard, so an unset SOURCE slot left the destination's old value in
# place — `our $x = 5; *x = *neverdefined` printed 5 where perl prints nothing.
# p-glob-copy is now CLEAR-THEN-COPY.  The negatives matter as much: the
# reference forms (`*dst = \&sub`, `*dst = \$scalar`) go through
# %p-glob-assign-slots' typed arms, touch ONE slot, and must keep every other
# slot — that is the import path, and a clear firing there would empty a live
# variable.  Every expectation is the live `perl` answer.

both_agree(<<'PL', '#602: a glob-to-glob assign EMPTIES the slots the source lacks');
our $x = 5; our @x = (9); our %x = (k => 1); sub x { 42 }
*x = *neverdefinedglob;
print "1 s=[$x] a=[@x] def=", (defined $x ? 1 : 0),
      " h=", scalar(keys %x), " code=", (defined &x ? 1 : 0), "\n";
PL

both_agree(<<'PL', '#602: source has SOME slots, destination has others');
our $a2 = 'as'; our @a2 = ('aa'); our %a2 = (ak => 1);
our @b2 = ('ba');
*a2 = *b2;
print "1 s=[", (defined $a2 ? $a2 : 'U'), "] a=[@a2] h=", scalar(keys %a2), "\n";
PL

both_agree(<<'PL', '#602: the clear never CREATES a variable perl would not');
no strict 'refs';
*c3 = *neverdefinedglob2;
print "1 defined-c3=", (defined $c3 ? 1 : 0), "\n";
PL

both_agree(<<'PL', '#602 negative: the REFERENCE forms touch one slot (the import path)');
sub srcsub { "S" }
our $d4 = 'keepme'; our @d4 = ('keeparr');
*d4 = \&srcsub;
print "1 code=", d4(), " s=[", (defined $d4 ? $d4 : 'U'), "] a=[@d4]\n";
our $f6 = 'old'; our @f6 = ('oldarr'); my $newsc = 'new';
*f6 = \$newsc;
print "2 s=[$f6] a=[@f6]\n";
sub e5src { "E" }
our $e5 = 'gone5'; our @e5 = ('gonearr5');
*e5 = *e5src;
print "3 code=", e5(), " s=[", (defined $e5 ? $e5 : 'U'), "] a=[@e5]\n";
PL

both_agree(<<'PL', '#602: t/re/pat.t:1715 — *^R = *<glob with no scalar> makes $^R undef');
no warnings 'once';
$^R = 'oldR';
our @caretRglobwithnoscalar = (1,2);
*^R = *caretRglobwithnoscalar;
print "1 caretR=[", (defined $^R ? $^R : 'UNDEF'), "]\n";
PL
