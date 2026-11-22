#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Transpile tests part 11 -- s543c, the verification batch (2026-11-21).
# Every row is the OWN REPRODUCER of a task the batch closed because it now
# agrees with perl, and that had no Pl/t row of its own: the fix came from an
# earlier change (named in each task's s543c block), so these rows are the
# guard that keeps it.  Rows are grouped by area, one program (= one SBCL
# launch) per group, and every row compares PCL against perl run on the same
# file at test time (no 5.40-only syntax: CI's perl is 5.38).
# NEW TESTS GO HERE -- the next transpile-test file after -10.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 14;

my $DIR = tempdir(CLEANUP => 1);

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(DIR => $DIR, SUFFIX => '.pl');
    binmode $fh;
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run,
# from $DIR so the programs' relative files land there.
sub run_cl {
    my ($pl_file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(DIR => $DIR, SUFFIX => '.lisp');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `cd $DIR && sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $pl_file = write_pl($code);
    my $perl = `cd $DIR && perl $pl_file 2>&1`;
    $perl =~ s/^\s*\n//gm;
    my $pcl = run_cl($pl_file);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- handles: #1425 print {$$fh}; #2780 a handle `my` embedded in an
#      expression survives a package block --------------------------------------
both_agree(<<'PL', '#1425 print {$$fh} / printf {$$fh} write; #2780 open my $fa survives { package O; }');
my $F = "t11-1425.out";
open(my $fh, ">", $F) or die; my $r = print {$$fh} "x\n"; close $fh;
open my $in, '<', $F or die; my $c = <$in>; close $in; unlink $F;
print "1 r=", (defined $r ? $r : "undef"), " file=[", ($c // ''), "]";
open(my $g, ">", $F) or die; my $r2 = printf {$$g} "%s\n", "y"; close $g;
open $in, '<', $F or die; $c = <$in>; close $in; unlink $F;
print "2 r=", ($r2 // 'undef'), " file=[", ($c // ''), "]";
open my $fa, "<", $0 or die;
{ package O; }
my $cc = 1;
print "3 [", (ref(\$fa) eq 'REF' || ref($fa) eq 'GLOB' ? "handle" : "lost"), "] ", (defined(scalar <$fa>) ? "reads" : "empty"), "\n";
PL

# ---- tie and local: #1483 a tied $_ fires FETCH / STORE for chop, a bare
#      filetest and a plain assignment; #2112 a `local` embedded in an
#      expression statement is restored at the end of its scope ----------------
both_agree(<<'PL', '#1483 tied $_ under chop / -x / assignment; #2112 an embedded local is restored');
package UnderScore;
sub TIESCALAR { bless \my $self, shift }
sub FETCH { die "read  \$_ forbidden\n" }
sub STORE { die "write \$_ forbidden\n" }
package main;
tie $_, 'UnderScore';
for my $t (['Reading', sub { print }], ['Matching', sub { my $x = /badness/ }], ['Concat', sub { $_ .= "a" }],
           ['Chop', sub { chop }], ['Filetest', sub { -x }], ['Assignment', sub { $_ = "Bad" }]) {
  eval { $t->[1]->() };
  printf "%-12s died=%d\n", $t->[0], ($@ ? 1 : 0);
}
untie $_;
our $r = "o"; sub rr { $r } my @s; (local $r = $_), push(@s, rr()) for 1..2; print "@s $r\n";
our $t = "o"; sub tt { $t } my @u; for (1..2) { (local $t = $_), push(@u, tt()); } print "@u $t\n";
PL

# ---- name resolution: #2195 an embedded `my` in a sub beside a later file
#      `my`; #2781 a file `my` masks an earlier `our` inside a named sub;
#      #3490 an embedded `my` is not visible later in its own statement;
#      #3610 "${e}[1]" in a sub is the scalar $e ---------------------------------
both_agree(<<'PL', '#2195 embedded my in a sub; #2781 our/my masking; #3490 own-statement my; #3610 "${e}[1]"');
sub op { open(my $h, "<", $0) or die; $h }
my $h = 5; print "1 ok $h\n";
sub cond { if ((my $g = 1) > 0) { return $g } } my $g = 7; print "2 g=$g ", cond(), "\n";
sub id { $_[0] } sub argm { id(my $k = 2); $k } my $k = 9; print "3 k=$k ", argm(), "\n";
our $x = "G";
my $x = "X";
sub f2 { "$x-" . $x }
print "4 ", f2(), "\n";
sub X::n { shift; $_ = "y" for @_; 1 }
for my $kk (1..2) { print "5 f ", X->n(my $t), " $kk ", ($t // ''), "\n" }
my $xx = 1; for (1..2) { my $l = (my $xx = $xx + 1); print "6 $l $xx\n" }
my $e = 1;
sub rf { return "${e}[1]" }
sub rh { return "${e}{k}" }
print "7 ", rf(), " ", rh(), " ${e}[1]\n";
PL

# ---- eval, construction, references: #1943 a string eval's last expression
#      takes the eval's context; #2540 `[ { g(@l) } ]` is ONE anon hash;
#      #3827 a ref-to-ref chain `$$$a` unwraps to the right level (the task
#      itself stays OPEN, narrowed to Data::Dumper's Purity rows, which still
#      differ; this row guards the half that agrees) -------------------------
both_agree(<<'PL', '#1943 scalar eval "%h{...}"; #2540 [ { g(@l) } ]; #3827 $$$a of a ref chain');
my %h = map { $_ => uc $_ } 'a'..'z';
print "1 ", scalar(eval "%h{'c','d','e'}"), " ", scalar(eval "\@h{'c','d','e'}"), "\n";
my $s = eval "(4,5,6)"; my @l = eval "(4,5,6)"; print "2 $s ", scalar(@l), "\n";
sub g { map { $_ => 1 } @_ }   my @gl = qw(a b);
print "3 ", scalar(@{ [ { g(@gl) } ] }), " ", ref([ { g(@gl) } ]->[0]), "\n";
use Scalar::Util qw(refaddr);
my $a = \\\\\'foo'; my $b = $$$a;
my @lv = ($a, $$a, $$$a, $$$$a);
print "4 ", join(" ", map { refaddr($lv[$_]) == refaddr($b) ? "L$_=b" : "L$_" } 0..3), "\n";
PL

# ---- #4302 a CR inside a string literal is a character of the program
#      (PPI's tokenizer used to rewrite CR LF / CR to LF): string-eval text
#      and a raw-CR source file ----------------------------------------------------
both_agree(<<'PL', '#4302 CR LF / CR inside quotes survive a string eval');
my $s = eval "'a\r\nb'";   my $t = eval "\"a\r\nb\""; my $v = eval "q{a\r\r\nb}"; my $w = eval "'c\rd'";
print join(" ", length($s), length($t), length($v), length($w), ($w =~ /\r/ ? "cr" : "nocr")), "\n";
PL
both_agree("my \$x = q{a\r\nb}; my \$y = \"c\rd\";\nprint length(\$x), \" \", length(\$y), \" \", (\$y =~ /\\r/ ? \"cr\" : \"nocr\"), \"\\n\";\n"
         . "my \@d = <DATA>; print scalar(\@d), \" \", (\$d[0] =~ /\\r\\n\\z/ ? \"crlf\" : \"lf\"), \"\\n\";\n__DATA__\nd1\r\nd2\n",
           '#4302 a raw CR in the SOURCE: q{} / "" literals and __DATA__ keep it');

# ---- modules: #1617 core File::Copy loads and copies; #2012 File::Temp's
#      tempdir(CLEANUP => 1) without a template ----------------------------------
both_agree(<<'PL', '#1617 core File::Copy: copy / move / cp');
require File::Copy;
print "loaded\n";
open my $o, '>', "t11-1617.src" or die; print $o "data\n"; close $o;
print "copy=", (File::Copy::copy("t11-1617.src", "t11-1617.dst") ? 1 : 0), "\n";
open my $i, '<', "t11-1617.dst" or die; print "dst=", scalar <$i>; close $i;
print "move=", (File::Copy::move("t11-1617.dst", "t11-1617.mv") ? 1 : 0), " exists=", (-e "t11-1617.mv" ? 1 : 0), "\n";
print "cp=", (File::Copy::cp("t11-1617.src", "t11-1617.cp") ? 1 : 0), "\n";
unlink "t11-1617.src", "t11-1617.mv", "t11-1617.cp";
PL
both_agree(<<'PL', '#2012 File::Temp tempdir(CLEANUP => 1) with no template');
use File::Temp qw(tempdir tempfile);
my $d = tempdir(CLEANUP => 1);
print "dir=", (-d $d ? "ok" : "FAIL"), " abs=", ($d =~ m{^/} ? 1 : 0), "\n";
my ($fh, $fn) = tempfile(DIR => $d); print "file=", (-f $fn ? "ok" : "FAIL"), "\n";
PL

# ==== s543e (2026-11-21): seven live silent wrongs, one guard group each ====

# ---- #1030 the %ENV value layer: an undef store keeps the element UNDEF
#      (defined 0, exists 1) while the child sees an empty variable; the
#      element CELL makes undef / s/// / chop / ++ / substr / \$ENV{K} /
#      foreach / @_ aliasing write the environment; a reference or a number
#      is stringified on store; #5105 grep over %ENV sees its pairs ----------
both_agree(<<'PL', '#1030 $ENV{K} = undef stays undef; lvalue uses of $ENV{K} write; #5105 grep %ENV');
no warnings;
sub st { my ($k, $t) = @_; my $v = $ENV{$k}; my $c = `printenv $k; echo "rc=\$?"`; $c =~ s/\n/|/g;
  print "$t d=", (defined $v ? 1 : 0), " e=", (exists $ENV{$k} ? 1 : 0), " v=[", ($v // 'U'), "] c=[$c]\n" }
$ENV{T11A} = undef; st('T11A', 1);
$ENV{T11B} = "x"; undef $ENV{T11B}; st('T11B', 2);
$ENV{T11C} = "o"; { local $ENV{T11C}; st('T11C', '3in') } st('T11C', '3out');
{ local $ENV{T11D} = undef; st('T11D', '4in') } st('T11D', '4out');
$ENV{T11E} = undef; delete $ENV{T11E}; st('T11E', 5);
$ENV{T11F} = 0; st('T11F', '6a'); $ENV{T11F} = ""; st('T11F', '6b'); $ENV{T11F} = 3.5; st('T11F', '6c');
my $ar = [1]; $ENV{T11G} = $ar; my $r = ($ENV{T11G} = $ar);
print "6d ref=[", ref($ENV{T11G}), "] same=", ($ENV{T11G} eq "$ar" ? 1 : 0), " aval-ref=[", ref($r), "]\n";
$ENV{T11H} = undef; my %c = %ENV; print "7 copy e=", (exists $c{T11H} ? 1 : 0), " d=", (defined $c{T11H} ? 1 : 0), "\n";
@ENV{qw(T11I T11J)} = (undef, 1); st('T11I', '8a'); st('T11J', '8b');
my $nu = grep { !defined } values %ENV; my $ne = 0; while (my ($k, $v) = each %ENV) { $ne++ if !defined $v }
print "9 undef values=$nu each=$ne\n";
my @k1 = keys %ENV; $ENV{T11J} = undef; my @k2 = keys %ENV; print "10 order kept=", ("@k1" eq "@k2" ? 1 : 0), "\n";
%ENV = (%ENV, T11K => undef); st('T11K', 11);
$ENV{T11L} = "abc"; $ENV{T11L} =~ s/b/B/; chop $ENV{T11L}; st('T11L', 12);
$ENV{T11M} = 5; $ENV{T11M}++; $ENV{T11M} += 2; st('T11M', 13);
$ENV{T11N} = "hello"; substr($ENV{T11N}, 0, 1) = "J"; st('T11N', 14);
my $q = \$ENV{T11O}; $$q = "viaref"; st('T11O', 15);
for my $e ($ENV{T11P}) { $e = "alias" } st('T11P', 16);
sub w { $_[0] = "arg" } w($ENV{T11Q}); st('T11Q', 17);
for (@ENV{qw(T11R)}) { $_ = "slice" } st('T11R', 18);
$ENV{T11S} = undef; $ENV{T11S} //= "dflt"; st('T11S', 19);
print "20 grep=", scalar(grep { $_ eq 'T11S' } %ENV), "\n";
undef %ENV; print "21 undef %ENV keys=", scalar(keys %ENV), " T11S=[", ($ENV{T11S} // 'U'), "]\n";
PL

# ---- #1039 an @array / %hash VALUE reaching a STRING operator is its COUNT
#      (string context is scalar context: %p-str-operand) -- a reference, a
#      blessed one, an overloaded one, an element holding one, "@a", join and
#      print @a are untouched ------------------------------------------------------
both_agree(<<'PL', '#1039 "count: " . @a is 3; lc / length / eq / x / .= of an array; refs still ARRAY(0x...)');
no warnings;
my @a = (7,8,9); our @p = (4,5); my $r = \@a; my %h = (a => 1, b => 2, c => 3); my $hr = {x => 1};
sub isref { $_[0] =~ /^(?:\w+=)?(ARRAY|HASH)\(0x[0-9a-f]+\)$/ ? "R$1" : "[$_[0]]" }
sub f { @a } sub g { return @a } sub h { my $x = 1; @a } sub t { $_[0] ? @a : @p } sub d { do { @a } }
print "1 ", @a . "", " ", "count: " . @a, " ", "n=" . @p, " ", "d=" . @$r, " ", "x=" . @{[1,2,3,4]}, " ", "r=" . $r->@*, "\n";
print "2 ", f() . "", " ", g() . "", " ", h() . "", " ", t(1) . "", " ", t(0) . "", " ", d() . "", "\n";
print "3 ", lc(f()), " ", length(f()), " ", (f() eq '3' ? "EQ" : "NE"), " ", length(@a), " ", uc @p, " ", quotemeta(@a), " ", ord(@a), " ", index(@a, "3"), " ", substr(@a, 0), "\n";
print "4 ", (@a eq '3' ? "EQ" : "NE"), " ", (@a lt '4' ? "LT" : "GE"), " ", (@a cmp '3'), " ", ('3' cmp @a), " ", (@a . @p), "\n";
my $s = "s"; $s .= @a; my @e = ("x"); $e[0] .= @a; my %k = (k => "y"); $k{k} .= @a; my $er = ["z"]; $er->[0] .= @a;
my $buf = ""; for (1..3) { $buf .= @a; $buf .= "-" } print "5 $s $e[0] $k{k} $er->[0] $buf ", @a x 2, " ", "-" x @a, "\n";
print "6 ", %h . "", " ", "h=" . %h, " ", "k=" . keys(%h), " ", "g=" . grep({ $_ > 7 } @a), " ", "m=" . map({ $_ } @a), " ", "x" . (1 ? @a : 0), "\n";
my @emp; print "7 [", @emp . "", "] ", (@a == 3 ? "num" : "bad"), " ", @a + 0, "\n";
my $o = bless [5,6], 'Obj'; { package OvS; use overload '""' => sub { "S" . scalar(@{$_[0]}) }, fallback => 1; } my $ov = bless [1,2,3], 'OvS';
my @aa = ([1,2], [3]); { package TA; sub TIEARRAY { bless [], shift } sub FETCHSIZE { 5 } sub FETCH { 1 } } tie my @t, 'TA';
print "8 ", isref("$r"), " ", isref($r . ""), " ", isref($r x 1), " ", isref($hr . ""), " ", isref($o . ""), " ", $ov . "", " ", lc($ov), " ", isref($aa[0] . ""), " ", @t . "", "\n";
print "9 ", @a, " ", "@a", " ", "$#a", " ", join("-", @a), " ", ($r eq $r ? "EQ" : "NE"), "\n";
PL

# ---- #3100 a ONE-index list slice of an EMPTY list is the empty list in list
#      context (one value for a non-empty one, undef past its end); #2766 a
#      list slice as a sub's tail / `return` value is its LAST element in
#      scalar context, never its count (one lowering: the slice context rule) --
both_agree(<<'PL', '#3100 my @x = (stat $missing)[7] is empty; #2766 sub g { (@_)[0,1] } in scalar context is 8');
no warnings;
my @e; my $f = "/nonexistent/t11-3100"; my @a = (5,6,7); sub empty { () } sub three { (1,2,3) } sub n { scalar @_ }
my @x = (@e)[-1]; my @y = (@e)[0]; my @z = ()[0]; my @w = (empty())[0]; my @v = (stat $f)[7];
my ($size) = (stat $f)[7]; my @o = (1)[1]; my @o2 = (1,2)[5]; my @m = (@e)[0,1];
print "1 ", join(" ", map { scalar @$_ } \@x, \@y, \@z, \@w, \@v, \@o, \@o2, \@m), " ", (defined $size ? "def" : "undef"), " ", ((stat $f)[7] ? "T" : "F"), "\n";
my %h = (a => (@e)[0], b => 2); my @p; push @p, (@e)[0]; push @p, (@a)[1]; push @p, (@a)[0, 2]; my @l = ((@e)[0], 1, (@a)[0]);
my $cnt = () = (@e)[0]; my $c = scalar((@e)[0]);
print "2 ", join(",", map { "$_=" . ($h{$_} // 'U') } sort keys %h), " [@p] [@l] $cnt ", (defined $c ? "def" : "undef"), " ", n((empty())[0]), " [", (@e)[0], "]\n";
sub r { return (@e)[0] } sub t { (@e)[0] } my @rl = r(); my $rs = r(); my @tl = t(); my $tsv = t();
for ((@a)[1]) { $_ .= "!" } my @mm = map { (@e)[0] } 1..3; my @tr = (three())[1]; my @sl = (@a)[1..2];
{ package TA; sub TIEARRAY { bless [], shift } sub FETCHSIZE { 0 } sub FETCH { "f" } } tie my @ta, 'TA'; my @tt = (@ta)[0];
print "3 ", scalar(@rl), " ", (defined $rs ? "def" : "undef"), " ", scalar(@tl), " ", (defined $tsv ? "def" : "undef"), " [@a] ", scalar(@mm), " [@tr] [@sl] ", scalar(@tt), "\n";
sub g  { (@_)[0, 1] }            my $s1 = g(7, 8, 9);  my @l1 = g(7, 8, 9);
sub h2 { return (@_)[1, 2] }     my $s2 = h2(7, 8, 9);
sub st { return (localtime(0))[5, 4] } my $s3 = st(); my @l3 = st();
sub f3 { (7, 8, 9) } sub fs { return (f3())[1, 0] } my $s7 = fs();
sub d1 { do { (@_)[0, 1] } } my $s8 = d1(4, 5, 6); sub i1 { if ($_[0]) { (@_)[1, 2] } else { () } } my $s9 = i1(1, 5, 6);
sub tn { return $_[0] ? (@_)[0, 1] : () } my $s10 = tn(3, 4, 5); sub ml { my $x = shift; (@_)[0, 1] } my $s11 = ml(0, 4, 5);
sub ar { my @q = @_; return @q[0, 1] } my $s5 = ar(7, 8, 9); my %hh = (a => 1, b => 2); sub hs { return @hh{qw(a b)} } my $s6 = hs();
print "4 $s1 [@l1] $s2 $s3 [@l3] $s7 $s8 $s9 $s10 $s11 $s5 $s6 ", scalar(g(1, 2, 3)), " ", g(1, 2, 3) + 0, " ", (g(0, 0) ? "T" : "F"), " ", g(4, 5, 6), "\n";
sub ee { (@_)[0, 1] } my $s12 = ee(); my @l12 = ee(); sub oo { (@_)[3] } my $s13 = oo(1); my @l13 = oo(1);
print "5 ", (defined $s12 ? "def" : "undef"), " ", scalar(@l12), " ", (defined $s13 ? "def" : "undef"), " ", scalar(@l13), "\n";
PL

# ---- #1955 a foreach list whose value is ONE reference -- a sub / method /
#      code-ref call, scalar(...), a do block, a ternary arm -- runs ONCE with
#      the reference (a BOX is one element); bare arrays, lists, explicit
#      derefs and several values are unchanged -----------------------------------
both_agree(<<'PL', '#1955 for (mk()) with sub mk { [1,2] } runs once with $_ the ARRAY ref');
no warnings;
sub mk { return [1,2] } sub mk2 { [3,4] } sub hr { {a=>1} } sub cr { sub { 1 } } sub lst { (1,2,3) } sub none { () }
sub wa { wantarray ? (5,6) : [7] } sub many { return ([1],[2]) } sub arr { my @q = (1,2); return @q }
{ package Obj; sub new { bless { items => [1,2,3] }, shift } sub items { @{$_[0]{items}} } sub list { $_[0]{items} } }
my $o = Obj->new; my $c = \&mk; my @a = (8,9); my $ar = [1,2,3]; my %h = (k => [1,2]);
my @r; sub out { print "$_[0] @r\n"; @r = () }
for (mk()) { push @r, ref($_) } out(1); for (mk(), mk()) { push @r, ref($_) } out(2); for (mk(), 1) { push @r, ref($_) || $_ } out(3);
for my $x (mk()) { push @r, ref($x) } out(4); for (@{ mk() }) { push @r, $_ } out(5); for ($o->items) { push @r, $_ } out(6);
for ($o->list) { push @r, ref($_) } out(7); for (Obj->new->list) { push @r, ref($_) } out(8); for (lst()) { push @r, $_ } out(9);
for (none()) { push @r, "x" } out(10); for (hr()) { push @r, ref($_) } out(11); for (cr()) { push @r, ref($_) } out(12);
for (wa()) { push @r, ref($_) || $_ } out(13); for (&$c()) { push @r, ref($_) } out(14); for ($c->()) { push @r, ref($_) } out(15);
for (\@a) { push @r, ref($_) } out(16); for (scalar(mk())) { push @r, ref($_) } out(17); push @r, ref($_) for mk(); out(18);
for (@a) { push @r, $_ } for (@a, @a) { push @r, $_ } out(19); for ($ar) { push @r, ref($_) } out(20);
for (many()) { push @r, ref($_) } for (arr()) { push @r, $_ } out(21); for (do { [5,6] }) { push @r, ref($_) } out(22);
for (1 ? mk() : ()) { push @r, ref($_) } out(23); for ($h{k}) { push @r, ref($_) } out(24);
print "25 ", join(",", map { ref($_) } mk()), " ", scalar(grep { ref } mk()), "\n";
PL

# ---- #750 a DEREFERENCED array / hash on a list-assignment LHS is GREEDY
#      (`@$r`, `@{EXPR}`, `$r->@*`, `%$h`, `%{EXPR}`, `$h->%*`): it absorbs
#      the rest and starves what follows; a scalar deref / an element stays one
#      slot; the odd trailing key of `%$h = LIST` is undef ----------------------
both_agree(<<'PL', '#750 ($x, @$r) = (1,2,3) fills @$r; deref hashes, starving, vivify, tie, list value');
no warnings;
sub hs { my $h = shift; join(",", map { "$_=" . ($h->{$_} // 'U') } sort keys %$h) }
my $r = []; my $x; my $hr = {}; my $y; my $z;
($x, @$r) = (1,2,3); print "1 x=$x r=[@$r] n=", scalar(@$r), "\n";
($y, %$hr) = (9, a=>1, b=>2); my @l = ((@{[]}) = (1,2)); print "2 y=$y h=[", hs($hr), "] ", scalar(@l), "\n";
$r = []; ($x, @$r, $z) = (1,2,3); print "3 x=$x r=[@$r] z=", ($z // 'U'), "\n";
$r = []; ($x, $r->@*) = (4,5,6); $hr = {}; ($y, $hr->%*) = (8, c => 3); print "4 [@$r] [", hs($hr), "]\n";
$r = [7]; (@$r) = (); my $s = [1]; my $q = [2]; (@$q, @$s) = (1,2); print "5 n=", scalar(@$r), " q=[@$q] s=[@$s]\n";
$hr = {}; (%$hr) = (a=>1, b=>2, 'c'); my $h2 = {}; %$h2 = (d => 1, 'e'); print "6 [", hs($hr), "] [", hs($h2), "]\n";
my $o = {}; sub f { (5,6,7) } ($x, @{$o->{list}}) = f(); $r = []; (undef, @$r) = (1,2,3); print "7 x=$x list=[@{$o->{list}}] [@$r]\n";
$r = [1,2,3]; my $n = (($x, @$r) = (4,5)); $q = []; my @v = (($x, @$q) = (4,5,6)); print "8 n=$n r=[@$r] v=[@v]\n";
my $u; ($x, @$u) = (1,2); my $uh; ($x, %$uh) = (1, k => 'v'); print "9 ", ref($u), " [@$u] ", ref($uh), " [", hs($uh), "]\n";
$r = [1,2,3]; ($x, @$r) = @$r; my $sc = \my $t; ($x, $$sc) = (1,2,3); my $e = [0]; ($x, $e->[0]) = (1,2,3); print "10 x=$x r=[@$r] t=$t [@$e]\n";
{ package TA; sub TIEARRAY { bless { a => [] }, shift } sub FETCHSIZE { scalar @{$_[0]{a}} } sub FETCH { $_[0]{a}[$_[1]] }
  sub STORE { push @main::log, "S$_[1]"; $_[0]{a}[$_[1]] = $_[2] } sub CLEAR { push @main::log, "C"; @{$_[0]{a}} = () } sub EXTEND { push @main::log, "E$_[1]" }
  sub STORESIZE { push @main::log, "Z$_[1]" } }
our @log; tie my @ta, 'TA'; my $tr = \@ta; ($x, @$tr) = (1, 2, 3); print "11 [@ta] log=@log\n";
$r = []; for (($x, @$r) = (7,8)) { $_ .= "!" } my $cnt = () = (($x, @$q) = (1,2,3)); $r = [9]; $hr = {q => 1}; (@$r, %$hr) = (1,2);
print "12 $x $cnt [@$r] [", hs($hr), "]\n";
PL

# ==== s543j (2026-11-22) ====

# ---- #5210 `local %ENV` keeps the environment's magic: a child process
#      started inside the scope sees the localized environment (a bare
#      `local %ENV` an empty one), the old one comes back at the exit -- also
#      by die, and with the #1030 undef marks; `local $ENV{K}` unchanged ----
both_agree(<<'PL', '#5210 local %ENV reaches a child process and is restored at the scope exit');
no warnings;
$ENV{T11A} = "outer"; $ENV{T11U} = undef; my $c = 1;
sub sh { my $o = `/bin/sh -c 'echo "[\$T11A][\$T11B]"'`; chomp $o; $o }
{ local %ENV = (T11B => "inner"); print "1 ", sh(), " ", ($ENV{T11A} // 'U'), "\n"; }
{ local %ENV = (%ENV, T11B => "b"); delete $ENV{T11A}; print "2 ", sh(), "\n"; }
{ local %ENV; $ENV{T11B} = "x"; print "3 ", sh(), " ", scalar(keys %ENV), "\n"; }
sub f { local %ENV = (T11B => $_[0]); sh() } print "4 ", f("p"), f("q"), "\n";
eval { local %ENV = (T11B => "d"); die "x\n" }; print "5 ", sh(), "\n";
{ local %ENV = (T11B => "m") if $c; print "6 ", sh(), "\n"; }
{ local $ENV{T11A} = "e"; print "7 ", sh(), "\n"; }
print "8 ", sh(), " ", (defined $ENV{T11U} ? "D" : "U"), (exists $ENV{T11U} ? "E" : "N"), "\n";
PL
