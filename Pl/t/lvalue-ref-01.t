#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# lvalue-ref-01.t — \substr / \pos / \vec are live write-through references to
# scalar magic lvalues, implemented via a p-magic-cell (kind :lvalue) intercepted
# at the box chokepoints (unbox / box-set / box-sv / box-nv), the same mechanism
# `tie` and \$#array (arylen) use.
#
# Session 219: previously \substr(...) compiled to (p-backslash (p-substr ...)),
# backslashing a COPY of the extracted value — so $$ref = X did not write back.
# Now it compiles to (p-substr-ref ...): reading yields the current value, writing
# replaces the region in place. ref()/reftype()/stringify all report "LVALUE"
# (arylen's cell has kind nil → "SCALAR"). Fixes ref.t substr/pos/vec lvalue rows.

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

plan tests => 38;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# --- codegen ---
like(transpile('my $s="x"; my $r=\substr($s,0,1);'),
    qr/\(p-substr-ref /, '\\substr compiles to (p-substr-ref ...)');
like(transpile('my $s="x"; my $r=\vec($s,0,8);'),
    qr/\(p-vec-ref /, '\\vec compiles to (p-vec-ref ...)');

# --- \substr ---
test_cl('read through \substr ref',
    q{my $s="hello"; my $r=\substr($s,1,3); print "$$r\n";}, "ell\n");
test_cl('write through \substr ref (same length)',
    q{my $s="hello"; my $r=\substr($s,0,1); $$r="J"; print "$s\n";}, "Jello\n");
test_cl('write through \substr ref (multi-char replace)',
    q{my $s="hello world"; my $r=\substr($s,0,5); $$r="HELLO"; print "$s\n";},
    "HELLO world\n");

# --- \vec ---
test_cl('write then read through \vec ref',
    q{my $s=""; my $r=\vec($s,0,8); $$r=65; print "$s $$r\n";}, "A 65\n");

# --- ref() / reftype() / stringify report LVALUE ---
test_cl('ref(\substr) is LVALUE (direct)',
    q{my $s="hi"; print ref(\substr($s,0,1)),"\n";}, "LVALUE\n");
test_cl('ref(\substr) is LVALUE (stored through variable)',
    q{my $s="hi"; my $r=\substr($s,0,1); print ref($r),"\n";}, "LVALUE\n");
test_cl('ref(\pos) is LVALUE',
    q{my $s="x"; my $r=\pos($s); print ref($r),"\n";}, "LVALUE\n");
test_cl('ref(\vec) is LVALUE',
    q{my $s="x"; my $r=\vec($s,0,1); print ref($r),"\n";}, "LVALUE\n");
test_cl('stringify of \substr ref matches LVALUE(0x..)',
    q{my $s="x"; my $r=\substr($s,0,1); print "$r"=~/^LVALUE\(0x[0-9a-f]+\)$/?"ok\n":"$r\n";},
    "ok\n");

# --- arylen ref must STILL be SCALAR, not LVALUE (kind nil) ---
test_cl('arylen \$#a ref stays SCALAR (not LVALUE)',
    q{my @a=(1,2,3); print ref(\$#a),"\n";}, "SCALAR\n");

# --- foreach aliasing of a substr() lvalue (perl #24346) ---
# `for (substr(...)) { $_ = ... }` must bind $_ to the substr lvalue window so
# the assignment writes through, mirroring `for (@a) { $_ = ... }`.  Uses the same
# bare magic-cell as \substr (p-substr-lvalue-cell), via the foreach codegen.
like(transpile('my $x="abcdef"; for (substr($x,1,3)) { $_="XX" }'),
    qr/\(p-substr-lvalue-cell /, 'for(substr) compiles to p-substr-lvalue-cell');
test_cl('for(substr) write-through to source',
    q{my $x="abcdef"; for (substr($x,1,3)) { $_="XX" } print "$x\n";}, "aXXef\n");
test_cl('for(substr) reads current window value',
    q{my $x="abcdef"; for (substr($x,1,3)) { print "$_\n" }}, "bcd\n");
# Edit-tracking: a fixed positive-length window re-anchors to the written length,
# so the second assignment replaces just the 2 chars written by the first.
test_cl('for(substr) edit-tracking (shrink then re-assign)',
    q{my $x="abcdef"; for (substr($x,1,3)) { $_="XX"; $_="Y" } print "$x\n";},
    "aYef\n");
# A positive-start to-end window keeps tracking from that offset to the new end.
test_cl('for(substr) positive-start to-end tracks appended text',
    q{my $x="abcdef"; for (substr($x,1)) { $_="XX"; $x.="z"; print "$_\n" }},
    "XXz\n");
# A negative start re-anchors from the END after an edit (perl #24346): the
# window becomes substr($x,-2), so after appending it reads the last 2 chars.
test_cl('for(substr) negative start re-anchors from end',
    q{my $x="abcdef"; for (substr($x,-5)) { $_="XX"; $x.="z"; print "$_\n" }},
    "Xz\n");

# --- \ + a SIGIL-DEREF carrying a subscript (task #861) -------------------
# The same "backslashed a COPY" failure as \substr above, arriving by a
# different route: PPI tags `\` as a PPI::Token::Cast exactly like the
# sigils, so `\$$h{k}` reached the subscript builder as a TWO-cast run.
# That sent it down the #305 widened path, which then spliced the WHOLE run
# away -- the `\` included.  The emission held no p-backslash at all, so the
# "reference" was the element's VALUE and the write through it was lost.
# `\` is not a deref: the term grammar is `cast* primary postfix*` with
# `cast := $ @ % & *`, and `\$$h{k}` is `\( ${$h}{k} )` -- the very thing
# `\$h->{k}` is, which is why the arrow spellings were always right.
like(transpile('my $h={}; my $r = \$$h{k};'),
    qr/\(p-backslash \(p-gethash-deref-box /,
    '\\$$h{k} compiles to (p-backslash (p-gethash-deref-box ...))');
like(transpile('my $a=[]; my $r = \${$a}[0];'),
    qr/\(p-backslash \(p-aref-deref-box /,
    '\\${$a}[0] compiles to (p-backslash (p-aref-deref-box ...))');
test_cl('\\ + sigil-deref-with-subscript takes a REAL reference, all four spellings',
    q{my $h={k=>"v"}; my $a=["z"];
      my $r1 = \$$h{k};   $$r1 = "W1";
      my $r2 = \${$h}{k}; $$r2 = "W2";
      my $r3 = \$$a[0];   $$r3 = "W3";
      my $r4 = \${$a}[0]; $$r4 = "W4";
      print join("|", ref($r1), ref($r2), ref($r3), ref($r4)), "\n";
      print join("|", $h->{k}, $a->[0]), "\n";},
    "SCALAR|SCALAR|SCALAR|SCALAR\nW2|W4\n");
# The arrow and named twins, and a chained subscript, must answer the same.
test_cl('\\ arrow / named / chained twins agree with the deref spellings',
    q{my %n=(k=>"v"); my @m=("z"); my $h={k=>"v"}; my $a=["z"];
      my $d = { x => [ { y => "Y" } ] };
      my $r5 = \$h->{k};      $$r5 = "W5";
      my $r6 = \$a->[0];      $$r6 = "W6";
      my $r7 = \$n{k};        $$r7 = "W7";
      my $r8 = \$m[0];        $$r8 = "W8";
      my $r9 = \$$d{x}[0]{y}; $$r9 = "W9";
      print join("|", $h->{k}, $a->[0], $n{k}, $m[0], $d->{x}[0]{y}), "\n";},
    "W5|W6|W7|W8|W9\n");
# The multi-cast deref runs the #305 path exists for must NOT move: a `\`
# stops the run, every sigil still continues it.
test_cl('multi-cast deref runs are unchanged by the \\ stop',
    q{my %H=(a=>1,b=>2); my @A=(10,20,30);
      my $h=\%H; my $a=\@A; my $hh=\$h; my $aa=\$a;
      print join("|", $$h{a}, $$a[1], $$$hh{a}, $$$aa[2]), "\n";
      print join("|", join(",",@$a[0,1]), join(",",@$h{qw(a b)}),
                      join(",",@$$aa[0,1]), join(",",@$$hh{qw(a b)})), "\n";
      print join("|", $$hh->{a}, $$aa->[0], ${$h}{a}, ${$a}[0]), "\n";},
    "1|20|1|30\n10,20|1,2|10,20|1,2\n1|10|1|10\n");

# ---- #939: an lvalue substr as an `=~` TARGET -----------------------------
# perl performs an s///-or-tr/// THROUGH the substr window, so the write lands
# in the ORIGINAL string (t/op/substr.t's `$#ta` block, and the wild
# `substr($munged_seed,-1)=~tr/0-9a-f/1-9a-f0/` in t/run/runenv_hashseed.t).
# PCL evaluated substr as an RVALUE and handed do-regex-subst a raw CL string,
# so the write had nowhere to land: a silent no-op until #911 turned that site
# into perl's read-only death.  The `=~` target is now rewritten to the
# write-through CELL when — and only when — the RHS actually writes
# (_rhs_writes_match_target: never for m//, /r, or a tr that cannot change its
# target), the SAME cell `\substr` and `for (substr(…))` already bind.

sub both_agree {
    my ($code, $desc) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code; close $fh;
    my $perl = `perl $pl_file 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree(<<'PL', '#939: substr as an =~ target writes THROUGH the window');
my $s = "hello"; substr($s,0,2) =~ s/he/XY/;            print "1:$s\n";
my $t = "hello"; substr($t,3)   =~ s/lo/LO/;            print "2:$t\n";
my $u = "hello"; substr($u,-2)  =~ s/lo/LO/;            print "3:$u\n";
my $v = "hello"; substr($v,0,2) =~ tr/a-z/A-Z/;         print "4:$v\n";
my $w = "hello"; substr($w,0,2) =~ s/he/XYZ/;           print "5:$w\n";
my $x = "hello"; substr($x,0,3) =~ s/hel/H/;            print "6:$x\n";
my $y = "hello"; my $n = substr($y,0,5) =~ s/l/L/g;     print "7:$y:$n\n";
my %h = (k=>"hello"); substr($h{k},0,2) =~ s/he/XY/;    print "8:$h{k}\n";
my $r = ["hello"];    substr($r->[0],0,2) =~ s/he/XY/;  print "9:$r->[0]\n";
our @ta; $#ta = -1; substr($#ta,0,2) =~ s/\A..\z/23/s;  print "10:$#ta\n";
PL

# The same target in the three CONTEXTS `ctx_bind` wraps an operand in.  A
# context bind says nothing about where a value LIVES, so the routing looks
# through it — without that, only the scalar and boolean spellings were
# rewritten and the LIST one (`push @r, (…)`, a `(p-list-ctx (p-substr …))`)
# declined to the read-only death.
both_agree(<<'PL', '#939: …in scalar, boolean and LIST context alike');
my $s = "hello"; my $n = (substr($s,0,2) =~ s/he/XY/); print "1:$s:$n\n";
my $t = "hello"; if (substr($t,0,2) =~ s/he/AB/) { print "2:$t\n" }
my @r; my $u = "hello"; push @r, (substr($u,0,2) =~ s/he/CD/); print "3:$u:@r\n";
my $v = "hello"; my @w = (substr($v,0,2) =~ s/he/EF/); print "4:$v:@w\n";
PL

both_agree(<<'PL', '#939: the NON-writing =~ spellings leave the target alone');
my $a = "hello"; my $c = substr($a,0,2) =~ s/he/XY/r;    print "1:$a:$c\n";
my $b = "hello"; my $d = substr($b,0,2) =~ s/zz/XY/;     print "2:$b:", ($d||0), "\n";
my $e = "hello"; my $f = substr($e,0,2) =~ tr/a-z/A-Z/r; print "3:$e:$f\n";
my $g = "hello"; my $i = substr($g,0,4) =~ tr/l//;       print "4:$g:$i\n";
my $j = "hello"; print "5:", (substr($j,0,2) =~ /he/ ? 1 : 0), ":$j\n";
PL

# The INVERSE: everything the routing must NOT move.  Passes on the base tree
# (verified on a 4c354bc worktree) except the two positive shape rows.
like(transpile('my $s="x"; substr($s,0,1) =~ m/x/;'),
    qr/\(p-=~ \(p-substr \$s 0 1\)/,
    '#939 inverse: a READ (m//) target keeps the plain p-substr rvalue');
like(transpile('my $s="x"; substr($s,0,1) =~ s/x/y/r;'),
    qr/\(p-=~ \(p-substr \$s 0 1\)/,
    '#939 inverse: an /r target keeps the plain p-substr rvalue');
like(transpile('my $s="x"; substr($s,0,1) =~ s/x/y/;'),
    qr/\(p-=~ \(p-substr-lvalue-cell \$s 0 1\)/,
    '#939: a WRITING target binds p-substr-lvalue-cell');
like(transpile('my @a; substr($#a,0,2) =~ s/x/y/;'),
    qr/\(p-substr-lvalue-cell \(p-arylen-lvalue-cell \@a\) 0 2\)/,
    "#939: substr's own target argument is a place too (loose lvalue context)");
test_cl('#939 inverse: \\substr / \\$#a / plain substr-assign / foreach alias unmoved',
    q{my @a=(1,2,3); my $x="hello"; my $y="hello";
      substr($y,0,2) = "AB";
      for (substr($x,0,2)) { $_ = "CD" }
      print join("|", ref(\substr($x,0,2)), ref(\$#a), $x, $y), "\n";},
    "LVALUE|SCALAR|CDllo|ABllo\n");

# ── #960 half (b): the OTHER TWO write spellings of a magic-lvalue window ────
# #939 routed `substr(X,…) =~ s///`.  `substr(X,…) = V` and the 4-argument
# `substr(X,…,V)` never passed through that routing, so their target stayed an
# RVALUE — and once elements went RAW (s455) `(p-aref @a 0)` is a plain string,
# p-substr's 4-arg form is SILENT on a non-box, and the write landed nowhere
# and said nothing.  perl gives substr/vec/pos their FIRST argument in LOOSE
# LVALUE CONTEXT, so all three spellings write THROUGH the element; one
# rewriter (_lvalue_target_form) now serves all three.
both_agree(<<'PL', '#960: substr(ELEM,…) = V and the 4-arg call write through');
my @a=("hello"); substr($a[0],0,2) = "AB";     print "1:$a[0]\n";
my @b=("hello"); substr($b[0],0,2,"AB");       print "2:$b[0]\n";
my %h=(k=>"hello"); substr($h{k},0,2) = "AB";  print "3:$h{k}\n";
my %g=(k=>"hello"); substr($g{k},0,2,"AB");    print "4:$g{k}\n";
my $r=["hello"]; substr($r->[0],0,2) = "AB";   print "5:$r->[0]\n";
my $q=["hello"]; substr($q->[0],0,2,"AB");     print "6:$q->[0]\n";
my @c=("hello"); substr($c[0],-2) = "ZZ";      print "7:$c[0]\n";
my @d=("hello"); my $o = substr($d[0],0,2,"AB"); print "8:$o/$d[0]\n";
my $n=[["hello"]]; substr($n->[0][0],0,2)="AB"; print "9:$n->[0][0]\n";
PL

# The SIBLINGS of substr in %MAGIC_LVALUE_BASE take the same argument the same
# way, and `$#a` is the one that makes "loose lvalue context" visible: writing
# through a substr window onto an array's LENGTH resizes the array.
both_agree(<<'PL', '#960: vec / pos / $#a targets, and the nested spellings');
our @ta; @ta=(1)x10; substr($#ta,0,2) = 23;      print "1:", scalar(@ta), "\n";
our @tb; @tb=(1)x10; substr($#tb,0,2,23);        print "2:", scalar(@tb), "\n";
my @a=("\0\0"); vec($a[0],0,8) = 65;             print "3:$a[0]\n";
my @b=("abcdef"); $b[0]=~/abc/g; pos($b[0])=1;   print "4:", pos($b[0]), "\n";
my $c={tmp=>"abcdef"}; pos($c->{tmp})=2;         print "5:", pos($c->{tmp}), "\n";
my $foo="12345678"; vec(substr($foo,1,3),5,4)=3; print "6:", unpack("H*",$foo), "\n";
my $bar="12345678"; substr(substr($bar,1,3),0,1)="Z"; print "7:$bar\n";
PL

# THE @_ ALIAS, which is where the two shapes occur in real code (t/op/
# tiehandle.t writes its buffer through `substr($_[1], OFF) = …`).
both_agree(<<'PL', '#960: a write through an @_ element reaches the caller');
sub wr  { substr($_[0],0,2) = "XY" }
sub wr4 { substr($_[0],0,2,"XY") }
sub len { $#{$_[0]} = $_[1] }
my $s="hello"; wr($s);            print "1:$s\n";
my $t="hello"; wr4($t);           print "2:$t\n";
my $a=[1,2,3]; len($a,5);         print "3:", scalar(@$a), "\n";
sub rd { substr($_[0],0,2) }
my $u="hello"; my $x=rd($u);      print "4:$x/$u\n";
PL

# THE INVERSE, and it is the point of the whole design: a READ must NOT ask for
# the place.  Imposing lvalue context on `substr($a[0],0,2)` unconditionally
# would promote the element to a box on every access — exactly the cost the
# boxed-aggregates work removed — so the gate is the WRITE POSITION, spelled
# as the argument COUNT for the 4-arg call and as the `=` operator for the
# assignment.  These four rows pass on the 6e6f191 base tree; the positive
# ones above do not.
like(transpile('my @a; my $x = substr($a[0],0,2);'),
    qr/\(p-substr \(p-aref \@a 0\) 0 2\)/,
    '#960 inverse: a 3-argument READ keeps the raw element');
like(transpile('my @a; my $x = substr($a[0],0);'),
    qr/\(p-substr \(p-aref \@a 0\) 0\)/,
    '#960 inverse: a 2-argument READ keeps the raw element');
like(transpile('my @a; substr($a[0],0,2,"AB");'),
    qr/\(p-substr \(p-aref-box \@a 0\) 0 2 "AB"\)/,
    '#960: the 4-argument call takes the element BOX');
like(transpile('my @a; substr($a[0],0,2) = "AB";'),
    qr/\(p-setf \(p-substr \(p-aref-box \@a 0\) 0 2\) "AB"\)/,
    '#960: the `=` spelling takes the element BOX, head unchanged');
