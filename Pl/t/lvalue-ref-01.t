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

plan tests => 23;

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
