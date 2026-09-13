#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ref-to-ref-01.t — ref() / reftype() distinguish a scalar reference from a
# reference-to-a-reference (session 217).
#
# The bug: p-ref classified a ref-to-ref ("REF") as "SCALAR" because box-nesting
# depth does not separate the two — a `my`-bound ref ($r = \$x) round-trips to
# the same depth as a plain scalar ref. The fix uses the box `is-ref` flag (set
# only by p-backslash on scalar-ref wrappers) to find the referent, and reports
# "REF" iff that referent is itself a wrapper or *holds* a reference. The
# held-a-ref test is non-recursive, so a self-referential scalar ($x = \$x) does
# not loop and plain scalars (incl. undef, '' array elements) stay "SCALAR".

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

plan tests => 45;

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

# ── plain scalar ref vs ref-to-ref, through `my` variables ───────────────────
# NB run_cl strips blank lines, so an empty ref() prints nothing observable.
test_cl('ref(non-ref) is empty',
    'my $x = 1; print "[", ref($x), "]\n";', "[]\n");

test_cl('ref($r) where $r = \\$x is SCALAR',
    'my $x = 1; my $r = \$x; print ref($r), "\n";', "SCALAR\n");

test_cl('ref($rr) where $rr = \\$r is REF',
    'my $x = 1; my $r = \$x; my $rr = \$r; print ref($rr), "\n";', "REF\n");

# ── direct (no intermediate variable) ────────────────────────────────────────
test_cl('ref(\\$x) direct is SCALAR',
    'my $x = 5; print ref(\$x), "\n";', "SCALAR\n");

test_cl('ref(\\\\1) direct is REF',
    'print ref(\\\\1), "\n";', "REF\n");

test_cl('ref(\\$r) direct is REF',
    'my $x = 1; my $r = \$x; print ref(\$r), "\n";', "REF\n");

# ── aggregate refs unaffected ────────────────────────────────────────────────
test_cl('ref(\\@a) is ARRAY',
    'my @a = (1,2); print ref(\@a), "\n";', "ARRAY\n");

test_cl('ref of scalar holding an arrayref, through a var, is REF',
    'my @a = (1,2); my $ar = \@a; my $rar = \$ar; print ref($rar), "\n";', "REF\n");

# ── undef referent stays SCALAR (regression guard: *p-undef* must not look ref) ─
test_cl('ref(\\$undef) is SCALAR',
    'my $u; print ref(\$u), "\n";', "SCALAR\n");

# ── self-referential scalar must not loop, and is REF ────────────────────────
test_cl('self-referential $x = \\$x is REF (no hang)',
    'my $x; $x = \$x; print ref($x), "\n";', "REF\n");

# ── reftype of a ref-to-ref is REF, not SCALAR: perl reports SvTYPE of the
#    referent, and an SV that HOLDS a reference is an RV (probed 5.40.3 —
#    `my $x=1; my $r=\$x; my $rr=\$r; reftype($rr)` is REF, and so is ref($rr);
#    only a referent holding a PLAIN scalar is SCALAR, the row below).  This
#    row asserted SCALAR until #1619, where it was the reftype half of the
#    same wrong level. ──────────────────────────────────────────────────────
test_cl('reftype($rr) of a ref-to-ref is REF, like ref($rr)',
    'use Scalar::Util qw(reftype);'
  . 'my $x=1; my $r=\$x; my $rr=\$r; print reftype($rr), " ", ref($rr), "\n";',
    "REF REF\n");

test_cl('reftype($r) of a plain scalar ref is SCALAR',
    'use Scalar::Util qw(reftype);'
  . 'my $x=1; my $r=\$x; print reftype($r), "\n";', "SCALAR\n");

# ── glob ref numifies to a (non-zero) address like other refs; a *bare* glob
#    numifies to 0.  Both share box-value=typeglob — the is-ref flag set by
#    p-backslash (and preserved by box-set) is the only discriminator.  (Exact
#    address round-trip is GC-fragile, so we only assert non-zero-ness here.)
test_cl('glob ref numifies to a non-zero address',
    'our $g_t = 5; my $r = \*g_t; print +($r + 0 != 0 ? "nonzero" : "zero"), "\n";',
    "nonzero\n");

test_cl('bare glob in scalar numifies to 0',
    'our $g_t = 5; my $g = *g_t; print 0 + $g, "\n";', "0\n");

# ── ${ARRAY-, HASH- or CODE ref} is perl's fatal (#1249(1) s473h, #1592 s483a)
#    The REFERENT rule is the discriminator: `\@a`'s referent is the array
#    itself, while a `\$aref` read back out of a container has the same UNBOXED
#    shape but a scalar BOX for a referent (#154's ambiguity).  CODE was
#    excluded until s483a on a measurement of a DIFFERENT check (a type sniff on
#    the unboxed value, which Sub::Quote's `${$_[1]->{'$t'}}` does take down —
#    see the #1592 rows below and Pl/t/moo-01.t).
test_cl('${$aryref} dies "Not a SCALAR reference"',
    'my @a=(1,2); my $r=\@a; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${$hashref} dies "Not a SCALAR reference"',
    'my %h=(k=>1); my $r=\%h; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${\&named} dies "Not a SCALAR reference" (#1592)',
    'sub cc { 1 } my $r=\&cc; my $v = eval { "".${$r} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "died\n");

test_cl('${ \\$aryref } is the array ref, not a fatal',
    'my @a=(1,2); my $ar=\@a; my $rr=\$ar; print ref(${$rr}), "\n";', "ARRAY\n");

test_cl('${ \\$hashref } is the hash ref, not a fatal',
    'my %h=(k=>1); my $hr=\%h; my $rr=\$hr; print ref(${$rr}), "\n";', "HASH\n");

test_cl('\\(@b, 9) is (ARRAY, SCALAR) and ${$r[0]} is the fatal',
    'my @b=(7,8); my @r = \(@b, 9);'
  . 'print scalar(@r), ref($r[0]), ref($r[1]), "\n";'
  . 'eval { my $v = "".${$r[0]} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no"), "\n";',
    "2ARRAYSCALAR\ndied\n");

# ── #1592 (s483a): a CODE referent is the same fatal, and the shapes that must
#    keep working are Sub::Quote's.  `${$coderef}` used to hand the code ref
#    back silently, and `${$coderef} = 5` CLOBBERED the variable holding it, so
#    the next call through that variable died "Undefined subroutine &main::5".
test_cl('${$coderef} dies and the sub stays callable',
    'my $cr = sub { 7 }; my $v = eval { "".${$cr} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";'
  . 'print $cr->(), "\n";',
    "died\n7\n");

test_cl('${CODE} dies through a container element and as a literal',
    'my $cr = sub { 7 }; my %h = (c => $cr); my @a = ($cr); my $o = "";'
  . 'for my $t (sub { ${$h{c}} }, sub { ${$a[0]} }, sub { ${sub {7}} }) {'
  . '  eval { my $v = "".$t->() };'
  . '  $o .= ($@ =~ /^Not a SCALAR reference/ ? "d" : "n");'
  . '} print $o, "\n";',
    "ddd\n");

test_cl('the CALL spellings still call; @{$cr} and %{$cr} keep their own fatals',
    'my $cr = sub { 7 }; print $cr->(), &{$cr}(), &$cr(), "\n";'
  . 'eval { my @x = @{$cr} };'
  . 'print +($@ =~ /^Not an ARRAY reference/ ? "ary-died" : "no:[$@]"), "\n";'
  . 'eval { my %x = %{$cr} };'
  . 'print +($@ =~ /^Not a HASH reference/ ? "hash-died" : "no:[$@]"), "\n";',
    "777\nary-died\nhash-died\n");

test_cl('a ref TO a scalar holding a code ref still derefs (Sub::Quote)',
    'my $cr = sub { 7 }; my $rr = \$cr; print ref(${$rr}), "\n";'
  . 'my $caps = { q{$t} => \$cr };'
  . 'my $s = eval q{ sub { my $t = ${$_[1]->{q{$t}}}; ref($t) } };'
  . 'print $s->(undef, $caps), "\n";',
    "CODE\nCODE\n");

test_cl('${$ref} = 5 is the same fatal for ARRAY, HASH and CODE referents',
    'my @a=(1,2); my %h=(k=>1); my $cr = sub { 7 }; my $o = "";'
  . 'for my $t (sub { ${\@a} = 5 }, sub { ${\%h} = 5 }, sub { ${$cr} = 5 }) {'
  . '  eval { $t->() };'
  . '  $o .= ($@ =~ /^Not a SCALAR reference/ ? "d" : "n");'
  . '} print $o, "\n";'
  . 'print scalar(@a), scalar(keys %h), $cr->(), "\n";',
    "ddd\n217\n");

test_cl('the scalar-ref and symbolic WRITE paths are untouched',
    'my $x = 1; my $r = \$x; ${$r} = 5; print $x, "\n";'
  . 'my $cr = sub { 7 }; my $rc = \$cr; ${$rc} = 9; print $cr, "\n";'
  . 'our $nm; my $n = "nm"; ${$n} = 3; print $main::nm, "\n";',
    "5\n9\n3\n");

test_cl('"$$coderef" interpolation is the fatal too',
    'my $cr = sub { 7 }; my $s = eval { "$$cr" };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$s]"), "\n";',
    "died\n");

# ── #1592, the other half: `\` on a CODE VALUE gains the anonymous scalar perl
#    gives it, so a reference TO a code ref is one level deeper than the code
#    ref.  It used to be idempotent (box(FN) either way), which is what made
#    the fatal above undecidable — and it answered ref(\\&f) = CODE.
test_cl('ref(\\\\&f) is REF and ref(\\&f) is CODE',
    'sub f { 1 } print ref(\&f), " ", ref(\\\\&f), " ", ref(\ sub { 7 }), "\n";',
    "CODE REF REF\n");

test_cl('a ref to a code ref derefs to the code ref, and once more is the fatal',
    'sub mysub2 { lc shift } our $subrefref = \\\\&mysub2;'
  . 'print $$subrefref->("GOOD"), "\n";'
  . 'my $rr = \\\\&mysub2; print ref(${$rr}), " ", ${$rr}->("X"), "\n";'
  . 'eval { my $v = "".${${$rr}} };'
  . 'print +($@ =~ /^Not a SCALAR reference/ ? "died" : "no:[$@]"), "\n";',
    "good\nCODE x\ndied\n");

test_cl('\\&$coderef is that same code ref, not a reference to it',
    'my $cr = sub { 7 }; my $r = \&$cr; print ref($r), " ", $r->(), "\n";'
  . 'sub g { 5 } my $n = "g"; my $r2 = \&$n; print ref($r2), " ", $r2->(), "\n";',
    "CODE 7\nCODE 5\n");

# ── #1619: `ref(\$var)` reports the blessing of the REFERENT ITSELF, and a
#    scalar that merely HOLDS a blessed reference is not blessed.  PCL kept the
#    class of the held reference in the variable box's class SLOT, where it
#    collided with the *other* thing that slot means — the SvSTASH `bless \$x`
#    writes on the scalar — so every reader answered the held object's class
#    (probed 5.40.3: REF for all four payload kinds below).  The scalar's own
#    stash now lives in its own table (*p-sv-stash*).
test_cl('ref(\$obj) is REF for every blessed payload kind',
    'my $h = bless {}, "H"; my $a = bless [], "A";'
  . 'my $c = bless sub { 11 }, "K"; my $s = "x"; my $b = bless \$s, "S";'
  . 'print join(" ", ref(\$h), ref(\$a), ref(\$c), ref(\$b)), "\n";'
  . 'print join(" ", ref($h), ref($a), ref($c), ref($b)), "\n";',
    "REF REF REF REF\nH A K S\n");

test_cl('an element holding an object is not a blessed scalar either',
    'my %hh; $hh{k} = bless {}, "H"; my @aa; $aa[0] = bless {}, "H";'
  . 'my $rr = \\(bless {}, "H");'
  . 'print join(" ", ref(\$hh{k}), ref(\$aa[0]), ref($rr)), "\n";'
  . 'print join(" ", ref($hh{k}), ref($aa[0]), ref($$rr)), "\n";',
    "REF REF REF\nH H H\n");

test_cl('bless \$x writes the SCALAR stash and leaves what $x HOLDS alone',
    'my $h = {}; bless \$h, "S"; print ref(\$h), " ", ref($h), "\n";'
  . 'my $h2 = bless {}, "H"; bless \$h2, "S";'
  . 'print ref(\$h2), " ", ref($h2), "\n";'
  . 'my $a = bless [], "A"; bless \$a, "S";'
  . 'print ref(\$a), " ", ref($a), "\n";',
    "S HASH\nS H\nS A\n");

test_cl('blessing a ref TO an object does not re-class the object',
    'my $a1 = bless {}, "A"; my $r = \$a1; bless $r, "F";'
  . 'print ref($a1), " ", ref($r), " ", ref(\$a1), "\n";',
    "A F F\n");

test_cl('blessed() and reftype() follow ref() through the same rule',
    'use Scalar::Util qw(blessed reftype);'
  . 'my $o = bless {}, "H"; my $s = "x"; my $b = bless \$s, "S";'
  . 'print join(" ", map { defined $_ ? $_ : "undef" }'
  . '  blessed(\$o), blessed($o), blessed(\$b), blessed($b)), "\n";'
  . 'print join(" ", reftype(\$o), reftype($o), reftype(\$b), reftype($b)), "\n";',
    "undef H undef S\nREF HASH REF SCALAR\n");

test_cl('a plain scalar ref keeps its own stash, and never shows it through $x',
    'my $s = "x"; bless \$s, "S";'
  . 'print ref(\$s), " ", ref($s), " [$s]", "\n";'
  . 'my $u; print ref(\$u), " ", ref(\5), "\n";',
    "S  [x]\nSCALAR SCALAR\n");

# ── #1618: `$coderef->[0]` is perl's "Not an ARRAY reference" on the READ path
#    too (the write path and the whole HASH twin already died).  p-aref-deref
#    answered "the sub itself" for a raw function in container position, a rule
#    written in April 2026 for the ONE-ELEMENT LIST SLICE `(sub{…})[0]`, which
#    reached that entry bare.  It no longer does — a list-slice operand is
#    wrapped in `(vector …)` — so the arm served only the deref, and the slice
#    family below is unmoved (probed: perl answers the sub for every one).
test_cl('$coderef->[0] dies on the read path, in all three spellings',
    'my $cr = sub { 7 }; my $o = "";'
  . 'for my $t (sub { $cr->[0] }, sub { ${$cr}[0] }, sub { $$cr[0] },'
  . '           sub { $cr->[1] }, sub { $cr->[-1] }) {'
  . '  eval { my $v = "".$t->() };'
  . '  $o .= ($@ =~ /^Not an ARRAY reference/ ? "d" : "n");'
  . '} print $o, "\n"; print $cr->(), "\n";',
    "ddddd\n7\n");

test_cl('a one-element LIST SLICE of a sub is still the sub',
    'my $cr = sub { 7 };'
  . 'my $a = (sub { 7 })[0]; my $b = (sub {7}, sub {8})[1];'
  . 'my $c = ($cr)[0]; my $d = ($cr)[1];'
  . 'print join(" ", ref($a), ref($b), $b->(), ref($c), $c->(),'
  . '  (defined $d ? "def" : "undef")), "\n";'
  . 'sub mk { return (sub { 5 }, sub { 6 }) }'
  . 'my $e = (mk())[1]; print ref($e), " ", $e->(), "\n";',
    "CODE CODE 8 CODE 7 undef\nCODE 6\n");

test_cl('the ARRAY fatal does not reach an ARRAY ref or the HASH twin',
    'my $ar = [5,6]; my $hr = {k=>9}; my $cr = sub { 7 };'
  . 'print $ar->[0], $hr->{k}, "\n";'
  . 'eval { my $v = "".$cr->{k} };'
  . 'print +($@ =~ /^Not a HASH reference/ ? "hash-died" : "no:[$@]"), "\n";'
  . 'my $s = "xy"; my $x = $s->[0];'
  . 'print +(defined $x ? "def" : "undef"), "\n";',
    "59\nhash-died\nundef\n");

# NB: the declarations are spelled in TWO statements, never `\(my $hr = \%h)`
#    — that shape loses the whole-aggregate fatal on its own (task #1639,
#    pre-existing and measured on the base tree), so writing it here would make
#    a row assert the bug instead of the rule.
# ── #1628: element access through a reference of the WRONG KIND is perl's
#    fatal, and "wrong kind" includes a ref-to-ref.  The referent rule already
#    told a `\$x` from the representation layer of a `\%h`; what it also asked
#    was what the referent scalar HOLDS, so a `\$hr` (a scalar holding a
#    hashref) answered "not a scalar ref" and `$$hrr{k}` reached SBCL's GETHASH
#    (a host error naming a P-BOX), `$$arr[0]` answered undef with NO error at
#    all, and `@$arr` handed the referent box back as a one-element list.
#    perl asks only the referent's TYPE: an SV holding an RV is still an SV.
test_cl('element access through a ref-to-ref dies, read and write, both kinds',
    'my %h = (k=>"v"); my $hr = \%h; my $hrr = \$hr;'
  . 'my @a = (10,11);  my $ar = \@a; my $arr = \$ar;'
  . 'my $o = "";'
  . 'for my $t (sub { $$hrr{k} }, sub { $$arr[0] }, sub { $$hrr{k} = 1 },'
  . '           sub { $$arr[0] = 1 }, sub { $arr->[0] }, sub { $hrr->{k} }) {'
  . '  eval { my $v = "".($t->() // "") };'
  . '  $o .= ($@ =~ /^Not a(?:n)? (?:HASH|ARRAY) reference/ ? "d" : "n");'
  . '} print $o, "\n";'
  . 'print $$hr{k}, $$ar[0], "\n";',
    "dddddd\nv10\n");

test_cl('exists / delete through a ref-to-ref die like perl, not quietly',
    'my %h = (k=>"v"); my $hr = \%h; my $hrr = \$hr;'
  . 'my @a = (10,11);  my $ar = \@a; my $arr = \$ar;'
  . 'my $o = "";'
  . 'for my $t (sub { exists $$hrr{k} }, sub { delete $$hrr{k} },'
  . '           sub { exists $$arr[0] }, sub { delete $$arr[0] }) {'
  . '  eval { my $v = "".($t->() // "") };'
  . '  $o .= ($@ =~ /^Not a(?:n)? (?:HASH|ARRAY) reference/ ? "d" : "n");'
  . '} print $o, "\n";'
  . 'print +(exists $$hr{k} ? 1 : 0), (exists $$ar[1] ? 1 : 0), "\n";',
    "dddd\n11\n");

test_cl('the whole-aggregate cast through a ref-to-ref is the same fatal',
    'my %h = (k=>"v"); my $hr = \%h; my $hrr = \$hr;'
  . 'my @a = (10,11);  my $ar = \@a; my $arr = \$ar;'
  . 'my $o = "";'
  . 'for my $t (sub { scalar @$arr }, sub { scalar keys %$hrr },'
  . '           sub { scalar @$hr },  sub { scalar keys %$ar }) {'
  . '  eval { my $v = "".$t->() };'
  . '  $o .= ($@ =~ /^Not a(?:n)? (?:HASH|ARRAY) reference/ ? "d" : "n");'
  . '} print $o, "\n";'
  . 'print scalar(@$ar), scalar(keys %$hr), "\n";',
    "dddd\n21\n");

test_cl('a SYMBOLIC ref — a plain string in the slot — keeps working',
    'no strict "refs"; our @ga = (1,2); our %gh = (z=>9);'
  . 'my $an = "main::ga"; my $hn = "main::gh";'
  . 'print $$an[1], $$hn{z}, "\n";'
  . '$$an[2] = 3; $$hn{q} = 5;'
  . 'print $ga[2], $gh{q}, (exists $$hn{z} ? 1 : 0), "\n";',
    "29\n351\n");

test_cl('a qr// ref is a container of no kind at all',
    'my $qr = qr/x/; my $o = "";'
  . 'for my $t (sub { $$qr[0] }, sub { $$qr{k} }, sub { $qr->[0] },'
  . '           sub { scalar @$qr }, sub { scalar keys %$qr }) {'
  . '  eval { my $v = "".($t->() // "") };'
  . '  $o .= ($@ =~ /^Not a(?:n)? (?:HASH|ARRAY) reference/ ? "d" : "n");'
  . '} print $o, "\n";'
  . 'print +("axb" =~ $qr ? "m" : "no"), " ", ref($qr), "\n";',
    "ddddd\nm Regexp\n");

test_cl('reftype still separates SCALAR from REF (the #1619 answer is intact)',
    'use Scalar::Util qw(reftype);'
  . 'my $x = 7; my $h = {}; my $sr = \$x; my $rr = \$h;'
  . 'print join(" ", reftype($sr), reftype($rr), reftype(\$sr)), "\n";'
  . 'my $bs = bless \(my $p = 7), "S"; my $br = bless \(my $q = {}), "S";'
  . 'print reftype($bs), " ", reftype($br), "\n";',
    "SCALAR REF REF\nSCALAR REF\n");
