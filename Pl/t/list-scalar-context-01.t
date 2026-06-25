#!/usr/bin/env perl
# list-scalar-context-01.t — array/list/slice behaviour in scalar context,
# and slice interpolation in strings.
#
# Covers (session 215):
#  - array variable in scalar context  -> element COUNT      (return @a, my $s=@a)
#  - slice in scalar context           -> LAST element       (return @a[..], my $s=@a[..])
#  - map/grep in scalar context        -> COUNT
#  - `return do { @a }` inherits the caller's context (scalar -> count, list -> elems)
#  - array/hash slice interpolated in a string joins with $" (was: ARRAY(0x..))
#  - split nested in join: pattern arg stays scalar (was: =~ returned a list)

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 27;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# ── array variable in scalar context = count ─────────────────────────────────
test_cl('return @a in scalar context = count',
    'our @a=(7,8,9); sub g { return @a } my $s=g(); print "$s\n";',
    "3\n");

test_cl('my $s = @a is count',
    'our @a=(7,8,9); my $s = @a; print "$s\n";',
    "3\n");

test_cl('return map in scalar context = count',
    'our @a=(7,8,9); sub g { return map {$_} @a } my $s=g(); print "$s\n";',
    "3\n");

# ── slice in scalar context = LAST element ───────────────────────────────────
test_cl('return @a[..] in scalar context = last element',
    'our @a=(5,3,1,9); sub g { return @a[0,1,2] } my $s=g(); print "$s\n";',
    "1\n");

test_cl('my $s = @a[..] is last element',
    'our @a=(5,3,1,9); my $s = @a[0,1,2]; print "$s\n";',
    "1\n");

test_cl('my $s = @h{..} is last value',
    'my %h=(x=>10,y=>20,z=>30); my $s = @h{qw(x y z)}; print "$s\n";',
    "30\n");

# ── nested parenthesised list flattens list-valued elements (ranges, arrays,
#    list-returning calls) in list context — was nesting them as one element ───
test_cl('range inside nested () flattens',
    'print join(",", (1..3, 5..7)), "\n";',
    "1,2,3,5,6,7\n");
test_cl('range mixed with scalar inside nested () flattens',
    'print join(",", (1..3, 9)), "\n";',
    "1,2,3,9\n");
test_cl('array inside nested () flattens',
    'my @b=(1,2); print join(",", (0, @b, 3)), "\n";',
    "0,1,2,3\n");
test_cl('list-returning call inside nested () flattens',
    'print join(",", (reverse(1,2,3), 9)), "\n";',
    "3,2,1,9\n");
test_cl('nested () of nested () flattens',
    'print join("-", (("a","b"), "c")), "\n";',
    "a-b-c\n");

# ── slices stay full lists in list context ───────────────────────────────────
test_cl('@a[..] in list assignment keeps all elements',
    'our @a=(5,3,1,9); my @b = @a[0,1,2]; print "@b\n";',
    "5 3 1\n");

# ── return do { @a } inherits caller context ─────────────────────────────────
test_cl('return do { @a } scalar context = count',
    'our @a=(7); my $x = sub { do { return do { @a } }; 2 }->(); print "$x\n";',
    "1\n");

test_cl('return do { @a } list context = elements',
    'our @a=(7,8); my @x = sub { do { return do { @a } }; 2 }->(); print "@x\n";',
    "7 8\n");

# ── array/hash slice interpolation joins with $" ─────────────────────────────
test_cl('array slice interpolates joined (not ARRAY(0x..))',
    'our @a=(5,3,1,9); print "@a[0,2]\n";',
    "5 1\n");

test_cl('hash slice interpolates joined',
    'my %h=(x=>10,y=>20,z=>30); print "@h{qw(x z)}\n";',
    "10 30\n");

test_cl('single element interpolation still works',
    'our @a=(5,3,1,9); print "$a[2]\n";',
    "1\n");

# ── split nested in join: pattern arg evaluated in scalar context ────────────
# `'abc' =~ /b/` is split's scalar pattern (-> 1, matches the "1" chars); the
# enclosing join must not impose list context (which made =~ return a list).
test_cl('split pattern arg stays scalar inside join',
    q{my $r = join ':', split('abc' =~ /b/, 'p1q1r1s'); print "$r\n";},
    "p:q:r:s\n");

# ── (LIST) x $n as a return element inherits caller context ──────────────────
# `return (@a, (@b) x $n)` in list context must list-repeat (@b), not scalar-
# repeat scalar(@b).  At codegen the `x` node has INHERIT_CTX, so we emit a
# runtime *wantarray* check: p-list-x in list context, p-str-x in scalar.
test_cl('(LIST) x $n in return list = list repeat in list context',
    q{my @a=(11,12); my @b=(21,22,23);
      my $c = sub { my ($n)=@_; return (@a, (@b) x $n) };
      my @x = $c->(1); print "@x\n";},
    "11 12 21 22 23\n");

test_cl('(LIST) x $n in return list = string repeat in scalar context',
    q{my @a=(11,12); my @b=(21,22,23);
      my $c = sub { my ($n)=@_; return (@a, (@b) x $n) };
      my $s = $c->(2); print "$s\n";},
    "33\n");

# ── \(LIST) refgen distributes \ over the list (cross-cutting bug #5) ─────────
# In scalar context `\(LIST)` yields a ref to the LAST element (comma-operator
# semantics): a SCALAR ref, not an ARRAY ref.  `bless \(map ...), "C"` was a
# blessed ARRAY ref before this fix.
test_cl('\(map ...) in scalar context is a SCALAR ref to last element',
    q{my $c = \(map "$_", "p", "q"); print ref($c), " ", $$c, "\n";},
    "SCALAR q\n");

test_cl('bless \(map ...) is a blessed SCALAR ref',
    q{my $b = bless \(map "$_", "test"), "C"; print ref($b), " ", $$b, "\n";},
    "C test\n");

# In list context \(LIST) keeps one ref per element.
test_cl('\(map ...) in list context = one ref per element',
    q{my @r = \(map "$_", "a", "b"); print scalar(@r), " ", ref($r[0]), " ", ${$r[1]}, "\n";},
    "2 SCALAR b\n");

# ── wantarray-leak family: a wantarray-sensitive builtin in a scalar position
#    (my $x = …) must NOT inherit the enclosing sub's list context. These all
#    failed before %WANTARRAY_SENSITIVE / _wrap_wantarray_ctx covered them. ────
test_cl('scalar each inside list-context sub = key',
    q{sub f { my %h=(a=>1); my $e = each %h; return "$e"; } my @r=(f()); print "$r[0]\n";},
    "a\n");

test_cl('scalar splice inside list-context sub = last removed elem',
    q{sub f { my @a=(10,20,30,40); my $s = splice(@a,1,2); return "$s"; } my @r=(f()); print "$r[0]\n";},
    "30\n");

test_cl('scalar readline inside list-context sub reads ONE line',
    q{open my $w,'>','/tmp/pcl_wal.dat'; print $w "L1\nL2\nL3\n"; close $w;
      sub f { open my $fh,'<','/tmp/pcl_wal.dat'; my $a=<$fh>; my $b=<$fh>; close $fh; return "$a$b"; }
      my @r=(f()); print $r[0]; unlink '/tmp/pcl_wal.dat';},
    "L1\nL2\n");

test_cl('scalar reverse inside list-context sub = reversed string',
    q{sub f { my $s = reverse("abc"); return "$s"; } my @r=(f()); print "$r[0]\n";},
    "cba\n");
