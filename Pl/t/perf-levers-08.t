#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-08.t — perf round 39 (s507p, docs/faster-codegen-suggestions.md
# §0.2w).  RUNTIME levers: the generated code is unchanged, so each lever's
# MECHANISM row reads the loaded runtime (it fails on the round's base), and
# its ANSWER rows run a program and compare with perl 5.40.3's own output
# (probed; the expected text below IS perl's).
#
#   #2539 one predicate for box-set's two pos() resets (%p-clear-match-pos):
#         a store asks the table only when some pos() exists;
#   #2637 %p-tied refuses a NON-EMPTY container before the weak tie table
#         (a tied container is an empty shell), so one live tie no longer
#         costs every untied container a weak-table lookup.
#   #2111 an in-memory filehandle owns a PRIVATE buffer; its scalar answers
#         simple-string snapshots (a :memfh magic cell), so no later print
#         changes a copy, an element, a hash value or a hash key.
#   #2115 (a) a readline record is a simple string; (c) `.=` on a scalar,
#         a hash / array element or a deref element appends IN PLACE once the
#         string is long (a :strbuf cell answering snapshots), so 200k
#         appends are linear and no copy, key, element or closure changes.
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
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub src_file {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}

sub run_pl {
    my ($src) = @_;
    my $file = src_file($src);
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# Every line of EXPECTED must appear, whole, in OUT, in order and alone.
sub answers {
    my ($src, $expected, $what) = @_;
    is(run_pl($src), $expected, $what);
}

# The runtime's own answers to a few forms (the MECHANISM rows).
sub lisp_out {
    my ($forms) = @_;
    my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $lfh "(in-package :pcl)\n$forms";
    close $lfh;
    return scalar `sbcl @sbcl_rt --load $lfile 2>&1`;
}

# ─────────────────────────────────────────────────────────────────────────────
# MECHANISM (runtime reads)
# ─────────────────────────────────────────────────────────────────────────────
my $mech = lisp_out(<<'LISP');
(format t "clearpos ~a~%" (and (fboundp '%p-clear-match-pos) t))
(format t "tiedshape ~a~%"
  (and (search "%p-tie-shell-shape-p"
               (string-downcase (prin1-to-string (macroexpand-1 '(%p-tied x)))))
       t))
(format t "memfh ~a~%"
  (let* ((b (make-p-box "x")) (fh (make-p-box *p-undef*)))
    (%p-open-memory fh ">" b)
    (let ((c (p-box-value b)))
      (list (and (p-magic-cell-p c) (p-magic-cell-kind c))
            (simple-string-p (unbox b))))))
(format t "shapes ~a~%"
  (list (%p-tie-shell-shape-p (make-hash-table))
        (%p-tie-shell-shape-p (let ((h (make-hash-table))) (setf (gethash :__class__ h) "C") h))
        (%p-tie-shell-shape-p (let ((h (make-hash-table :test 'equal)))
                                (setf (gethash "a" h) 1 (gethash "b" h) 2) h))
        (%p-tie-shell-shape-p (make-array 0 :adjustable t :fill-pointer 0))
        (%p-tie-shell-shape-p (make-array 2 :adjustable t :fill-pointer 2))
        (%p-tie-shell-shape-p "")
        (%p-tie-shell-shape-p (make-p-box 1))))
(format t "records ~a~%"
  (list (simple-string-p (%p-read-record (make-string-input-stream (format nil "ab~%cd")) (string #\Newline)))
        (simple-string-p (%p-read-record (make-string-input-stream "abXYcd") "XY"))
        (simple-string-p (%p-read-record (make-string-input-stream "abcd") nil))))
(format t "strbuf ~a~%"
  (let* ((b (make-p-box (make-string 300 :initial-element #\a))))
    (%p-append-box b "x")
    (%p-append-box b "y")
    (list (p-magic-cell-kind (p-box-value b)) (simple-string-p (unbox b)) (length (unbox b))
          (p-magic-cell-p (p-box-value (%p-append-box (make-p-box "s") "t"))))))
(format t "livelen ~a~%"
  (let* ((b (make-p-box "")) (fh (make-p-box *p-undef*)))
    (%p-open-memory fh ">" b)
    (let ((s (p-get-stream fh)))
      (write-string "abcd" s)
      (let ((n (p-length b)))
        (list n (null (psos-snap s)) (p-length (let ((x (make-p-box (make-string 300 :initial-element #\z)))) (%p-append-box x "q"))))))))
LISP
like($mech, qr/^clearpos T$/mi,
     '#2539: box-set\'s two pos() resets share %p-clear-match-pos');
like($mech, qr/^tiedshape T$/mi,
     '#2637: %p-tied asks the shell-shape test before the weak tie table');
like($mech, qr/^memfh \(MEMFH T\)$/mi,
     '#2111: a writable in-memory handle owns its buffer; the scalar holds a :memfh cell answering a simple-string snapshot');
like($mech, qr/^shapes \(T T NIL T NIL NIL NIL\)$/mi,
     '#2637: only an empty vector or a hash of at most one entry can be a tied shell');
like($mech, qr/^records \(T T T\)$/mi,
     '#2115 (a): a readline record is a SIMPLE string (line, multi-char separator, slurp), so a store keeps it without a snapshot');
like($mech, qr/^strbuf \(STRBUF T 302 NIL\)$/mi,
     '#2115 (c): a long string appended with .= lives in a :strbuf cell whose reads are simple snapshots; a short one stays plain');
like($mech, qr/^livelen \(4 T 301\)$/mi,
     '#2111 / #2115: length() of a :memfh or :strbuf scalar reads the live buffer and takes no snapshot');

# ─────────────────────────────────────────────────────────────────────────────
# ANSWERS (perl's)
# ─────────────────────────────────────────────────────────────────────────────
answers(<<'PERL', <<'EXPECTED', '#2539: a store resets pos() on both store arms');
my $s = "aaa"; $s =~ /a/g; print "p1 ", pos($s), "\n";
$s = "bbbb"; print "fast ", (defined pos($s) ? "def" : "undef"), "\n";
$s =~ /b/g; $s =~ /b/g; print "p2 ", pos($s), "\n";
my $t = "cc"; $s = $t; print "general ", (defined pos($s) ? "def" : "undef"), "\n";
$s =~ /c/g; my $r = [1]; $s = $r; print "ref ", (defined pos($s) ? "def" : "undef"), "\n";
my $u = "zz"; my $n = 0; $n++ while $u =~ /z/g; print "loop $n\n";
my $w = "abab"; my @p; while ($w =~ /b/g) { push @p, pos($w) } $w = "x"; print "w @p ", (defined pos($w) ? "def" : "undef"), "\n";
PERL
p1 1
fast undef
p2 2
general undef
ref undef
loop 2
w 2 4 undef
EXPECTED

answers(<<'PERL', <<'EXPECTED', '#2637: untied containers beside a live tie, and the tied ones');
require Tie::Hash; require Tie::Array;
my %one = (a => 1); my %empty; my @emp; my @full = (1, 2);
my $obj = bless {}, 'Foo'; my $obj1 = bless { x => 1 }, 'Foo';
my %pre = (h => 9); tie my %t, 'Tie::StdHash'; $t{x} = 1; $t{y} = 2;
tie my @ta, 'Tie::StdArray'; push @ta, 3, 4;
tie %pre, 'Tie::StdHash';
print "t ", join(",", map { "$_=$t{$_}" } sort keys %t), "\n";
print "ta ", join(",", @ta), " n=", scalar(@ta), "\n";
print "pre tied ", (exists $pre{h} ? "has-h" : "no-h"), " n=", scalar(keys %pre), "\n";
$pre{z} = 3; print "pre z ", $pre{z}, "\n";
$one{b} = 2; print "one ", join(",", sort keys %one), "\n";
$empty{q} = 1; print "empty ", join(",", keys %empty), "\n";
push @emp, 5; print "emp @emp\n"; push @full, 3; print "full @full\n";
my @c = @full; my %c = %one; print "copies ", scalar(@c), " ", scalar(keys %c), "\n";
$obj->{k} = 1; print "obj ", ref($obj), " ", join(",", keys %$obj), "\n";
print "obj1 ", $obj1->{x}, "\n";
print "tied? ", (tied(%t) ? 1 : 0), (tied(%one) ? 1 : 0), (tied(@ta) ? 1 : 0), (tied(@emp) ? 1 : 0), (tied(%empty) ? 1 : 0), "\n";
my $bt = bless {}, 'Bar'; tie %$bt, 'Tie::StdHash'; $bt->{m} = 4; print "blessed tied ", ref($bt), " ", $bt->{m}, " ", (tied(%$bt) ? 1 : 0), "\n";
untie %pre; print "pre after untie ", join(",", map { "$_=$pre{$_}" } sort keys %pre), "\n";
untie @ta; print "ta after untie n=", scalar(@ta), "\n";
tie my @ta2, 'Tie::StdArray'; @ta2 = (1, 2, 3); print "ta2 ", shift(@ta2), " ", scalar(@ta2), "\n";
PERL
t x=1,y=2
ta 3,4 n=2
pre tied no-h n=0
pre z 3
one a,b
empty q
emp 5
full 1 2 3
copies 3 2
obj Foo k
obj1 1
tied? 10100
blessed tied Bar 4 1
pre after untie h=9
ta after untie n=0
ta2 1 2
EXPECTED

answers(<<'PERL', <<'EXPECTED', '#2111: an in-memory handle\'s writes never reach a copy, an element, a hash value or KEY');
use strict; use warnings;
my $n = 0;
sub show { my ($tag, @v) = @_; $n++; print "$n $tag: ", join(" | ", map { defined $_ ? "[" . ($_ =~ s{\0}{0}gr) . "]" : "undef" } @v), "\n" }

# 1. the #2111 reproducer
{ my $buf = ""; open my $fh, ">", \$buf or die; print $fh "a";
  my $copy = $buf; my @arr = ($buf); my %h = (k => $buf); my %kk; $kk{$buf} = 1;
  print $fh "b"; close $fh;
  show("repro", $copy, $arr[0], $h{k}, join(",", keys %kk), (exists $kk{"a"} ? "key-a" : "no-key-a"), $buf); }
# 2. every mode
for my $mode (">", ">>", "+<", "+>", "+>>") {
  my $buf = "XYZ"; open my $fh, $mode, \$buf or die "$mode: $!";
  print $fh "ab"; my $mid = $buf; print $fh "cd";
  show("mode $mode", $mid, $buf, tell($fh)); close $fh; show("mode $mode closed", $buf);
}
# 3. read mode
{ my $buf = "l1\nl2\n"; open my $fh, "<", \$buf or die; my $l = <$fh>; $buf = "changed\n"; my $l2 = <$fh>;
  show("read", $l, $l2, $buf); }
# 4. reads between prints: length, regex, substr, element, hash key, value
{ my $buf; open my $fh, ">", \$buf or die; my (@len, @m, @el, %hk, %hv);
  for my $i (1 .. 4) { print $fh "x$i"; push @len, length $buf; push @m, ($buf =~ /x(\d)$/ ? $1 : "-");
    push @el, $buf; $hk{$buf} = $i; $hv{$i} = $buf; }
  show("between", "@len", "@m", "@el", join(",", map { "$_=$hk{$_}" } sort keys %hk), join(",", map { "$_=$hv{$_}" } sort keys %hv));
  show("hk exists", (exists $hk{"x1"} ? 1 : 0), (exists $hk{"x1x2"} ? 1 : 0)); }
# 5. seek / tell / truncate
{ my $buf = ""; open my $fh, "+>", \$buf or die; print $fh "hello world";
  my $snap = $buf; seek($fh, 0, 0); print $fh "J"; show("seek", $snap, $buf, tell($fh));
  seek($fh, 0, 2); show("tell end", tell($fh)); seek($fh, 15, 0); print $fh "!"; (my $vis = $buf) =~ s/\0/0/g; show("gap", $vis, length $buf);
  truncate($fh, 3); ($vis = $buf) =~ s/\0/0/g; show("truncate", $vis, length $buf); }
# 6. a write through the scalar while the handle is open
{ my $buf = ""; open my $fh, ">", \$buf or die; print $fh "ab"; $buf .= "x"; print $fh "c"; show("dot-eq", $buf);
  $buf = ""; print $fh "d"; (my $vis = $buf) =~ s/\0/0/g; show("assign-empty", $vis, length $buf);
  $buf = "QQQQQQ"; print $fh "e"; show("assign-long", $buf); my $c = $buf; print $fh "f"; show("copy after", $c, $buf); }
# 7. close and reopen
{ my $buf = ""; open my $fh, ">", \$buf or die; print $fh "one"; close $fh; my $c1 = $buf;
  open $fh, ">>", \$buf or die; print $fh "two"; my $c2 = $buf; print $fh "three"; close $fh;
  show("reopen", $c1, $c2, $buf); }
# 8. references, ref(), defined, numeric
{ my $buf; open my $fh, ">", \$buf or die; print $fh "42"; my $r = \$buf; show("ref", ref($r), $$r, defined($buf) ? 1 : 0, $buf + 1);
  my $sref = $r; print $fh "7"; show("ref after", $$sref, $buf * 2); }
# 9. printf / say / syswrite / write via select
{ my $buf = ""; open my $fh, ">", \$buf or die; printf $fh "%03d", 7; my $a = $buf; { local $\ = "\n"; print $fh "z" } my $b = $buf;
  my $old = select($fh); $| = 1; print "sel"; select($old); show("printf", $a, $b, $buf); }
# 10. two handles on one scalar
{ my $buf = ""; open my $f1, ">", \$buf or die; print $f1 "aaa"; open my $f2, ">>", \$buf or die; print $f2 "B"; print $f1 "c";
  show("two", $buf); }
# 11. pushed onto an array, sorted, joined, interpolated, passed to a sub that keeps it
{ my @keep; sub keep { push @keep, $_[0]; my $x = $_[0]; push @keep, $x } my $buf = ""; open my $fh, ">", \$buf or die;
  print $fh "p"; keep($buf); my $s = "<$buf>"; my $j = join("-", $buf, $buf); print $fh "q";
  show("kept", @keep, $s, $j, $buf); }
# 12. chomp / chop / s/// / tr on the scalar while open
{ my $buf = ""; open my $fh, ">", \$buf or die; print $fh "line\n"; chomp $buf; print $fh "X"; show("chomp", $buf);
  $buf =~ s/l/L/; print $fh "Y"; show("subst", $buf); (my $t = $buf) =~ tr/a-z/A-Z/; show("tr", $t, $buf); }
# 13. a closure capturing the scalar, a returned value
{ my $buf = ""; open my $fh, ">", \$buf or die; my $get = sub { $buf }; print $fh "c1"; my $v1 = $get->();
  sub ret { return $_[0] } my $rv = ret($buf); print $fh "c2"; show("closure", $v1, $rv, $get->()); }
our $g = ""; { open my $fh, ">", \$g or die; print $fh "g1"; { local $g = "L"; show("local inside", $g) } print $fh "g2"; show("local after", $g); }
PERL
1 repro: [a] | [a] | [a] | [a] | [key-a] | [ab]
2 mode >: [ab] | [abcd] | [4]
3 mode > closed: [abcd]
4 mode >>: [XYZab] | [XYZabcd] | [7]
5 mode >> closed: [XYZabcd]
6 mode +<: [abZ] | [abcd] | [4]
7 mode +< closed: [abcd]
8 mode +>: [ab] | [abcd] | [4]
9 mode +> closed: [abcd]
10 mode +>>: [XYZab] | [XYZabcd] | [7]
11 mode +>> closed: [XYZabcd]
12 read: [l1
] | [nged
] | [changed
]
13 between: [2 4 6 8] | [1 2 3 4] | [x1 x1x2 x1x2x3 x1x2x3x4] | [x1=1,x1x2=2,x1x2x3=3,x1x2x3x4=4] | [1=x1,2=x1x2,3=x1x2x3,4=x1x2x3x4]
14 hk exists: [1] | [1]
15 seek: [hello world] | [Jello world] | [1]
16 tell end: [11]
17 gap: [Jello world0000!] | [16]
18 truncate: [Jello world0000!] | [16]
19 dot-eq: [abc]
20 assign-empty: [000d] | [4]
21 assign-long: [QQQQeQ]
22 copy after: [QQQQeQ] | [QQQQef]
23 reopen: [one] | [onetwo] | [onetwothree]
24 ref: [SCALAR] | [42] | [1] | [43]
25 ref after: [427] | [854]
26 printf: [007] | [007z
] | [007z
sel]
27 two: [aaac]
28 kept: [p] | [p] | [<p>] | [p-p] | [pq]
29 chomp: [line0X]
30 subst: [Line0XY]
31 tr: [LINE0XY] | [Line0XY]
32 closure: [c1] | [c1] | [c1c2]
33 local inside: [L]
34 local after: [g1g2]
EXPECTED

answers(<<'PERL', <<'EXPECTED', '#2115 (c): copies, keys, elements, closures, pos, local and evaluation order around an in-place .=');
my $B = "b" x 250; our ($g); my $n = 0;
sub show { my ($tag, @v) = @_; $n++; print "$n $tag: ", join(" | ", map { !defined $_ ? "undef" : length($_) > 20 ? length($_) . ":" . substr($_, -4) : "[$_]" } @v), "\n" }
$g = $B; $g .= "1"; my $c2 = $g; $g .= "2"; show("pkg copy", $c2, $g);
my %h = (k => $B); $h{k} .= "1"; my $c3 = $h{k}; $h{k} .= "2"; show("helem copy", $c3, $h{k});
my @a = ($B); $a[0] .= "1"; my $c4 = $a[0]; $a[0] .= "2"; show("aelem copy", $c4, $a[0]);
my $o = { buf => $B }; $o->{buf} .= "1"; my $c5 = $o->{buf}; $o->{buf} .= "2"; show("deref copy", $c5, $o->{buf});
my %k; $g = $B; $g .= "x"; $k{$g} = 1; push my @keep, $g; $g .= "y"; show("key push", (exists $k{$B . "x"} ? "yes" : "no"), $keep[0], $g);
my $get = sub { $g }; $g .= "c1"; my $v1 = $get->(); $g .= "c2"; show("closure", $v1, $get->());
$g = "a" x 220; $g .= "ab"; $g =~ /a/g; my $p1 = pos($g); $g .= "c"; show("pos", $p1, pos($g));
$g = $B; $g .= "s"; $g .= $g; show("self append", $g);
sub gb { $g = $B; "W" } $g = "short"; $g .= gb(); show("order", $g);
$g = $B; $g .= "L"; { local $g = "in"; $g .= "side"; show("local in", $g); } $g .= "M"; show("local out", $g);
$o->{buf} .= "end\n"; chomp $o->{buf}; $o->{buf} =~ s/d$/D/; show("chomp s///", $o->{buf}, ref(\$o->{buf}));
my $d = delete $h{k}; show("delete", $d, scalar(keys %h));
PERL
1 pkg copy: 251:bbb1 | 252:bb12
2 helem copy: 251:bbb1 | 252:bb12
3 aelem copy: 251:bbb1 | 252:bb12
4 deref copy: 251:bbb1 | 252:bb12
5 key push: [yes] | 251:bbbx | 252:bbxy
6 closure: 254:xyc1 | 256:c1c2
7 pos: [1] | undef
8 self append: 502:bbbs
9 order: 251:bbbW
10 local in: [inside]
11 local out: 252:bbLM
12 chomp s///: 255:2enD | [SCALAR]
13 delete: 252:bb12 | [0]
EXPECTED

done_testing();
