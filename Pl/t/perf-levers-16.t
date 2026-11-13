#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-16.t — s540d, the runtime / compiler half of the low-hanging
# speed list (docs/speed-low-hanging-review-s540.md).  MECHANISM rows (the
# runtime's or the compiler's own answer: each FAILS on the base 24988e35,
# where the mechanism does not exist) and ANSWER rows: programs whose
# expected output below IS perl's own (probed on 5.40.3; nothing in them
# needs perl >= 5.38).
#
#   #4981 tr/// is EXPANDED ONCE PER OP: the op (one per site, %p-op-once)
#         carries its expansion in %compiled after the first use; the /c
#         complement keeps its distinct search codepoints sorted once.
#   #4983 $| is read through a lock-free MIRROR of the weak autoflush table
#         (a weak hash table is a synchronized one in SBCL).
#   #4984 a `{...}` constructor's table is MADE for its pair count.
#   #4982 a LITERAL `use constant` is folded at its uses (Kind-A
#         `const-fold`): the literal, not a call.  &NAME, ->NAME, a list or
#         non-literal constant and an imported one stay calls, and
#         PCL_OPT=-const-fold runs identically.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
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

sub transpile {
    my ($src, $env) = @_;
    return PCLCore::transpile(($env // '') . qq{ $pl2cl } . src_file($src));
}

sub run_pl {
    my ($src, $env) = @_;
    my $cl = transpile($src, $env);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub answers {
    my ($src, $expected, $what, $env) = @_;
    is(run_pl($src, $env), $expected, $what);
}

# The runtime's own answers to a few forms (the MECHANISM rows).
sub lisp_out {
    my ($forms) = @_;
    my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $lfh "(in-package :pcl)\n$forms";
    close $lfh;
    my $out = `sbcl @sbcl_rt --load $lfile 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# MECHANISM, all four runtime levers in one image.
#   #4981: the site's op is empty before its first use and holds its
#          expansion after it; the SAME op serves the next call.  A /c op's
#          sorted search list holds each codepoint once (the O(n^2)
#          remove-duplicates is gone) and the Latin-1 table answers the
#          complement's rank ('d' is the 97th codepoint not in {a,b,c}).
#   #4984: a 20-pair constructor's table is the size a table made for 20 is.
#   #4983: one marked stream is in a one-entry mirror; another is not; a
#          DIRECT table write (cl/pcl-test.lisp's) is seen through the count
#          check; more than 8 marked streams leave the vector out and the
#          table answers; unmarking all of them answers NIL again.
{
    my $out = lisp_out(<<'END_LISP');
(defun s540d-op () (p-tr :from "a-z" :to "A-Z" :flags ""))
(let ((op (s540d-op)))
  (format t "fresh-empty ~A~%" (null (p-tr-op-%compiled op)))
  (format t "count ~A~%" (do-tr (make-p-box "abc") op))
  (format t "same-op ~A compiled ~A~%" (eq op (s540d-op))
          (p-tr-compiled-p (p-tr-op-%compiled op))))
(let ((tc (%p-tr-compiled (%p-tr-parts "aab-cb" "" "c"))))
  (format t "sorted ~A rank-d ~A identical ~A~%" (%trc-sorted-from tc)
          (svref (%trc-latin tc) 100) (%trc-identical-p tc)))
(let ((pairs (loop for i below 20 collect (format nil "k~D" i) collect i)))
  (format t "size ~A~%" (= (hash-table-size (apply #'p-hash pairs))
                           (hash-table-size (make-hash-table :test 'equal :size 20)))))
(let ((s1 (make-string-output-stream)) (s2 (make-string-output-stream)))
  (%p-autoflush-mark s1 t)
  (format t "on ~A off ~A~%" (and (%p-autoflush-p s1) t) (and (%p-autoflush-p s2) t))
  (format t "mirror ~A ~A~%" (car **p-autoflush-mirror**)
          (length (cdr **p-autoflush-mirror**)))
  (setf (gethash s2 *p-autoflush-handles*) t)
  (format t "direct ~A~%" (and (%p-autoflush-p s2) t))
  (let ((more (loop repeat 8 collect (make-string-output-stream))))
    (dolist (s more) (%p-autoflush-mark s t))
    (format t "ten ~A vec ~A last ~A~%" (hash-table-count *p-autoflush-handles*)
            (cdr (%p-autoflush-remirror)) (and (%p-autoflush-p (car (last more))) t))
    (dolist (s (list* s1 s2 more)) (%p-autoflush-mark s nil))
    (format t "cleared ~A ~A~%" (hash-table-count *p-autoflush-handles*)
            (and (%p-autoflush-p s1) t))))
END_LISP
    like($out, qr/^fresh-empty T\ncount 3\nsame-op T compiled T$/mi,
         '#4981 a tr site\'s op is expanded on its first use and kept for the next');
    like($out, qr/^sorted #\(97 98 99\) rank-d 97 identical T$/mi,
         '#4981 a /c op keeps its distinct search codepoints sorted once; the Latin-1 table answers the rank');
    like($out, qr/^size T$/mi,
         '#4984 a 20-pair {...} table is made for 20 pairs (not grown from the default)');
    like($out, qr/^on T off NIL\nmirror 1 1\ndirect T$/mi,
         '#4983 $| is read through the mirror; a direct table write is seen through the count');
    like($out, qr/^ten 10 vec NIL last T\ncleared 0 NIL$/mi,
         '#4983 more than 8 marked streams: the table answers; cleared answers NIL')
        or diag $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# #4981 ANSWERS: every flag combination on ASCII, Latin-1 and wide strings, a
# count in a loop (the cached op used many times), map, the topic, and a
# 100,000-character /c count.  Expected = perl 5.40.3's output, probed.
my $tr_expected = <<'END_OUT';
count=10 t=hello world | up=10 t=HELLO WORLD | cnt-c=1 | c=1 t=hello_world | cd=1 t=helloworld | s=10 t=helo world | d=0 t=hello world | short=0 t=hello world | cs=1 t=hello_world | r=HELLO WORLD t=hello world | wide=0 | hi=0 | smile=0 t=hello world | hic=11 | y=5 t=he001 w1r0d
count=9 t=aaabbbccc | up=9 t=AAABBBCCC | cnt-c=0 | c=0 t=aaabbbccc | cd=0 t=aaabbbccc | s=9 t=abc | d=9 t=xxxyyy | short=9 t=xxxyyyyyy | cs=0 t=aaabbbccc | r=AAABBBCCC t=aaabbbccc | wide=0 | hi=0 | smile=0 t=aaabbbccc | hic=9 | y=0 t=aaabbbccc
count=8 t=Ünïcödé ☺ smile | up=8 t=ÜNïCöDé ☺ SMILE | cnt-c=7 | c=7 t=_n_c_d____smile | cd=7 t=ncdsmile | s=8 t=Ünïcödé ☺ smile | d=1 t=Ünïödé ☺ smile | short=1 t=Ünïyödé ☺ smile | cs=7 t=_n_c_d_smile | r=ÜNïCöDé ☺ SMILE t=Ünïcödé ☺ smile | wide=1 | hi=1 | smile=1 t=Ünïcödé X smile | hic=15 | y=1 t=Ünïcödé ☺ smi0e
count=0 t= | up=0 t= | cnt-c=0 | c=0 t= | cd=0 t= | s=0 t= | d=0 t= | short=0 t= | cs=0 t= | r= t= | wide=0 | hi=0 | smile=0 t= | hic=0 | y=0 t=
count=4 t=a-b-c\d | up=4 t=A-B-C\D | cnt-c=3 | c=3 t=a_b_c_d | cd=3 t=abcd | s=4 t=a-b-c\d | d=3 t=x-y-\d | short=3 t=x-y-y\d | cs=3 t=a_b_c_d | r=A-B-C\D t=a-b-c\d | wide=0 | hi=0 | smile=0 t=a-b-c\d | hic=7 | y=0 t=a-b-c\d
count=0 t=\0\x01ÿĀ | up=0 t=\0\x01ÿĀ | cnt-c=4 | c=4 t=____ | cd=4 t= | s=0 t=\0\x01ÿĀ | d=0 t=\0\x01ÿĀ | short=0 t=\0\x01ÿĀ | cs=4 t=_ | r=\0\x01ÿĀ t=\0\x01ÿĀ | wide=1 | hi=1 | smile=0 t=\0\x01ÿĀ | hic=3 | y=0 t=\0\x01ÿĀ
loop 76
ab cd
ret t 2
none <0>
topic 5
long 1
END_OUT
# The sixth input begins with NUL and \x01, written as the text \0\x01 above.
$tr_expected =~ s/\\0\\x01/\0\x01/g;
answers(<<'END_PL', $tr_expected, '#4981 tr/// flag combinations, ranges, wide characters = perl');
binmode STDOUT, ':utf8';
my @in = ("hello world", "aaabbbccc", "\x{dc}n\x{ef}c\x{f6}d\x{e9} \x{263A} smile", "", "a-b-c\\d", "\0\x01\xff\x{100}");
for my $s (@in) {
  my @r;
  { my $t = $s; my $n = ($t =~ tr/a-z//); push @r, "count=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/a-z/A-Z/); push @r, "up=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/a-zA-Z//c); push @r, "cnt-c=$n" }
  { my $t = $s; my $n = ($t =~ tr/a-zA-Z/_/c); push @r, "c=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/a-zA-Z//cd); push @r, "cd=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/a-z//s); push @r, "s=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/a-c/xy/d); push @r, "d=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/a-c/xy/); push @r, "short=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/a-zA-Z/_/cs); push @r, "cs=$n t=$t" }
  { my $t = $s; my $r = ($t =~ tr/a-z/A-Z/r); push @r, "r=$r t=$t" }
  { my $t = $s; my $n = ($t =~ tr/\0-\377//c); push @r, "wide=$n" }
  { my $t = $s; my $n = ($t =~ tr/\x{100}-\x{10ffff}//); push @r, "hi=$n" }
  { my $t = $s; my $n = ($t =~ tr/\x{263A}/X/); push @r, "smile=$n t=$t" }
  { my $t = $s; my $n = ($t =~ tr/\x{100}-\x{300}//c); push @r, "hic=$n" }
  { my $t = $s; my $n = ($t =~ y/lo/01/); push @r, "y=$n t=$t" }
  print join(" | ", @r), "\n";
}
my $x = "abcabc"; my $n = 0;
for my $w ("abcabc", "xyz", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") { $n += ($w =~ tr/abc//) }
print "loop $n\n";
my @l = map { (my $t = $_) =~ tr/a-z//cd; $t } "a1b2", "c3d4";
print "@l\n";
my $c = "aXbX"; my $v = ($c =~ tr/X//); print "ret ", ($v ? "t" : "f"), " $v\n";
my $none = "zzz"; my $w = ($none =~ tr/a//); print "none <$w>\n";
$_ = "topic"; my $t2 = tr/a-z//; print "topic $t2\n";
my $long = "x" x 100000 . "\x{100}"; my $k = ($long =~ tr/\0-\377//c); print "long $k\n";
END_PL

# ─────────────────────────────────────────────────────────────────────────────
# #4983 ANSWERS: $| on the selected handle, a select()ed file handle, local,
# ten marked handles (past the mirror's 8) and one cleared again, a write that
# must reach the file before -s reads it, and the 0/1 clamp.
{
    my $tmp = tempdir(CLEANUP => 1);
    (my $src = <<'END_PL') =~ s/__TMP__/$tmp/g;
my $f = "__TMP__/af";
print "start $|\n";
$| = 1; print "on $|\n";
open my $fh, '>', $f or die; my $old = select($fh); print STDOUT "fh-sel $|\n"; $| = 1; print STDOUT "fh-set $|\n"; select($old);
print "back $|\n";
{ local $| = 0; print "local $|\n"; } print "restored $|\n";
$| = 0; print "off $|\n";
my @h; for my $i (1 .. 10) { open my $g, '>', "$f.$i" or die; my $o = select($g); $| = 1; select($o); push @h, $g }
my $on = 0; for my $g (@h) { my $o = select($g); $on += $|; select($o) } print "ten $on\n";
my $o2 = select($h[9]); $| = 0; select($o2);
$on = 0; for my $g (@h) { my $o = select($g); $on += $|; select($o) } print "nine $on\n";
print $fh "x" x 10; my $sz = -s $f; print "flushed $sz\n";
close $_ for @h, $fh; unlink $f, map { "$f.$_" } 1 .. 10;
$| = 5; print "clamp $|\n"; $| = 0; --$|; print "toggle $|\n";
END_PL
    answers($src, <<'END_OUT', '#4983 $| per selected handle, local, ten handles, clamp = perl');
start 0
on 1
fh-sel 0
fh-set 1
back 1
local 0
restored 1
off 0
ten 10
nine 9
flushed 10
clamp 1
toggle 1
END_OUT
}

# #4984 ANSWERS: a constructor that SPREADS (a hash, an array) takes the
# copying path, a later key wins, an empty one, a 20-pair literal, nested.
answers(<<'END_PL', <<'END_OUT', '#4984 {...} constructors, spread and plain = perl');
my %h = (a => 1, b => 2);
my @kv = (c => 3);
my $r = { %h, x => 9, @kv, a => 7 };
print join(",", map { "$_=$r->{$_}" } sort keys %$r), "\n";
my $s = { map { ("k$_" => $_) } 1 .. 12 };
print scalar(keys %$s), " $s->{k12}\n";
my $e = {}; print scalar(keys %$e), "\n";
my $big = { 1 .. 40 }; print scalar(keys %$big), " $big->{39}\n";
my @objs = map { { id => $_, name => "n$_", tags => [$_], meta => { k => $_ } } } 1 .. 3;
print join(";", map { "$_->{id}:$_->{name}:$_->{tags}[0]:$_->{meta}{k}" } @objs), "\n";
END_PL
a=7,b=2,c=3,x=9
12 12
0
20 40
1:n1:1:1;2:n2:2:2;3:n3:3:3
END_OUT

# ─────────────────────────────────────────────────────────────────────────────
# #4982 THE CONSTANT FOLD.  One program with every shape the scope names:
# literal number / string / negative number, the hash form, a list constant,
# a reference constant, `&NAME`, `->NAME`, a list, a `!`, a hash init.
my $shapes = <<'END_PL';
use strict; use warnings;
use constant DEBUG => 0;
use constant NAME => 'abc';
use constant NEG => -2.5;
use constant { A1 => 1, B1 => "two", C1 => [1] };
use constant LIST => (1, 2);
my $t = DEBUG + DEBUG() + A1 + NEG;
print NAME, " ", B1, " $t ", &DEBUG, " ", main->DEBUG, " ", scalar(LIST), " ", C1->[0], "\n";
my @l = (DEBUG, NAME); print scalar(@l), "\n";
print "yes\n" if !DEBUG;
my %h = (DEBUG, 1); print join(",", keys %h), "\n";
package Other;
print defined(&Other::DEBUG) ? "def\n" : "undef\n";
END_PL
{
    my $cl = transpile($shapes);
    like($cl, qr/\(p-\+ \(p-\+ \(p-\+ 0 0\) 1\) -2\.5\)/,
         '#4982 NAME, NAME() and the hash form fold to their literals (a negative number too)');
    like($cl, qr/\(p-print "abc"\s+" "\s+"two"/, '#4982 a string constant folds to the string');
    like($cl, qr/\(p-amp-call \(pl-DEBUG \@_\)\)/, '#4982 &NAME stays a call');
    like($cl, qr/\(p-method-call \(p-resolve-invocant "main"\) "DEBUG"\)/,
         '#4982 a method call stays a call');
    like($cl, qr/\(p-scalar-ctx \(pl-LIST\)\)/, '#4982 a list constant stays a call');
    like($cl, qr/\(pl-C1\)/, '#4982 a reference constant stays a call');
    my $gen = transpile($shapes, 'PCL_OPT=-const-fold');
    like($gen, qr/\(p-scalar-ctx \(pl-DEBUG\)\)/, '#4982 PCL_OPT=-const-fold emits the call');
    unlike($gen, qr/\(p-\+ 0 0\)/, '... and no folded literal');
}
my $shapes_out = "abc two -1.5 0 0 2 1\n2\nyes\n0\nundef\n";
answers($shapes, $shapes_out, '#4982 the shapes program, folded = perl');
answers($shapes, $shapes_out, '#4982 the shapes program under PCL_OPT=-const-fold = perl',
        'PCL_OPT=-const-fold');

# THE RULED GUARD ROW (DECIDED ## s540): perl inlined the bare `PI` before
# the glob assignment, so it stays 3, while `&PI` and the method call find the
# new sub.  The general form (a call at every use) answered 9 9 9.
answers(<<'END_PL', "after glob redefine: 3 9 9\n", '#4982 a folded use keeps the literal after *PI = sub; &PI and ->PI see the new sub = perl');
use constant PI => 3;
no warnings;
*PI = sub () { 9 };
print "after glob redefine: ", PI, " ", &PI, " ", main->PI, "\n";
END_PL

# An IMPORTED constant stays a call: its value is not in this unit's text
# (Pl::Parser::_imported_record drops the fold).
{
    my $lib = tempdir(CLEANUP => 1);
    open my $fh, '>', "$lib/KC4982.pm" or die "KC4982.pm: $!";
    print $fh "package KC4982;\nuse constant KV => 5;\nuse Exporter 'import';\nour \@EXPORT = ('KV');\n1;\n";
    close $fh;
    my $cl = PCLCore::transpile("PERL5LIB=$lib $pl2cl " . src_file("use KC4982;\nmy \$v = KV + 1;\nprint \"\$v\\n\";\n"));
    like($cl, qr/\(p-\+ \(p-scalar-ctx \(pl-KV\)\) 1\)/, '#4982 an imported literal constant stays a call');
}

done_testing();
