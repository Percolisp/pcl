#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-15.t — perf round 47 (s517c, docs/speed-review-s509.md §2
# "In round 47").  MECHANISM rows (the runtime's own answer) and ANSWER rows:
# programs whose expected output below IS perl's own (probed on 5.40.3;
# nothing in them needs perl >= 5.38).
#
#   #3221 print / say / printf are on Parser2's %TOPIC_RAW_WORD, so a
#         `for (A..B)` topic loop whose body prints binds $_ to the RAW
#         counter (p-foreach-range-raw); the residue -- a TIED handle's
#         PRINT that WRITES the global $_ -- is shown, and PCL_OPT=-raw-topic
#         restores perl's answer.
#   #3121 a bracket class of plain characters / ranges is ONE 256-bit
#         table test plus its high ranges (%pcl-scanner-tree rewrites it to (:property FN)
#         in the parse tree; cl-ppcre itself is untouched).
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

sub transpile { my ($src) = @_; return PCLCore::transpile(qq{$pl2cl } . src_file($src)) }

sub run_pl {
    my ($src) = @_;
    my $cl = transpile($src);
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
# #3221 MECHANISM: a topic loop whose body is print / say / printf binds $_
# raw; a bareword handle (a Word the allowlist does not know) keeps the box.
{
    my $cl = transpile(<<'END_PL');
use feature 'say';
my $buf = '';
open my $fh, '>', \$buf or die;
print {$fh} $_ for 1..3;
say {$fh} $_ for 4..5;
printf {$fh} "%d,", $_ for 6..8;
print STDOUT $_ for 1..2;
END_PL
    like($cl, qr/\(p-foreach-range-raw \(\$_ 1 3\) \(p-print :fh \$fh \$_\)\)/,
         '#3221 print {$fh} $_ for A..B binds $_ raw');
    like($cl, qr/\(p-foreach-range-raw \(\$_ 4 5\) \(p-say :fh \$fh \$_\)\)/,
         '#3221 say {$fh} $_ for A..B binds $_ raw');
    like($cl, qr/\(p-foreach-range-raw \(\$_ 6 8\) \(p-printf :fh \$fh/,
         '#3221 printf {$fh} FMT, $_ for A..B binds $_ raw');
    like($cl, qr/\(p-foreach-range \(\$_ 1 2\)/,
         '#3221 a bareword handle (STDOUT) keeps the boxed loop');
}

# #3221 ANSWERS (perl 5.40.3's output): plain handles, an in-memory handle,
# and an operand with a `""` overload (the handler runs, the value prints).
answers(<<'END_PL', <<'END_OUT', '#3221 print/say/printf in a raw topic loop = perl');
use feature 'say';
my $buf = '';
open my $fh, '>', \$buf or die;
print {$fh} $_ for 1..3;
print $fh "\n";
say {$fh} $_ for 4..5;
printf {$fh} "%d,", $_ for 6..8;
close $fh;
print $buf, "\n";
say $_ for 1..2;
printf "<%s>", $_ for 3..4; print "\n";
package O; use overload '""' => sub { "O(" . $_[0][0] . ")" }; sub new { bless [$_[1]] }
package main;
my @o = map { O->new($_) } 0..2;
print $o[$_], " " for 0..2; print "|\n";
END_PL
123
4
5
6,7,8,
1
2
<3><4>
O(0) O(1) O(2) |
END_OUT

# #3221 a tied handle's PRINT that READS the global $_ sees the current
# element (perl's answer).
answers(<<'END_PL', <<'END_OUT', '#3221 tied PRINT reading the global $_ = perl');
package T; sub TIEHANDLE { bless {} } sub PRINT { shift; print STDOUT "[PRINT sees \$_=$_ arg=@_]"; 1 }
package main;
tie *FH, 'T';
$_ = "out"; my $fh = \*FH;
print {$fh} $_ for 1..3;
print "\n";
END_PL
[PRINT sees $_=1 arg=1][PRINT sees $_=2 arg=2][PRINT sees $_=3 arg=3]
END_OUT

# #3221 THE RESIDUE, made visible (docs/not-supported.md "A raw-topic
# `for (A..B)` loop"): a tied PRINT that WRITES the global $_.  perl prints
# <9><9><9>; under the default PCL's raw loop prints <1><2><3>, and
# PCL_OPT=-raw-topic restores perl's answer.  If the first row ever starts
# printing perl's answer, the residue is gone: update not-supported.md.
{
    my $src = <<'END_PL';
package T; sub TIEHANDLE { bless {} } sub PRINT { shift; $_ = 9; 1 }
package main;
tie *FH, 'T';
my $fh = \*FH;
for (1..3) { print {$fh} $_; print "<$_>"; }
print "\n";
END_PL
    is(run_pl($src), "<1><2><3>\n",
       '#3221 residue: tied PRINT writing $_ is not seen by the raw loop (perl: <9><9><9>)');
    local $ENV{PCL_OPT} = '-raw-topic';
    is(run_pl($src), "<9><9><9>\n",
       '#3221 residue: PCL_OPT=-raw-topic restores perl\'s <9><9><9>');
}

# ─────────────────────────────────────────────────────────────────────────────
# #3121 MECHANISM: %pcl-scanner-tree rewrites a bracket class of plain
# characters / ranges to ONE table test, (:property FN) (a table below 256,
# its high ranges walked above); a class with a symbol class or any /i (option or inline) keeps
# cl-ppcre's own class (NIL = compile the text as it is).
like(lisp_out(<<'END_LISP'),
(defun shape (p &rest o)
  (let ((tr (%pcl-scanner-tree p o)))
    (cond ((null tr) "TEXT")
          ((search "property" (string-downcase (prin1-to-string tr))) "TABLE")
          (t "TREE"))))
(format t "R:~{~A~^,~}~%"
        (list (shape "[abc]") (shape "[^a-z0-9]") (shape "x[\\x22\\x5c\\n]y")
              (shape "[\\d\\s]") (shape (format nil "[~Aa]" (code-char 256)))
              (shape "[abc]" :case-insensitive-mode t) (shape "(?i)[abc]")
              (shape "(?i:x)[abc]") (shape "abc")))
(let* ((tr (%pcl-scanner-tree "[^a-c]" nil))
       (fn (second (second tr))))
  (format t "T:~{~A~^,~}~%"
          (mapcar (lambda (c) (if (funcall fn c) 1 0))
                  (list #\a #\c #\d (code-char 255) (code-char 256) (code-char 9786)))))
(let* ((tr (%pcl-scanner-tree (format nil "[a~A-~A]" (code-char 9786) (code-char 9787)) nil))
       (fn (second (second tr))))
  (format t "H:~{~A~^,~}~%"
          (mapcar (lambda (c) (if (funcall fn c) 1 0))
                  (list #\a #\b (code-char 9785) (code-char 9786) (code-char 9787) (code-char 9788)))))
END_LISP
    qr/R:TABLE,TABLE,TABLE,TEXT,TABLE,TEXT,TEXT,TEXT,TEXT\nT:0,0,1,1,1,1\nH:1,0,0,1,1,0/,
    '#3121 the class table: which classes are rewritten, and the inverted test above 255');

# #3121 ANSWERS (perl 5.40.3's output): classes on and off the table path,
# s///g / split / tr-free substitution over them, JSON::PP's escape class.
answers(<<'END_PL', <<'END_OUT', '#3121 bracket classes on and off the table path = perl');
binmode STDOUT, ':utf8';
my $s = "abc XYZ 012 \t\n\x01\x1f \x{e9}\x{ff}\x{100}\x{263a}-_]^\\\"";
my @p = (
  qr/[abc]/, qr/[^abc]/, qr/[a-z]/, qr/[\x00-\x1f]/, qr/[\d\s]/,
  qr/[\x{100}a]/, qr/[a-c]/i, qr/[^a-c]/i, qr/[^\x00-\x{10FFFF}]/, qr/[\x80-\xff]/,
  qr/[^\x80-\xff]/, qr/[-_\]^]/, qr/[\\"]/, qr/[\x22\x5c\n\r\t\f\b]/, qr/(?i)[x-z]/,
  qr/(?i:[x-z])[0-2]/, qr/[\w]/, qr/[^\W\d]/, qr/[\x{263a}]/, qr/[\x{ff}]/,
  qr/[a][b][c]/, qr/[^\n]+/, qr/(?x) [ a b ] /,
);
for my $i (0 .. $#p) {
  my @m = $s =~ /$p[$i]/g;
  print "$i: ", scalar(@m), " [", join("|", map { sprintf "%vx", $_ } @m), "]\n";
}
(my $t = "a\"b\\c\nd\te") =~ s/([\x22\x5c\n\r\t\f\b])/sprintf("\\u%04x", ord $1)/ge;
print "esc: $t\n";
(my $u = "x\x01y\x1fz") =~ s/([\x00-\x08\x0b\x0e-\x1f])/sprintf("<%02x>", ord $1)/eg;
print "ctl: $u\n";
my $n = () = "aXbYc" =~ /[^A-Z]/g; print "inv: $n\n";
print "split: ", join(",", split /[,;]/, "a,b;c"), "\n";
print "subst: ", ("hello world" =~ s/[lo]/_/gr), "\n";
END_PL
0: 3 [61|62|63]
1: 24 [20|58|59|5a|20|30|31|32|20|9|a|1|1f|20|e9|ff|100|263a|2d|5f|5d|5e|5c|22]
2: 3 [61|62|63]
3: 4 [9|a|1|1f]
4: 9 [20|20|30|31|32|20|9|a|20]
5: 2 [61|100]
6: 3 [61|62|63]
7: 24 [20|58|59|5a|20|30|31|32|20|9|a|1|1f|20|e9|ff|100|263a|2d|5f|5d|5e|5c|22]
8: 0 []
9: 2 [e9|ff]
10: 25 [61|62|63|20|58|59|5a|20|30|31|32|20|9|a|1|1f|20|100|263a|2d|5f|5d|5e|5c|22]
11: 4 [2d|5f|5d|5e]
12: 2 [5c|22]
13: 4 [9|a|5c|22]
14: 3 [58|59|5a]
15: 0 []
16: 13 [61|62|63|58|59|5a|30|31|32|e9|ff|100|5f]
17: 10 [61|62|63|58|59|5a|e9|ff|100|5f]
18: 1 [263a]
19: 1 [ff]
20: 1 [61.62.63]
21: 2 [61.62.63.20.58.59.5a.20.30.31.32.20.9|1.1f.20.e9.ff.100.263a.2d.5f.5d.5e.5c.22]
22: 6 [61|62|20|20|20|20]
esc: a\u0022b\u005cc\u000ad\u0009e
ctl: x<01>y<1f>z
inv: 3
split: a,b,c
subst: he___ w_r_d
END_OUT

done_testing();
