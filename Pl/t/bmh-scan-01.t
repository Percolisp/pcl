#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# bmh-scan-01.t — literal-prefix regex scanning: Boyer-Moore-Horspool with a
# HASHED skip table (task #1461, s473s).
#
# cl-ppcre picks the scan for a pattern's constant prefix/suffix with
# `*use-bmh-matchers*`.  Its default (NIL) is the generic `search` — 11.5 % of
# the `textproc` bench row.  Its BMH branch is 6.6x faster on the isolated scan
# but allocates a DENSE array of *regex-char-code-limit* (1,114,112) fixnums —
# 8.5 MB PER LITERAL PATTERN, unbounded across a module load.  So PCL replaces
# `cl-ppcre::create-bmh-matcher` with the same algorithm over a 256-way table
# keyed on the low 8 bits of a character's code (2 KB/pattern) and turns the
# flag on.
#
# THE TWO THINGS THAT COULD GO WRONG, and the rows that catch them:
#   * a WRONG ANSWER.  Hashing collides characters, so a bucket takes the
#     MINIMUM skip over everything in it.  That is safe — BMH only ever
#     UNDER-skips, which costs re-tests, never a missed match — and section A
#     row 5 proves it by answering every case BOTH ways in one image (our
#     replacement honours the flag, so binding it NIL gives cl-ppcre's own
#     generic search as the oracle).  Section B asks the same question from
#     Perl, against real perl 5.40.3's answers.
#   * the DENSE TABLE COMING BACK.  If a future cl-ppcre inlined or
#     block-compiled its call, the redefinition would be invisible while the
#     flag stayed T, and cl-ppcre would allocate its own 8.5 MB table per
#     pattern — strictly worse than doing nothing.  The runtime self-tests for
#     that at load; row 3 asserts the self-test's own signal and row 4 measures
#     the heap directly (20 literal-prefix patterns: 8.5 MB each would be
#     170 MB).
#
# Inverse guard: on a tree without the change rows 1-3 fail (the flag is NIL,
# there is no %pcl-create-bmh-matcher, and nothing counts matchers).
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

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan skip_all => "sbcl not found"  if ! `which sbcl 2>/dev/null`;
plan tests => 6;

# -- A. the installed lever, in raw CL (one SBCL spawn) --------------------
my $LISP = <<'LISP';
(in-package :pcl)
(format t "1 ~A~%" (if cl-ppcre:*use-bmh-matchers* "on" "off"))
(format t "2 ~A~%" (if (eq (fdefinition 'cl-ppcre::create-bmh-matcher)
                           #'%pcl-create-bmh-matcher)
                       "ours" "theirs"))
;; the call must actually REACH us: build a scanner for a literal prefix and
;; watch the counter the runtime's own install-time self-test reads
(let ((before *pcl-bmh-matchers-built*))
  (cl-ppcre:create-scanner "someliteralprefix")
  (format t "3 ~A~%" (- *pcl-bmh-matchers-built* before)))
;; 20 literal-prefix patterns must not cost megabytes (the dense table is
;; 8.5 MB EACH).  Keep the scanners alive so nothing is collected under us.
(defvar *kept* nil)
(sb-ext:gc :full t)
(let ((before (sb-kernel:dynamic-usage)))
  (dotimes (i 20)
    (push (cl-ppcre:create-scanner (format nil "literalprefix~a-tail" i)) *kept*))
  (sb-ext:gc :full t)
  (format t "4 ~A~%" (round (/ (- (sb-kernel:dynamic-usage) before) 1048576))))
;; every answer, BOTH ways: the hashed table against cl-ppcre's generic search
(defun probe (pat subj)
  (multiple-value-bind (s e) (cl-ppcre:scan (cl-ppcre:create-scanner pat) subj)
    (format nil "~a/~a" s e)))
(let ((cases (list
              (list "abcdefgh" "xx abcdefgh yy")
              (list "abcdefgh" "xx abcdefg yy")
              (list "needle" "haystack with a needle inside")
              (list "(?i)HeLLo World" "say hello world now")
              (list "(?i)HeLLo World" "say HELLO WORLD now")
              ;; a needle whose characters are all above Latin-1
              (list (coerce (list (code-char 256) (code-char 512) (code-char 768) #\q) 'string)
                    (coerce (list #\a #\b (code-char 256) (code-char 512) (code-char 768) #\q #\!) 'string))
              ;; (code-char 353) hashes into the same bucket as #\a (353 = 97 + 256)
              (list "abcxyz" (coerce (list (code-char 353) (code-char 353) #\a #\b #\c #\x #\y #\z) 'string))
              (list "toolongneedleforthesubject" "short")
              (list "aaa" "aaaaa")
              (list "abcabd" "zzabcabcabdzz")
              (list "mississippi" "the mississippi river")
              (list "banana" "ba bana bananana")
              (list "\\w+endswithlongtail$" "q endswithlongtail")
              (list "TARGETSTRING" (concatenate 'string (make-string 5000 :initial-element #\z)
                                                "TARGETSTRING")))))
  (let ((bad 0))
    (dolist (c cases)
      (let ((hashed (probe (first c) (second c)))
            (generic (let ((cl-ppcre:*use-bmh-matchers* nil))
                       (probe (first c) (second c)))))
        (unless (string= hashed generic) (incf bad))))
    (format t "5 ~A of ~A~%" bad (length cases))))
LISP

my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $lfh $LISP;
close $lfh;
my $lout = `sbcl @sbcl_rt --load $lfile 2>&1`;
my %got = map { /^(\d+) (.*)$/ ? ($1 => $2) : () } split /\n/, $lout;
diag("raw CL output was:\n$lout") if ! defined $got{5};

is($got{1}, 'on',   'literal-prefix (BMH) scanning is switched on');
is($got{2}, 'ours', 'cl-ppcre::create-bmh-matcher is PCL\'s hashed version');
is($got{3}, '1',    'cl-ppcre reaches it — a fresh literal-prefix scanner builds one hashed matcher');
cmp_ok($got{4}, '<', 2,
       'twenty literal-prefix patterns cost under 2 MB (a dense table would be 170)');
is($got{5}, '0 of 14',
   'the hashed table answers every case exactly as cl-ppcre\'s generic search');

# -- B. the same question from Perl ---------------------------------------
# Every expected line is real perl 5.40.3's output for this program.
sub run_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode($fh, ':utf8');
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    binmode($cl_fh, ':utf8');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

my $prog = <<'EOF';
use strict; use warnings;
use utf8;
binmode(STDOUT, ":utf8");
my $s = "xx abcdefgh yy";
print "1 ", ($s =~ /abcdefgh/ ? "$-[0],$+[0]" : "no"), "\n";
print "2 ", ("xx abcdefg yy" =~ /abcdefgh/ ? "hit" : "no"), "\n";
print "3 ", ("aa prefixMID suffix" =~ /prefix(\w+)/ ? $1 : "no"), "\n";
print "4 ", ("say HELLO WORLD now" =~ /hello world/i ? "$-[0]" : "no"), "\n";
print "5 ", ("say hello world now" =~ /HeLLo WoRLd/i ? "$-[0]" : "no"), "\n";
my $needle = "\x{100}\x{200}\x{300}q";
my $subj   = "ab" . $needle . "!";
print "6 ", ($subj =~ /\Q$needle\E/ ? "$-[0],$+[0]" : "no"), "\n";
my $coll = "\x{161}\x{161}abcxyz";
print "7 ", ($coll =~ /abcxyz/ ? "$-[0],$+[0]" : "no"), "\n";
print "8 ", ("short" =~ /toolongneedleforthesubject/ ? "hit" : "no"), "\n";
print "9 ", ("abc" =~ /^/ ? "$-[0],$+[0]" : "no"), "\n";
print "10 ", ("zzabcabcabdzz" =~ /abcabd/ ? "$-[0],$+[0]" : "no"), "\n";
print "11 ", ("the mississippi river" =~ /mississippi/ ? "$-[0]" : "no"), "\n";
print "12 ", ("q endswithlongtail" =~ /\w+endswithlongtail$/ ? "$-[0]" : "no"), "\n";
my $long = ("z" x 5000) . "TARGETSTRING" . ("z" x 500) . "TARGETSTRING" . ("z" x 10);
my @at;
while ($long =~ /TARGETSTRING/g) { push @at, $-[0] }
print "13 ", join(",", @at), "\n";
my $t = "a::LITERAL::b::LITERAL::c";
print "14 ", join("|", split /::LITERAL::/, $t), "\n";
(my $u = $t) =~ s/LITERAL/X/g;
print "15 $u\n";
my $p = "aaa bbb aaa bbb";
my $n = 0;
$n++ while $p =~ /bbb/g;
print "16 $n\n";
EOF
is(run_pl($prog),
   "1 3,11\n2 no\n3 MID\n4 4\n5 4\n6 2,6\n7 2,8\n8 no\n9 0,0\n10 5,11\n"
   . "11 4\n12 no\n13 5000,5512\n14 a|b|c\n15 a::X::b::X::c\n16 2\n",
   'literal-prefix matching answers perl: hits, misses, /i, non-Latin-1, colliding buckets, //g, split, s///');
