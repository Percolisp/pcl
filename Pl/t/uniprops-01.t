#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# uniprops-01.t — Unicode properties in patterns: `\p{…}`, `\pX`, `\P{…}`
# (task #2060, s496a).
#
# cl-ppcre reads a property only when its *property-resolver* is set, and PCL
# never set it: every `\p` in every pattern silently NEVER matched, and core
# Text::Wrap 5.40.3 (whose main loop is `\PM\pM*`) died "This shouldn't
# happen" on every wrap().  The answer comes from perl's OWN tables:
# tools/rebuild-uniprops writes cl/pcl-uniprops.lisp from
# Unicode::UCD::prop_invlist, and %pcl-property-resolver answers from it.
#
# The rows:
#   1-4  the ARTIFACT: stamped with the oracle perl's Unicode version, current
#        (regenerate + compare bytes -- only under the perl the stamp names,
#        skipped with both versions elsewhere, e.g. CI's stock 5.38), no `gen=` stamp (so the compiler-
#        staleness gate does not adopt it), and its 38 General_Category lists
#        agree with SBCL's own sb-unicode at EVERY code point (two independent
#        copies of Unicode 15.0 — this proves the generator's plumbing).
#   5    the Lisp normalizer equals the tool's `norm` over the census.
#   6-   behaviour vs perl, byte for byte: the grammar (loose matching, Is/In,
#        name=value, ^, scripts, blocks, Age), /i (fold THEN complement, and
#        perl's caseless equivalents), the compile errors (#2372's die), user-
#        defined properties, \X, and core Text::Wrap / Text::Tabs.
# The design is docs/regex-unicode-properties.md; ir-spec §10-prop.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $root    = "$RealBin/../..";
my $runtime = "$root/cl/pcl-runtime.lisp";
my $art     = "$root/cl/pcl-uniprops.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# --- 1-3. the artifact ------------------------------------------------------
open my $afh, '<:raw', $art or die "cannot read $art: $!";
my $line1 = <$afh>;
close $afh;
require Unicode::UCD;
my $uv = Unicode::UCD::UnicodeVersion();
like($line1, qr/^;;; pcl-uniprops unicode=\Q$uv\E perl=\S+ tool=tools\/rebuild-uniprops$/,
     "the artifact is stamped with the oracle perl's Unicode version ($uv)");
unlike($line1, qr/gen=/,
       'the artifact carries no gen= stamp (it is perl data, not compiler output)');

# Row 3 regenerates the artifact and compares BYTES.  The artifact is written
# under the ORACLE perl its stamp names (perl=...), and the stamp carries the
# generating perl's version, so under any other perl the regeneration can
# never be byte-identical: the comparison would test the ENVIRONMENT, not the
# artifact (CI's stock Ubuntu perl is 5.38 -- s499h).  Row 1's unicode= check
# above is the property-data contract and runs everywhere; the byte comparison
# runs only under the stamp's perl and SKIPS, naming both versions, elsewhere.
my ($stamp_perl) = $line1 =~ /\bperl=(\S+)/;
my $running_perl = sprintf "%vd", $^V;
SKIP: {
    skip("byte comparison runs only under the oracle perl the stamp names "
         . "(perl=" . ($stamp_perl // '?') . "); this is perl $running_perl", 1)
      if !defined $stamp_perl || $stamp_perl ne $running_perl;
    my (undef, $regen) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    system("$root/tools/rebuild-uniprops", '-o', $regen) == 0
      or die "tools/rebuild-uniprops failed";
    my $same = do { local $/; open my $a, '<:raw', $art; open my $b, '<:raw', $regen; <$a> eq <$b> };
    ok($same, 'cl/pcl-uniprops.lisp is current: regenerating it gives the same bytes')
      or diag("regenerate with tools/rebuild-uniprops (and read its --stats)");
}

# --- 4. the gc cross-check against sb-unicode --------------------------------
sub run_lisp {
    my ($form) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $fh $form;
    close $fh;
    my $out = `sbcl @sbcl_rt --load $file 2>&1`;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}
my $xcheck = run_lisp(<<'LISP');
(in-package :pcl)
(%pcl-uniprop-tables)
(let ((bad 0) (first nil) (tests (make-hash-table)))
  (loop for cp from 0 below char-code-limit
        for gc = (sb-unicode:general-category (code-char cp))
        for test = (or (gethash gc tests)
                       (setf (gethash gc tests)
                             (let ((code (gethash (%pcl-uniprop-normalize
                                                   (format nil "gc=~A" (symbol-name gc)))
                                                  *pcl-uniprop-keys*)))
                               (and code (%pcl-uniprop-test code)))))
        unless (and test (funcall test (code-char cp)))
          do (incf bad) (unless first (setf first (list cp gc))))
  (format t "GC-DISAGREE ~D ~S~%" bad first))
(sb-ext:exit)
LISP
like($xcheck, qr/^GC-DISAGREE 0 NIL$/mi,
     'the artifact\'s General_Category lists agree with sb-unicode at every code point')
  or diag($xcheck);

# --- 5. the two normalizers agree ------------------------------------------
{
    package RebuildUniprops;
    do "$root/tools/rebuild-uniprops" or die "cannot load the tool: $@";
}
my @spellings;
open my $cfh, '<:raw', "$root/docs/uniprops-census-s496.tsv" or die "census: $!";
while (my $l = <$cfh>) {
    next if $l =~ /^#/;
    my ($s) = split /\t/, $l;
    push @spellings, $s if defined $s && $s =~ /\S/ && $s !~ /["\\~]/ && $s !~ /[^\x20-\x7e]/;   # ASCII: perl refuses the rest
}
close $cfh;
push @spellings, 'L_', 'l &', 'Is L&', 'Age=011', 'Age=11.0', 'Age=V1_1', 'In = 1.1',
                 'General Category : Uppercase Letter', 'Is-Mark', 'Present_In=2.0';
my $lisp_list = join ' ', map { qq{"$_"} } @spellings;
my $lisp_norm = run_lisp(<<"LISP");
(in-package :pcl)
(dolist (s (list $lisp_list)) (format t "N<~A>~%" (%pcl-uniprop-normalize s)))
(sb-ext:exit)
LISP
my @got = $lisp_norm =~ /^N<(.*)>$/mg;
my @want = map { RebuildUniprops::norm($_) } @spellings;
is_deeply(\@got, \@want,
          'the Lisp normalizer equals tools/rebuild-uniprops\' norm over the census ('
          . scalar(@spellings) . ' spellings)');

# --- 6-. behaviour vs perl --------------------------------------------------
sub agrees {
    my ($program, $desc) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $program;
    close $fh;
    my $exp = `perl $file 2>&1`;
    my $got = `$root/runpcl $file 2>&1`;
    is($got, $exp, $desc);
}

agrees(<<'PL', 'the property grammar: loose names, Is/In, name=value, ^, scripts, blocks, Age');
my $acute = "\x{301}";
sub m1 { ($_[0] =~ $_[1]) ? 1 : 0 }
print join("", m1($acute, qr/\pM/), m1($acute, qr/\p{M}/), m1($acute, qr/\p{ mark }/),
  m1($acute, qr/\p{Is_Mark}/), m1($acute, qr/\p{gc:Mn}/),
  m1($acute, qr/\p{General_Category=Nonspacing_Mark}/), m1($acute, qr/\P{M}/),
  m1($acute, qr/\p{^M}/), m1($acute, qr/\P{^M}/), m1("a", qr/\PM/)), "\n";
print join("", m1("a", qr/\p{Lu}/), m1("A", qr/\p{Upper}/), m1("\x{2160}", qr/\p{Alpha}/),
  m1("\x{2160}", qr/\p{L}/), m1("\x{663}", qr/\p{Digit}/), m1("\x{663}", qr/\p{PosixDigit}/),
  m1("\x{a0}", qr/\p{Space}/), m1('$', qr/\p{Punct}/), m1('$', qr/\p{XPosixPunct}/),
  m1("\t", qr/\p{Print}/), m1("a", qr/\p{L&}/), m1("a", qr/\p{L_}/), m1("1", qr/\p{LC}/)), "\n";
print join("", m1("\x{378}", qr/\p{Assigned}/), m1("\x{378}", qr/\p{Cn}/),
  m1("\x{e9}", qr/\p{Latin}/), m1("\x{e9}", qr/\p{sc=Latn}/), m1("\x{3b1}", qr/\p{Script=Greek}/),
  m1("a", qr/\p{InBasicLatin}/), m1("\x{e9}", qr/\p{Block=Basic_Latin}/),
  m1("\x{e9}", qr/\p{Latin1}/), m1("a", qr/\p{Age=1.1}/), m1("\x{1F600}", qr/\p{Present_In=1.1}/),
  m1("a", qr/\p{IsAge=1.1}/), m1("\x{1F600}", qr/\p{Age=6.1}/), m1("1", qr/\p{Alpha=N}/),
  m1("\x{1F600}", qr/\p{Emoji}/), m1("\x{4e00}", qr/\p{Han}/)), "\n";
print join("", m1("5", qr/^[\p{L}\d]$/), m1("a", qr/[^\p{L}]/), m1("-", qr/^[\p{L}-]$/),
  m1("ab", qr/^\p{L}{2}$/), m1('\pM', qr/\Q\pM\E/), m1($acute, qr/\Q\pM\E/),
  m1("\t", qr/[\p{Zl}\p{C}\p{Zp}]/), m1(" \x{e9}5", qr/^\h\p{L}[[:digit:]]$/),
  m1("a", qr/ \p{ L } /x)), "\n";
my $re = qr/\p{L}/;
my $s = "a\x{301}b"; (my $t = $s) =~ s/\pM//g;
my $u = "p\\p"; $u =~ tr/\\p/xy/;
print m1("xa", qr/x$re/), " ", join("|", split /\p{Z}+/, "a\x{a0}b c"), " ", length($t), " $u\n";
PL

agrees(<<'PL', '/i: fold THEN complement, and perl\'s caseless equivalents (Lu/i is LC, Upper/i is Cased)');
sub m1 { ($_[0] =~ $_[1]) ? 1 : 0 }
print join("", m1("a", qr/\p{Lu}/i), m1("A", qr/\p{Ll}/i), m1("1", qr/\p{Lu}/i),
  m1("a", qr/\P{Lu}/i), m1("a", qr/\p{^Lu}/i), m1("a", qr/[\P{Lu}]/i),
  m1("a", qr/[^\P{Lu}]/i), m1("a", qr/[^\p{Lu}]/i), m1("a", qr/(?i)\p{Lu}/),
  m1("a", qr/(?-i:\p{Lu})/i), m1("\x{df}", qr/\p{Lu}/i), m1("\x{2160}", qr/\p{Upper}/i),
  m1("\x{2160}", qr/\p{Lt}/i), m1("\x{2160}", qr/\p{Lu}/i), m1("z", qr/\p{PosixUpper}/i)), "\n";
PL

agrees(<<'PL', 'a property perl cannot find DIES at the match, trappably, in perl\'s words (#2372)');
for my $bad ('NoSuchProp', '', 'Is_NoSuch') {
  my $pat = "\\p{$bad}";
  my $r = eval { "a" =~ /$pat/; 1 };
  print $r ? "no-die\n" : ($@ =~ /(Can't find Unicode property definition "\w*"|Empty \\p\{\}|Unknown user-defined property name \\p\{main::\w+\})/ ? "$1\n" : "other: $@");
}
print "after\n";
PL

agrees(<<'PL', 'user-defined properties: ranges, +/-/!/& lines, comments, /i argument, packages, user wins');
sub InMyProp { "0061\t0063\n" }
sub IsDigits { return "+utf8::Nd\n" }
sub IsAlpha { "0030\t0039\n" }
sub IsNotAB { "!utf8::ASCII\n0041\n0042\n" }
sub IsVowelsNoE { "0061\n0065\n0069\n006F\n0075\n-main::IsE\n" }
sub IsE { "0065" }
sub InCI { my $ci = shift; $ci ? "0041\n" : "0061\n" }
sub IsAndL { "+utf8::L\n&utf8::ASCII\n" }
sub IsComment { "# comment\n\n0078   # x\n" }
package Foo;
sub IsFooProp { "0066\n" }
sub t { print "foo-local:", ("f" =~ /\p{IsFooProp}/ ? 1 : 0), "\n" }
package main;
sub row { my ($re, @s) = @_; join "", map { $_ =~ $re ? 1 : 0 } @s }
print row(qr/\p{InMyProp}/, qw(a b c d)), " ", row(qr/\P{InMyProp}/, qw(a b c d)), " ",
  row(qr/\p{^InMyProp}/, qw(a d)), " ", row(qr/[\p{InMyProp}x]/, qw(a x d)), "\n";
print row(qr/\p{IsDigits}/, "5", "a", "\x{663}"), " ", row(qr/\p{IsAlpha}/, qw(5 a)), " ",
  row(qr/\p{IsNotAB}/, "A", "B", "C", "\x{100}"), " ", row(qr/\p{IsVowelsNoE}/, qw(a e i x)), " ",
  row(qr/\p{InCI}/, qw(a A)), "/", row(qr/\p{InCI}/i, qw(a A b)), " ",
  row(qr/\p{IsAndL}/, "a", "\x{100}", "1"), " ", row(qr/\p{IsComment}/, qw(x y)), "\n";
Foo::t();
print "qualified: ", ("f" =~ /\p{Foo::IsFooProp}/ ? 1 : 0), ("a" =~ /\p{main::InMyProp}/ ? 1 : 0), "\n";
PL

agrees(<<'PL', 'core Text::Wrap wrap()/fill() and Text::Tabs, incl. combining marks, as perl prints them');
use Text::Wrap qw(wrap fill $columns);
use Text::Tabs;
binmode STDOUT, ":utf8";
print wrap("", "", "a b c"), "\n";
my @w = map { ("lorem", "ipsum", "dolor", "sit", "amet,", "consectetur")[$_ % 6] . $_ } 1 .. 120;
$columns = 40;
print wrap("  ", "> ", join(" ", @w)), "\n";
print fill("", "", "para one line one\nline two\n\npara two here " x 4), "\n";
$columns = 20;
print wrap("", "", join " ", ("e\x{301}") x 30), "\n";
print expand("a\tb\x{301}\tc"), "|", unexpand("a       b       c"), "|\n";
PL

done_testing();
