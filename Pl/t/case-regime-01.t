#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# case-regime-01.t — s494u, task #2092 PHASE 1: chars 128-255 get perl's
# REGIMES (docs/ir-spec.md §3.2h).
#
# perl maps chars 128-255 by one of two regimes.  At a `unicode_strings` site
# (`use v5.12`+, `use feature 'unicode_strings'`) every string gets Unicode
# rules; everywhere else (perl's /d) only a string carrying the UTF-8 flag
# does.  PCL keeps no flag (#1389), so at a /d site it SNIFFS: a string whose
# high chars all sit in well-formed UTF-8 sequences is undecoded bytes (ASCII
# rules), anything else decoded text (Unicode rules).  Before this, PCL was
# Unicode everywhere, and `lc` of raw UTF-8 bytes turned C3 into E3 = invalid
# UTF-8, silently.  And the /a regex modifier was silently IGNORED.
#
# Every byte row is written with \x escapes (no literal high byte in this
# file), so the rows do not depend on how the SOURCE is decoded (#2192).
# ANSWER rows are perl 5.40.3's output (scratch/s494u/progA.pl progB.pl
# aprobe.pl, probed perl -> the 209e7533 base -> this tree), except the rows
# marked DOCUMENTED, which pin the sniff's known error (not-supported.md
# "The per-scalar UTF-8 flag").
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

sub write_pl {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}
sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }
sub run_pl {
    my $cl = emitted($_[0]);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    return $out;
}
# Run SRC once and check each labelled output line against EXPECT.
sub rows {
    my ($what, $src, %expect) = @_;
    my %got = map { /^(\S+) (.*)$/ ? ($1 => $2) : () } split /\n/, run_pl($src);
    for my $k (sort { ($a =~ /(\d+)/)[0] <=> ($b =~ /(\d+)/)[0] } keys %expect) {
        my ($want, $desc) = @{ $expect{$k} };
        is($got{$k}, $want, "$what $k: $desc");
    }
}

my $HX = 'sub hx { join " ", map { sprintf "%02X", ord } split //, $_[0] } ';

# ─────────────────────────────────────────────────────────────────────────────
# THE SHAPES — the regime reaches the runtime as ONE extra operand, only at a
# unicode_strings site; a /d site's emission is what it always was.
# ─────────────────────────────────────────────────────────────────────────────
my $d = emitted(q{my $b = "x"; print lc($b), uc($b), ucfirst($b), lcfirst($b), "\U$b";});
unlike($d, qr/:u\)/, 'a /d site emits no regime operand');
my $u = emitted(q{use v5.12; my $b = "x"; print lc($b), uc($b), ucfirst($b), lcfirst($b), "\U$b"; { no feature "unicode_strings"; print lc $b; }});
like($u, qr/\(p-lc \$b\S* :u\)/, 'lc at a `use v5.12` site passes :u');
like($u, qr/\(p-uc \$b\S* :u\)/, '... uc');
like($u, qr/\(p-ucfirst \$b\S* :u\)/, '... ucfirst');
like($u, qr/\(p-lcfirst \$b\S* :u\)/, '... lcfirst');
like($u, qr/\(p-list-ctx \(p-lc \$b\S*\)\)\)/, '`no feature "unicode_strings"` in a block turns it back off');
my $f = emitted(q{use feature ':5.12'; print lc "x";});
like($f, qr/\(p-lc "x" :u\)/, 'a `:5.12` feature bundle is a unicode_strings site');
my $v = emitted(q{use feature 'unicode_strings'; use 5.010; print lc "x";});
unlike($v, qr/:u\)/, '`use VERSION` below 5.011 REPLACES the feature set (off)');

# ─────────────────────────────────────────────────────────────────────────────
# /d — no pragma
# ─────────────────────────────────────────────────────────────────────────────
rows('/d', $HX . q{
my $b = "\xC3\x80B";
print "A1 ", hx(lc $b), "\n";
my $d = $b; utf8::decode($d);
print "A2 ", hx(lc $d), "\n";
my $u = chr(0xC9); utf8::upgrade($u);
print "A3 ", hx(lc $u), "\n";
print "A4 ", hx(lc(chr(0xC9) . chr(0x100))), "\n";
print "A5 ", hx(ucfirst "\xC3\xA9lan"), "\n";
print "A6 ", hx(uc "stra\xC3\x9Fe"), "\n";
print "A7 ", hx(lcfirst $b), " ", lcfirst("ABC"), " ", ucfirst("abc"), " ", uc("mIx"), " ", lc("MiX"), "\n";
{ use feature 'fc'; print "A8 ", hx(fc $b), "\n"; }
print "A9 ", hx("\L$b\E"), " ", hx("\U$b"), " ", hx("\u\L$b"), "\n";
print "A10 ", hx("\LA\xC3\x80B"), "\n";
my $e = "caf\xC3\xA9 \xC3\x80 LA"; print "A11 ", hx(lc $e), "\n";
print "A12 ", hx(uc($d . "x")), "\n";
print "A13 ", hx(lc chr(0xC9)), " ", hx("\U\xE9x"), "\n";
print "A14 ", hx(lc "\xC0\x80A"), " ", hx(uc "\xED\xA0\x80a"), " ", hx(lc "\xC3"), "\n";
},
  A1  => ['C3 80 62', 'lc of raw UTF-8 bytes leaves the high bytes alone (was E3: invalid UTF-8)'],
  A2  => ['E0 62', 'lc of DECODED text maps by Unicode rules'],
  A3  => ['E9', 'lc of an upgraded chr(0xC9) (a lone high char is not UTF-8 bytes)'],
  A4  => ['E9 101', 'a string with a char > 255 is decoded text'],
  A5  => ['C3 A9 6C 61 6E', 'ucfirst of raw bytes leaves the lead byte alone'],
  A6  => ['53 54 52 41 C3 9F 45', 'uc of raw bytes: ASCII rules only'],
  A7  => ['C3 80 42 aBC Abc MIX mix', 'lcfirst of raw bytes + the ASCII cases'],
  A8  => ['C3 80 62', 'fc of raw bytes (feature fc, no unicode_strings)'],
  A9  => ['C3 80 62 C3 80 42 C3 80 62', 'the \L \U \u escapes over a raw-byte variable'],
  A10 => ['61 C3 80 62', 'a constant "\L..." literal folds by the same rule at compile time'],
  A11 => ['63 61 66 C3 A9 20 C3 80 20 6C 61', 'lc of a mixed raw-byte line keeps it valid UTF-8'],
  A12 => ['C0 42 58', 'uc of decoded text + ASCII'],
  A13 => ['E9 C9 58', 'DOCUMENTED (sniff error ii): a lone Latin-1 byte is folded (perl: C9 E9 58)'],
  A14 => ['E0 80 61 CD A0 80 41 E3',
          'DOCUMENTED: an overlong, a surrogate and a truncated tail are NOT UTF-8 bytes (strict sniff)'],
);

# ─────────────────────────────────────────────────────────────────────────────
# /u — unicode_strings, lexical, inherited by a string eval
# ─────────────────────────────────────────────────────────────────────────────
rows('/u', $HX . q{
my $b = "\xC3\x80B";
sub dsite { lc $_[0] }
use v5.12;
sub usite { lc $_[0] }
print "B1 ", hx(lc $b), " ", hx(lc chr(0xC9)), "\n";
print "B2 ", hx("\L$b"), " ", hx("\LA\xC3\x80B"), " ", hx(ucfirst "\xE9t\xE9"), "\n";
print "B3 ", hx(dsite($b)), " ", hx(usite($b)), "\n";
{ no feature 'unicode_strings'; print "B4 ", hx(lc $b), " ", hx(eval 'lc $b'), "\n"; }
print "B5 ", hx(eval 'lc $b'), " ", hx(uc $b), "\n";
},
  B1 => ['E3 80 62 E9', 'under use v5.12 every string maps by Unicode rules (perl corrupts raw bytes too)'],
  B2 => ['E3 80 62 61 E3 80 62 C9 74 E9', 'the escapes and a constant literal follow the site'],
  B3 => ['C3 80 62 E3 80 62', 'the regime is the SITE\'s: a sub body before the pragma is /d'],
  B4 => ['C3 80 62 C3 80 62', 'no feature turns it off, and a string eval there inherits /d'],
  B5 => ['E3 80 62 C3 80 42', 'a string eval at a unicode_strings site inherits it'],
);

# ─────────────────────────────────────────────────────────────────────────────
# /a — ASCII-restricted classes (was silently ignored)
# ─────────────────────────────────────────────────────────────────────────────
rows('/a', q{
my $K = "\x{212A}";
print "R3 ", ($K =~ /k/aai ? 1 : 0), "\n";
print "R4 ", ($K =~ /\w/a ? 1 : 0), ($K =~ /\w/ai ? 1 : 0), "\n";
print "R8 ", ("\x{e9}" =~ /\w/a ? 1 : 0), ("\x{a0}" =~ /\s/a ? 1 : 0), ("\x0b" =~ /\s/a ? 1 : 0), ("\x{660}" =~ /\d/a ? 1 : 0), "\n";
my $d = "caf\x{e9} x"; my @b; while ($d =~ /\b/ag) { push @b, pos($d) }
print "R12 ", join(",", @b), "\n";
@b = (); while ($d =~ /\B/ag) { push @b, pos($d) }
print "R16 ", join(",", @b), "\n";
print "R13 ", ("\x{e9}" =~ /[\w]/a ? 1 : 0), ("\x{e9}" =~ /[^\W]/a ? 1 : 0), ("\x{e9}" =~ /\W/a ? 1 : 0), ("\x{e9}" =~ /[[:alpha:]]/a ? 1 : 0), "\n";
print "R19 ", do { my $r = qr/\w/a; ("\x{e9}" =~ /x|$r/ ? 1 : 0) }, ("\x{e9}" =~ /(?a:\w)/ ? 1 : 0), ("\x{e9}" =~ /(?a)\w/ ? 1 : 0), ("\x{e9}" =~ /(?a:x)|\w/ ? 1 : 0), "\n";
print "R22 ", join("|", split /\W+/a, "caf\x{e9}x y"), "\n";
print "R23 ", do { (my $t = "caf\x{e9} x") =~ s/\w/_/ag; join " ", map { sprintf "%X", ord } split //, $t }, "\n";
print "R24 ", qr/\w/a, " ", qr/x/aai, "\n";
},
  R3  => ['0', '/aa: KELVIN SIGN does not match k'],
  R4  => ['00', '/a: \w is ASCII-only, with or without /i'],
  R8  => ['0010', '/a: \w \s \d ASCII-only; \s includes VT'],
  R12 => ['0,3,5,6', '/a: \b sees e-acute as a non-word char'],
  R16 => ['1,2,4', '/a: \B likewise'],
  R13 => ['0010', '/a inside a bracket class, negated, and POSIX'],
  R19 => ['0001', 'a qr//a interpolated, (?a:...) and (?a) inline, and a (?a:...) scope ENDS at its paren'],
  R22 => ['caf|x|y', 'split honours /a'],
  R23 => ['5F 5F 5F E9 20 5F', 's///g honours /a'],
  R24 => ['(?^a:\w) (?^aai:x)', 'qr stringifies its charset flag, so /a survives interpolation'],
);

done_testing();
