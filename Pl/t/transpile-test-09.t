#!/usr/bin/env perl
# Transpile tests part 9.  Opened s321 because the user closed -07.
#
# The constraint on these files is WALL TIME, not row count (CLAUDE.md 6):
# `prove -j8` waits for the slowest file, and each test_transpile row costs a
# perl oracle run AND an SBCL transpile+run.  Keep related cases inside ONE
# snippet rather than splitting them across calls.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    # Shell-escape embedded single quotes ('…' -> '\''), or any tick in the
    # snippet truncates the -e arg.
    (my $sh_code = $full_code) =~ s/'/'\\''/g;
    my $output = `perl -e '$sh_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}


# Task #179: /xx is a MODIFIER OF ITS OWN, not `x` written twice — it also
# ignores unescaped whitespace INSIDE a bracketed character class.  PCL used to
# treat it as plain /x (the per-character modifier loop just re-set :x), so
# `[a<TAB>b]/xx` still matched a tab.  Every case below is one line of t/re/
# keep_tabs.t, which this fix takes from 12/2 to 14/0 OK.
#
# INVERSE GUARDS, all in the same snippet because each row costs an SBCL run:
#   * plain /x must still KEEP class whitespace (the whole point of the pair),
#   * ESCAPED whitespace must survive /xx (perl keeps `[a\<TAB>b]` matching a
#     tab) — it leaves via the backslash branch before the class branch sees it,
#   * the `(?xx:)` GROUP form must behave like the flag form, and `(?x:)` must
#     not, since they travel different paths through the normaliser,
#   * a /xx pattern with no class at all must be unaffected.
test_transpile("/xx ignores whitespace inside [...]; /x does not", '
my $tab = "\t";
printf "x-class-tab=%d (want 1)\n",   ("\t" =~ /[a	b]/x   ? 1 : 0);
printf "xx-class-tab=%d (want 0)\n",  ("\t" =~ /[a	b]/xx  ? 1 : 0);
printf "xx-class-a=%d (want 1)\n",    ("a"  =~ /[a	b]/xx  ? 1 : 0);
printf "xx-class-b=%d (want 1)\n",    ("b"  =~ /[a	b]/xx  ? 1 : 0);
printf "xx-esc-tab=%d (want 1)\n",    ("\t" =~ /[a\	b]/xx ? 1 : 0);
printf "xx-esc-a=%d (want 1)\n",      ("a"  =~ /[a\	b]/xx ? 1 : 0);
printf "grp-xx-tab=%d (want 0)\n",    ("\t" =~ /(?xx:[a	b])/ ? 1 : 0);
printf "grp-xx-a=%d (want 1)\n",      ("a"  =~ /(?xx:[a	b])/ ? 1 : 0);
printf "grp-x-tab=%d (want 1)\n",     ("\t" =~ /(?x:[a	b])/  ? 1 : 0);
printf "xx-outside=%d (want 1)\n",    ("ab" =~ /a b/xx ? 1 : 0);
printf "qr-xx-tab=%d (want 0)\n",     ("\t" =~ qr/[a	b]/xx ? 1 : 0);
printf "qr-xx-a=%d (want 1)\n",       ("a"  =~ qr/[a	b]/xx ? 1 : 0);
');

# Task #182: the s/// replacement (non-/e) was built by a HAND-ROLLED mini
# interpolator that understood only \1..\9, $1..$9, ${name} and $name — where
# `name` had to start with [a-zA-Z_].  Everything else fell through as LITERAL
# TEXT, silently, for one of the most common idioms in Perl.  FOUR categories
# were broken, all of them found by this one delegation:
#   1. subscripts   s/(a)/$h{$1}/  ->  "{a}"        (the reported bug)
#   2. arrays       s/a/@arr/      ->  "@arr"       (no @ branch at all)
#   3. ${digit}     s/(a)/${1}x/   ->  "${1}x"      (the brace branch wanted a letter)
#   4. punctuation  s/a/$^O/       ->  "$^O"        (so did the bare branch)
# 3 and 4 were live in the corpus — perl-tests/sprintf.t emitted both as text.
# The fix routes the replacement through the REAL double-quoted-string parser
# (CLAUDE.md 11), so these follow whatever `"..."` already does.
#
# INVERSE GUARDS, same snippet: everything the old path DID handle must still
# work — both backref spellings, dq escapes, an escaped $, a braced NAME, a
# literal backslash, and /e (which takes a different branch entirely).
test_transpile("s/// replacement interpolates like a dq string (#182)", '
my %h = (k => "H", a => "A"); my @arr = ("P","Q"); my $v = "V"; my $s;
$s = "a";  $s =~ s/(a)/$h{$1}/;    print "1 subscript-cap=$s\n";
$s = "a";  $s =~ s/a/$h{k}/;       print "2 subscript-lit=$s\n";
$s = "a";  $s =~ s/a/$arr[1]/;     print "3 array-elem=$s\n";
$s = "a";  $s =~ s/a/@arr/;        print "4 array-interp=$s\n";
$s = "ab"; $s =~ s/(\w)/<$1>/g;    print "5 wrap-each=$s\n";
$s = "a";  $s =~ s/(a)/${1}Z/;     print "6 braced-digit=$s\n";
$s = "xy"; $s =~ s/(x)(y)/$2$1/;   print "7 swap=$s\n";
$s = "xy"; $s =~ s/(x)(y)/\2\1/;   print "8 swap-backslash=$s\n";
$s = "a";  $s =~ s/a/${v}/;        print "9 braced-name=$s\n";
$s = "a";  $s =~ s/a/\$notavar/;   print "10 escaped-dollar=$s\n";
$s = "a";  $s =~ s/a/p\tq/;        print "11 tab-escape=", length($s), "\n";
$s = "a";  $s =~ s/a/x\\y/;        print "12 literal-backslash=$s\n";
$s = "a";  $s =~ s/(a)/$h{$1} . "!"/e; print "13 e-mode=$s\n";
$s = "a";  $s =~ s/a/plain text/;  print "14 no-interp=$s\n";
');

# Task #181: the `(?^flags:…)` wrapper — the form a qr// stringifies as, and
# the form that carries its flags into a bigger pattern.  THREE defects, one
# family:
#   1. /xx printed as `(?^x:` — one x — so an interpolated /xx sub-pattern
#      silently reverted to /x and class whitespace came back.
#   2. `qr/$re/` DOUBLE-WRAPPED: `(?^:(?:abcdef))` where perl gives
#      `(?^:abcdef)`.  In perl a pattern that is exactly one interpolated qr IS
#      that qr — it keeps the inner flags and IGNORES the outer modifiers
#      (`qr/$re/i` does not become case-insensitive).
#   3. The variable holding a qr was FROZEN TO TEXT by the raw-string verdict
#      whenever its uses were all stringy (`print "$re"` plus `qr/$re/`), so by
#      the time the qr reached another regex it was already a string and rule 2
#      could not see it.  That is the root of 1 and 2 both: the annotator now
#      refuses to freeze a write whose RHS is an object (`write-object`).
#
# INVERSE GUARDS in the same snippet: a qr NOT interpolated anywhere must still
# print its own flags; a qr used as PART of a larger pattern must still embed
# (that is where a wrapper is correct, and perl embeds `(?^:` verbatim); the
# inner flags must still apply through the embedding (`x$i` matches "xABC" only
# under the inner /i); and a plain STRING interpolated with an outer /i must
# still take the /i — the lone-qr rule must not leak to strings.
test_transpile("qr wrapper: /xx keeps both x's, a lone interpolated qr is itself (#181)", '
my $re = qr/abcdef/;
my $i  = qr/abc/i;
my $x  = qr/[a b]/xx;
my $plain = "abc";
print "1 plain-qr=$re\n";
print "2 lone-interp=${\ qr/$re/}\n";
print "3 lone-interp-i=${\ qr/$i/}\n";
print "4 xx-str=$x\n";
print "5 lone-interp-xx=${\ qr/$x/}\n";
print "6 embedded=${\ qr/x$re/}\n";
print "7 outer-i-ignored=", ("ABC" =~ qr/$i/ ? 1 : 0), " (want 1)\n";
print "8 outer-i-ignored2=", ("ABC" =~ /$re/i ? 1 : 0), " (want 0)\n";
print "9 embedded-inner-i=", ("xABC" =~ /x$i/ ? 1 : 0), " (want 1)\n";
print "10 string-takes-i=", ("ABC" =~ /$plain/i ? 1 : 0), " (want 1)\n";
my $xcopy = qr/$x/;
print "11 xx-roundtrip=", (" " =~ /$xcopy/ ? 1 : 0), " (want 0)\n";
print "12 xx-still-matches=", ("a" =~ /$xcopy/ ? 1 : 0), " (want 1)\n";
print "13 ref=", ref($re), "\n";
my @g; my $t = "abc"; my $w = qr/(\w)/;
while ($t =~ /$w/g) { push @g, $1 }
print "14 outer-g-survives=@g\n";
my $r = "aXa"; (my $copy = $r) =~ s/$w/-/g;
print "15 s-with-qr-g=$copy\n";
');

done_testing();
