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

done_testing();
