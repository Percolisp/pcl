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

    # Transpile stderr must NOT be folded into the .lisp: pl2cl ANNOUNCES on
    # stderr for effect-only drops (the #268/#270 anon-sub `:prototype`
    # family), and a `PCL: …` line at the top of the file is a CL read error
    # ("illegal terminating character after a colon").  A warning has no perl
    # counterpart either — run_perl compares perl's own stderr, and perl says
    # nothing here.  On a FAILED transpile the stderr is still appended, so a
    # real error is as visible as it was before.  (Each transpile-test-NN.t
    # carries its own copy of this helper; only this one is fixed.)
    my (undef, $err_file) = tempfile(SUFFIX => '.err', UNLINK => 1);
    my $cl_code = `$pl2cl $pl_file 2>$err_file`;
    if ($?) {
        open my $err_in, '<', $err_file or die "open $err_file: $!";
        $cl_code .= do { local $/; <$err_in> };
        close $err_in;
    }

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

# Task #186: `_` is perl's STAT-CACHE FILEHANDLE — `-e $f and -f _ and -r _`
# reuses the last stat's answer instead of calling stat(2) three times.  PCL
# emitted the bareword as a bare CL symbol `(p--f _)`, which was unbound: an
# internal crash ("The variable Test::_ is unbound"), and it took every module
# using the idiom with it — perl's own Test.pm line 200 does, so `use Test`
# died outright (12 t-files in the widened CPAN board).  `_` is now a defvar
# in :pcl holding a marker, and every filetest resolves its operand through
# ONE funnel (%p--path -> %p-stat-arg) that maintains the cache.
#
# INVERSE GUARDS in the same snippet:
#   * `_` BEFORE any stat must be false, not "the cwd" — (probe-file "")
#     answers the cwd in SBCL, so -e needed an explicit empty-path reject,
#   * a filetest that does NOT take `_` must still stat its own operand, and
#     must UPDATE the cache for the next `_`,
#   * a FAILED test leaves `_` false rather than answering from a stale entry,
#   * an explicit stat()/lstat() must feed the cache too (perl's rule),
#   * a real filehandle operand must keep working (it resolves via /dev/fd).
# NOT tested against the oracle: `_` in a PRISTINE process.  perl answers from
# whatever its own last internal stat was — loading any module stats the file
# it found in @INC — so the "before any stat" state is a property of the
# harness, not of the language.  PCL's answer there is false (%p--path yields
# the empty path, and -e rejects it explicitly).
test_transpile("`_` stat-cache filehandle (#186)", '
my $tmp = "/tmp/pcl-statcache-$$.txt";
open(my $o, ">", $tmp) or die "open: $!"; print $o "hello\n"; close $o;
print "1 chain=", ((-e $tmp and -f _ and -r _) ? 1 : 0), " (want 1)\n";
print "2 cached-d=", (-d _ ? 1 : 0), " (want 0)\n";
print "3 cached-size=", (-s _), " (want 6)\n";
print "4 dir-then-cache=", ((-e "/tmp" and -d _) ? 1 : 0), " (want 1)\n";
print "5 dir-not-file=", (-f _ ? 1 : 0), " (want 0)\n";
stat($tmp);
print "6 after-stat-f=", (-f _ ? 1 : 0), " (want 1)\n";
print "7 after-stat-s=", (-s _), " (want 6)\n";
lstat("/tmp");
print "8 after-lstat-d=", (-d _ ? 1 : 0), " (want 1)\n";
open(my $in, "<", $tmp) or die "open: $!";
print "9 fh-operand=", (-f $in ? 1 : 0), " (want 1)\n";
print "10 fh-size=", (-s $in), " (want 6)\n";
close $in;
print "11 own-operand-still-works=", (-d "/tmp" ? 1 : 0), " (want 1)\n";
print "12 cache-followed-it=", (-d _ ? 1 : 0), " (want 1)\n";
print "13 failed-stat=", (-e "/pcl-no-such-path" ? 1 : 0), " (want 0)\n";
print "14 cache-after-failure=", (-f _ ? 1 : 0), " (want 0)\n";
unlink $tmp;
');

# Task #186, second half: `*STDOUT{IO}` had NO branch in p-glob-slot, so it fell
# through to undef — and the two consumers then disagreed SILENTLY.  `print $io
# ...` treated the undef handle as EBADF and printed NOTHING, while `printf $io
# ...` passed nil to princ, which CL reads as *standard-output* — so half of
# Test.pm''s header appeared and half vanished.  Both now resolve through
# %p-resolve-fh, and printf bails like print instead of guessing.
test_transpile("*FH{IO} is a real handle; print and printf agree (#186)", '
my $out = *STDOUT{IO};
print $out "1 print-via-glob-IO\n";
printf $out "2 printf-via-glob-IO %d\n", 42;
print {$out} "3 block-form\n";
my $err = *STDERR{IO};
print "4 stderr-io-defined=", (defined $err ? 1 : 0), " (want 1)\n";
print "5 no-such-glob-io=", (defined *pcl_no_such_handle{IO} ? 1 : 0), " (want 0)\n";
print "6 glob-name=", *STDOUT{NAME}, "\n";
print "6b glob-package=", *STDOUT{PACKAGE}, "\n";
my $tmp = "/tmp/pcl-globio-$$.txt";
open(my $c, ">", $tmp) or die "open: $!"; close $c;
open(my $r, "<", $tmp) or die "open: $!"; close $r;
my $rc = printf $r "SHOULD NOT APPEAR\n";
print "7 printf-closed-rc=", (defined $rc ? "def" : "undef"), " (want undef)\n";
my $rc2 = print $r "SHOULD NOT APPEAR EITHER\n";
print "8 print-closed-rc=", (defined $rc2 ? "def" : "undef"), " (want undef)\n";
unlink $tmp;
');

# Task #187: a `use` inside an EXPRESSION BLOCK (do{}/eval{}/anon-sub) was
# silently DROPPED under v2 — no p-use in the output at all, so the module
# never loaded and the only symptom was an undefined function at run time
# (`use Class::Method::Modifiers` inside `do { package Class; … }`, the shape
# 9 of that dist''s t-files use).  Two defects, one family:
#   1. v1''s block lowering HOISTS a use/BEGIN/our out of the block into the
#      `definitions` bucket.  Under v2 that parser is only the expression
#      SEAM, and its hoist took the v1-only DEFERRAL path (a buffer flushed by
#      _process_children, which v2 never calls) — the text went nowhere.
#   2. Even hoisted, the use landed in the ENCLOSING package: a `package Foo;`
#      inside a do-block is only a runtime switch, so *package* at the hoisted
#      form is the outer one.  p-use now takes an explicit `:into`.
#
# INVERSE GUARDS in the same snippet: the import must NOT leak into main when
# the use is inside `package PkgA` (row 2 — the whole point of `:into`); a
# main-level do-block use must still import into main (row 3); the do-block
# must keep its VALUE (row 4 — the hoist must not eat the tail); the imported
# sub must actually work (row 5); and eval{} takes the same path as do{}
# (row 6).
test_transpile("`use` inside a do/eval block is not dropped (#187)", '
do { package PkgA; use File::Basename; sub w { defined &basename ? "yes" : "no" } };
print "1 pkg-import=", PkgA::w(), " (want yes)\n";
print "2 not-leaked-to-main=", (defined &basename ? "yes" : "no"), " (want no)\n";
do { use List::Util qw(first); };
print "3 main-import=", (defined &first ? "yes" : "no"), " (want yes)\n";
my $v = do { use List::Util qw(sum); 42 };
print "4 do-value=$v (want 42)\n";
print "5 sum=", sum(1,2,3), " (want 6)\n";
eval { package PkgB; use List::Util qw(max); sub w2 { defined &max ? "yes" : "no" } };
print "6 eval-block-pkg=", PkgB::w2(), " (want yes)\n";
');

# Task #188: an UNMATCHED capture group came back as raw CL nil, and raw nil
# means "the empty list" to %p-flatten-list — which is what a list ASSIGNMENT
# flattens through.  So `my ($dir,$file) = $path =~ m{^(.*/)?(.*)}` put the
# FILENAME in $dir and undef in $file for every path without a slash: every
# later capture shifted up one slot, silently.  That is verbatim the shape
# File::Basename::fileparse uses, so `dirname("c.txt")` answered "c.txt".
# An unmatched group is perl UNDEF; it now stores *p-undef*.
#
# INVERSE GUARDS: the ARRAY target was always right (it keeps the hole) and
# must stay right; a middle and a trailing unmatched group must land in their
# own slots; /g list context has its own capture loop (the second copy) and
# gets the same treatment; a group that DID match must be unaffected.
test_transpile("an unmatched capture is undef, not an empty list (#188)", '
sub d { join ",", map { defined $_ ? "[$_]" : "U" } @_ }
my ($a1,$b1) = ("c.txt" =~ m{^(.*/)?(.*)}s);
print "1 leading=", d($a1,$b1), " (want U,[c.txt])\n";
my ($a2,$b2) = ("/x/c.txt" =~ m{^(.*/)?(.*)}s);
print "2 matched=", d($a2,$b2), " (want [/x/],[c.txt])\n";
my ($j,$k,$m) = ("xy" =~ /(a)?(x)(b)?/);
print "3 middle+trailing=", d($j,$k,$m), " (want U,[x],U)\n";
my @arr = ("xy" =~ /(a)?(x)(b)?/);
print "4 array-target=", scalar(@arr), " ", d(@arr), " (want 3 U,[x],U)\n";
my @g = ("ab ab" =~ /(a)?(b)/g);
print "5 g-list=", scalar(@g), " ", d(@g), " (want 4 [a],[b],[a],[b])\n";
my @g2 = ("b b" =~ /(a)?(b)/g);
print "6 g-list-unmatched=", scalar(@g2), " ", d(@g2), " (want 4 U,[b],U,[b])\n";
my ($p,$q) = ("zz" =~ /(a)?(b)?/);
print "7 both-unmatched=", d($p,$q), " (want U,U)\n";
use File::Basename;
print "8 dirname=", dirname("c.txt"), " (want .)\n";
print "9 dirname2=", dirname("/a/b/c.txt"), " (want /a/b)\n";
print "10 basename=", basename("/a/b/"), " (want b)\n";
');

# Task #190: `divide $text => 4` is a call to the DECLARED sub `divide`, not
# indirect method syntax — Perl resolves the bareword at compile time and a
# known sub wins.  PCL agreed at statement level and DISAGREED inside a
# `( … )` or `[ … ]`, because only the nested form reaches PExpr''s
# indirect-object pre-pass, which had no notion of "this name is already a
# sub".  `[ divide $stdtext => 4 ]` therefore died with `Can''t locate object
# method "divide" via package "<the string value of $stdtext>"` — the whole of
# Text-Balanced''s 05_extmul.t (0 ok before, 59 after).
#
# INVERSE GUARDS in the same snippet: the paren-form call must be unchanged;
# `WORD $obj` where WORD is NOT a sub of the CURRENT package must keep whatever
# it did (rows 6/7 — `Widget::show` is not visible as a bare `show` from main,
# so the guard asks the package-QUALIFIED question; the unqualified version of
# this fix broke exactly that); and the fat comma must still autoquote.
test_transpile("a declared sub beats indirect-object syntax (#190)", '
sub divide { my ($t, @i) = @_; return ("d", $t, @i) }
sub tag    { return "T(" . join("|", @_) . ")" }
my $s = "TEXT";
print "1 stmt=", join(",", divide $s => 4), "\n";
my @a = (divide $s => 4);
print "2 in-parens=", join(",", @a), "\n";
my $r = [ divide $s => 4 ];
print "3 in-brackets=", join(",", @$r), "\n";
my $r2 = [ divide $s, 4 ];
print "4 comma-form=", join(",", @$r2), "\n";
my $r3 = [ divide($s => 4) ];
print "5 call-parens=", join(",", @$r3), "\n";
my %h = ( tag => 1 );
print "6 fat-comma-autoquote=", join(",", sort keys %h), "\n";
print "7 nested=", join(",", @{[ tag $s => divide $s => 2 ]}), "\n";
');

# #193: under strict-subs an undeclared bareword in operator context is a
# CALL, never a string — a sub installed via a dynamic glob in BEGIN is
# invisible to the transpiler but real at runtime (File::Path's _IS_MSWIN32,
# whose leading underscore also slips the ALL-CAPS funcall escape).  Covers
# the operator-loop shapes that used to string-ify: ternary, &&, ||,
# non-final list element.  INVERSE GUARD in row 4: `=>` autoquotes its left
# word even under strict, even when a sub of that name exists.
test_transpile("strict-subs bareword before a binary operator is a call (#193)", '
use strict;
BEGIN { no strict "refs";
        for (qw(aaa bbb)) { *{"_T_\U$_"} = $_ eq "aaa" ? sub(){1} : sub(){0} } }
print "1 ternary=", (_T_AAA ? "y" : "n"), (_T_BBB ? "y" : "n"), "\n";
print "2 and=", (_T_AAA && "yes"), "\n";
print "3 list=", join(",", (_T_AAA, _T_BBB, 9)), "\n";
my %h = (_T_AAA => 5);
print "4 fatcomma=", join(",", sort keys %h), "\n";
my $r = _T_BBB || "fallback";
print "5 or=$r\n";
');

# INVERSE of #193: with NO strict in effect the old reading stands — an
# unknown bareword in operator context is the string of its own name.
test_transpile("no-strict bareword before a binary operator stays a string (#193 inverse)", '
my $v = Bare_word && "t";
print "1 and=$v\n";
print "2 ternary=", (Bare_word ? "y" : "n"), "\n";
print "3 val=", join(",", (Bare_word, 9)), "\n";
');

# readdir/opendir semantics (found under #193, separate runtime bug):
# opendir on a path WITHOUT a trailing slash used to list the PARENT
# directory (the last component parsed as a file name and the "*.*" wildcard
# replaced it); a subdirectory's entry came back as "" (file-namestring of a
# directory pathname), which is what File::Path's remove_tree then tried to
# unlink; "." and ".." were missing; and list-context readdir returned only
# ONE entry (no drain).  End-to-end guard: make_path + remove_tree.
test_transpile("readdir: list drain, subdir names, dot entries, right directory", '
use strict;
use File::Path qw(make_path remove_tree);
my $d = "/tmp/pcl-t09-rd-$$";
make_path("$d/sub1/sub2");
open my $fh, ">", "$d/file1" or die "open: $!"; print $fh "x"; close $fh;
opendir my $dh, $d or die "opendir: $!";
my @all = sort(readdir($dh));
closedir $dh;
print "1 list=", join(",", @all), "\n";
opendir my $dh2, $d or die "opendir2: $!";
my @loop;
while (my $e = readdir $dh2) { push @loop, $e }
closedir $dh2;
print "2 loop=", join(",", sort @loop), "\n";
open my $fh0, ">", "$d/0" or die; close $fh0;
my ($seen, $dummy, $name) = (0, "", "");
opendir my $dh3, $d or die "opendir3: $!";
while (($seen ? $dummy : $name) = readdir $dh3) {
  $seen++ if $name eq "0";
}
closedir $dh3;
print "3 ternary-lvalue-seen=", ($seen > 0 ? "yes" : "no"), "\n";
remove_tree($d);
print "4 removed=", (-d $d ? "no" : "yes"), "\n";
');

# getprotobyname/getprotobynumber answer from /etc/protocols (s339, task #222).
# They used to answer from a four-entry static table, so "a protocol PCL never
# heard of" and "a protocol this host does not have" were the same undef.
# The INVERSE guards are the interesting half: the lookup is EXACT (an alias
# hits, a mis-cased name MISSES), a miss is the empty list, and the scalar
# return is asymmetric — by-name gives the number, by-number gives the NAME.
test_transpile("getproto*: /etc/protocols, exact match, both scalar shapes", '
my @tcp = getprotobyname("tcp");
print "1 list=@tcp\n";
print "2 scalar=", scalar(getprotobyname("tcp")), "\n";
my @alias = getprotobyname("TCP");
print "3 alias=@alias\n";
my @miscase = getprotobyname("Tcp");
print "4 miscase-count=", scalar(@miscase), "\n";
my @gre = getprotobyname("gre");
print "5 beyond-old-table=@gre\n";
my @miss = getprotobyname("nosuchprotocol");
print "6 miss-count=", scalar(@miss), "\n";
my @byn = getprotobynumber(6);
print "7 bynum=@byn\n";
print "8 bynum-scalar=", scalar(getprotobynumber(17)), "\n";
my @dup = getprotobynumber(0);
print "9 first-line-wins=@dup\n";
my @nonum = getprotobynumber(60000);
print "10 nonum-count=", scalar(@nonum), "\n";
');

# #240 step 2 (s351): inside `eval "package X; …"` an UNQUALIFIED name belongs
# to X, not to the caller.  p-eval-thunk binds *package* to X around the free-
# name resolution AND the body, so all three spellings agree with perl: a bare
# write (1), a bare read that must NOT see the caller's global (2), an `our`
# declared-then-read-back (3/4), and a symbolic deref, whose value AND package
# slot both have to land in X (5).  6-7 are the INVERSE guards — the caller's
# lexical still wins over the region package, and the capture the parser must
# never refuse still closes.  Shape guards live in Pl/t/parser2-02.t.
test_transpile("eval package-region: unqualified names resolve in X", q{
eval 'package F2; $Zz = 5; 1' or die $@;
print "1 ", (defined ${'F2::Zz'} ? ${'F2::Zz'} : 'undef'),
      " / ", (defined ${'main::Zz'} ? ${'main::Zz'} : 'undef'), "\n";
$main::G9 = 9;
my $g9 = eval 'package X9; $G9';
print "2 ", (defined $g9 ? $g9 : 'undef'), "\n";
print "3 ", eval 'package F1; our $Z = 5; $Z * 2', "\n";
print "4 ", eval 'package D1; our $Z = 5; my $n = "Z"; ${$n}', "\n";
my $d2 = eval 'package D2; $W = 7; my $n = "W"; ${$n}';
print "5 ", $d2, " / ", (defined ${'D2::W'} ? ${'D2::W'} : 'undef'), "\n";
my $q = 5;
print "6 ", eval 'package X8; $q + 1', "\n";
my $x = 5;
eval 'package Cap; sub f { $x } 1' or die $@;
print "7 ", Cap::f() * 11, "\n";
eval 'package X10; $S = 3; 1' or die $@;
print "8 ", eval 'package X10; $S', "\n";
eval 'package V1; our $VERSION = "1.25"; our @ISA = ("Exporter"); 1' or die $@;
print "9 ", ${'V1::VERSION'}, " / ", join(',', @{'V1::ISA'}), "\n";
my $o = eval 'package B7; sub mk { bless {} } mk()';
print "10 ", ref($o), "\n";
});

# ── E4.1 M1 (s353): leading-`my` eval regions — the Sub::Quote shape ────────
# 1 = the faithful write-through (capture value is \$lexical holding a ref;
# the region writes through two levels — Moo installs every accessor this
# way).  2 = the whole named-quote shape: bare-block wrapper, leading `my`s,
# region sub + `$$_UNQUOTED = \&name`, trailing `1;`.  INVERSE guards: a
# leading `my` whose init has a free package variable (3) or a bareword call
# (4) must NOT be swept into the region — perl resolves both in the CALLER.
test_transpile("eval region: leading-my collapse (Sub::Quote shape)", q{
sub install { return eval q!{ my $q = ${$_[1]->{"k"}}; package RG; $$q = 42; } 1;! || die $@ }
my $cell; my $uq = \$cell;
install(undef, { k => \$uq });
print "1 ", $cell, "\n";
sub mk { return eval q!{ my $u = ${$_[1]->{"u"}}; package SQ9; no warnings 'closure'; sub hi { "H-" . ${$_[0]} } $$u = \&hi; } 1;! || die $@ }
my $code; my $slot = \$code;
mk(undef, { u => \$slot });
print "2 ", $code->(\ "X"), "\n";
our $V = "caller"; our $W = "callerW";
my $r2 = eval 'my $v = $V; package M1X; our $V = 9; ($v eq "caller" ? "good" : "bad-$v")' // "refused";
print "3 ", ($r2 eq "good" || $r2 eq "refused" ? "ok" : $r2), "\n";
sub w { "from-w" }
my $r3 = eval 'my $v = w(); package M1Y; $v' // "refused";
print "4 ", ($r3 eq "from-w" || $r3 eq "refused" ? "ok" : $r3), "\n";
});

# ── E4.1 M2 (s353): poisoned cond-my with braced STRING interpolation renames
# natively; a real code-level ${name} deref keeps the v1 route but stays
# correct.  Plus the fat-comma non-idempotence fix: under strict, a hash key
# before => inside a same-name-shadow anon sub must stay a STRING (the old
# destructive `=>`→`,` tree rewrite turned re-parsed keys into calls —
# Moo's { no_install => 1 } became (pl-no_install)).
test_transpile("cond-my braced interp + strict fat-comma keys", q{
use strict;
sub f {
  my ($spec) = @_;
  if (my $name = $spec) { return "_set_${name}"; }
  return "none";
}
$main::name = "G";
print f("x"), " ", $main::name, "\n";
sub defer_sub { my ($n,$c)=@_; return $c }
package P; sub gen { my ($c,$p,$o)=@_; return $o->{no_install} } package main;
my $fx = defer_sub "k" => sub {
  my $fx = P->gen(
    "p", { no_install => 1, no_defer => 2 }
  );
  $fx;
};
print "keys ", $fx->(), "\n";
});

# ── E4.1 M5 (s353): the block static-variable idiom lowers natively — a
# block-scoped `my` captured by a nested named sub, with string evals
# naming the same bare in BOTH scopes.  Asserts the read (a), the eval
# write-through reaching the sub's shared cell (b), and the block-end
# restoration to the outer lexical (c).  Guard row for the per-site
# capture-alist pair (_eval_block_cells + the alist __file__N strip).
test_transpile("M5: block static-var + nested sub + evals in both scopes", q{
my $x = 1;
{
  my $x = 2;
  sub capinner9 { $x }
  print "a ", eval(q{$x}), "\n";
  eval q{$x = 7};
  print "b ", capinner9(), "\n";
}
print "c ", eval(q{$x}), " ", $x, "\n";
});

# ── #252 (s358): the GENERAL forward-goto shape (Text::Balanced's
# _match_tagged) — several standalone labels, gotos that CROSS an intervening
# label, and LIST `my` declarations hoisted past the catch boundary.  One
# snippet covers: jump over the decls (undef read), jump to each label from
# before the first, fallthrough through all labels, cross-label jumps from a
# middle segment, and a hoisted list-decl value surviving into the last
# segment.  INVERSE GUARD in the same snippet: a backward goto to a
# standalone label (classic tagbody machinery) must keep working.
test_transpile("#252: multi-label forward goto + list-decl hoist + backward goto", q{
sub m3 {
    my ($mode, $x) = @_;
    my $extra;
    my ($a, $b) = (10, 20);
    goto failed if $mode eq 'f';
    goto matched if $mode eq 'm';
    $extra = $a + $b;
    goto short if $mode eq 's';
    $extra += 100;
short:
    $extra = ($extra // 0) + 1;
    goto matched if $mode eq 'sm';
    goto failed if $mode eq 'sf';
matched:
    return "M:$x:" . ($extra // 'undef');
failed:
    return "F:$x:" . ($extra // 'undef');
}
print m3($_->[0], $_->[1]), "\n"
  for (['f',1], ['m',2], ['s',3], ['sm',4], ['sf',5], ['z',6]);
my $n = 0;
AGAIN:
$n++;
goto AGAIN if $n < 3;
print "back:$n\n";
});

# ── #252 (s358): interpolation of a SUBSCRIPTED variable whose chain
# continues — "$_[0]->{k}", "$a[0]{k}", "$h{a}[1]" — used to leave the tail
# LITERAL (silent wrong; inside an overloaded '""' it recursed forever:
# Text::Balanced 04_extdel.t).  Plus the list-decl element false self-ref:
# `my ($class) = ($class[$i])` reads @class's element, NOT the (nonexistent)
# outer scalar — the over-fire emitted an unbound package-var read.
# INVERSE GUARDS: single subscripts and plain arrow chains unchanged; a REAL
# scalar self-ref in a list decl still reads the outer value.
test_transpile("#252: interp subscript chains + list-decl element self-ref", q{
sub f { return "got $_[0]->{error} at $_[0]->{pos}" }
print f({error=>"E1", pos=>7}), "\n";
my @a = ({k=>"V", l=>[5,6]});
my %h = (a => {b => "X"}, c => [1,2]);
print "chain $a[0]{k} $a[0]{l}[1] $h{a}{b} $h{c}[0]\n";
print "single $a[0]{k} plain ", join(",", map { $_->{k} // () } @a), "\n";
my @class = ("CA", "CB"); my @func = ("FA", "FB");
my $i = 1;
my ($class, $func) = ($class[$i], $func[$i]);
print "elem $class $func\n";
my $s = "outer";
my ($p, $s2) = ($s, "x");
print "real-selfref $p\n";
});

# ── #153 step 3b (s361): the strictly-1-arg operand site.  Two bugs, one
# family — how many arguments a paren-less call takes, and where the single
# operand TERM ends.
#   * `sub f ($;$)` called without parens used to SILENTLY DROP everything
#     after the first argument: the site read `min_params` (a MINIMUM) as if
#     it were "exactly one".  Prototype `max args` decides it now.
#   * a TRAILING `;` in a prototype gives the sub LIST-operator precedence in
#     perl, so `listop 0 || 5` is `listop(0 || 5)` == 5 — while a plain `($)`
#     is a named unary, so `unary 0 || 5` is `unary(0) || 5` == 0.  PCL used
#     to answer 5 for BOTH (task #147's family).
#   * the operand term now comes from the ONE term-grammar walker, so
#     `getc $$pair[0]` reads the ELEMENT; it used to stop at `$$pair` and
#     read the wrong handle (perl's own t/io/utf8.t:397 is the live case).
# INVERSE GUARD: the bareword-filehandle shape (`close F, 'desc'`) is a
# walker DECLINE and must still take only the handle.
test_transpile("#153/#147: prototype arity, list-op vs unary precedence, 1-arg term extent", q{
sub two ($;$)   { print "two=@_\n" }
sub mymk (_;$)  { print "mymk=@_\n" }
sub listop ($;) { print "listop=$_[0]\n" }
sub unary ($)   { print "unary=$_[0]\n" }
my ($p, $q) = ("x", "y");
two $p, $q;
mymk foo => 0755;
listop 0 || 5;
unary 0 || 5;
my $tf = "/tmp/pcl-t9-getc-probe.txt";
open my $w, ">", $tf or die "w: $!"; print $w "AB"; close $w;
open my $fh, "<", $tf or die "r: $!";
my $pair = [$fh, "A"];
my $c = getc $$pair[0];
print "getc=$c\n";
print "close=", (close $fh ? 1 : 0), " desc\n";
unlink $tf;
});

# ── s361 SILENT WRONG: `foreach` over a SINGLE SCALAR that holds a ref.
# perl flattens a foreach LIST, but a scalar is one element even when it
# holds an array/hash ref.  PCL spread the REFERENT — `for ($r) {…}` ran
# once per element of @$r, with the loop var bound to an ELEMENT — because
# at runtime a box wrapping a vector is indistinguishable from an @array
# box.  The emitter knows the sigil; it now wraps the single scalar in the
# same (vector …) shape a multi-element list already used.
# BOTH lowering sites are covered: the block form (Parser2) and the
# statement-modifier form (`EXPR for ($r)`, the v1 statement seam).
# INVERSE GUARDS, all in one snippet: `@$r`/`@a` must still SPREAD, `\(@a)`
# is perl's DISTRIBUTED ref (a list, one per element), a two-element list is
# unchanged, and aliasing must survive the wrap — `for ($x) { $_ = … }`
# still writes through to $x.
test_transpile("foreach over a single scalar holding a ref is ONE iteration", q{
my @a = ("a","b","c");
my %h = (k => [1,2]);
my $r = \@a;
my $obj = { r => [5,6] };
my ($n1,$n2,$n3,$n4,$n5,$n6,$n7,$n8) = (0) x 8;
for my $x ($r)        { $n1++; print "ref=", ref($x), "\n" }
for my $x (\%h)       { $n2++ }
$n3++ for ($h{k});
$n4++ for ($obj->{r});
$n5++ for (@$r);
$n6++ for (@a);
$n7++ for ($r, $obj);
$n8++ for (\(@a));
print "counts=$n1 $n2 $n3 $n4 $n5 $n6 $n7 $n8\n";
my $s = "orig";
for ($s) { $_ = "written" }
print "alias=$s\n";
my @w = (1,2);
for my $e (@w) { $e *= 10 }
print "spread-alias=@w\n";
});

# ── s363: the same ALIAS write-back, at the shapes d2bb91c's veto missed.
# The `foreach-alias-list` event force-boxes a bare `$name` used as the loop
# LIST, so a write through the loop var lands in the variable.  It fired only
# for the compound/block form and only for a ONE-element list, so both
# `$_ = … for ($s);` (the statement-modifier spelling never reaches that
# branch) and `for ($x, $y) {…}` (two operands) silently dropped the write.
# One helper now vetoes every bare-scalar operand for both spellings.
# INVERSE GUARDS in the same snippet: a non-bare operand (`$h{k}`, a literal)
# is untouched, and the loop still iterates once per operand.
test_transpile("foreach loop-var writes reach every bare scalar operand", q{
my $m = "orig";
$_ = "written" for ($m);
my $m2 = "orig";
$_ = "written" for $m2;
my $m3 = "orig";
$_ = "written" foreach ($m3);
print "mod=$m $m2 $m3\n";
my ($p, $q) = ("orig", "orig");
for ($p, $q) { $_ = "written" }
my ($u, $v) = ("orig", "orig");
$_ = "written" for ($u, $v);
print "two=$p $q $u $v\n";
my $n = 0;
my %h = (k => "keep");
$n++ for ($h{k}, "lit", 3);
print "count=$n keep=$h{k}\n";
});

# ── s363 #264 SILENT WRONG: `${x}` written at CODE level is a use of $x, but
# PPI spells it Cast + Block-holding-one-Word, so there is no Symbol token and
# every Symbol-driven pass was blind to it.  A file lexical used that way after
# a `package` statement read EMPTY: the span CHECKER never saw the use (so it
# did not gate either), and the renamer never rewrote it.  One helper
# (_brace_name_refs) now answers "which canonical variables does this node
# mention that way" for BOTH the detector and the renamer.
# INVERSE GUARDS in the same snippet: `${ $ref }` is a real deref of a SYMBOL
# and must keep working; `@{[ … ]}` is a baby-cart expression, not a name;
# `${main::g}` is a package global, not a lexical; and a `my $s` shadow inside
# a sub must still win over the outer name.
test_transpile("code-level \${x} of a file lexical spans a package boundary", q{
my $s = "S";
my $ref = \$s;
package Foo;
print "1 scalar-brace=${s}\n";
print "2 real-deref=${ $ref }\n";
print "3 babycart=@{[ 1+1 ]}\n";
package main;
our $g = "G";
package Bar;
print "4 global-brace=${main::g}\n";
sub inner { my $s = "LOCAL"; return "5 shadow=${s}" }
print inner(), "\n";
});

# ── s363 #254 B-i: a poisoned condition-`my` whose construct holds a STRING
# EVAL.  The rename mints a LET-BOUND `$i__cond__N` so the package global keeps
# its name; the eval body still says `$i`, and _eval_lexical_alist now strips
# `__cond__N` back to the original key (as it already did for __shadow__ /
# __file__ / __lex__).  What must hold is perl's scoping, both ways: an eval
# INSIDE the construct sees the LEXICAL, one outside sees the GLOBAL.
test_transpile("poisoned condition-my + string eval: eval sees the right binding", q{
our $i = "GLOBAL";
sub show { return "global=$i" }
my @list = (10, 20);
foreach my $n (@list) {
  if (my $i = $n * 2) {
    my $inside = eval '$i';
    print "in-construct eval=$inside\n";
  }
}
print show(), "\n";
my $outside = eval '$i';
print "outside eval=$outside\n";
});

# ── s365 #268: PPI LEXER BUG — an anonymous sub carrying an ATTRIBUTE at the
# START of an expression is tokenized as a LABEL (`sub :lvalue {…}` becomes
# Label('sub :') + Word('lvalue')), and inside a `for` LIST the label even gets
# a STATEMENT of its own, splitting one expression across siblings.  Nothing
# downstream could see an anon sub, so the expression fell through to
# "Missing case: [" and the whole statement was replaced by a PARSE ERROR
# comment — silently dropping code (op/sub_lval.t, which died on the C-style
# `for` branch's continue-die because the mis-lex also made PPI call the loop
# parens a Structure::For).  Mid-expression the same text tokenizes correctly,
# which is why `my $f = sub :lvalue {…}` always worked.
# INVERSE GUARDS in the same snippet: a REAL loop label must still lower as a
# label (`next OUT`), a named sub's attributes must still be dropped without
# disturbing its body, and a plain attribute-free anon sub in a list must be
# untouched — the pass keys on the mis-lexed shape only.
test_transpile("anon sub with attributes in expression position (#268)", q{
my @a = (sub :lvalue { 1 }, sub { 2 }, sub :lvalue :method { 3 });
print "n=", scalar(@a), " v=", join(",", map { $_->() } @a), "\n";
my $one = sub :lvalue { 7 };
print "stmt=", $one->(), "\n";
my $suffix = '';
for my $s (sub :lvalue { 4 }, sub :lvalue { 5 }) {
  print "loop=", $s->(), "$suffix\n";
}
continue { $suffix = ' (second)' }
sub named :lvalue { 9 }
print "named=", named(), "\n";
OUT: for my $i (1, 2) { next OUT if $i == 1; print "label=$i\n" }
});

# ── s365 #265: a sub's OWN `my`, EMBEDDED in another statement, wrote the FILE
# LEXICAL of the same name.  `_captured_in_subs` (the promoter's test) ran its
# own Symbol/ArrayIndex loops before calling the shadow-aware
# `_block_captures_name`, so `$s->symbol eq '$x'` counted the sub's own `my $x`
# uses as a capture of the file `$x`.  The file lexical was then promoted to a
# defvar'd cell, and the embedded-`my` let is deliberately skipped for promoted
# names — so the sub had no lexical at all: it incremented the shared cell,
# state persisted across calls, and the outer variable was clobbered.  Deleting
# the blind loops makes the promoter agree with the GATE, which has used the
# shadow-aware test since M-C.
# INVERSE GUARD in the same snippet: a sub that GENUINELY captures the file
# lexical (no shadow of its own) must still share the one cell — the write
# inside must be visible outside.
test_transpile("a sub's embedded `my` shadows a same-named file lexical (#265)", q{
my $x;
sub f { ++my $x->{k}; return $x->{k} }
print "1=", f(), " 2=", f(), " outer=", (defined($x) ? "def" : "undef"), "\n";
my $y = "OUT";
sub g { my $r = "no"; $r = "yes" if my $y = "IN"; return "$r/$y" }
print "g=", g(), " outer-y=$y\n";
my $shared = 1;
sub bump { $shared++; return $shared }
print "cap=", bump(), " outer-shared=$shared\n";
});

# ── #270: an anon sub's `:prototype(…)` at the START of an expression, where
# the prototype text ends in `$`.  PPI already mis-lexes `sub :ATTR` there as a
# LABEL (#268); on top of that, the prototype's closing `)` is tokenized as the
# MAGIC VARIABLE `$)`, so the attribute's paren group never closes and PPI
# closes it on the SUB's own `)` — swallowing the block.  The #268 repair then
# found no Block at the end of its run and DECLINED SILENTLY, after which the
# whole statement was replaced by a PARSE-ERROR comment: perl printed 42, PCL
# printed nothing and exited 0.
# Covered here: the bare `($)`, the `;`-bearing spellings that lex one layer
# differently again, a chained attribute in front, and the `for`-LIST shape
# (where PPI splits the run across sibling statements) with TWO subs.
# INVERSE GUARDS in the same snippet: `($$)` and `(\@)` lex correctly and must
# keep taking the ordinary announce-and-drop path, and a NAMED sub's
# `:prototype($)` — which lexes correctly anywhere — must still impose its
# prototype on later calls.
test_transpile("anon sub :prototype ending in \$ at expression start (#270)", q{
my @l = (sub :prototype($) { 42 }, sub :prototype(;$) { 43 },
         sub :prototype($;$) { 44 }, sub :method :prototype($) { 45 },
         sub :prototype($$) { 46 }, sub :prototype(\@) { 47 });
print join(",", map { $_->(0) } @l), "\n";
for my $s (sub :prototype($) { "a" }, sub :prototype($) { "b" }) { print $s->(0) }
print "\n";
sub named :prototype($) { return "n:$_[0]" }
my @args = (7, 8);
print named(@args), "\n";
});

# ── #265 second half: an expression-embedded `my` INSIDE A NAMED SUB whose name
# another sub also mentions.  `_lower_block` refuses to let-bind such a decl on
# the theory that the other sub shares the forward-defvar'd global as its cell —
# true at FILE level, scope-blind inside a sub body, where the other sub cannot
# see this sub's lexical.  `++my $x->{foo}` then WROTE THE GLOBAL: the hash
# persisted across calls (perl re-makes it per call) and clobbered the global
# (op/my.t t47, [perl #29340]'s companion).  Narrowing the veto alone would be
# wrong — the let registers `$x` in _seg_lex and suppresses the GLOBAL's defvar,
# stranding the other sub — so the decl is RENAMED to `$x__emb__N` pre-analysis.
# INVERSE GUARDS in the same snippet: (a) the FILE-level shape the veto exists
# for — a sub genuinely closing over `open my $fh` — must still share one cell;
# (b) the package global must keep its value and its defvar across the renamed
# sub; (c) a string eval inside the sub must still reach the renamed lexical by
# its ORIGINAL name (the __emb__ suffix strip in _eval_lexical_alist).
test_transpile("embedded `my` in a sub vs a same-named package global (#265)", q{
sub setter { ($x, $y) = ("A", "B") }
sub foo3 {
    ++my $x->{foo};
    print "bar-defined=", (defined $x->{bar} ? 1 : 0), "\n";
    ++$x->{bar};
}
setter(); foo3(); foo3();
print "global x=$x y=$y\n";
my $tmp = "/tmp/pcl265.$$.txt";
open my $fh, '>', $tmp or die;
sub w { print $fh "shared\n" }
w(); close $fh;
open my $in, '<', $tmp or die; print "file=", scalar(<$in>); close $in; unlink $tmp;
sub setter3 { $e = "GLOBAL" }
sub emb3    { ++my $e->{k}; return eval '$e->{k}' }
setter3(); print "emb3=", emb3(), " global e=$e\n";
});

# ── #272 (s372): the same veto was still SCOPE-BLIND for an ANON sub body.  The
# rename pass keyed on "inside a NAMED sub", but the question the scope asks is
# only "inside SOME sub's body" — no other sub can see a lexical declared in one.
# So `my $f = sub { ++my $x->{foo} }` fell back to the package global another sub
# had written and died on it (type-error in P-GETHASH-BOX, not even a wrong
# value).  One predicate widened, `_enclosing_named_sub` -> `_enclosing_sub_body`
# (prototype/attribute-skipping anon-`sub` block recogniser, the shape
# _state_decl_route already uses).
# INVERSE GUARDS in the same snippet, all probed against perl: (a) the FILE-level
# shape the veto exists for still shares one cell (#199); (b) an anon sub NESTED
# in a named sub; (c) a map BLOCK is not a sub body, so its `my` is untouched;
# (d) BEGIN is Scheduled, not a sub body.
test_transpile("embedded `my` inside an ANON sub body (#272)", q{
sub setter { ($x, $y) = (7, 8) }
my $anon = sub { ++my $x->{foo}; return $x->{foo} };
setter();
print "anon=", $anon->(), $anon->(), " global x=$x\n";
my $fh2;
sub reader { return defined($fh2) ? "shared:$fh2" : "shared:undef" }
$fh2 = "FH"; print "keep=", reader(), "\n";
sub setz  { $z = "GLOBAL" }
sub outer { my $g = sub { ++my $z->{k}; return $z->{k} }; return $g->() }
setz(); print "nested=", outer(), " z=$z\n";
sub setw { $w = 5 }
setw(); my @r = map { my $w = $_ * 2; $w } (1,2);
print "map=@r w=$w\n";
});

# (#239) `package X;` INSIDE A BLOCK re-homes every unqualified package
# variable for the rest of that block.  v2 emits a block as ONE top-level CL
# form and the reader interns its symbols before the nested `(in-package :X)`
# ever runs, so eight of the nine block kinds wrote the ENCLOSING package's
# variable; only the plain bare block (the one shape D1-lite splits into
# separate top-level forms) was right.  All nine kinds in one snippet — each
# row costs a perl run and an SBCL run.
test_transpile("in-block `package X;` re-homes globals in every block kind (#239)", q{
no strict 'refs';
sub w { my $t = shift; join("", map { "$_=" . (defined(${"P::$_"}) ? ${"P::$_"} : "U")
        . "/" . (defined(${"main::$_"}) ? ${"main::$_"} : "U") . ";" } ($t)) }
{ package P; $bare = "V"; }
eval { package P; $ev = "V"; };
do { package P; $do = "V"; };
sub f { package P; $sb = "V"; } f();
BEGIN { package P; $bg = "V"; }
if (1) { package P; $if = "V"; }
my $n = 0; while ($n++ < 1) { package P; $wh = "V"; }
L: { package P; $lb = "V"; }
my @s = sort { package P; $so = "V"; $a <=> $b } (2,1);
print w($_) for qw(bare ev do sb bg if wh lb so);
print "\n";
# NESTED switch: the inner region wins, and the outer one does not reach past it
eval { package P; $out = "1"; { package Q; $in = "2"; } };
print "P::out=$P::out Q::in=$Q::in main::out=", (defined($main::out)?$main::out:"U"), "\n";
# CONTAINERS + interpolation + a nested sub BODY are all inside the region
eval { package P; @ar = (1,2); %h = (k=>"H"); $txt = "@ar|$ar[1]|$#ar|$h{k}"; };
eval { package P; sub setv { $viasub = "S" } }; P::setv();
print "ar=@P::ar txt=$P::txt viasub=$P::viasub\n";
});

# The four-way resolver, all inverse guards (the cases requalification must
# NOT touch).  The `our` row is the s377 review's mandatory guard: it diverged
# the OPPOSITE way (PCL wrote Bar::x where perl writes main::x) in the
# bare-block path that always looked correct.
test_transpile("in-block package switch leaves lexicals, our-aliases and specials alone (#239)", q{
no strict 'refs';
our $ali = "OUR";
my  $lex = "LEX";
{ package Bar; $ali = "W"; $seen = $lex; my $inner = "I"; $got = $inner; }
print "ali:main=$main::ali Bar=", (defined(${"Bar::ali"}) ? ${"Bar::ali"} : "U"),
      " seen=$Bar::seen got=$Bar::got\n";
# specials stay in main whatever package is in effect
$_ = "UND"; $ENV{PCL239} = "E";
{ package Baz; $sawund = $_; $sawenv = $ENV{PCL239}; @ARGV = ("A"); }
print "und=$Baz::sawund env=$Baz::sawenv argv=@ARGV\n";
# $a/$b are the sort lowering's own bindings — a switch must not move them.
# Measured s378: requalifying them takes Sort-Versions versions.t from 65/31
# to 96/0 AND makes every one of these three comparators return the list
# UNSORTED, because the emitted (lambda ($a $b) ...) binds the SECTION's two
# symbols while the body would read X::$a.  Both halves are the trade.
{ package Sortish; my @o = sort { $a <=> $b } (3,1,2); print "sorted=@o\n"; }
eval { package SoE; my @o = sort { $a <=> $b } (3,1,2); print "evalsort=@o\n"; };
do   { package SoD; my @o = sort { $a <=> $b } (5,4);   print "dosort=@o\n"; };
sub sof { package SoF; my @o = sort { $a <=> $b } (9,7); print "subsort=@o\n"; } sof();
# a `local` inside the region localizes the REQUALIFIED global
$Loc::v = "outer";
sub peek { $Loc::v }
{ package Loc; local $v = "inner"; print "local=", main::peek(), "\n"; }
print "after=$Loc::v\n";
});

# (s379 review) The resolver's two false verdicts on head elements:
#  - a SIGNATURE binds its parameters with no my/state/our keyword, so the
#    body's $x was requalified to $X::x while the seam still bound the plain
#    lexical (perl 42, PCL 1 — silent wrong value).  A Symbol inside a
#    DEFAULT expression is a use, not a parameter, and a PURE prototype
#    (`($$)`) binds nothing;
#  - `my`/`our` as a fat-comma key or bare hash subscript is a STRING, not a
#    declarator — it died "unclassifiable `my` declarator" on legal perl.
test_transpile("signature params and keyword-as-hash-key vs package-switch resolver (s379)", q{
use feature 'signatures'; no strict; no warnings;
sub f ($x, $y = $x + 1) { package P; "got:$x:$y" }
print f(41), "\n";
print "P::x=", (defined $P::x ? $P::x : "U"), "\n";
%h = (my => 1, our => 2, state => 3);
if ($h{my} + $h{our} + $h{state}) { package Q; $z = "ok"; }
print "z=", (defined $Q::z ? $Q::z : "U"), "\n";
});

done_testing();
