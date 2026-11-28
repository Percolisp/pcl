#          -*-Mode: CPerl -*-
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Test s/// substitution and tr/// transliteration

use v5.32;
use strict;
use warnings;

use lib ".";

use FindBin ();
use lib "$FindBin::RealBin/../..";
use Pl::VendorPPI;   # PPI, from vendor/PPI/lib (s527a)
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use Test::More;

BEGIN { use_ok('Pl::PExpr') };
BEGIN { use_ok('Pl::ExprToCL') };


# Helper: convert Perl to CL
sub perl_to_cl {
  my $code    = shift;

  my $doc     = PPI::Document->new(\$code);
  my @stmts   = $doc->children();
  my @exprs   = $stmts[0]->children();

  my $expr_o  = Pl::PExpr->new(e => \@exprs, full_PPI => $doc);
  my $node_id = $expr_o->parse_expr_to_tree(\@exprs);
  $expr_o->annotate_contexts($node_id);

  my $gen     = Pl::ExprToCL->new(expr_o => $expr_o);
  return $gen->generate($node_id);
}

sub test_codegen {
  my $perl_code = shift;
  my $expected  = shift;
  my $desc      = shift // "Perl: $perl_code";

  my $result = perl_to_cl($perl_code);
  is($result, $expected, $desc);
}


# ============================================================
diag "";
diag "-------- Basic s/// substitution (standalone - implicit \$_):";

# Standalone s/// gets wrapped with '$_ =~' for implicit target
test_codegen('s/foo/bar/',
             '(p-=~ $_ (p-subst :pat "foo" :rep "bar" :flags "" :tier :native))',
             's/foo/bar/ basic (implicit $_)');

test_codegen('s/hello/world/g',
             '(p-=~ $_ (p-subst :pat "hello" :rep "world" :flags "g" :tier :native))',
             's///g global (implicit $_)');

test_codegen('s/UPPER/lower/i',
             '(p-=~ $_ (p-subst :pat "UPPER" :rep "lower" :flags "i" :tier :native))',
             's///i case insensitive (implicit $_)');

test_codegen('s/pattern/replace/gi',
             '(p-=~ $_ (p-subst :pat "pattern" :rep "replace" :flags "gi" :tier :native))',
             's///gi multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- s/// with binding operator:";

test_codegen('$str =~ s/old/new/',
             '(p-=~ $str (p-subst :pat "old" :rep "new" :flags "" :tier :native))',
             '$str =~ s///');

test_codegen('$str =~ s/a/b/g',
             '(p-=~ $str (p-subst :pat "a" :rep "b" :flags "g" :tier :native))',
             '$str =~ s///g');

test_codegen('$str !~ s/x/y/',
             '(p-!~ $str (p-subst :pat "x" :rep "y" :flags "" :tier :native))',
             '$str !~ s///');


# ============================================================
diag "";
diag "-------- s/// with more modifiers (standalone - implicit \$_):";

test_codegen('s/pat/rep/s',
             '(p-=~ $_ (p-subst :pat "pat" :rep "rep" :flags "s" :tier :native))',
             's///s single-line (implicit $_)');

test_codegen('s/pat/rep/m',
             '(p-=~ $_ (p-subst :pat "pat" :rep "rep" :flags "m" :tier :native))',
             's///m multi-line (implicit $_)');

test_codegen('s/pat/rep/x',
             '(p-=~ $_ (p-subst :pat "pat" :rep "rep" :flags "x" :tier :native))',
             's///x extended (implicit $_)');

test_codegen('s/pat/rep/gimsxe',
             '(p-=~ $_ (p-subst :pat "pat" :rep (lambda () (p-scalar-ctx (pl-rep))) :flags "egimsx" :tier :native))',
             's/// all common modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- Basic tr/// transliteration (standalone - implicit \$_):";

test_codegen('tr/a-z/A-Z/',
             '(p-=~ $_ (p-tr :from "a-z" :to "A-Z" :flags ""))',
             'tr/a-z/A-Z/ uppercase (implicit $_)');

test_codegen('tr/A-Z/a-z/',
             '(p-=~ $_ (p-tr :from "A-Z" :to "a-z" :flags ""))',
             'tr/A-Z/a-z/ lowercase (implicit $_)');

test_codegen('y/abc/xyz/',
             '(p-=~ $_ (p-tr :from "abc" :to "xyz" :flags ""))',
             'y/// synonym for tr/// (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with modifiers (standalone - implicit \$_):";

test_codegen('tr/aeiou//d',
             '(p-=~ $_ (p-tr :from "aeiou" :to "" :flags "d"))',
             'tr///d delete (implicit $_)');

test_codegen('tr/a-z//c',
             '(p-=~ $_ (p-tr :from "a-z" :to "" :flags "c"))',
             'tr///c complement (implicit $_)');

test_codegen('tr/a-z//s',
             '(p-=~ $_ (p-tr :from "a-z" :to "" :flags "s"))',
             'tr///s squash (implicit $_)');

test_codegen('tr/a-z/A-Z/cds',
             '(p-=~ $_ (p-tr :from "a-z" :to "A-Z" :flags "cds"))',
             'tr/// multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with binding:";

test_codegen('$str =~ tr/a-z/A-Z/',
             '(p-=~ $str (p-tr :from "a-z" :to "A-Z" :flags ""))',
             '$str =~ tr///');

test_codegen('$count = ($str =~ tr/x//)',
             '(p-scalar-= $count (p-=~ $str (p-tr :from "x" :to "" :flags "")))',
             'Count chars with tr///');


diag "";
diag "-------- Named captures %+:";

# $+{name} in code should access the %+ hash
test_codegen('$+{year}',
             '(p-gethash %+ "year")',
             '$+{name} generates hash access on %+');

# $+{name} in string interpolation — wrapped in p-string-concat so the captured
# value is stringified (interpolation always stringifies its single part).
test_codegen('"$+{year}"',
             '(p-string-concat (p-gethash %+ "year"))',
             '"$+{name}" interpolates via p-gethash');

diag "";
diag "All s/// and tr/// tests completed!";

diag "";
diag "-------- heredoc inside \${\\ …} inside an s///e replacement (task #342):";

# The construct t/base/lex.t is built around: a heredoc OPENED inside an
# interpolation block, with the body inside the s/// delimiters.  PPI lexes
# `"${\<<END}"` as one Quote::Double token and never sees the opener, so the
# body and terminator were left as loose code the expression parser refused.
# perl is the oracle: the same source through both.
{
    my $root = "$RealBin/../..";
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'PL';
my $test = 42;
my $v = "V";
$_ = "";
s|(?:)|"${\<<END}"
ok $test - heredoc in "" in multiline s///e outside eval
END
|e;
print $_ || "not ok $test\n";
$_ = "cYd";
s|Y|"${\<<'RAW'}"
raw $v
RAW
|e;
print $_;
PL
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $pcl_out  = `$root/runpcl $file 2>&1`;
    is($pcl_out, $perl_out,
       '#342: heredoc in ${\ …} in an s///e replacement — same as perl')
      or diag("perl: [$perl_out]\nPCL:  [$pcl_out]");
    like($perl_out, qr/heredoc in "" in multiline/,
         '#342: …and the oracle really did substitute the heredoc');
}

diag "";
diag "-------- s///e replacement that cannot be compiled must DIE (task #342):";

# RULE 12: `s/…/EXPR/e` replaces matched text with EXPR's VALUE, so a
# replacement the compiler cannot build must not become nil — that substituted
# the empty string, exit 0, with only a warning nobody reads.  The trigger is
# VALID Perl that PCL genuinely cannot compile (smartmatch — a documented
# feature absence, Track A of Option B phase 2): perl prints 1 here.
{
    my $root = "$RealBin/../..";
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'PL';
no warnings;
$_ = "z";
s|z|my $q = 1; $q ~~ [1]|e;
print $_, "\n";
PL
    close $fh;
    my $err = "$file.err";
    my $out = `$root/pl2cl $file 2>$err`;
    my $rc  = $? >> 8;
    my $msg = do { open my $e, '<', $err or die; local $/; <$e> };
    unlink $err;
    # s526w2 (#3855): a replacement holding STATEMENTS is retried as
    # `do { ... }`, so the smartmatch statement now lowers to the ONE ruled
    # drop shape (s433: a perl-shaped run-time die at the drop site) and the
    # transpile succeeds.  Rule 12 is asserted where it now holds -- the RUN
    # dies naming the statement and never prints a substituted value.
    my $run_out = `$root/runpcl $file 2>$err`;
    my $run_rc  = $? >> 8;
    my $run_err = do { open my $e, '<', $err or die; local $/; <$e> };
    unlink $err;
    isnt($run_rc, 0, 's///e with an uncompilable statement in its replacement dies when run (rule 12)');
    like($run_err, qr/smart match \(~~\) is not supported/,
         '... and says which statement it could not compile');
    is($run_out, '', '... and prints nothing: the substitution never happens');
    unlike($out, qr/lambda \(\) nil/,
           '... and emits no nil replacement thunk');
}

# ============================================================
# Task #492 (s443g): WHICH REPLACEMENTS INTERPOLATE IS THE SCANNER'S ANSWER.
#
# `_replacement_interpolates` used to ask a private
# `(?<!\\)[\$\@][a-zA-Z_{]` of its own instead of Pl::InterpScan, and every
# spelling that class could not see went out as LITERAL TEXT — silently.  The
# reported one is the non-ASCII identifier (`s/Ｘ/$ｉ/`, guarded in
# Pl/t/utf8-source-01.t because it needs `use utf8`); these three are the same
# bug in ASCII, each probed against perl 5.40.3:
#
#   $::qq     a qualified name — `:` is not in the class          (was [$::qq])
#   $#arr     the last-index sigil — `#` is not in the class      (was [$#arr])
#   \\$x      an ESCAPED BACKSLASH before a sigil — the lookbehind
#             read the second `\` as escaping the `$`             (was [\$x])
#
# $1..$9 stay literal in the EMISSION: they are served by the runtime's own
# backref rewrite, which is the gate's one remaining deliberate narrowness.
# (The punctuation magics this comment used to call "a separate open hole" are
# fixed — task #520, §520 below.)
{
    my $root = "$RealBin/../..";
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'PL';
our $qq = "Q";
my @arr = (1,2,3);
my $x = "V";
my $a = "mAn"; $a =~ s/A/[$::qq]/;
my $b = "mAn"; $b =~ s/A/[$#arr]/;
my $c = "mAn"; $c =~ s/A/[\\$x]/;
my $d = "mAn"; $d =~ s/A/[\$x]/;
my $e = "mAn"; $e =~ s/(A)/[$1]/;
print "$a $b $c $d $e\n";
PL
    close $fh;
    my $want = `perl $file`;
    chomp(my $got = `$root/runpcl $file 2>&1`);
    chomp(my $exp = $want);
    is($got, $exp, "s/// replacement: \$::name, \$#array and \\\\\$x agree with perl (#492; perl: $exp)");
}

# ============================================================
# Tasks #520 / #522 / #521 (s450v): THE THREE THINGS AN s/// REPLACEMENT
# COULD NOT SAY.  Every row runs the same program under real perl and under
# PCL and compares — perl is the expectation, so a row cannot encode a wrong
# answer of ours.
#
#   #520  the PUNCTUATION MAGICS came out as literal text: `s/A/[$&]/` on
#         "xAy" gave `x[$&]y`.  TWO-SIDED — widening the gate alone puts them
#         on the lambda path, where `$\`` / `$'` / `$+` / `$^N` / `@-` were
#         EMPTY because that path set only `$&` and `$1..$9` by hand.  The
#         runtime half (p-subst calls the shared set-capture-groups +
#         set-match-vars, as the m// path always did) is what makes the gate
#         half correct.
#   #522  `\U` `\L` `\u` `\l` `\Q` `\E` stayed literal.  They are dq-string
#         operators, so the replacement is routed to the dq compiler that
#         already implements them (rule 11) — including with no variable in
#         the text at all.
#   #521  `s/A/${\ "L"}/` emitted an unreadable form and killed the whole
#         file at LOAD.  A fragment lifted out of a dq construct carries the
#         ESCAPED DELIMITER, and only that escape is undone.
#
# And the INVERSE, which the #520/#522 widening would otherwise have broken:
# a SINGLE-QUOTED replacement interpolates NOTHING — not a variable, not a
# magic, not a case shift, and not even `$1`/`\1`.
sub subst_agrees {
    my ($program, $desc) = @_;
    my $root = "$RealBin/../..";
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $program;
    close $fh;
    chomp(my $exp = `perl $file`);
    chomp(my $got = `$root/runpcl $file 2>&1`);
    is($got, $exp, "$desc (perl: $exp)");
}

sub subst_agrees_stdout {
    my ($program, $desc) = @_;
    my $root = "$RealBin/../..";
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $program;
    close $fh;
    chomp(my $exp = `perl $file 2>/dev/null`);
    chomp(my $got = `$root/runpcl $file 2>/dev/null`);
    is($got, $exp, "$desc (perl: $exp)");
}

# #692 (closed s543c; fixed by an unknown earlier change, verified on 7ea827a6):
# case shifts are a STACK -- `\E` pops one level -- also once an interpolated
# variable splits the text (it ended ALL shifting: `xa y` for perl's `xA Y`),
# in a dq string and in an s/// replacement; `\Q` nested in `\U` too.
subst_agrees(<<'PL', '#692: a nested case shift around an interpolation pops back to the outer one');
my $t = "A"; print "[\LX\U$t\E Y\E]\n";
my $x = "mAn"; $x =~ s/(A)/[\LX\U$1\E Y\E]/; print "$x\n";
print "[\LX\UaB\E cD\E] [\UX\Lab\E CD\E]\n";
my $q = "a.b"; print "[\U\Q$q\E x\E]\n";
my $y = "mAn"; $y =~ s/(A)/[\U\Q$1.\E x\E]/; print "$y\n";
my $u = "b"; print "[\Ua\L$u\EC\E]\n";
PL

subst_agrees(<<'PL', 's/// replacement: the punctuation magics are live (#520)');
my $a = "xAy";  $a =~ s/A/[$&]/;
my $b = "xAy";  $b =~ s/A/[$`|$']/;
my $c = "xAy";  $c =~ s/(A)/[$+]/;
my $d = "xbyc"; $d =~ s/(b)|(c)/-$^N/g;
my $e = "x1y";  $! = 2; $e =~ s/1/[$!]/;
my $f = "x123y"; $f =~ s/(1)(2)(3)/$#- (@-)/;
my $g = "aXbXc"; $g =~ s/X/($`|$')/g;
print "$a $b $c $d $e $f $g\n";
PL

subst_agrees(<<'PL', 's/// replacement: the case-shift escapes are the dq ones (#522)');
my $a = "mAn";  $a =~ s/(A)/[\U$1\E]/;
my $b = "mABn"; $b =~ s/(AB)/[\L$1\E]/;
my $c = "mabn"; $c =~ s/(ab)/[\u$1]/;
my $d = "mABn"; $d =~ s/(AB)/[\l$1]/;
my $e = "mAn";  $e =~ s/A/[\Uab\E]/;
my $f = "mAn";  $f =~ s/A/[\Eab]/;
my $g = "mAn";  $g =~ s/(A)/\Q$1.b\E/;
my $h = "mabcn"; $h =~ s/(a)(bc)/[\U$1\E$2]/;
print "$a $b $c $d $e $f $g $h\n";
PL

subst_agrees(<<'PL', 'a ${ EXPR } block in a replacement or a dq string (#521)');
my $a = "mAn"; $a =~ s/A/${\ "L"}/;
my $b = "mAn"; $b =~ s/A/${\ 7}/;
my $c = "mAn"; $c =~ s/A/${\ q(L)}/;
my $d = "mAn"; $d =~ s/A/${\ uc("l")}/;
my $e = "mAn"; $e =~ s/A/@{[ "L" ]}/;
my $f = "X${\ \"L\"}Y";
my $g = "X${\ \"a\\tb\"}Y";
my $h = "X@{[ \"a\\tb\" ]}Y";
print "$a $b $c $d $e $f $g $h\n";
PL

subst_agrees(<<'PL', "a SINGLE-QUOTED s''' interpolates nothing, on either side");
my $x = "V";
my $a = "mAn"; $a =~ s'A'[$x]';
my $b = "mAn"; $b =~ s'(A)'[$1]';
my $c = "mAn"; $c =~ s'(A)'[\1]';
my $d = "mAn"; $d =~ s'A'[\Uab\E]';
my $e = "mAn"; $e =~ s'A'[$&]';
my $f = "mAn"; $f =~ s'A'a\tb';
my $g = "mAn"; $g =~ s'A'a\'b';
my $h = "mAn"; $h =~ s{A}'[$x]';
my $i = 'm$xn'; $i =~ s'$x'Q';
print "$a $b $c $d $e $f $g $h $i\n";
PL

# Task #694: WHICH construct's delimiter decides.  #521's un-escape is right
# only where the source really was `"`-delimited — the dq string and the
# manufactured `"…"` token an s/// replacement travels in.  A heredoc, a
# `qq{…}`, a backtick command and a `<…>` glob reach the same code wrapped in
# a SYNTHETIC `"…"` token over their RAW text, where `\"` is the reference
# operator followed by a string, not an escaped quote — un-escaping it made
# `${\"L"}` into the symbolic ref `${ "L" }` and the interpolation came out
# EMPTY (t/op/exec.t row 32, `ls` → `l`).  Rows a/b/c are the escaped side and
# must not move; d–h are the raw side.  Measured on 718db6b: d, e, h and the
# qx row answer `XY`.
subst_agrees(<<'PL', 'the ORIGINAL delimiter decides whether a ${…} fragment is un-escaped (#694)');
my $a = "mAn"; $a =~ s/A/${\"L"}/;
my $b = "mAn"; $b =~ s/A/${\ "L"}/;
my $c = "X${\ \"L\"}Y";
my $d = qq{X${\"L"}Y};
my $e = <<"H";
X${\"L"}Y
H
chomp $e;
my $f = <<'H';
X${\"L"}Y
H
chomp $f;
my $g = qq{X${\ "L"}Y};
my $h = `echo X${\"L"}Y`; chomp $h;
print "a=$a b=$b c=$c d=$d e=$e f=$f g=$g h=$h\n";
PL

# Task #2372: a pattern that cannot COMPILE is a trappable DIE at each of the
# three ops -- it used to be a warning plus a VALUE (match: no-match, split:
# the whole string as one field, s///: "no substitution"), and the program
# carried on with it.  The pattern is interpolated, so perl too dies at the op
# (a literal one dies at perl's compile time, before anything prints).  Only
# the PLACE is asserted -- the message text is free (USER s494) -- plus that
# $@ names the regex and the program goes on after the eval.
subst_agrees(<<'PL', 'a bad pattern DIES at the match, trappably (#2372)');
my $p = "(";
my $r = eval { "a(" =~ /$p/; 1 };
print defined $r ? "survived" : "died", " ", ($@ =~ /in regex/ ? "regex" : "?"), " after\n";
PL
subst_agrees(<<'PL', 'a bad pattern DIES at split, trappably (#2372)');
my $p = "[a";
my @f = ("untouched");
my $r = eval { @f = split /$p/, "x[ay"; 1 };
print defined $r ? "survived" : "died", " ", ($@ =~ /in regex/ ? "regex" : "?"), " @f\n";
PL
subst_agrees(<<'PL', 'a bad pattern DIES at s///, trappably (#2372)');
my $p = "a)";
my $s = "xa)y"; my $n = "unset";
my $r = eval { $n = ($s =~ s/$p/Q/); 1 };
print defined $r ? "survived" : "died", " ", ($@ =~ /in regex/ ? "regex" : "?"), " $n $s\n";
PL

# Two perl-VALID spellings the #2372 die surfaced (they used to warn and never
# match; now they would die where perl does not): the USELESS inline flags
# c g o (perl warns "Useless (?c)" and ignores them) and perl 5.38's
# OPTIMISTIC code block `(*{…})`, stripped like `(?{…})`.  s496a.
# STDOUT only: the strip is ANNOUNCED on the transpile's stderr (#874), as for (?{...}).
subst_agrees_stdout(<<'PL', 'useless (?c)/(?g)/(?o) flags are ignored and (*{...}) is stripped, as (?{...}) is (#2372)');
no warnings;
my @r;
for my $p ('(?c)a', '(?-c)a', '(?g-o)a', '(?ogc)a', '(?ic:A)', '(?o-cg)a', '(?i-c)A', 'x(?c:y)') {
  push @r, eval { ("xa" =~ /$p/ || "xy" =~ /$p/) ? 1 : 0 } // "died";
}
my $count = 0;
push @r, (('aaa') =~ /(.*)(*{ $count++ })[bc]/) ? 1 : 0;
push @r, ("\\(?c)" =~ /\\\(\?c\)/) ? 1 : 0;
print "@r\n";
PL

# A /x qr whose text ENDS inside a `#` comment stringifies with perl's trailing
# newline, or interpolating it comments out the wrapper's own `)` (a compile
# error that #2372 made a die: t/re/pat_advanced.t line 753).  s496a.
subst_agrees(<<'PL', 'a /x qr ending in a # comment stringifies with a newline and interpolates (#2372)');
my $R = qr / A B C # D E/x;
print "[$R]\n";
print join(" ", ("ABCDE" =~ /($R)/ && $1 eq "ABC") ? 1 : 0, ("ABCDE" =~ m/$R/ && $& eq "ABC") ? 1 : 0), "\n";
my $T = qr/a [#] b/x; my $U = qr/a \# b/x; my $V = qr/a (?#c) b/x; my $S = qr/a#b/;
print "[$T][$U][$V][$S]\n";
PL

# The emission promises, in both directions: a `$1`-only replacement stays a
# STRING (the runtime's backref rewrite, no lambda per match), and a
# single-quoted one is a lambda over a constant — never that string, because
# the string path is exactly what would read its `$1` as a register.
test_codegen('s/(a)/[$1]/',
             '(p-=~ $_ (p-subst :pat "(a)" :rep "[$1]" :flags "" :tier :native))',
             'a $1-only replacement is still emitted as a plain string');
test_codegen(q{s'(a)'[$1]'},
             '(p-=~ $_ (p-subst :pat "(a)" :rep (lambda () "[$1]") :flags "" :tier :native))',
             "a single-quoted replacement is a lambda over the literal text");
test_codegen('s/(a)/[$&]/',
             '(p-=~ $_ (p-subst :pat "(a)" :rep (lambda () (p-string-concat "[" |$&| "]")) :flags "" :tier :native))',
             'a punctuation magic takes the lambda path (#520)');

# s496a member 7 (Fable review ruling, s499): qr// compiles EAGERLY, as perl
# does -- a bad pattern dies AT THE qr, so `eval { qr/$p/ }` answers undef with
# $@ set and the program goes on; it used to hand back an object that died
# later at the match, outside the eval.  A good qr keeps its flags, and a
# user-defined property is resolved in the package current at the qr.
subst_agrees(<<'PL', 'a bad interpolated qr DIES at the qr, trappably; the program goes on (#2372 member 7)');
my $p = "(";
my $q = eval { qr/$p/ };
print defined $q ? "object" : "undef", " ", ($@ =~ /in regex/ ? "regex" : "?"), "\n";
my $never = eval { my $unused = qr/a$p/; 1 };
print defined $never ? "survived" : "died", " unused\n";
print "b" =~ /b/ ? "after\n" : "no\n";
PL
subst_agrees(<<'PL', 'an eagerly compiled qr keeps its flags i/x/s/m and interpolates into a bigger pattern (#2372 member 7)');
my $t = "i";
my $i = qr/a${t}c/i; my $x = qr/a b  c/x; my $s = qr/a.b/s; my $m = qr/^b/m;
print join(",", ("AIC" =~ $i ? 1 : 0), ("abc" =~ $x ? 1 : 0), ("a\nb" =~ $s ? 1 : 0),
                ("a\nb" =~ $m ? 1 : 0), ("xAICy" =~ /^x${i}y$/ ? 1 : 0)), "\n";
PL
subst_agrees(<<'PL', 'a qr naming a user-defined property resolves it in the package current AT THE qr (#2372 member 7)');
package Foo;
sub IsFooProp { "0061\t0063\n" }
our $fq = qr/^\p{IsFooProp}+$/;
package main;
print join(",", ("abc" =~ $Foo::fq ? 1 : 0), ("abd" =~ $Foo::fq ? 1 : 0)), "\n";
PL

subst_agrees(<<'PL', 'a qr naming a user-defined property whose sub is not defined YET is deferred to its first match, as in perl (#2372 member 7)');
my $q;
BEGIN { $q = qr/^\p{IsLaterProp}+$/; }
my $u;
BEGIN { $u = qr/\p{IsNeverProp}/; }
print "made ", join(",", ("abc" =~ $q ? 1 : 0), ("abd" =~ $q ? 1 : 0)), "\n";
print eval { "a" =~ $u; 1 } ? "lived\n" : "died " . ($@ =~ /IsNeverProp/ ? "names it" : "?") . "\n";
sub IsLaterProp { "0061\t0063\n" }
PL

subst_agrees(<<'PL', 'user-defined properties: a -hhhh line excludes, the sub is called ONCE per name, and a not-yet-defined sub wins over a same-spelled perl property at the first match (#2372 member 7, #2387)');
our $calls;
my $q;
BEGIN { $q = qr/\p{InLatin1}/; }
sub InLatin1 { "0100\t10FFFF\n" }
sub IsDigitsBut5 { $calls++; "0030\t0039\n-0035\n" }
print join(",", ("\x{100}" =~ $q ? 1 : 0), ("\x{e9}" =~ $q ? 1 : 0)), "\n";
print join(",", ("4" =~ /\p{IsDigitsBut5}/ ? 1 : 0), ("5" =~ /\p{IsDigitsBut5}/ ? 1 : 0),
                ("5" =~ /\P{IsDigitsBut5}/ ? 1 : 0)), " calls=$calls\n";
PL

# Task #2380 (s499e): a POSSESSIVE quantifier is perl's own atomic group --
# `X++` = `(?>X+)`, `X*+` = `(?>X*)`, `X?+` = `(?>X?)`, `X{n,m}+` =
# `(?>X{n,m})` (perlre).  cl-ppcre has no possessive, so the runtime's rewrite
# scan wraps the quantified ATOM (a character, an escape, a class, a group).
# It used to DIE at the match (since #2372; before that it never matched), and
# at the qr since s496a member 7 -- t/re/pat_rt_report.t lost 2,321 rows to
# one `qr/(\x{100}++)/`.  The semantic pair: "aaa" =~ /a++a/ is 0, /a+a/ is 1.
subst_agrees(<<'PL', 'possessive quantifiers match as perl\'s atomic group, on every atom shape (#2380)');
sub t { my $r = eval { $_[0]->() ? 1 : 0 }; defined $r ? $r : "died" }
print join(",", map { t($_) } sub { "aab" =~ /^a++b/ }, sub { "aab" =~ /^[a]++b/ },
    sub { "aa" =~ /^a++a/ }, sub { "ab" =~ /^\w++$/ }, sub { "aaa" =~ /a++a/ },
    sub { "aaa" =~ /a+a/ }, sub { "abc" =~ /^.*+c/ }, sub { "a" =~ /^a?+a/ },
    sub { "xxx" =~ /^x{2,}+x/ }, sub { "xxxx" =~ /^x{2,3}+x$/ }, sub { "abab" =~ /^(?:a|b)*+b/ },
    sub { "abab" =~ /^(ab)++$/ && $1 eq "ab" }, sub { "ab" =~ /^\p{L}++$/ },
    sub { "aa" =~ /^(?=a++a)/ }, sub { "AAb" =~ /^a++b/i }, sub { "a)ba)b" =~ /^(a[)]b)++$/ },
    sub { "aaa" =~ /^(a)\1++$/ }), "\n";
my $q = qr/(\x{100}++)/;
print "made ", ("\x{100}\x{100}" =~ $q ? length $1 : "no"), "\n";
my $s = "aaab"; $s =~ s/a++/X/;
print "$s ", join("|", split /,++/, "a,,b,c"), "\n";
PL
subst_agrees(<<'PL', 'what is NOT possessive stays as perl reads it: an escaped or in-class plus, quoted text, a comment; a nested quantifier dies (#2380)');
no warnings;
sub t { my $r = eval { $_[0]->() ? 1 : 0 }; defined $r ? $r : "died" }
my ($p1, $p2, $p3) = ("a++*", "a+?+", "a+ +");
print join(",", map { t($_) } sub { "+++" =~ /^\++$/ }, sub { "++" =~ /^[+]+$/ },
    sub { "+" =~ /^[a++]$/ }, sub { " a++ " =~ /^\Q a++ \E$/ }, sub { "a" =~ /^a(?#a++)$/ },
    sub { "aa" =~ /$p1/ }, sub { "aa" =~ /$p2/ }, sub { "aa" =~ /$p3/ },
    sub { "\e\e" =~ /^\c[++$/ }, sub { "]a]" =~ /^[]a]++$/ }, sub { "aa" =~ /^a{2}+$/ }), "\n";
PL

# Task #2444: perl 5.34+ reads `X{,n}` as `X{0,n}`, and allows blanks around
# the numbers and the comma inside a counted quantifier (`{ 2 , 3 }`).  cl-ppcre
# read both as LITERAL text -- a silent no-match.  The runtime's one rewrite
# scan writes them blank-free with `0` for the absent minimum; `{,}` `{a}` and
# an escaped or in-class brace stay literal, as in perl (probed 5.40.3).
subst_agrees(<<'PL', 'the {,n} quantifier is {0,n}; blanks inside counted braces; lazy/possessive/x compose; literal forms stay literal (#2444)');
no warnings;
sub t { my ($s, $re) = @_; ($s =~ $re) ? "1($&)" : 0 }
print join(",", t("xx", qr/^x{,2}$/), t("xxx", qr/^x{,2}$/), t("", qr/^x{,2}$/),
    t("x{,2}", qr/^x{,2}$/), t("xxx", qr/x{,2}?/), t("xxx", qr/^x{,2}+x$/),
    t("xx", qr/^x{,3}+x$/), t("xx", qr/^x{ ,2}$/), t("xx", qr/^x{ , 2 }$/),
    t("xxx", qr/^x{ 2 , 3 }$/), t("xx", qr/^x{ 2}$/), t("xx", qr/^x { , 2 } $/x),
    t("abab", qr/^(ab){,2}$/), t("aa", qr/^[a]{,2}$/), t("", qr/^a{,0}$/)), "\n";
print join(",", t("x{,}", qr/^x{,}$/), t("xx", qr/^x{,}$/), t("x{,2}", qr/^x\{,2}$/),
    t(",", qr/^[{,2}]$/), t("x{,a}", qr/^x{,a}$/), t("x{,2}", qr/^\Qx{,2}\E$/)), "\n";
my $n = 2; (my $s = "xxxxx") =~ s/x{,$n}/Y/g;
print "$s ", t("xx", qr/^x{,$n}$/), " ", join("|", split /,{,2}/, "a,b,,c,,,d"), "\n";
PL

# Task #2471: after `\N`, a `{` that opens a VALID counted quantifier makes `\N`
# the "not a newline" escape, quantified -- `\N{3,4}` is never a named
# character.  PCL read the braces as a name and never matched.  Literal and
# interpolated patterns, s///, split; the possessive and lazy forms compose.
subst_agrees(<<'PL', '\N followed by a counted quantifier is \N quantified, never a named character (#2471)');
sub t { my ($s, $p) = @_; my $r = eval { $s =~ /$p/ ? "$&|$+[0]" : 0 }; defined $r ? $r : "died" }
print join(",", t("abbbbc", '\N{3,4}'), t("abbbbc", '\N{ 3 , 4 }'), t("ab\ncd", '\N{2}'),
    t("abc", '\N{,2}'), t("abbbbc", '\N{3,4}+c'), t("abbbbc", '\N{3,4}?'),
    t("abbbbc", '^\N{3,}$'), t("a\nb", '\N')), "\n";
print(("abbbbc" =~ /\N{3,4}/) ? "$&\n" : "0\n");
print(("ab\nc" =~ /^\N{2}$/m) ? "$&\n" : "0\n");
my $x = "aXbXXc"; (my $y = $x) =~ s/\N{2}/-/g;
print "$y ", join("|", split /\N{2}/, "abcdefg"), "\n";
PL

# Task #2443: under /x a blank or a `#` comment is no atom, so a quantifier
# after it applies to the atom before it -- `/a ++b/x` and `/a+ +b/x` are both
# `a++`.  The possessive rewrite read the blank as the atom (`a(?> +)`) and the
# match died.  The /x state follows inline scopes: `(?x)` `(?x:…)` turn it on,
# `(?-x:…)` off (there the blank IS the atom); a class or an escaped blank is
# an atom under /x; without /x nothing changes (the breaking cases).
subst_agrees(<<'PL', 'under /x a blank or comment before a possessive + is not an atom; inline x scopes; no-x unchanged (#2443)');
no warnings;
sub t { my ($s, $p, $f) = @_;
  my $r = eval { my $re = $f eq 'x' ? qr/$p/x : $f eq 'xx' ? qr/$p/xx : qr/$p/; $s =~ $re ? "1($&)" : 0 };
  defined $r ? $r : "died" }
print join(",", t("aab", '^a ++b', 'x'), t("aab", '^a+ +b', 'x'), t("aab", '^a + +b', 'x'),
    t("aab", "^a+ # c\n +b", 'x'), t("aab", "^a+ # (c[\n +b", 'x'), t("aab", '(?x)^a ++b', ''),
    t("aab", '^a ++b', 'xx'), t("aaa", '^a ++a', 'x'), t("aab", '^(?x: a ++ )b', ''),
    t("aab", '^(?x:a) ++b', 'x')), "\n";
print join(",", t("a  b", '^(?-x:a ++)b', 'x'), t("aab", '^(?-x:a ++)b', 'x'),
    t("a  b", '^a[ ]++b', 'x'), t("a  b", '^a\ ++b', 'x'), t("a  b", '^a ++b', ''),
    t("aab", '^a ++b', ''), t("aa b", '^a+ +b', ''), t("a  b", '(?x) ^ (?-x)a ++b', '')), "\n";
print(("aab" =~ /^a ++b/x) ? "1" : "0", ("aab" =~ /^a+ +b/x) ? "1" : "0", "\n");
my $s = "aaab"; $s =~ s/a + +/X/x; print "$s ", join("|", split / a ++ /x, "baaacab"), "\n";
PL

# Task #2386 (cheap half): the control verb `(*FAIL)` / `(*F)` IS `(?!)`
# (perlre), and cl-ppcre has `(?!)`.  It used to reach cl-ppcre as written and
# the match DIED.  Inside a class it is literal text, escaped it is literal.
subst_agrees(<<'PL', '(*FAIL) and (*F) match as (?!) in m//, s///, split; literal in a class (#2386)');
sub t { my ($s, $p) = @_; my $r = eval { $s =~ /$p/ ? "1($&)" : 0 }; defined $r ? $r : "died" }
print join(",", t("b", 'a(*FAIL)|b'), t("ab", 'a(*FAIL)|b'), t("abc", '(*F)'), t("", '(*FAIL)'),
    t("y", '(?:x(*FAIL))?y'), t("xy", '^(?:x(*FAIL))?y'), t("*", '[(*FAIL)]'),
    t("L", '[(*FAIL)]'), t("(*F)", '\(\*F\)'), t("b", 'a (*F) | b')), "\n";
print(("b" =~ /a(*FAIL)|b/) ? "1" : "0", ("ab" =~ /^a(*F)|b$/) ? $& : "0", "\n");
my $s = "abcabc"; $s =~ s/b(*FAIL)|c/X/g; print "$s ", join("|", split /,(*F)|;/, "a,b;c"), "\n";
PL

# ...and every OTHER verb still has no translation: it must DIE trappably at the
# match (#2372), never match silently wrong.  perl answers 1 for each of these;
# when #2386's engine half lands, this row fails and is rewritten to agree.
{
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh 'for my $p (\'a(*SKIP)b\', \'a(*ACCEPT)b\', \'(*:B)a\', \'a(*FAIL:x)|b\') {'
            . ' my $r = eval { "ab" =~ /$p/ ? 1 : 0 }; print defined $r ? "lived " : ($@ ? "died " : "silent ") }'
            . ' print "\n";';
    close $fh;
    chomp(my $got = `$RealBin/../../runpcl $file 2>/dev/null`);
    is($got, 'died died died died ', 'the untranslated control verbs still die trappably (#2386, #2372)');
}

# #1026: an embedded BLOCK in an s///e replacement -- an anon sub (TimeDate's
# Date/Format.pm:65 `($_[0]->can("format_$1") || sub { $1 })->($_[0])`), a
# map / grep / sort block, a do block, a closure over a lexical -- is compiled
# by the parser like any other expression's block.  It failed the whole
# TRANSPILE ("no form emitter for expression node type 'anon_sub'").
{
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'EOP';
sub f_Y { "y" }
my $z = "aXbY";
$z =~ s/([XY])/(main->can("f_$1") || sub { "<$1>" })->()/ge;
my @a = (1, 2); my $m = "M";
$m =~ s/M/join ":", map { $_ * 2 } @a/e;
my @w = ("b", "a", ""); my $s = "Q";
$s =~ s/Q/join "", sort { $a cmp $b } grep { length } @w/e;
my $lex = 7; my $t = "a1b2";
$t =~ s/(\d)/my $c = sub { $1 + $lex }; $c->()/ge;
my $n = "x"; $n =~ s/x/do { my $q = 3; $q + 1 }/e;
print "$z $m $s $t $n\n";
EOP
    close $fh;
    chomp(my $want = `$^X $file 2>&1`);
    chomp(my $got = `$RealBin/../../runpcl $file 2>&1`);
    is($want, 'a<X>by 2:4 ab a8b9 4', 'perl oracle for the s///e block rows (#1026)');
    is($got, $want, 'an s///e replacement compiles its anon sub / map / sort / grep / do blocks (#1026)');
}

# #1025: the replacement expression gets the ordinary CONTEXT annotation -- a
# list operator's arguments are in list context there too.  Unannotated, `..`
# was the scalar flip-flop and split returned its COUNT (URI::Escape's
# `split //, "$1"` escaped every character as "1").
{
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'EOP';
my $x = 'a-e'; $x =~ s/(\w)-(\w)/join ':', $1 .. $2/e;
my %e = (b => "%62", c => "%63"); my $t = "abcd";
$t =~ s{([bc])}{ join('', map +($e{$_} || "?"), split //, "$1") }ge;
my $u = "xy"; $u =~ s/(x)/join ",", reverse 1, 2, 3/e;
print "$x $t $u\n";
EOP
    close $fh;
    chomp(my $want = `$^X $file 2>&1`);
    chomp(my $got = `$RealBin/../../runpcl $file 2>&1`);
    is($want, 'a:b:c:d:e a%62%63d 3,2,1y', 'perl oracle for the s///e context rows (#1025)');
    is($got, $want, 'an s///e replacement puts a list operator\'s arguments in list context (#1025)');
}

# PPI §42 (#3749, s531e): a here-doc introduced in an s///e replacement on the
# statement's line takes its body from the lines after it (perl 5.18+).  PCL's
# PPI keeps those lines on the token (heredoc_lines) and the replacement is
# compiled with them after it; before, the body was lexed as CODE and the
# program died ("Undefined subroutine &main::END").
{
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'EOP';
$_ = "a"; s/a/<<END/e;
foo
END
print;
my $foo = "not ok 30\n";
$foo =~ s/^not /substr(<<EOF, 0, 0)/e;
  Ignored
EOF
print $foo;
my $r = "q"; $r =~ s/q/<<A . <<B/e; print "($r)", <<C;
a1
A
b1
B
c1
C
sub f { my $s = shift; $s =~ s/(\w)/uc($1) . <<~E/e; return $s }
    -tail
    E
print f("zz");
print qr/(?{<<HD})/ eq q{(?^:(?{<<HD}))} ? "qr ok\n" : "qr NOT\n";
body
HD
print "end\n";
EOP
    close $fh;
    my $want = `$^X $file 2>&1`;
    my $got  = `$RealBin/../../runpcl $file 2>/dev/null`;
    is($want, "foo\nok 30\n(a1\nb1\n)c1\nZ-tail\nzqr ok\nend\n", 'perl oracle for the here-doc-in-s///e rows (PPI §42)');
    is($got, $want, 'a here-doc in an s///e replacement or a (?{ }) block takes the lines after the statement (PPI §42)');
}

# #691 (s543g): a variable DEREFERENCED inside a quote-like leaf -- an s///
# replacement, an s/// or split pattern -- is not stringified there, so it is
# no `:str` use for the raw-slot verdict (VarAnnotator::_tw_scan_quote_leaf
# asks Pl::InterpScan::scan_one: a `deref` / `expr` reference is an opaque use
# of its base and of the scalars its code names).  The B-str freeze stored the
# stringified reference and the replacement read it back empty.  The first
# two rows FAIL on the 8959afae extraction; the third is the breaking shape (a
# plain `$v` in a replacement stays a str use, emission unchanged).
subst_agrees(q{sub t1 { my $r = \"Z"; my $x = "mAn"; $x =~ s/A/${$r}/; $x }
sub t2 { my $r = \"Z"; my $x = "mAn"; $x =~ s/A/$$r/; $x }
sub t3 { my $ar = [1,2]; my $x = "mAn"; $x =~ s/A/@{$ar}/; $x }
sub g1 { my $r = \"Z"; my $x = "mAnA"; $x =~ s/A/${$r}/g; $x }
sub b1 { my $r = \"Z"; my $x = "mAn"; $x =~ s{A}{@{[ $$r ]}}; $x }
print join(" ", t1(), t2(), t3(), g1(), b1()), "\n";},
  "#691: a deref in an s/// replacement reads the referent (subs, /g, \@{[ ]})");
subst_agrees(q{my $r = \"Z"; my $ar = [1, 2]; my $x = "mAn"; my $y = "mAn";
$x =~ s/A/$$r/; $y =~ s/A/@{$ar}/;
sub q1 { my $p = \"A"; my $s = "mAn"; $s =~ s/${$p}/x/; $s }
sub q4 { my $p = \"A"; join("|", split /$$p/, "mAn") }
print "$x $y ", q1(), " ", q4(), "\n";},
  "#691: a file-level deref in a replacement, and a deref in an s/// or split PATTERN");
subst_agrees(q{sub s1 { my $v = "V"; my $x = "mAn"; $x =~ s/A/$v/; $x }
sub s2 { my $v = 5; my $x = "mAn"; $x =~ s/A/x$v/g; $x }
my $t = ""; for my $i (1..3) { my $s = "aXb"; $s =~ s/X/$i/; $t .= $s }
print join(" ", s1(), s2(), $t), "\n";},
  "#691 breaking shape: a plain \$v in a replacement is still interpolated");

# #5252 (s543k): a POSTFIX dereference in an s/// replacement under
# postderef_qq -- the manufactured `"..."` of the replacement read the feature
# at no position (off); it is read at the s/// token now (the ONE reader,
# StringInterpolation::postderef_qq_active_for), and VarAnnotator's quote-leaf
# scan reads `$r->@*` as a dereference (no B-str freeze of `$r`).  A string
# eval inherits the site's feature (the `postderef_qq` pseudo-feature on the
# #364 list).  The first two rows FAIL on the 4a0fd0ec extraction; the third is
# the breaking shape (without the feature `->@*` stays text after the ref).
subst_agrees(q{use v5.24;
sub p1 { my $r = \"Z"; my $x = "mAn"; $x =~ s/A/$r->$*/; $x }
sub p2 { my $r = [1,2]; my $x = "mAnAn"; $x =~ s/A/<$r->@*>/g; $x }
sub p3 { my $r = [1,2,3]; my $x = "q"; $x =~ s/q/$r->$#*|$r->@[0,1]/; $x }
sub p4 { my $h = {a => 1, b => 2}; my $x = "q"; $x =~ s/q/$h->@{qw(a b)}/; $x }
print join(" ", p1(), p2(), p3(), p4()), "\n";},
  "#5252: a postfix deref in an s/// replacement under postderef_qq (subs, /g, \$#*, slices)");
subst_agrees(q{use v5.24;
my $r = \"Z"; my $x = "mAn"; eval q{ $x =~ s/A/$r->$*/; 1 } or die $@;
print "$x ", eval(q{"$r->$*"}), "\n";},
  "#5252: a string eval inherits the site's postderef_qq (a replacement and a dq text)");
subst_agrees(q{my $r = [1,2]; my $x = "mAn"; $x =~ s/A/$r->@*/; $x =~ s/ARRAY\(0x[0-9a-f]+\)/REF/;
{ use v5.24; no feature "postderef_qq"; my $y = "mAn"; $y =~ s/A/$r->@*/; $y =~ s/ARRAY\(0x[0-9a-f]+\)/REF/; print "$x $y\n"; }},
  "#5252 breaking shape: without postderef_qq `->\@*` stays text after the reference");

# #5560 (s543u): `\K` (keep) moves the match's reported START to where it
# stands -- $&, $`, @-[0], the span s/// replaces, what a list-context //g
# without groups returns, the separator split removes -- and leaves the
# groups, their numbers and the match's end alone.  The rewrite scan spells it
# as a mark that the parse tree makes an invisible register (renumbering the
# back-references past it); every row FAILS on the 5d511ff8 extraction, where
# cl-ppcre read `\K` as the letter K and the pattern never matched.
subst_agrees(<<'PL', "#5560: \\K moves the match start: \$& \$` \@- \@+, list //g, a quantified / alternated / unmatched \\K, back-references, split, pos");
my $s = "key=val; k2=v2";
if ($s =~ /key=\Kval/) { print "1 [$&] [$`] [$'] $-[0] $+[0]\n" }
my @a = $s =~ /=\K\w+/g; my @b = $s =~ /(\w+)=\K(\w+)/g; print "2 @a | @b\n";
if ("xaybz" =~ /(a)y\K(b)z/) { print "3 [$&] @- | @+ [$1$2] $+\n" }
print "4 ", join(",", map { "pq" =~ $_ ? "[$&]" : "no" } qr/(?:p\K)+q/, qr/p\Kq|r\Ks/, qr/(z\K)?q/), "\n";
print "5 ", ("aab" =~ /(a)\K\1b/ ? "[$&][$1]" : "no"), " ", ("abb" =~ /(a)\K(b)\2/ ? "[$&][$2]" : "no"), "\n";
print "6 ", join("|", split /,\K/, "a,b,c"), " ", join("|", split /x\K|;\K /, "axb; c"), "\n";
my $n = 0; $n++ while "aaa" =~ /a\K/g; my @p; my $t = "a1a2"; while ($t =~ /a\K\d/g) { push @p, "$&@" . pos($t) } print "7 $n @p\n";
my $k = '\K'; print "8 ", ("ab" =~ /a${k}b/ ? "[$&]" : "no"), ("a\\Kb" =~ /a\\K/ ? " y[$&]" : " n"), ("K" =~ /[\K]/ ? " y" : " n"), "\n";
PL
subst_agrees(q{my $s = "key=val; k2=v2";
(my $a = $s) =~ s/key=\Kval/X/; (my $b = $s) =~ s/\w+=\K\w+/V/g; print "1 $a | $b\n";
my $w = "abcdefgh"; my $m = 3; print "2 ", $w =~ s/.{1,$m}\K/-/gr, " ", $w =~ s/.{3}\K/-/gr, "\n";
print "3 ", join(",", map { (my $x = "aaa") =~ s/$_/-/g; $x } 'a\K', '\K', '^\K'), "\n";
print "4 ", "aXbXc" =~ s/X\K(?=b)/-/r, " ", "ab ab ab cd" =~ s!(\w\w)\K(( \1)+)!"<" . length($2) . ">"!ger, "\n";
(my $c = "ab") =~ s/a\Kb/[$&]/; (my $e = "a1b2") =~ s/[a-z]\K(\d)/$1*2/ge; print "5 $c $e\n";
(my $f = "xyxy") =~ s/(?:x\K|y)/-/g; my $g = "zazbzzc"; my $cnt = ($g =~ s/z\K/-/g); print "6 $f $cnt $g\n";
(my $h = "AB") =~ s/a\Kb/c/i; (my $i = "ab") =~ s/ a \K b /X/x; print "7 $h $i\n";},
  "#5560: s/// with \\K replaces from the keep position: /g with a zero-length kept match, /r /e, groups, \$& in the replacement, /i /x");
subst_agrees(q{for my $p ('(?<=a\K)b', 'a(?=b\K)') { my $r = eval { "ab" =~ /$p/; 1 }; print $r ? "lived" : ($@ =~ /not permitted/ ? "died" : "other"), "\n" }
(my $x = "ab") =~ s/ab/<$1>/; (my $y = "ab") =~ s/(a)(b)/[$3$2]/g; print "$x $y\n";},
  "#5560: \\K in a lookaround dies trappably (perl 5.32+); a template \$N past the pattern's groups is empty, never a Lisp error");

# #5461 / #5462 (s543u): `\G` asserts the TARGET's pos() (0 when unset) -- for
# a //g step after the first and every s///g step, the previous match's end --
# wherever it stands.  The rewrite scan spells it as a mark the parse tree makes
# a filter on *p-gpos*, which s/// binds as m// does (the substitution path read
# `\G` as the letter G); a leading `\G` in m// keeps the scan-from-pos fast path.
# A floating `\G` (`-\G`, `.\G`) works in both; under //g a match ending at its
# \G counts as zero-length (perl's start + gofs == end).  A list-context `\G//gc`
# keeps pos (#5462).  Every row FAILS on the 5d511ff8 extraction.
subst_agrees(<<'PL', "#5461: \\G in s/// anchors at the target's pos(): in place, /r, /g, /e, a floating -\\G inside a //g loop, after a lookahead //g, pos reset, qr");
my $s = "abcabc"; print "1 ", $s =~ s/\Ga/X/r, "\n";
pos($s) = 3; print "2 ", $s =~ s/\Ga/X/r, " ", pos($s), "\n";
pos($s) = 2; print "3 ", $s =~ s/\G.*//r, "|\n";
my $g = "Gab"; print "4 ", $g =~ s/\Ga/X/r, "\n";
my $t = "aaab"; $t =~ s/\Ga/X/g; my $u = "baaa"; $u =~ s/\Ga/X/g; print "5 $t $u\n";
my $c = "a-b-c"; my @r; while ($c =~ /-/g) { push @r, $c =~ s/-\G/+/r, $c =~ s/-\G/ uc("y") /er } print "6 @r\n";
$_ = "q00r00"; /r(?=00)/g and print "7 ", s/\G00/X/r, "\n";
my $f = "aaa"; pos($f) = 1; $f =~ s/\Ga/b/; print "8 ", pos($f) // 'u', " $f\n";
my $w = "aaab"; my $n = 0; while ($w =~ s/\Ga/x/) { last if ++$n > 9 } print "9 $w $n\n";
my $v = "1 2 3"; pos($v) = 2; print "10 ", $v =~ s/\G(\d)/<$1>/gr, " ", "aGb" =~ s/[\G]/!/r, " ", "a\\Gb" =~ s/\\G/!/r, "\n";
my $q = qr/\Gb/; my $x = "abc"; pos($x) = 1; print "11 ", $x =~ s/$q/B/r, "\n";
my $d = "123456"; pos($d) = 4; my $nd = ($d =~ s/\d\d(?=\d\G)/78/g); my $e = "abcdef"; $e =~ s/bc|(.)\G(.)/$1 ? "[$1-$2]" : "XX"/ge;
(my $y = "aaXbb") =~ s/X\G|b/-/g; my $z = "aaXbb"; pos($z) = 3; my $nz = ($z =~ s/X\G|b/-/g); print "12 $nd $d $e $y $nz $z\n";
PL
subst_agrees(<<'PL', "#5461 #5462: a floating \\G in m// and //g, a quantified or alternated \\G, \\G with \\K, a list \\G//gc keeps pos");
my $s = "abc"; pos($s) = 1; print "1 ", ($s =~ /b\G/ ? "y" : "n"), ($s =~ /a\G/ ? "y[$&]" : "n"), "\n";
$s = "a-b-c"; pos($s) = 2; my @l = $s =~ /-\G/g; my $n = 0; pos($s) = 2; $n++ while $s =~ /-\G/g; print "2 ", scalar(@l), " $n\n";
$s = "aaXbb"; pos($s) = 3; @l = $s =~ /(?:a|X)\Gb/g; print "3 @l\n";
$s = "aaab"; pos($s) = 0; print "4 ", ($s =~ /(?:\Ga)+/ ? "[$&]" : "no"), " ", ("xb" =~ /\Ga|b/ ? "[$&]" : "no"), "\n";
$s = "a.b.c"; my @o; while ($s =~ /\./g) { push @o, ($s =~ /.\G/ ? $& : "-") } print "5 @o\n";
$s = "aab"; pos($s) = 0; @l = $s =~ /\Ga/gc; print "6 ", pos($s) // 'u'; pos($s) = 1; @l = $s =~ /\Gq/gc; print " ", pos($s) // 'u', "\n";
$s = "ab"; pos($s) = 1; print "7 ", ($s =~ /\Gb\K/ ? "[$&]" : "no"), " ", ($s =~ s/\G\Kb/B/r), "\n";
my @parts = "a/b\\/c/d" =~ m{(?:\A|\G/)((?:[^/\\]|\\.)*)}g; $s = "xab"; pos($s) = 0; @l = $s =~ /\Ga|b/g;
my @f; $s = "a,b,,c"; while ($s =~ /(?:\A|\G,)([^,]*)/g) { push @f, "[$1]" } print "8 ", join("|", @parts), " @l @f\n";
PL
{
    # A //g whose \G at an offset a match can BYPASS (`X\G|b`) is refused, loud
    # and trappable: perl advances it by an offset it computes for the whole
    # pattern (docs/not-supported.md ns-regex-escapes); without /g it runs, and
    # s///g stops where perl's REXEC_FAIL_ON_UNDERFLOW does (row 73).
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'EOP';
my $s = "aaXbb"; pos($s) = 3;
for my $code (sub { my @l = $s =~ /X\G|b/g }, sub { my $n = 0; $n++ while $s =~ /X\G|b/g }) {
  print eval { $code->(); 1 } ? "lived " : ($@ =~ /bypass is not supported/ ? "refused " : "other ") }
pos($s) = 3; print $s =~ /X\G|b/ ? "[$&]" : "no", "\n";
EOP
    close $fh;
    chomp(my $got = `$RealBin/../../runpcl $file 2>&1`);
    is($got, 'refused refused [X]', '#5461: a //g whose \G a match can bypass is refused trappably; a single match runs');
}

# #5561 (s543u): `\b{wb}` / `\B{wb}` are perl's Unicode word boundaries --
# UAX #29 (SBCL's sb-unicode:words) with perl's white-space tailoring and the
# regional indicators paired per run; `\b{gcb}` / `\b{g}` are the boundaries
# of \X's clusters.  The rewrite scan spells each as a mark the parse tree
# makes a filter.  Positions only are printed, the subjects are built with
# chr(): nothing here reads the locale.  #5562: `\g` in every spelling
# (`\gN` `\g{N}` `\g-N` `\g{-N}` `\g{name}`) is a back-reference.  Every row
# FAILS on the 5d511ff8 extraction, where cl-ppcre read the letters.
subst_agrees(<<'PL', "#5561: \\b{wb} and \\B{wb}: apostrophes, digit separators, white-space spans, a mark after spaces, indicator pairs, the empty string, //g, s///g");
sub at { my ($s, $re) = @_; my @p; while ($s =~ /$re/g) { push @p, pos($s) } join(",", @p) }
my ($acute, $ri1, $ri2) = (chr(0x301), chr(0x1F1FA), chr(0x1F1F8));
my @s = ("ab cd", "don't stop", "1,000 x", "a.b", "a  b", "a \t b", "x-y", "3.14", "can't.", "a\n\nb",
         "a  ${acute}b", "a ${acute}b", "$ri1$ri2$ri1$ri2!", "Hi! Yo.", "a_b 9z");
my $i = 0;
for my $s (@s) { $i++; print "$i ", at($s, qr/\b{wb}/), " | ", at($s, qr/\B{wb}/), "\n" }
print "e ", ("" =~ /\b{wb}/ ? "y" : "n"), ("" =~ /\B{wb}/ ? "y" : "n"), "\n";
print "w ", join("|", "ab cd" =~ /\b{wb}(\w+)/g), " ", join("|", "don't stop" =~ /\b{wb}(\S+?)\b{ wb }/g), "\n";
(my $r = "don't stop now") =~ s/\b{wb}/|/g; print "r $r\n";
PL
subst_agrees(<<'PL', "#5561: \\b{gcb} / \\b{g} are \\X's cluster boundaries (combining marks, CRLF)");
sub at { my ($s, $re) = @_; my @p; while ($s =~ /$re/g) { push @p, pos($s) } join(",", @p) }
my $acute = chr(0x301);
for my $s ("e${acute}x", "a\r\nb", "${acute}a", "ab${acute}${acute}c") {
  my @x; my $p = 0; while ($s =~ /\X/g) { push @x, pos($s) }
  print at($s, qr/\b{gcb}/), " | ", at($s, qr/\b{g}/), " | ", at($s, qr/\B{gcb}/), " | 0,", join(",", @x), "\n";
}
PL
subst_agrees(<<'PL', "#5562: \\g as a back-reference: absolute, relative, named, braced or not, in s///");
sub t { my ($s, $p) = @_; my $r = eval { $s =~ /$p/ ? "y[$&]" : "n" }; defined $r ? $r : "died" }
print "1 ", t("aa", '(a)\g{-1}'), " ", t("abab", '(a)(b)\g{-2}\g{-1}'), " ", t("abcabc", '(a)(b)(c)\g{-3}\g{-2}\g{-1}'), "\n";
print "2 ", t("aa", '(a)\g1'), " ", t("aa", '(a)\g{1}'), " ", t("aa", '(?<n>a)\g{n}'), " ", t("a_a", '(?<n_x>a)_\g{n_x}'), "\n";
print "3 ", t("aa", '(a)\g-1'), " ", t("abb", '(a)(b)\g-1'), " ", t("aba", '(a)(b)\g{-2}'), " ", t("abba", '(a)(?:(b)\g{-1})\g{-2}'), "\n";
print "4 ", t("a", '\g{-1}'), " ", t("aa", '(a\g{-1})'), " ", t("aa", '(a)(\g{-2})'), " ", t("aa0", '(a)\g{-1}0'), " ", t("x", '[\g1]'), "\n";
(my $x = "abab") =~ s/(a)(b)\g{-2}/<$1>/; print "5 $x\n";
PL
# s543u, the rule-12 read of the rewriter against perlre / perlrebackslash: the
# spellings cl-ppcre read as letters (`\o{}` `\k'n'` `\k{n}`: silently never
# matched) or refused (`(?'n'...)` `(?P<n>...)` `(?P=n)`, the alphabetic
# assertions `(*pla:` .. `(*atomic:`) are translated to the forms it has.  Every
# row FAILS on the 5d511ff8 extraction.
subst_agrees(<<'PL', "s543u: \\o{} \\k'n' \\k{n} (?'n') (?P<n>) (?P=n) and the alphabetic assertions (*pla: (*plb: (*nla: (*nlb: (*atomic:");
sub t { my ($s, $p) = @_; my $r = eval { $s =~ /$p/ ? "y[$&]" : "n" }; defined $r ? $r : "died" }
print "1 ", t("A", '\o{101}'), " ", t("xA", '[\o{101}]'), " ", t("ab", 'a\o{142}'), "\n";
print "2 ", t("aa", "(?<n>a)\\k'n'"), " ", t("aa", '(?<n>a)\k{n}'), " ", t("aXa", "(?'q'a)X\\k<q>"), " ", t("aa", '(?P<q>a)(?P=q)'), " ", t("a_a", '(?P<q_r>a)_\k<q_r>'), "\n";
print "3 ", t("ab", 'a(*pla:b)'), " ", t("ab", '(*plb:a)b'), " ", t("ac", 'a(*nla:b)'), " ", t("cb", '(*nlb:a)b'), " ", t("ab", 'a(*positive_lookahead:b)'), " ", t("ab", '(*negative_lookbehind:x)b'), "\n";
print "4 ", t("aab", '(*atomic:a+)b'), " ", t("aaa", '(*atomic:a+)a'), " ", t("ab", '(*pla:(a))\1b'), "\n";
PL
{
    # \b{sb} and \b{lb} are refused, loud and trappable (sentence and line
    # boundaries; docs/not-supported.md ns-regex-escapes) -- never a silent no-match.
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh 'for my $p (\'a\b{sb}\', \'a \b{lb}\', \'\B{sb}\') { print eval { "a b" =~ /$p/; 1 } ? "lived " : ($@ =~ /is not supported/ ? "refused " : "other ") } print "\n";';
    close $fh;
    chomp(my $got = `$RealBin/../../runpcl $file 2>&1`);
    is($got, 'refused refused refused ', '#5561: \b{sb} and \b{lb} are refused trappably');
}


done_testing();
