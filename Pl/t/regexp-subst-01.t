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

use PPI;
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
             '(p-=~ $_ (p-subst "foo" "bar"))',
             's/foo/bar/ basic (implicit $_)');

test_codegen('s/hello/world/g',
             '(p-=~ $_ (p-subst "hello" "world" :g))',
             's///g global (implicit $_)');

test_codegen('s/UPPER/lower/i',
             '(p-=~ $_ (p-subst "UPPER" "lower" :i))',
             's///i case insensitive (implicit $_)');

test_codegen('s/pattern/replace/gi',
             '(p-=~ $_ (p-subst "pattern" "replace" :g :i))',
             's///gi multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- s/// with binding operator:";

test_codegen('$str =~ s/old/new/',
             '(p-=~ $str (p-subst "old" "new"))',
             '$str =~ s///');

test_codegen('$str =~ s/a/b/g',
             '(p-=~ $str (p-subst "a" "b" :g))',
             '$str =~ s///g');

test_codegen('$str !~ s/x/y/',
             '(p-!~ $str (p-subst "x" "y"))',
             '$str !~ s///');


# ============================================================
diag "";
diag "-------- s/// with more modifiers (standalone - implicit \$_):";

test_codegen('s/pat/rep/s',
             '(p-=~ $_ (p-subst "pat" "rep" :s))',
             's///s single-line (implicit $_)');

test_codegen('s/pat/rep/m',
             '(p-=~ $_ (p-subst "pat" "rep" :m))',
             's///m multi-line (implicit $_)');

test_codegen('s/pat/rep/x',
             '(p-=~ $_ (p-subst "pat" "rep" :x))',
             's///x extended (implicit $_)');

test_codegen('s/pat/rep/gimsxe',
             '(p-=~ $_ (p-subst "pat" (lambda () (p-scalar-ctx (pl-rep))) :e :g :i :m :s :x))',
             's/// all common modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- Basic tr/// transliteration (standalone - implicit \$_):";

test_codegen('tr/a-z/A-Z/',
             '(p-=~ $_ (p-tr "a-z" "A-Z"))',
             'tr/a-z/A-Z/ uppercase (implicit $_)');

test_codegen('tr/A-Z/a-z/',
             '(p-=~ $_ (p-tr "A-Z" "a-z"))',
             'tr/A-Z/a-z/ lowercase (implicit $_)');

test_codegen('y/abc/xyz/',
             '(p-=~ $_ (p-tr "abc" "xyz"))',
             'y/// synonym for tr/// (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with modifiers (standalone - implicit \$_):";

test_codegen('tr/aeiou//d',
             '(p-=~ $_ (p-tr "aeiou" "" :d))',
             'tr///d delete (implicit $_)');

test_codegen('tr/a-z//c',
             '(p-=~ $_ (p-tr "a-z" "" :c))',
             'tr///c complement (implicit $_)');

test_codegen('tr/a-z//s',
             '(p-=~ $_ (p-tr "a-z" "" :s))',
             'tr///s squash (implicit $_)');

test_codegen('tr/a-z/A-Z/cds',
             '(p-=~ $_ (p-tr "a-z" "A-Z" :c :d :s))',
             'tr/// multiple modifiers (implicit $_)');


# ============================================================
diag "";
diag "-------- tr/// with binding:";

test_codegen('$str =~ tr/a-z/A-Z/',
             '(p-=~ $str (p-tr "a-z" "A-Z"))',
             '$str =~ tr///');

test_codegen('$count = ($str =~ tr/x//)',
             '(p-scalar-= $count (p-=~ $str (p-tr "x" "")))',
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
    isnt($rc, 0, 's///e with an uncompilable replacement exits nonzero (rule 12)');
    like($msg, qr/cannot compile the s\/\/\/e replacement/,
         '... and says which replacement it could not compile');
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

# The emission promises, in both directions: a `$1`-only replacement stays a
# STRING (the runtime's backref rewrite, no lambda per match), and a
# single-quoted one is a lambda over a constant — never that string, because
# the string path is exactly what would read its `$1` as a register.
test_codegen('s/(a)/[$1]/',
             '(p-=~ $_ (p-subst "(a)" "[$1]"))',
             'a $1-only replacement is still emitted as a plain string');
test_codegen(q{s'(a)'[$1]'},
             '(p-=~ $_ (p-subst "(a)" (lambda () "[$1]")))',
             "a single-quoted replacement is a lambda over the literal text");
test_codegen('s/(a)/[$&]/',
             '(p-=~ $_ (p-subst "(a)" (lambda () (p-string-concat "[" |$&| "]"))))',
             'a punctuation magic takes the lambda path (#520)');

done_testing();
