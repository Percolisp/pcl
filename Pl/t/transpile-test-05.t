#!/usr/bin/env perl
# Transpile tests part 5: foreach-over-arrayrefs and sprintf-style arrayref iteration

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_transpile {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Issue 1: for over literal list of arrayrefs ──────────────────────────────
# Old bug: gen_progn emitted (progn aref1 aref2) which returns only the last
# element; p-flatten-for-list then spread its contents.  Result: iterated over
# the *contents* of [2,"b"] instead of over both arrayrefs.

test_transpile("for literal arrayrefs: iteration count", '
my $n = 0;
for ([1,"a"], [2,"b"]) { $n++ }
print "$n\n";
', "2\n");

test_transpile("for literal arrayrefs: each \$_ is a ref", '
for ([1,"a"], [2,"b"]) { print ref($_), "\n" }
', "ARRAY\nARRAY\n");

test_transpile("for literal arrayrefs: access elements", '
for ([10,"x"], [20,"y"]) { print $_->[0], " ", $_->[1], "\n" }
', "10 x\n20 y\n");

# ── Issue 2: sprintf-style arrayref iteration (sprintf.t regression) ─────────
# Session 78 broke sprintf.t (0/566 -> 0/473).  The test builds @tests as an
# array of arrayrefs [fmt, [args...], expected], then iterates and unpacks.
# Regression test: this pattern must work end-to-end.

test_transpile("sprintf-style: push arrayrefs then iterate", '
my @tests;
push @tests, ["%d",   [42],      "42"];
push @tests, ["%s",   ["hello"], "hello"];
push @tests, ["%.2f", [3.14159], "3.14"];
my ($pass, $fail) = (0, 0);
for (@tests) {
    my ($fmt, $args, $exp) = @$_;
    my $x = sprintf($fmt, @$args);
    if ($x eq $exp) { $pass++ } else { $fail++ }
}
print "pass=$pass fail=$fail\n";
', "pass=3 fail=0\n");

test_transpile("sprintf-style: iteration count is not spread", '
my @tests;
push @tests, ["a", [1], "x"];
push @tests, ["b", [2], "y"];
push @tests, ["c", [3], "z"];
my $n = 0;
for (@tests) { $n++ }
print "$n\n";
', "3\n");

test_transpile("sprintf-style: unpack nested arrayref field", '
my @tests;
push @tests, ["%d %d", [10, 20], "10 20"];
for (@tests) {
    my ($fmt, $args, $exp) = @$_;
    my $x = sprintf($fmt, @$args);
    print $x eq $exp ? "ok" : "fail:$x", "\n";
}
', "ok\n");

# ── Bug fix: sub named after a Perl operator (e.g. `x`) called as function ───
# `x` is the string-repetition operator, but `sub x { }; x()` must call the
# user-defined sub, not emit (p-str-x).

test_transpile("sub named 'x' callable as x()", '
sub x { return 42; }
my $r = x();
print "$r\n";
', "42\n");

test_transpile("sub named 'x' with arg via assignment", '
sub x { return $_[0] * 2; }
my $r = x(5);
print "$r\n";
', "10\n");

test_transpile("sub named 'x' return value used in expression", '
sub x { return 3; }
my $n = x() + 1;
print "$n\n";
', "4\n");

# ── Bug fix: `<` + `>` in ternary expression not misidentified as glob ────────
# sort { $a<$b?1:$a>$b?-1:0 } was generating a PARSE ERROR because
# _fix_ppi_glob_after_block was turning <$b?1:$a> into a glob token.
# The fix: < preceded by a symbol/number/string is always less-than.

test_transpile("ternary with < > as comparison ops: sort descending", '
my @g = (2, 3, 1);
my @r = sort { $a<$b?1:$a>$b?-1:0 } @g;
print "@r\n";
', "3 2 1\n");

test_transpile("ternary < > not treated as glob in expression", '
my $a = 2; my $b = 3;
my $x = $a<$b?1:$a>$b?-1:0;
print "$x\n";
', "1\n");

# ── Bug fix: sort(func(args)) — func not treated as sort comparator ────────────
# sort(routine(1)) was being parsed as sort with 'routine' as the comparator
# sub and '1' as the list, because sort(NAME LIST) detection did not check
# whether NAME is immediately followed by (...) — which means it's a call.

test_transpile("sort(func(args)) calls func not uses it as comparator", '
sub routine { return "one", "two" }
my @a = sort(routine(1));
print "@a\n";
', "one two\n");

# ── Bug fix: *wantarray* was t in foreach loop body, making regex =~ ──────────
# return an empty vector (#()) instead of t on match-with-no-captures,
# causing all boolean regex tests inside foreach bodies to fail.

test_transpile("regex match inside foreach body is in scalar context", '
my @flags = ("-", "+");
my @results;
for my $flag (@flags) {
    push @results, ($flag =~ /-/ ? "yes" : "no");
}
print join(",", @results), "\n";
', "yes,no\n");

test_transpile("function called from foreach: regex in function body correct", '
sub has_minus { my $s = shift; return $s =~ /-/ ? 1 : 0 }
my @flags = ("--", "++", "-+");
my @r;
for my $f (@flags) { push @r, has_minus($f) }
print join(",", @r), "\n";
', "1,0,1\n");

# ── Bug fix: s/// with variable interpolation in pattern/replacement ──────────
# s/($dx)/$dx$1/ was generating (p-subst "($dx)" "$dx$1") with the variable
# names as literal strings instead of being evaluated at runtime.

test_transpile("s/// with variable in pattern", '
my $dx = "X";
my $str = "X";
$str =~ s/$dx/Y/;
print "$str\n";
', "Y\n");

test_transpile("s/// with variable in pattern and replacement (back)", '
my $dx = "\x{10f2}";
my $str = "\x{10f2}";
$str =~ s/($dx)/$dx$1/;
print length($str), "\n";
', "2\n");

test_transpile("s/// with variable in pattern and replacement (front)", '
my $dx = "\x{10f2}";
my $str = "\x{10f2}";
$str =~ s/($dx)/$1$dx/;
print length($str), "\n";
', "2\n");

test_transpile("LHS list repeat: (\$x)xN assigns N times (last wins)", '
my $a; my $b;
(($a)x3, $b) = 1..10;
print "$a, $b\n";
', "3, 4\n");

test_transpile("LHS list repeat: (undef)x\$dynamic skips dynamic slots", '
my $a; my $b;
($a, (undef)x${\6}, $b) = "a".."z";
print "$a$b\n";
', "ah\n");

test_transpile("(map ...)[N] list subscript returns Nth element", '
my @r = (map { {a=>$_} } ("x"))[0]->{a};
print "@r\n";
', "x\n");

test_transpile("(sort ...)[N] list subscript returns Nth element", '
my $f = (sort { $a <=> $b } 3,1,2)[0];
print "$f\n";
', "1\n");

test_transpile("(grep ...)[N] list subscript returns Nth element", '
my @s = (grep { $_ > 1 } 1,2,3)[1];
print "@s\n";
', "3\n");

test_transpile("bareword array subscript \$a[bar] treated as string index", '
my @a = (10, 20, 30);
my $x = $a[bar];
print defined($x) ? "ok" : "undef", "\n";
', "ok\n");

test_transpile("bareword hash subscript \$h{key} treated as string key", '
my %h = (foo => 42, bar => 99);
my $x = $h{bar};
print "$x\n";
', "99\n");

test_transpile("delete with bareword array subscript", '
my @a = (1, 2, 3);
delete $a[bar];
print scalar(@a), "\n";
', "3\n");

# --- my sub (lexical sub) name extraction ---
test_transpile("my sub name is extracted correctly (not 'my')", '
my sub greet () { "hello" }
print greet(), "\n";
', "hello\n");

test_transpile("my sub with signature works", '
my sub add ($x, $y) { $x + $y }
print add(3, 4), "\n";
', "7\n");

# --- alarm() no-op ---
test_transpile("alarm no-op returns 0", '
my $prev = alarm(5);
print defined($prev) ? "ok" : "undef", "\n";
alarm(0);
print "done\n";
', "ok\ndone\n");

# --- bareword strings: RHS of binary operator (a .. c) ---
test_transpile("bareword c in 'a .. c' range becomes string", '
my @r = ("a" .. "c");
print join(",", @r), "\n";
', "a,b,c\n");

test_transpile("bareword range a .. c (no quotes)", '
my @r = (a .. c);
print join(",", @r), "\n";
', "a,b,c\n");

# --- last LABEL across function call (dynamic scope) ---
test_transpile("last LABEL from inside called sub exits labeled block", '
my $ok = 0;
sub exit_block { last OUTER }
OUTER: {
    $ok = 1;
    exit_block();
    $ok = 0;
}
print "$ok\n";
', "1\n");

# --- goto LABEL and standalone LABEL ---
test_transpile("goto LABEL jumps to label within bare block", '
my $count = 0;
{
    top:
    $count++;
    goto top if $count < 3;
}
print "$count\n";
', "3\n");

done_testing;
