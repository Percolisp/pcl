#!/usr/bin/env perl
# refaliasing-01.t — `use feature 'refaliasing' / 'declared_refs'`: a \-cast in
# LVALUE position is an ALIAS, not a value write (task #325).
#
# Before this existed, `\$x = \$y` emitted (p-setf (p-backslash $x) …), whose
# place was a FRESH ref box — so the write landed in a temporary and vanished.
# Four of the six spellings were SILENT WRONG that way; the other two were hard
# refusals.  The fix is one arm in p-setf's place dispatch: a \-cast place
# rebinds the NAME'S STORAGE to the right-hand referent, which in PCL's model
# is exactly "both names now hold the same box / vector / hash-table".
#
# Every expectation here was probed against real perl 5.40.3 first.
#
# The INVERSE rows matter as much as the positive ones: `my $x = \$y` must stay
# an ordinary reference (a \-cast in RVALUE position is untouched), and a
# right-hand side of the wrong kind must still die perl's death.

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

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 29;

my $PREAMBLE = "use feature 'refaliasing', 'declared_refs';\n"
             . "no warnings 'experimental';\n";

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $PREAMBLE, $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $PREAMBLE, $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# --- codegen: the \-cast place, and the one-element \(…) lvalue -------------

like(transpile('my $y; my $x; \$x = \$y;'),
    qr/\(p-setf \(p-backslash \$x\) /,
    'a \-cast LVALUE reaches p-setf as a (p-backslash …) place');
# `\($x) = LIST` is a LIST assignment even with one element (perl gives it list
# context); the rvalue shortcut \(scalar) == \scalar must not reach an lvalue.
like(transpile('my $x; my @l; \($x) = @l;'),
    qr/\(p-list-= \(vector \(p-backslash \$x\)\)/,
    '\($x) = LIST keeps its list-ness as an lvalue');

# --- scalars ---------------------------------------------------------------

test_cl('\$x = \$y aliases (write through either name)',
    q{my $y = 5; my $x; \$x = \$y; $x = 7; print "$y $x\n";}, "7 7\n");
test_cl('\my $x = \$y aliases at the declaration',
    q{my $y = 5; \my $x = \$y; $x = 7; print "$y $x\n";}, "7 7\n");
test_cl('an alias makes the two refs IDENTICAL',
    q{my $y; \my $x = \$y; print \$x == \$y ? "same\n" : "different\n";},
    "same\n");
# The right-hand side may be a VARIABLE holding the reference, which sits one
# box deeper than the \-wrapper — is-ref is what tells the two apart.
test_cl('alias through a variable that holds the ref',
    q{my $z = 3; my $r = \$z; \my $al = $r; $al = 7; print "$z\n";}, "7\n");
test_cl('alias through a ref-to-ref does not peel too far',
    q{my $w = 1; my $rr = \\\\$w; \my $x = $$rr; $x = 5; print "$w\n";}, "5\n");

# --- aggregates ------------------------------------------------------------

test_cl('\my @b = \@a aliases the array',
    q{my @a = (1,2); \my @b = \@a; push @b, 3; print "@a\n";}, "1 2 3\n");
test_cl('\my %g = \%h aliases the hash',
    q{my %h = (k=>1); \my %g = \%h; $g{j} = 2;
      print join(",", map {"$_=$h{$_}"} sort keys %h), "\n";}, "j=2,k=1\n");

# --- package variables, and the `our \$T` declaration ----------------------

test_cl('our \$T = \$::TODO declares the cell and aliases it',
    q{our $TODO = "orig"; our \$T = \$::TODO; $T = "tv"; print "$::TODO\n";},
    "tv\n");

# --- container slots -------------------------------------------------------

test_cl('\$h{k} = \$v aliases the hash SLOT',
    q{my $v = 5; my %h; \$h{k} = \$v; $h{k} = 8; print "$v\n";}, "8\n");
test_cl('\$a[i] = \$v aliases the array SLOT',
    q{my $v = 5; my @a; \$a[0] = \$v; $a[0] = 11; print "$v\n";}, "11\n");

# --- list forms ------------------------------------------------------------

test_cl('(\$x) = @list aliases through a list assignment',
    q{my $t = 1; @_ = \$t; my $x; (\$x) = @_; $x = 9; print "$t\n";}, "9\n");
test_cl('\(my $p) = @list aliases the fresh lexical',
    q{my $t = 1; @_ = \$t; \(my $p) = @_; $p = 9; print "$t\n";}, "9\n");

# --- foreach (task #327) ---------------------------------------------------
#
# PPI cannot lex `for \my %e (LIST) {…}` at all — the compound statement comes
# out holding only the word `for` and swallows the rest of the file — so the
# token-stream repair re-spells it as `for my $tmp (LIST) { \my %e = $tmp; … }`
# and the alias mechanism above does the work.

test_cl('for \my %e (LIST) aliases each element in turn',
    q{my @in = ({x=>1},{x=>2}); for \my %e (@in) { $e{seen} = 1 }
      print join("|", map { join(",", sort keys %$_) } @in), "\n";},
    "seen,x|seen,x\n");
test_cl('for \my $x (LIST) binds the referent, not the ref',
    q{my @out; for \my $topic (\"p", \"q") { push @out, $topic } print "@out\n";},
    "p q\n");
test_cl('for \my @b (LIST) aliases arrays',
    q{my @out; for \my @b ([1,2],[3,4]) { push @out, @b } print "@out\n";},
    "1 2 3 4\n");
# A PACKAGE loop variable needs no save/restore: probed on perl 5.40.3, perl
# does NOT restore an aliased package loop variable when the loop ends.
test_cl('for \%::a (LIST) leaves the last alias in place, as perl does',
    q{our %a = (orig=>1); my @in = ({x=>1},{x=>2});
      for \%::a (@in) { } print join(",", sort keys %a), "\n";}, "x\n");
test_cl('for \$::s (LIST) likewise',
    q{our $s = "orig"; for \$::s (\"a", \"b") { } print "$s\n";}, "b\n");
# INVERSE: an ordinary foreach — and one whose LIST holds \-refs — must be
# untouched by the repair (it keys on a \-cast in the LOOP VARIABLE position).
test_cl('an ordinary foreach over a list of refs is not rewritten',
    q{my ($p,$q) = (1,2); my @out;
      for my $r (\$p, \$q) { push @out, $$r } print "@out\n";}, "1 2\n");

# --- n-at-a-time foreach (task #329) ---------------------------------------
#
# perl 5.36's `for my ($q, $r) (LIST)`.  PPI cannot lex it either, and the
# repair re-spells it as a `while` over `map \$_, LIST` — one write-through ref
# per element — with `\my $q = $L[$I]` doing the aliasing.  So these rows guard
# the same mechanism from a second direction.

test_cl('for my ($q,$r) (LIST) walks two at a time',
    q{my @have; for my ($q,$r) ('A','B','C','D') { push @have, "$q;$r" }
      print "@have\n";}, "A;B C;D\n");
test_cl('a short final chunk leaves the extra variables undef',
    q{my @have; for my ($q,$r,$s) (1..5) {
        push @have, join ";", map { $_ // 'undef' } $q,$r,$s } print "@have\n";},
    "1;2;3 4;5;undef\n");
test_cl('the loop variables ALIAS the array elements',
    q{my @b = (1,2,3,4); for my ($q,$r) (@b) { ($q,$r) = ($r,$q) } print "@b\n";},
    "2 1 4 3\n");
# A multi-term list is the case `\(LIST)` gets wrong (it would give one ARRAY
# ref per term); `map \$_` distributes over the ELEMENTS, as perl does.
test_cl('a list of several arrays aliases element by element',
    q{my @Q = qw(a b c); my @A = qw(d e f);
      for my ($l,$r) (@Q,@A) { $l = uc $l } print "@Q | @A\n";},
    "A b C | d E f\n");
test_cl('a hash flattens to key/value pairs and the VALUES alias',
    q{my %h = (k=>1); for my ($key,$val) (%h) { $val = 9 } print "$h{k}\n";},
    "9\n");
# next runs the continue block and the step, redo runs neither, and the loop
# keeps its own continue block's statements.
test_cl('redo / next / continue land where perl puts them',
    q{my @n = (3,2,1,0); my ($redo,$next,$cont);
      for my ($l,$r) (@n) { $l *= 3; ++$r; redo unless $redo++; next unless $next++;
                            $l *= 5; $r *= 7 } continue { $cont .= 'x' }
      print "@n|$redo|$next|$cont\n";}, "27 4 15 7|3|2|xx\n");
# INVERSE: an ordinary one-variable foreach must be untouched by the repair.
test_cl('an ordinary foreach is not rewritten',
    q{my @o; for my $x (1,2,3) { push @o, $x*2 } print "@o\n";}, "2 4 6\n");

# --- INVERSE: a \-cast in RVALUE position is untouched ---------------------

test_cl('my $r = \$y is still a REFERENCE, not an alias',
    q{my $y = 5; my $r = \$y; $r = 7; print "$y $r\n";}, "5 7\n");
# A wrong-kind right-hand side must die perl's death rather than alias anything.
like(run_cl(q{my $x; my @a; eval { \$x = \@a; 1 } or print "died\n";}),
    qr/died/, 'aliasing a scalar to an ARRAY ref dies');
