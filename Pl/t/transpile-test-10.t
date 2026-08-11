#!/usr/bin/env perl
# Transpile tests part 10 — the direction-D global-cell flip (tasks #289/#290).
# An ORDINARY package global is now a symbol macro over its own global cell
# (p-defcell) instead of a `defvar` special, and `local` on one lowers to
# p-local-cell instead of a dynamic `let`.  The rows here are the SEMANTIC
# consequences, each probed against real perl (docs/direction-d-plan.md §3).
# NEW TESTS GO HERE — the biggest file bounds the parallel suite's wall time
# (one SBCL spawn per row), so start a new file rather than grow one.

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

# Transpile-only helper (no SBCL spawn) for the two emission-shape rows.
sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `$pl2cl $pl_file 2>&1`;
}

# ---------------------------------------------------------------------------
# THE PARTITION, as emitted.  An ordinary user global is declared with
# p-defcell (symbol macro over its own global cell); $a/$b — bound by the sort
# lowering, so a dynamic binding IS the mechanism — stay defvar.  Pinning both
# arms in one row is the point: a partition that answers the same for both
# would still pass a one-arm test.
# ---------------------------------------------------------------------------
{
    my $cl = transpile('our $counter = 0; $counter++; print "$counter\n";');
    like($cl, qr/\(p-defcell \$counter /, 'ordinary global declared as a cell');
    like($cl, qr/\(defvar \$a \(make-p-box nil\)\)/,
         'the sort pair stays a defvar (exception set)');
}

# `local` follows the same partition: ordinary → p-local-cell (save/install/
# restore over the cell), exception → today's dynamic let.  Both in one file so
# the row fails if either arm drifts.
{
    my $cl = transpile('our $g = 1; sub w { local $g = 2; local $_ = "u"; f() } sub f {}');
    like($cl, qr/\(p-local-cell \$g /, 'local on an ordinary global uses the cell');
    like($cl, qr/\(let \(\(\$_ \(p-box-for-local/,
         'local on $_ keeps the dynamic let');
}

# ---------------------------------------------------------------------------
# §3.1 — a called sub must see the PACKAGE variable, not the caller's `my`
# shadow of the same name.  Under the old defvar model the caller's `let` was a
# dynamic binding, so the shadow leaked into the callee (this is what the
# poisoned-`my` rename machinery existed to paper over).
# ---------------------------------------------------------------------------
test_transpile('a my-shadow does not leak into a called sub', '
our $x = "global";
sub show { print "sub sees: $x\n" }
sub caller_with_my { my $x = "lexical"; show(); print "my sees: $x\n"; }
caller_with_my();
print "after: $x\n";
');

# §3.2 — a symbolic ref names the PACKAGE variable even where a `my` of the
# same name is in scope (perl: symbolic refs never see lexicals).
test_transpile('symbolic deref under a my-shadow reads the package variable', '
our $x = "global";
sub deref { my $x = "lexical"; my $n = "x"; no strict "refs"; print "symref: ${$n}\n"; }
deref();
');

# §3.3 — p-local-cell restores through a non-local exit, not just a normal one.
test_transpile('local on a cell restores after die', '
our $g = "outer";
sub boom { local $g = "inner"; die "bang\n" }
eval { boom() };
print "err=$@";
print "after die: $g\n";
');

# §3.4 — a declared-but-never-assigned global still reads as undef (p-defcell
# initializes the cell exactly once, like defvar).
test_transpile('never-assigned globals read as undef/empty', '
our $never; our @none; our %nohash;
print "never: [", (defined $never ? $never : "undef"), "]\n";
print "sizes: ", scalar(@none), " ", scalar(keys %nohash), "\n";
');

# ---------------------------------------------------------------------------
# `foreach $pkgvar (LIST)` IMPLICITLY LOCALIZES the package variable: the body
# (and anything it calls) sees the current element, and the old value is back
# after the loop.  With the loop var in a cell a plain `let` would install a
# lexical shadow the callee cannot see — so the loop macros localize the cell
# instead (%p-cell-loop-var-p).  Both loop flavors are here because they are
# separate expanders (p-foreach and p-foreach-range).
#
# NOT covered here: `for my $i (…)` where `$i` is ALSO a package global still
# lets the callee see the loop value (perl: it sees the global).  Probed at
# HEAD and after this change — identical, a pre-existing divergence; task #294.
# ---------------------------------------------------------------------------
test_transpile('foreach over a package global localizes the cell', '
our $i = "before";
sub peek { print "  callee sees: $i\n" }
for $i (1, 2) { print "body: $i\n"; peek() }
print "after list: $i\n";
for $i (7 .. 8) { print "range: $i\n"; peek() }
print "after range: $i\n";
');

# A loop that exits early must still restore — the restore is an
# unwind-protect, not a fall-through assignment.
test_transpile('foreach over a global restores after last/die', '
our $i = "before";
for $i (1, 2, 3) { last if $i == 2 }
print "after last: $i\n";
eval { for $i (1, 2, 3) { die "stop\n" if $i == 2 } };
print "after die: $i\n";
');

# ---------------------------------------------------------------------------
# One `local` statement can name several variables of BOTH partitions at once;
# the ordinary ones become nested cell opens and the exception ones stay in the
# let, and every one of them must restore.
# ---------------------------------------------------------------------------
test_transpile('mixed local: cells and dynamic bindings restore together', '
our @arr = (1,2,3); our %h = (a=>1); our $s = "S";
sub dump_all { print "in: @arr / ", join(",", map {"$_=$h{$_}"} sort keys %h), " / $s / $_\n" }
sub work { local @arr = (9); local %h = (z=>26); local $s = "T"; local $_ = "underscore"; dump_all() }
$_ = "outer_";
work();
dump_all();
');

# A multi-variable `local` with an initializer: the RHS reads the OLD values
# (it is evaluated in the let, before any cell is overwritten).
test_transpile('local (LIST) = RHS reads the old values', '
our ($p, $q) = ("P", "Q");
sub swap { local ($p, $q) = ($q, $p); print "in: $p $q\n" }
swap();
print "out: $p $q\n";
');

done_testing();
