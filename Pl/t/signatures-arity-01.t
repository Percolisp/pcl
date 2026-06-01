#!/usr/bin/env perl
# signatures-arity-01.t - Runtime behavior of subroutine-signature arity:
#   - args flatten into the signature (foo(@arr) spreads), which previously
#     crashed (the sub got the raw array as a single arg)
#   - empty signature () rejects extra args
#   - optional defaults and slurpy params bind from the flattened @_
#   - too-few / too-many throw Perl's exact error message, including the
#     " at (eval N) line 1." suffix that string eval appends
#
# Fixes implemented (session 226): signature subs now capture via &rest,
# flatten %_args into @_, run p-check-arity, and bind params from @_.

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

plan tests => 10;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code; close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code; close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub test_cl { my ($n, $c, $e) = @_; is(run_cl($c), $e, $n); }

my $H = 'use feature "signatures"; no warnings;';

# 1: required params bind positionally
test_cl('two required params',
    "$H\nsub f (\$a, \$b) { \"\$a/\$b\" }\nprint f(3, 4), \"\\n\";",
    "3/4\n");

# 2: array argument FLATTENS into the signature (was a crash)
test_cl('array arg flattens into signature',
    "$H\nsub f (\$a, \$b) { \"\$a/\$b\" }\nmy \@x = (10, 20);\nprint f(\@x), \"\\n\";",
    "10/20\n");

# 3: optional default used when arg omitted
test_cl('optional default applied',
    "$H\nsub f (\$a, \$b = 99) { \"\$a/\$b\" }\nprint f(5), \"|\", f(5, 6), \"\\n\";",
    "5/99|5/6\n");

# 4: optional default can reference an earlier param
test_cl('optional default references earlier param',
    "$H\nsub f (\$a, \$b = \$a * 2) { \"\$a/\$b\" }\nprint f(4), \"\\n\";",
    "4/8\n");

# 5: slurpy @rest collects the remaining args
test_cl('slurpy array param',
    "$H\nsub f (\$x, \@r) { \"\$x:[\@r]\" }\nprint f(1, 2, 3, 4), \"\\n\";",
    "1:[2 3 4]\n");

# 6: empty signature accepts zero args
test_cl('empty signature, zero args ok',
    "$H\nsub f () { 42 }\nprint f(), \"\\n\";",
    "42\n");

# 7: empty signature rejects extra args (eval returns undef)
test_cl('empty signature rejects extra args',
    "$H\nsub f () { 42 }\nmy \$r = eval { f(1) };\nprint defined(\$r) ? \"got \$r\" : \"undef\", \"\\n\";",
    "undef\n");

# 8: too-few throws Perl's exact message (string eval adds the eval-line suffix)
test_cl('too few arguments message',
    "$H\nsub t (\$a, \$b) { 1 }\neval(\"t(5)\");\nprint \$\@;",
    "Too few arguments for subroutine 'main::t' (got 1; expected 2) at (eval 1) line 1.\n");

# 9: too-many (fixed arity) throws Perl's exact message
test_cl('too many arguments message',
    "$H\nsub t (\$a, \$b) { 1 }\neval(\"t(5, 6, 7)\");\nprint \$\@;",
    "Too many arguments for subroutine 'main::t' (got 3; expected 2) at (eval 1) line 1.\n");

# 10: flexible wording for subs with optional params ("at most")
test_cl('flexible "at most" message for optional-param sub',
    "$H\nsub t (\$a = 1) { 1 }\neval(\"t(5, 6)\");\nprint \$\@;",
    "Too many arguments for subroutine 'main::t' (got 2; expected at most 1) at (eval 1) line 1.\n");
