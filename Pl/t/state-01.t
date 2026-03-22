#!/usr/bin/env perl
# state-01.t - Tests for state variable code generation and runtime behavior
#
# Fixes implemented (session 94):
#   1. %p-flatten-list treated nil as empty list (listp nil = T in CL)
#      → changed (listp item) to (consp item) in %p-flatten-list
#   2. p-post++ returned nil for undef box instead of 0 (Perl undef++ = 0)
#      → old = (if (null val) 0 val)
#   3. state ($t) //= 3 list form and //= operator not handled
#      → _process_state_declaration now handles PPI::Structure::List and //=
#   4. Nested state vars in bare blocks not found by _find_all_declarations
#      → now recurses into PPI::Structure::Block (except anon sub bodies)
#   5. Anon sub state rename map replaced parent renames instead of merging
#      → merge with existing renames before setting
#   6. State vars initialized to nil (not a box) caused p-pre++/p-post++ to fail
#      → initial binding changed from nil to (make-p-box nil)

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

plan tests => 20;

# ── Helpers ─────────────────────────────────────────────────────────────────

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Transpilation tests ──────────────────────────────────────────────────────

# Test 1: state var generates outer let wrapper
{
    my $cl = transpile('use feature ":5.10"; sub f { state $x = 1; $x }');
    like($cl, qr/let.*state__f__x/s, 'state var generates outer let with unique name');
}

# Test 2: state var generates init guard
{
    my $cl = transpile('use feature ":5.10"; sub f { state $x = 1; $x }');
    like($cl, qr/unless.*__init/s, 'state var generates unless-init guard');
}

# Test 3: state ($t) //= expr generates init guard
{
    my $cl = transpile('use feature ":5.10"; sub f { state ($t) //= 3; $t }');
    like($cl, qr/unless.*__init/s, 'state list form with //= generates init guard');
}

# ── Runtime tests ────────────────────────────────────────────────────────────

# Test 4: basic state var without initializer — undef, persists
test_cl('state $x without init starts as undef',
    'use feature ":5.10";
     sub f { state $x; return $x // "undef" }
     print f(), "\n";',
    "undef\n");

# Test 5: state $x++ returns 0 on first call (undef++ = 0 in Perl)
test_cl('undef state var post-increment returns 0',
    'use feature ":5.10";
     sub f { state $x; return $x++ }
     my $r = f();
     print "$r\n";',
    "0\n");

# Test 6: state var persists across calls
test_cl('state var persists across calls',
    'use feature ":5.10";
     sub f { state $x = 10; ++$x }
     print f(), "\n";
     print f(), "\n";
     print f(), "\n";',
    "11\n12\n13\n");

# Test 7: my var does not persist (reinitialized each call)
test_cl('my var does not persist across calls',
    'use feature ":5.10";
     sub f { state $s = 0; my $m = 0; return (++$s, ++$m) }
     my ($s1, $m1) = f();
     my ($s2, $m2) = f();
     print "$s1 $m1\n";
     print "$s2 $m2\n";',
    "1 1\n2 1\n");

# Test 8: full stateful with all state forms (the core state.t test)
test_cl('full stateful function',
    'use feature ":5.10";
     sub stateful {
         state $x;
         state $y = 1;
         my $z = 2;
         state ($t) //= 3;
         return ($x++, $y++, $z++, $t++);
     }
     my ($x, $y, $z, $t) = stateful();
     print "$x $y $z $t\n";
     ($x, $y, $z, $t) = stateful();
     print "$x $y $z $t\n";
     ($x, $y, $z, $t) = stateful();
     print "$x $y $z $t\n";',
    "0 1 2 3\n1 2 2 4\n2 3 2 5\n");

# Test 9: state var in nested bare block
test_cl('state var in nested bare block inside sub',
    'use feature ":5.10";
     sub nesting {
         state $foo = 10;
         my $t;
         { state $bar = 12; $t = ++$bar }
         ++$foo;
         return ($foo, $t);
     }
     my ($x, $y) = nesting();
     print "$x $y\n";
     ($x, $y) = nesting();
     print "$x $y\n";',
    "11 13\n12 14\n");

# Test 10: generator closure — each call to generator gets fresh state
test_cl('generator: each closure gets independent state',
    'use feature ":5.10";
     sub generator {
         my $outer;
         sub { ++$outer; ++state $inner }
     }
     my $f1 = generator();
     print $f1->(), "\n";
     print $f1->(), "\n";
     my $f2 = generator();
     print $f2->(), "\n";
     print $f1->(), "\n";
     print $f2->(), "\n";',
    "1\n2\n1\n3\n2\n");

# Test 11: state array
test_cl('state array persists',
    'use feature ":5.10";
     sub f { state @arr; push @arr, scalar(@arr); return scalar(@arr) }
     print f(), "\n";
     print f(), "\n";
     print f(), "\n";',
    "1\n2\n3\n");

# Test 12: state hash
test_cl('state hash persists',
    'use feature ":5.10";
     sub f { state %h; my $k = scalar(keys %h); $h{$k} = 1; return scalar(keys %h) }
     print f(), "\n";
     print f(), "\n";',
    "1\n2\n");

# Test 13: state var is undef by default
test_cl('state var is undef by default',
    'use feature ":5.10";
     sub f { state $x; return !defined($x) ? "undef" : "defined" }
     print f(), "\n";',
    "undef\n");

# Test 14: multiple state vars in same sub
test_cl('multiple state vars are independent',
    'use feature ":5.10";
     sub f {
         state $a = 1;
         state $b = 100;
         return ($a++, $b--);
     }
     my ($a, $b) = f();
     print "$a $b\n";
     ($a, $b) = f();
     print "$a $b\n";',
    "1 100\n2 99\n");

# Test 15: state var in multiple different subs are independent
test_cl('state vars in different subs are independent',
    'use feature ":5.10";
     sub fa { state $x = 0; ++$x }
     sub fb { state $x = 0; ++$x }
     print fa(), "\n";
     print fa(), "\n";
     print fb(), "\n";
     print fa(), "\n";',
    "1\n2\n1\n3\n");

# Test 16: package-level state is same as package-level my (runs once)
test_cl('package-level state acts like my',
    'use feature ":5.10";
     state $pkg_state = 42;
     print "$pkg_state\n";',
    "42\n");

# Test 17: ++state $x in expression context (no separate state statement)
test_cl('++state $x in expression context',
    'use feature ":5.10";
     sub counter { ++state $n }
     print counter(), "\n";
     print counter(), "\n";
     print counter(), "\n";',
    "1\n2\n3\n");

# Test 18: state var not reset when called from different places
test_cl('state var persists regardless of call site',
    'use feature ":5.10";
     sub inc { state $n = 0; ++$n }
     sub double { inc(); inc() }
     print inc(), "\n";    # 1
     print double(), "\n"; # 3 (calls inc twice)
     print inc(), "\n";    # 4',
    "1\n3\n4\n");

# Test 19: p-pre-- on state var
test_cl('state var with pre-decrement',
    'use feature ":5.10";
     sub f { state $x = 5; --$x }
     print f(), "\n";
     print f(), "\n";',
    "4\n3\n");

# Test 20: state var init fires only once even if state var is changed
test_cl('state init fires only once',
    'use feature ":5.10";
     sub f {
         state $x = 99;
         my $old = $x;
         $x = 0;
         return $old;
     }
     print f(), "\n";  # 99 (init fires)
     print f(), "\n";  # 0  (stays 0, init does not re-fire)
     print f(), "\n";  # 0',
    "99\n0\n0\n");
