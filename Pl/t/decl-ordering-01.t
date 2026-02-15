#!/usr/bin/env perl

# Tests for declaration ordering / two-phase reordering system.
# Focus: corner cases that stress the Phase 1 (defvar hoisting) and
# Phase 2 (compile-time vs runtime reordering) in Parser.pm.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use File::Spec;

use lib ".";
use Pl::Parser;

# Helper: parse and return CL output (no SBCL, fast)
sub parse_pl {
    my $code = shift;
    return Pl::Parser->parse_code($code);
}

# Helper: get the main section of output (after in-package :pcl or :main)
# Returns lines after last in-package, stripping comment-only and blank lines
sub main_forms {
    my $cl = shift;
    my @lines = split /\n/, $cl;
    my @forms;
    for my $line (@lines) {
        next if $line =~ /^\s*$/;
        next if $line =~ /^;;\s/;
        push @forms, $line;
    }
    return @forms;
}

# Helper: find relative position of first match for two patterns
# Returns -1 if A before B, 0 if same or not found, 1 if A after B
sub relative_order {
    my ($cl, $pat_a, $pat_b) = @_;
    my $pos_a = -1;
    my $pos_b = -1;
    my @lines = split /\n/, $cl;
    for my $i (0 .. $#lines) {
        $pos_a = $i if $pos_a < 0 && $lines[$i] =~ $pat_a;
        $pos_b = $i if $pos_b < 0 && $lines[$i] =~ $pat_b;
    }
    return 0 if $pos_a < 0 || $pos_b < 0;
    return $pos_a <=> $pos_b;
}

my $pl2cl   = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';

# Helper: run transpiled code through SBCL
sub run_pcl {
    my ($code) = @_;

    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl --no-cache $pl_file 2>/dev/null`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

    # Filter SBCL noise
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;

    unlink $pl_file, $cl_file;
    chomp $output;
    return $output;
}


# ============================================================
# Parse-level tests: verify output STRUCTURE
# ============================================================

note "-------- Phase 2: compile-time before runtime";

# Test: forward call — sub definition appears before runtime call
{
    my $cl = parse_pl(q{
        print foo();
        sub foo { return 42; }
    });
    is(relative_order($cl, qr/\(pl-sub pl-foo/, qr/\(pl-print/), -1,
       'forward call: pl-sub before pl-print');
}

# Test: interleaved subs and runtime — all subs moved before all runtime
{
    my $cl = parse_pl(q{
        sub a { return 1; }
        print "x";
        sub b { return a() + 1; }
        print "y";
    });
    is(relative_order($cl, qr/\(pl-sub pl-b/, qr/\(pl-print "x"/), -1,
       'interleaved: sub b before first runtime print');
    is(relative_order($cl, qr/\(pl-sub pl-a/, qr/\(pl-sub pl-b/), -1,
       'interleaved: source order preserved — sub a before sub b');
}

# Test: use constant interleaved with runtime (only constants, no regular subs)
# This is the key corner case: use constant generates pl-sub but doesn't register
# as a declared sub, so Phase 2 must still run.
{
    my $cl = parse_pl(q{
        use constant A => 1;
        $x = A;
        use constant B => 2;
        print $x + B;
    });
    is(relative_order($cl, qr/\(pl-sub pl-B/, qr/\(pl-scalar-= \$x/), -1,
       'use constant only: constant B reordered before runtime $x assignment');
    is(relative_order($cl, qr/\(pl-sub pl-A/, qr/\(pl-sub pl-B/), -1,
       'use constant only: source order preserved — A before B');
}

# Test: use constant + sub mixed — Phase 2 moves all before runtime
{
    my $cl = parse_pl(q{
        use constant SCALE => 10;
        $x = 1;
        use constant OFFSET => 5;
        sub compute { return $x * SCALE + OFFSET; }
        print compute();
    });
    is(relative_order($cl, qr/\(pl-sub pl-OFFSET/, qr/\(pl-scalar-= \$x/), -1,
       'mixed constant+sub: OFFSET before runtime');
    is(relative_order($cl, qr/\(pl-sub pl-compute/, qr/\(pl-scalar-= \$x/), -1,
       'mixed constant+sub: compute before runtime');
    is(relative_order($cl, qr/\(pl-sub pl-SCALE/, qr/\(pl-sub pl-OFFSET/), -1,
       'mixed constant+sub: source order SCALE before OFFSET');
}

# Test: END block classified as compile-time
{
    my $cl = parse_pl(q{
        sub foo { return 1; }
        print "runtime";
        END { print "end"; }
    });
    is(relative_order($cl, qr/\(push \(lambda/, qr/\(pl-print "runtime"/), -1,
       'END block reordered before runtime');
}

# Test: my $x at file scope stays in runtime (box-set, not moved to compile-time)
{
    my $cl = parse_pl(q{
        sub foo { return 1; }
        my $x = foo();
        print $x;
    });
    # my $x = foo() at file scope generates defvar + box-set
    # The box-set should be AFTER the sub (in runtime section)
    is(relative_order($cl, qr/\(pl-sub pl-foo/, qr/\(box-set \$x/), -1,
       'my $x (box-set) stays in runtime, after compile-time sub');
}

# Test: END blocks interleaved with subs — all compile-time, order preserved
{
    my $cl = parse_pl(q{
        sub a { 1 }
        END { print "end1"; }
        sub b { 2 }
        END { print "end2"; }
        print "runtime";
    });
    # All four compile-time forms before runtime
    is(relative_order($cl, qr/\(push \(lambda/, qr/\(pl-print "runtime"/), -1,
       'END blocks before runtime');
    # Source order within compile-time: sub a, END1, sub b, END2
    # Match END blocks via their comment lines (push (lambda is multi-line)
    is(relative_order($cl, qr/\(pl-sub pl-a/, qr/;; END.*end1/), -1,
       'source order: sub a before END1');
    is(relative_order($cl, qr/;; END.*end1/, qr/\(pl-sub pl-b/), -1,
       'source order: END1 before sub b');
}


note "-------- Phase 1: defvar hoisting";

# Test: defvar hoisted before sub definitions
{
    my $cl = parse_pl(q{
        sub get_x { return $x; }
        our $x = 10;
    });
    is(relative_order($cl, qr/defvar \$x/, qr/\(pl-sub pl-get_x/), -1,
       'defvar hoisted before sub that uses the variable');
}

# Test: multiple defvars all hoisted before subs
{
    my $cl = parse_pl(q{
        sub compute { local $x = 10; local $y = 20; return $x + $y; }
        our $x = 1;
        our $y = 2;
    });
    is(relative_order($cl, qr/defvar \$x/, qr/\(pl-sub pl-compute/), -1,
       'defvar $x hoisted before sub');
    is(relative_order($cl, qr/defvar \$y/, qr/\(pl-sub pl-compute/), -1,
       'defvar $y hoisted before sub');
}

# Test: defvar value assignment stays at original runtime position
{
    my $cl = parse_pl(q{
        sub foo { return $x; }
        our $x = 42;
        print foo();
    });
    # defvar (declaration) before sub
    is(relative_order($cl, qr/defvar \$x/, qr/\(pl-sub pl-foo/), -1,
       'defvar declaration before sub');
    # setf (value) after sub (in runtime section)
    is(relative_order($cl, qr/\(pl-sub pl-foo/, qr/setf.*pl-box-value.*\$x.*42/), -1,
       'defvar value assignment stays in runtime section');
}


note "-------- Phase 1: nested sub stubs";

# Test: nested sub (inside another sub) gets pl-declare-sub stub
{
    my $cl = parse_pl(q{
        sub outer {
            sub inner { return 42; }
            return inner();
        }
        print outer();
    });
    like($cl, qr/\(pl-declare-sub pl-inner\)/,
         'nested sub gets pl-declare-sub stub');
    is(relative_order($cl, qr/\(pl-declare-sub pl-inner\)/, qr/\(pl-sub pl-outer/), -1,
       'stub appears before enclosing sub');
}

# Test: top-level sub does NOT get pl-declare-sub stub
{
    my $cl = parse_pl(q{
        sub foo { return 1; }
        print foo();
    });
    unlike($cl, qr/\(pl-declare-sub pl-foo\)/,
           'top-level sub does not get pl-declare-sub');
}


note "-------- Phase 1: package pre-declarations";

# Test: qualified sub triggers defpackage for its package
{
    my $cl = parse_pl(q{
        sub Util::helper { return 1; }
    });
    like($cl, qr/\(defpackage :Util\b/, 'qualified sub triggers defpackage');
    is(relative_order($cl, qr/defpackage :Util/, qr/Util::pl-helper/), -1,
       'defpackage before the qualified sub');
}

# Test: pipe-quoted package reference triggers defpackage
{
    my $cl = parse_pl(q{
        package Foo::Bar;
        sub test { return 1; }
        package main;
        print Foo::Bar::test();
    });
    like($cl, qr/defpackage :\|Foo::Bar\|/,
         'Foo::Bar gets pipe-quoted defpackage');
}


note "-------- Multi-package sections";

# Test: multiple packages reordered independently
{
    my $cl = parse_pl(q{
        package A;
        print "a-runtime";
        sub a_func { return 1; }
        package B;
        print "b-runtime";
        sub b_func { return 2; }
    });
    # In A section: sub before runtime
    is(relative_order($cl, qr/\(pl-sub pl-a_func/, qr/pl-print "a-runtime"/), -1,
       'package A: sub before runtime');
    # In B section: sub before runtime
    is(relative_order($cl, qr/\(pl-sub pl-b_func/, qr/pl-print "b-runtime"/), -1,
       'package B: sub before runtime');
}

# Test: same package appearing twice — second section also reordered
{
    my $cl = parse_pl(q{
        package A;
        sub first { return 1; }
        print "first-run";
        package B;
        sub mid { return 2; }
        package A;
        print "second-run";
        sub second { return 3; }
    });
    # Second A section: sub second before "second-run"
    # Find the SECOND occurrence of in-package :A
    my @lines = split /\n/, $cl;
    my $second_a_start = -1;
    my $seen_a = 0;
    for my $i (0 .. $#lines) {
        if ($lines[$i] =~ /\(in-package :A\)/) {
            $seen_a++;
            $second_a_start = $i if $seen_a == 2;
        }
    }
    ok($second_a_start > 0, 'found second in-package :A');

    # After the second in-package :A, sub should come before print
    my ($found_sub, $found_print) = (0, 0);
    for my $i ($second_a_start .. $#lines) {
        $found_sub   = $i if !$found_sub   && $lines[$i] =~ /pl-sub pl-second/;
        $found_print = $i if !$found_print && $lines[$i] =~ /pl-print "second-run"/;
    }
    ok($found_sub && $found_print && $found_sub < $found_print,
       'second A section: sub second before "second-run"');
}


# ============================================================
# Runtime tests: verify SEMANTIC correctness
# ============================================================

note "-------- Runtime: forward calls";

# Test: simple forward call
{
    my $output = run_pcl(q{
        print foo();
        sub foo { return 42; }
    });
    is($output, '42', 'forward call: sub used before definition');
}

# Test: mutual recursion
{
    my $output = run_pcl(q{
        sub is_even {
            my ($n) = @_;
            return 1 if $n == 0;
            return is_odd($n - 1);
        }
        sub is_odd {
            my ($n) = @_;
            return 0 if $n == 0;
            return is_even($n - 1);
        }
        print is_even(4) ? "yes" : "no";
    });
    is($output, 'yes', 'mutual recursion works (both subs defined before call)');
}

# Test: forward call with runtime code interleaved
{
    my $output = run_pcl(q{
        our $order = "";
        $order .= "R1 ";
        sub foo { return "F" }
        $order .= "R2 ";
        sub bar { return foo() . "B" }
        $order .= "R3 ";
        print $order . bar();
    });
    is($output, 'R1 R2 R3 FB',
       'forward call: runtime code runs in source order, subs available');
}


note "-------- Runtime: local / dynamic scoping (defvar ordering)";

# Test: local provides dynamic scoping (defvar must precede defun)
{
    my $output = run_pcl(q{
        our $x = 10;
        sub get_x { return $x; }
        sub test { local $x = 20; return get_x(); }
        print test() . " " . get_x();
    });
    is($output, '20 10', 'local: dynamic scoping works (defvar before defun)');
}

# Test: triple-deep local — wrap calls deeper calls show
{
    my $output = run_pcl(q{
        our $x = "global";
        sub show { return $x; }
        sub wrap {
            local $x = "wrapped";
            return deeper();
        }
        sub deeper {
            local $x = "deep";
            return show();
        }
        print wrap() . " " . show();
    });
    is($output, 'deep global',
       'triple-deep local: innermost dynamic binding visible');
}

# Test: multiple local variables in same sub
{
    my $output = run_pcl(q{
        our $a = 1;
        our $b = 2;
        sub get_sum { return $a + $b; }
        sub test {
            local $a = 10;
            local $b = 20;
            return get_sum();
        }
        print test() . " " . get_sum();
    });
    is($output, '30 3', 'multiple local vars: both dynamically scoped');
}


note "-------- Runtime: use constant interactions";

# Test: use constant used in sub and runtime
{
    my $output = run_pcl(q{
        use constant FACTOR => 7;
        sub multiply { my ($n) = @_; return $n * FACTOR; }
        print multiply(6);
    });
    is($output, '42', 'use constant in sub body');
}

# Test: use constant with local — constant + dynamic interaction
{
    my $output = run_pcl(q{
        use constant SCALE => 10;
        our $base = 5;
        sub compute {
            local $base = 100;
            return inner();
        }
        sub inner { return $base * SCALE; }
        print compute() . " " . inner();
    });
    is($output, '1000 50', 'use constant + local: constant value and dynamic var');
}

# Test: use constant only (no regular subs) — constants reordered before runtime
{
    my $output = run_pcl(q{
        use constant X => 10;
        my $result = X + 5;
        use constant Y => 20;
        print $result + Y;
    });
    is($output, '35', 'use constant only: constants available before runtime');
}


note "-------- Runtime: nested subs and closures";

# Test: nested sub callable via forward stub
{
    my $output = run_pcl(q{
        sub outer {
            sub inner { return 42; }
            return inner();
        }
        print outer();
    });
    is($output, '42', 'nested sub: inner callable from outer via stub');
}

# Test: nested sub callable from outside outer (Perl semantics)
# In Perl, sub inner {} inside sub outer {} installs inner as a package sub
# when outer runs. Before outer runs, inner may or may not be defined.
{
    my $output = run_pcl(q{
        sub outer {
            sub inner { return 99; }
        }
        outer();
        print inner();
    });
    is($output, '99', 'nested sub: inner callable after outer runs');
}


note "-------- Runtime: cross-package interactions";

# Test: cross-package forward call
{
    my $output = run_pcl(q{
        package Util;
        sub double { my ($n) = @_; return $n * 2; }

        package main;
        print Util::double(21);
    });
    is($output, '42', 'cross-package: main calls Util sub');
}

# Test: two packages calling each other
{
    my $output = run_pcl(q{
        package A;
        sub value { return 10; }

        package B;
        sub value { return A::value() + 5; }

        package main;
        print B::value();
    });
    is($output, '15', 'cross-package: B calls A, main calls B');
}

# Test: cross-package with local/dynamic scoping
{
    my $output = run_pcl(q{
        package Config;
        our $mode = "normal";
        sub get_mode { return $mode; }

        package main;
        sub run_debug {
            local $Config::mode = "debug";
            return Config::get_mode();
        }
        print run_debug() . " " . Config::get_mode();
    });
    is($output, 'debug normal',
       'cross-package local: dynamic scoping across packages');
}


note "-------- Runtime: BEGIN + sub interactions";

# Test: BEGIN calls sub defined before it (source order)
{
    my $output = run_pcl(q{
        our $result = "";
        sub greet { return "hello"; }
        BEGIN { $result = greet(); }
        print $result;
    });
    like($output, qr/hello/, 'BEGIN calls sub defined before it');
}

# Test: sub defined after BEGIN — not available in BEGIN
# (In Perl, both run at compile-time in source order)
# We test that the sub IS available at runtime even if BEGIN can't use it
{
    my $output = run_pcl(q{
        sub late { return "late"; }
        print late();
    });
    is($output, 'late', 'sub defined and called — basic sanity');
}


note "-------- Runtime: use/require in BEGIN blocks";

# Test: BEGIN does require, sub uses required function
{
    my $tempdir = tempdir(CLEANUP => 1);
    my $mod = File::Spec->catfile($tempdir, "Helper.pm");
    open my $fh, '>', $mod or die;
    print $fh <<'EOF';
package Helper;
sub compute { return $_[0] * 2; }
1;
EOF
    close $fh;

    my $output = run_pcl(qq{
        use lib "$tempdir";
        BEGIN { require Helper; }
        sub doubled { return Helper::compute(21); }
        print doubled();
    });
    is($output, '42', 'BEGIN requires module, sub uses its function');
}

# Test: use inside BEGIN (same as regular use, but explicit)
{
    my $tempdir = tempdir(CLEANUP => 1);
    my $mod = File::Spec->catfile($tempdir, "Adder.pm");
    open my $fh, '>', $mod or die;
    print $fh <<'EOF';
package Adder;
sub add { return $_[0] + $_[1]; }
1;
EOF
    close $fh;

    my $output = run_pcl(qq{
        use lib "$tempdir";
        use Adder;
        print Adder::add(20, 22);
    });
    is($output, '42', 'use loads module, function callable');
}


note "-------- Runtime: edge cases in ordering";

# Test: sub that calls another sub defined after it (both top-level)
{
    my $output = run_pcl(q{
        sub first { return second() + 1; }
        sub second { return 41; }
        print first();
    });
    is($output, '42', 'sub calls later-defined sub (both compile-time)');
}

# Test: defvar value not available at compile-time (only declaration is)
# In Perl: our $x = 10; BEGIN { print $x } — $x is undef in BEGIN
{
    my $output = run_pcl(q{
        our $x = 10;
        sub get_x { return $x; }
        print get_x();
    });
    is($output, '10', 'our var: value available at runtime');
}

# Test: multiple use constants used in a computation
{
    my $output = run_pcl(q{
        use constant PI  => 3.14159;
        use constant TAU => 2 * PI;
        print int(TAU * 100);
    });
    # TAU = 2 * PI; but PI is a sub call, so this tests that constant
    # definitions are available in order
    is($output, '628', 'use constant: TAU depends on PI, both available');
}

# Test: auto-vivified global + forward call (variable forward decl + Phase 2)
{
    my $output = run_pcl(q{
        $x = 42;
        sub get_x { return $x; }
        print get_x();
    });
    is($output, '42', 'auto-vivified global: defvar + forward call');
}


# Test: bare require stays in source order (not hoisted before chdir)
# Regression: sessions 28-29 reordering hoisted require before chdir,
# breaking oct.t, context.t, etc.
{
    my $cl = parse_pl(q{
        chdir 't' if -d 't';
        require './test.pl';
        print "hello";
    });
    # require should come AFTER chdir, not be hoisted before it
    is(relative_order($cl, qr/pl-chdir/, qr/pl-require-file/), -1,
       'bare require stays after chdir (not hoisted as compile-time)');
}

# Test: use IS still hoisted (compile-time)
{
    my $cl = parse_pl(q{
        $x = 1;
        use Carp;
        $y = 2;
    });
    # use should come before the runtime setf
    is(relative_order($cl, qr/pl-use.*Carp/, qr/pl-scalar-=.*\$y/), -1,
       'use is still hoisted as compile-time');
}

done_testing();
