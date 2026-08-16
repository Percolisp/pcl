#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

use lib ".";
use Pl::Parser2;

# Helper to parse and return CL output
sub parse_pl {
    my $code = shift;
        return Pl::Parser2->parse_code($code);
}

# Helper to run transpiled code.  Through pl2cl (not parse_code): the
# package-switching tests below need the (p-defpackage :main) preamble
# that only the file entry emits — a bare body's (in-package :main)
# would hit a package that does not exist yet.
sub run_pl {
    my $code = shift;
    my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl');
    print $pfh $code;
    close $pfh;
    my $cl_code = PCLCore::transpile(qq{./pl2cl "$pl_file"});
    unlink $pl_file;

    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;

    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;

    # Filter SBCL noise and style warnings
    $output =~ s/^;.*\n//gm;             # Remove comment lines (warnings)
    $output =~ s/^\s*\n//gm;             # Remove blank lines
    $output =~ s/PCL Runtime loaded\n?//g;  # Remove runtime message
    $output =~ s/^caught .*\n//gm;       # Remove "caught N conditions" lines
    $output =~ s/^compilation unit.*\n//gm; # Remove compilation unit messages
    $output =~ s/^\s+//;                 # Trim leading whitespace

    return $output;
}

plan tests => 30;

say "# -------- 'our' Transpilation Tests:";

# Test: our $x = value
# Declaration (compile-time) uses nil, initialization (runtime) sets value
{
    my $cl = parse_pl('our $count = 0;');
    like($cl, qr/p-defcell \$count/, q{our $x = val declares an ordinary global cell});
    # Same claim as v1's `setf p-box-value` row: the init is a RUNTIME write
    # into the declared box, separate from the compile-time defvar.  v2
    # spells that write (p-scalar-= $count 0).
    like($cl, qr/p-scalar-= \$count 0/, 'our with value generates runtime init assignment');
}

# Test: bare our $x
{
    my $cl = parse_pl('our $flag;');
    like($cl, qr/p-defcell \$flag/, q{bare our declares a cell});
    like($cl, qr/make-p-box nil/, 'bare our initializes to nil');
}

# Test: our with list
{
    my $cl = parse_pl('our ($x, $y) = (1, 2);');
    like($cl, qr/p-defcell \$x/, q{our list declares first var});
    like($cl, qr/p-defcell \$y/, q{our list declares second var});
}

# Test: our in package
{
    my $cl = parse_pl('package Counter; our $count = 0;');
    like($cl, qr/in-package :Counter/, 'package declaration emitted');
    like($cl, qr/p-defcell \$count/, q{our in package declares a cell});
}

say "# -------- 'our' Runtime Tests:";

# Test: our variable persists across calls
{
    my $output = run_pl(q{
package Counter;
our $count = 0;
sub increment { $count++; return $count; }

package main;
say Counter::increment();
say Counter::increment();
say Counter::increment();
});
    like($output, qr/^1\n2\n3/, 'our variable persists across calls');
}

# Test: our variable accessible in multiple subs
{
    my $output = run_pl(q{
package State;
our $value = 10;
sub get { return $value; }
sub set { $value = $_[0]; }
sub double { $value = $value * 2; }

package main;
say State::get();
State::set(5);
say State::get();
State::double();
say State::get();
});
    like($output, qr/^10\n5\n10/, 'our variable shared between subs');
}

# Test: our with array
{
    my $output = run_pl(q{
package Data;
our @items = ();
sub add { push @items, $_[0]; }
sub count { return scalar @items; }

package main;
Data::add("a");
Data::add("b");
Data::add("c");
say Data::count();
});
    like($output, qr/^3/, 'our array works');
}

# Test: our with hash
{
    my $output = run_pl(q{
package Cache;
our %data = ();
sub set { $data{$_[0]} = $_[1]; }
sub get { return $data{$_[0]}; }

package main;
Cache::set("key", "value");
say Cache::get("key");
});
    like($output, qr/^value/, 'our hash works');
}

# Test: cross-package variable access
{
    my $output = run_pl(q{
package Config;
our $debug = 1;
our $version = "2.0";

package main;
say $Config::debug;
say $Config::version;
});
    like($output, qr/^1\n2\.0/, 'cross-package our access works');
}

# Test: multiple packages with independent our vars
{
    my $output = run_pl(q{
package A;
our $val = 100;

package B;
our $val = 200;

package main;
say $A::val;
say $B::val;
});
    like($output, qr/^100\n200/, 'independent our vars in different packages');
}

# Test: nested package names with our
{
    my $cl = parse_pl(q{
package Foo::Bar;
our $setting = "test";
});
    like($cl, qr/\|Foo::Bar\|/, 'nested package name uses pipe quoting');
    like($cl, qr/p-defcell \$setting/, q{our in nested package works});
}

# Test: our variable modification persists
{
    my $output = run_pl(q{
package Store;
our $total = 0;
sub add { $total = $total + $_[0]; }
sub get { return $total; }

package main;
Store::add(5);
Store::add(10);
Store::add(3);
say Store::get();
});
    like($output, qr/^18/, 'our variable accumulates correctly');
}

# Test: our array with push/pop
{
    my $output = run_pl(q{
package Stack;
our @items = ();
sub push_item { push @items, $_[0]; }
sub pop_item { return pop @items; }
sub size { return scalar @items; }

package main;
Stack::push_item("a");
Stack::push_item("b");
Stack::push_item("c");
say Stack::size();
say Stack::pop_item();
say Stack::size();
});
    like($output, qr/^3\nc\n2/, 'our array with push/pop works');
}

# Test: our hash with keys
{
    my $output = run_pl(q{
package Registry;
our %items = ();
sub register { $items{$_[0]} = $_[1]; }
sub count { return scalar keys %items; }

package main;
Registry::register("a", 1);
Registry::register("b", 2);
Registry::register("c", 3);
say Registry::count();
});
    like($output, qr/^3/, 'our hash with keys works');
}

say "# -------- 'local' Transpilation Tests:";

# The declarer and the `local` lowering below both changed shape in s382h
# (task #289, direction D): an ORDINARY package global is a symbol macro over
# its own cell (`p-defcell`) and `local` on one is `p-local-cell`, because a
# `let` of a cell name would be a LEXICAL shadow no called sub can see.  The
# exception set ($a/$b, punctuation magic) keeps defvar + the dynamic let —
# pinned by Pl/t/transpile-test-10.t, which asserts both arms together.

# Test: local $x = value generates let
{
    my $cl = parse_pl('local $x = 20;');
    like($cl, qr/\(p-local-cell \$x /, q{local on an ordinary global opens a p-local-cell});
    like($cl, qr/p-box-for-local\s+20/, 'local with init uses p-box-for-local');
}

# Test: bare local $x generates let with nil
{
    my $cl = parse_pl('local $x;');
    like($cl, qr/\(p-local-cell \$x /, q{bare local opens a p-local-cell});
    like($cl, qr/make-p-box nil/, 'bare local initializes to nil');
}

say "# -------- 'local' Runtime Tests:";

# Test: local restores value after scope
{
    my $output = run_pl(q{
our $x = 10;
sub test {
    local $x = 20;
    say $x;
}
test();
say $x;
});
    like($output, qr/^20\n10/, 'local restores value after scope exit');
}

# Test: local provides true dynamic scoping
{
    my $output = run_pl(q{
our $x = 10;
sub get_x { return $x; }
sub test {
    local $x = 20;
    say get_x();
}
test();
say get_x();
});
    like($output, qr/^20\n10/, 'local provides dynamic scoping to called functions');
}

# Test: nested local
{
    my $output = run_pl(q{
our $x = 1;
sub outer {
    local $x = 2;
    inner();
}
sub inner {
    local $x = 3;
    say $x;
}
outer();
say $x;
});
    like($output, qr/^3\n1/, 'nested local works correctly');
}

# Test: local with array
{
    my $cl = parse_pl('local @arr;');
    like($cl, qr/make-array 0/, 'local @arr creates empty array');
}

# ── s410: an `our` statement's TAIL must run ─────────────────────────────────
#
# perl declares the package cell unconditionally (a compile-time act) and runs
# the rest as an ordinary statement.  Two shapes used to lose that tail in
# SILENCE: a trailing statement modifier (v2 handed the modifier tokens to the
# expression parser, which dropped the whole statement; v1 discarded them), and
# a tail that is not an `=` assignment (`our $c++`, `our $V ||= 7` — Exporter's
# idiom), which v1's `=` scan never saw.  Both pipelines are exercised: the
# named sub stays v2-native, the anon sub containing a `local` routes its whole
# body through the v1 statement layer, which is where the second copy lived.
{
    my $output = run_pl(<<'PERL');
our $gl = 0;
sub v2native {
    our $c = 0;   our $c++;
    our $V ||= 7;
    our $d = 3;   our $d += 4;
    our $e = 1 if 1;
    our $z = 5 if 0;
    print "v2 $c $V $d $e ", (defined $z ? "BAD" : "undef"), "\n";
}
my $v1routed = sub {
    local $gl = 1;
    our $C = 0;   our $C++;
    our $W ||= 7;
    our $D = 3;   our $D += 4;
    our $E = 1 if 1;
    our @A = (1,2) if 1;
    print "v1 $C $W $D $E @A\n";
};
v2native();
$v1routed->();
# the shape this was found through: a comma expression whose right half is a
# `return`, guarded by a modifier, in a recursive closure (op/sub.t's
# [perl #122845]).  Losing the tail made it recurse forever.
our $depth = 0; our $ok = 0;
my $r; $r = sub { local $depth = $depth + 1;
                  our $ok++, return if $depth == 2;
                  $r->() };
$r->();
print "rec $ok\n";
PERL
    like($output, qr/^v2 1 7 7 1 undef$/m,
         'v2: our tail runs — ++, ||=, +=, and a modifier that is false');
    like($output, qr/^v1 1 7 7 1 1 2$/m,
         'v1-routed block: the same five shapes, second copy of the mechanism');
    like($output, qr/^rec 1$/m,
         '`our $ok++, return if COND` returns — the tail is not dropped');
}

done_testing();
