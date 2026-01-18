#!/usr/bin/env perl

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser;

# Helper to parse and return CL output
sub parse_pl {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    return $parser->parse();
}

# Helper to run transpiled code
sub run_pl {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    my $cl_code = $parser->parse();

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

plan tests => 27;

say "# -------- 'our' Transpilation Tests:";

# Test: our $x = value
# Declaration (compile-time) uses nil, initialization (runtime) sets value
{
    my $cl = parse_pl('our $count = 0;');
    like($cl, qr/defvar \$count/, 'our $x = val generates defvar');
    like($cl, qr/setf.*pl-box-value.*\$count.*0/s, 'our with value generates setf for init');
}

# Test: bare our $x
{
    my $cl = parse_pl('our $flag;');
    like($cl, qr/defvar \$flag/, 'bare our generates defvar');
    like($cl, qr/make-pl-box nil/, 'bare our initializes to nil');
}

# Test: our with list
{
    my $cl = parse_pl('our ($x, $y) = (1, 2);');
    like($cl, qr/defvar \$x/, 'our list declares first var');
    like($cl, qr/defvar \$y/, 'our list declares second var');
}

# Test: our in package
{
    my $cl = parse_pl('package Counter; our $count = 0;');
    like($cl, qr/in-package :Counter/, 'package declaration emitted');
    like($cl, qr/defvar \$count/, 'our in package generates defvar');
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
    like($cl, qr/defvar \$setting/, 'our in nested package works');
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

# Test: local $x = value generates let
{
    my $cl = parse_pl('local $x = 20;');
    like($cl, qr/\(let \(\(\$x/, 'local generates let binding');
    like($cl, qr/make-pl-box\s+20/, 'local wraps value in make-pl-box');
}

# Test: bare local $x generates let with nil
{
    my $cl = parse_pl('local $x;');
    like($cl, qr/\(let \(\(\$x/, 'bare local generates let');
    like($cl, qr/make-pl-box nil/, 'bare local initializes to nil');
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

done_testing();
