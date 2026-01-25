#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test statement-level code generation: for loops, print/say

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 31;
BEGIN { use_ok('Pl::Parser') };


# Helper: parse code and return generated CL
sub parse_code {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    return $parser->parse;
}


# Helper: check if output contains expected string
sub output_contains {
    my $code     = shift;
    my $expected = shift;
    my $desc     = shift // "contains: $expected";

    my $result = parse_code($code);
    like($result, qr/\Q$expected\E/, $desc);
}


# ========================================
diag "";
diag "-------- foreach loops:";

output_contains('for my $x (@array) { }',
                '(pl-foreach ($x @array)',
                'foreach with variable');

output_contains('foreach (@list) { print $_; }',
                '(pl-foreach ($_ @list)',
                'foreach without variable uses $_');

output_contains('for $item (1, 2, 3) { }',
                '(pl-foreach ($item',
                'foreach with list literal');


# ========================================
diag "";
diag "-------- C-style for loops:";

output_contains('for (my $i = 0; $i < 10; $i++) { }',
                '(pl-for ((pl-setf $i 0))',
                'C-style for: init');

output_contains('for (my $i = 0; $i < 10; $i++) { }',
                '((pl-< $i 10))',
                'C-style for: condition');

output_contains('for (my $i = 0; $i < 10; $i++) { }',
                '((pl-post++ $i))',
                'C-style for: increment');

output_contains('for ($i = 0; $i <= $max; $i += 2) { }',
                '(pl-for ((pl-setf $i 0))',
                'C-style for without my');

output_contains('for (;;) { last; }',
                '(pl-for ()',
                'C-style infinite loop');


# ========================================
diag "";
diag "-------- print statements:";

output_contains('print "hello";',
                '(pl-print "hello")',
                'print string');

output_contains('print $x;',
                '(pl-print $x)',
                'print variable');

output_contains('print $x, $y;',
                '(pl-print $x $y)',
                'print multiple args');

output_contains('print STDERR "error";',
                "(pl-print :fh 'STDERR \"error\")",
                'print to bareword filehandle');

output_contains('print $fh "data";',
                '(pl-print :fh $fh "data")',
                'print to variable filehandle');


# ========================================
diag "";
diag "-------- say statements:";

output_contains('say "hello";',
                '(pl-say "hello")',
                'say string');

output_contains('say $x;',
                '(pl-say $x)',
                'say variable');

output_contains('say STDOUT "output";',
                "(pl-say :fh 'STDOUT \"output\")",
                'say to filehandle');


# ========================================
diag "";
diag "-------- Combined for + print:";

{
    my $result = parse_code('for my $x (@items) { print $x; }');
    like($result, qr/pl-foreach/, 'foreach with print: loop');
    like($result, qr/\(pl-print \$x\)/, 'foreach with print: print inside');
}

{
    my $result = parse_code('for (my $i = 0; $i < 10; $i++) { say $i; }');
    like($result, qr/pl-for/, 'C-style for with say: loop');
    like($result, qr/\(pl-say \$i\)/, 'C-style for with say: say inside');
}


# ========================================
diag "";
diag "-------- Package nesting (3 levels):";

{
    my $code = q{
package Outer {
    package Middle {
        package Inner {
            sub deep { 1 }
        }
    }
}
};
    my $result = parse_code($code);
    like($result, qr/;;; package Outer/, '3-level: Outer package');
    like($result, qr/;;; package Middle/, '3-level: Middle package');
    like($result, qr/;;; package Inner/, '3-level: Inner package');
    like($result, qr/;;; end package Inner/, '3-level: end Inner');
    like($result, qr/;;; end package Middle/, '3-level: end Middle');
    like($result, qr/;;; end package Outer/, '3-level: end Outer');
}


# ========================================
diag "";
diag "-------- Regression tests (session 3):";

# Regression: foreach with range operator
# Range was returning list but foreach expects vector
output_contains('foreach my $i (0..5) { print $i; }',
                '(pl-foreach ($i (pl-.. 0 5))',
                'Regression: foreach with range operator');

# Regression: push with @array argument should flatten
# Was not flattening second array
output_contains('push @x, @y;',
                '(pl-push @x (pl-flatten @y))',
                'Regression: push @x, @y flattens @y');

# Regression: push with anonymous array should NOT flatten
output_contains('push @x, [1, 2, 3];',
                '(pl-push @x (pl-array-init 1 2 3))',
                'Regression: push @x, [1,2,3] does not flatten');

# Regression: push with array deref should flatten
output_contains('push @x, @{$ref};',
                '(pl-push @x (pl-flatten (pl-cast-@ $ref)))',
                'Regression: push @x, @{$ref} flattens deref');


done_testing();
