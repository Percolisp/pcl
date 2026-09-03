#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

#          -*-Mode: CPerl -*-

# Test statement-level code generation: for loops, print/say

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 41;
BEGIN { use_ok('Pl::Parser2') };


# Helper: parse code and return generated CL
sub parse_code {
    my $code = shift;
        return Pl::Parser2->parse_code($code);
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

# EITHER member of the foreach family — this row's subject is the lowering
# SHAPE `($x @array)`, not which arm binds the loop variable.  A read-only
# `my` loop var takes p-foreach-raw (task #862 ARM A), the same way a sole
# range takes p-foreach-range-raw and the range rows here already spell it.
# WHICH arm fires, and when it must not, is pinned in Pl/t/foreach-raw-01.t.
like(parse_code('for my $x (@array) { }'), qr/\(p-foreach(?:-raw)? \(\$x \@array\)/,
     'foreach with variable');

output_contains('foreach (@list) { print $_; }',
                '(p-foreach ($_ @list)',
                'foreach without variable uses $_');

output_contains('for $item (1, 2, 3) { }',
                '(p-foreach ($item',
                'foreach with list literal');


# ========================================
diag "";
diag "-------- C-style for loops:";

# v2 hoists the `my $i = 0` init into the loop binding (a raw slot) and
# spells the void `$i++` p-incf-raw — same init/condition/step semantics as
# v1's in-form (p-my-= …)/(p-post++ …) spellings.
output_contains('for (my $i = 0; $i < 10; $i++) { }',
                '(p-let (($i :scalar 0)) (p-for ()',
                'C-style for: init');

output_contains('for (my $i = 0; $i < 10; $i++) { }',
                '((p-< $i 10))',
                'C-style for: condition');

output_contains('for (my $i = 0; $i < 10; $i++) { }',
                '((p-incf-raw $i))',
                'C-style for: increment');

output_contains('for ($i = 0; $i <= $max; $i += 2) { }',
                '(p-for ((p-scalar-= $i 0))',
                'C-style for without my');

output_contains('for (;;) { last; }',
                '(p-for ()',
                'C-style infinite loop');


# ========================================
diag "";
diag "-------- print statements:";

output_contains('print "hello";',
                '(p-print "hello")',
                'print string');

output_contains('print $x;',
                '(p-print $x)',
                'print variable');

output_contains('print $x, $y;',
                '(p-print $x $y)',
                'print multiple args');

output_contains('print STDERR "error";',
                "(p-print :fh 'STDERR \"error\")",
                'print to bareword filehandle');

output_contains('print $fh "data";',
                '(p-print :fh $fh "data")',
                'print to variable filehandle');


# ========================================
diag "";
diag "-------- say statements:";

output_contains('say "hello";',
                '(p-say "hello")',
                'say string');

output_contains('say $x;',
                '(p-say $x)',
                'say variable');

output_contains('say STDOUT "output";',
                "(p-say :fh 'STDOUT \"output\")",
                'say to filehandle');


# ========================================
diag "";
diag "-------- Combined for + print:";

{
    my $result = parse_code('for my $x (@items) { print $x; }');
    like($result, qr/p-foreach/, 'foreach with print: loop');
    like($result, qr/\(p-print \$x\)/, 'foreach with print: print inside');
}

{
    my $result = parse_code('for (my $i = 0; $i < 10; $i++) { say $i; }');
    like($result, qr/p-for/, 'C-style for with say: loop');
    like($result, qr/\(p-say \$i\)/, 'C-style for with say: say inside');
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
    # v1 echoed a ;;; comment per package open/close; v2 emits only the
    # outermost pair.  The SEMANTIC facts the six rows guarded: each nested
    # package exists, the innermost sub lands in ITS package (perl nested
    # `package` statements do not nest names — `deep` is Inner::deep, not
    # Outer::Middle::Inner::deep), and control returns to main after.
    like($result, qr/;;; package Outer/, '3-level: Outer package');
    like($result, qr/\(p-defpackage :Middle\)/, '3-level: Middle package exists');
    like($result, qr/\(p-defpackage :Inner\)/, '3-level: Inner package exists');
    like($result, qr/\(p-sub Inner::pl-deep/, '3-level: deep lands in Inner (not name-nested)');
    like($result, qr/\(in-package :main\)/, '3-level: control returns to main after the block');
    like($result, qr/;;; back to package main/, '3-level: end of outermost package marked');
}


# ========================================
diag "";
diag "-------- Regression tests (session 3):";

# Regression: foreach with range operator
# Range was returning list but foreach expects vector.  v2 lowers a
# constant integer range to the raw counting loop (s286b) — no list is
# built at all, which subsumes the original claim.
output_contains('foreach my $i (0..5) { print $i; }',
                '(p-foreach-range-raw ($i 0 5)',
                'Regression: foreach with range operator');

# Regression: push with @array argument should flatten
# Was not flattening second array
output_contains('push @x, @y;',
                '(p-push @x (p-flatten @y))',
                'Regression: push @x, @y flattens @y');

# Regression: push with anonymous array should NOT flatten
output_contains('push @x, [1, 2, 3];',
                '(p-push @x (make-p-box (p-array-init 1 2 3)))',
                'Regression: push @x, [1,2,3] does not flatten');

# Regression: push with array deref should flatten
output_contains('push @x, @{$ref};',
                '(p-push @x (p-flatten (p-cast-@ $ref)))',
                'Regression: push @x, @{$ref} flattens deref');


# ========================================
diag "";
diag "-------- Regression tests (session 5):";

# Regression: for/foreach statement modifier should use p-foreach, not p-for
# "EXPR for LIST" is foreach, not C-style for
output_contains('push @foo, $_ for 1..3;',
                '(p-foreach ($_ (p-.. 1 3)) (p-push @foo $_))',
                'Regression: for statement modifier uses p-foreach');

# Regression: our %hash = (...) should generate p-hash-= with vector (not progn)
output_contains('our %h = (a => 1, b => 2);',
                '(p-hash-= %h (vector "a" 1 "b" 2))',
                'Regression: our %hash initialization uses p-hash');


# -------- continue blocks --------

# Regression: while loop with continue block
output_contains('while ($x) { $a = 1; } continue { $b = 2; }',
                ':continue',
                'while loop with continue generates :continue');

# Regression: redo LABEL generates p-redo with label argument
output_contains('redo OUTER;',
                '(p-redo OUTER)',
                'redo LABEL generates p-redo with label');

# Regression: bare block continue - PPI puts continue as sibling statement
# Parser must detect and consume the continue sibling
output_contains('{ next; } continue { $ok = 1; }',
                '(progn',
                'bare block continue from PPI sibling generates continue code');

# Regression: labeled bare block with continue - PPI keeps continue as child
output_contains('LABEL: { next LABEL; } continue { $ok = 1; }',
                "pcl::%pcl-loop-tag \"NEXT\" 'LABEL",
                'labeled bare block continue uses pcl:: qualified catch tag');

# Regression: labeled bare block with redo catch tag
output_contains('LABEL: { redo LABEL; }',
                "pcl::%pcl-loop-tag \"REDO\" 'LABEL",
                'labeled bare block has pcl:: qualified redo catch tag');

# Regression: bare block continue - trailing tokens after continue block
# PPI merges "$ok = 1;" into the continue statement
output_contains('{ next; } continue { $a = 1; } $ok = 1;',
                '(p-scalar-= $ok 1)',
                'trailing code after bare block continue is preserved');

# Regression: postfix-if with PPI::Structure::Condition (parenthesized condition)
# PPI wraps `if (COND)` as Structure::Condition, which must be unwrapped in Parser.pm
output_contains('return gcd($_[0] - $_[1]) if ($_[0] > $_[1]);',
                'p-if',
                'postfix-if with parenthesized condition generates p-if');

output_contains('return gcd($_[0] - $_[1]) if ($_[0] > $_[1]);',
                'p-return',
                'postfix-if with return generates p-return in body');

done_testing();
