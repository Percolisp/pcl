#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test eval { } block handling and $@ error variable

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 34;
use File::Temp qw(tempfile);
BEGIN { use_ok('Pl::Parser') };
BEGIN { use_ok('Pl::Environment') };


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


# Helper: check if output matches a regex
sub output_matches {
    my $code     = shift;
    my $pattern  = shift;
    my $desc     = shift // "matches pattern";

    my $result = parse_code($code);
    like($result, $pattern, $desc);
}


# ========================================
diag "";
diag "-------- Basic eval { } block:";

output_contains('eval { 1 };',
                'p-eval-block',
                'eval block generates p-eval-block');

output_contains('eval { die "oops" };',
                'p-eval-block',
                'eval with die generates p-eval-block');

output_contains('eval { die "oops" };',
                'p-die',
                'die inside eval block');


# ========================================
diag "";
diag "-------- $@ error variable:";

output_contains('print $@;',
                '(p-print $@)',
                '$@ used as variable');

output_contains('if ($@) { print "error" }',
                '$@',
                '$@ in condition');


# ========================================
diag "";
diag "-------- eval { } return value:";

output_contains('my $result = eval { 42 };',
                'p-eval-block',
                'eval block in assignment');


# ========================================
diag "";
diag "-------- eval { } with multiple statements:";

# Multi-statement blocks are inlined directly in p-eval-block (no funcall)
output_matches('eval { my $x = 1; $x + 1 };',
               qr/p-eval-block/s,
               'eval with multiple statements generates p-eval-block');


# ========================================
diag "";
diag "-------- Nested eval blocks:";

output_matches('eval { eval { die "inner" }; print $@ };',
               qr/p-eval-block.*p-eval-block/s,
               'nested eval blocks');


# ========================================
diag "";
diag "-------- eval with exception objects:";

output_contains('eval { die $exception };',
                '(p-die $exception)',
                'die with variable exception object');


# ========================================
diag "";
diag "-------- eval combined with control flow:";

output_matches('if (eval { dangerous() }) { ok() }',
               qr/p-if.*p-eval-block/s,
               'eval in condition');


# ========================================
diag "";
diag "-------- Runtime: eval STRING:";

my $runtime = "cl/pcl-runtime.lisp";
my $pl2cl   = "./pl2cl";

# Helper: write Perl to temp file, transpile, run under SBCL, return output
sub run_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/PCL Runtime loaded\n?//g;
    $out =~ s/^\s*\n//gm;
    return $out;
}

SKIP: {
    skip "pl2cl not found", 22 unless -x $pl2cl;
    skip "sbcl not found",  22 unless `which sbcl 2>/dev/null`;

    # Test 1: basic arithmetic
    {
        my $out = run_pl('my $r = eval "1 + 2"; print $r, "\n";');
        like($out, qr/^3/, 'eval "string": basic arithmetic returns 3');
    }

    # Test 2: string result
    {
        my $out = run_pl('my $r = eval q{"hello"}; print $r, "\n";');
        like($out, qr/^hello/, 'eval "string": string literal result');
    }

    # Test 3: $@ is empty string on success
    {
        my $out = run_pl('eval "1"; print length($@), "\n";');
        like($out, qr/^0/, 'eval "string": $@ has length 0 on success');
    }

    # Test 4: die inside eval sets $@
    {
        my $out = run_pl('eval "die \"boom\""; print $@ =~ /boom/ ? "caught" : "missed", "\n";');
        like($out, qr/^caught/, 'eval "string": die sets $@');
    }

    # Test 5: eval returns undef on die
    {
        my $out = run_pl('my $r = eval "die \"err\""; print defined($r) ? "defined" : "undef", "\n";');
        like($out, qr/^undef/, 'eval "string": returns undef on die');
    }

    # Test 6: eval undef — returns undef, $@ = ""
    {
        my $out = run_pl('my $r = eval undef; print defined($r) ? "defined" : "undef", "\n"; print length($@), "\n";');
        like($out, qr/undef.*0/s, 'eval undef: returns undef, $@ is ""');
    }

    # Test 7: multi-statement, return value is last expression
    {
        my $out = run_pl('my $r = eval "my \$x = 10; \$x * 3"; print $r, "\n";');
        like($out, qr/^30/, 'eval "string": multi-statement returns last value');
    }

    # Test 8: package (our) variable visible in eval
    {
        my $out = run_pl('our $n = 5; my $r = eval "\$n + 1"; print $r, "\n";');
        like($out, qr/^6/, 'eval "string": package variable visible in eval');
    }

    # Test 9: multiple evals in same program — each independent, $@ cleared
    {
        my $out = run_pl(
            'my $a = eval "2 * 3"; my $b = eval "10 - 1"; print $a, " ", $b, "\n";'
        );
        like($out, qr/^6 9/, 'multiple evals in same program work independently');
    }

    # Test 10: second eval after a failing one — $@ is cleared on success
    {
        my $out = run_pl(
            'eval "die \"first\""; my $r = eval "42"; print $r, " ", length($@), "\n";'
        );
        like($out, qr/^42 0/, '$@ cleared by successful eval after failing one');
    }

    # Test 11: eval calling a sub defined in outer code
    {
        my $out = run_pl(
            'sub double { my $x = shift; $x * 2 }' . "\n" .
            'my $r = eval "double(7)"; print $r, "\n";'
        );
        like($out, qr/^14/, 'eval "string": can call sub defined in outer code');
    }

    # Test 12: eval with string variable (not a literal)
    {
        my $out = run_pl(
            'my $code = "3 + 4"; my $r = eval $code; print $r, "\n";'
        );
        like($out, qr/^7/, 'eval $var: eval of string variable');
    }

    # Test 13: cache hit — same code evaluated twice gives same result
    {
        my $out = run_pl(
            'my $a = eval "100"; my $b = eval "100"; print $a == $b ? "same" : "diff", "\n";'
        );
        like($out, qr/^same/, 'eval cache: identical string evaluated twice');
    }

    # Test 14: eval with nested eval block (string eval containing block eval)
    {
        my $out = run_pl(
            'my $r = eval "eval { 5 * 5 }"; print $r, "\n";'
        );
        like($out, qr/^25/, 'nested: eval "string" containing eval { block }');
    }

    # Test 15: object exception propagates correctly through eval
    {
        my $out = run_pl(
            'eval "die bless { msg => \"objdie\" }, \"MyErr\"";' . "\n" .
            'print ref($@), "\n";'
        );
        like($out, qr/^MyErr/, 'eval "string": object die preserved in $@');
    }

    # Test 16: eval inside a sub
    {
        my $out = run_pl(
            'sub compute { eval "2 ** 8" }' . "\n" .
            'print compute(), "\n";'
        );
        like($out, qr/^256/, 'eval "string" inside a sub');
    }

    # Test 17: eval with array result
    {
        my $out = run_pl(
            'my @a = (eval "1 + 1", eval "2 + 2"); print "@a\n";'
        );
        like($out, qr/^2 4/, 'eval "string": multiple evals in list context');
    }

    # ========================================
    diag "";
    diag "-------- Persistent transpiler server:";

    # Test 18: 50 distinct eval strings in a loop — each is a cache miss,
    # exercising the persistent server directly.  With per-process spawning
    # (old behaviour) 50 * ~500ms = 25s → would timeout; with the persistent
    # server 50 * ~5ms = ~250ms.
    {
        my $out = run_pl(q{
            my $sum = 0;
            for my $n (1..50) {
                $sum += eval "$n";
            }
            print $sum, "\n";
        });
        like($out, qr/^1275/, 'persistent server: 50 distinct evals sum to 1275');
    }

    # Test 19: eval that defines a sub — sub becomes callable in current package
    {
        my $out = run_pl(q{
            eval 'sub runtime_greet { "Hello, " . shift }';
            print runtime_greet("world"), "\n";
        });
        like($out, qr/^Hello, world/, 'eval "sub ...": defines callable sub');
    }

    # Test 20: eval can assign to a package (our) variable
    {
        my $out = run_pl(q{
            our $counter = 10;
            eval '$counter = $counter * 3';
            print $counter, "\n";
        });
        like($out, qr/^30/, 'eval "string": assigns to our variable');
    }

    # Test 21: eval inside map — each call independent, returns list of results
    {
        my $out = run_pl(q{
            my @sq = map { eval "$_ ** 2" } 1..5;
            print join(" ", @sq), "\n";
        });
        like($out, qr/^1 4 9 16 25/, 'eval in map: squares 1..5');
    }

    # Test 22: repeated eval of same string — cache returns consistent results
    {
        my $out = run_pl(q{
            my @r;
            push @r, eval "2 ** 10" for 1..5;
            print join(",", @r), "\n";
        });
        like($out, qr/^1024,1024,1024,1024,1024/,
             'cache: repeated identical eval returns same value each time');
    }
}
