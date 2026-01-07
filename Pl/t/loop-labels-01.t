#!/usr/bin/env perl
# Test loop labels: LABEL: while/for/foreach, next LABEL, last LABEL

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use lib "$RealBin/../..";

use Pl::Parser;

# Helper to run Perl and CL code and compare
sub run_perl {
    my ($code) = @_;
    my $full = 'use feature "state"; ' . $code;
    my $result = `perl -e '$full' 2>&1`;
    chomp $result;
    return $result;
}

sub run_cl {
    my ($lisp_code) = @_;
    my $lisp_file = "/tmp/pcl-test-$$.lisp";
    open my $fh, '>', $lisp_file or die;
    print $fh $lisp_code;
    close $fh;

    my $result = `sbcl --noinform --non-interactive --load "$RealBin/../../cl/pcl-runtime.lisp" --load "$lisp_file" 2>&1`;
    unlink $lisp_file;

    # Extract output (skip compiler warnings)
    my @lines = split /\n/, $result;
    my @output;
    for my $line (@lines) {
        next if $line =~ /^;|caught WARNING|undefined variable|compilation unit|file:/;
        next if $line =~ /^\s*$/;
        next if $line eq 'PCL Runtime loaded';
        push @output, $line;
    }
    return join("\n", @output);
}

sub test_transpile {
    my ($name, $code) = @_;
    my $parser = Pl::Parser->new(code => $code);
    my @cl = $parser->parse();
    my $cl_code = join("\n", @cl);

    my $perl_result = run_perl($code);
    my $cl_result = run_cl($cl_code);

    is($cl_result, $perl_result, $name);
}

plan tests => 18;

diag "-------- next LABEL:";

test_transpile('next LABEL - skip to outer loop',
    'my $r = ""; OUTER: for my $i (1, 2, 3) { for my $j (1, 2, 3) { if ($j == 2) { next OUTER; } $r .= "$i:$j "; } } print $r;');

test_transpile('next LABEL - while loop',
    'my $r = ""; my $i = 0; OUTER: while ($i < 3) { $i++; my $j = 0; while ($j < 3) { $j++; if ($j == 2) { next OUTER; } $r .= "$i:$j "; } } print $r;');

diag "-------- last LABEL:";

test_transpile('last LABEL - exit outer loop',
    'my $r = ""; OUTER: for my $i (1, 2, 3) { for my $j (1, 2, 3) { if ($i == 2 && $j == 2) { last OUTER; } $r .= "$i:$j "; } } print $r;');

test_transpile('last LABEL - while loop',
    'my $r = ""; my $i = 0; OUTER: while ($i < 3) { $i++; my $j = 0; while ($j < 3) { $j++; if ($i == 2 && $j == 2) { last OUTER; } $r .= "$i:$j "; } } print $r;');

diag "-------- Labeled C-style for:";

test_transpile('next LABEL - C-style for',
    'my $r = ""; OUTER: for (my $i = 1; $i <= 3; $i++) { for (my $j = 1; $j <= 3; $j++) { if ($j == 2) { next OUTER; } $r .= "$i:$j "; } } print $r;');

test_transpile('last LABEL - C-style for',
    'my $r = ""; OUTER: for (my $i = 1; $i <= 3; $i++) { for (my $j = 1; $j <= 3; $j++) { if ($i == 2 && $j == 2) { last OUTER; } $r .= "$i:$j "; } } print $r;');

diag "-------- Unlabeled still works:";

test_transpile('unlabeled next still works',
    'my $r = ""; for my $i (1, 2, 3) { if ($i == 2) { next; } $r .= "$i "; } print $r;');

test_transpile('unlabeled last still works',
    'my $r = ""; for my $i (1, 2, 3, 4, 5) { if ($i == 3) { last; } $r .= "$i "; } print $r;');

diag "-------- Triple-nested loops:";

test_transpile('triple nested - next to outermost',
    'my $r = "";
     OUTER: for my $i (1, 2) {
       MIDDLE: for my $j (1, 2) {
         for my $k (1, 2, 3) {
           if ($k == 2) { next OUTER; }
           $r .= "$i:$j:$k ";
         }
       }
     }
     print $r;');

test_transpile('triple nested - next to middle',
    'my $r = "";
     OUTER: for my $i (1, 2) {
       MIDDLE: for my $j (1, 2) {
         for my $k (1, 2, 3) {
           if ($k == 2) { next MIDDLE; }
           $r .= "$i:$j:$k ";
         }
       }
     }
     print $r;');

test_transpile('triple nested - last to outermost',
    'my $r = "";
     OUTER: for my $i (1, 2, 3) {
       for my $j (1, 2, 3) {
         for my $k (1, 2, 3) {
           if ($i == 2 && $j == 2 && $k == 2) { last OUTER; }
           $r .= "$i$j$k ";
         }
       }
     }
     print $r;');

diag "-------- Mixed loop types:";

test_transpile('foreach outer, while inner - next OUTER',
    'my $r = "";
     OUTER: for my $i (1, 2, 3) {
       my $j = 0;
       while ($j < 3) {
         $j++;
         if ($j == 2) { next OUTER; }
         $r .= "$i:$j ";
       }
     }
     print $r;');

test_transpile('while outer, foreach inner - last OUTER',
    'my $r = "";
     my $i = 0;
     OUTER: while ($i < 3) {
       $i++;
       for my $j (1, 2, 3) {
         if ($i == 2 && $j == 2) { last OUTER; }
         $r .= "$i:$j ";
       }
     }
     print $r;');

diag "-------- Multiple labels in same code:";

test_transpile('two labeled loops - use both',
    'my $r = "";
     FIRST: for my $i (1, 2) {
       for my $j (1, 2, 3) {
         if ($j == 2) { next FIRST; }
         $r .= "A$i:$j ";
       }
     }
     SECOND: for my $x (1, 2) {
       for my $y (1, 2, 3) {
         if ($y == 3) { last SECOND; }
         $r .= "B$x:$y ";
       }
     }
     print $r;');

diag "-------- Labeled and unlabeled in same nesting:";

test_transpile('labeled outer, unlabeled inner controls',
    'my $r = "";
     OUTER: for my $i (1, 2, 3) {
       for my $j (1, 2, 3, 4) {
         if ($j == 2) { next; }      # skip j=2 only
         if ($j == 4) { next OUTER; } # skip rest of inner, go to next i
         $r .= "$i:$j ";
       }
     }
     print $r;');

test_transpile('conditional label choice',
    'my $r = "";
     OUTER: for my $i (1, 2, 3) {
       INNER: for my $j (1, 2, 3) {
         if ($i == 1 && $j == 2) { next INNER; }
         if ($i == 2 && $j == 2) { next OUTER; }
         if ($i == 3 && $j == 2) { last OUTER; }
         $r .= "$i:$j ";
       }
     }
     print $r;');

diag "-------- Edge cases:";

test_transpile('label on single iteration loop',
    'my $r = "";
     LOOP: for my $i (42) {
       $r .= "before ";
       last LOOP;
       $r .= "after ";
     }
     print $r;');

test_transpile('label on two-element list',
    'my $r = "";
     LOOP: for my $i (42, 17) {
       $r .= "$i ";
       if ($i == 17) { last LOOP; }
     }
     print $r;');

done_testing();
