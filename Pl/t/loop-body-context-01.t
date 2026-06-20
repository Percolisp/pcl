#!/usr/bin/env perl
# loop-body-context-01.t — statements inside a loop body run in VOID context,
# not the enclosing sub's wantarray.
#
# Bug (session 262, found via Text::Balanced::_match_bracketed): when a sub is
# called in LIST context, statement-level expressions in a while/for/foreach body
# wrongly inherited the sub's list wantarray.  A bare `m//g` match in the loop
# body then ran in list context (match-all) instead of void/scalar (single match,
# advance pos), so a `while (pos < len) { ... m/\G.../gc ... }` tokenizer never
# advanced pos() and hung.  Loop bodies are never in value/tail position; their
# statements must be void.  See _process_block $void_body + _generate_if_clauses
# $void threading in Pl/Parser.pm.

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser;

sub run_pl {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    my $cl_code = $parser->parse();
    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;
    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s+//;
    return $output;
}

plan tests => 5;

# 1. The core bug: a \G/gc tokenizer loop in a sub called in LIST context must
#    terminate (each m//g in the void loop body advances pos by one token).
is(run_pl(<<'PL'), "abc:3\n", 'while m/\G/gc loop in list-context sub terminates');
sub toks {
    my $s = shift;
    my @t;
    my $n = 0;
    while (pos($s) < length($s)) {
        last if ++$n > 1000;
        if ($s =~ m/\G(\w)/gc) { push @t, $1; }
        else { $s =~ m/\G./gcs; }
    }
    return ($t[0].$t[1].$t[2], $n);
}
my @r = toks("abc");
print "$r[0]:$r[1]\n";
PL

# 2. Same hazard via an if/elsif/ELSE chain in the loop body (the else branch's
#    tail statement must also be void).
is(run_pl(<<'PL'), "3\n", 'm//g in an else-branch inside a list-context loop is void');
sub count_digits {
    my $s = shift;
    my $c = 0;
    my $guard = 0;
    while (pos($s) < length($s)) {
        last if ++$guard > 1000;
        if    ($s =~ m/\G(\d)/gc) { $c++; }
        elsif ($s =~ m/\G\s/gc)   { }
        else  { $s =~ m/\G./gcs; }
    }
    return $c;
}
my @r = (count_digits("a1 b2 c3"));
print "$r[0]\n";
PL

# 3. A plain scalar /g match used as a loop-body statement (void) advances pos
#    one match at a time, even when the sub is in list context.
is(run_pl(<<'PL'), "3\n", 'scalar /g statement in list-context loop advances singly');
sub nmatch {
    my $s = shift;
    my $c = 0;
    while ($s =~ /\d/g) { $c++; last if $c > 100; }
    return $c;
}
my @r = nmatch("a1b2c3");
print "$r[0]\n";
PL

# 4. Regression guard: tail context still propagates — a sub whose last statement
#    is a bare list returns a list in list context.
is(run_pl(<<'PL'), "1 2 3\n", 'tail list still propagates to list context');
sub lst { my @a = (1,2,3); @a }
my @r = lst();
print "@r\n";
PL

# 5. Regression guard: if/else as the sub's tail still propagates wantarray.
is(run_pl(<<'PL'), "L|S\n", 'tail if/else still propagates wantarray');
sub pick { if (1) { wantarray ? "L" : "S" } else { "x" } }
my @l = pick();
my $s = pick();
print "$l[0]|$s\n";
PL
