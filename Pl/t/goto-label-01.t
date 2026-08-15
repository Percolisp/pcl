#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Intra-sub `goto LABEL` (forward error gotos and backward retry gotos), plus
# `goto LABEL` nested inside an if/while at the top level.  CL `go` needs a
# lexically-enclosing tagbody; the parser wraps the minimal run of complete
# forms that span each label and its reachable goto in a (tagbody …).

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use lib "$RealBin/../..";

use Pl::Parser2;

sub run_perl {
    my ($code) = @_;
    my $result = `perl -e '$code' 2>&1`;
    chomp $result;
    return $result;
}

sub run_cl {
    my ($lisp_code) = @_;
    my $lisp_file = "/tmp/pcl-goto-$$.lisp";
    open my $fh, '>', $lisp_file or die;
    print $fh $lisp_code;
    close $fh;
    my $result = `sbcl --noinform --non-interactive --load "$RealBin/../../cl/pcl-runtime.lisp" --load "$lisp_file" 2>&1`;
    unlink $lisp_file;
    my @output;
    for my $line (split /\n/, $result) {
        next if $line =~ /^;|caught WARNING|undefined variable|compilation unit|file:/;
        next if $line =~ /^\s*$/;
        next if $line eq 'PCL Runtime loaded';
        push @output, $line;
    }
    return join("\n", @output);
}

sub test_transpile {
    my ($name, $code) = @_;
    # #255: through the production v2 pipeline (v1's file-level entry is
    # being retired) — same behavioural assertion, real compiler.
    my $cl = Pl::Parser2->parse_code($code);
    is(run_cl($cl), run_perl($code), $name);
}

plan tests => 9;

# Forward error-goto inside a sub, jumped to from inside an if branch.
test_transpile('forward goto to error label inside sub',
    'sub f { my $x = shift; if ($x < 0) { goto FAIL; } print "ok:$x\n"; return; FAIL: print "fail\n"; }'
  . ' f(5); f(-1);');

# Backward goto (retry loop) inside a sub.
test_transpile('backward goto retry loop',
    'sub retry { my $n = 0; AGAIN: $n++; goto AGAIN if $n < 3; return $n; } print retry();');

# Implicit return of the last expression after a label (value preserved because
# the post-label form stays outside the tagbody).
test_transpile('implicit return after label',
    'sub g { my $x = shift; goto SKIP if $x; $x = 99; SKIP: "r=$x"; } print g(0), "|", g(1);');

# Two labels with explicit returns.
test_transpile('two labels, explicit returns',
    'sub h { my $x = shift; goto A if $x == 1; goto B if $x == 2; return "none"; A: return "A"; B: return "B"; }'
  . ' print h(1), h(2), h(3);');

# goto from inside a while loop to a sub-body label.
test_transpile('goto out of while loop',
    'sub m2 { my $s = shift; my $i = 0; while ($i < length($s)) { if (substr($s,$i,1) eq "X") { goto HIT; } $i++; } return "none"; HIT: return "hit:$i"; }'
  . ' print m2("abXc"), "|", m2("abc");');

# Top-level goto nested inside an if (the goto is inside a multi-line form).
test_transpile('top-level goto inside if',
    'my $x = -1; if ($x < 0) { goto FAIL; } print "ok\n"; FAIL: print "fail\n";');

# A `use` pragma inside the block that contains the goto must not break wrapping.
test_transpile('goto with use pragma in same block',
    'sub u { my $x = shift; unless ($x) { use warnings; goto FAIL; } return "ok"; FAIL: return "fail"; }'
  . ' print u(1), "|", u(0);');

# ── #63 (s295b) top-level forward/backward gotos.  (Until #255 this file
# drove Pl::Parser (v1) directly; it now runs the v2 pipeline like
# transpile-test-01b.t, which guards the map/grep-lambda catch-wrap shapes.)

# Plain top-level forward goto (no lambda).
test_transpile('top-level forward goto skips statements',
    'my $x = 1; goto SKIP; $x = 99; SKIP: print "x=$x\n";');

# Backward goto through the same label machinery still re-executes.
test_transpile('backward goto at top level still lexical',
    'my $n = 0; AGAIN: $n++; goto AGAIN if $n < 3; print "n=$n\n";');
