#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# A leading run of SCALAR-DEREF casts binds WITH the arrow's target (#1620):
# `$$r->who` is `${$r}->who`, never `${ $r->who }`.  perl's sigil deref is
# part of the TERM, so `->` cannot get between them.
#
# Two arrow arms already had this reading privately — #305 for `$$r->(…)` and
# #211 for `$$r->{k}` / `$$r->[0]` (their rows are in transpile-test-10.t) —
# and the METHOD arms and the POSTFIX-DEREF arms did not, so `$$r->who` was
# parsed as a scalar deref OF THE METHOD CALL and died "Can't call method who
# on unblessed reference", and `$$ar->$#*` answered -1.  `Pl::PExpr::
# _arrow_invocant` is now the one reading every arrow arm asks.
#
# Every row's expectation is real perl's, compared row for row (test_transpile
# runs the same snippet under perl and under PCL).  The third row is the
# must-NOT-change side: `\` is an OPERATOR, not a cast, so `\$x->m` is
# `\($x->m)`; without an arrow the OUTERMOST cast picks the access kind
# (`$$r{k}` stays one level); and bare `$$` is still the PID.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 6;

sub run_perl {
    my ($code) = @_;
    (my $sh_code = $code) =~ s/'/'\\''/g;
    return `perl -e '$sh_code' 2>&1`;
}

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return PCLCore::transpile(qq{$pl2cl $pl_file});
}

my $pkg = '
package H;
sub new  { my $c = shift; return bless { k => "Kv", n => 7 }, $c }
sub who  { return "H::who" }
sub arg  { my ($s, $a) = @_; return "H::arg($a)" }
sub self { my $s = shift; return $s }
package main;
';

# ── the METHOD arms: name, name+args, qualified name, $variable name,
#    $variable name + args, computed ${\ …} name, a chain past the cast, and
#    two and three cast levels. ───────────────────────────────────────────────
test_transpile('#1620: every method spelling derefs the cast run FIRST', $pkg . '
sub who { return "main::who_sub" }
my $h = H->new; my $r = \$h; my $rr = \$r; my $m = "arg";
print "a=", $$r->who, "\n";
print "b=", $$r->arg(3), "\n";
print "c=", $$$rr->who, "\n";
print "d=", ${$r}->who, "\n";
print "e=", $$r->$m(4), "\n";
print "f=", $$r->${\ "who"}(), "\n";
print "g=", $$r->H::who, "\n";
print "h=", $$r->self->who, "\n";
print "i=", ($$r->can("who") ? "yes" : "no"), "\n";
print "j=", length($$r->who) + 1, "\n";
print "k=", who(), "\n";
');

# ── the POSTFIX-DEREF arms (perl 5.20+): ->@*, ->$#*, ->%*, ->@[…] — the
#    `$#*` one answered -1 on the old reading, a SILENT wrong. ───────────────
test_transpile('#1620: the postfix-deref arms too (->@*, ->$#*, ->%*, ->@[…])', '
my @a = (10, 11); my $ar = \@a; my $arr = \$ar;
my %h = (k => "Kv"); my $hr = \%h; my $hrr = \$hr;
print "a=", scalar($$arr->@*), "\n";
print "b=", $$arr->$#*, "\n";
print "c=", join(",", $$arr->@[0,1]), "\n";
my %c = $$hrr->%*;
print "d=", $c{k}, "\n";
print "e=", $$arr->[1], "\n";
');

# ── the must-NOT-change side: EVERY conjunct here answers the same on the
#    pre-fix tree (measured), so a regression in the other direction fails
#    this row alone.  `\` is an operator (a LOWER-precedence one), so it
#    stays outside the run; with no arrow the OUTERMOST cast is the access
#    kind (`$$hr{k}` is one level, `@$$arr[0,1]` a slice of the inner ref);
#    bare `$$` is the PID; and a non-`$` cast is not a deref level to fold.
#    (The "a method name that is also a sub in scope is still a method"
#    conjunct is row 1's `a=`, where `sub who` shadows nothing.) ────────────
test_transpile('#1620 inverse: \\ stays outside, no-arrow keeps its level, $$ is the PID', $pkg . '
sub who { return "main::who_sub" }
my $h = H->new; my $r = \$h;
my %hh = (a=>1, b=>2, k=>"Kv"); my $hr = \%hh; my $hrr = \$hr;
my @aa = (7, 8, 9); my $ar = \@aa; my $arr = \$ar;
print "a=", ref(\$h->self), "\n";
print "b=", ref(\ $$r), "\n";
print "c=", $$hr{k}, "\n";
print "d=", $$ar[1], "\n";
print "e=", join(",", @$$arr[0,1]), "\n";
print "f=", join(",", @$$hrr{qw(a b)}), "\n";
print "g=", ($$ > 0 ? "pid" : "bad"), "\n";
print "h=", who(), "\n";
print "i=", scalar(@$ar), "\n";
print "j=", $#{$ar}, "\n";
');

# ── the cast run reaches the arrow arms through every enclosing context the
#    term walker uses (a map block, a string concat, a hash key, a list). ────
test_transpile('#1620: the reading holds in every enclosing context', $pkg . '
my $h = H->new; my $r = \$h;
my $chain = [ { k => H->new } ]; my $cr = \$chain;
print "a=", join(",", map { $$r->who } (1,2)), "\n";
print "b=", "x" . $$r->who, "\n";
my %z = ($$r->who => 1);
print "c=", join(",", keys %z), "\n";
print "d=", join("|", ($$r->who, 2)), "\n";
print "e=", $$cr->[0]{k}->who, "\n";
print "f=", $$cr->[0]->{k}->who, "\n";
print "g=", (1 ? $$r->who : "no"), "\n";
');

# ── and as EMITTED: the cast belongs to the invocant, not to the call. ──────
{
    my $cl = transpile('my $h = bless {}, "H"; my $r = \$h; print $$r->who;');
    like($cl, qr/\(p-method-call \(p-cast-\$ \$r\) "who"\)/,
         '#1620: the cast run is emitted INSIDE the invocant');
    unlike($cl, qr/\(p-cast-\$ \([^)]*p-method-call \$r /,
         '#1620: … and never wraps the method call');
}
