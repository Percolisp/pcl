#!/usr/bin/env perl

# eval-named-sub-01.t — a NAMED sub defined inside eval "STRING".
#
# Two facets (see docs/method-modifiers-plan.md + docs/eval-free-vars-plan.md):
#   FIXED  (Fix 1, p-eval form-by-form): `package X;` inside the eval routes the
#          named sub to X (it used to land in main).  Locked here.
#   TODO   (eval free-var capture via BlockAnalyzer): a named sub defined in the
#          eval that references an enclosing lexical must close over it.  Not yet
#          implemented — marked TODO so these run, document the gap, and flip
#          (with a harness warning) once the free-vars plan lands.
#
# Differential vs real perl.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);

my $pl2cl   = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_pcl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code; close $fh;
    my $cl_code = `$pl2cl --no-cache $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code; close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    unlink $cl_file;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    chomp $output;
    return $output;
}

sub run_perl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code; close $fh;
    my $out = `perl $pl_file 2>&1`;
    chomp $out;
    return $out;
}

sub both_agree {
    my ($name, $code) = @_;
    my $p = run_perl($code);
    my $c = run_pcl($code);
    is($c, $p, $name) or diag "perl=[$p] pcl=[$c]";
}

# ---- FIXED: in-package routing of a named sub defined in eval (Fix 1) ----

both_agree('named sub in eval installs in the eval-selected package', <<'PL');
use v5.30;
eval "package Target; sub greet { return 42 }";
print Target::greet(), "\n";
PL

both_agree('named sub in eval does NOT leak into main', <<'PL');
use v5.30;
eval "package Target; sub only_target { 1 }";
print "main has it: ", (main->can("only_target") ? "yes" : "no"), "\n";
print "Target has it: ", (Target->can("only_target") ? "yes" : "no"), "\n";
PL

both_agree('non-capturing named sub in eval, no package switch', <<'PL');
use v5.30;
eval "sub plain { return 7 }";
print plain(), "\n";
PL

# ---- TODO: free-variable capture by a named sub defined in eval ----
# Implemented by docs/eval-free-vars-plan.md (BlockAnalyzer-driven). When that
# lands these should pass and the harness will flag them as unexpectedly OK.

TODO: {
    local $TODO = "eval named-sub free-var capture (docs/eval-free-vars-plan.md)";

    both_agree('named sub in eval captures an enclosing lexical', <<'PL');
use v5.30;
sub make_method {
    my $secret = "SECRET";
    eval "package Target; sub greet { return \$secret; }";
}
make_method();
print Target::greet(), "\n";
PL

    # NOTE: the enclosing lexical must be a SUB-scoped `my` (a CL `let`), not a
    # top-level `my` (which compiles to a defvar/special and is already reachable
    # by the eval).  Wrapping in a sub exercises the genuine capture path.
    both_agree('eval named-sub capture respects shadowing', <<'PL');
use v5.30;
sub setup {
    my $z = 9;
    eval "sub get_z { return \$z } sub other { my \$z = 1; return \$z }";
}
setup();
print get_z(), "-", other(), "\n";
PL

    both_agree('var free in one eval sub, bound in another', <<'PL');
use v5.30;
sub setup2 {
    my $shared = "OUT";
    eval "sub uses_it { return \$shared } sub binds_it { my \$shared = 'IN'; return \$shared }";
}
setup2();
print uses_it(), "-", binds_it(), "\n";
PL
}

done_testing();
