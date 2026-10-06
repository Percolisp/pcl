# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ctx-bind-01.t -- the *wantarray* bind at a BUILT-IN call site (s510c).
#
# Since #2775 a built-in call is bound to its static context ONLY when the
# built-in is in Pl::ExprToCL's %WANTARRAY_SENSITIVE: every other built-in is
# emitted bare in every slot (the old list-only fallback bind is gone).  That
# makes the table load-bearing, so the eight built-ins it was missing (#2801:
# getpw*/getgr*, the CALL spellings glob(...) / readline(...)) must be in it.
#
# Two inverse groups, each with a perl-PROBED expected output (perl 5.40.3;
# nothing here needs > 5.38):
#   S-*  a SCALAR slot inside a list-context call's argument list -- FAILS on
#        main before s510c (the outer list bind leaked in: `ARRAY(0x1)`).
#   L-*  a LIST slot no enclosing macro binds -- FAILS when the fallback is
#        removed WITHOUT the table addition (each answers 1).
# The fixture directory makes every count independent of the machine.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# One SBCL run per group; one row per output line, keyed by its label.
sub rows_agree {
    my ($code, $probed, $what) = @_;
    my %got = map { /^(\S+) (.*)$/ ? ($1 => $2) : () } split /\n/, run_cl($code);
    for my $line (split /\n/, $probed) {
        my ($label, $want) = $line =~ /^(\S+) (.*)$/;
        is($got{$label}, $want, "$what: $label");
    }
}

my $D = tempdir(CLEANUP => 1);
for my $f (qw(a1 a2 a3)) { open my $t, '>', "$D/$f" or die; close $t }
{ open my $t, '>', "$D/lines.txt" or die; print $t "l$_\n" for 1 .. 5; close $t }

# ── #2775 + #2801: the eight built-ins the table was missing ──────────────
rows_agree(qq{my \$D = "$D";\n} . <<'PL', <<'PROBED', '#2801 table');
sub id { @_ }
sub cnt { scalar @_ }
sub fh { open my $h, "<", "$D/lines.txt" or die; $h }
sub bad { $_[0] =~ /^ARRAY/ ? "bad" : "ok" }
print "S-pwuid ",  bad(id("" . getpwuid 0)), "\n";
print "S-pwnam ",  bad(id("" . getpwnam "root")), "\n";
print "S-grgid ",  bad(id("" . getgrgid 0)), "\n";
print "S-glob ",   (id("" . glob "$D/a*"))[0] =~ s{.*/}{}r, "\n";
{ my $h = fh(); my ($v) = id("" . readline($h)); chomp $v; print "S-readline $v\n"; }
{ my $c = cnt(getpwuid 0); print "L-pwuid ", ($c > 1 ? "list" : "scalar"), "\n"; }
{ my $c = cnt(getgrgid 0); print "L-grgid $c\n"; }
{ my $c = cnt(glob "$D/a*"); print "L-glob $c\n"; }
{ my $h = fh(); my $c = cnt(readline($h)); print "L-readline $c\n"; }
my @p; push @p, getgrgid 0; print "L-push ", scalar @p, "\n";
my $r = [glob "$D/a*"]; print "L-anon ", scalar @$r, "\n";
my %h = (k => [glob "$D/a*"]); print "L-hash ", scalar @{$h{k}}, "\n";
sub inS { my $z = cnt(glob "$D/a*"); $z } my $q = inS(); print "L-inS ", $q, "\n";
PL
S-pwuid ok
S-pwnam ok
S-grgid ok
S-glob a1
S-readline l1
L-pwuid list
L-grgid 4
L-glob 3
L-readline 5
L-push 4
L-anon 3
L-hash 3
L-inS 3
PROBED

done_testing();
