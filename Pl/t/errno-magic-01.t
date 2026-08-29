#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# errno-magic-01.t — task #561: the COMPUTED magics reach the glob path, and
# %! exists.
#
# `$!` worked, but it was not backed by an ordinary glob slot an alias could
# reach: ordinary emission reads it through an ACCESSOR (%SPECIAL_VARS: '$!' =>
# ['p-errno-string']), so no |$!| variable existed, p-glob-copy's `boundp` said
# no, and `*Y = *!; print $Y` printed "" where perl prints the strerror text.
# `%!` did not exist at all — `keys %!` was an unbound CL variable and the
# whole file died at LOAD.  t/re/reg_namedcapture.t is exactly that file.
#
# The fix is the shape `|$.|` already had: a defvar'd box holding a
# p-magic-cell whose getter/setter ARE p-errno-string and its setf, so the box
# and the accessor read one state; and a REAL hash on |%!| whose VALUES are
# magic cells over *p-errno-table*, so keys/values/each/exists are ordinary
# hash operations and only the element read is computed.  ZERO glob-path
# changes were needed: a scalar glob slot's value IS the p-box, so an alias is
# box aliasing once the variable exists.
#
# WHAT THE ROWS PIN.  The dualvar must survive the alias in BOTH directions
# ($Y+0 is the errno, "$Y" is the message); a write THROUGH the alias must
# reach C errno; `local $!` must still restore; %! must answer perl's own
# values ($!{NAME} is the errno NUMBER when $! holds it and 0 otherwise —
# never 1, and always defined) and its STORE must die as Errno's tied hash
# does.  Every expectation is the live `perl` answer, so a row that drifts
# reports what perl said.
#
# BOUNDARY, probed and deliberately NOT pinned: perl's `*! = *src` REPLACES
# the glob and $! becomes src's plain scalar; PCL's plain $! keeps computing,
# because its emission never reads the slot (docs/ir-spec.md §8).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 8;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the #561 scalar half --------------------------------------------------

both_agree(<<'PL', '#561: *Y = *{"!"} and *Z = *! both alias the errno scalar');
no strict 'refs';
$! = 2;
print "1 direct=[$!]\n";
our $Y; *Y = *{"!"};
print "2 symref=[$Y]\n";
our $Z; *Z = *!;
print "3 star=[$Z]\n";
PL

both_agree(<<'PL', '#561: the DUALVAR survives the alias in both directions');
no strict 'refs';
$! = 13;
our $Y; *Y = *!;
print "1 str=[$Y] num=", $Y+0, "\n";
my $r = \$Y;
print "2 through-ref=[$$r] num=", $$r+0, "\n";
PL

both_agree(<<'PL', '#561: a WRITE through the alias sets C errno');
no strict 'refs';
$! = 2;
our $Y; *Y = *!;
$Y = 13;
print "1 errno=", $!+0, " str=[$!]\n";
PL

both_agree(<<'PL', '#561: `local $!` still saves and restores');
$! = 2;
{
  local $!;
  print "1 inside=", $!+0, "\n";
  $! = 5;
  print "2 inside-after-write=", $!+0, "\n";
}
print "3 restored=", $!+0, "\n";
PL

# ---- the %! half -----------------------------------------------------------

both_agree(<<'PL', '#561: %! element values are perl\'s (the NUMBER, or 0 — never 1)');
$! = 2;
print "1 ENOENT=[", $!{ENOENT}, "] true=", ($!{ENOENT} ? 1 : 0),
      " num=", $!{ENOENT}+0, "\n";
print "2 EACCES=[", $!{EACCES}, "] true=", ($!{EACCES} ? 1 : 0),
      " defined=", (defined $!{EACCES} ? 1 : 0), "\n";
$! = 13;
print "3 EACCES-now=[", $!{EACCES}, "] ENOENT-now=[", $!{ENOENT}, "]\n";
print "4 exists-EACCES=", (exists $!{EACCES} ? 1 : 0),
      " exists-BOGUS=", (exists $!{NOSUCHERRNONAME} ? 1 : 0), "\n";
print "5 aliased-numbers EAGAIN/EWOULDBLOCK: "; $! = 11;
print $!{EAGAIN}, "/", $!{EWOULDBLOCK}, "\n";
PL

both_agree(<<'PL', '#561: keys/values/each see one real entry per errno name');
$! = 2;
my @k = sort keys %!;
print "1 first5=", join(",", @k[0..4]), "\n";
my @v = values %!;
my $t = 0; for my $x (@v) { $t++ if $x }
print "2 same-count=", (scalar(@v) == scalar(@k) ? 1 : 0), " true=$t\n";
my $c = 0; while (my ($kk, $vv) = each %!) { $c++ }
print "3 each-count-matches=", ($c == scalar(@k) ? 1 : 0), "\n";
print "4 bool=", (%! ? 1 : 0), " has-ENOENT=", ((grep { $_ eq 'ENOENT' } @k) ? 1 : 0), "\n";
PL

# NOT pinned here: `$!` AFTER the failed store.  perl's own die path
# (Carp::confess) makes syscalls that clobber C errno, so perl reads 0 where
# PCL still reads 2 — perl's error machinery, not a semantic of %!.
both_agree(<<'PL', '#561: a STORE into %! is fatal, as Errno\'s tied hash is');
$! = 2;
my $ok = eval { $!{ENOENT} = 1; 1 };
print "1 write-ok=", ($ok ? 1 : 0), " msg=[", ($@ =~ /ERRNO hash is read only/ ? "read-only" : "OTHER"), "]\n";
$! = 2;
print "2 unchanged=[", $!{ENOENT}, "]\n";
PL

both_agree(<<'PL', '#561: *Y = *! reaches %! too — t/re/reg_namedcapture.t:26');
no strict 'refs';
$! = 2;
our %W; *W = *!;
print "1 gt0=", (0 < keys(%W) ? 1 : 0), "\n";
print "2 through-alias ENOENT=[", $W{ENOENT}, "] EACCES=[", $W{EACCES}, "]\n";
PL
