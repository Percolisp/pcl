#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# autodie-01.t -- task #2873: `use autodie` makes the built-ins it names DIE
# on failure, under PCL through lib/autodie.pm (the facts) and the
# builtin-override registry (the mechanism: a weak keyword imported into a
# package is called as that package's sub from the `use` on, to a `no` of a
# module that has an unimport).  Before, `use autodie` loaded perl's own
# autodie.pm, whose Fatal.pm wrappers are built by string evals that do not
# compile under PCL, and with no import list the call sites stayed builtins:
# a failed open was SILENT.
#
# Every row compares with perl's STDOUT.  The programs print the message's
# leading "Can't NAME" and the exception's class, never its location: under
# PCL caller() does not name the perl source (task #233, deferred), so the
# shim claims no " at FILE line N" (docs/not-supported.md "autodie").
# Also here: #2923, `unlink` / `chmod` / `chown` / `utime` of an ARRAY did
# nothing and answered 0 (the runtime did not flatten the list).
# No 5.40 syntax: CI's perl is 5.38.

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

plan tests => 12;

# A per-program scratch directory, and a reporter for one eval's outcome.
my $PRE = <<'PL';
no warnings;
my $T = "/tmp/pcl-autodie-01-$$"; CORE::mkdir $T;
sub rep { my ($tag) = @_; my $e = $@;
  if (!$e) { print "$tag: ok\n"; return }
  my ($what) = "$e" =~ /\A(Can't \w+)/;
  print "$tag: died [", ($what // '?'), "] ", ref($e), "\n" }
END { system("rm", "-rf", $T) }
PL

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "$PRE$code";
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code, $stderr) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $redir = $stderr ? '2>&1 >/dev/null' : '2>/dev/null';
    my $output = `sbcl @sbcl_rt --load $cl_file $redir`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $file = write_pl($code);
    my $perl = `perl $file 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree(<<'PL', 'open: dies on failure (reading / writing / 2-arg), succeeds silently');
use autodie;
eval { open(my $fh, '<', "$T/none") }; rep('r');
eval { open(my $fh, '>', "$T/no/such") }; rep('w');
eval { open(my $fh, "$T/none") }; rep('2arg');
eval { open(my $fh, '>', "$T/f"); print $fh "x\n"; close $fh }; rep('ok');
eval { open(FH, '<', "$T/f"); my $l = <FH>; close FH; print "line $l" }; rep('bareword');
PL

both_agree(<<'PL', 'the exception object: class, matches, return');
use autodie;
eval { unlink("$T/none") };
print ref($@), " ", ($@->matches('unlink') ? 1 : 0), ($@->matches(':filesys') ? 1 : 0),
      ($@->matches('open') ? 1 : 0), "\n";
CORE::open(my $w, '>', "$T/a"); CORE::close($w);
eval { unlink("$T/a", "$T/none") }; print "return ", $@->return, "\n";
PL

both_agree(<<'PL', 'close / opendir / closedir');
use autodie;
CORE::open(my $w, '>', "$T/f"); CORE::close($w);
eval { open(my $fh, '<', "$T/f"); close($fh); close($fh) }; rep('close');
eval { opendir(my $d, "$T/none") }; rep('opendir');
eval { opendir(my $d, $T); my @e = grep { !/^\./ } readdir($d); closedir($d); print "entries @e\n" }; rep('dir ok');
PL

both_agree(<<'PL', 'unlink / rename / mkdir / rmdir / chdir');
use autodie;
eval { unlink("$T/none") }; rep('unlink');
eval { rename("$T/none", "$T/x") }; rep('rename');
eval { mkdir("$T/no/such") }; rep('mkdir');
eval { mkdir("$T/d") }; rep('mkdir ok');
eval { rmdir("$T/d") }; rep('rmdir ok');
eval { rmdir("$T/d") }; rep('rmdir');
eval { chdir("$T/none") }; rep('chdir');
PL

both_agree(<<'PL', 'chmod / utime / truncate / read / seek / binmode');
use autodie;
CORE::open(my $w, '>', "$T/f"); print $w "hello world\n"; CORE::close($w);
eval { chmod(0644, "$T/f") }; rep('chmod ok');
eval { chmod(0644, "$T/none") }; rep('chmod');
eval { utime(undef, undef, "$T/f") }; rep('utime ok');
eval { utime(undef, undef, "$T/none") }; rep('utime');
eval { truncate("$T/none", 0) }; rep('truncate');
eval { open(my $fh, '<', "$T/f"); my $b; my $n = read($fh, $b, 5); print "read $n [$b]\n";
       seek($fh, 0, 0); binmode($fh); close($fh) }; rep('io ok');
PL

both_agree(<<'PL', 'kill 0: the answer when USED, a death in void context');
use autodie;
my $n; eval { $n = kill(0, 999999) }; rep('kill0'); print "n=$n\n";
eval { kill(0, 999999) }; rep('void');
PL

both_agree(<<'PL', 'in force from the `use`, and ended by `no autodie`');
eval { open(my $fh, '<', "$T/none") or print "before: false\n" }; rep('before');
use autodie;
eval { open(my $fh, '<', "$T/none") }; rep('during');
no autodie;
eval { open(my $fh, '<', "$T/none") or print "after: false\n" }; rep('after');
PL

both_agree(<<'PL', 'an explicit list wraps only what it names');
use autodie qw(open);
eval { open(my $fh, '<', "$T/none") }; rep('open');
eval { unlink("$T/none") or print "unlink: false\n" }; rep('unlink');
PL

both_agree(<<'PL', 'a tag in the list (:filesys)');
use autodie qw(:filesys);
eval { unlink("$T/none") }; rep('unlink');
eval { open(my $fh, '<', "$T/none") or print "open: false\n" }; rep('open');
PL

my $err = run_cl("use autodie;\nprint \"ran\\n\";\n", 1);
like($err, qr/\APCL: autodie is not in effect for: .*\bsocket\b/,
     'the :default built-ins the shim does not wrap are ANNOUNCED on stderr');

# ---- #2923: list built-ins over an ARRAY (no autodie) --------------------

both_agree(<<'PL', '#2923: unlink / chmod / chown over an array');
my @f = map { "$T/f$_" } 1 .. 3;
for (@f) { CORE::open(my $w, '>', $_); CORE::close($w) }
print "chmod ", chmod(0600, @f), "\n";
print "chown ", chown($<, $( + 0, @f), "\n";
print "unlink ", unlink(@f), " left ", scalar(grep { -e } @f), "\n";
PL

both_agree(<<'PL', '#2923: utime with the times in variables, over an array');
my @f = map { "$T/g$_" } 1 .. 2;
for (@f) { CORE::open(my $w, '>', $_); CORE::close($w) }
my ($at, $mt) = (1000000000, 1000000000);
print "utime ", utime($at, $mt, @f), " mtime ", (stat $f[0])[9], "\n";
my ($u, $v);
print "now ", utime($u, $v, @f), " recent ", ((stat $f[1])[9] > 1000000000 ? 1 : 0), "\n";
PL
