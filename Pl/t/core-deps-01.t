# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# core-deps-01.t — PCL depends on exactly PPI and Moo; nothing else that is
# not core perl (s440).
#
# The first CI run (s439) failed because three Pl/*.pm files imported
# Data::Dump for their debug dumps: present on every dev machine via a distro
# package, absent on a stock runner, so the INSTALLED pl2cl died at compile
# time ("Can't locate Data/Dump.pm") — a failure the local rehearsal in a
# sanitized HOME could not see, because the dev perl is the dev perl.  This
# file is the static version of a stock machine:
#
#   1. the compiler (every Pl/**/*.pm, tools/lib/*.pm) and what the three
#      runners import are LOADED under the running perl, and every file that
#      pulls in must be (a) in this repository, (b) core for this perl, or
#      (c) PPI's or Moo's own namespace, or loaded by PPI / a Moo class with
#      has-new-with-extends (their dependency closure).  Anything else is a
#      dependency the README does not name and the installer does not check.
#
#   2. a test file's `use X` (anywhere — transpile subjects included) whose X
#      is a real installed module that is neither core nor in that closure is
#      flagged, UNLESS the file declares it as a FIXTURE with the guard
#      `eval { require X; 1 }` (the SKIP convention of misc-fixes-01.t,
#      pcl-dash-m-01.t, transpile-test-07.t: the row skips where the CPAN
#      module is absent and CI installs it so the row still runs).  A name
#      that does not resolve under this perl at all (`use TestMod` inside a
#      transpile subject) is a local fixture and is not flagged.

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use Cwd qw(abs_path);
use File::Find;
use File::Temp qw(tempfile);
use Module::CoreList;

my $root = abs_path("$RealBin/../..");
my $perl = $^X;

# Run perl code in a child; return the sorted %INC paths it ended with.
sub child_inc {
    my ($label, $code) = @_;
    my ($efh, $err) = tempfile(UNLINK => 1);
    my $prog = $code . '; print "$_\n" for sort values %INC';
    my @out = qx{$perl -I"$root" -I"$root/tools/lib" -e '$prog' 2>"$err"};
    my $stderr = do { local $/; open my $e, '<', $err; <$e> // '' };
    is($?, 0, "$label: child perl loaded cleanly") or diag($stderr);
    chomp @out;
    return @out;
}

sub path_to_module {
    my ($rel) = @_;
    $rel =~ s/\.pm$//;
    $rel =~ s{/}{::}g;
    return $rel;
}

my @inc_dirs = grep { !ref } @INC;

# The module name a %INC path stands for, by the @INC dir it was found under.
sub module_of {
    my ($path) = @_;
    for my $dir (@inc_dirs) {
        next if index($path, "$dir/") != 0;
        return path_to_module(substr($path, length($dir) + 1)) if $path =~ /\.pm$/;
        return undef;                       # .ph / .pl / .al helper files
    }
    return undef;
}

# ---- 1. the closure of the two declared dependencies, exercised
my %closure = map { $_ => 1 } child_inc('PPI+Moo closure',
    'require PPI; require PPI::Dumper; require PPI::Document;'
  . 'package CoreDeps::R; use Moo::Role; requires "x";'
  . 'package CoreDeps::T; use Moo; has x => (is => "rw"); has y => (is => "lazy", builder => sub { 1 });'
  . 'package CoreDeps::U; use Moo; extends "CoreDeps::T"; with "CoreDeps::R"; around x => sub { my $o = shift; $o->(@_) };'
  . 'package main; CoreDeps::U->new(x => 1)->y');
ok(scalar(keys %closure) > 10, "PPI+Moo closure loaded (" . scalar(keys %closure) . " files)");

sub allowed {
    my ($path) = @_;
    return 'repo'    if index(abs_path($path) // $path, $root) == 0;
    return 'closure' if $closure{$path};
    my $mod = module_of($path);
    return 'helper'  if !defined $mod;                            # .ph/.pl/.al next to a core module
    return 'core'    if Module::CoreList::is_core($mod, undef, $]);
    return 'dep-namespace' if $mod =~ /^(?:PPI|Moo)(?:::|$)/;
    return "NOT CORE: $mod ($path)";
}

# ---- 2. the compiler + the runners' own imports
my @mods;
find(sub { push @mods, path_to_module(substr($File::Find::name, length("$root/"))) if /\.pm$/ }, "$root/Pl");
@mods = grep { !/^Pl::t::/ } @mods;                 # the gate's helpers are not the compiler
for my $f (qw(pl2cl runpcl pcl)) {
    open my $fh, '<', "$root/$f" or die "$f: $!";
    while (<$fh>) { push @mods, $1 if /^use\s+([A-Z][\w:]*)/ }
    close $fh;
}
push @mods, path_to_module((split m{/}, $_)[-1]) for glob "$root/tools/lib/*.pm";
my %seen;
@mods = grep { !$seen{$_}++ } @mods;
my @loaded = child_inc('compiler + runners', join('; ', map { "require $_" } @mods));
my @bad = grep { /^NOT CORE/ } map { allowed($_) } @loaded;
is_deeply(\@bad, [], "the compiler + runners load only repo / core / PPI+Moo files")
    or diag("offenders:\n  " . join("\n  ", @bad));

# ---- 3. the test files
my %flag;
my %resolves;                                        # module -> installed path or ''
for my $t (sort(glob("$root/Pl/t/*.t"), glob("$root/Pl/t/*.pm"))) {
    open my $fh, '<', $t or die "$t: $!";
    my $text = do { local $/; <$fh> };
    close $fh;
    my %names = map { $_ => 1 } $text =~ /^\s*use\s+([A-Z][\w:]*)/mg;
    for my $m (sort keys %names) {
        next if $m =~ /^(?:Pl|PCL|Pcl)/ || $m =~ /^(?:PPI|Moo)(?:::|$)/;
        next if Module::CoreList::is_core($m, undef, $]);
        if (!exists $resolves{$m}) {
            (my $file = "$m.pm") =~ s{::}{/}g;
            my ($dir) = grep { -f "$_/$file" } @inc_dirs;
            $resolves{$m} = $dir ? "$dir/$file" : '';
        }
        next if $resolves{$m} eq '';                 # a local fixture: not installed here either
        next if index($resolves{$m}, $root) == 0;    # resolved from the repo
        next if $closure{ $resolves{$m} };
        next if $text =~ /eval \{ require \Q$m\E; 1 \}/;   # a declared CPAN fixture (SKIP guard)
        push @{ $flag{$m} }, substr($t, length("$root/"));
    }
}
is_deeply(\%flag, {}, "no test file imports an installed-but-non-core module without a fixture guard")
    or diag(join("\n", map { "$_: @{$flag{$_}}" } sort keys %flag));

done_testing();
