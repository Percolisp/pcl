#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/run-dist-t.pl (the CPAN board's per-file runner) spawns SBCL through
# the ONE command-line builder, tools/lib/PCLSbcl.pm (task #344).  It used to
# spell the line by hand in SOURCE mode, so every board file recompiled the
# whole runtime (#2959).  These rows read the line the runner actually spawns
# (PCL_SHOW_SBCL=1) and compare it with PCLSbcl's.
#
# NOT part of the Pl/t gate (it measures the harness).  Run it directly:
#   prove tools/t/run-dist-t-sbcl.t
use strict;
use warnings;
use Test::More tests => 6;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLSbcl qw(sbcl_prefix_str);

my $root = "$RealBin/../..";
chomp(my $abs = `cd "$root" && pwd`);
my $dist = tempdir(CLEANUP => 1);
mkdir "$dist/t" or die $!;
open my $fh, '>', "$dist/t/a.t" or die $!;
print $fh "use Test::More tests => 1;\nok(1, 'one');\n";
close $fh;

sub spawned_line {
  my (%env) = @_;
  local @ENV{keys %env} = values %env;
  local $ENV{PCL_SHOW_SBCL} = 1;
  my $out = `"$abs/tools/run-dist-t.pl" --rows --no-dist-lib "$dist" t/a.t 2>&1`;
  my ($line) = $out =~ /^SBCL\[run-dist-t\]: (.*)$/m;
  return ($line, $out);
}

my $tail = qr{ --eval "\(setf pcl::\*pcl-skip-cache\* t\)" --load /tmp/run-dist-t_\d+\.lisp$};
my $rt = "$abs/cl/pcl-runtime.lisp";

{
  my ($line, $out) = spawned_line();
  my $want = sbcl_prefix_str(runtime => $rt, quote => 0);
  ok(defined $line && index($line, "$want ") == 0,
     'the default line IS PCLSbcl::sbcl_prefix_str (the cached core when it exists)')
    or diag("got: " . ($line // "(none)") . "\nwant prefix: $want");
  like($line // '', $tail, 'followed by the runner\'s own skip-cache --eval and the test --load');
  like($out, qr/^pass=1 fail=0/m, 'and the file runs: pass=1 fail=0');
}
{
  my ($line, $out) = spawned_line(PCL_NO_CORE => 1);
  ok(defined $line && $line !~ /--core/, 'PCL_NO_CORE=1: no --core (source mode)');
  like($line // '', qr{^sbcl --control-stack-size 512 --noinform --non-interactive --load \Q$rt\E$tail},
       'PCL_NO_CORE=1 gives back the old hand-written source-mode line');
  like($out, qr/^pass=1 fail=0/m, 'and the file runs there too');
}
