# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-24-resource-idioms.pl
use strict; use warnings;
my $f = "/tmp/pcl-b2-res-$$"; my @ev;
{ open(my $fh, ">", $f) or die; print $fh "scoped\n"; } push @ev, (-s $f ? "closed-at-scope-exit" : "NOT-flushed");
sub with_file { my ($p, $cb) = @_; open(my $fh, "<", $p) or die "open: $!"; my @r = eval { $cb->($fh) }; my $err = $@; close $fh; die $err if $err; @r }
push @ev, with_file($f, sub { my $h = shift; my $l = <$h>; chomp $l; $l }); push @ev, (eval { with_file($f, sub { die "cb failed\n" }); 1 } ? "lived" : "propagated:$@");
my $pid = open(my $p, "-|") // die; if (!$pid) { print "from child\n"; exit 0 } my $cl = <$p>; close $p; chomp $cl; push @ev, $cl, "status=$?";
my $kid = fork // die; if (!$kid) { exit 7 } waitpid($kid, 0); push @ev, "exit=" . ($? >> 8);
unlink $f; chomp @ev; print join(" | ", @ev), "\n";
