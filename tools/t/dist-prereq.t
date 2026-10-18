#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Unit tests for PCLDistPrereq -- the BUILT prerequisites' blibs that
# tools/run-dist-t.pl puts on PERL5LIB for a dist's run, as `make test` under
# the CPAN shell has them (task #3520).
#
# NOT part of the Pl/t gate (it measures a measurement tool).  Run it directly:
#   prove tools/t/dist-prereq.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use FindBin;
use lib "$FindBin::RealBin/../lib";
use PCLDistPrereq;

my $tmp   = tempdir(CLEANUP => 1);
my $build = "$tmp/build";
my $dist  = "$build/My-Dist-1.0-0";

sub wfile { my ($p, $c) = @_; (my $d = $p) =~ s{/[^/]+\z}{}; make_path($d); open my $f, '>', $p or die "$p: $!"; print $f $c; close $f }
sub fake_build {
  my ($dir, $mod, $ver, $arch) = @_;
  (my $rel = "$mod.pm") =~ s{::}{/}g;
  wfile("$build/$dir/blib/lib/$rel", "package $mod;\nour \$VERSION = '$ver';\n1;\n");
  make_path("$build/$dir/blib/arch") if $arch;
}

# The dist requires: Fake::Xs 2.0 (box has 1.5; built 1.0 and 2.1),
# Fake::Ok 1.0 (box has 3 -- satisfied, nothing added), Fake::Gone 4 (box has
# none, built nowhere -- reported), and perl itself (never a module).
wfile("$dist/META.json", <<'J');
{ "name": "My-Dist", "version": "1.0",
  "prereqs": { "runtime": { "requires": { "perl": "5.006", "Fake::Xs": "2.0", "Fake::Ok": "1.0" } },
               "test":    { "requires": { "Fake::Gone": "4" } },
               "configure": { "requires": { "Fake::Cfg": "9" } } } }
J
fake_build('Fake-Xs-1.0-0', 'Fake::Xs', '1.0', 1);
fake_build('Fake-Xs-2.1-0', 'Fake::Xs', '2.1', 1);
fake_build('Fake-Ok-3-0',   'Fake::Ok', '3',   0);
# The dist's OWN blib never counts as a prerequisite's build.
fake_build('My-Dist-1.0-0', 'Fake::Gone', '5', 0);

my %box = ('Fake::Xs' => '1.5', 'Fake::Ok' => '3');
my ($added, $notes) = PCLDistPrereq::built_prereq_blibs(
  dist => $dist, build_root => $build, installed => sub { $box{$_[0]} });

is(scalar(@$added), 1, 'one prerequisite added: the one the box perl does not satisfy');
is($added->[0]{module}, 'Fake::Xs', '... Fake::Xs');
is($added->[0]{built}, '2.1', '... from the build whose version satisfies the requirement (not 1.0)');
is($added->[0]{dir}, "$build/Fake-Xs-2.1-0", '... that build dir');
is_deeply([PCLDistPrereq::perl5lib_dirs($added)],
          ["$build/Fake-Xs-2.1-0/blib/lib", "$build/Fake-Xs-2.1-0/blib/arch"],
          'PERL5LIB gets its blib/lib then blib/arch');
is(scalar(@$notes), 1, 'one requirement reported as built nowhere');
like($notes->[0], qr/^Fake::Gone 4: the box perl has none/, '... Fake::Gone (the dist\'s own blib is not a build of it)');
ok(!grep({ $_->{module} eq 'Fake::Cfg' } @$added) && !grep({ /Fake::Cfg/ } @$notes),
   'a CONFIGURE requirement is not a run-time one');

# A dist with no META adds nothing and reports nothing.
make_path("$build/Bare-0.1-0");
my ($a2, $n2) = PCLDistPrereq::built_prereq_blibs(
  dist => "$build/Bare-0.1-0", build_root => $build, installed => sub { undef });
is_deeply([$a2, $n2], [[], []], 'no META: nothing added, nothing reported');

# META.yml is read when there is no META.json.
wfile("$build/Yml-0.1-0/META.yml", "---\nname: Yml\nrequires:\n  Fake::Xs: 2.0\n");
my ($a3) = PCLDistPrereq::built_prereq_blibs(
  dist => "$build/Yml-0.1-0", build_root => $build, installed => sub { $box{$_[0]} });
is(join(',', map { "$_->{module}=$_->{built}" } @$a3), 'Fake::Xs=2.1', 'META.yml requires are read');

done_testing;
