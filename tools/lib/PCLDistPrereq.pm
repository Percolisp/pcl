# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCLDistPrereq -- the environment `make test` has under the CPAN shell, for a
# dist's own t/ run (task #3520).
#
# A dist test that runs a CHILD with `$^X` runs the box perl (under PCL `$^X`
# is the box perl by design, #2689), with that perl's own core modules.  When
# the dist requires a NEWER version of a module than the box perl ships
# (IO-Compress 2.214 wants Compress::Raw::Zlib 2.214; perl 5.40.3 ships
# 2.212), the CPAN shell has BUILT that prerequisite and `make test` runs with
# its blib on PERL5LIB -- so the child loads.  Without it every child dies at
# load and its rows say nothing about PCL (IO-Compress t/011-streamzip.t: 84
# rows).
#
# built_prereq_blibs() reproduces exactly that: for each requirement in the
# dist's META (runtime, build and test phases) that the box perl does NOT
# satisfy, the newest BUILT copy under the build root whose version does
# satisfy it.  A requirement the box perl satisfies adds nothing (the CPAN
# shell would not have built it either); one that is built nowhere is
# REPORTED, never fetched or built here.

package PCLDistPrereq;
use strict;
use warnings;
use JSON::PP ();
use Module::Metadata ();
use version ();
use File::Basename qw(basename);

# The dist's requirements: { module => minimum version string ('0' = any) }.
sub requirements {
  my ($dist) = @_;
  my %req;
  my $add = sub {
    my ($m, $v) = @_;
    return if $m eq 'perl';
    $v = '0' if !defined $v || $v eq '';
    $req{$m} = $v if !exists $req{$m} || _vcmp($v, $req{$m}) > 0;
  };
  if (open my $fh, '<:raw', "$dist/META.json") {
    my $meta = JSON::PP->new->utf8->decode(do { local $/; <$fh> });
    my $pr = $meta->{prereqs} || {};
    for my $phase (qw(runtime build test)) {
      my $r = ($pr->{$phase} || {})->{requires} || {};
      $add->($_, $r->{$_}) for keys %$r;
    }
  } elsif (-f "$dist/META.yml") {
    require CPAN::Meta::YAML;
    my $y = CPAN::Meta::YAML->read("$dist/META.yml") or return {};
    my $meta = $y->[0] || {};
    for my $key (qw(requires build_requires test_requires)) {
      my $r = $meta->{$key} || {};
      $add->($_, $r->{$_}) for keys %$r;
    }
  }
  return \%req;
}

sub _vcmp {
  my ($a, $b) = @_;
  my ($va, $vb) = map { my $x = eval { version->parse($_) }; $x // version->parse('0') } $a, $b;
  return $va <=> $vb;
}

sub _file_version {
  my ($file) = @_;
  my $md = eval { Module::Metadata->new_from_file($file) } or return undef;
  my $v = $md->version;
  return defined $v ? "$v" : undef;
}

# Returns (\@added, \@notes): each added entry { module, required, built,
# installed, dir, lib, arch }; each note a one-line string for a requirement
# the box perl does not satisfy and no build satisfies.
#   dist       => the dist directory
#   build_root => where the CPAN shell builds (default ~/.cpan/build)
#   installed  => CODE(module) -> the box perl's version or undef (default:
#                 Module::Metadata over this perl's @INC; a test injects one)
sub built_prereq_blibs {
  my (%a) = @_;
  my $dist = $a{dist} // die "built_prereq_blibs: dist\n";
  my $root = $a{build_root} // "$ENV{HOME}/.cpan/build";
  my $inst = $a{installed} // sub {
    my $md = eval { Module::Metadata->new_from_module($_[0], collect_pod => 0) } or return undef;
    my $v = $md->version; return defined $v ? "$v" : undef;
  };
  my $self_name = basename($dist);
  my $req = requirements($dist);
  my (@added, @notes);
  for my $m (sort keys %$req) {
    my $want = $req->{$m};
    my $have = $inst->($m);
    next if defined $have && _vcmp($have, $want) >= 0;
    (my $rel = "$m.pm") =~ s{::}{/}g;
    my $best;
    for my $f (sort glob("$root/*/blib/lib/$rel")) {
      my $dir = substr($f, 0, length($f) - length("/blib/lib/$rel"));
      next if basename($dir) eq $self_name;
      my $v = _file_version($f) // '0';
      next if _vcmp($v, $want) < 0;
      $best = { dir => $dir, built => $v }
        if !$best || _vcmp($v, $best->{built}) >= 0;
    }
    if (!$best) {
      push @notes, sprintf "%s %s: the box perl has %s and no build under %s satisfies it",
        $m, $want, $have // 'none', $root;
      next;
    }
    push @added, { module => $m, required => $want, built => $best->{built},
                   installed => $have, dir => $best->{dir},
                   lib => "$best->{dir}/blib/lib",
                   (-d "$best->{dir}/blib/arch" ? (arch => "$best->{dir}/blib/arch") : ()) };
  }
  return (\@added, \@notes);
}

# The PERL5LIB entries for the added builds, each build dir once, in order.
sub perl5lib_dirs {
  my ($added) = @_;
  my (%seen, @d);
  for my $e (@$added) {
    for my $d (grep { defined } $e->{lib}, $e->{arch}) { push @d, $d if !$seen{$d}++ }
  }
  return @d;
}

1;
