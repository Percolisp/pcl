# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLVendor;

# PCLVendor -- what a VENDORED copy is, as a program can check it (s527a).
#
# PCL carries its own PPI as vendor/PPI/ (docs/ppi-fix-at-source-s527.md):
# the `lib/` of the branch `pcl` of github.com/Percolisp/PPI, which is
# upstream PPI master plus PCL's fix commits.  vendor/PPI/UPSTREAM is the
# stamp that says which commits that copy is.  Two helpers:
#
#   tree_sha(DIR)    the git TREE id of DIR's contents, computed in perl --
#                    the same id `git rev-parse <branch>:lib` prints, so the
#                    stamp can be checked on a tree with no git (an
#                    installed PCL) and on an uncommitted copy alike.
#   read_stamp(FILE) the stamp's `key: value` lines as a hash (repeated
#                    keys -- `fix:` -- as an array under the key).
#   ppi_deps()       THE ONE LIST of the vendored PPI's non-core dependencies
#                    (task #3924), as [module, Debian/Ubuntu package] pairs.
#
# Used by tools/vendor-ppi (writes the stamp) and Pl/t/vendor-ppi-01.t
# (checks it).  ppi_deps() is read by tools/install-pcl (its remedy
# text) and checked by Pl/t/core-deps-01.t: the vendored PPI's MEASURED non-core
# closure must be in it (a new upstream dependency fails that gate row, not a
# CI run), and the README, .github/workflows/ci.yml and
# tools/install-matrix/deps.sh lines that install them must name exactly it.

use strict;
use warnings;
use Digest::SHA ();

# A git tree id: each file a blob "blob <len>\0<bytes>", each directory a
# tree of "<mode> <name>\0<20-byte id>" entries in git's order (a directory
# sorts as if its name ended in "/").  Files are mode 100644, or 100755 when
# executable; nothing in a vendored lib is a symlink.
sub tree_sha {
  my ($dir) = @_;
  opendir my $dh, $dir or die "tree_sha: $dir: $!\n";
  my @names = grep { $_ ne '.' && $_ ne '..' } readdir $dh;
  closedir $dh;
  my @ent;
  for my $n (@names) {
    my $p = "$dir/$n";
    die "tree_sha: $p is a symlink\n" if -l $p;
    if (-d $p) {
      push @ent, [ "$n/", "40000 $n\0" . pack('H*', tree_sha($p)) ];
    } else {
      open my $fh, '<:raw', $p or die "tree_sha: $p: $!\n";
      local $/;
      my $c = <$fh>;
      $c = '' if !defined $c;
      close $fh;
      my $blob = Digest::SHA::sha1("blob " . length($c) . "\0" . $c);
      my $mode = -x $p ? '100755' : '100644';
      push @ent, [ $n, "$mode $n\0$blob" ];
    }
  }
  my $body = join '', map { $_->[1] } sort { $a->[0] cmp $b->[0] } @ent;
  return Digest::SHA::sha1_hex("tree " . length($body) . "\0" . $body);
}

sub read_stamp {
  my ($file) = @_;
  open my $fh, '<', $file or die "read_stamp: $file: $!\n";
  my %s;
  while (my $l = <$fh>) {
    next if $l =~ /^\s*(?:#|$)/;
    chomp $l;
    my ($k, $v) = $l =~ /^(\w[\w-]*):\s*(.*)$/ or die "read_stamp: $file: bad line '$l'\n";
    if ($k eq q{fix} || $k eq q{commit}) { push @{ $s{$k} }, $v } else { $s{$k} = $v }
  }
  close $fh;
  return \%s;
}

# THE VENDORED PPI'S NON-CORE DEPENDENCIES (task #3924).  Clone, Params::Util
# and Safe::Isa are loaded by PPI at run time (measured: the closure the gate
# row computes); Task::Weaken is PPI's dist.ini prerequisite that only forces
# Scalar::Util's weaken to exist, so it is installed and never loaded.
# YAML::PP is NOT here: PPI reads it only for $ENV{PPI_CUSTOM_FEATURE_INCLUDES}
# and, on PCL's branch, loads it only then (s527b, group L).
my @PPI_DEPS = (
  [ 'Clone',        'libclone-perl' ],
  [ 'Params::Util', 'libparams-util-perl' ],
  [ 'Safe::Isa',    'libsafe-isa-perl' ],
  [ 'Task::Weaken', 'libtask-weaken-perl' ],
);
sub ppi_deps { return map { [@$_] } @PPI_DEPS }

1;
