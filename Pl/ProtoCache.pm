# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::ProtoCache;

use v5.20;
use strict;
use warnings;

use Cwd ();
use File::Basename qw(dirname basename);
use Digest::MD5 qw(md5_hex);

=head1 NAME

Pl::ProtoCache - the on-disk memo for C<_extract_module_prototypes> (task #560)

=head1 DESCRIPTION

C<Pl::Parser::_extract_module_prototypes> walks a module for the PARSE FACTS a
using file needs — prototypes (a C<(&@)> makes a trailing block a block-form
argument; a C<($)> imposes scalar context; an empty one makes a bareword a
term) and exported sub NAMES (a bareword before a comma is a call only for a
known sub).  The walk is RECURSIVE: the module's own C<use> statements are
followed, so C<use Test2::API qw/intercept/> parses 27 modules and 242 KB of
Perl through PPI.

That walk was memoized in a C<state> hash, which dies with the process, so
every C<pl2cl> paid it again: measured over the 289-file cpan-t census
population, 226.6 s of which the Test2 stack is the bulk (task #560; the
4.3x that #478's by-name skip list had been hiding, at the price of a
mis-parse).

The walk is PURE — module file in, facts out — so this memoizes its RESULT on
disk, ONE ENTRY PER MODULE, consulted before the walk recurses: a hit prunes
the whole branch, and a module's own edit invalidates exactly its own entry.

=head2 What makes an entry valid

An entry is used only when every input the walk read is unchanged:

=over 4

=item * the module file itself — absolute path + mtime + size, hashed into the
entry's FILENAME and repeated inside the JSON (a mismatch on read means the
entry is ignored and rebuilt: that is also what a corrupted or truncated file
looks like, since it fails to decode);

=item * C<*pcl-cache-generation*> — the project's existing "emission changed"
stamp (a C<collect_prototypes> change is an emission change);

=item * a fingerprint of the COMPILER itself (the absolute path of the C<Pl/>
directory plus every C<Pl/**.pm>'s mtime+size), because the facts are a
function of the code that derived them.  This is also what keeps two worktrees
that happen to share a generation string from reading each other's entries —
the same rule C<PCLSbcl> uses to key a saved core by the runtime's absolute
path;

=item * C<$PPI::VERSION> and C<$]>, since the token stream is the walk's input;

=item * every DEPENDENCY the walk resolved.  The facts an entry holds are
dependency-INCLUSIVE (the walk merges what it finds below), so the entry
records each dependency it resolved — module name, the path it resolved to
under the inc_paths in force, mtime and size — and a hit re-resolves each name
and re-stats it.  That covers both "a dependency changed" and "a dependency
now resolves somewhere else" (a dist's C<t/lib> shadowing a shipped shim),
which a path-and-mtime key alone would not.

=back

=head2 What is never cached

A walk whose result depends on WHERE IT STARTED is not a fact about the
module, and is never stored:

=over 4

=item * a walk truncated by CYCLE detection (A uses B uses A: whichever module
the process reached first is the one whose facts are complete).  The taint
marks every frame on the stack, and propagates to any later walk in the same
process that reuses the truncated result through the in-process memo;

=item * a walk that changed C<@INC> under itself (a C<use lib> inside a
module): the next walk in the same process would resolve differently.
Detected by comparing the inc_paths list across the walk, so it stays true
however the mutation is spelled.

=back

=head2 Layout

C<~/.pcl-cache/proto/E<lt>moduleE<gt>-E<lt>hash16E<gt>.json> (or under
C<$PCL_CACHE_DIR>).  Writes are temp-file + rename, because C<prove -j8> and
the census run many transpiles at once.  C<PCL_NO_PROTO_CACHE=1> turns the
whole thing off (the A/B switch for measurement); C<pcl --clear-cache> removes
the directory.

=cut

# ---------------------------------------------------------------- constants

# Absolute Pl/ directory, resolved at load time: a later chdir (perl's own
# tests do `chdir 't'`) must not change what the stamp names.
my $PL_DIR = Cwd::abs_path(dirname(__FILE__)) // dirname(__FILE__);

sub enabled { return $ENV{PCL_NO_PROTO_CACHE} ? 0 : 1 }

# PCL_PROTO_STATS=1 prints hit/miss/store counts on exit — how the hit rate is
# measured.  Off by default and never on stdout (a transpile's stdout is the
# emitted CL).
our %STATS;
END {
  return unless $ENV{PCL_PROTO_STATS};
  warn sprintf("PCL proto-cache: hit %d  miss %d  stale %d  store %d  taint %d\n",
               map { $STATS{$_} // 0 } qw(hit miss stale store taint));
  # =2 also names the modules that took each path, which is how "why is this
  # one never cached" is answered without a debugger.
  return unless $ENV{PCL_PROTO_STATS} >= 2;
  for my $k (qw(missed tainted)) {
    warn "PCL proto-cache $k: @{ $STATS{$k} }\n" if $STATS{$k};
  }
}

sub cache_dir {
  my $root = $ENV{PCL_CACHE_DIR} // (($ENV{HOME} // '.') . '/.pcl-cache');
  return "$root/proto";
}

# THE generation string, read from the runtime's `*pcl-cache-generation*` —
# the single source of truth (pl2cl's pipeline marker reads the same file
# through this function; do NOT write the string down anywhere else).
my $GEN;
sub generation {
  return $GEN if defined $GEN;
  $GEN = 'unknown';
  my $rt = dirname($PL_DIR) . '/cl/pcl-runtime.lisp';
  if (open my $fh, '<', $rt) {
    while (my $line = <$fh>) {
      if ($line =~ /\(defparameter\s+\*pcl-cache-generation\*\s+"([^"]+)"/) {
        $GEN = $1;
        last;
      }
    }
    close $fh;
  }
  return $GEN;
}

# Fingerprint of the compiler that derives the facts (see the POD).
my $COMPILER_STAMP;
sub _compiler_stamp {
  return $COMPILER_STAMP if defined $COMPILER_STAMP;
  my @parts = ($PL_DIR);
  for my $dir (_pm_dirs()) {
    opendir my $dh, $dir or next;
    for my $f (sort grep { /\.pm$/ } readdir $dh) {
      my @s = stat "$dir/$f";
      push @parts, "$dir/$f:" . ($s[9] // 0) . ':' . ($s[7] // 0);
    }
    closedir $dh;
  }
  return $COMPILER_STAMP = md5_hex(join "\0", @parts);
}

sub _pm_dirs {
  my @d = ($PL_DIR);
  opendir my $dh, $PL_DIR or return @d;
  push @d, map { "$PL_DIR/$_" }
           sort grep { $_ !~ /^\.\.?$/ && -d "$PL_DIR/$_" } readdir $dh;
  closedir $dh;
  return @d;
}

sub _key {
  my ($path, $mtime, $size) = @_;
  return join "|", $path, $mtime, $size, generation(), _compiler_stamp(),
                   ($PPI::VERSION // ''), $];
}

sub _entry_path {
  my ($module, $key) = @_;
  (my $slug = $module) =~ s/[^\w.]+/_/g;
  $slug = substr($slug, 0, 60);
  return cache_dir() . "/$slug-" . substr(md5_hex($key), 0, 16) . '.json';
}

# ------------------------------------------------------------- walk frames
#
# One frame per walk in progress.  A frame collects the DEPENDENCIES that walk
# resolved (its own and, when a nested walk finishes or hits a cache, that
# walk's transitive set) and whether the walk was truncated in a way that makes
# its result depend on where the process started (see the POD).

our @FRAMES;

sub begin_walk { push @FRAMES, { deps => {}, taint => 0 }; return }

# Drop every frame: called once per SOURCE, so a transpile that died mid-walk
# cannot leave a half-unwound stack for the next one (pl2cl --server).
sub reset_walks { @FRAMES = (); return }

# Pops the frame and returns { deps => [sorted list], taint => 0|1 }.
sub end_walk {
  my $f = pop @FRAMES or return { deps => [], taint => 1 };
  return { deps => [ map { $f->{deps}{$_} } sort keys %{ $f->{deps} } ],
           taint => $f->{taint} };
}

# Record one resolved dependency in the walk currently in progress.  PATH may
# be undef ("this name does not resolve"), which is a fact the walk used.
sub note_dep {
  my ($kind, $name, $path) = @_;
  return unless @FRAMES;
  my ($mtime, $size) = (0, 0);
  if (defined $path) {
    my @s = stat $path or return;   # vanished under us: record nothing
    ($mtime, $size) = ($s[9], $s[7]);
  }
  $FRAMES[-1]{deps}{"$kind\0$name"} = [ $kind, $name, $path, $mtime, $size ];
  return;
}

# Merge a finished walk's transitive dependency list into the enclosing walk.
sub note_deps {
  my ($deps) = @_;
  return unless @FRAMES && ref $deps eq 'ARRAY';
  $FRAMES[-1]{deps}{ $_->[0] . "\0" . $_->[1] } = $_ for @$deps;
  return;
}

# This result depends on where the process started — no walk in progress may
# be stored, at any depth.
sub taint { $_->{taint} = 1 for @FRAMES; return }

# ------------------------------------------------------------------ the I/O

sub _json {
  require JSON::PP;
  return JSON::PP->new->utf8->canonical;
}

=head2 load($module, $path, $resolve)

Returns C<< { env => Pl::Environment, deps => \@deps } >> for a valid entry, or
undef.  C<$resolve> is a coderef mapping a module NAME to the path it resolves
to now (C<_find_module_file>), used to re-check every recorded dependency.

=cut

sub load {
  my ($module, $path, $resolve) = @_;
  return undef unless enabled();
  my @s = stat $path or return undef;
  my $key  = _key($path, $s[9], $s[7]);
  my $file = _entry_path($module, $key);
  open my $fh, '<:raw', $file or do {
    $STATS{miss}++;
    push @{ $STATS{missed} }, $module if ($ENV{PCL_PROTO_STATS} // 0) >= 2;
    return undef;
  };
  my $json = do { local $/; <$fh> };
  close $fh;
  # A truncated or corrupted entry fails HERE, and the caller rebuilds — the
  # same path a key mismatch or a moved dependency takes.
  my $rec = (defined $json && length $json)
          ? eval { _json()->decode($json) } : undef;
  if (   !$rec || ref $rec ne 'HASH'
      || ($rec->{key} // '') ne $key
      || ref $rec->{prototypes} ne 'HASH'
      || !_deps_valid($rec->{deps}, $resolve)) {
    $STATS{stale}++;
    return undef;
  }
  my $env = _env_from_record($rec) or do { $STATS{stale}++; return undef };
  $STATS{hit}++;
  return { env => $env, deps => $rec->{deps} };
}

sub _deps_valid {
  my ($deps, $resolve) = @_;
  return 0 unless ref $deps eq 'ARRAY';
  for my $d (@$deps) {
    return 0 unless ref $d eq 'ARRAY' && @$d == 5;
    my ($kind, $name, $path, $mtime, $size) = @$d;
    if ($kind eq 'mod') {
      my $now = $resolve ? $resolve->($name) : undef;
      return 0 if defined($now) != defined($path);
      next unless defined $now;              # still unresolvable: as recorded
      return 0 unless $now eq $path;
    }
    return 0 unless defined $path;
    my @s = stat $path or return 0;
    return 0 unless $s[9] == $mtime && $s[7] == $size;
  }
  return 1;
}

sub _env_from_record {
  my ($rec) = @_;
  require Pl::Environment;
  my $env = Pl::Environment->new();
  $env->prototypes($rec->{prototypes});
  $env->pkg_prototypes($rec->{pkg_prototypes} || {});
  $env->export_names({ map { ($_ => 1) } @{ $rec->{export_names} || [] } });
  # get_prototype resolves an unqualified name in the CURRENT package when a
  # bare name has competing declarations, so the walk's end state is part of
  # the record.
  $env->package_stack([ $rec->{current_package} // 'main' ]);
  return $env;
}

=head2 store($module, $path, $env, $frame)

Write the facts C<$env> carries for C<$module>, where C<$frame> is what
C<end_walk> returned.  ONE place decides whether an entry may be written, so
"this walk is not a fact about the module" is asked once: a tainted frame
leaves no entry (and is counted, because a taint that fired everywhere would
mean the cache does nothing).  Never fatal: a record that does not serialise
(or a cache directory that cannot be written) simply leaves no entry either.

=cut

sub store {
  my ($module, $path, $env, $frame) = @_;
  return unless enabled();
  my $deps = $frame->{deps} || [];
  if ($frame->{taint}) {
    $STATS{taint}++;
    push @{ $STATS{tainted} }, $module;
    return;
  }
  my @s = stat $path or return;
  my $key = _key($path, $s[9], $s[7]);
  my $rec = {
    v               => 1,
    module          => $module,
    path            => $path,
    mtime           => $s[9],
    size            => $s[7],
    gen             => generation(),
    key             => $key,
    current_package => $env->current_package,
    prototypes      => $env->prototypes,
    pkg_prototypes  => $env->pkg_prototypes,
    export_names    => [ sort keys %{ $env->export_names // {} } ],
    deps            => $deps || [],
  };
  my $json = eval { _json()->encode($rec) };
  return unless defined $json;
  _atomic_write(_entry_path($module, $key), $json);
  $STATS{store}++;
  return;
}

sub _atomic_write {
  my ($file, $bytes) = @_;
  my $dir = cache_dir();
  if (!-d $dir) {
    require File::Path;
    eval { File::Path::make_path($dir) };
    return unless -d $dir;
  }
  my $tmp = "$file.$$." . int(rand 1_000_000);
  open my $o, '>:raw', $tmp or return;
  my $ok = print {$o} $bytes;
  $ok &&= close $o;
  if (!$ok) { unlink $tmp; return }
  # temp + rename: many transpiles run at once (prove -j8, the census), and a
  # reader must never see a half-written entry.
  rename($tmp, $file) or unlink $tmp;
  return;
}

=head2 clear()

Remove every entry (used by C<pcl --clear-cache>).  Returns the count.

=cut

sub clear {
  my $dir = cache_dir();
  return 0 unless -d $dir;
  opendir my $dh, $dir or return 0;
  my @f = map { "$dir/$_" } grep { /\.json/ } readdir $dh;
  closedir $dh;
  return @f ? unlink(@f) : 0;
}

1;
