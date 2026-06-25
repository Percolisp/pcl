# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package File::Spec;
use strict;

# ── WHY THIS IS A SHIM (not because File::Spec is XS — it is pure Perl) ──────
#
# The real File/Spec.pm is only an OS dispatcher:
#
#     my $module = $module{$^O} || 'Unix';
#     require "File/Spec/$module.pm";          # runtime, $^O-interpolated path
#     our @ISA = ("File::Spec::$module");       # inherit the OS-specific class
#
# All the real methods live in File::Spec::Unix (catfile/catdir/catpath/
# splitpath/…).  Two things historically blocked transpiling the real module —
# BOTH are now FIXED (session 256):
#   1. require "File/Spec/$module.pm" — string require with an interpolated,
#      runtime-only path.  (String require via @INC: session 255; interpolation
#      of the require *argument*: session 256.)
#   2. our @ISA = ("File::Spec::$module") — an interpolated, runtime-only parent
#      class name.  _process_isa_declaration now keeps interpolated @ISA
#      elements OUT of the compile-time CLOS defclass and pushes them onto @ISA
#      at run time, where %pcl-isa-ancestry resolves them (session 256).
#      Verified: with this shim REMOVED, `require File::Spec; File::Spec->catfile`
#      loads File::Spec::Unix and returns "a/b/c".
#
# WHY THE SHIM STILL EXISTS (deferred removal): dropping it makes File::Spec
# resolve to the real File::Spec::Unix repo-wide (unknown blast radius), and the
# module that motivated this — Class::Inspector — is still blocked by a SEPARATE
# bug: a block-scoped `package Baz; @ISA=...` CLOS "Not a legal superclass name"
# crash (same family as the Safe::Isa FINALIZE-INHERITANCE crash), NOT by
# File::Spec.  So removal is now safe-to-attempt but deferred until (a) that
# CLOS crash is fixed and (b) a full CPAN re-survey confirms the real
# File::Spec::Unix runs cleanly under PCL.  See docs/session-log.md (session
# 256).  Until then this is a Unix-only subset; keep it in sync with the real
# File::Spec::Unix as methods are needed.

sub catfile {
    my $class = shift if @_ > 0 && !ref($_[0]) && $_[0] eq 'File::Spec';
    my @parts = @_;
    return join('/', @parts);
}

sub catdir {
    my $class = shift if @_ > 0 && !ref($_[0]) && $_[0] eq 'File::Spec';
    my @parts = grep { $_ ne '' } @_;
    return '/' if @parts == 0;
    my $path = join('/', @parts);
    $path =~ s{//+}{/}g;
    return $path;
}

sub splitdir {
    my ($class, $path) = @_;
    $path = $class unless defined $path;  # handle non-OO call
    return split(/\//, $path, -1);
}

sub splitpath {
    my ($class, $path, $no_file) = @_;
    $no_file = 0 unless defined $no_file;
    # Unix has no volume; $no_file means the whole path is the directory.
    return ('', $path, '') if $no_file;
    if ($path =~ m{^(.*/)([^/]*)$}) {
        return ('', $1, $2);
    }
    return ('', '', $path);
}

# catpath($volume, $directory, $file) — join a split path back together.
# Mirrors File::Spec::Unix::catpath (volume is ignored on Unix).
sub catpath {
    my ($class, $volume, $directory, $file) = @_;
    if ($directory ne '' && $file ne ''
        && substr($directory, -1) ne '/'
        && substr($file, 0, 1) ne '/') {
        $directory .= "/$file";
    } else {
        $directory .= $file;
    }
    return $directory;
}

sub rel2abs {
    my ($class, $path, $base) = @_;
    return $path if $path =~ m{^/};
    unless (defined $base) {
        require Cwd;            # real File::Spec::Unix loads Cwd lazily here too
        $base = Cwd::cwd();
    }
    return $base . '/' . $path;
}

sub curdir  { return '.'; }
sub updir   { return '..'; }
sub rootdir { return '/'; }
sub devnull { return '/dev/null'; }

sub tmpdir {
    # File::Spec::Unix prefers $ENV{TMPDIR} (when it is a usable absolute dir),
    # falling back to /tmp.  A simplified, deterministic version is enough here.
    my $t = $ENV{TMPDIR};
    return $t if defined $t && length $t && $t =~ m{^/};
    return '/tmp';
}

sub file_name_is_absolute {
    my ($class, $path) = @_;
    return $path =~ m{^/} ? 1 : 0;
}

sub no_upwards {
    my $class = shift;
    return grep { $_ ne '.' && $_ ne '..' } @_;
}

sub path {
    return split(/:/, $ENV{PATH} // '');
}

1;
