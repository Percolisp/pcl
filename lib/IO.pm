# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package IO;

# PCL shim for core IO.pm.
#
# In perl, IO.pm is two things: a convenience loader (`use IO qw(Handle File)`)
# and `XSLoader::load 'IO'`, which installs the XS half of IO::Handle into that
# package.  PCL has no IO.so, so `use IO::Handle` — core's own pure-Perl file,
# which starts with `use IO ()` — died with
#   Can't locate loadable object for module IO in @INC
# and took every module that autoflushes or dups a handle with it (all 23 of
# Capture-Tiny's test files, task #197).
#
# This file supplies that XS half in plain Perl.  Core's IO/Handle.pm is then
# used UNCHANGED on top of it, which is the point: the shim is the missing data
# (the C-level subs), not a reimplementation of the module.
#
# Anything that cannot be expressed without the C layer croaks by name rather
# than returning a plausible value (CLAUDE.md rule 12) — a handle that reports
# "synced" without syncing is worse than one that says it cannot sync.

use strict;
use warnings;
use Carp ();

our $VERSION = "1.55";

sub import {
    shift;
    # Same loader as core: `use IO qw(Handle File)` pulls in IO::Handle,
    # IO::File.  Parameterless `use IO` is deprecated in perl but still loads
    # the historical set.
    my @l = @_ ? @_ : qw(Handle Seekable File Pipe Socket Dir);
    eval join("", map { "require IO::" . (/(\w+)/)[0] . ";\n" } @l)
      or Carp::croak($@);
}

package IO::Handle;

use Carp ();

#--------------------------------------------------------------------------#
# Constants (IO.xs supplies these; IO/Handle.pm re-exports them)
#--------------------------------------------------------------------------#

sub SEEK_SET () { 0 }
sub SEEK_CUR () { 1 }
sub SEEK_END () { 2 }

sub _IOFBF () { 0 }
sub _IOLBF () { 1 }
sub _IONBF () { 2 }

#--------------------------------------------------------------------------#
# Flushing and buffering
#--------------------------------------------------------------------------#

# The pre-XS pure-Perl flush, and still exact: assigning a true value to $|
# flushes the currently selected handle.  Returns perl's "0 but true".
sub flush {
    @_ == 1 or Carp::croak('usage: $io->flush()');
    my ($io) = @_;
    my $old  = select($io);
    my $prev = $|;
    $| = 1;
    $| = $prev;
    select($old);
    return "0 but true";
}

# setbuf/setvbuf select a buffering DISCIPLINE.  PCL exposes exactly one knob
# for that, $| (flush after every write), so the unbuffered and line-buffered
# modes map onto autoflush and the fully-buffered mode maps onto its absence.
# The buffer argument is perl's caller-supplied storage; PCL owns its buffers,
# so it is accepted and ignored — same observable behaviour, no C buffer.
sub setvbuf {
    @_ == 4 or Carp::croak('usage: $io->setvbuf(BUF, TYPE, SIZE)');
    my ($io, undef, $type, undef) = @_;
    $io->autoflush($type == _IOFBF() ? 0 : 1);
    return "0 but true";
}

sub setbuf {
    ( @_ == 1 or @_ == 2 ) or Carp::croak('usage: $io->setbuf([BUF])');
    my ($io) = @_;
    $io->autoflush(0);
    return "0 but true";
}

#--------------------------------------------------------------------------#
# Line input
#--------------------------------------------------------------------------#

sub getline {
    @_ == 1 or Carp::croak('usage: $io->getline()');
    my ($io) = @_;
    return scalar <$io>;
}

sub getlines {
    @_ == 1 or Carp::croak('usage: $io->getlines()');
    wantarray or Carp::croak('Can\'t call $io->getlines in a scalar context');
    my ($io) = @_;
    return <$io>;
}

# gets() is perl's deprecated spelling of getline().
sub gets {
    my ($io) = @_;
    return $io->getline;
}

#--------------------------------------------------------------------------#
# Error state
#--------------------------------------------------------------------------#

# perl latches a sticky error flag in the C FILE*; PCL has no such flag — a
# failed read/write reports itself at the call, through a false return and $!.
# So there is never a latched error to report or to clear, and both of these
# answer that honestly (0 = "no error", perl's own success value for clearerr).
sub error    { 0 }
sub clearerr { 0 }

# PCL does not implement taint mode, so no handle is ever tainted and there is
# nothing to untaint: 0 is perl's success value.
sub untaint { 0 }

#--------------------------------------------------------------------------#
# Not expressible without the C layer — these croak rather than lie
#--------------------------------------------------------------------------#

sub sync {
    # fsync(2).  PCL has no fsync, and a handle that claims durability it did
    # not provide is the worst possible answer.
    Carp::croak('IO::Handle::sync is not implemented under PCL (no fsync)');
}

sub blocking {
    # Needs fcntl(F_GETFL/F_SETFL), which PCL does not provide yet.
    Carp::croak('IO::Handle::blocking is not implemented under PCL (no fcntl)');
}

sub ungetc {
    Carp::croak('IO::Handle::ungetc is not implemented under PCL');
}

1;
