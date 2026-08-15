# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

#
# Pure-Perl shim for Try::Tiny (upstream v0.32).  The real module runs
# `finally` blocks from Try::Tiny::ScopeGuard::DESTROY at scope exit; PCL has
# no GC-driven DESTROY (docs/not-supported.md §DESTROY called by garbage
# collector), so under PCL the finallys silently never ran.  This shim keeps
# the module's whole observable contract but calls the finally blocks
# DIRECTLY at the points the guard objects would have died:
#   - try succeeded:        after the try block, no arguments
#   - try failed:           after the catch block (or immediately if none),
#                           with the ORIGINAL error as the one argument
#   - catch itself died:    finallys still run (originial error as argument),
#                           then the catch's error propagates
# An exception inside a finally is warned, never propagated — same as the
# upstream DESTROY guard.  The Sub::Name/Sub::Util renaming dance is dropped:
# names are cosmetic (caller()/debugger only) and set_subname is a no-op
# rename under PCL anyway.
#
# KNOWN LIMIT (one row in the dist's finally.t): a NON-LOCAL EXIT out of the
# try block that is neither a return nor a die — e.g. Test::More's skip()
# doing `last SKIP` — leaves try() without reaching the direct finally calls.
# Only a scope-guard DESTROY can observe that unwind; accepting the gap is
# the point of this shim.

package Try::Tiny;
use 5.006;
our $VERSION = '0.32';
use strict;
use warnings;
use Exporter 5.57 'import';
our @EXPORT = our @EXPORT_OK = qw(try catch finally);
use Carp;
$Carp::Internal{+__PACKAGE__}++;

sub _run_finally {
  my ($finally, @args) = @_;
  for my $code (@$finally) {
    eval {
      $code->(@args);
      1;
    } or do {
      warn
        "Execution of finally() block $code resulted in an exception, which "
      . '*CAN NOT BE PROPAGATED* due to fundamental limitations of Perl. '
      . 'Your program will continue as if this event never took place. '
      . "Original exception text follows:\n\n"
      . (defined $@ ? $@ : '$@ left undefined...')
      . "\n"
      ;
    };
  }
  return;
}

sub try (&;@) {
  my ( $try, @code_refs ) = @_;

  my $wantarray = wantarray;
  my ( $catch, @finally ) = ();
  foreach my $code_ref (@code_refs) {
    if ( ref($code_ref) eq 'Try::Tiny::Catch' ) {
      croak 'A try() may not be followed by multiple catch() blocks'
        if $catch;
      $catch = ${$code_ref};
    } elsif ( ref($code_ref) eq 'Try::Tiny::Finally' ) {
      push @finally, ${$code_ref};
    } else {
      croak(
        'try() encountered an unexpected argument ('
      . ( defined $code_ref ? $code_ref : 'undef' )
      . ') - perhaps a missing semi-colon before or'
      );
    }
  }

  my $prev_error = $@;
  my ( @ret, $error );

  my $failed = not eval {
    $@ = $prev_error;
    if ( $wantarray ) {
      @ret = $try->();
    } elsif ( defined $wantarray ) {
      $ret[0] = $try->();
    } else {
      $try->();
    };
    return 1;
  };
  $error = $@;
  $@ = $prev_error;

  # Every eval{} in here (the catch wrapper, _run_finally's guard) CLEARS $@
  # on success, but the caller must see $@ exactly as it was before try()
  # ("$@ untouched", basic.t) — so each non-die exit restores it last.
  if ( !$failed ) {
    _run_finally(\@finally);
    $@ = $prev_error;
    return $wantarray ? @ret : $ret[0];
  }

  # try failed; catch (if any) runs in try()'s own context, then the
  # finallys, then the catch's results (or its own death) surface.
  if ( $catch ) {
    my @cret;
    my $catch_failed = not eval {
      $@ = $prev_error;
      for ($error) {
        if ( $wantarray ) {
          @cret = $catch->($error);
        } elsif ( defined $wantarray ) {
          $cret[0] = $catch->($error);
        } else {
          $catch->($error);
        }
      }
      return 1;
    };
    my $catch_error = $@;
    _run_finally(\@finally, $error);
    die $catch_error if $catch_failed;
    $@ = $prev_error;
    return $wantarray ? @cret : $cret[0];
  }

  _run_finally(\@finally, $error);
  $@ = $prev_error;
  return;
}

sub catch (&;@) {
  my ( $block, @rest ) = @_;

  croak 'Useless bare catch()' unless wantarray;

  return (
    bless(\$block, 'Try::Tiny::Catch'),
    @rest,
  );
}

sub finally (&;@) {
  my ( $block, @rest ) = @_;

  croak 'Useless bare finally()' unless wantarray;

  return (
    bless(\$block, 'Try::Tiny::Finally'),
    @rest,
  );
}

1;
