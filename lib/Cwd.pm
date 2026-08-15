# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

#
# Cwd — PCL shim (task #166).
#
# Before this existed, `use Cwd;` and `require Cwd;` both DIED under PCL even
# though cwd() was already a builtin, so any module reaching for Cwd — which is
# most of CPAN, and File::Spec::Unix's own rel2abs — hit a wall.
#
# cwd/getcwd delegate to the builtins.  abs_path/realpath are implemented HERE,
# in Perl, on top of readlink/-l/-e: that is the layer rule (CLAUDE.md 9a —
# module behaviour lives in lib/<Module>.pm, never in the runtime), and the
# primitives it needs already exist.  They are deliberately NOT aliased to
# cwd(): abs_path must resolve symlinks, and a shim that silently returned an
# unresolved path would be worse than a missing one.
#
# Semantics verified against real perl 5.40.3 (Cwd 3.75):
#   abs_path(".")            -> cwd
#   abs_path("link")         -> the symlink's target, resolved
#   abs_path("link/f")       -> intermediate symlinks resolved too
#   abs_path("abslink")      -> an ABSOLUTE target replaces everything so far
#   abs_path("real/../real") -> ".." resolved
#   abs_path("real/newfile") -> OK: only the DIRECTORY part must exist
#   abs_path("nope/x")       -> undef: a missing directory component fails
#   abs_path("") / abs_path()-> cwd
#   abs_path("/")            -> "/"

package Cwd;
use strict;

our @EXPORT    = qw(cwd getcwd fastcwd fastgetcwd);
our @EXPORT_OK = qw(abs_path fast_abs_path realpath fast_realpath getdcwd);
our %EXPORT_TAGS = ( DEFAULT => \@EXPORT, ALL => [@EXPORT, @EXPORT_OK] );

# No custom import: PCL imports @EXPORT / a requested subset automatically.

# cwd() and getcwd() are PCL builtins; Cwd's are the same thing.  The "fast"
# variants are aliases in the real module too (no faster path exists on POSIX).
sub cwd        { return CORE::cwd(); }
sub getcwd     { return CORE::cwd(); }
sub fastcwd    { return CORE::cwd(); }
sub fastgetcwd { return CORE::cwd(); }

# getdcwd([DRIVE]) is a Windows/VMS concept; on POSIX perl returns the cwd.
sub getdcwd { return CORE::cwd(); }

sub abs_path {
    my ($path) = @_;
    $path = '.' if !defined $path || $path eq '';
    $path = CORE::cwd() . '/' . $path if $path !~ m{^/};

    # "." components carry no information; "" comes from doubled slashes.
    my @parts = grep { length($_) && $_ ne '.' } split(m{/+}, $path);
    my @out;
    my $links = 0;

    while (@parts) {
        my $p = shift @parts;
        if ($p eq '..') { pop @out; next }

        my $cand = '/' . join('/', @out, $p);

        if (-l $cand) {
            # A symlink loop must terminate rather than hang.  perl uses the
            # system limit; 32 is the usual ELOOP threshold.
            if (++$links > 32) { return undef }
            my $target = readlink($cand);
            return undef if !defined $target;
            @out = () if $target =~ m{^/};   # absolute target restarts the walk
            unshift @parts, grep { length($_) && $_ ne '.' } split(m{/+}, $target);
            next;
        }

        push @out, $p;

        # Only the DIRECTORY part has to exist: abs_path("existing/newfile") is
        # a valid answer, abs_path("missing/x") is undef.
        if (@parts && ! -e $cand) { return undef }
    }

    return '/' . join('/', @out);
}

sub realpath      { return abs_path($_[0]); }
sub fast_abs_path { return abs_path($_[0]); }
sub fast_realpath { return abs_path($_[0]); }

1;
