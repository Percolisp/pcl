# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# File::Glob without XS (task #3160, batch s514e).  The real module is XS, so
# `use File::Glob` died "Can't locate loadable object" -- and with it
# File::GlobMapper, and so Compress::Zlib, IO::Compress::Gzip and
# IO::Uncompress::Gunzip, which all load it.
#
# perl's CORE::glob IS bsd_glob(3) run with the csh flags, and PCL's runtime
# has that ONE engine (builtin::bsd_glob, docs/ir-spec.md "The glob engine").
# This file owns only File::Glob's DATA and surface: the GLOB_* numbers, the
# export tags, the flag word -> engine flag names, GLOB_ERROR, the
# `:bsd_glob` / `:globally` / `:case` / `:nocase` import tags, and the
# scalar-context iterator of csh_glob / the `:bsd_glob` override.
#
# NOT IMPLEMENTED, and loud about it: GLOB_ALTDIRFUNC (it needs C function
# pointers; nothing in Perl can supply them) and GLOB_LIMIT die naming
# themselves when a flag word carries them.  The scalar-context iterator is
# keyed by PATTERN, as CORE::glob's is in PCL (perl keys it by call site, #489).

package File::Glob;
use strict;
our ($DEFAULT_FLAGS, $ERROR);

our $VERSION = '1.42';

sub GLOB_ABEND ()       { -2 }
sub GLOB_ALPHASORT ()   { 0x2000 }
sub GLOB_ALTDIRFUNC ()  { 0x0040 }
sub GLOB_BRACE ()       { 0x0080 }
sub GLOB_CSH ()         { 0x2e80 }
sub GLOB_ERR ()         { 0x0004 }
sub GLOB_LIMIT ()       { 0x4000 }
sub GLOB_MARK ()        { 0x0008 }
sub GLOB_NOCASE ()      { 0x1000 }
sub GLOB_NOCHECK ()     { 0x0010 }
sub GLOB_NOMAGIC ()     { 0x0200 }
sub GLOB_NOSORT ()      { 0x0020 }
sub GLOB_NOSPACE ()     { -1 }
sub GLOB_QUOTE ()       { 0x0400 }
sub GLOB_TILDE ()       { 0x0800 }

sub GLOB_ERROR { $ERROR || 0 }

our %EXPORT_TAGS = (
    'glob' => [ qw(
        GLOB_ABEND GLOB_ALPHASORT GLOB_ALTDIRFUNC GLOB_BRACE GLOB_CSH GLOB_ERR
        GLOB_ERROR GLOB_LIMIT GLOB_MARK GLOB_NOCASE GLOB_NOCHECK GLOB_NOMAGIC
        GLOB_NOSORT GLOB_NOSPACE GLOB_QUOTE GLOB_TILDE bsd_glob
    ) ],
    # perl's tag is the same list; `glob` is the override `:bsd_glob` installs
    # in the caller (perl assigns *{caller."::glob"} in import, below) --
    # naming it here as well is what tells PCL's compiler, which reads export
    # lists, that the caller's glob is no longer the builtin.
    'bsd_glob' => [ qw(
        GLOB_ABEND GLOB_ALPHASORT GLOB_ALTDIRFUNC GLOB_BRACE GLOB_CSH GLOB_ERR
        GLOB_ERROR GLOB_LIMIT GLOB_MARK GLOB_NOCASE GLOB_NOCHECK GLOB_NOMAGIC
        GLOB_NOSORT GLOB_NOSPACE GLOB_QUOTE GLOB_TILDE bsd_glob glob
    ) ],
);

our @EXPORT_OK = qw(
    GLOB_ABEND GLOB_ALPHASORT GLOB_ALTDIRFUNC GLOB_BRACE GLOB_CSH GLOB_ERR
    GLOB_ERROR GLOB_LIMIT GLOB_MARK GLOB_NOCASE GLOB_NOCHECK GLOB_NOMAGIC
    GLOB_NOSORT GLOB_NOSPACE GLOB_QUOTE GLOB_TILDE bsd_glob csh_glob glob
);

# The flag bits the engine implements, and the name it knows each by.
my @FLAG_NAMES = (
    [GLOB_BRACE,     'brace'],     [GLOB_NOCASE,  'nocase'],
    [GLOB_MARK,      'mark'],      [GLOB_NOCHECK, 'nocheck'],
    [GLOB_NOMAGIC,   'nomagic'],   [GLOB_QUOTE,   'quote'],
    [GLOB_TILDE,     'tilde'],     [GLOB_NOSORT,  'nosort'],
    [GLOB_ALPHASORT, 'alphasort'], [GLOB_ERR,     'err'],
);

sub _flag_names {
    my ($flags) = @_;
    my @names;
    for my $f (@FLAG_NAMES) {
        next if !($flags & $f->[0]);
        push @names, $f->[1];
        $flags &= ~$f->[0];
    }
    die "File::Glob: GLOB_ALTDIRFUNC is not supported by PCL\n"
        if $flags & GLOB_ALTDIRFUNC;
    die "File::Glob: GLOB_LIMIT is not supported by PCL\n"
        if $flags & GLOB_LIMIT;
    die sprintf("File::Glob: unknown flag bits 0x%x\n", $flags) if $flags;
    return @names;
}

sub _run {
    my ($pat, @names) = @_;
    my @r = builtin::bsd_glob($pat, join(",", @names));
    $ERROR = shift @r;
    return @r;
}

sub bsd_glob {
    my ($pat, $flags) = @_;
    $flags = $DEFAULT_FLAGS if @_ < 2;
    my @r = _run($pat, _flag_names($flags));
    # perl's XS returns its LIST: scalar context sees the LAST element
    # (undef for none), never the count (#3246).
    return wantarray ? @r : $r[-1];
}

# csh_glob: perl's CORE::glob -- the pattern is a whitespace-separated LIST.
my %ITER;
sub _iterate {
    my ($key, $list) = @_;
    if (!exists $ITER{$key}) {
        $ITER{$key} = [ $list->() ];
    }
    my $l = $ITER{$key};
    if (!@$l) {
        delete $ITER{$key};
        return undef;
    }
    return shift @$l;
}

sub csh_glob {
    my $pat = @_ ? $_[0] : $_;
    my $all = sub { _run($pat, 'words', _flag_names($DEFAULT_FLAGS)) };
    return $all->() if wantarray;
    return _iterate("csh\0$pat", $all);
}

# What `use File::Glob ':bsd_glob'` makes the caller's glob: ONE pattern,
# spaces and all, with the default flags.
sub bsd_glob_override {
    my $pat = @_ ? $_[0] : $_;
    my $all = sub { bsd_glob($pat, $DEFAULT_FLAGS) };
    return $all->() if wantarray;
    return _iterate("bsd\0$pat", $all);
}
*glob = \&bsd_glob_override;

sub import {
    require Exporter;
    local $Exporter::ExportLevel = $Exporter::ExportLevel + 1;
    Exporter::import(grep {
        my $passthrough;
        if ($_ eq ':case') {
            $DEFAULT_FLAGS &= ~GLOB_NOCASE();
        }
        elsif ($_ eq ':nocase') {
            $DEFAULT_FLAGS |= GLOB_NOCASE();
        }
        elsif ($_ eq ':globally') {
            no warnings 'redefine';
            *CORE::GLOBAL::glob = \&File::Glob::csh_glob;
        }
        elsif ($_ eq ':bsd_glob') {
            no strict;
            *{caller."::glob"} = \&bsd_glob_override;
            $passthrough = 1;
        }
        else {
            $passthrough = 1;
        }
        $passthrough;
    } @_);
}

$DEFAULT_FLAGS = GLOB_CSH();

1;
