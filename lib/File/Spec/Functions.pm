# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package File::Spec::Functions;
use strict;
use File::Spec;

our @EXPORT = qw(
    catfile catdir splitdir splitpath rel2abs curdir updir rootdir
    file_name_is_absolute no_upwards path devnull tmpdir
);
our @EXPORT_OK = @EXPORT;
our %EXPORT_TAGS = ( DEFAULT => \@EXPORT, ALL => \@EXPORT );

# No custom import: PCL imports @EXPORT / a requested subset (and :DEFAULT/:ALL
# tags) automatically.  A hand-rolled Exporter here would just duplicate that.

# EVERY function here is a thin wrapper over the File::Spec method of the same
# name — which is exactly what real File::Spec::Functions is.  It used to
# re-implement all thirteen, and the two copies DRIFTED APART in both
# directions (found s319, task #167):
#
#   * this file's splitpath IGNORED the $no_file flag for the directory part,
#     returning "/x/y/" for ("/x/y/t", 1) where perl returns "/x/y/t".  That is
#     not a cosmetic difference: t/op/chdir.t asks "is my cwd named t?" via
#     splitpath(...,1), so under PCL the answer was always no, its `skip
#     ("Already in t/", 2)` never fired, and PCL RAN two tests perl SKIPS —
#     two failures that were an artifact of the shim, not of PCL;
#   * File::Spec.pm's rel2abs returned "$base/." for rel2abs('.') and its
#     catdir() returned '/' instead of ''.
#
# Both copies were partly right, which is the reason for the rule: one
# implementation, one place to fix (CLAUDE.md 11).  File::Spec.pm is the home;
# corrections go there and arrive here for free.

sub catfile              { File::Spec->catfile(@_) }
sub catdir               { File::Spec->catdir(@_) }
sub splitdir             { File::Spec->splitdir(@_) }
sub splitpath            { File::Spec->splitpath(@_) }
sub rel2abs              { File::Spec->rel2abs(@_) }
sub curdir               { File::Spec->curdir }
sub updir                { File::Spec->updir }
sub rootdir              { File::Spec->rootdir }
sub devnull              { File::Spec->devnull }
sub tmpdir               { File::Spec->tmpdir }
sub file_name_is_absolute { File::Spec->file_name_is_absolute(@_) }
sub no_upwards           { File::Spec->no_upwards(@_) }
sub path                 { File::Spec->path }

1;
