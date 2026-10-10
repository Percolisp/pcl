# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for File::Glob (task #3160; docs/shipped-modules.md
# "Facts overlays").  Declarations only: PCL reads this beside the module's
# source; the module (lib/File/Glob.pm, PCL's shim) still runs and installs
# the real subs.
package File::Glob;

# The compiler does not walk File::* modules for their prototypes (a cost
# measure), so the shim's `()` prototypes do not reach a caller: without
# them `GLOB_BRACE + 0 > 0 ? ...` reads as GLOB_BRACE(+0 > 0 ? ...) and
# swallows the rest of the argument list (probed).  A `()` prototype is a
# PARSE fact, so it is stated here.
sub GLOB_ABEND ();
sub GLOB_ALPHASORT ();
sub GLOB_ALTDIRFUNC ();
sub GLOB_BRACE ();
sub GLOB_CSH ();
sub GLOB_ERR ();
sub GLOB_LIMIT ();
sub GLOB_MARK ();
sub GLOB_NOCASE ();
sub GLOB_NOCHECK ();
sub GLOB_NOMAGIC ();
sub GLOB_NOSORT ();
sub GLOB_NOSPACE ();
sub GLOB_QUOTE ();
sub GLOB_TILDE ();

# `:bsd_glob` makes the CALLER's glob File::Glob's (perl's import assigns
# *{caller."::glob"}); listing `glob` in that tag is what tells the
# compiler the caller's glob is no longer the builtin.
our @EXPORT_OK = qw(
    GLOB_ABEND GLOB_ALPHASORT GLOB_ALTDIRFUNC GLOB_BRACE GLOB_CSH GLOB_ERR
    GLOB_ERROR GLOB_LIMIT GLOB_MARK GLOB_NOCASE GLOB_NOCHECK GLOB_NOMAGIC
    GLOB_NOSORT GLOB_NOSPACE GLOB_QUOTE GLOB_TILDE bsd_glob csh_glob glob
);
our %EXPORT_TAGS = (
    glob => [qw(
        GLOB_ABEND GLOB_ALPHASORT GLOB_ALTDIRFUNC GLOB_BRACE GLOB_CSH GLOB_ERR
        GLOB_ERROR GLOB_LIMIT GLOB_MARK GLOB_NOCASE GLOB_NOCHECK GLOB_NOMAGIC
        GLOB_NOSORT GLOB_NOSPACE GLOB_QUOTE GLOB_TILDE bsd_glob
    )],
    bsd_glob => [qw(
        GLOB_ABEND GLOB_ALPHASORT GLOB_ALTDIRFUNC GLOB_BRACE GLOB_CSH GLOB_ERR
        GLOB_ERROR GLOB_LIMIT GLOB_MARK GLOB_NOCASE GLOB_NOCHECK GLOB_NOMAGIC
        GLOB_NOSORT GLOB_NOSPACE GLOB_QUOTE GLOB_TILDE bsd_glob glob
    )],
);

1;
