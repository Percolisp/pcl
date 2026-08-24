# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package English;

# PCL shim for core English.pm (task #502).
#
# WHY A SHIM.  Core English.pm aliases every English name to its punctuation
# variable with a GLOB-to-glob assignment whose right-hand side is a
# punctuation glob:
#
#     *LAST_PAREN_MATCH = *+ ;
#
# PCL cannot lower `*+` (the glob-VALUE family, tasks #463 items 3-5), so the
# whole module died at transpile and every English name was unreachable -- the
# repro was `use English; print defined($ORS) ? "d" : "u";`.  A module's
# behaviour belongs in lib/ (CLAUDE.md rule 9a), and PCL already has two
# mechanisms that express what English needs, so this file supplies the same
# aliases through them:
#
#   *NAME = \$PUNCT       a SCALAR-slot alias.  Live in both directions --
#                         `$ORS = "!"` changes `$\` and vice versa -- for every
#                         punctuation variable PCL keeps in an ordinary cell,
#                         which is most of them.  Probed name by name vs perl.
#
#   tie $NAME, ...        for the six that are NOT cells in PCL and so cannot
#                         be aliased by value:
#                           $&  $`  $'  $+  $^N   the runtime rebinds these
#                                                 raw globals on every match
#                                                 (set-match-vars), so a
#                                                 scalar-ref alias freezes the
#                                                 value at load time;
#                           $!                    is not a variable at all --
#                                                 it is a call into C errno.
#                         FETCH reads the punctuation variable at each access,
#                         which is what perl's shared glob gives for free.
#                         $ARG is tied for the same reason: perl's `*ARG = *_`
#                         tracks the DYNAMIC $_ that foreach/map/grep bind,
#                         and `\$_` (in perl too) does not.
#
# KNOWN DIVERGENCE, documented in docs/not-supported.md and owned by a task:
# @ARG is not the running sub's @_.  perl gets that from the shared glob; PCL
# binds @_ per call, no pure-Perl mechanism can reach the caller's copy, and a
# tied array's FETCH runs in its own frame.  @ARG is aliased to the file-scope
# @_ here, which is what perl's @main::_ holds outside a sub (empty).
#
# Everything else -- @MINIMAL_EXPORT/@MATCH_EXPORT/@COMPLETE_EXPORT, the
# -no_match_vars import switch, the `$NAME`-in-the-import-list grandfathering
# -- follows core English.pm 1.11.

our $VERSION = '1.11';

require Exporter;
our @ISA = qw(Exporter);

no warnings;

# ------------------------------------------------------------------
# Tie classes for the variables PCL does not hold in an ordinary cell.
# One class per variable: FETCH is on the read path of every access, so it
# reads its variable directly rather than dispatching on a stored tag.
# ------------------------------------------------------------------

sub _readonly {
    require Carp;
    Carp::croak("Modification of a read-only value attempted");
}

{   package English::_Match;
    sub TIESCALAR { my $class = shift; my $x = 0; return bless \$x, $class }
    sub FETCH     { return $& }
    sub STORE     { English::_readonly() }
}
{   package English::_Prematch;
    sub TIESCALAR { my $class = shift; my $x = 0; return bless \$x, $class }
    sub FETCH     { return $` }
    sub STORE     { English::_readonly() }
}
{   package English::_Postmatch;
    sub TIESCALAR { my $class = shift; my $x = 0; return bless \$x, $class }
    sub FETCH     { return $' }
    sub STORE     { English::_readonly() }
}
{   package English::_LastParen;
    sub TIESCALAR { my $class = shift; my $x = 0; return bless \$x, $class }
    sub FETCH     { return $+ }
    sub STORE     { English::_readonly() }
}
{   package English::_LastSubmatch;
    sub TIESCALAR { my $class = shift; my $x = 0; return bless \$x, $class }
    sub FETCH     { return $^N }
    sub STORE     { English::_readonly() }
}
{   package English::_Errno;
    # $! is a call into C errno, not a cell -- and it is writable, so STORE
    # assigns through.  $^E is the same variable on POSIX (probed: perl gives
    # the identical string), which is how EXTENDED_OS_ERROR is served here.
    sub TIESCALAR { my $class = shift; my $x = 0; return bless \$x, $class }
    sub FETCH     { return $! }
    sub STORE     { $! = $_[1]; return }
}
{   package English::_Arg;
    # $_ is a cell, but foreach/map/grep BIND it dynamically, so a value alias
    # would miss every loop.  FETCH/STORE see the binding in effect, which is
    # what perl's shared *_ glob gives.
    sub TIESCALAR { my $class = shift; my $x = 0; return bless \$x, $class }
    sub FETCH     { return $_ }
    sub STORE     { $_ = $_[1]; return }
}

tie $ARG,                  'English::_Arg';
tie $MATCH,                'English::_Match';
tie $PREMATCH,             'English::_Prematch';
tie $POSTMATCH,            'English::_Postmatch';
tie $LAST_PAREN_MATCH,     'English::_LastParen';
tie $LAST_SUBMATCH_RESULT, 'English::_LastSubmatch';
tie $OS_ERROR,             'English::_Errno';
tie $ERRNO,                'English::_Errno';
tie $EXTENDED_OS_ERROR,    'English::_Errno';

# ------------------------------------------------------------------
# Scalar-slot aliases -- live both ways.
# ------------------------------------------------------------------

# The ground of all being: @ARG.  ($ARG is tied above.)

	*ARG					= \@_	;

# Matching.

	*LAST_MATCH_START			= \@-	;
	*LAST_MATCH_END				= \@+	;

# Input.

	*INPUT_LINE_NUMBER			= \$.	;
	    *NR					= \$.	;
	*INPUT_RECORD_SEPARATOR			= \$/	;
	    *RS					= \$/	;

# Output.

	*OUTPUT_AUTOFLUSH			= \$|	;
	*OUTPUT_FIELD_SEPARATOR			= \$,	;
	    *OFS				= \$,	;
	*OUTPUT_RECORD_SEPARATOR		= \$\	;
	    *ORS				= \$\	;

# Interpolation "constants".

	*LIST_SEPARATOR				= \$"	;
	*SUBSCRIPT_SEPARATOR			= \$;	;
	    *SUBSEP				= \$;	;

# Formats.

	*FORMAT_PAGE_NUMBER			= \$%	;
	*FORMAT_LINES_PER_PAGE			= \$=	;
	*FORMAT_LINES_LEFT			= \$-	;
	*FORMAT_NAME				= \$~	;
	*FORMAT_TOP_NAME			= \$^	;
	*FORMAT_LINE_BREAK_CHARACTERS		= \$:	;
	*FORMAT_FORMFEED			= \$^L	;

# Error status.  ($OS_ERROR/$ERRNO/$EXTENDED_OS_ERROR are tied above.)

	*CHILD_ERROR				= \$?	;
	*EVAL_ERROR				= \$@	;

# Process info.

	*PROCESS_ID				= \$$	;
	    *PID				= \$$	;
	*REAL_USER_ID				= \$<	;
	    *UID				= \$<	;
	*EFFECTIVE_USER_ID			= \$>	;
	    *EUID				= \$>	;
	*REAL_GROUP_ID				= \$(	;
	    *GID				= \$(	;
	*EFFECTIVE_GROUP_ID			= \$)	;
	    *EGID				= \$)	;
	*PROGRAM_NAME				= \$0	;

# Internals.

	*PERL_VERSION				= \$^V	;
	*OLD_PERL_VERSION			= \$]	;
	*ACCUMULATOR				= \$^A	;
	*DEBUGGING				= \$^D	;
	*SYSTEM_FD_MAX				= \$^F	;
	*INPLACE_EDIT				= \$^I	;
	*PERLDB					= \$^P	;
	*LAST_REGEXP_CODE_RESULT		= \$^R	;
	*EXCEPTIONS_BEING_CAUGHT		= \$^S	;
	*BASETIME				= \$^T	;
	*WARNING				= \$^W	;
	*EXECUTABLE_NAME			= \$^X	;
	*OSNAME					= \$^O	;

# $^C (COMPILING) has no cell in PCL, and PCL has no compile-only mode, so the
# value perl reports at run time -- 0 -- is the only one it can ever hold.
# (A program that names $^C directly still dies unbound; that is PCL's gap,
# filed separately, not English's to paper over.)

our $COMPILING = 0;

# ------------------------------------------------------------------
# Exports -- core English.pm's lists, verbatim.
# ------------------------------------------------------------------

our @MINIMAL_EXPORT = qw(
	*ARG
	*LAST_PAREN_MATCH
	*INPUT_LINE_NUMBER
	*NR
	*INPUT_RECORD_SEPARATOR
	*RS
	*OUTPUT_AUTOFLUSH
	*OUTPUT_FIELD_SEPARATOR
	*OFS
	*OUTPUT_RECORD_SEPARATOR
	*ORS
	*LIST_SEPARATOR
	*SUBSCRIPT_SEPARATOR
	*SUBSEP
	*FORMAT_PAGE_NUMBER
	*FORMAT_LINES_PER_PAGE
	*FORMAT_LINES_LEFT
	*FORMAT_NAME
	*FORMAT_TOP_NAME
	*FORMAT_LINE_BREAK_CHARACTERS
	*FORMAT_FORMFEED
	*CHILD_ERROR
	*OS_ERROR
	*ERRNO
	*EXTENDED_OS_ERROR
	*EVAL_ERROR
	*PROCESS_ID
	*PID
	*REAL_USER_ID
	*UID
	*EFFECTIVE_USER_ID
	*EUID
	*REAL_GROUP_ID
	*GID
	*EFFECTIVE_GROUP_ID
	*EGID
	*PROGRAM_NAME
	*PERL_VERSION
	*OLD_PERL_VERSION
	*ACCUMULATOR
	*COMPILING
	*DEBUGGING
	*SYSTEM_FD_MAX
	*INPLACE_EDIT
	*PERLDB
	*BASETIME
	*WARNING
	*EXECUTABLE_NAME
	*OSNAME
	*LAST_REGEXP_CODE_RESULT
	*EXCEPTIONS_BEING_CAUGHT
	*LAST_SUBMATCH_RESULT
	@LAST_MATCH_START
	@LAST_MATCH_END
);

our @MATCH_EXPORT = qw(
	*MATCH
	*PREMATCH
	*POSTMATCH
);

our @COMPLETE_EXPORT = ( @MINIMAL_EXPORT, @MATCH_EXPORT );

# Grandfather $NAME import, and honour -no_match_vars.
sub import {
    my $this = shift;
    my @list = grep { ! /^-no_match_vars$/ } @_ ;
    local $Exporter::ExportLevel = 1;
    if ( @_ == @list ) {
        *EXPORT = \@COMPLETE_EXPORT ;
    }
    else {
        *EXPORT = \@MINIMAL_EXPORT ;
    }
    # Core spells this `grep {s/^\$/*/} @list`, whose filter keeps exactly the
    # entries that HAD a `$` sigil; written out so it does not depend on
    # grep's aliasing of $_ to the list's elements.
    my @syms;
    for my $s (@list) {
        my $t = $s;
        next unless $t =~ s/^\$/*/;
        push @syms, $t;
    }
    Exporter::import($this, @syms);
}

1;
