# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real Carp.pm cannot run under PCL (measured s499j: perl's own
# Carp.pm on PERL5LIB leaves `croak` undefined in the importer, task #2491).  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).

#
# PCL shim for Carp.  Since s539d (task #1361) it is perl's Carp 1.54
# ALGORITHM over PCL's caller(): croak / carp report the location of the
# first caller OUTSIDE the calling package (short_error_loc: %CarpInternal,
# %Internal, @CARP_NOT / @ISA trust, $CarpLevel), confess / cluck a
# backtrace from where the error happened (long_error_loc).  What it does not
# have, and why:
#   * the LOCATION is caller()'s, so it is perl's file and line in a program
#     compiled with `line-track' and PCL's placeholder `(eval 0) line 0'
#     otherwise -- the same text a runtime-raised die carries there
#     (docs/not-supported.md ns-caller-fidelity);
#   * a backtrace line names the sub WITHOUT its arguments: they come from
#     @DB::args, which PCL does not keep;
#   * no `, <FH> line N' tail ($. and ${^LAST_FH}), no CORE::GLOBAL::caller
#     override, no `thread N'.

package Carp;
use strict;
use warnings;
use Exporter 'import';

our @EXPORT      = qw(carp croak confess);
our @EXPORT_OK   = qw(cluck longmess shortmess verbose);
our @EXPORT_FAIL = qw(verbose);    # `use Carp qw(verbose)' turns $Verbose on

our $VERSION = '1.50';

our $MaxEvalLen = 0;
our $Verbose    = 0;
our $CarpLevel  = 0;
our $MaxArgLen  = 64;
our $MaxArgNums = 8;
our @CARP_NOT;

# Packages a short message never reports a location in: perl's own lists.
our %CarpInternal;
our %Internal;
$CarpInternal{Carp}++;
$CarpInternal{warnings}++;
$Internal{Exporter}++;
$Internal{'Exporter::Heavy'}++;

sub export_fail { shift; $Verbose = shift if $_[0] eq 'verbose'; @_ }

sub croak   { die shortmess(@_) }
sub confess { die longmess(@_) }
sub carp    { warn shortmess(@_) }
sub cluck   { warn longmess(@_) }

# The backwards-compatible wrappers: longmess skips one more level unless it
# was called from inside Carp; shortmess trusts the package that called it.
sub longmess {
    local ($!, $^E);
    my $call_pack = caller();
    if ($Internal{$call_pack} || $CarpInternal{$call_pack}) {
        return longmess_heavy(@_);
    }
    local $CarpLevel = $CarpLevel + 1;
    return longmess_heavy(@_);
}

sub shortmess {
    local ($!, $^E);
    local @CARP_NOT = scalar(caller());
    return shortmess_heavy(@_);
}

# A REFERENCE as the first argument is an exception OBJECT and is passed
# through untouched ("don't break references as exceptions"), so
# `croak { code => 1 }` dies with the hash itself (#3867).
sub longmess_heavy {
    return wantarray ? @_ : $_[0] if ref $_[0];
    my $i = long_error_loc();
    return ret_backtrace($i, @_);
}

sub shortmess_heavy {
    return longmess_heavy(@_) if $Verbose;
    return wantarray ? @_ : $_[0] if ref $_[0];
    my $i = short_error_loc();
    return $i ? ret_summary($i, @_) : longmess_heavy(@_);
}

# One frame of the stack as perl's Carp names it (caller_info + get_subname,
# without the arguments -- see the header).
sub caller_info {
    my $i = shift(@_) + 1;
    my %ci;
    @ci{qw(pack file line sub has_args wantarray evaltext is_require)} = caller($i);
    return () unless defined $ci{file};
    my $name;
    if (defined $ci{evaltext}) {
        if ($ci{is_require}) { $name = "require $ci{evaltext}" }
        else {
            (my $e = $ci{evaltext}) =~ s/([\\\'])/\\$1/g;
            $name = "eval '" . str_len_trim($e, $MaxEvalLen) . "'";
        }
    }
    elsif (!defined $ci{sub}) { $name = '__ANON__::__ANON__' }
    else { $name = $ci{sub} eq '(eval)' ? 'eval {...}' : $ci{sub} }
    $ci{sub_name} = $name;
    return wantarray ? %ci : \%ci;
}

sub str_len_trim {
    my ($str, $max) = @_;
    $max ||= 0;
    substr($str, $max - 3) = '...' if 2 < $max && $max < length($str);
    return $str;
}

sub ret_summary {
    my ($i, @error) = @_;
    my $err = join '', @error;
    my %i = caller_info($i + 1);
    return "$err at $i{file} line $i{line}.\n";
}

sub ret_backtrace {
    my ($i, @error) = @_;
    my $err = join '', @error;
    $i++;
    my %i = caller_info($i);
    my $mess = "$err at $i{file} line $i{line}.\n";
    while (my %f = caller_info(++$i)) {
        $mess .= "\t$f{sub_name} called at $f{file} line $f{line}\n";
    }
    return $mess;
}

# Where the long message's first line points: the first frame not inside
# Carp itself, $CarpLevel frames further out.
sub long_error_loc {
    my $i = 0;
    my $lvl = $CarpLevel;
    {
        ++$i;
        my @caller = caller($i);
        my $pkg = $caller[0];
        if (!defined $pkg) {
            if (%Internal) { local %Internal; $i = long_error_loc(); last }
            if (defined $caller[2]) { redo unless 0 > --$lvl; last }
            return 2;
        }
        redo if $CarpInternal{$pkg};
        redo unless 0 > --$lvl;
        redo if $Internal{$pkg};
    }
    return $i - 1;
}

# Where the short message points: the first CALL whose caller does not trust
# the called package (and vice versa) -- a different package, not one of its
# @ISA / @CARP_NOT, not Carp-internal; $CarpLevel such calls further out.
sub short_error_loc {
    my $cache = {};
    my $i = 1;
    my $lvl = $CarpLevel;
    {
        my $called = caller($i);
        $i++;
        my $caller = caller($i);
        if (!defined $caller) {
            my @caller = caller($i);
            if (@caller) {
                redo if defined($called) && $CarpInternal{$called};
                redo unless 0 > --$lvl;
                last;
            }
            return 0;
        }
        redo if $Internal{$caller};
        redo if $CarpInternal{$caller};
        redo if $CarpInternal{$called};
        redo if trusts($called, $caller, $cache);
        redo if trusts($caller, $called, $cache);
        redo unless 0 > --$lvl;
    }
    return $i - 1;
}

# Does CHILD inherit from (or name in @CARP_NOT) PARENT, transitively?
sub trusts {
    my ($child, $parent, $cache) = @_;
    my ($known, $partial) = get_status($cache, $child);
    while (@$partial && !exists $known->{$parent}) {
        my $anc = shift @$partial;
        next if exists $known->{$anc};
        $known->{$anc}++;
        my ($anc_knows, $anc_partial) = get_status($cache, $anc);
        @$known{keys %$anc_knows} = ();
        push @$partial, @$anc_partial;
    }
    return exists $known->{$parent};
}

sub get_status {
    my ($cache, $pkg) = @_;
    $cache->{$pkg} ||= [ { $pkg => $pkg }, [ trusts_directly($pkg) ] ];
    return @{ $cache->{$pkg} };
}

# A package's @CARP_NOT if it has one, else its @ISA.
sub trusts_directly {
    my $class = shift;
    no strict 'refs';
    my @not = @{"${class}::CARP_NOT"};
    return @not if @not;
    return @{"${class}::ISA"};
}

1;
