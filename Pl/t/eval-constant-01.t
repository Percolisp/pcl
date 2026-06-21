#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Regression tests for two fixes found while running JSON::PP through PCL:
#
#  1. utf8::unicode_to_native / native_to_unicode were undefined (JSON::PP
#     builds its invalid-char regex with chr(utf8::unicode_to_native($i))).
#     On any ASCII platform both are the identity.
#
#  2. A `use constant` name used as an ARRAY subscript inside eval "string"
#     was autoquoted to a string index (-> index 0) instead of being called
#     as the zero-arg sub it is.  In Perl, $a[BAREWORD] always calls the sub
#     (only HASH subscripts autoquote); the constant sub exists at runtime, so
#     eval-mode must keep an ALL-CAPS subscript bareword callable.  This was
#     the root cause of JSON::PP's ->canonical wrongly enabling ascii escaping
#     (canonical wrote to PROPS[P_CANONICAL] but P_CANONICAL resolved to 0,
#     the ascii slot).

use v5.30;
use strict;
use warnings;

use lib ".";

use Test::More tests => 5;
use File::Temp qw(tempfile);

my $runtime = "cl/pcl-runtime.lisp";
my $pl2cl   = "./pl2cl";

sub run_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/PCL Runtime loaded\n?//g;
    $out =~ s/^\s*\n//gm;
    return $out;
}

SKIP: {
    skip "pl2cl not found", 5 unless -x $pl2cl;
    skip "sbcl not found",  5 unless `which sbcl 2>/dev/null`;

    # --- utf8::unicode_to_native / native_to_unicode are identity ---
    is(run_pl('print utf8::unicode_to_native(65);'), "65",
       'utf8::unicode_to_native is identity on ASCII');
    is(run_pl('print utf8::native_to_unicode(0x41);'), "65",
       'utf8::native_to_unicode is identity on ASCII');
    is(run_pl('print chr(utf8::unicode_to_native(34));'), '"',
       'chr(utf8::unicode_to_native(34)) is the double-quote char');

    # --- constant as ARRAY subscript inside eval "string" ---
    is(run_pl(<<'PL'), "idx0=undef idx4=1",
use constant P_FOUR => 4;
my @props;
eval q{ $props[P_FOUR] = 1; };
print "idx0=", (defined $props[0] ? $props[0] : "undef"),
      " idx4=", (defined $props[4] ? $props[4] : "undef");
PL
       'use-constant bareword as array subscript resolves inside eval string');

    # The same constant via a nested-deref array (the JSON::PP ->canonical shape)
    is(run_pl(<<'PL'), "idx0=undef idx4=1",
use constant P_FOUR => 4;
my $self = { PROPS => [] };
eval q{ $self->{PROPS}->[P_FOUR] = 1; };
print "idx0=", (defined $self->{PROPS}[0] ? $self->{PROPS}[0] : "undef"),
      " idx4=", (defined $self->{PROPS}[4] ? $self->{PROPS}[4] : "undef");
PL
       'constant subscript on nested arrayref resolves inside eval string');
}
