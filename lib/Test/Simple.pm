# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# Test::Simple - Minimal test library for PCL
# A subset of Perl's t/test.pl that avoids features PCL doesn't support
#
# Provides: plan, done_testing, ok, is, isnt, like, unlike,
#           pass, fail, skip, skip_all, diag, note
#
# Does NOT use: eval "string", pack/unpack, fork, file tests, etc.

package Test::Simple;
use strict;
use warnings;

our $VERSION = '0.01';

# Test state
our $test_count = 0;
our $planned = 0;
our $no_plan = 0;
our $Level = 1;    # For caller() depth
our $TODO = '';

# Export all test functions
sub import {
    my $pkg = shift;
    my $caller = caller;

    # Export functions to caller's namespace
    no strict 'refs';
    *{"${caller}::plan"} = \&plan;
    *{"${caller}::done_testing"} = \&done_testing;
    *{"${caller}::ok"} = \&ok;
    *{"${caller}::is"} = \&is;
    *{"${caller}::isnt"} = \&isnt;
    *{"${caller}::like"} = \&like;
    *{"${caller}::unlike"} = \&unlike;
    *{"${caller}::pass"} = \&pass;
    *{"${caller}::fail"} = \&fail;
    *{"${caller}::skip"} = \&skip;
    *{"${caller}::skip_all"} = \&skip_all;
    *{"${caller}::diag"} = \&diag;
    *{"${caller}::note"} = \&note;
    *{"${caller}::cmp_ok"} = \&cmp_ok;
    *{"${caller}::BAIL_OUT"} = \&BAIL_OUT;
}

# Output functions - avoid $| and other magic
sub _print {
    print STDOUT @_;
}

sub _print_stderr {
    print STDERR @_;
}

sub _comment {
    my @lines;
    for my $msg (@_) {
        for my $line (split /\n/, $msg) {
            if ($line =~ /^#/) {
                push @lines, "$line\n";
            } else {
                push @lines, "# $line\n";
            }
        }
    }
    return @lines;
}

# plan(N) or plan(tests => N) or plan('no_plan')
sub plan {
    my $n;
    if (@_ == 1) {
        $n = shift;
        if ($n eq 'no_plan') {
            $no_plan = 1;
            return;
        }
    } else {
        my %args = @_;
        if ($args{skip_all}) {
            skip_all($args{skip_all});
        }
        $n = $args{tests};
    }
    $planned = $n;
    _print("1..$n\n");
}

sub done_testing {
    my $n = @_ ? shift : $test_count;
    _print("1..$n\n");
    $planned = $n;
}

sub skip_all {
    my $reason = shift;
    if (defined $reason && $reason ne '') {
        _print("1..0 # Skip $reason\n");
    } else {
        _print("1..0\n");
    }
    exit(0);
}

sub BAIL_OUT {
    my $reason = shift;
    _print("Bail out!  $reason\n");
    exit(255);
}

# Core test function
sub _ok {
    my ($pass, $name, @diag) = @_;
    $test_count = $test_count + 1;

    my $out;
    if (defined $name && $name ne '') {
        # Escape # in name
        $name =~ s/#/\\#/g;
        $out = $pass ? "ok $test_count - $name" : "not ok $test_count - $name";
    } else {
        $out = $pass ? "ok $test_count" : "not ok $test_count";
    }

    if ($TODO) {
        $out = $out . " # TODO $TODO";
    }

    _print("$out\n");

    unless ($pass) {
        _print_stderr(_comment(@diag)) if @diag;
    }

    return $pass;
}

# Quote a value for display
sub _q {
    my $x = shift;
    return 'undef' unless defined $x;
    my $q = $x;
    $q =~ s/\\/\\\\/g;
    $q =~ s/'/\\'/g;
    return "'$q'";
}

# Basic assertion
sub ok {
    my ($test, $name) = @_;
    return _ok($test ? 1 : 0, $name);
}

# Equality test
sub is {
    my ($got, $expected, $name) = @_;

    my $pass;
    if (!defined $got && !defined $expected) {
        $pass = 1;
    } elsif (!defined $got || !defined $expected) {
        $pass = 0;
    } else {
        $pass = ($got eq $expected) ? 1 : 0;
    }

    unless ($pass) {
        return _ok(0, $name,
            "     got: " . _q($got),
            "expected: " . _q($expected));
    }
    return _ok(1, $name);
}

# Inequality test
sub isnt {
    my ($got, $expected, $name) = @_;

    my $pass;
    if (!defined $got && !defined $expected) {
        $pass = 0;
    } elsif (!defined $got || !defined $expected) {
        $pass = 1;
    } else {
        $pass = ($got ne $expected) ? 1 : 0;
    }

    unless ($pass) {
        return _ok(0, $name,
            "got: " . _q($got),
            "expected: anything else");
    }
    return _ok(1, $name);
}

# Regex match test
sub like {
    my ($got, $expected, $name) = @_;

    my $pass = (defined $got && $got =~ $expected) ? 1 : 0;

    unless ($pass) {
        return _ok(0, $name,
            "                  got: " . _q($got),
            "expected to match: $expected");
    }
    return _ok(1, $name);
}

# Regex non-match test
sub unlike {
    my ($got, $expected, $name) = @_;

    my $pass = (!defined $got || $got !~ $expected) ? 1 : 0;

    unless ($pass) {
        return _ok(0, $name,
            "                      got: " . _q($got),
            "expected NOT to match: $expected");
    }
    return _ok(1, $name);
}

# Comparison with operator
# Note: We implement common operators directly to avoid eval "string"
sub cmp_ok {
    my ($got, $op, $expected, $name) = @_;

    my $pass = 0;

    # Handle common operators
    if ($op eq '==') {
        $pass = (defined $got && defined $expected && $got == $expected) ? 1 : 0;
    } elsif ($op eq '!=') {
        $pass = (defined $got && defined $expected && $got != $expected) ? 1 : 0;
    } elsif ($op eq '<') {
        $pass = (defined $got && defined $expected && $got < $expected) ? 1 : 0;
    } elsif ($op eq '>') {
        $pass = (defined $got && defined $expected && $got > $expected) ? 1 : 0;
    } elsif ($op eq '<=') {
        $pass = (defined $got && defined $expected && $got <= $expected) ? 1 : 0;
    } elsif ($op eq '>=') {
        $pass = (defined $got && defined $expected && $got >= $expected) ? 1 : 0;
    } elsif ($op eq 'eq') {
        $pass = (defined $got && defined $expected && $got eq $expected) ? 1 : 0;
    } elsif ($op eq 'ne') {
        $pass = (defined $got && defined $expected && $got ne $expected) ? 1 : 0;
    } elsif ($op eq 'lt') {
        $pass = (defined $got && defined $expected && $got lt $expected) ? 1 : 0;
    } elsif ($op eq 'gt') {
        $pass = (defined $got && defined $expected && $got gt $expected) ? 1 : 0;
    } elsif ($op eq 'le') {
        $pass = (defined $got && defined $expected && $got le $expected) ? 1 : 0;
    } elsif ($op eq 'ge') {
        $pass = (defined $got && defined $expected && $got ge $expected) ? 1 : 0;
    } elsif ($op eq '=~') {
        $pass = (defined $got && $got =~ $expected) ? 1 : 0;
    } elsif ($op eq '!~') {
        $pass = (!defined $got || $got !~ $expected) ? 1 : 0;
    } else {
        # Unknown operator - fail with message
        return _ok(0, $name, "Unknown operator '$op' in cmp_ok");
    }

    unless ($pass) {
        return _ok(0, $name,
            "     got: " . _q($got),
            "expected: $op " . _q($expected));
    }
    return _ok(1, $name);
}

# Explicit pass/fail
sub pass {
    my $name = shift;
    return _ok(1, $name);
}

sub fail {
    my $name = shift;
    return _ok(0, $name);
}

# Skip tests
sub skip {
    my ($reason, $count) = @_;
    $count = 1 unless defined $count;

    for (1 .. $count) {
        $test_count = $test_count + 1;
        _print("ok $test_count # skip $reason\n");
    }
    no warnings 'exiting';
    last SKIP;
}

# Diagnostic output
sub diag {
    return unless @_;
    _print_stderr(_comment(@_));
}

# Note output (to stdout)
sub note {
    return unless @_;
    _print(_comment(@_));
}

# END block to check test count
END {
    if ($planned && $test_count != $planned) {
        _print_stderr("# Looks like you planned $planned tests but ran $test_count.\n");
    } elsif ($no_plan) {
        _print("1..$test_count\n");
    }
}

1;
