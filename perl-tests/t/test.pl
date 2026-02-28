# Minimal test.pl for PCL test infrastructure
# These functions are provided by pcl-test.lisp at runtime,
# but we need stubs here so the transpiler can parse them.

package main;  # Explicitly in main package (Perl's default)

# Used by many tests for tracking call depth (e.g., local $::Level = $::Level + 1)
our $Level = 1;

# plan - provided by pcl-test.lisp, do NOT define here or it will override

sub set_up_inc {
    # No-op for PCL - @INC is set up differently
}

sub curr_test {
    # Returns/sets current test number - not needed for basic tests
    return 1;
}

# skip, skip_all - provided by pcl-test.lisp, do NOT define here or it will override

sub skip_all_without_unicode_tables {
    # No-op - we'll handle unicode tests differently
}

# fresh_perl_* run code in new Perl processes - skip these tests
sub fresh_perl_is {
    return;
}

sub fresh_perl_like {
    return;
}

# watchdog for timeout tests
sub watchdog {
    # No-op - timeout handling not needed
}

# run_multiple_progs - skip
sub run_multiple_progs {
    # Skip - runs multiple Perl scripts
}

# runperl - runs Perl code in subprocess, skip
sub runperl {
    return "";
}

# is_miniperl - check if running miniperl (always false for PCL)
sub is_miniperl {
    return 0;
}

# skip_if_miniperl - skip N tests if running miniperl
# PCL can't run subprocess tests with -C/-CE switches, so always skip these
sub skip_if_miniperl {
    my ($reason, $n) = @_;
    skip($reason, $n // 1);
}

# isa_ok - check if object is blessed into class
sub isa_ok {
    my ($obj, $class, $name) = @_;
    $name //= "object isa $class";
    if (ref($obj) && UNIVERSAL::isa($obj, $class)) {
        print "ok - $name\n";
        return 1;
    } else {
        print "not ok - $name\n";
        return 0;
    }
}

# cmp_ok - provided by pcl-test.lisp, do NOT define here or it will override

# like, unlike - provided by pcl-test.lisp, do NOT define here or it will override

# pass, fail, note, diag - provided by pcl-test.lisp, do NOT define here or it will override

# tempfile - returns a unique temp filename (used by I/O tests)
my $tempfile_counter = 0;
sub tempfile {
    $tempfile_counter++;
    return "/tmp/pcl-test-$$-$tempfile_counter";
}

# object_ok - check if value is a blessed object (optionally of a specific class)
sub object_ok {
    my ($obj, $class, $name) = @_;
    $name //= "object is blessed";
    if (ref($obj)) {
        if (defined $class) {
            ok(ref($obj) eq $class, $name);
        } else {
            ok(1, $name);
        }
    } else {
        ok(0, $name);
    }
}

# watchdog - set alarm for test timeout (no-op in PCL)
sub watchdog {
    # No-op - PCL doesn't support alarm signals
}

# within - check if got is within range of expect (used by pow.t etc.)
sub within {
    my ($got, $expect, $range, $test) = @_;
    my $ok = $range == 0 ? $got == $expect : abs($got - $expect) <= $range;
    ok($ok, $test);
}

1;
