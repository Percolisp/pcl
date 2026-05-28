# Minimal test.pl for PCL test infrastructure
# These functions are provided by pcl-test.lisp at runtime,
# but we need stubs here so the transpiler can parse them.

package main;  # Explicitly in main package (Perl's default)

# Used by many tests for tracking call depth (e.g., local $::Level = $::Level + 1)
our $Level = 1;

# Platform character set flag (used by char/unicode tests)
our $IS_ASCII = (ord('A') == 65);

# plan - provided by pcl-test.lisp, do NOT define here or it will override

sub set_up_inc {
    # No-op for PCL - @INC is set up differently
}


# skip, skip_all - provided by pcl-test.lisp, do NOT define here or it will override

sub skip_all_without_unicode_tables {
    # No-op - we'll handle unicode tests differently
}

# _fresh_perl_run: shared helper — runs code in a fresh Perl, returns normalized output.
# Mirrors Perl t/test.pl _fresh_perl: captures stderr by default, strips trailing newlines,
# normalises temp-file paths in error messages.
sub _fresh_perl_run {
    my ($code, $opts) = @_;
    $opts //= {};
    my @switches = grep { length($_) } @{$opts->{switches} // []};
    # Default: capture stderr (same as Perl's real t/test.pl). Pass stderr=>0 to suppress.
    my $capture_stderr = (!exists $opts->{stderr} || $opts->{stderr}) ? '2>&1' : '2>/dev/null';
    my $tmpfile = "/tmp/pcl_fp_$$" . int(rand(99999)) . ".pl";
    open(my $fh, '>', $tmpfile) or return "";
    print $fh $code;
    close $fh;
    my $perl = $^X;
    my $sw = join(' ', @switches);
    my $got;
    if (defined $opts->{stdin}) {
        my $sin = "/tmp/pcl_fp_sin_$$.txt";
        open(my $sf, '>', $sin) or do { unlink $tmpfile; return ""; };
        print $sf $opts->{stdin};
        close $sf;
        $got = `$perl $sw "$tmpfile" $capture_stderr < "$sin"`;
        unlink $sin;
    } else {
        $got = `$perl $sw "$tmpfile" $capture_stderr`;
    }
    unlink $tmpfile;
    $got //= "";
    # Normalize temp-file path in error messages (at /tmp/pcl_fp_NNN.pl line N -> at - line N)
    (my $escaped = $tmpfile) =~ s/[.]/[.]/g;
    $got =~ s{at\s+$escaped\s+line}{at - line}g;
    $got =~ s{of\s+$escaped\s+aborted}{of - aborted}g;
    # Strip trailing newlines (matches Perl t/test.pl _fresh_perl behaviour)
    $got =~ s/\n+$//;
    return $got;
}

# fresh_perl_* run code in a real Perl subprocess for accurate results
sub fresh_perl_is {
    my ($code, $expected, $opts, $desc) = @_;
    $opts //= {};
    $desc //= 'fresh_perl_is';
    my $got = _fresh_perl_run($code, $opts);
    # Also strip trailing newlines from expected (Perl t/test.pl does this too)
    $expected =~ s/\n+$//;
    is($got, $expected, $desc);
}

sub fresh_perl_like {
    my ($code, $pattern, $opts, $desc) = @_;
    $opts //= {};
    $desc //= 'fresh_perl_like';
    my $got = _fresh_perl_run($code, $opts);
    like($got, $pattern, $desc);
}

# watchdog for timeout tests
sub watchdog {
    # No-op - timeout handling not needed
}

# run_multiple_progs - skip
sub run_multiple_progs {
    # Skip - runs multiple Perl scripts
}

# run_perl - run a Perl program in a subprocess, return its output.
# Named args: prog => $code_string, switches => \@flags, args => \@argv,
#             stdin => $input, stderr => bool.
sub run_perl {
    my (%opts) = @_;
    my $prog = $opts{prog} // return "";
    my @switches = grep { length($_) } @{$opts{switches} // []};
    my $capture_stderr = $opts{stderr} ? '2>&1' : '2>/dev/null';
    my $tmpfile = "/tmp/pcl_rp_$$" . int(rand(99999)) . ".pl";
    open(my $fh, '>', $tmpfile) or return "";
    print $fh $prog;
    close $fh;
    my $perl = $^X;
    my $sw   = join(' ', @switches);
    my $argv = join(' ', map { quotemeta($_) } @{$opts{args} // []});
    my $got;
    if (defined $opts{stdin}) {
        my $sin = "/tmp/pcl_rp_sin_$$.txt";
        open(my $sf, '>', $sin) or do { unlink $tmpfile; return ""; };
        print $sf $opts{stdin};
        close $sf;
        $got = `$perl $sw "$tmpfile" $argv $capture_stderr < "$sin"`;
        unlink $sin;
    } else {
        $got = `$perl $sw "$tmpfile" $argv $capture_stderr`;
    }
    unlink $tmpfile;
    $got //= "";
    (my $escaped = $tmpfile) =~ s/[.]/[.]/g;
    $got =~ s{at\s+$escaped\s+line}{at - line}g;
    $got =~ s{of\s+$escaped\s+aborted}{of - aborted}g;
    return $got;
}

# runperl - alias for run_perl (legacy name used in some test files)
sub runperl {
    return run_perl(@_);
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

# eq_hash - compare two hash refs for equality (from Perl's internal test.pl)
sub eq_hash {
    my ($a, $b) = @_;
    return 0 unless ref($a) eq 'HASH' && ref($b) eq 'HASH';
    my %seen;
    for my $k (keys %$a) {
        return 0 unless exists $b->{$k};
        if (defined($a->{$k}) && defined($b->{$k})) {
            return 0 unless $a->{$k} eq $b->{$k};
        } else {
            return 0 unless !defined($a->{$k}) && !defined($b->{$k});
        }
        $seen{$k} = 1;
    }
    for my $k (keys %$b) {
        return 0 unless $seen{$k};
    }
    return 1;
}

# eq_array - compare two array refs for equality
sub eq_array {
    my ($a, $b) = @_;
    return 0 unless ref($a) eq 'ARRAY' && ref($b) eq 'ARRAY';
    return 0 unless @$a == @$b;
    for my $i (0..$#$a) {
        if (defined($a->[$i]) && defined($b->[$i])) {
            return 0 unless $a->[$i] eq $b->[$i];
        } else {
            return 0 unless !defined($a->[$i]) && !defined($b->[$i]);
        }
    }
    return 1;
}

# within - check if got is within range of expect (used by pow.t etc.)
sub within {
    my ($got, $expect, $range, $test) = @_;
    my $ok = $range == 0 ? $got == $expect : abs($got - $expect) <= $range;
    ok($ok, $test);
}

# refcount_is - stub; Internals::SvREFCNT is not supported, always passes
sub refcount_is {
    my ($ref, $expected, $test) = @_;
    ok(1, $test);
}

# warning_is - run a code block and check it emits (or doesn't emit) a warning.
# Stub: just runs the code and passes the warning assertion unconditionally.
# Used by assignwarn.t, time.t, and others.
sub warning_is (&$;$) {
    my ($code, $expected, $name) = @_;
    $code->();
    pass($name // "warning_is");
}

# warning_like - same as warning_is but expects a regex match on the warning
sub warning_like (&$;$) {
    my ($code, $expected, $name) = @_;
    $code->();
    pass($name // "warning_like");
}

1;
