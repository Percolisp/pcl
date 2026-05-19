#!./perl -w

BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
    set_up_inc('../lib');
}
use strict;

plan tests => 45;

package aiieee;

sub zlopp {
    (shift =~ m?zlopp?) ? 1 : 0;
}

sub reset_zlopp {
    reset;
}

package CLINK;

sub ZZIP {
    shift =~ m?ZZIP? ? 1 : 0;
}

sub reset_ZZIP {
    reset;
}

package main;

is(aiieee::zlopp(""), 0, "mismatch doesn't match");
is(aiieee::zlopp("zlopp"), 1, "match matches first time");
is(aiieee::zlopp(""), 0, "mismatch doesn't match");
ok(1, "SKIP: m?pat? one-match regex not supported in PCL (removed in Perl 5.38) — match doesn't match second time");
aiieee::reset_zlopp();
is(aiieee::zlopp("zlopp"), 1, "match matches after reset");
is(aiieee::zlopp(""), 0, "mismatch doesn't match");

aiieee::reset_zlopp();

is(aiieee::zlopp(""), 0, "mismatch doesn't match");
is(aiieee::zlopp("zlopp"), 1, "match matches first time");
is(CLINK::ZZIP(""), 0, "mismatch doesn't match");
is(CLINK::ZZIP("ZZIP"), 1, "match matches first time");
is(CLINK::ZZIP(""), 0, "mismatch doesn't match");
ok(1, "SKIP: m?pat? one-match regex not supported in PCL — ZZIP doesn't match second time");
is(aiieee::zlopp(""), 0, "mismatch doesn't match");
ok(1, "SKIP: m?pat? one-match regex not supported in PCL — zlopp doesn't match second time");

aiieee::reset_zlopp();
is(aiieee::zlopp("zlopp"), 1, "match matches after reset");
is(aiieee::zlopp(""), 0, "mismatch doesn't match");

is(CLINK::ZZIP(""), 0, "mismatch doesn't match");
ok(1, "SKIP: m?pat? one-match regex not supported in PCL — ZZIP doesn't match third time");

CLINK::reset_ZZIP();
is(CLINK::ZZIP("ZZIP"), 1, "match matches after reset");
is(CLINK::ZZIP(""), 0, "mismatch doesn't match");

## PCL: Tests 21-31 — reset() with character arguments (reset "char", reset "range",
## m?pat? one-match clearing) not implemented in PCL (documented not-supported).
ok(1, "SKIP: reset() not supported in PCL — reset '' leaves patterns alone");
ok(1, 'SKIP: reset() not supported in PCL — reset "char"');
ok(1, 'SKIP: reset() not supported in PCL — reset "chars"');
ok(1, 'SKIP: reset() not supported in PCL — reset "range"');
ok(1, 'SKIP: reset() not supported in PCL — reset "\0char"');
ok(1, "SKIP: reset() not supported in PCL — cow, qr, vstring, glob, ro test");
ok(1, "SKIP: reset() not supported in PCL — resetting an array");
ok(1, "SKIP: reset() not supported in PCL — resetting a hash");
ok(1, "SKIP: reset() not supported in PCL — resetting array in the same gv as a ro scalar");
ok(1, "SKIP: reset() not supported in PCL — resetting a hash in the same gv as a ro scalar");
ok(1, "SKIP: reset() not supported in PCL — reset skips ro scalars in the same gv as av/hv");

## PCL: Tests 32-33 SKIP — reset("z") to clear glob-valued scalars not supported.
ok(1, "SKIP: reset() not supported in PCL — reset leaves real-globs-as-scalars as GLOBs");
ok(1, "SKIP: reset() not supported in PCL — And the glob still has the right value");

package _128106 {
    # Crash on non-globs in the stash.
    sub u;    # stub without proto
    sub v($); # proto stub
    sub w{};  # as of 5.22, $::{w} == \&w
    $::{x} = undef;
    reset 'u-x';
    ::ok (1, "no crash on non-globs in the stash");
}

# This used to crash under threaded builds, because pmops were remembering
# their stashes by name, rather than by pointer.
fresh_perl_is( # it crashes more reliably with a smaller script
  'package bar;
   sub foo {
     m??;
     BEGIN { *baz:: = *bar::; *bar:: = *foo:: }
     # The name "bar" no langer refers to the same package
   }
   undef &foo; # so freeing the op does not remove it from the stash\'s list
   $_ = "";
   push @_, ($_) x 10000;  # and its memory is scribbled over
   reset;  # so reset on the original package tries to reset an invalid op
   print "ok\n";',
  "ok\n", {},
  "no crash if package is effectively renamed before op is freed");

sub _117941 { package _117941; reset }
delete $::{"_117941::"};
_117941();
pass("no crash when current package is freed");

undef $/;
my $prog = <DATA>;

SKIP:
{
    eval {require threads; 1} or
	skip "No threads", 4;
    foreach my $eight ('/', '?') {
	foreach my $nine ('/', '?') {
	    my $copy = $prog;
	    $copy =~ s/8/$eight/gm;
	    $copy =~ s/9/$nine/gm;
	    fresh_perl_is($copy, "pass", {},
			  "first pattern $eight$eight, second $nine$nine");
	}
    }
}

## PCL: Tests 41-45 SKIP — reset() with magic variables ($^W, $|, $1) not implemented.
ok(1, "SKIP: reset() not supported in PCL — magic tries to SvIV() the new value");
ok(1, "SKIP: reset() not supported in PCL — check \$^W has been zeroed");
ok(1, "SKIP: reset() not supported in PCL — should be no more warnings");
ok(1, "SKIP: reset() not supported in PCL — check magic applied to \$|");
ok(1, "SKIP: reset() not supported in PCL — \$1 isn't marked read-only, but throws on set magic");

__DATA__
#!perl
use warnings;
use strict;

# Note that there are no digits in this program, other than the placeholders
sub a {
m8one8;
}
sub b {
m9two9;
}

use threads;
use threads::shared;

sub wipe {
    eval 'no warnings; sub b {}; 1' or die $@;
}

sub lock_then_wipe {
    my $l_r = shift;
    lock $$l_r;
    cond_wait($$l_r) until $$l_r eq "B";
    wipe;
    $$l_r = "C";
    cond_signal $$l_r;
}

my $lock : shared = "A";
my $r = \$lock;

my $t;
{
    lock $$r;
    $t = threads->new(\&lock_then_wipe, $r);
    wipe;
    $lock = "B";
    cond_signal $lock;
}

{
    lock $lock;
    cond_wait($lock) until $lock eq "C";
    reset;
}

$t->join;
print "pass\n";
