#!/usr/bin/env perl
# Transpile tests part 6: embedded-block structured lowering (task #78) and
# onward.  NEW TESTS GO HERE (or a future -07) — 04b and earlier are large
# and slow (one SBCL spawn per test); keep per-file test counts small.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    # Add common 'use' statements for features we support
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    my $output = `perl -e '$full_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    # Write Perl code to temp file
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    # Transpile
    my $cl_code = `$pl2cl $pl_file 2>&1`;

    # Write CL to temp file
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    # Run with sbcl (saved core if PCL_TEST_CORE is set, else --load the runtime)
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    # Filter out warnings and "PCL Runtime loaded"
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# ============ EMBEDDED-BLOCK STRUCTURED LOWERING (task #78) ============

test_transpile("eval-block tail inherits call-site context", '
sub f { wantarray ? ("a","b") : "s" }
my @x = eval { f() };
my $y = eval { f() };
print "@x|$y\n";
');

test_transpile("map block per-iteration closure capture", '
my @subs = map { my $n = $_; sub { $n * 10 } } (1,2,3);
print $subs[0]->(), $subs[1]->(), $subs[2]->(), "\n";
');

test_transpile("early return from map block leaves the sub", '
sub outer { my @x = map { return "early" if $_ == 2; $_ } @_; "full:@x" }
print outer(1,3), "|", outer(2), "\n";
');

test_transpile("yada-yada statement in a sub body dies Unimplemented", '
sub u { ... }
eval { u() };
print $@ =~ /^Unimplemented/ ? "died\n" : "no:$@\n";
');

test_transpile("multi-statement sort block with side effects", '
my %h = (a=>3, b=>1, c=>2);
my $cmps = 0;
my @k = sort { $cmps++; $h{$a} <=> $h{$b} } keys %h;
print "@k|", ($cmps > 0 ? "counted" : "none"), "\n";
');

test_transpile("anon sub closure assigned inside a loop keeps its iteration", '
my $c; for my $i (1..3) { my $n = $i; $c = sub { $n } if $i == 2; }
print $c->(), "\n";
');

test_transpile("last escapes a do-block condition (loop transparency)", '
my @acc; for my $i (1..3) { last if do { push @acc, $i; $i == 2 }; }
print "@acc\n";
');

test_transpile("anon sub recursion through the closed-over ref", '
my $rec; $rec = sub { my $n = shift; $n <= 1 ? 1 : $n * $rec->($n-1) };
print $rec->(5), "\n";
');

test_transpile("tail declaration yields its statement value (s307)", '
sub f { my $x = 5 }
sub s1 { my $x = shift }
sub s2 { my $i = 3; my $j = $i; }
sub g { my @a = (1,2) }
sub h { my $y }
my $e = eval { my $z = 42 };
my $n = eval { my @a = (7,8,9) };
my $d = do { my $q = "dq" };
my @m = map { my $y = $_ * 2 } (1..3);
my @gr = grep { my $ok = $_ > 1 } (1..3);
my @so = sort { my $c = $a <=> $b } (3,1,2);
print f(), "|", s1("v"), "|", s2(), "|", join(",",g()), "|",
      (defined h() ? "def" : "undef"), "|$e|$n|$d|@m|@gr|@so\n";
');

test_transpile("embedded-block tail compounds yield their perl values (s308)", '
my @a = map { if ($_ > 1) { "big" } else { "small" } } (1,2,3);
my @b = map { if ($_ > 1) { "big" } } (1,2,3);
my @c = map { unless ($_ > 1) { "small" } } (1,2,3);
my $k = eval { if (0) { "yes" } };
my @m = map { my $v = $_; { $v * 2; } } (1,2);
my @e = map { my $s = 0; for my $i (1..$_) { $s += $i } } (2,3);
my @g = grep { if ($_ > 1) { 1 } } (1,2,3);
sub d { defined $_[0] ? "<$_[0]>" : "U" }
print "@a|", join(",", map { d($_) } @b), "|", join(",", map { d($_) } @c),
      "|", d($k), "|@m|", join(",", map { d($_) } @e), "|@g\n";
');

test_transpile("multi-value return spreads under a :void caller (s308)", '
print "A:", join("-", sort { return ($b <=> $a, $a <=> $b) } 3,1,2,4), "|";
print "B:", join("-", sort { for ($b <=> $a) { return ($b <=> $a, $a <=> $b) } } 3,1,2,4), "|";
sub two { return (7, 9) }
my @l = two(); my $s = two();
print "C:@l,$s\n";
');

test_transpile("embedded-block tail statement modifiers yield perl values (s308b)", '
sub d { defined $_[0] ? "<$_[0]>" : "U" }
my @a = map { $_ * 2 if $_ > 1 } (1,2,3);
my @b = map { $_ * 2 unless $_ > 1 } (1,2,3);
my $c = eval { "x" if 0 };
my @h = map { ($_, $_+10) if $_ > 1 } (1,2);
my $g = do { "v" if 1 };
my @f = grep { 1 if $_ > 1 } (1,2,3);
print join(",", map { d($_) } @a), "|", join(",", map { d($_) } @b),
      "|", d($c), "|@h|", d($g), "|@f\n";
');

test_transpile("string-eval value is the tail statement value (s308b E3)", '
sub d { defined $_[0] ? "<$_[0]>" : "U" }
my $a = eval q{my $x = 42};
my $b = eval q{my @a = (7,8,9)};
my $e = eval q{my $x = 5; my $y = $x + 1};
my $g = eval q{if (0) { 5 }};
my $h = eval q{"x" if 0};
my $j = eval q{local $main::lv = 9; my $t = $main::lv * 2};
print join("|", map { d($_) } ($a,$b,$e,$g,$h,$j,$main::lv)), "\n";
');

test_transpile("range endpoints outside IV range die like perl (s308)", '
my $MAX_INT = ~0 >> 1;
for my $ii (~0, ~0+1) {
  eval { my $lim = 0; for ($MAX_INT-10 .. $ii) { last if $lim++ > 100 } };
  print $@ =~ /^Range iterator outside integer range/ ? "died|" : "lived|";
}
eval { my @r = (9999999999999999999 .. 10000000000000000001) };
print $@ =~ /^Range iterator outside integer range/ ? "died\n" : "lived\n";
');

test_transpile("backslash-paren list with range mix distributes refs", '
my @r = \(1..2, 3);
my $x = 5;
my @s = \($x, 1..3);
${$s[0]} = 7;
print scalar(@r), "|", scalar(@s), "|", ${$r[0]}, ${$r[2]}, ${$s[3]}, "|", $x, "\n";
');

done_testing();
