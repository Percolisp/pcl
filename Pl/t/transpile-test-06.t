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

test_transpile("pragmas inside embedded blocks convert and still apply (s308b)", '
my @x = map { use integer; $_ / 2 } (5,7,9);
my @y = grep { no strict; $_ > 1 } (1,2,3);
my $z = do { no warnings; 6 / 4 };
print "@x|@y|$z\n";
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

test_transpile("magic aggregate elements: @-/@+/%-/%+ swap, interp, exists, truncation (s309)", '
"xfoobar" =~ /(?<first>foo)(?<second>bar)?(?<none>zzz)?/;
print "$+{first}|$-{first}[0]|", defined $-{second}[0] ? $-{second}[0] : "U", "|";
print exists $-{none} ? (defined $-{none}[0] ? "D" : "U") : "NOKEY", "|";
print "$-[0] $+[0] $-[1] $+[1]|";
print scalar(@-), " ", $+[3] // "U", "|";
print join(",", sort keys %-), "|", scalar @{$-{first}}, "\n";
');

test_transpile("package-qualified foreach loop var binds the CL-ordered global (s309)", '
our $tm;
for $main::tm (1..3) { print $main::tm, "-" }
sub f { $main::tm }
for $main::tm ("a".."c") { print f(), "+" }
print "after:", $main::tm // "undef", "\n";
');

test_transpile("unicode package in nested block is pre-declared (s309)", '
use utf8;
binmode STDOUT, ":utf8";
{
  package 닌g난ㄬ;
  sub who { "korean" }
}
{
  package Càt;
  sub who { "cat" }
}
print 닌g난ㄬ::who(), "|", Càt::who(), "\n";
');

test_transpile("warnings:: query API via always-available shim stubs (s309)", '
use warnings;
package MyMod;
sub check { return warnings::enabled("utf8") ? "on" : "off" }
sub checkf { return warnings::fatal_enabled("utf8") ? "F" : "nf" }
package main;
print MyMod::check(), "|", MyMod::checkf(), "\n";
');

test_transpile("whitespace between m/qr operator and delimiter (s310)", '
my $s = "hello world";
print +($s =~ m /wor/ ? "m" : "-");
print +($s =~ qr /o w/ ? "q" : "-");
my $w = "wor";
print +($s =~ m /$w/ ? "i" : "-");
print +($s =~ m {hello} ? "b" : "-"), "\n";
');

test_transpile("qr deref stringifies/numifies like perl REGEXP sv (s310)", '
my $q = qr/abc/;
print "[${$q}]|[${qr /abc/i}]|";
print 0 + ${qr //}, "|";
$_ = ${qr //};
$_--;
print $_, "\n";
');

test_transpile("plain string scalar as =~ pattern compiles (s310)", '
my $pat = "b.d";
print "s1=", ("xbcd" =~ $pat ? "y" : "n"), "|";
my $p2 = "(?^i:bcd)";
print "s2=", ("xBCd" =~ $p2 ? "y" : "n"), "|";
my $q = qr/b(c)d/i;
my $d = $$q;
print "deref-match=", ("xBCd" =~ $d ? "y$1" : "n"), "|";
print "neg=", ("zzz" !~ $pat ? "y" : "n"), "\n";
');

test_transpile("builtin arity family: bare mkdir/rmdir, chown(), 4-arg select, evalbytes unary (s310)", '
$_ = "pcl-t06-arity-dir";
rmdir;
print "mk=", (mkdir ? 1 : 0), "|rm=", (rmdir ? 1 : 0), "|";
print "chown=", chown(+()), "|";
my $blank = "";
eval { select $blank, undef, $blank, 0 };
print "sel=[$@]|";
use feature "evalbytes";
my @r = (evalbytes("3+4"), "x");
print "eb=", join(",", @r), "\n";
');

test_transpile("sysread/syswrite LEN/OFFSET semantics incl. perl errors (s310)", '
my $f = "/tmp/pcl-t06-sysio-$$";
open my $o, ">", $f or die;
my $x = "abc";
eval { syswrite($o, $x, -1) };  print +($@ =~ /^Negative length / ? "e1" : "B1:$@"), "|";
eval { syswrite($o, $x, 1, 4) }; print +($@ =~ /^Offset outside string / ? "e2" : "B2:$@"), "|";
syswrite($o, $x, 1, 3);
print "w1=", syswrite($o, "0123456789", 2, 5), "|";
print "w2=", syswrite($o, "0123456789", 5, -3), "\n";
close $o;
open my $i, "<", $f or die;
my $a = "0123456789";
eval { sysread($i, $a, -1) };    print +($@ =~ /^Negative length / ? "e3" : "B3:$@"), "|";
eval { sysread($i, $a, 1, -40) }; print +($@ =~ /^Offset outside string / ? "e4" : "B4:$@"), "|[$a]|";
sysread($i, $a, 2);
sysread($i, $a, 2, 5);
sysread($i, $a, 3, -2);
print "[$a]\n";
close $i;
unlink $f;
');

test_transpile("require VERSION dies past current perl; %INC = () clears (s310)", '
eval { require 5.005 };   print "a=[$@]|";
eval { require 10.2 };    print "b=", ($@ =~ /^Perl v10\.200\.0 required/ ? "ok" : "BAD:$@"), "|";
eval { require 10.0.2 };  print "c=", ($@ =~ /^Perl v10\.0\.2 required/ ? "ok" : "BAD:$@"), "|";
my $ver = 5.005_63;
eval { require $ver };    print "d=[$@]|";
$ver = 10.2;
eval { require $ver };    print "e=", ($@ =~ /^Perl v10\.200\.0 required/ ? "ok" : "BAD:$@"), "|";
%INC = ();
print "inc=", scalar(keys %INC), "\n";
');

test_transpile("sysseek builtin + trailing-:: package call Bear::::baz (s310)", '
my $f = "/tmp/pcl-t06-sysseek-$$";
open my $o, ">", $f or die; print $o "#!/pcl"; close $o;
open(I, "<", $f) or die;
print "a=", sysseek(I, 2, 0), "|";
sysread(I, my $x, 1);
print "b=", sysseek(I, -2, 1), "|";
print "c=", (sysseek(I, 0, 0) eq "0 but true" ? "ok" : "no"), "|";
print "d=", (defined sysseek(I, -1, 1) ? "def" : "undef"), "|";
close I; unlink $f;
{ package Bear::; sub baz {7} package main; }
print "e=", (eval { Bear::::baz() } // "undef"), "\n";
');

test_transpile("process-credential specials \$< \$> \$( \$) (s310)", '
print "u=", ($< == $>) ? "same" : "diff", "|";
print "ids=[$(]==[$)]", ($( eq $)) ? "y" : "n", "|";
my ($rgid) = split " ", $(;
print "num=", ($< =~ /^\d+$/ ? "y" : "n"), ($rgid =~ /^\d+$/ ? "y" : "n"), "\n";
');

test_transpile("unicode variable names merge into one Symbol (PPI split gap, s310)", '
use utf8;
binmode STDOUT, ":utf8";
$ᕘ = "val"; my $ᴮᛅ = "lex";
print "a=$ᕘ|b=$ᴮᛅ|";
sub ᕘ { return "sub-" . $ᕘ }
print "c=", ᕘ(), "|";
$main::ᕘ = "qual";
print "d=$main::ᕘ|";
my %h온 = (k => 5); my @a온 = (7);
print "e=$h온{k}$a온[0]\n";
');

test_transpile("keys/values/each %ENV iterate the process environment (s310)", '
$ENV{PCL_T06_ENV} = "v1";
my @k = keys %ENV;
print "haskey=", (grep { $_ eq "PCL_T06_ENV" } @k) ? 1 : 0, "|";
print "nonzero=", (@k > 0 ? 1 : 0), "|";
my %seen;
while (my ($k, $v) = each %ENV) { $seen{$k} = $v }
print "each=", ($seen{PCL_T06_ENV} // "miss"), "|";
print "hasval=", (grep { $_ eq "v1" } values %ENV) ? 1 : 0, "\n";
delete $ENV{PCL_T06_ENV};
');

done_testing();
