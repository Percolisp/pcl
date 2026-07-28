#!/usr/bin/env perl
# Transpile tests part 6: embedded-block structured lowering (task #78) and
# onward.  FULL at 50 tests — new tests go in transpile-test-07.t: the
# BIGGEST file bounds the parallel suite's wall time (one SBCL spawn per
# test), so start a new file instead of growing the current largest.

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
    # Shell-escape embedded single quotes ('…' -> '\''), or any tick in the
    # snippet (e.g. the Perl-4 package separator tests) truncates the -e arg.
    (my $sh_code = $full_code) =~ s/'/'\\''/g;
    my $output = `perl -e '$sh_code' 2>&1`;
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

test_transpile("special-block phase order: UNITCHECK/CHECK reverse, INIT source order, eval too-late (s310)", '
print ":main1";
CHECK { print ":c1" }
INIT { print ":i1" }
UNITCHECK { print ":u1" }
CHECK { print ":c2" }
INIT { print ":i2" }
UNITCHECK { print ":u2" }
eval q{CHECK { print ":cE" } INIT { print ":iE" } 1} or die;
END { print ":end\n" }
print ":main2";
');

test_transpile("exit during compile phase drains UNITCHECK/CHECK, skips INIT/main (s310)", '
UNITCHECK { print "u\n" }
CHECK { print "c1\n" }
CHECK { print "c2\n"; exit 0 }
CHECK { print "c3\n" }
INIT { print "i\n" }
END { print "e\n" }
BEGIN { print "b\n" }
print "main\n";
');

test_transpile("underscore in named-capture group names (cl-ppcre gap, s310)", '
my $s = "1000(bernt)";
if ($s =~ /^(?<gid>\d+)\((?<gr_name>.+)\)$/x) { print "a=$+{gr_name},$+{gid}|" } else { print "a=no|" }
if ("abcabc" =~ /(?<my_grp>abc)\k<my_grp>/) { print "b=$+{my_grp}\n" } else { print "b=no\n" }
');


# `goto LABEL` where PPI glues the label onto a LOOP or a BARE BLOCK (only
# those two shapes — `LBL: if (…)` / `LBL: $x++;` leave the label standalone).
# Both crashed with "attempt to GO to nonexistent tag" before s316: the label
# never reached the standalone-label tagbody path, so the `(go :LBL)` had no
# tag at all.  sigtrap.pm's import loop is the real-world case.
test_transpile("goto LABEL back into a labeled while loop (sub tail value)", '
sub f {
    my $n = 0;
  Arg_loop:
    while (@_) { my $x = shift; $n++; }
    if (!$n) { @_ = (1,2); goto Arg_loop; }
    return $n;
}
print f(), ",", f(5,6), "\n";
');

test_transpile("goto LABEL into a labeled bare block at top level", '
my $tries = 0;
RETRY: {
    $tries++;
    if ($tries < 3) { goto RETRY; }
}
print "tries=$tries\n";
');

test_transpile("goto LABEL from inside the labeled loop, and last/next on it", '
sub g {
    my $seen = 0;
  L: foreach my $i (3,2,1) {
        $seen++;
        if ($i == 2 && $seen < 5) { goto L; }
    }
    return $seen;
}
sub h {
    my $n = 0;
  M: while (1) { $n++; next M if $n == 1; last M if $n > 2; }
    if ($n < 10) { $n += 100; goto M if $n < 105; }
    return $n;
}
print g(), ",", h(), "\n";
');

# %SIG is pre-populated with every signal name (values undef), like perl —
# `exists $SIG{HUP}` is true before any handler is installed.  Without it
# sigtrap.pm loops forever.  Count is platform-specific, so compare against
# perl and check the named lookups explicitly.
test_transpile("%SIG pre-populated with the platform signal names (s316)", '
print scalar(keys %SIG), " ";
print join(",", map { exists $SIG{$_} ? 1 : 0 } qw(HUP INT PIPE TERM SYS ALRM)), " ";
print defined $SIG{HUP} ? "def" : "undef", "\n";
');


# End-to-end for the s316 capture promotion: the outer sub must see the file
# lexical's live value (the promoted cell) while the shadowing sub keeps its
# own, both through interpolation and through plain symbol reads.
test_transpile("captured file lexical + same-name shadow, interpolated (s316)", '
my $v = "outer";
sub show  { print "in-sub=$v/", $v, "\n"; }
sub other { my $v = "inner"; print "shadow=$v\n"; }
sub bump  { $v = $v . "+"; }
show(); other(); bump(); show(); other();
print "top=$v\n";
');

# &$ref / &{expr} with NO argument list is a CALL passing the current @_
# (same rule as `&name;`), not a coderef fetch — s316d.  The mention
# parents (\, defined, exists) keep the coderef itself.
test_transpile("&\$ref / &{expr} with no parens call with current \@_ (s316d)", '
sub abc { print "got(@_)\n" }
my $r = \&abc;
&$r;
&{$r};
&{"abc"};
sub outer { &abc; &$r; }
outer(1,2);
print "d=", (defined &$r ? 1 : 0), (exists &{"abc"} ? 1 : 0),
      (defined &{"nope"} ? 1 : 0), "\n";
my $q = \&{"abc"};
print "q=", $q->("z"), "\n";
sub abc2 { return "R(@_)" }
my $v = &{"abc2"};
print "v=$v\n";
');

# Perl-4 tick package separator: sub declarations are normalised before PPI
# (which cannot tokenize them at all), symbolic name strings at runtime.
test_transpile("Perl-4 ' package separator: sub decl + symbolic call (s316d)", '
no warnings;
sub x\'y { print "tick(@_)\n"; return "ty" }
&{"x\'y"};
my $w = &{"x\'y"}("a");
print "w=$w\n";
print "d=", (defined &{"x\'y"} ? 1 : 0), "\n";
');

# Count-only tr (empty replacement, no /d) on a read-only value: perl
# accepts it and just counts — no "Cannot modify" warning may leak.
test_transpile("count-only tr on a literal counts without warning (s316d)", '
print "abcba" =~ tr/ab//, "\n";
my @w; local $SIG{__WARN__} = sub { push @w, @_ };
print "x" =~ tr/x//, " w=", scalar(@w), "\n";
');

# Symbolic call of a sub declared with a 3-segment qualified name: the
# resolver must use the multi-segment package rule (|aa::bb| keeps case).
test_transpile("symbolic call of 3-segment qualified sub name (s316d)", '
sub aa::bb::cc { print "seg3(@_)\n" }
&{"aa::bb::cc"};
&{"aa::bb::cc"}("k");
');

# Deferred-element (defelem) aliasing of array HOLES (task #127, fresh_perl
# case 28): a foreach alias over a hole reads undef, vivifies the source
# slot only on WRITE — and read-only iteration must NOT vivify.
test_transpile("foreach write through hole slots vivifies (defelem)", '
my @a; $a[2] = 1;
for (@a) { $_ = 2 }
print "@a\n";
');

test_transpile("read-only foreach leaves holes unvivified", '
my @b; $b[2] = 7;
for (@b) { }
print exists $b[0] ? "vivified\n" : "still hole\n";
');

test_transpile("multi-array foreach list ties holes to each source array", '
my @c; my @d; $c[1] = 1; $d[1] = 2;
for (@c, @d) { $_ = 9 }
print "@c|@d\n";
');

test_transpile("\@_ hole slot: undef, non-exists, write vivifies caller array", '
sub f { print defined $_[0] ? "def" : "undef",
              exists $_[0] ? " exists" : " noexists", "\n";
        $_[0] = 5 }
my @e; $e[1] = 1; f(@e);
print "@e\n";
');

test_transpile("push during foreach extends the live iteration", '
my @e = (1,2);
for (@e) { push @e, 9 if @e < 4 }
print "@e\n";
');

test_transpile("grep/map \$_ writes vivify hole slots like foreach", '
my @a; $a[1] = 1; grep { $_ = 5 } @a; print "g:@a\n";
my @b; $b[1] = 1; map { $_ = 6 } @b; print "m:@b\n";
');

# Task #126: a pure-prototype sub lowers natively (no v1 seam), and a
# forward goto to a standalone label past `my` declarations works — the
# t/test.pl watchdog shape.  The jumped-over `my` reads undef.
test_transpile("prototyped sub: forward goto past my-decls (watchdog shape)", '
sub w ($;$) {
    my $timeout = shift;
    my $method = shift || "";
    my $msg = "T";
    if ($method eq "alarm") { goto WVA; }
    my $late = 5;
    print "before:$late\n";
    return;
    WVA:
    print "via:$timeout:$msg:", (defined $late ? $late : "undef"), "\n";
}
w(3, "alarm");
w(7);
');

# The standalone-label-in-tail-position regime: the label remainder is the
# sub return value (setf-RET tagbody bracket, task #64 regime).
test_transpile("standalone label in tail position keeps the sub value", '
sub tailval { my $x = shift; if ($x) { goto DONE; } my $y = 2; DONE: "tail-$x" }
print tailval(1), "|", tailval(0), "\n";
');

done_testing();
