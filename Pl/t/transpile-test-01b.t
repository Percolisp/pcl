#!/usr/bin/env perl
# Transpile tests (part 2): compare Perl output with transpiled CL output

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
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $output = `perl -e '$code' 2>&1`;
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

    # Run with sbcl
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

# ============ TESTS ============

# split in scalar context returns count
test_transpile("split: scalar context", '
  my $str = "a,b,c,d";
  my $n = split(/,/, $str);
  print $n, "\n";
');

# string ranges
test_transpile("range: multi-char string", '
  print join(",", "aa".."af"), "\n";
');

test_transpile("range: string wrap-around", '
  print join(",", "x".."ad"), "\n";
');

# KV slice
test_transpile("kv slice: access and delete", '
  my %h = (a => 1, b => 2, c => 3);
  my @kv = %h{"a","c"};
  print join(",", @kv), "\n";
  my @del = delete %h{"a"};
  print join(",", @del), "\n";
  print join(",", sort keys %h), "\n";
');

# $#array lvalue
test_transpile('$#array: pre-decrement shrinks', '
  my @ary = (1,2,3,4,5);
  --$#ary;
  print scalar @ary, "\n";
  print $ary[-1], "\n";
');

test_transpile('$#array: assignment grows', '
  my @ary = (1,2,3);
  $#ary = 5;
  print scalar @ary, "\n";
  print $ary[0], "\n";
');

test_transpile('$#array: assignment shrinks', '
  my @ary = (1,2,3,4,5);
  $#ary = 1;
  print scalar @ary, "\n";
  print $ary[0], "\n";
  print $ary[1], "\n";
');

test_transpile('$#array: post-increment', '
  my @ary = (1,2,3);
  my $old = $#ary++;
  print $old, "\n";
  print scalar @ary, "\n";
');

# Numeric formatting: trailing decimal point fix
test_transpile("float: integer-valued float stringifies without dot", '
  print 0.0 + 0, "\n";
  print 1.0 + 0, "\n";
  print 100.0 + 0, "\n";
  print -10.0 + 0, "\n";
');

# Numeric formatting: small numbers use exponential
test_transpile("float: Inf/NaN stringify", '
  my $inf = "Inf" + 0;
  my $ninf = "-Inf" + 0;
  my $nan = "NaN" + 0;
  print $inf, "\n";
  print $ninf, "\n";
  print $nan, "\n";
');

# --- splice tests ---
test_transpile("splice: remove from end", '
  my @a = (1, 2, 3, 4, 5);
  my @removed = splice(@a, 3);
  print join(",", @removed), "\n";
  print join(",", @a), "\n";
', "4,5\n1,2,3\n");

test_transpile("splice: remove middle", '
  my @a = (1, 2, 3, 4, 5);
  my @removed = splice(@a, 1, 2);
  print join(",", @removed), "\n";
  print join(",", @a), "\n";
', "2,3\n1,4,5\n");

test_transpile("splice: replace elements", '
  my @a = (1, 2, 3, 4, 5);
  splice(@a, 1, 2, 10, 20, 30);
  print join(",", @a), "\n";
', "1,10,20,30,4,5\n");

test_transpile("splice: insert without removing", '
  my @a = (1, 2, 3);
  splice(@a, 1, 0, 10, 20);
  print join(",", @a), "\n";
', "1,10,20,2,3\n");

# --- $| pipe-quoting test ---
test_transpile("autoflush variable", '
  $| = 1;
  print "ok\n";
', "ok\n");

# --- sprintf format specifiers ---
test_transpile("sprintf: %d integer", '
  print sprintf("%d", 42), "\n";
', "42\n");

test_transpile("sprintf: %05d zero-padded", '
  print sprintf("%05d", 42), "\n";
', "00042\n");

test_transpile("sprintf: %x hex", '
  print sprintf("%x", 255), "\n";
', "ff\n");

test_transpile("sprintf: %#x hex with prefix", '
  print sprintf("%#x", 255), "\n";
', "0xff\n");

test_transpile("sprintf: %08x zero-padded hex", '
  print sprintf("%08x", 255), "\n";
', "000000ff\n");

test_transpile("sprintf: %o octal", '
  print sprintf("%o", 255), "\n";
', "377\n");

test_transpile("sprintf: %#o octal with prefix", '
  print sprintf("%#o", 255), "\n";
', "0377\n");

test_transpile("sprintf: %b binary", '
  print sprintf("%b", 42), "\n";
', "101010\n");

test_transpile("sprintf: %e exponential", '
  print sprintf("%e", 3.14), "\n";
', "3.140000e+00\n");

test_transpile("sprintf: %f fixed-point", '
  print sprintf("%f", 3.14), "\n";
', "3.140000\n");

test_transpile("sprintf: %g general float", '
  print sprintf("%g", 3.14), "\n";
', "3.14\n");

test_transpile("sprintf: %g small number (exponential)", '
  print sprintf("%g", 0.00001), "\n";
', "1e-05\n");

test_transpile("sprintf: %10.3f width+precision", '
  print sprintf("%10.3f", 3.14), "\n";
', "     3.140\n");

test_transpile("sprintf: %-10s left-justify", '
  print sprintf("%-10s|", "hello"), "\n";
', "hello     |\n");

test_transpile("sprintf: %+d force sign", '
  print sprintf("%+d %+d", 42, -42), "\n";
', "+42 -42\n");

test_transpile("sprintf: %c character", '
  print sprintf("%c", 65), "\n";
', "A\n");

test_transpile("sprintf: %u unsigned", '
  print sprintf("%u", -1), "\n";
', "18446744073709551615\n");

test_transpile("sprintf: %.*f star precision", '
  print sprintf("%.*f", 3, 3.14159), "\n";
', "3.142\n");

test_transpile("sprintf: %*d star width", '
  print sprintf("%*d", 10, 42), "\n";
', "        42\n");

test_transpile("sprintf: %.5g precision with g", '
  print sprintf("%.5g", 3.14159), "\n";
', "3.1416\n");

test_transpile("sprintf: %020.10g zero-pad with g", '
  print sprintf("%020.10g", 3.14159), "\n";
', "00000000000003.14159\n");

test_transpile("sprintf: %X uppercase hex", '
  print sprintf("%X", 255), "\n";
', "FF\n");

test_transpile("sprintf: %% literal percent", '
  print sprintf("100%%"), "\n";
', "100%\n");

test_transpile("sprintf: multiple format args", '
  print sprintf("%s is %d years old", "Alice", 30), "\n";
', "Alice is 30 years old\n");

test_transpile("exponentiation overflow returns Inf", '
  print 9**9**9, "\n";
  print 2**1024, "\n";
  print (-2)**3, "\n";
', "Inf\nInf\n-8\n");

# String escape sequences: \x, \x{}, \o{}, octal
test_transpile("hex escape \\xHH", '
  print "\x41\x42\x43\n";
', "ABC\n");

test_transpile("hex escape \\x{HHHH}", '
  print "\x{4E}\x{69}\x{63}\x{65}\n";
', "Nice\n");

test_transpile("hex escape \\x{} with underscores and spaces", '
  print "\x{6_9}\x{ 4E }\n";
', "iN\n");

test_transpile("octal escape \\NNN", '
  print "\101\102\103\n";
', "ABC\n");

test_transpile("octal escape \\o{}", '
  print "a\o{120}b\n";
', "aPb\n");

test_transpile("bare \\0 is null char", '
  print "a\0b" eq "a\x00b" ? "yes" : "no", "\n";
', "yes\n");

# Non-ASCII hex escapes (bytes > 127 must produce valid UTF-8 in CL output)
test_transpile("non-ASCII hex escape \\xDD", '
  print length("\xdd"), "\n";
', "1\n");

test_transpile("non-ASCII hex escape \\xFF", '
  print "\xff" eq chr(255) ? "yes" : "no", "\n";
', "yes\n");

# Unknown escape \X → X (backslash dropped for non-special chars)
test_transpile("unknown escape stripped", '
  print "a\.b\*c\n";
', "a.b*c\n");

# Case-changing escapes in interpolated strings
test_transpile("\\U in interpolated string", '
  my $x = "hello"; print "\U$x\E world\n";
', "HELLO world\n");

test_transpile("\\L in interpolated string", '
  my $x = "HELLO"; print "\L$x\E world\n";
', "hello world\n");

test_transpile("\\u in interpolated string", '
  my $x = "hello"; print "\u$x\n";
', "Hello\n");

test_transpile("\\l in interpolated string", '
  my $x = "HELLO"; print "\l$x\n";
', "hELLO\n");

test_transpile("\\Q in interpolated string", '
  my $x = "hi.*"; print "\Q$x\E\n";
', "hi\\.\\*\n");

# Inline package block inside a function body (regression: section restore fix)
test_transpile("inline package inside function body", '
sub run {
    package Point {
        sub new { bless { x => $_[1], y => $_[2] }, $_[0] }
        sub x   { $_[0]->{x} }
    };
    my $p = Point->new(3, 4);
    print $p->x(), "\n";
    print "after\n";
}
run();
', "3\nafter\n");

# Inline package: local variable from outer scope visible after the block
test_transpile("inline package: outer vars visible after block", '
sub run {
    my $val = 42;
    package Dummy { sub noop { 1 } };
    print $val, "\n";
}
run();
', "42\n");

# Multiple inline packages in the same function
test_transpile("multiple inline packages in function", '
sub run {
    package Dog { sub speak { "woof" } };
    package Cat { sub speak { "meow" } };
    print Dog::speak(), "\n";
    print Cat::speak(), "\n";
}
run();
', "woof\nmeow\n");

# prototype() always returns undef (stub)
test_transpile("prototype() returns undef", '
  sub foo { }
  my $p = prototype(\&foo);
  print defined($p) ? "defined" : "undef", "\n";
', "undef\n");

# Unary + is a pure no-op disambiguator (perlop): must not numify or collapse a
# list. `map +(LIST)` must preserve the list; +(EXPR) must not become a 1-vector.
test_transpile("unary +: map +(LIST) preserves list", '
  my @x = map +($_, $_*10), (1,2,3);
  print "@x\n";
', "1 10 2 20 3 30\n");

test_transpile("unary +: map +(key,val) over hash", '
  my %h = (1,2,3,4);
  my @x = map +($_, $h{$_}), sort keys %h;
  print "@x\n";
', "1 2 3 4\n");

test_transpile("unary +: print +(EXPR) stays scalar", '
  print +(2+3), "\n";
', "5\n");

test_transpile("unary +: no-op on string (no numify)", '
  my $s = +"3abc";
  print "$s\n";
', "3abc\n");

test_transpile("unary +: parenthesised array spreads in list", '
  my @a = (1,2,3);
  my @b = +(@a);
  print "@b\n";
', "1 2 3\n");

# A %hash used as a list element must flatten to its key/value pairs (list
# context), not stringify as HASH(0x..). Covers join, foreach, and print.
test_transpile("hash flattens in join", '
  my %h = (e => 1);
  print join(":", %h), "\n";
', "e:1\n");

test_transpile("hash flattens in foreach", '
  my %h = (a => 1);
  for (%h) { print "$_ " }
  print "\n";
', "a 1 \n");

test_transpile("hash flattens mixed with scalars in join", '
  my %h = (x => 9);
  print join(",", "p", %h, "q"), "\n";
', "p,x,9,q\n");

# print takes a LIST: a bare @array / %hash must flatten, not stringify as a ref.
test_transpile("print flattens array", '
  my @a = (1,2,3);
  print @a, "\n";
', "123\n");

test_transpile("print flattens hash", '
  my %h = (e => 1);
  print %h, "\n";
', "e1\n");

test_transpile("print keeps a ref as a scalar", '
  my @a = (1,2,3);
  my $r = \@a;
  print ref($r), "\n";
', "ARRAY\n");

test_transpile("push flattens a hash", '
  my %h = (e => 1);
  my @a;
  push @a, %h;
  print "@a\n";
', "e 1\n");

test_transpile("map over a hash flattens it", '
  my %h = (a => 1);
  my @r = map { uc } %h;
  print join(",", sort @r), "\n";
', "1,A\n");

# ── M-F (s295) string-eval alias rule (ir-spec §9.1): a promoted/renamed
# file cell stays visible to string eval by its ORIGINAL name — read,
# write-back, dynamic (dark) eval — and a sibling block shadow neither
# hides it afterwards nor leaks into later evals (the s294 registry bug).

test_transpile("promoted cell: eval read/write-back survives sibling shadow", '
  my $x = 3;
  sub capx { $x }
  print eval(q{$x}), "\n";
  { my $x = 2; print eval(q{$x}), "\n"; }
  print eval(q{$x}), "\n";
  eval q{$x = 4};
  print "$x\n";
', "3\n2\n3\n4\n");

test_transpile("promoted cell: dynamic eval + write-back by original name", '
  my $x = 42;
  sub keepx { $x }
  my $code = q{$x + 15};
  print eval($code), "\n";
  eval q{$x = 84};
  print "$x\n";
', "57\n84\n");

test_transpile("sub defined in eval: nested dark eval reaches promoted cell", '
  my $x = "aa";
  sub touchx { $x }
  eval q{ sub do_ev { eval $_[0]; die $@ if $@ } };
  die $@ if $@;
  do_ev(q{print "$x\n"});
  $x++;
  do_ev(q{print "$x\n"});
  do_ev(q{$x = "zz"});
  print "$x\n";
', "aa\nab\nzz\n");

test_transpile("outer my enclosing same-name captured cell: evals see each scope", '
  my $x = 1;
  {
    my $x = 2;
    sub capinner { $x }
    print eval(q{$x}), "\n";
  }
  print eval(q{$x}), "\n";
', "2\n1\n");

# ── #63 (s295b): forward goto to a standalone label — lowered as
# (catch :pcl-goto-LBL prefix…) + throw, so the jump works DYNAMICALLY from
# inside a map/grep lambda (Perl: goto may leave a pseudo-block).
# perl-tests/array.t #132729 shape; v1 cannot compile it (naked (go)).

test_transpile("forward goto out of map lambda", '
  my @a = (1, 2, 3); my @r;
  map { push @r, $_; goto DONE; } @a;
  DONE: print "r=@r\n";
', "r=1\n");

test_transpile("two sequential forward-goto labels from lambdas", '
  map { goto L1; } (1);
  L1: print "one\n";
  map { goto L2; } (1);
  L2: print "two\n";
', "one\ntwo\n");

# ---- task #62 step 1: coercing compound assigns store to RAW slots via the
# ---- -raw macro twins ((p-incf-raw $s …)); these verify value parity with
# ---- perl across the raw regime and its exclusions.

# The intloop bench shape: += accumulator in a counting loop goes raw.
test_transpile("raw compound: += accumulator in range loop", '
  my $s = 0;
  for my $i (1..100) { $s += $i; }
  print "$s\n";
');

# Compound op family parity on raw slots (%= negative, **= negative, x=, <<=).
test_transpile("raw compound: op family parity", '
  my $m = -7; $m %= 3;  print "$m\n";
  my $p = 2;  $p **= -1; print "$p\n";
  my $r = "ab"; $r x= 3; print "$r\n";
  my $b = 6;  $b &= 3; $b <<= 2; print "$b\n";
  my $u; $u .= "x"; $u .= "y"; print "$u\n";
');

# ||= stores the RHS UNCHANGED (may be a ref) — must stay boxed/non-raw.
test_transpile("raw compound exclusion: ||= keeps ref semantics", '
  my $r; $r ||= [1,2]; push @$r, 3; print scalar(@$r), "\n";
');

# \$s ref-taken vetoes the raw slot: += must stay visible through the ref.
test_transpile("raw compound exclusion: ref-taken write-through", '
  my $s = 1; my $r = \$s; $s += 5; print "$$r\n";
');

# Seam (map block) and modifier positions keep the boxed compound path.
test_transpile("raw compound exclusion: seam and modifier positions", '
  my $e = 0; my @l = map { $e += $_ } (1,2,3); print "$e\n";
  my $q = 2; $q += 1 if $e; print "$q\n";
');

done_testing();
