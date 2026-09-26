#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# fatals-01.t — task #2103: perl's RUN-TIME fatals, died-vs-lived (s493's Q3
# battery, ~/pcl-agent-scratch/s493/q3/fatals.pl, 67 cases each in its own
# `eval {}`).  ONLY the died/lived column is compared — exact text is not a
# goal (USER s494: "fail in the same places"); the expected column is perl
# 5.40.3's, recorded at authoring time.  A row PCL still gets wrong sits in a
# TODO block NAMING ITS TASK — never deleted, never weakened (rule 5): when the
# task lands the row passes and prove reports it as an unexpected success,
# which is the signal to take it out of %TODO.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh PCLCore::transpile("$pl2cl $pl_file");
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

# Rows PCL still answers wrong, each with the task that owns it.
my %TODO = (
  'require false-returning file'   => '#1688 (require never enforces a true value)',
  'invalid regex at run time'      => '#2372 (a regex compile error is a warn + a wrong value)',
  'invalid quantifier regex'       => '#2372',
  'exists on non-element'          => '#2403 (strict refs on the vivifying chain / write path)',
  'local on lexical-free special'  => '#2405 (`$/ = \0` is accepted; needs a store hook on $/)',
);

my $battery = <<'PERL';
use strict; use warnings; no warnings; $| = 1;
sub t { my ($name, $code) = @_; my $r = eval { $code->(); 1 }; my $why = $r ? "" : ($@ =~ /^PCL:/ ? " [PCL-internal]" : ""); print sprintf("%-34s %s%s\n", $name, ($r ? "LIVED" : "died"), $why) }
my ($undef, $str, $num, $aref, $href, $cref, $obj) = (undef, "text", 5, [1,2], {k=>1}, sub {1}, bless({}, "Klass"));
{ package Klass; sub hello { "hi" } }
t("deref undef as ARRAY (rvalue)",  sub { my @x = @$undef; });
t("deref undef as HASH (rvalue)",   sub { my @k = keys %$undef; });
t("string as ARRAY ref (strict)",   sub { my @x = @$str; });
t("string as HASH ref (strict)",    sub { my $v = $str->{k}; });
t("string as CODE ref (strict)",    sub { $str->(); });
t("string as SCALAR ref (strict)",  sub { my $v = $$str; });
t("number as ARRAY ref",            sub { my $v = $num->[0]; });
t("hashref used as ARRAY",          sub { my $v = $href->[0]; });
t("arrayref used as HASH",          sub { my $v = $aref->{k}; });
t("arrayref used as CODE",          sub { $aref->(); });
t("coderef used as HASH",           sub { my $v = $cref->{k}; });
t("hashref used as SCALAR ref",     sub { my $v = $$href; });
t("method on undef",                sub { $undef->hello; });
t("method on unblessed ref",        sub { $aref->hello; });
t("method on empty string",         sub { my $e = ""; $e->hello; });
t("missing method",                 sub { $obj->nope; });
t("missing method on class name",   sub { Klass->nope; });
t("missing class",                  sub { No::Such::Class->new; });
t("undefined sub call",             sub { no strict "refs"; nosuchsub(1); });
t("undefined sub via &\$name",      sub { no strict "refs"; my $n = "nosuch2"; &$n(); });
t("undef coderef call",             sub { $undef->(); });
t("division by zero",               sub { my $z = 0; my $v = 1 / $z; });
t("modulus zero",                   sub { my $z = 0; my $v = 1 % $z; });
t("sqrt of negative",               sub { my $v = sqrt(-1); });
t("log of zero",                    sub { my $v = log(0); });
t("modify read-only literal",       sub { for my $x (1) { $x = 2 } });
t("modify constant via \$_ alias",  sub { $_++ for (1, 2); });
t("chop on literal alias",          sub { for ("abc") { chop } });
t("require missing module",         sub { require No::Such::Module; });
t("require false-returning file",   sub { my $f = "/tmp/s493-false.$$.pm"; open my $o, ">", $f or die; print $o "0;\n"; close $o; my $ok = eval { require $f; 1 }; unlink $f; die $@ if !$ok; });
t("invalid regex at run time",      sub { my $p = "(unclosed"; my $m = "x" =~ /$p/; });
t("invalid quantifier regex",       sub { my $p = "*abc"; my $m = "x" =~ /$p/; });
t("sort sub returns non-number ok", sub { my @s = sort { "a" } 3, 1, 2; });
t("last outside a loop",            sub { last; });
t("goto missing label",             sub { goto NOWHERE; });
t("negative array length",          sub { my @a = (1); $#a = -5; });
t("splice past end (lvalue ok)",    sub { my @a = (1); splice(@a, 5, 0, 9); });
t("substr outside string lvalue",   sub { my $s = "ab"; substr($s, 10, 1) = "x"; });
t("substr outside string rvalue",   sub { my $s = "ab"; my $v = substr($s, 10, 1); });
t("vec negative offset",            sub { my $s = ""; vec($s, -1, 8) = 1; });
t("sprintf missing arg (warn only)", sub { my $v = sprintf("%d %d", 1); });
t("pack bad template",              sub { my $v = pack("y", 1); });
t("unpack bad template",            sub { my @v = unpack("y", "x"); });
t("pack 'x' outside string unpack", sub { my @v = unpack("x5 a", "ab"); });
t("hash in list assignment odd ok", sub { my %h = (1, 2, 3); });
t("exists on non-element",          sub { my $v = exists $href->{k}{j}{i}; });
t("local on lexical-free special",  sub { local $/ = \0; });
t("bless into a reference",         sub { my $o = bless {}, $aref; });
t("bless non-reference",            sub { my $o = bless "str", "Klass"; });
t("can't locate object via string", sub { my $c = "Klass"; $c->nope; });
t("open dies only with or-die",     sub { open(my $f, "<", "/no/such/file") or die "cannot: $!"; });
t("close unopened ok",              sub { close(NOFH); });
t("print to closed handle ok",      sub { open my $f, ">", "/tmp/s493-c.$$" or die; close $f; print $f "x"; unlink "/tmp/s493-c.$$"; });
t("readline on unopened ok",        sub { my $l = <NOFH2>; });
t("die with object",                sub { die bless({}, "Klass"); });
t("die in sort block",              sub { my @s = sort { die "in sort\n" } 2, 1; });
t("die in map propagates",          sub { my @m = map { die "in map\n" if $_ == 2; $_ } 1..3; });
t("nested eval rethrow",            sub { eval { die "inner\n" }; die "outer: $@" if $@; });
t("\$SIG{__DIE__} does not swallow", sub { local $SIG{__DIE__} = sub { }; die "still dies\n"; });
t("exit inside eval not trapped",   sub { 1 });
t("deep recursion 5000 ok",         sub { my $f; $f = sub { $_[0] ? $f->($_[0] - 1) : 0 }; $f->(5000); });
t("array index huge negative",      sub { my @a = (1, 2); $a[-5] = 1; });
t("use of freed / undef glob deref", sub { my $g; my @x = @{*$g}; });
t("string increment on ref ok",     sub { my $r = []; my $v = $r + 1; });
t("numeric op on undef ok",         sub { my $v = $undef + 1; });
t("string repetition negative ok",  sub { my $v = "a" x -1; });
t("join undef ok",                  sub { my $v = join(",", undef, 1); });
print "done\n";
PERL

my $perl_says = <<'WANT';
deref undef as ARRAY (rvalue)      died
deref undef as HASH (rvalue)       LIVED
string as ARRAY ref (strict)       died
string as HASH ref (strict)        died
string as CODE ref (strict)        died
string as SCALAR ref (strict)      died
number as ARRAY ref                died
hashref used as ARRAY              died
arrayref used as HASH              died
arrayref used as CODE              died
coderef used as HASH               died
hashref used as SCALAR ref         died
method on undef                    died
method on unblessed ref            died
method on empty string             died
missing method                     died
missing method on class name       died
missing class                      died
undefined sub call                 died
undefined sub via &$name           died
undef coderef call                 died
division by zero                   died
modulus zero                       died
sqrt of negative                   died
log of zero                        died
modify read-only literal           died
modify constant via $_ alias       died
chop on literal alias              died
require missing module             died
require false-returning file       died
invalid regex at run time          died
invalid quantifier regex           died
sort sub returns non-number ok     LIVED
last outside a loop                died
goto missing label                 died
negative array length              LIVED
splice past end (lvalue ok)        LIVED
substr outside string lvalue       died
substr outside string rvalue       LIVED
vec negative offset                died
sprintf missing arg (warn only)    LIVED
pack bad template                  died
unpack bad template                died
pack 'x' outside string unpack     died
hash in list assignment odd ok     LIVED
exists on non-element              died
local on lexical-free special      died
bless into a reference             died
bless non-reference                died
can't locate object via string     died
open dies only with or-die         died
close unopened ok                  LIVED
print to closed handle ok          LIVED
readline on unopened ok            LIVED
die with object                    died
die in sort block                  died
die in map propagates              died
nested eval rethrow                died
$SIG{__DIE__} does not swallow     died
exit inside eval not trapped       LIVED
deep recursion 5000 ok             LIVED
array index huge negative          died
use of freed / undef glob deref    died
string increment on ref ok         LIVED
numeric op on undef ok             LIVED
string repetition negative ok      LIVED
join undef ok                      LIVED
done
WANT

# The read-only LITERAL rows (#1391 / #2103), with the shapes that must stay
# WRITABLE (a range, an array, a variable) — perl 5.40.3's answers.
my $ro_prog = <<'PERL';
sub t { my ($n, $c) = @_; my $r = eval { $c->(); 1 }; my $e = $@; $e =~ s/ at .*//s; print "$n: ", ($r ? "lived" : "died $e"), "\n" }
my $v = "v";
t("assign my",       sub { for my $x (1) { $x = 2 } });
t("postinc topic",   sub { $_++ for (1, 2) });
t("chop topic",      sub { for ("abc") { chop } });
t("read only",       sub { my $s = 0; for my $x (1, 2, 3) { $s += $x } die "sum\n" if $s != 6 });
t("range write",     sub { for (1 .. 3) { $_++ } });
t("mixed first",     sub { for my $x ($v, "a") { $x .= "!" } });
t("mixed var ok",    sub { my $w = "w"; for my $x ($w) { $x .= "!" } die "no\n" if $w ne "w!" });
t("s/// on literal", sub { for my $s ("a", "b") { $s =~ s/a/b/ } });
t("tr on literal",   sub { for (qw(a b)) { tr/a/b/ } });
t("qw read",         sub { my $o = ""; for (qw(a b)) { $o .= $_ } die "no\n" if $o ne "ab" });
t("sub reads alias", sub { sub rd { $_[0] } my $o = ""; for my $x (1, 2) { $o .= rd($x) } die "no\n" if $o ne "12" });
t("sub writes alias", sub { sub wr { $_[0] = 9 } for my $x (1, 2) { wr($x) } });
t("ref to literal",  sub { my @r; for (1, 2) { push @r, \$_ } die "no\n" if ${$r[1]} != 2 });
t("chomp literal",   sub { for ("x\n") { chomp } });
t("array elems ok",  sub { my @a = (1, 2); $_ *= 2 for @a; die "no\n" if "@a" ne "2 4" });
t("list+array",      sub { my @a = (1); for my $x (@a, 5) { $x++ } });
t("nested last",     sub { my $n = 0; for my $x (1, 2, 3) { $n++; last if $x == 2 } die "no\n" if $n != 2 });
t("return value",    sub { my @o = map { $_ * 2 } (1, 2); for (@o) { $_++ } die "no\n" if "@o" ne "3 5" });
PERL

my $ro_want = <<'WANT';
assign my: died Modification of a read-only value attempted
postinc topic: died Modification of a read-only value attempted
chop topic: died Modification of a read-only value attempted
read only: lived
range write: lived
mixed first: died Modification of a read-only value attempted
mixed var ok: lived
s/// on literal: died Modification of a read-only value attempted
tr on literal: died Modification of a read-only value attempted
qw read: lived
sub reads alias: lived
sub writes alias: died Modification of a read-only value attempted
ref to literal: lived
chomp literal: died Modification of a read-only value attempted
array elems ok: lived
list+array: died Modification of a read-only value attempted
nested last: lived
return value: lived
WANT

my %want = map { /^(.*?)\s+(LIVED|died)\b/ ? ($1 => $2) : () } split /\n/, $perl_says;
my %got  = map { /^(.*?)\s+(LIVED|died)\b/ ? ($1 => $2) : () } split /\n/, run_cl($battery);
for my $name (map { /^(.*?)\s+(?:LIVED|died)\b/ ? $1 : () } split /\n/, $perl_says) {
    local $TODO = $TODO{$name};
    is($got{$name} // '<missing>', $want{$name}, $name);
}
my @rw = split /\n/, $ro_want;
my @rg = split /\n/, run_cl($ro_prog);
for my $i (0 .. $#rw) {
    my ($tag) = $rw[$i] =~ /^([^:]+)/;
    is($rg[$i] // '<missing>', $rw[$i], "read-only literal: $tag");
}
done_testing();
