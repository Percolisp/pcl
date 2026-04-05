#!/usr/bin/env perl
# overload-01.t — use overload operator overloading

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 19;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}


# ── Stringify overload '""' ───────────────────────────────────────────────

test_cl('stringify overload ""',
    'package MyStr;
     use overload q("") => \&str;
     sub new { bless { s => $_[1] }, $_[0] }
     sub str { "<<" . $_[0]->{s} . ">>" }
     package main;
     my $o = MyStr->new("hello");
     print $o, "\n";',
    "<<hello>>\n");

# ── Single-line use overload (regression: was parsing OK) ────────────────

test_cl('single-line use overload',
    'package MyS;
     use overload q("") => sub { "X" };
     sub new { bless {}, $_[0] }
     package main;
     my $o = MyS->new;
     print $o, "\n";',
    "X\n");

# ── Multi-line use overload (was crashing with "=> is unbound") ──────────

test_cl('multi-line use overload',
    'package MyNum;
     use overload
       "+"  => \&add,
       "0+" => \&numify,
       q("") => \&stringify,
       "<=>" => \&spaceship;
     sub new { bless { val => $_[1] }, $_[0] }
     sub add { MyNum->new($_[0]->{val} + $_[1]->{val}) }
     sub numify { $_[0]->{val} }
     sub stringify { "MyNum(" . $_[0]->{val} . ")" }
     sub spaceship { $_[0]->{val} <=> $_[1]->{val} }
     package main;
     my $a = MyNum->new(3);
     my $b = MyNum->new(5);
     print $a + $b, "\n";',
    "MyNum(8)\n");

# ── Numify overload '0+' ─────────────────────────────────────────────────

test_cl('numify overload 0+',
    'package MyN;
     use overload "0+" => \&num, q("") => \&str;
     sub new { bless { n => $_[1] }, $_[0] }
     sub num { $_[0]->{n} * 2 }
     sub str { "MyN=" . $_[0]->{n} }
     package main;
     my $o = MyN->new(7);
     print $o + 1, "\n";',
    "15\n");

# ── Bool overload ─────────────────────────────────────────────────────────

test_cl('bool overload',
    'package MyBool;
     use overload "bool" => \&as_bool, q("") => \&str;
     sub new { bless { v => $_[1] }, $_[0] }
     sub as_bool { $_[0]->{v} > 0 }
     sub str { "MB(" . $_[0]->{v} . ")" }
     package main;
     my $t = MyBool->new(5);
     my $f = MyBool->new(-1);
     print $t ? "yes" : "no", "\n";
     print $f ? "yes" : "no", "\n";',
    "yes\nno\n");

# ── Arithmetic operators ──────────────────────────────────────────────────

test_cl('subtraction overload -',
    'package MyInt;
     use overload "-" => \&sub_, q("") => \&str;
     sub new { bless { v => $_[1] }, $_[0] }
     sub sub_ {
       my ($a, $b, $r) = @_;
       $r ? MyInt->new($b - $a->{v}) : MyInt->new($a->{v} - $b)
     }
     sub str { $_[0]->{v} }
     package main;
     my $a = MyInt->new(10);
     print $a - 3, "\n";',
    "7\n");

test_cl('multiplication overload *',
    'package MyMul;
     use overload "*" => sub { MyMul->new($_[0]->{v} * $_[1]) }, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyMul->new(6);
     print $o * 7, "\n";',
    "42\n");

test_cl('negation overload neg',
    'package MyNeg;
     use overload "neg" => sub { MyNeg->new(-$_[0]->{v}) }, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyNeg->new(5);
     print -$o, "\n";',
    "-5\n");

# ── Comparison overload ───────────────────────────────────────────────────

test_cl('spaceship overload <=>',
    'package MyCmp;
     use overload "<=>" => \&cmp_, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     sub cmp_ { $_[2] ? $_[1] <=> $_[0]->{v} : $_[0]->{v} <=> $_[1] }
     package main;
     my @objs = map { MyCmp->new($_) } (3, 1, 4, 1, 5, 9);
     my @sorted = sort { $a <=> $b } @objs;
     print join(" ", @sorted), "\n";',
    "1 1 3 4 5 9\n");

test_cl('numeric eq overload ==',
    'package MyEq;
     use overload "0+" => sub { $_[0]->{v} }, q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $a = MyEq->new(5);
     print(($a == 5 ? "eq" : "ne"), "\n");
     print(($a == 6 ? "eq" : "ne"), "\n");',
    "eq\nne\n");

# ── String comparison overload ────────────────────────────────────────────

test_cl('cmp overload for string sort',
    'package MyWord;
     use overload "cmp" => \&mycmp, q("") => \&str;
     sub new { bless { w => $_[1] }, $_[0] }
     sub str { $_[0]->{w} }
     sub mycmp {
       my ($self, $other, $rev) = @_;
       my $sw = $self->{w};
       my $ow = ref($other) eq "MyWord" ? $other->{w} : $other;
       $rev ? $ow cmp $sw : $sw cmp $ow
     }
     package main;
     my @objs = map { MyWord->new($_) } qw(banana apple cherry);
     my @sorted = sort { $a cmp $b } @objs;
     print join(" ", @sorted), "\n";',
    "apple banana cherry\n");

# ── Concatenation overload '.' ────────────────────────────────────────────

test_cl('concatenation overload .',
    'package MyCat;
     use overload "." => \&concat, q("") => sub { $_[0]->{s} };
     sub new { bless { s => $_[1] }, $_[0] }
     sub concat {
       my ($a, $b, $r) = @_;
       $r ? MyCat->new($b . $a->{s}) : MyCat->new($a->{s} . $b)
     }
     package main;
     my $o = MyCat->new("world");
     print "hello " . $o, "\n";',
    "hello world\n");

# ── Fallback via 0+ / <=> ─────────────────────────────────────────────────

test_cl('fallback numeric comparison via 0+',
    'package MyFB;
     use overload "0+" => sub { $_[0]->{n} }, q("") => sub { $_[0]->{n} };
     sub new { bless { n => $_[1] }, $_[0] }
     package main;
     my $a = MyFB->new(3);
     my $b = MyFB->new(7);
     print(($a < $b ? "less" : "not less"), "\n");
     print(($b > $a ? "more" : "not more"), "\n");',
    "less\nmore\n");

# ── overload::StrVal bypasses stringify ───────────────────────────────────

test_cl('overload::StrVal bypasses stringify overload',
    'package MyV;
     use overload q("") => sub { "OVERLOADED" };
     sub new { bless {}, $_[0] }
     package main;
     my $o = MyV->new;
     my $s = overload::StrVal($o);
     print(($s =~ /^MyV=HASH/ ? "ok" : "bad: $s"), "\n");',
    "ok\n");

# ── overload::Overloaded query ────────────────────────────────────────────

test_cl('overload::Overloaded returns true for overloaded object',
    'package MyOL;
     use overload q("") => sub { "X" };
     sub new { bless {}, $_[0] }
     package main;
     my $o = MyOL->new;
     print(overload::Overloaded($o) ? "yes" : "no", "\n");
     my $plain = bless {}, "PlainClass";
     print(overload::Overloaded($plain) ? "yes" : "no", "\n");',
    "yes\nno\n");

# ── Inheritance: subclass inherits parent overloads ───────────────────────

test_cl('subclass inherits stringify overload',
    'package Base;
     use overload q("") => sub { "Base:" . $_[0]->{x} };
     sub new { bless { x => $_[1] }, $_[0] }
     package Child;
     our @ISA = ("Base");
     sub new { bless { x => $_[1] }, $_[0] }
     package main;
     my $c = Child->new(42);
     print $c, "\n";',
    "Base:42\n");

# ── Anonymous sub handler (inline) ───────────────────────────────────────

test_cl('anonymous sub as overload handler',
    'package MyAnon;
     use overload q("") => sub { "anon:" . $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyAnon->new(99);
     print $o, "\n";',
    "anon:99\n");

# ── String eq overload ────────────────────────────────────────────────────

test_cl('string eq via cmp overload',
    'package MyStr2;
     use overload "cmp" => sub { $_[0]->{s} cmp $_[1] }, q("") => sub { $_[0]->{s} };
     sub new { bless { s => $_[1] }, $_[0] }
     package main;
     my $a = MyStr2->new("foo");
     print(($a eq "foo" ? "eq" : "ne"), "\n");
     print(($a eq "bar" ? "eq" : "ne"), "\n");',
    "eq\nne\n");

# ── Division overload / ───────────────────────────────────────────────────

test_cl('division overload /',
    'package MyDiv;
     use overload "/" => sub { $_[2] ? $_[1] / $_[0]->{v} : $_[0]->{v} / $_[1] },
                  q("") => sub { $_[0]->{v} };
     sub new { bless { v => $_[1] }, $_[0] }
     package main;
     my $o = MyDiv->new(10);
     print $o / 2, "\n";',
    "5\n");
