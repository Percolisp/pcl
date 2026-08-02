#!/usr/bin/env perl
# writes-args-01.t — task #189: a sub that writes through @_ writes its
# CALLER's variables, and the element targets of s/// and tr/// are the
# element itself.  Both halves were SILENT wrongs.
#
#   sub setit { $_[0] = "changed" }        # perl: the caller's $x changes
#   my $x = "orig"; setit($x);             # PCL: $x stayed "orig"
#
# perl's @_ elements are ALIASES.  PCL can only honour that when the caller
# passed a BOX, and boxing every argument is off the table, so the callee's
# body is scanned once (Parser2::_sub_writes_args), the fact rides sub_info
# as `writes_args`, and VarAnnotator turns it into an `arg-to-writer` boxing
# event at the call sites — the same mechanism `chomp $x` already uses.
#
# The scan is CONSERVATIVE (fable-answers-s323.md §1.1): every @_ / $_[N]
# occurrence must be a proven read, and the aliasing escapes (\$_[N], \@_,
# `&callee;`, `goto &sub`, handing @_ onward) count as writes.  A false
# positive costs one boxed argument; a false negative is a silent wrong.
#
# The INVERSE rows matter as much as the positive ones: a read-only sub's
# arguments must stay RAW, or the fact would be a blanket boxing rule with
# extra steps.
#
# NOT covered (still broken, task #209): substr as an lvalue and 4-arg substr
# on an element — `substr($_[0],0,1) = "Z"` and `substr($_[0],0,1,"Q")` reach
# the callee correctly boxed, but the callee lowers substr's target as a
# VALUE, so the write is lost.  Same missing mechanism this file fixes for
# s///, one table further along (%lvalue_funcs in ExprToCL).

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

plan tests => 13;

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---------------------------------------------- 1. the write shapes

is(run_cl(q{sub setit  { $_[0] = "I" }
sub incit  { $_[1]++ }
sub chompit{ chomp $_[0] }
my $a = "orig"; setit($a);
my $n = 5;      incit(0, $n);
my $c = "line
";              chompit($c);
print "$a|$n|[$c]\n";
}), "I|6|[line]\n", 'assignment, ++ and chomp through @_ reach the caller');

is(run_cl(q{sub inner { $_[0] = "I" }
sub esc    { my $r = \$_[0]; $$r = "E" }
sub outer  { &inner; }
sub jump   { goto &inner; }
sub pass   { inner(@_); }
sub loop   { for my $e (@_) { $e = uc $e } }
my $a = "abc"; esc($a);
my $b = "abc"; outer($b);
my $c = "abc"; jump($c);
my $d = "abc"; pass($d);
my $e = "abc"; loop($e);
print "$a|$b|$c|$d|$e\n";
}), "E|I|I|I|ABC\n",
   'the aliasing ESCAPES count as writes: \$_[0], &callee;, goto &sub, @_ onward, foreach alias');

# INVERSE: a read-only callee must leave its caller's variable RAW.  Checked on
# the EMISSION, because the runtime answer is identical either way — the whole
# point is that the fact must not become a blanket boxing rule.
my $ro = transpile(q{sub ro { my ($v) = @_; return uc $v }
my $z = "keep";
print ro($z), "\n";
});
like($ro, qr/\(let \(\(\$z "keep"\)\)/,
   'INVERSE: a read-only sub leaves its argument a RAW slot (no make-p-box)');

my $rw = transpile(q{sub rw { $_[0] = "x" }
my $z = "keep";
rw($z);
});
like($rw, qr/\(let \(\(\$z \(make-p-box/,
   'a writing sub DOES box the caller variable it is handed');

# INVERSE, second shape: the read-only uses of @_ that a naive scan would
# flag — a list-assignment copy, a value-consuming builtin, $#_ arithmetic.
my $ro2 = transpile(q{sub r1 { my ($a,$b) = @_; return "$a$b" }
sub r2 { return join(",", @_) }
sub r3 { return scalar(@_) + $#_ }
sub r4 { my $s = ""; $s .= $_ for @_; return $s }
my $p = "p";
print r1($p,"q"), r2($p,"q"), r3($p,"q"), r4($p,"q"), "\n";
});
like($ro2, qr/\(let \(\(\$p "p"\)\)/,
   'INVERSE: `my (…) = @_`, join(@_), scalar/$#_ and a read-only foreach stay reads');

# ------------------------------------- 2. s/// and tr/// on an ELEMENT

is(run_cl(q{my @a = ("aXb", "cXd");
for my $i (0..$#a) { $a[$i] =~ s/X/-/ }
my %h = (k => "aXb");
$h{k} =~ s/X/-/;
my %g = (k => "aXb");
$g{k} =~ tr/X/-/;
print "$a[0]|$a[1]|$h{k}|$g{k}\n";
}), "a-b|c-d|a-b|a-b\n",
   's/// and tr/// bound to an array or hash ELEMENT modify the element');

is(run_cl(q{my @a = ("aXb");
my $hit = ($a[0] =~ /X/) ? "y" : "n";
print "$hit|$a[0]\n";
}), "y|aXb\n",
   'INVERSE: a plain MATCH on an element does not write it');

# Taking the element as an lvalue CREATES it, so which ops take it matters:
# perl vivifies for s/// and a modifying tr///, and does NOT for a counting
# `tr/N/N/`, for /r, or for a match.  Getting this wrong cost perl-tests/tr.t
# two passing rows ("doesn't extend the array") — caught by #204's LOST bucket.
is(run_cl(q{my @a; eval '$a[2] =~ s/x/y/';   my $s  = scalar @a;
my @b; eval '$b[2] =~ tr/N/N/';               my $tc = scalar @b;
my @c; eval '$c[2] =~ tr/x/y/';               my $tm = scalar @c;
my @d; eval '$d[2] =~ /x/';                   my $m  = scalar @d;
my @e; eval '$e[2] =~ tr/x/y/r';              my $r  = scalar @e;
print "$s|$tc|$tm|$m|$r\n";
}), "3|0|3|0|0\n",
   'element vivification follows perl exactly: s/// and modifying tr/// create it, count-only tr, /r and m// do not');

is(run_cl(q{sub strip { $_[0] =~ s{/+$}{} }
my $p = "/a/b/";
strip($p);
print "$p\n";
}), "/a/b\n", 's/// through @_ — the two halves together (core File::Basename shape)');

# ------------------------------------- 3. the shim this deletes

is(run_cl(q{use File::Basename;
printf "%s|%s|%s\n", dirname("/a/b/c"), dirname("/a/b/c/"), basename("/a/b/c.txt", ".txt");
}), "/a/b|/a/b|c\n",
   'core File::Basename (no PCL shim) answers dirname correctly');

is(run_cl(q{use File::Basename;
my ($n,$p,$s) = fileparse("/a/b/c.txt", qr/\.[^.]*/);
print "$n|$p|$s\n";
}), "c|/a/b/|.txt\n", 'core File::Basename fileparse');

# ------------------------------------- 4. the fact does not leak

is(run_cl(q{sub w { $_[0] = "W" }
sub r { return "R" . $_[0] }
my $a = "a"; my $b = "b";
w($a);
my $out = r($b);
print "$a|$b|$out\n";
}), "W|b|Rb\n",
   'two subs in one file: only the writer aliases; the reader leaves its arg alone');

is(run_cl(q{sub w { $_[0] = "W" }
my @a = ("x");
w($a[0]);
print "$a[0]\n";
}), "W\n", 'an ELEMENT handed to a writing sub is aliased too (defelem, task #131)');
