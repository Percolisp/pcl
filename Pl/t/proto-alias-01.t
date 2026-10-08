#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# proto-alias-01.t -- task #2860: a `$` PROTOTYPE slot imposes scalar CONTEXT
# on its argument expression and nothing else, so @_ still ALIASES a scalar
# lvalue argument (a scalar variable, an element of a named container, $_[N],
# a scalar assignment) exactly as it does without a prototype.
#
#   sub g ($) { $_[0] .= "!" }   my $t = "a"; g($t);   # perl: a!   PCL was: a
#
# Two independent copies, both SILENT wrongs:
#   1. a prototyped sub had no sub_info record, so its writes_args fact never
#      reached VarAnnotator and the caller's `$t` stayed a RAW slot (a value);
#   2. an element / scalar-assignment argument under a `$` slot was wrapped in
#      p-scalar, which unboxes -- a copy even with every optimisation off.
# Half 2 is Text::Balanced's shape: `sub { extract_variable($_[0], '') }` hands
# its $_[0] to a `(;$$)` sub that takes `\$_[0]` and sets pos() on it; with a
# copy, extract_multiple's field loop never advanced (#2067 / #1512).
#
# Every expected string below was probed on perl 5.40.3 and uses no spelling
# newer than 5.36 (CI's perl is 5.38).
#
# NOT here: `$_[0] =~ /x/g; pos($_[0])` straight on an @_ element records no
# pos with or without a prototype -- task #1567; the rows below take pos
# through `\$_[0]`, the spelling Text::Balanced uses.

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

plan tests => 9;

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

# ------------------------------------------- 1. half 1: the writes_args fact

is(run_cl(q{sub g  ($)   { $_[0] .= "!" }
sub g2 ($$)  { $_[0] .= "!"; $_[1] .= "?" }
sub go (;$)  { $_[0] .= "o" }
sub h  ($)   { $_[0] = "set" }
my $t = "a"; g($t);
my ($u, $v) = ("a", "b"); g2($u, $v);
my $y = "a"; &g($y);
my $o = "a"; go($o);
my $hh = "a"; h($hh);
print "$t|$u $v|$y|$o|$hh\n";
}), "a!|a! b?|a!|ao|set\n",
   'a write through $_[N] in a ($) / ($$) / (;$) sub and an &-call reaches the caller');

# ------------------------------------------- 2. half 2: no p-scalar copy

is(run_cl(q{sub g ($) { $_[0] .= "!" }
sub outer ($) { g($_[0]) }
my @a = ("q"); g($a[0]);
my %h = (k => "a"); g($h{k});
my $o = "a"; outer($o);
my $z = "a"; g(my $copy = $z);
my $c; g($c = "b");
my $n = "n"; g($n .= "x");
print "$a[0]|$h{k}|$o|$z $copy|$c|$n\n";
}), "q!|a!|a!|a a!|b!|nx!\n",
   'an element, a chain through $_[0], a scalar assignment and an op-assign alias under ($)');

is(run_cl(q{sub ev (;$$) { my $r = \$_[0]; $$r =~ /x/g; pos($$r) }
my $f = sub { ev($_[0], '') };
sub viaplain { ev($_[0], '') }
my $s = "axbx";
pos($s) = undef; my $p1 = $f->($s);      my $c1 = pos($s) // 'undef';
pos($s) = undef; my $p2 = viaplain($s);  my $c2 = pos($s) // 'undef';
pos($s) = undef; my $p3 = ev($s, '');    my $c3 = pos($s) // 'undef';
my %h = (k => "axbx"); my $p4 = ev($h{k}); my $c4 = pos($h{k}) // 'undef';
print "$p1 $c1|$p2 $c2|$p3 $c3|$p4 $c4\n";
}), "2 2|2 2|2 2|2 2\n",
   'the Text::Balanced shape: a closure / named sub hands $_[0] to a (;$$) sub that sets pos via \$_[0]');

# ------------------------------------------- 3. INVERSE rows

is(run_cl(q{sub cnt ($) { $_[0] }
sub w ($) { $_[0] .= "!" }
sub rd ($) { my $v = $_[0]; defined $v ? 1 : 0 }
my @a = (1, 2, 3);
my %h; rd($h{zz});
my $viv_read = exists $h{zz} ? "yes" : "no";
my %g; w($g{zz});
print cnt(@a), "|", cnt(1+2), "|", cnt("lit"), "|$viv_read|",
      (exists $g{zz} ? "yes:$g{zz}" : "no"), "\n";
}), "3|3|lit|no|yes:!\n",
   'INVERSE: f(@a) under ($) passes the COUNT, an expression / a literal pass their value, a read creates no key (the last field, a WRITE vivifying $g{zz}, is the positive half)');

is(run_cl(q{use feature 'signatures'; no warnings;
sub sg ($x, $y) { $x .= "!"; return "$x$y" }
my $v = "a"; my $r = sg($v, "b");
print "$v|$r\n";
}), "a|a!b\n",
   'INVERSE: a signature binds COPIES -- the caller variable is untouched');

my $ro = transpile(q{sub ro ($) { my $v = shift; return uc $v }
my $z = "keep";
print ro($z), "\n";
});
like($ro, qr/\(p-let \(\(\$z :scalar "keep"\)\)/,
   'INVERSE: a prototyped sub that only READS leaves its argument a RAW slot');

my $rw = transpile(q{sub rw ($) { $_[0] = "x" }
my $z = "keep";
rw($z);
});
like($rw, qr/\(p-let \(\(\$z :box \(make-p-box/,
   'a prototyped sub that WRITES @_ boxes the caller variable it is handed');

my $sig = transpile(q{use feature 'signatures'; no warnings;
sub sg ($x) { return $x . "!" }
my $z = "keep";
print sg($z), "\n";
});
like($sig, qr/\(p-let \(\(\$z :scalar "keep"\)\)/,
   'INVERSE: a signature sub (no @_ in its body) leaves its argument RAW');

my $elem = transpile(q{sub ev (;$$) { my $r = \$_[0]; pos($$r) }
sub cl { ev($_[0], '') }
cl("x");
});
unlike($elem, qr/\(p-scalar \(p-aref-argbox/,
   'an element argument under a $ slot is passed as its argbox, not a p-scalar copy');
