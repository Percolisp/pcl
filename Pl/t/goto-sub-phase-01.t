#!/usr/bin/env perl
# goto-sub-phase-01.t — the four blockers behind Capture-Tiny (task #199),
# every one of them a SILENT wrong answer before s328:
#
#   1. `goto \&NAME` / `goto $coderef` lowered to p-goto-computed, which was
#      documented as "silently ignore" — the sub returned undef, no error, no
#      output.  Capture::Tiny's whole public API is `goto \&_capture_tee`.
#   2. a `local(...)` inside a BEGIN/END/CHECK/INIT/UNITCHECK block opened a
#      (let …) nobody closed, so v1 emitted one paren too few and the block's
#      (push (lambda …) *end-blocks*) swallowed every later top-level form.
#   3. require_ok/use_ok reported ok WITHOUT LOADING ANYTHING.
#   4. an `open my $fh, …` was vetoed out of its own let-binding by any
#      sibling sub that merely DECLARED the same name — leaving both sides
#      referring to a package global that is never emitted.
#
# Inverse guards included where the inverse is the interesting half (the
# embedded-my veto must still fire for a sub that only REFERENCES the name).

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

plan tests => 17;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^# PCL Test library loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---------------------------------------------------------------- 1. goto EXPR

my $impl = q{sub impl { return "impl(" . join(",", @_) . ")" }};

is(run_cl(qq{$impl
sub g { unshift \@_, 5, 6; goto \\&impl; }
print g(9), "\\n";
}), "impl(5,6,9)\n", 'goto \\&NAME tail-calls with the current \@_');

is(run_cl(qq{$impl
my \$cr = \\&impl;
sub g { goto \$cr; }
print g(1,2), "\\n";
}), "impl(1,2)\n", 'goto $coderef tail-calls');

is(run_cl(qq{sub g { my \$c = sub { "anon(" . join(",",\@_) . ")" }; goto \$c; }
print g(7), "\\n";
}), "anon(7)\n", 'goto $anon_coderef tail-calls');

# The half `goto &NAME` always handled — kept so a regression in the shared
# p-goto-sub mechanism shows up on both spellings.
is(run_cl(qq{$impl
sub g { goto &impl; }
print g(1,2), "\\n";
}), "impl(1,2)\n", 'goto &NAME still tail-calls');

# INVERSE: a non-coderef operand is a computed LABEL goto, which CL cannot
# express.  It must SAY SO by name on stderr — announced, not silent (the #155
# tie shape; a die would abort whole files over one rare construct, measured:
# state.t 157/166 rows with the warning vs 69 with a die).
like(run_cl(q{my $target = "SOMEWHERE";
sub g { goto $target; return "fellthrough"; }
print g(), "\n";
}), qr/goto to the computed LABEL "SOMEWHERE" is not supported/,
   'goto to a computed LABEL names itself on stderr instead of doing nothing');

# The shape Capture::Tiny actually uses: the sub is DEFINED BY STRING EVAL and
# its body is nothing but `unshift @_, …; goto \&_real`.
is(run_cl(q{sub impl { return "impl(" . join(",", map { ref($_) ? "CODE" : $_ } @_) . ")" }
eval "sub g { unshift \@_, 5; goto \\&impl; }";
die $@ if $@;
print g(sub {1}, 9), "\n";
}), "impl(5,CODE,9)\n",
   'a string-eval-defined sub whose body is goto \\&NAME runs (Capture::Tiny shape)');

# goto replaces the FRAME, so the target inherits the ORIGINAL caller's
# context — not the goto statement's own (void) statement context.  Before
# s329 both spellings ran the target as if in scalar context: p-goto-sub
# restored the caller's package/subname stacks but never *wantarray*
# (found by the s329 review probe; fix mirrors p-return's restore).
is(run_cl(q{sub t { return wantarray ? "L" : "S" }
sub g { goto &t }
my @r = g(); my $s = g();
print "@r $s\n";
}), "L S\n", 'goto &NAME target sees the original caller wantarray');

is(run_cl(q{sub t { return wantarray ? "L" : "S" }
sub g { goto \&t }
my @r = g(); my $s = g();
print "@r $s\n";
}), "L S\n", 'goto \&NAME target sees the original caller wantarray');

# ------------------------------------------------- 2. local in a phase block

is(run_cl(q{our $g = "outer";
sub show { print "in $_[0]: g=$g\n" }
END   { local($g); $g = "e"; show("END"); }
show("main");
}), "in main: g=outer\nin END: g=e\n",
   'END { local(...) } closes its let — later top-level forms are not swallowed');

is(run_cl(q{our $g = "outer";
sub show { print "$_[0]:$g " }
BEGIN { local($g); $g = "b"; show("BEGIN"); }
INIT  { local($g); $g = "i"; show("INIT"); }
CHECK { local($g); $g = "c"; show("CHECK"); }
UNITCHECK { local($g); $g = "u"; show("UNITCHECK"); }
show("main");
print "\n";
}), "BEGIN:b UNITCHECK:u CHECK:c INIT:i main:outer \n",
   'BEGIN/UNITCHECK/CHECK/INIT with local() run in perl order and restore');

# INVERSE: a phase block WITHOUT local must not gain a stray closing paren.
is(run_cl(q{our $g = "outer";
END { print "end:$g\n"; }
print "main:$g\n";
}), "main:outer\nend:outer\n", 'END without local still emits balanced');

# ------------------------------------------------ 3. require_ok / use_ok load

is(run_cl(q{use Test::More tests => 2;
require_ok('File::Basename');
ok(defined &File::Basename::basename, 'require_ok actually loaded the module');
}), "ok 1 - require File::Basename;\nok 2 - require_ok actually loaded the module\n",
   'require_ok loads the module (it used to report ok and load nothing)');

is(run_cl(q{use Test::More tests => 2;
use_ok('File::Basename');
ok(defined &main::basename, 'use_ok imported into the caller');
}), "ok 1 - use File::Basename;\nok 2 - use_ok imported into the caller\n",
   'use_ok loads AND imports into the calling package');

# INVERSE: a module that cannot be found must FAIL the row, not pass it.
like(run_cl(q{use Test::More tests => 1;
require_ok('No::Such::Module::Here');
}), qr/^not ok 1 - require No::Such::Module::Here;/m,
   'require_ok of a missing module is not ok');

# ------------------------------------------- Test::More->builder output handles

is(run_cl(q{use Test::More tests => 1;
my $builder = Test::More->builder;
binmode($builder->failure_output, ':utf8');
binmode($builder->todo_output, ':utf8');
ok(ref($builder) eq 'Test::Builder', 'builder is a Test::Builder');
}), "ok 1 - builder is a Test::Builder\n",
   'Test::More->builder returns handles binmode accepts');

# ------------------------------------------------- 4. embedded-my veto scoping

is(run_cl(q{sub other { my $fh; return defined($fh) ? "d" : "u"; }
sub nf { open my $fh, ">", "/dev/null" or die; my $n = fileno $fh; close $fh; return defined($n); }
print nf() ? "ok\n" : "no\n";
}), "ok\n",
   'open my $fh keeps its lexical when a sibling sub DECLARES the same name');

# INVERSE: a sibling sub that only REFERENCES the name still vetoes, so the
# shared-cell shape that relied on the forward-declared global keeps working.
like(run_cl(q{sub reader { return defined($fh) ? "d" : "u"; }
sub emb { open my $fh, ">", "/dev/null" or die; my $r = reader(); close $fh; return $r; }
print emb(), "\n";
}), qr/^\w$/m,
   'a sibling sub that only references the name still resolves (veto path intact)');
