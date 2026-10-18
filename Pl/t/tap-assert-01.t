#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tap-assert-01.t — task #202: the TAP layer's own assertions must be able to
# FAIL.  Every row here is an INVERSE probe: it drives an assertion into the
# state where it must say `not ok`, or into a state it used to answer with a
# manufactured verdict.
#
# What each guard was before the fix (cl/pcl-test.lisp):
#
#   * unlike() swallowed a scanner error into a PASS — `unlike($s, "(")` was
#     an assertion that could not fail, for any typo'd pattern.
#   * cmp_ok() knew twelve operators and reported everything else as a
#     FAILURE after printing a comment: `cmp_ok(1,'<=>',2)` and
#     `cmp_ok($s,'=~',qr/x/)` — ordinary perl — were published as failures.
#   * eq_hash() unwrapped its argument TWICE and type-errored on every real
#     call, killing the file.  It had never worked.
#   * use_ok()'s description carried the import list; Test::More prints
#     "use Foo;" for every form, and descriptions are join keys here.
#   * isa_ok() printed "The object isa X" for an object, a plain reference, a
#     class name and undef alike — one key for four different assertions.
#   * can_ok('Foo') printed "->can(...)" with an empty "method(s) not found:".
#   * skip_without_dynamic_extension() skipped unconditionally — a claim
#     about the environment it never checked.
#   * plan() fell through unrecognized forms silently, leaving the whole file
#     unjudgeable.
#   * the IMPORT-ARG spelling of the plan (`use Test::More tests => N`) was
#     dropped entirely, so such a file published TAP with no plan line at all
#     (task #275).  Every expected block below now opens with its `1..N`.
#
# Plus the runtime bug the audit's own probes found: scalar() DEREFERENCED a
# reference (`ref(scalar($aref))` was "", `scalar($aref)` was the element
# count), so an assertion on a literal ref was judging the wrong value.

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

plan tests => 35;

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
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ------------------------------------------------- 1. unusable regex ≠ verdict

my $bad_re = run_cl(q{use Test::More tests => 3;
my $bad = "/(unclosed/";   # Test::More reads a "/RE/" string as RE (#3436)
unlike("abc", $bad, 'unlike unusable');
like("abc", $bad, 'like unusable');
unlike("abc", "/b/", 'unlike a USABLE pattern that matches');
});
like($bad_re, qr/^not ok 1 - unlike unusable/m,
   'unlike() with an uncompilable pattern is NOT a pass (it used to be)');
like($bad_re, qr/^not ok 2 - like unusable/m,
   'like() with an uncompilable pattern fails');
like($bad_re, qr/Unusable regex:/,
   'the unusable-pattern row names the scanner error');
# INVERSE: a usable pattern still produces a real verdict in both directions.
like($bad_re, qr/^not ok 3 - unlike a USABLE pattern that matches/m,
   'unlike() still fails on a pattern that matches');

# ------------------- 1b. Test::More's like reads a "/RE/flags" STRING (#3436)
# Test::Builder::maybe_regex: "/RE/flags" and "m<d>RE<d>flags" (the SAME
# delimiter twice) are that regex; any other string is not a pattern and the
# row fails "doesn't look much like a regex to me" (perl 5.40.3, probed).
# IO-Compress's own tests spell `like $@, "/^$TopType: expected .../"`.
my $tm_str = run_cl(q{use Test::More tests => 6;
like("Header Error: CRC mismatch.", "/Header Error: CRC mismatch/", "slash string");
like("abc", "/A/i", "slash flag");
like("abc", "m!^A!i", "m-bang flag");
unlike("abc", "/z/", "unlike slash");
like("abc", "abc", "plain string");
like("abc", "m{^a}", "m-braces");
});
is(join("", map { /^(ok|not ok) \d/ ? ($1 eq 'ok' ? 1 : 0) : () } split /\n/, $tm_str),
   "111100", 'Test::More like/unlike: "/RE/" strings are regexes, a plain string is not (#3436)');
like($tm_str, qr/^#\s+'abc' doesn't look much like a regex to me\.$/m,
     '...and the non-regex row says why, as Test::Builder does');
# INVERSE: perl-tests/t/test.pl's like() INTERPOLATES a string pattern (its
# `$got =~ /$expected/`), so the slashes are literal there -- a caller that
# did not `use Test::More` keeps that reading (the sweep's rows depend on it).
{
  my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh qq{BEGIN { require "$project_root/perl-tests/t/test.pl"; }\n}
          . q{plan(tests => 2);
like("x/foo/y", "/foo/", "slashes literal");
unlike("foo", "/foo/", "no slashes, no match");
};
  close $fh;
  my $cl = `$pl2cl $pl 2>/dev/null`;
  my ($cfh, $clf) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
  print $cfh $cl; close $cfh;
  my $out = `sbcl @sbcl_rt --load $project_root/cl/pcl-test.lisp --load $clf 2>&1`;
  is(join("", $out =~ /^(ok \d|not ok \d)/mg), "ok 1ok 2",
     "test.pl's like keeps interpolating a '/RE/' string (#3436 leaves it alone)");
}

# ------------------------------------------------------- 2. cmp_ok's operators

is(run_cl(q{use Test::More tests => 6;
cmp_ok(1, '<=>', 2, 'spaceship unequal is true');
cmp_ok(1, '<=>', 1, 'spaceship equal is false');
cmp_ok("a", 'cmp', "b", 'cmp unequal is true');
cmp_ok("a", 'cmp', "a", 'cmp equal is false');
cmp_ok("abc", '=~', qr/b/, 'bind matches');
cmp_ok("abc", '!~', qr/b/, 'negated bind matches');
}), <<'TAP', 'cmp_ok answers <=>, cmp, =~ and !~ instead of failing them');
1..6
ok 1 - spaceship unequal is true
not ok 2 - spaceship equal is false
#      got: '1'
# expected: <=> '1'
ok 3 - cmp unequal is true
not ok 4 - cmp equal is false
#      got: 'a'
# expected: cmp 'a'
ok 5 - bind matches
not ok 6 - negated bind matches
#      got: 'abc'
# expected: !~ '(?^:b)'
TAP

# An operator the dispatch does not implement must NAME itself, not become a
# plausible verdict (CLAUDE.md rule 12, announce-flavoured: the row is a
# visible failure carrying the reason, so the rest of the file still runs).
my $unknown_op = run_cl(q{use Test::More tests => 2;
cmp_ok(1, '&&', 1, 'unhandled operator');
ok(1, 'the file keeps running');
});
like($unknown_op, qr/^not ok 1 - unhandled operator/m,
   'an unhandled cmp_ok operator is not a pass');
like($unknown_op, qr/cannot evaluate the operator '&&'/,
   'the unhandled operator is named in the diagnostic');
like($unknown_op, qr/^ok 2 - the file keeps running/m,
   'an unhandled operator does not abort the file');

# --------------------------------------------------------------- 3. eq_hash

is(run_cl(q{use Test::More tests => 3;
ok(eq_hash({a=>1,b=>2}, {a=>1,b=>2}), 'eq_hash equal');
ok(!eq_hash({a=>1}, {a=>2}), 'eq_hash different value');
ok(!eq_hash({a=>1}, {a=>1,b=>2}), 'eq_hash different size');
}), "1..3\nok 1 - eq_hash equal\nok 2 - eq_hash different value\nok 3 - eq_hash different size\n",
   'eq_hash runs at all (it type-errored on every hashref) and both verdicts are reachable');

# --------------------------------------------------- 3b. eq_set (task #3432)
# Test::More's eq_set: same elements in any order, DUPLICATES COUNT, the
# references kept in their own order and compared deeply (probed vs perl
# 5.40.3).  It was absent: "Undefined subroutine &main::eq_set".
is(run_cl(q{use Test::More tests => 5;
ok(eq_set([1,2,3],[3,1,2]), 'eq_set order-free');
ok(!eq_set([1,1,2],[1,2,2]), 'eq_set duplicates count');
ok(!eq_set([[1],[2]],[[2],[1]]), 'eq_set refs keep their order');
ok(eq_set([[1],[2]],[[1],[2]]), 'eq_set refs compare deeply');
ok(!eq_set([1],[1,1]), 'eq_set length');
}), "1..5\nok 1 - eq_set order-free\nok 2 - eq_set duplicates count\nok 3 - eq_set refs keep their order\nok 4 - eq_set refs compare deeply\nok 5 - eq_set length\n",
   'eq_set exists and both of its verdicts are reachable');

# ---------------------------------------------------- 4. Test::More's wording

is(run_cl(q{use Test::More tests => 1;
use_ok('File::Basename', 'basename');
}), "1..1\nok 1 - use File::Basename;\n",
   'use_ok description is "use Foo;" — never the import list (descriptions are keys)');

is(run_cl(q{use Test::More tests => 4;
isa_ok(bless({}, 'Foo'), 'Bar');
isa_ok([1,2], 'Bar');
isa_ok('Plain', 'Bar');
isa_ok(undef, 'Bar');
}), <<'TAP', 'isa_ok names WHAT the thing is — four kinds, four distinct rows');
1..4
not ok 1 - An object of class 'Foo' isa 'Bar'
#     The object of class 'Foo' isn't a 'Bar'
not ok 2 - A reference of type 'ARRAY' isa 'Bar'
#     The reference of type 'ARRAY' isn't a 'Bar'
not ok 3 - The class (or class-like) 'Plain' isa 'Bar'
#     The class (or class-like) 'Plain' isn't a 'Bar'
not ok 4 - undef isa 'Bar'
#     undef isn't defined
TAP

is(run_cl(q{use Test::More tests => 3;
my $o = bless {}, 'Foo';
can_ok($o, 'nope');
can_ok($o, 'nope1', 'nope2');
can_ok('Foo');
}), <<'TAP', 'can_ok names the class and the single method, and rejects a no-method call');
1..3
not ok 1 - Foo->can('nope')
#     Foo->can('nope') failed
not ok 2 - Foo->can(...)
#     Foo->can('nope1') failed
#     Foo->can('nope2') failed
not ok 3 - Foo->can(...)
#     can_ok() called with no methods
TAP

# #3245: a Test::More function is a perl SUB, so an ARRAY argument flattens --
# `can_ok($class, @methods, @accessors)` (HTTP-Tiny t/001_api.t) read
# `main->can('ARRAY(0x1)') failed`, and diag/note joined their flattened list
# (perl's `join '', @_`) only after it printed `# ARRAY(0x..)`.  The expected
# text is perl's (probed 5.40.3) less its `Failed test` / `at` lines, which
# this layer does not print (the rows above show the same).
is(run_cl(q{use Test::More tests => 2;
sub new {} sub get {} sub agent {} sub timeout {}
my @m = qw(new get); my @a = qw(agent timeout);
can_ok("main", @m, @a);
can_ok("main", @m, "nope");
my @d = ("d1", "d2\nd3");
note(@d);
}), <<'TAP', 'can_ok flattens its method list; note/diag join theirs (#3245)');
1..2
ok 1 - main->can(...)
not ok 2 - main->can(...)
#     main->can('nope') failed
# d1d2
# d3
TAP

# #3245, second half: Test::More's functions are subs IN the Test::More stash
# (perl: `Test::More->can('ok')`, `defined &Test::More::is`), can_ok's class
# slot is `$` (an array there is its COUNT: `2->can(...)`), and use_ok's
# import list flattens.  Expected = perl 5.40.3 less the `Failed test` lines.
is(run_cl(q{use Test::More tests => 4;
my @imp; BEGIN { @imp = qw(max min) }
BEGIN { use_ok('List::Util', @imp) }
print "# ", max(1,5,3), min(4,2), "\n";
can_ok('Test::More', 'ok', 'is');
my @args = ('main', 'new');
can_ok(@args);
ok(defined &Test::More::is && !Test::More->can('nope'), 'stash');
}), <<'TAP', 'Test::More functions are in its stash; can_ok($@); use_ok flattens (#3245)');
1..4
ok 1 - use List::Util;
# 52
ok 2 - Test::More->can(...)
not ok 3 - 2->can(...)
#     can_ok() called with no methods
ok 4 - stash
TAP

# ------------------------------------------- 5. skip_without_dynamic_extension

my $skip_ext = run_cl(q{use Test::More tests => 2;
SKIP: { skip_without_dynamic_extension("No::Such::Extension", 1);
        fail("must not run"); }
SKIP: { skip_without_dynamic_extension("File::Basename", 1);
        ok(1, "an AVAILABLE module is not skipped"); }
});
like($skip_ext, qr/^ok 1 # skip No::Such::Extension was not built/m,
   'skip_without_dynamic_extension still skips what really is missing');
like($skip_ext, qr/^ok 2 - an AVAILABLE module is not skipped/m,
   'skip_without_dynamic_extension no longer skips a module PCL can load');

# --------------------------------------------------------- 6. plan says so

like(run_cl(q{use Test::More;
plan('bogus');
ok(1, 'never judged');
}), qr/unrecognized plan form/,
   'an unrecognized plan form names itself instead of leaving the file unjudgeable');

# ------------------- 6b. the plan can also be spelled as an import list (#275)
#
# Test::More's import IS its plan setter, so `use Test::More tests => N`,
# `use Test::More 'no_plan'` and `use Test::More skip_all => "why"` must mean
# exactly the matching plan() call.  PCL used to DROP the whole import list for
# a PCL-provided module, so a file spelling its plan this way published TAP
# with no plan line and no done_testing — one no harness can judge.  The
# `tests => N` spelling is guarded by the `1..N` first line of every expected
# block above; these two cover the other spellings.  A VERSION
# (`use Test::More 0.88`) is not an import arg and must stay plan-less.
is(run_cl(q{use Test::More 'no_plan';
ok(1, 'a'); ok(1, 'b');
}) . "--\n" . run_cl(q{use Test::More skip_all => 'why not';
ok(1, 'never runs');
}) . "--\n" . run_cl(q{use Test::More 0.88;
ok(1, 'a');
}), "ok 1 - a\nok 2 - b\n1..2\n--\n1..0 # Skip why not\n--\nok 1 - a\n",
   'no_plan / skip_all / a bare VERSION each reach the TAP layer as perl reads them');

# ---------------- 6c. plan()'s argument list FLATTENS, like any perl sub (#317)
#
# `plan reverse 9;` is t/op/select.t's first statement.  `plan` is a perl sub
# in t/test.pl, so its arguments flatten and it receives ONE argument, 9; PCL
# implements it in CL, so the call site handed it the value of `(p-reverse 9)`
# — a one-element vector — and the form dispatch DIED, which left that whole
# file with no TAP at all.  pl-plan now spreads its arguments through
# p-flatten-args, the same flattening a p-sub does to build @_.
# (Probed against perl: `plan reverse 3` there prints `1..3`.)
is(run_cl(q{use Test::More;
plan reverse 3;
ok(1, 'one'); ok(1, 'two'); ok(1, 'three');
}), "1..3\nok 1 - one\nok 2 - two\nok 3 - three\n",
   'plan reverse N — a one-element aggregate is the count, as perl flattens it');

# ------------------------------- 7. scalar() never dereferences (runtime fix)

is(run_cl(q{my $ar = [1,2,3]; my %h; my $hr = \%h;
print "A:", ref(scalar($ar)), "\n";
print "B:", ref(scalar([1,2])), "\n";
print "C:", ref(scalar(\5)), "\n";
print "D:", ref(scalar($hr)), "\n";
my @a = (1,2,3); print "E:", scalar(@a), "\n";
my %g = (x=>1); print "F:", scalar(%g), "\n";
print "G:", scalar("str"), "\n";
}), "A:ARRAY\nB:ARRAY\nC:SCALAR\nD:HASH\nE:3\nF:1\nG:str\n",
   'scalar(REF) is the ref itself; scalar(@a)/scalar(%h) are still the counts');

# 7b. …and a hash ref that was NOT built by `\%h` (task #1525).  The row above
# passed for two years while this one was broken, because `\%h` sets the box's
# is-ref flag and a `{…}` CONSTRUCTOR does not: the ref arm caught the first
# spelling and the hash arm merely DECLINED for the second, so it fell through
# to the catch-all and returned the RAW HASH TABLE.  `scalar($href)` then lost
# the reference — an overloaded object printed HASH(0x1) instead of running its
# `""` handler, and `my $c = scalar($href)` stored the KEY COUNT.  The array arm
# had been given the box-preserving shape and the hash arm had not (rule 11).
# Every expected value below is real perl 5.40.3's (probed).
is(run_cl(q{{ package H; use overload '""' => sub { "H<".$_[0]{v}.">" }, fallback => 1;
  sub new { my ($c,$v)=@_; bless {v=>$v}, $c } }
my $h = H->new(9);
my $c = scalar($h);
print "A:", scalar($h), "\n";
print "B:$c\n";
print "C:", ref($c), "\n";
my $a = { v => 9 };
print "D:", ref(scalar($a)), "\n";
print "E:", scalar(@{[1,2,3]}), "\n";
my %g = (x=>1, y=>2); print "F:", scalar(%g), "\n";
my @ar = (1,2,3);     print "G:", scalar(@ar), "\n";
print "H:", (scalar(%ENV) > 0 ? "many" : "none"), "\n";
print "I:", ref(scalar(\%g)), "\n";
}), "A:H<9>\nB:H<9>\nC:H\nD:HASH\nE:3\nF:2\nG:3\nH:many\nI:HASH\n",
   'scalar(HASHREF) keeps the reference — overload, assignment and ref(); %h/%ENV/@a counts unmoved');

# ------------------------------------ 8. explain() DUMPS a ref (task #236)
#
# Test::More::explain renders a ref with Data::Dumper (Indent 1, Terse 1,
# Sortkeys 1) and passes a non-ref through.  PCL stringified instead, so every
# is_deeply failure that printed its operands read `got 'ARRAY(0x53)'` and was
# undiagnosable.  The expected text below is the LIVE Data::Dumper output for
# each value (probed s374), minus Dumper's trailing newline — PCL omits it so
# pl-diag does not emit a bare `# ` line after every dump.

is(run_cl(q{use Test::More;
plan tests => 1;
print explain([1,[2,3]]), "|\n";
print explain({b=>2,a=>1}), "|\n";
print explain([undef,1.5,"x","10",-3]), "|\n";
print explain("plain"), "|\n";
ok(1);
}), <<'EXPECT', 'explain() renders arrays, hashes, sorted keys, number-vs-string quoting');
1..1
[
  1,
  [
    2,
    3
  ]
]|
{
  'a' => 1,
  'b' => 2
}|
[
  undef,
  '1.5',
  'x',
  '10',
  -3
]|
plain|
ok 1
EXPECT

is(run_cl(q{use Test::More;
plan tests => 1;
print explain(bless({a=>1},"Foo")), "|\n";
print explain(\"str"), "|\n";
print explain(sub {1}), "|\n";
print explain([]), "|", explain({}), "|\n";
print explain(["it's","a\\\\b"]), "|\n";
ok(1);
}), <<'EXPECT', 'explain() renders bless/scalar-ref/code-ref/empty forms and quotes like Dumper');
1..1
bless( {
  'a' => 1
}, 'Foo' )|
\'str'|
sub { "DUMMY" }|
[]|{}|
[
  'it\'s',
  'a\\b'
]|
ok 1
EXPECT

# A cycle must TERMINATE with Dumper's back-reference, not recurse forever;
# a shared ref prints the path of its first occurrence.
is(run_cl(q{use Test::More;
plan tests => 1;
my $c = [1]; push @$c, $c;
print explain($c), "|\n";
my $s = {k=>1};
print explain([$s,$s]), "|\n";
ok(1);
}), <<'EXPECT', 'explain() prints $VAR1 back-references for a cycle and a shared ref');
1..1
[
  1,
  $VAR1
]|
[
  {
    'k' => 1
  },
  $VAR1->[0]
]|
ok 1
EXPECT

# ── s473t1 / task #1432: a perl-tests/t/ SHARED HELPER must not be a stub ────
#
# The same family as the rows above — a harness file that silently removes
# assertions.  `perl-tests/t/op/caller.pl` is perl's own file, `do`ne by
# perl-tests/caller.t for 47 of its 112 rows.  It had been replaced by a
# 4-line stub ("Stub: XS hint_fetch/hint_exists not available in PCL", commit
# 6b8e524c) on a premise that is false: caller.t DEFINES hint_fetch and
# hint_exists itself, in plain perl, over `(caller $n)[10]`.  The loss was
# invisible: caller.t sat PARTIAL 65/112 and the sweep's shortfall said
# UNEXPLAINED, because a `do FILE` that returns 1 having run nothing looks
# exactly like success.  Restored from perl 5.40.3 — the file produces all
# 112 rows now.  Two independent conjuncts: it is perl's file (its own first
# line), and it actually carries the shared assertions (a stub carries none).
{
  my $helper = "$project_root/perl-tests/t/op/caller.pl";
  open my $hfh, '<:raw', $helper or die "$helper: $!";
  my $text = do { local $/; <$hfh> };
  close $hfh;
  my $asserts = () = $text =~ /^\s*(?:is|ok|isnt)\(/gm;
  my $is_perls = $text =~ /^# tests shared between/;
  ok($is_perls && $asserts >= 40,
     "perl-tests/t/op/caller.pl is perl's shared helper, not a stub"
     . " (perls=" . ($is_perls ? 1 : 0) . " asserts=$asserts)");
}

# ── task #1576: Test::More's `subtest` -- a NESTED stream, one parent row ────
# Probed against perl 5.40.3's Test::More (stdout): the child stream is the
# same TAP indented four spaces per level, opened by `# Subtest: NAME`; its
# plan is its own (declared inside, or `1..N` printed at the END); a child
# with no assertions fails the parent as `No tests run for subtest "NAME"`;
# `plan skip_all` inside makes the parent `ok N # skip REASON`; subtest
# returns 1 / 0; a die inside closes the child, reports the parent row, and
# PROPAGATES (perl 5.40 does not trap it).  Before #1576 every one of these
# died "Undefined subroutine &main::subtest" with no TAP at all.
{
  my $st = run_cl(q{use Test::More tests => 5;
subtest 'inner' => sub { plan tests => 1; ok(1, 'a') };
my $r = subtest 'failing' => sub { ok(0, 'bad'); ok(1, 'good'); };
print "# r=$r\n";
subtest 'empty' => sub { 1 };
$r = subtest 'nested' => sub { ok(1,'n1'); subtest 'deep' => sub { ok(1,'d1'); done_testing; }; };
print "# r=$r\n";
subtest 'skipper' => sub { plan skip_all => 'why'; ok(1) };
});
  like($st, qr/^# Subtest: inner\n    1\.\.1\n    ok 1 - a\nok 1 - inner$/m,
     'subtest: the child stream is indented, with its own plan and numbering');
  like($st, qr/^    not ok 1 - bad\n    ok 2 - good\n    1\.\.2\n(?:.*\n)*?not ok 2 - failing\n(?:#.*\n)*?# r=0$/m,
     'subtest: a failing child fails the parent row; the implicit plan is printed at the end; answers 0');
  like($st, qr/^    1\.\.0\n(?:.*\n)*?not ok 3 - No tests run for subtest "empty"$/m,
     'subtest: a child with no assertions fails the parent (No tests run)');
  like($st, qr/^    # Subtest: deep\n        ok 1 - d1\n        1\.\.1\n    ok 2 - deep\n    1\.\.2\nok 4 - nested\n# r=1$/m,
     'subtest: nested subtests indent further; a passing one answers 1');
  like($st, qr/^    1\.\.0 # SKIP why\nok 5 # skip why$/m,
     'subtest: plan skip_all inside a child is a skip row in the parent, not an exit');
  my $die = run_cl(q{use Test::More tests => 2;
subtest 'dies' => sub { ok(1,'x'); die "boom\n"; };
ok(1, 'never');
});
  ok($die =~ /^    ok 1 - x\n    1\.\.1\nok 1 - dies$/m && $die =~ /boom/ && $die !~ /never/,
     'subtest: a die inside closes the child, reports the parent row, then propagates');
}
