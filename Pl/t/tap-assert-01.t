#!/usr/bin/env perl
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
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^# PCL Test library loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ------------------------------------------------- 1. unusable regex ≠ verdict

my $bad_re = run_cl(q{use Test::More tests => 3;
my $bad = "(unclosed";
unlike("abc", $bad, 'unlike unusable');
like("abc", $bad, 'like unusable');
unlike("abc", "b", 'unlike a USABLE pattern that matches');
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

# ------------------------------------------------------- 2. cmp_ok's operators

is(run_cl(q{use Test::More tests => 6;
cmp_ok(1, '<=>', 2, 'spaceship unequal is true');
cmp_ok(1, '<=>', 1, 'spaceship equal is false');
cmp_ok("a", 'cmp', "b", 'cmp unequal is true');
cmp_ok("a", 'cmp', "a", 'cmp equal is false');
cmp_ok("abc", '=~', qr/b/, 'bind matches');
cmp_ok("abc", '!~', qr/b/, 'negated bind matches');
}), <<'TAP', 'cmp_ok answers <=>, cmp, =~ and !~ instead of failing them');
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
}), "ok 1 - eq_hash equal\nok 2 - eq_hash different value\nok 3 - eq_hash different size\n",
   'eq_hash runs at all (it type-errored on every hashref) and both verdicts are reachable');

# ---------------------------------------------------- 4. Test::More's wording

is(run_cl(q{use Test::More tests => 1;
use_ok('File::Basename', 'basename');
}), "ok 1 - use File::Basename;\n",
   'use_ok description is "use Foo;" — never the import list (descriptions are keys)');

is(run_cl(q{use Test::More tests => 4;
isa_ok(bless({}, 'Foo'), 'Bar');
isa_ok([1,2], 'Bar');
isa_ok('Plain', 'Bar');
isa_ok(undef, 'Bar');
}), <<'TAP', 'isa_ok names WHAT the thing is — four kinds, four distinct rows');
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
not ok 1 - Foo->can('nope')
#     Foo->can('nope') failed
not ok 2 - Foo->can(...)
#     Foo->can('nope1') failed
#     Foo->can('nope2') failed
not ok 3 - Foo->can(...)
#     can_ok() called with no methods
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
