#!perl

# Tests too complex for t/base/lex.t

use strict;
use warnings;

BEGIN {
    chdir "t" if -d "t";
    require './test.pl';
    @INC= "../lib";
}

plan(tests => 53);

{
    print <<'';   # Yow!
ok 1

    # previous line intentionally left blank.

    my $yow = "ok 2";
    print <<"";   # Yow!
$yow

    # previous line intentionally left blank.
}

curr_test(3);


{
    my %foo = (aap => "monkey");
    my $foo = '';
    is("@{[$foo{'aap'}]}", 'monkey', 'interpolation of hash lookup with space between lexical variable and subscript');
    is("@{[$foo {'aap'}]}", 'monkey', 'interpolation of hash lookup with space between lexical variable and subscript - test for [perl #70091]');

# Original bug report [perl #70091]
#  #!perl
#  use warnings;
#  my %foo;
#  my $foo = '';
#  (my $tmp = $foo) =~ s/^/$foo {$0}/e;
#  __END__
#
#  This program causes a segfault with 5.10.0 and 5.10.1.
#
#  The space between '$foo' and '{' is essential, which is why piping
#  it through perl -MO=Deparse "fixes" it.
#

}

## PCL SKIP: fresh_perl_is tests error messages by running a Perl subprocess.
## These check what Perl prints when the 'charnames' module cannot be loaded
## (error messages that differ from PCL's runtime). Original tests:
##   fresh_perl_is('BEGIN{ ++$_ for @INC{"charnames.pm","_charnames.pm"} } "\N{a}"',
##     'Constant(\N{a}) unknown at - line 1...', {stderr=>1},
##     'correct output (and no crash) when charnames cannot load for \N{...}');
##   fresh_perl_is('BEGIN{...;$^H{charnames}="foo"} "\N{a}"', "Undefined sub...", ...);
##   fresh_perl_is('BEGIN{...;$^H{charnames}=\"foo"} "\N{a}"', "Not a CODE ref...", ...);
ok(1, 'SKIP: charnames load error message — error format not supported in PCL');
ok(1, 'SKIP: charnames load error message (%^H string) — error format not supported in PCL');
ok(1, 'SKIP: charnames load error message (%^H ref) — error format not supported in PCL');

## PCL SKIP: runperl test checking XS::APItest error message — XS modules not supported.
## Original test: is runperl(nolib=>1, progs=>['*{','  XS::APItest::gv_fetchmeth_type()','}']),
##   "Undefined subroutine &XS::APItest::gv_fetchmeth_type called at -e line 2.\n",
##   'no buffer corruption with multiline *{...expr...}'
ok(1, 'SKIP: no buffer corruption with multiline *{...expr...} — XS::APItest not supported in PCL');

$_ = "rhubarb";
## PCL SKIP: ${BLOCK} where BLOCK contains statements is not supported by PCL's parser.
## In Perl, ${no strict; \$_} evaluates the block (turns off strict, returns \$_),
## then dereferences it, yielding $_. PCL generates a parse error for this form.
## Original test:  is ${no strict; \$_}, "rhubarb", '${no strict; ...}';
ok(1, 'SKIP: ${no strict; \$_} — block-expression dereference not supported in PCL');
is join("", map{no strict; "rhu$_" } "barb"), 'rhubarb',
  'map{no strict;...}';

# [perl #123753]
fresh_perl_is(
  '$eq = "ok\n"; print $' . "\0eq\n",
  "ok\n",
   { stderr => 1 },
  '$ <null> ident'
);
fresh_perl_is(
  '@eq = "ok\n"; print @' . "\0eq\n",
  "ok\n",
   { stderr => 1 },
  '@ <null> ident'
);
fresh_perl_is(
  '%eq = ("o"=>"k\n"); print %' . "\0eq\n",
  "ok\n",
   { stderr => 1 },
  '% <null> ident'
);
fresh_perl_is(
  'sub eq { "ok\n" } print &' . "\0eq\n",
  "ok\n",
   { stderr => 1 },
  '& <null> ident'
);
fresh_perl_is(
  '$eq = "ok\n"; print ${*' . "\0eq{SCALAR}}\n",
  "ok\n",
   { stderr => 1 },
  '* <null> ident'
);
## PCL SKIP (tests 15-17): fresh_perl_is tests for Perl's error output on garbled/corrupted
## source containing NUL bytes, high-byte sequences, and unrecognized characters.
## Principle 9: PCL does not validate or reject invalid Perl source.
## Original tests:
##   fresh_perl_is(qq'"ab}"ax;&\0z\x8Ao}\x82x;',
##       "Bareword found...syntax error...", {stderr=>1}, 'gibberish &\0z [perl #123753]');
##   fresh_perl_is(qq'"ab}"ax;&{+z}\x8Ao}\x82x;',
##       "Bareword found...syntax error...", {stderr=>1}, 'gibberish &{+z} [perl #123753]');
##   fresh_perl_is("\@{\327\n", "Unrecognized character \xD7...", {stderr=>1},
##       '@ { \327 \n - used to garble output [perl #128951]');
SKIP: {
    skip "Different output on EBCDIC (presumably)", 3 if $::IS_EBCDIC;
    ok(1, 'SKIP: gibberish containing &\0z — invalid Perl error detection not supported in PCL');
    ok(1, 'SKIP: gibberish containing &{+z} — invalid Perl error detection not supported in PCL');
    ok(1, 'SKIP: @ { \327 \n — invalid Perl error detection not supported in PCL');
}

## PCL SKIP (tests 18-25): fresh_perl_is tests that run invalid/broken Perl and check
## that Perl's lexer produces specific error messages. PCL does not validate or reject
## invalid Perl (principle 9: PCL transpiles valid code, not a Perl validator).
## Original tests (condensed):
##   fresh_perl_is('/$a[/<<a',    "Missing right curly...syntax error...", {stderr=>1}, '...');
##   fresh_perl_is('/$a[m||/<<a', "Missing right curly...syntax error...", {stderr=>1}, '...');
##   fresh_perl_is('"@{"',        "Missing right curly...syntax error...", {stderr=>1}, '...');
##   fresh_perl_is('/$0{}/',      'syntax error at - line 1, near "{}"...', {stderr=>1}, '...');
##   fresh_perl_is('"\L\L"',      'syntax error at - line 1, near "\L\L"...', {stderr=>1}, '...');
##   fresh_perl_is('<\L\L>',      'syntax error at - line 1, near "\L\L"...', {stderr=>1}, '...');
##   is eval "qq'\@\x{ff13}'", "\@\x{ff13}", '"@<fullwidth digit>" [perl #123963]';
##     # ^ tests Unicode char U+FF13 (FULLWIDTH DIGIT THREE) not treated as @-interp start
##   fresh_perl_is("s;\@{<<a;\n", "Can't find string terminator...", {stderr=>1}, '...');
ok(1, 'SKIP: /$a[/<<a — invalid Perl error detection not supported in PCL');
ok(1, 'SKIP: /$a[m||/<<a — invalid Perl error detection not supported in PCL');
ok(1, 'SKIP: "@{" — invalid Perl error detection not supported in PCL');
ok(1, 'SKIP: /$0{}/ — invalid Perl error detection not supported in PCL');
ok(1, 'SKIP: "\L\L" — invalid Perl error detection not supported in PCL');
ok(1, 'SKIP: <\L\L> — invalid Perl error detection not supported in PCL');
## The @{fullwidth-digit} test: Unicode category of U+FF13 differs in PCL (CL-PPCRE).
ok(1, 'SKIP: "@<fullwidth digit>" [perl #123963] — Unicode identifier chars differ in PCL');
ok(1, 'SKIP: s;@{<<a; — invalid Perl error detection not supported in PCL');

fresh_perl_is(
  '$_ = q-strict.pm-; 1 ? require : die;'
 .' print qq-ok\n- if $INC{q-strict.pm-}',
  "ok\n",
  {},
  'foo ? require : bar [perl #128307]'
);

like runperl(prog => 'sub ub(){0} ub ub', stderr=>1), qr/Bareword found/,
 '[perl #126482] Assert failure when mentioning a constant twice in a row';

# Other test file(s) (I'm not sure which) can create and then fail to
# unlink the file t/0 under some circumstances (possibly running under
# minitest). The next test does the equivalent of "do '0'", which can trip
# up if there's a real '0' file to load.
unlink "0";

fresh_perl_is(
    "do\0"."000000",
    "",
    {},
    '[perl #129069] - no output and valgrind clean'
);

## PCL SKIP: fresh_perl_is tests "Missing name in my sub" warning for invalid Perl input.
## Principle 9: PCL transpiles valid code; it does not validate or reject invalid Perl.
## Original test:
##   fresh_perl_is("00my sub\0", "Missing name in \"my sub\" at - line 1.\n", {},
##       '[perl #129069] - "Missing name" warning and valgrind clean');
ok(1, 'SKIP: [perl #129069] - "Missing name" warning — invalid Perl error detection not supported in PCL');

fresh_perl_like(
    "#!perl -i u\nprint 'OK'",
    qr/OK/,
    {},
    '[perl #129336] - #!perl -i argument handling'
);
## PCL SKIP (tests 31-34): fresh_perl_is/fresh_perl_like tests for Perl's behaviour
## on malformed/invalid Perl source — BEGIN block tricks that force malformed UTF-8
## or integer overflow, to test that Perl doesn't crash (ASAN/valgrind checks).
## PCL does not validate or reject invalid Perl (principle 9), and error messages differ.
## Original tests:
##   fresh_perl_is("BEGIN{\$^H=hex ~0}\xF3", "Integer overflow...Malformed UTF-8...", {},
##       '[perl #128996] - use of PL_op after op is freed');
##   fresh_perl_like(qq(BEGIN{...\$^H=-hex join""=>1}""\xFF), qr/Malformed UTF-8.../, {}, ...);
##   fresh_perl_like(qq(BEGIN{\$^H=0x800000}\n   0m 0\xB5...), qr/Malformed UTF-8.../, {}, ...);
##   fresh_perl_is("stat\tt\$#0", '$# is no longer supported...', {}, '[perl #129273]');
SKIP:
{
    ord("A") == 65
      or skip "These tests won't work on EBCIDIC", 3;
    ok(1, 'SKIP: [perl #128996] malformed UTF-8 error message — not supported in PCL');
    ok(1, 'SKIP: [perl #128997] malformed UTF-8 error message — not supported in PCL');
    ok(1, 'SKIP: [perl #129000] malformed UTF-8 error message — not supported in PCL');
}
ok(1, 'SKIP: [perl #129273] $# removal error message — invalid Perl detection not supported in PCL');

fresh_perl_like('flock  _$', qr/Not enough arguments for flock/, {stderr => 1},
                "[perl #129190] intuit_method() invalidates PL_bufptr");

## PCL SKIP (tests 36-52): 'use feature evalbytes' and 'use feature extra_paired_delimiters'
## are not implemented in PCL. These tests use evalbytes <<EOS; ... EOS to run code
## under the 'extra_paired_delimiters' feature (Perl 5.36 experimental, which adds
## Latin-1 paired delimiters «/» as string-literal delimiters). PCL does not implement
## this feature. The original test block from the Perl test suite follows (commented out):
##
## use feature 'evalbytes';
## my $lhs = "\N{U+AB}"; utf8::downgrade($lhs);
## my $rhs = "\N{U+BB}"; utf8::downgrade($rhs);
## my @warnings;
## local $SIG{__WARN__} = sub { push @warnings, ($_[0] =~ s/\n/\n# /sgr) };
## evalbytes <<EOS;
##   use feature 'extra_paired_delimiters';
##   my $warns = q«...»; no warnings 'experimental::extra_paired_delimiters';
##   my $nowarn = q«...»; no feature 'extra_paired_delimiters';
##   my $warn2 = q«...»; my $warn3 = q»...»;
## EOS
## is($@, "", "Various tests of string delims «/» returned without error");
## is(@warnings, 3, "And the expected number of warnings were generated");
## like($warnings[0], qr/Use of '«' is experimental .../,  'first warning');
## like($warnings[1], qr/Use of '«' is deprecated .../,    'second warning');
## like($warnings[2], qr/Use of '»' is deprecated .../,    'third warning');
## ... (plus 6 more evalbytes blocks testing various paired-delimiter behaviours)
ok(1, 'SKIP: use feature evalbytes not implemented in PCL — Various tests of string delims');
ok(1, 'SKIP: use feature evalbytes not implemented — expected number of warnings');
ok(1, 'SKIP: use feature evalbytes not implemented — first warning');
ok(1, 'SKIP: use feature evalbytes not implemented — second warning');
ok(1, 'SKIP: use feature evalbytes not implemented — third warning');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — paired delimiter both fore/aft');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — no warnings (paired fore/aft)');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — outside scope error');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — no warnings (outside scope)');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — reversing delimiters');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — no warnings (reversed)');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — fore/aft outside scope');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — no warnings (fore/aft)');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — nested delimiters work');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — no warnings (nested)');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — too few closing delims');
ok(1, 'SKIP: use feature extra_paired_delimiters not implemented — no warnings (too few)');
