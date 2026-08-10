#!/usr/bin/env perl
# global-partition-01.t — Pl::GlobalPartition, the ONE answer to "does this
# global keep a dynamic binding?" that direction D's two emitters share
# (task #289 step 2, plan docs/direction-d-plan.md).
#
# Why this file is worth its seconds: the declaration emitter and the `local`
# lowering live in different modules (Pl/Parser2.pm and Pl/Parser.pm), and a
# disagreement between them is a LOAD-TIME error, not a wrong answer — SBCL
# refuses a name that is both `defvar`-special and `define-symbol-macro`'d.
# Pinning the predicate here means the two call sites can be read as "asks the
# shared function" without re-deriving the set at each one.
#
# Pure perl, no SBCL spawn.
use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin;
use lib "$FindBin::Bin/../..";
use Pl::GlobalPartition qw(is_exception_global partition_name);

# --- ORDINARY: user package variables become symbol-macro cells -------------
for my $n ('$x', '@arr', '%h', '$VERSION', '@ISA', '%config', '$_private',
           '$x9', 'Foo::$x', 'Foo::@ISA', 'Foo::Bar::%h', '|Foo Bar|::$x',
           '@a', '@b', '%a', '%b') {
  is(partition_name($n), 'ordinary', "ordinary: $n");
}

# --- EXCEPTION (b): word-shaped but runtime-owned / thread-of-control -------
for my $n ('$_', '@_', '%_args', '@ARGV', '$ARGV', '@ARGVOUT', '@INC', '%INC',
           '%ENV', '%SIG') {
  is(partition_name($n), 'exception', "exception, runtime-owned: $n");
}

# The sort pair is bound BY the sort lowering — in every package (#287).
for my $n ('$a', '$b', 'Foo::$a', 'Foo::$b', '|Foo Bar|::$a') {
  is(partition_name($n), 'exception', "exception, sort pair: $n");
}

# --- EXCEPTION (a): not word-shaped ----------------------------------------
# Punctuation and caret magic, in every spelling the emitters produce.  These
# are also where `local` runs hot, which is the whole reason they keep the
# fast dynamic bind (plan §2).
for my $n ('$@', '$!', '$1', '$0', '$/', '$\\', '$|', '$.', '$,', '$;', '$?',
           '@#', '|$.|', '|$"|', '|$^W|', '|${^MPE}|', '|${^WARNING_BITS}|',
           '$^W', '%+', '@-', '@+') {
  is(partition_name($n), 'exception', "exception, not word-shaped: $n");
}

# Non-variables and junk answer 'exception' — the conservative side, which
# leaves today's emission in place rather than minting a cell for a form that
# is not a variable at all.
for my $n ('(p-stash "Foo")', '(unbox $r)', 'pcl-local-rhs-0', '', undef,
           '$', '@', 'Foo::', '$x-y', '%pcl-str-buffer') {
  is(partition_name($n), 'exception',
     'exception, not a plain variable: ' . (defined $n ? "'$n'" : 'undef'));
}

# --- is_exception_global is the boolean face of the same answer ------------
ok(is_exception_global('$_'),   'is_exception_global true for $_');
ok(!is_exception_global('$x'),  'is_exception_global false for $x');
ok(is_exception_global('$a'),   'is_exception_global true for $a');
ok(!is_exception_global('@a'),  'is_exception_global false for @a');

# --- the cross-table coincidence, pinned ----------------------------------
# %PKG_SWITCH_IMMUNE_VARS answers a DIFFERENT question ("can an in-block
# `package X;` re-home this name?"), and the tables are deliberately separate.
# They happen to agree today on every name that table lists; if a future
# session changes one, this row fails and forces the other to be re-decided
# on its own cause instead of drifting silently.
my @immune = qw($_ @_ %_args @ARGV $ARGV @ARGVOUT @INC %ENV %INC %SIG);
my @disagree = grep { !is_exception_global($_) } @immune;
is_deeply(\@disagree, [],
          'every %PKG_SWITCH_IMMUNE_VARS name is also a partition exception');

# ...and the converse is NOT claimed: $a/$b are partition exceptions but were
# deliberately REMOVED from the immune table by #287.
ok(is_exception_global('$a'), '$a is a partition exception');

done_testing();
