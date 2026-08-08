#!/usr/bin/env perl
#
# PPI tokenizer/lexer bug report — four small cases.
# Tested against PPI 1.291 / perl 5.40.3.  All four tests currently FAIL (the bugs).
#
#   perl ppi-bug-report.t
#
use strict;
use warnings;
use Test::More tests => 4;
use PPI;

# Significant tokens of a snippet, as "Class=content" strings.
sub toks {
    my $doc = PPI::Document->new(\$_[0]) or return ();
    return map { ref($_) . '=' . $_->content }
           grep { $_->significant } $doc->tokens;
}

# ── Bug 1: triple dereference $$$ref is mis-tokenized as the PID var $$ ────────
#
# $$$ref means ${ ${ $ref } } (a triple scalar deref).  Compare $$ref, which PPI
# correctly gives as Cast '$' + Symbol '$ref'.  For $$$ref PPI instead emits the
# special process-ID variable $$ (PPI::Token::Magic) + Symbol '$ref', so the
# extra dereference is lost and there is no PID variable in this code at all.
{
    my @t = toks('$$$ref');
    ok( !grep(/^PPI::Token::Magic=\$\$$/, @t),
        '$$$ref should NOT tokenize as the PID variable $$ (it is a triple deref)' )
        or diag "got: @t";
}

# ── Bug 2: C99 hex-float literal 0x1.8p+1 is split into 5 tokens ───────────────
#
# Perl 5.22+ accepts hex floating-point literals; 0x1.8p+1 == 3.0 is ONE number.
# PPI splits it into  Number::Hex '0x1' + Number::Float '.8' + Word 'p' +
# Operator '+' + Number '1'.
{
    my @t = toks('my $n = 0x1.8p+1;');
    my @nums = grep { /^PPI::Token::Number/ } @t;
    is( scalar(@nums), 1,
        '0x1.8p+1 should be a single numeric token' )
        or diag "got: @t";
}

# ── Bug 3: 7%-3 mis-tokenized as the magic hash %- (modulo operator lost) ──────
#
# `%-`/`%+` are the named-capture magic *hashes*; they only ever appear in term
# position.  In `7%-3` the `%` follows a term (7) so it is the modulo operator
# and `-3` is its operand — exactly what PPI gives for the spaced form `7 % -3`.
# PPI instead emits Magic `%-`, losing the `%` operator.
{
    my @t = toks('7%-3');
    ok( !grep(/^PPI::Token::Magic=\%-$/, @t),
        '7%-3 should tokenize as 7 % -3, not as the magic hash %-' )
        or diag "got: @t";
}

# ── Bug 4: for ${*$f} (LIST) {} — LEXER DIES ("Illegal state in 'for' …") ──────
#
# A block-deref lvalue as the foreach loop variable is valid Perl (it aliases
# the glob's scalar slot per iteration):
#
#   $ perl -e 'no strict "refs"; my $f = "v"; for ${*$f} (5,11,33) { print "$v " }'
#   5 11 33
#
# (It appears in perl's own test suite: t/op/for.t, the low-refcount-package-var
# assert/SEGV regression test.)  PPI::Document->new returns undef on it —
# not a mis-tokenization but a hard lexer failure:
#   Lexer failed: Illegal state in 'for' compound statement
# Plain `for $x (…)` is fine; `foreach ${*$f} (…)` fails identically, as does
# the construct buried anywhere in a larger document (the whole parse dies).
{
    my $src = 'no strict "refs"; my $f = "v"; for ${*$f} (5,11,33) { print }';
    my $doc = PPI::Document->new(\$src);
    ok( $doc,
        'for ${*$f} (LIST) {} should parse (valid Perl foreach lvalue)' )
        or diag "PPI errstr: " . PPI::Document->errstr;
}
