#!/usr/bin/env perl
#
# PPI tokenizer bug report — two small cases.
# Tested against PPI 1.291 / perl 5.40.3.  Both tests currently FAIL (the bugs).
#
#   perl ppi-bug-report.t
#
use strict;
use warnings;
use Test::More tests => 2;
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
