#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

#
# PPI tokenizer/lexer bug report — fifteen small cases.
# Tested against PPI 1.291 / perl 5.40.3.  All fifteen tests currently FAIL (the bugs).
#
#   perl ppi-bug-report.t
#
use strict;
use warnings;
use Test::More tests => 51;
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

# ── Bug 1b: $${EXPR} — the same mis-lex, plus a STRUCTURE error on the braces ──
#
# $${$ref} means ${ ${$ref} }.  PPI emits Magic '$$' again, and then — because
# the braces follow what it believes is a variable — structures them as a
# PPI::Structure::Subscript, i.e. a hash key.  Compare ${$ref}, which PPI
# correctly gives as Cast '$' + PPI::Structure::Block.  A consumer that repairs
# only the Magic still has a subscript with no base in front of it.
{
    my $doc = PPI::Document->new(\'$${$ref}');
    my ($sub) = @{ $doc->find('PPI::Structure::Subscript') || [] };
    ok( !$sub,
        '$${$ref}: the braces are a deref BLOCK, not a Subscript' )
        or diag 'got a Subscript: ' . $sub->content;
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

# ── Bug 5: a VARIABLE declaration's attribute is not a Token::Attribute ────────
#
# `my $x : shared = 1;` is valid Perl (prints 1).  PPI produces a
# PPI::Token::Attribute for the same syntax on a SUB (`sub f : lvalue {…}`),
# but inside a PPI::Statement::Variable the attribute run comes back as a bare
# Operator ':' plus ordinary Words — indistinguishable from an unrelated
# expression fragment, so a consumer that supports `my $x <trailing expr>`
# silently takes ": shared = 1" as that expression.
{
    my @t = toks('my $x : shared = 1;');
    ok( grep(/^PPI::Token::Attribute=/, @t),
        'my $x : shared = 1 should yield a Token::Attribute, as sub attributes do' )
        or diag "got: @t";
}

# ── Bug 6: ${ PUNCTUATION } is a variable, but lexes as Cast + Block ───────────
#
# Perl's ${ NAME } accepts a punctuation name, so @{+} IS the magic array @+
# (and ${!} is $!, %{+} is %+):
#
#   $ perl -e '"ab" =~ /(a)/; print "@{+}"'
#   a
#
# PPI already folds the IDENTIFIER spelling (@{foo} -> @foo) and the caret
# spelling, but leaves the punctuation ones as Cast + Block{lone Operator}.  A
# deref block holding exactly one Operator token can never be an expression, so
# the fold is unambiguous.
{
    my @t = toks('@{+}');
    ok( !grep(/^PPI::Token::Structure=\{$/, @t),
        '@{+} should fold to the magic array @+, as @{foo} folds to @foo' )
        or diag "got: @t";
}

# ── Bug 7: `for` takes only [my] $scalar, and swallows the rest of the file ────
#
# Both of these are valid Perl — `for \my %e (@l)` (refaliasing, 5.22+) and
# `for my ($q, $r) (@l)` (n-at-a-time, 5.36+).  PPI's loop-variable slot accepts
# neither, and unlike bug 4 it does not fail loudly: the Statement::Compound
# keeps ONLY the keyword, and the rest of the construct plus every following
# statement up to the next ';' is swallowed into one flat sibling statement.  So
# a consumer sees a `for` with no list and no block, and loses unrelated code
# with it.  (Compare `for my $q (@l) { A() } print "x";`, which parses into two
# statements as expected.)
{
    my $src = 'for my ($q, $r) (@l) { A() } print "x";';
    my $doc = PPI::Document->new(\$src);
    my ($first) = $doc ? $doc->schildren : ();
    ok( $doc && $first && $first->content =~ /\{/,
        'for my ($q,$r) (LIST) {…} should keep its list and block in the Compound' )
        or diag "compound was: " . ($first ? $first->content : '(no parse)');
}

# ── Bug 8: /PATTERN/ after a paren-less WORD is read as division ───────────────
#
# After a bareword that is not a known unary operator, perl expects a TERM, so
# the `/` starts a match against $_:
#
#   $ perl -e 'sub ok { print "ok(@_)\n" } $_ = "aa"; ok /a/, "desc"'
#   ok(1 desc)
#   $ perl -e '$_ = "aa"; print /a/, "\n"'      # print, a core list operator
#   1
#
# PPI already gets this right after `grep`, `return`, `(` and `=`, but after any
# other Word it emits Operator '/' + Word + Operator '/' — i.e. two divisions —
# so the match, and the whole argument list with it, is lost.  A consumer cannot
# recover it: the pattern's own text has been re-tokenized as code.
{
    my @t = toks('ok /x/, "d";');
    ok( grep(/^PPI::Token::Regexp::Match=/, @t),
        '/x/ after a paren-less word should be a match, as it is after grep/return' )
        or diag "got: @t";
}

# ── Bug 9: `)*name` is lexed as a GLOB instead of multiplication ──────────────
#
# A `*` where a term has just ENDED can only be multiplication; a glob starts
# where a TERM can.  perl agrees:
#
#   $ perl -e 'my ($s,$k)=(0,"ab"); $s += length($k)*length($k); print "$s\n"'
#   4
#
# PPI makes `*length` one Token::Symbol when the previous token ends a term —
# after `)`, `]`, a subscript `}`, a Symbol or a Quote.  With a NUMBER on the
# left (`2*length($k)`) or a single space (`) * length`) it is correct, which is
# what makes this easy to miss.  A consumer sees `Word List Symbol List` and has
# no way back: the multiplication is gone.
{
    my @t = toks('$s += length($k)*length($k);');
    ok( !grep(/^PPI::Token::Symbol=\*/, @t),
        ')*name after a term should be Operator(*) + Word, not a glob Symbol' )
        or diag "got: @t";
}

# ── Bug 10: parsing depends on $/ — a trailing __END__ gains a newline ────────
#
# `$/` is the input-record separator for READING; it has no business affecting
# how source text is tokenized.  With `$/` undef (slurp mode — what code that
# has just read the source with `local $/` leaves behind), a document whose last
# line is `__END__`/`__DATA__` comes back one byte longer: serialize is no
# longer the identity, and the DATA section gains a line the file never had.
{
    my $src = "# c\n__END__\n";
    my $slurped = do { local $/; PPI::Document->new(\$src)->serialize };
    is( $slurped, $src,
        'serialize round-trips a trailing __END__ section regardless of $/' );
}

# ── Bug 11: <FH> / <glob> after a list operator or a block is lexed as < … > ──
#
# `<…>` in TERM position is a readline/glob wherever that position comes from.
# perl:
#
#   $ echo x | perl -e 'print <STDIN>'
#   x
#
# PPI gets it right after `=` and after a comma, and wrong after a
# list-operator Word (`sort <STDIN>`, `print <STDIN>`) or after a closing brace
# it took for the end of a term (`map { $h{$_}++ } <op/*>`), where it emits
# Operator('<') … Operator('>') — a comparison chain.  This is the same
# operator-vs-term error as bugs 8 and 9, in the third direction.
{
    my @t = toks('sort <STDIN>;');
    ok( grep(/^PPI::Token::QuoteLike::Readline=/, @t),
        '<STDIN> after a list operator should be a readline, as it is after a comma' )
        or diag "got: @t";
}
#
# THE CASCADE, which is the expensive half: once `>` has been taken for an
# operator, the NEXT `/` is in term position too, so it starts a match — and
# an unterminated one swallows the rest of the statement.
#
#   $ perl -e 'open(my $f,"<","/etc/hostname"); my $x = 1 ? <$f> // "" : ""; print $x'
#   <the first line of /etc/hostname>
#
{
    my @t = toks('my $x = $ok ? <$f> // "" : "";');
    ok( !grep(/^PPI::Token::Regexp::Match=/, @t),
        '`<$f> // ""` in a ternary branch: the // is defined-or, not an empty match' )
        or diag "got: @t";
}
{
    my @t = toks('my $x = $ok ? <$f> / 2 : 0;');
    ok( !grep(m{^PPI::Token::Regexp::Match=/ 2 : 0;$}, @t),
        '`<$f> / 2` in a ternary branch: the / is division, and must not eat the statement' )
        or diag "got: @t";
}
#
# THE CASCADE IS WORSE WHEN THE BODY IS A GLOB PATTERN, because the `/` that
# starts the match is INSIDE the diamond — so the closing `>` is swallowed too,
# and the match runs on to the next `/`, which is usually lines away:
#
#   $ ls ./nope-*-xyz ; perl -e 'my @f = sort <./nope-*-xyz>; print "ok\n"'
#   ok
#
# `sort <./nope-*-xyz>; print "y"; print "z";` is FOUR tokens to PPI, the last
# of them a Regexp::Match holding both prints.
{
    my @t = toks('my @f = sort <./nope-*-xyz>;');
    ok( grep(/^PPI::Token::QuoteLike::Readline=/, @t),
        '`sort <./nope-*-xyz>` — a `.`-relative glob pattern should be a readline token' )
        or diag "got: @t";
}
{
    my @t = toks('my @f = sort <./a-*>; print "y";');
    ok( !grep(m{^PPI::Token::Regexp::Match=/a-\*>; print "y";$}, @t),
        'the glob-pattern cascade must not swallow the statements that follow' )
        or diag "got: @t";
}

# ── Bug 12: `)` followed by -1 swallows the operator into a negative NUMBER ───
#
#   $ perl -e 'print( (1+2)-1 )'
#   2
#
# After `)` a term has ENDED, so `-` is an operator.  PPI emits Number('-1'),
# and the subtraction is gone: the consumer sees two adjacent terms.  With a
# space (`(1+2) - 1`) it is correct, which is what makes it easy to miss.
{
    my @t = toks('my $x = (1+2)-1;');
    ok( !grep(/^PPI::Token::Number=-1$/, @t),
        ')-1 should be Operator(-) + Number(1), not a negative literal' )
        or diag "got: @t";
}

# ── Bug 13: perl 5.40's `^^` (logical XOR) is two `^` operators ───────────────
#
# 5.40 added `^^`.  PPI emits Operator('^') twice, which is a different
# expression (bitwise XOR applied twice) and cannot be told apart from one.
{
    my @t = toks('my $r = $a ^^ $b;');
    ok( grep(/^PPI::Token::Operator=\^\^$/, @t),
        '^^ should be one logical-XOR operator token' )
        or diag "got: @t";
}

# ── Bug 14: a SUBSCRIPT after a deref / a KV slice gets the wrong structure ───
#
# `${$r}[0]` is element 0 of @$r and `%h{...}` (5.20+) is a key/value slice: the
# bracketed part SUBSCRIPTS what precedes it.  PPI builds Structure::Constructor
# (an anonymous arrayref) and Structure::Block (a code block) instead — while
# the sibling `@h{...}` correctly gets a Structure::Subscript.
{
    my $doc = PPI::Document->new(\'my $v = ${$r}[0];');
    my @s = map { ref } @{ $doc->find(sub { $_[1]->isa('PPI::Structure') }) || [] };
    ok( (grep { $_ eq 'PPI::Structure::Subscript' } @s),
        '[0] after a braced deref should be a Subscript, not a Constructor' )
        or diag "got: @s";
}

# ── Bug 15: `finally {…}` is not part of the try statement, and eats the next ─
#
# With `use feature 'try'` in scope, `try {…} catch (VAR) {…} finally {…}` is
# ONE self-terminating statement.  PPI builds a Statement::Compound for the
# try/catch part and stops; `finally {…}` starts an unterminated statement that
# then swallows everything up to the next `;` — here, a whole assertion.
{
    my $doc = PPI::Document->new(\<<'PERL');
use feature 'try';
try { foo(); } catch ($e) { bar($e); } finally { baz(); }
is($x, 1, 'desc');
PERL
    my @s = grep { $_->isa('PPI::Statement') } $doc->schildren;
    ok( (grep { $_->content =~ /^is\(/ } @s),
        'the statement after a finally block should be its own statement' )
        or diag "got: " . join(' | ', map { $_->content =~ s/\s+/ /gr } @s);
}

# ── Bug 16: a call to a sub named `x` is lexed as the repetition operator ────
#
# `x` is both an operator and a legal sub name, and perl decides by asking
# whether a complete TERM precedes it.  After a list operator there is none, so
# `print x(), "|\n"` calls x() — PPI counts the Word `print` as a term and
# emits Operator(x), which reads as "print $_ repeated () times".
{
    my $doc = PPI::Document->new(\'sub x { "PKG" } print x(), "|\n";');
    my ($op) = grep { $_->isa('PPI::Token::Operator') && $_->content eq 'x' }
               $doc->tokens;
    ok( !$op, '`x` after a list operator should be a Word (a call), not an Operator' )
        or diag "got Operator(x) at: " . join(' ',
             map { ref($_) . '[' . $_->content . ']' }
             grep { $_->significant } $doc->tokens);
}
# ── Bug 16b: the same `x` STARTING a statement ──────────────────────────────
#
# `x` is INFIX, so a statement cannot begin with it — yet PPI begins one with it
# whenever the previous token is a sub definition's closing brace, which is
# exactly how a sub named `x` is called with neither parens nor arguments.
# perl's own t/op/lexsub.t writes `{ my sub x {…} x }` twice.
{
    my $doc = PPI::Document->new(\'sub x { print "PKG\n" } x');
    my ($op) = grep { $_->isa('PPI::Token::Operator') && $_->content eq 'x' }
               $doc->tokens;
    ok( !$op, 'a statement cannot BEGIN with the infix `x` — it is a call' )
        or diag "got Operator(x) at: " . join(' ',
             map { ref($_) . '[' . $_->content . ']' }
             grep { $_->significant } $doc->tokens);
}
{
    my $doc = PPI::Document->new(\'my sub x { print "LEX\n" } x');
    my ($op) = grep { $_->isa('PPI::Token::Operator') && $_->content eq 'x' }
               $doc->tokens;
    ok( !$op, 'the lexical-sub spelling lexes the same way (t/op/lexsub.t)' );
}
# The `;` spelling proves the intent: one added semicolon and PPI lexes the
# very same `x` as the Word it is.
{
    my $doc = PPI::Document->new(\'sub x { print "PKG\n" }; x');
    ok( (grep { $_->isa('PPI::Token::Word') && $_->content eq 'x' } $doc->tokens) >= 2,
        '`}; x` lexes the call as a Word (the control)' );
}

# ── Bug 17: only one of perl's three ways to enable `try` is recognised ──────
#
# `use feature 'try'`, `use experimental 'try'` and the version bundles
# `use v5.40` / `use 5.040` all enable the same feature in perl.  PPI knows only
# the first, so under the other two the construct lexes like the no-pragma case:
# ONE unterminated statement that swallows the statement after it.
{
    my $doc = PPI::Document->new(\<<'PERL');
use v5.40;
try { foo(); } catch ($e) { bar($e); }
is($x, 1, 'desc');
PERL
    my @s = grep { $_->isa('PPI::Statement') } $doc->schildren;
    ok( (grep { $_->content =~ /^is\(/ } @s),
        'a version bundle >= 5.39 should enable try (its bundle contains it)' )
        or diag "got: " . join(' | ', map { $_->content =~ s/\s+/ /gr } @s);
}
{
    my $doc = PPI::Document->new(\<<'PERL');
use experimental 'try';
try { foo(); } catch ($e) { bar($e); }
is($x, 1, 'desc');
PERL
    my @s = grep { $_->isa('PPI::Statement') } $doc->schildren;
    ok( (grep { $_->content =~ /^is\(/ } @s),
        "use experimental 'try' should enable try (it IS feature->import)" )
        or diag "got: " . join(' | ', map { $_->content =~ s/\s+/ /gr } @s);
}
# …and it must not answer about a feature it was not asked about: the
# experimental branch returns `signatures => 0` for ANY argument list, so this
# turns OFF the signatures the line before switched on.
{
    my $doc = PPI::Document->new(\"use feature 'signatures';\nuse experimental 'try';\n");
    my (undef, $exp) = @{ $doc->find('PPI::Statement::Include') || [] };
    my $mods = $exp->feature_mods || {};
    ok( !exists $mods->{signatures},
        "use experimental 'try' should say nothing about signatures" )
        or diag "got: " . join(', ', map { "$_=$mods->{$_}" } sort keys %$mods);
}

# ── Bug 20: a term-initial `~~` is lexed as the smart-match operator ─────────
# `~~` is the smart match only where an operator may stand.  Where a TERM is
# expected perl reads two complements — `~(~$x)`, the "numify" idiom that
# perl's own t/op/bop.t asserts twice.  PPI gives one Operator token in every
# position, so a statement that starts an argument with it has a binary
# operator with no left operand.  (PPI already makes exactly this
# term-or-not decision correctly for `x` and for `/PATTERN/`.)
{
    my $doc = PPI::Document->new(\'is(~~$y, 3);');
    my @ops = grep { $_->isa('PPI::Token::Operator') } $doc->tokens;
    ok( !(grep { $_->content eq '~~' } @ops),
        'a `~~` with no term before it should lex as two `~` complements' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}

# ── Bug 21: a filetest after a SCALAR filehandle is split into `-` + WORD ────
# `print FILEHANDLE LIST` takes a bareword, a scalar or a block as the handle,
# and a leading `-X` in the LIST is ONE filetest operator in all three (perl
# -MO=Deparse agrees).  PPI gets the bareword right and splits the scalar and
# block forms into Operator('-') + Word('e') — a subtraction of a call.  There
# is no competing reading to protect: `-e` cannot be a binary operator at all,
# and `$n -e $b` is a perl syntax error.  Adjacency is the discriminator —
# `print $fh - e $f` really IS `-(e($f))`, and perl honours the space.
{
    my $doc = PPI::Document->new(\'print $fh -e $f;');
    my @ops = grep { $_->isa('PPI::Token::Operator') } $doc->tokens;
    ok( (grep { $_->content eq '-e' } @ops),
        'a filetest after a scalar filehandle should lex as one `-e` operator' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# The block-handle form splits the same way…
{
    my $doc = PPI::Document->new(\'print {$x} -e $f;');
    my @ops = grep { $_->isa('PPI::Token::Operator') } $doc->tokens;
    ok( (grep { $_->content eq '-e' } @ops),
        'a filetest after a block filehandle should lex as one `-e` operator' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# …while the BAREWORD handle already lexes correctly — the inverse that shows
# the two paths disagree about the same operator.
{
    my $doc = PPI::Document->new(\'print STDERR -e $f;');
    my @ops = grep { $_->isa('PPI::Token::Operator') } $doc->tokens;
    ok( (grep { $_->content eq '-e' } @ops),
        'a filetest after a BAREWORD filehandle lexes as one `-e` (control)' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}

# ── Bug 22: a `$` scalar with a NON-ASCII name splits into Cast + Word ───────
# perl has allowed unicode identifiers since 5.8.  PPI reads them for every
# sigil except `$`: PPI/Token/Unknown.pm's `$` branch tests /[a-z_]/i where its
# `*`, `%`, `&` and `@` siblings test /[\w:]/.  The name itself is fine — once
# the token is classed Symbol, PPI::Token::Symbol consumes it with
# m/\G([\w:\']+)/gc.
{
    my $doc = PPI::Document->new(\"use utf8;\n\$\x{ff38} = 1;\n");
    my @sym = grep { $_->isa('PPI::Token::Symbol') } $doc->tokens;
    ok( (grep { $_->content eq "\$\x{ff38}" } @sym),
        'a scalar with a non-ASCII name should lex as one Symbol' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# The same name under `%` lexes correctly — the inverse that shows the two
# paths disagree about the same identifier.
{
    my $doc = PPI::Document->new(\"use utf8;\n%\x{ff38} = ();\n");
    my @sym = grep { $_->isa('PPI::Token::Symbol') } $doc->tokens;
    ok( (grep { $_->content eq "%\x{ff38}" } @sym),
        'a HASH with a non-ASCII name lexes as one Symbol (control)' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# Second-order: because the lexer sees a bareword, the subscript that follows
# is built as a BLOCK (and `[…]` as an anonymous-array constructor), so the
# statement is not a subscripted variable at all.
{
    my $doc = PPI::Document->new(\"use utf8;\n\$\x{ff38}{a} = 1;\n");
    ok( scalar @{ $doc->find('PPI::Structure::Subscript') || [] },
        'the `{…}` after a non-ASCII scalar should be a Subscript' )
        or diag "got: " . join(' ', map { ref($_) } grep { $_->significant } $doc->tokens);
}
# Same bug, worse consequence: a non-ASCII FOREACH loop variable makes the LEXER
# fail the whole document ("Illegal state in 'foreach' compound statement"),
# because the foreach-slot state machine sees the Cast the `$` split produced
# where it expects a Symbol.  `for (my $X=0;...)` and `while (my $X = ...)`
# lex fine.  PPI::Document->new returns undef, so no repair is possible.
{
    my $doc = PPI::Document->new(\"use utf8;\nfor my \$\x{ff29} (1,2) { 1 }\n");
    ok( defined $doc,
        'a foreach with a non-ASCII loop variable should lex at all' )
        or diag "errstr: " . (PPI::Document->errstr // '(none)');
}

# ── Bug 24: a PUNCTUATION-named array is split into Cast + Operator ───────────
#
# perl lets any punctuation character name a global, and real code writes them:
# t/re/subst.t:346 is `ok( ! @?, 'parsing of split subst with comment' );`.
# PPI has %MAGIC entries for the arrays perl documents (@-, @+, @*, @_, …) and
# its Symbol name regex is word-bounded, so every OTHER punctuation name falls
# through to the `@` CAST branch and the next character is tokenized alone.
#
# It is not a missing feature: in valid perl a `@` Cast is only ever followed by
# `$`, `{` or an identifier, so `Cast + Operator` is a parse of no legal program
# at all.  perl accepts @? @! @. @/ @~ @^ @& @% @= @< @> (probed, 5.40.3).
{
    my $doc = PPI::Document->new(\'@? = (1,2);');
    my @sym = grep { $_->isa('PPI::Token::Symbol') } $doc->tokens;
    ok( (grep { $_->content eq '@?' } @sym),
        'a punctuation-named array `@?` should lex as one Symbol' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# The documented siblings prove the intent — @- and @+ ARE single Magic tokens,
# so the name rule is an enumeration where perl has a character class.
{
    my $doc = PPI::Document->new(\'my $n = scalar(@!);');
    my @sym = grep { $_->isa('PPI::Token::Symbol') || $_->isa('PPI::Token::Magic') }
                   $doc->tokens;
    ok( (grep { $_->content eq '@!' } @sym),
        'a punctuation-named array `@!` should lex as one token, as `@-` does' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# The `%` sigil is worse, because there is no Cast to notice: `keys %?` comes
# out as Word(keys) Operator(%) Operator(?), i.e. a modulus of nothing.  perl
# decides by POSITION, and the surprise is which way it goes:
#
#   $ perl -e 'sub f { 7 } print f % 3, "\n"'
#   7                        # `%3, "\n"` is the HASH %3, passed to f
#   $ perl -e 'sub f { 7 } print f() % 3, "\n"'
#   1                        # after `)` a term has ended: modulo
{
    my $doc = PPI::Document->new(\'my @k = keys %?;');
    my @sym = grep { $_->isa('PPI::Token::Symbol') } $doc->tokens;
    ok( (grep { $_->content eq '%?' } @sym),
        'a punctuation-named hash `%?` should lex as one Symbol after `keys`' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}

# ── Bug 25: `-name` after a token that ENDS A TERM is one negative-bareword ───
#
# The third sibling of bugs 12 (`)*name`) and 15 (`)-1`).  After a `)` a term
# has ended, so `-` is binary minus; the negative-bareword string form can only
# start where a TERM can.  perl -MO=Deparse gives `length('abc') - length('a')`.
# A space fixes the lexing, and PPI already makes exactly this operator-vs-term
# decision correctly for `x` and for `/PATTERN/`.
{
    my $doc = PPI::Document->new(\'my $z = length("abc")-length("a");');
    ok( !(grep { $_->isa('PPI::Token::Word') && $_->content eq '-length' } $doc->tokens),
        '`)-length` should lex as Operator(-) + Word(length), not one Word' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# The spaced spelling proves the intent: the SAME source with one blank added
# lexes the way perl reads both of them.
{
    my $doc = PPI::Document->new(\'my $z = length("abc") - length("a");');
    ok( (grep { $_->isa('PPI::Token::Operator') && $_->content eq '-' } $doc->tokens),
        '`) - length` lexes as a minus operator (the control)' );
}

# ── Bug 26: a glob named by PUNCTUATION or DIGITS is split into two tokens ────
#
# perl names a glob with whatever names a variable, and punctuation and digits
# name variables, so `*-`, `*!` and `*1` are globs.  perl's own test suite
# writes them: `*X = *-;` (t/re/reg_namedcapture.t:18) aliases the
# named-capture hash, `local *a = *1;` (t/re/subst.t:951) aliases $1.
#
#   $ perl -e '"X"=~/(?<X>X)/; our %X; *X = *-; print keys %X'
#   X
#
# PPI::Token::Symbol's name is word-bounded, so only `*word` is a Symbol; the
# rest arrive as Operator('*') + Operator/Number.  Same class as bug 24
# (`@?` → Cast + Operator), one sigil over.
{
    my $doc = PPI::Document->new(\'our %X; *X = *-;');
    ok( (grep { $_->isa('PPI::Token::Symbol') && $_->content eq '*-' } $doc->tokens),
        '`*-` should lex as one Symbol, as `*foo` does' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
{
    my $doc = PPI::Document->new(\'local *a = *1;');
    ok( (grep { $_->isa('PPI::Token::Symbol') && $_->content eq '*1' } $doc->tokens),
        '`*1` should lex as one Symbol — a digit run names a glob too' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# Two more spellings of the same bug.  `*^R` is perl's caret convention for the
# glob named chr(18) — the one `$^R` reads — so it is three tokens, not two:
#
#   $ perl -e 'our $s = "V"; *^R = *s; print $^R'
#   V
#
# and `*]` (t/op/tie_fetch_count.t:189) is the glob whose scalar slot is `$]`,
# where PPI hands the bracket over as a Token::Structure parked in a
# PPI::Statement::UnmatchedBrace.
{
    my $doc = PPI::Document->new(\'*^R = *g;');
    ok( (grep { $_->isa('PPI::Token::Symbol') && $_->content eq '*^R' } $doc->tokens),
        '`*^R` should lex as one Symbol — the caret convention names a glob too' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
{
    my $doc = PPI::Document->new(\'tie my $v => "main", *];');
    ok( (grep { $_->isa('PPI::Token::Symbol') && $_->content eq '*]' } $doc->tokens),
        '`*]` should lex as one Symbol — a closing bracket names the glob holding $]' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# And the SIGIL spelling, where NEITHER token is an Operator: `$@` is a
# variable, so `*@` is the glob that holds it — but `@` is also a sigil, so PPI
# makes both tokens Casts.  perl-tests/local.t:828 is `local *@;`.
#
#   $ perl -e 'eval { die "b\n" }; { local *@; } print "kept=[$@]"'
#   kept=[b
#   ]
{
    my $doc = PPI::Document->new(\'local *@;');
    ok( (grep { $_->isa('PPI::Token::Symbol') && $_->content eq '*@' } $doc->tokens),
        '`*@` should lex as one Symbol — a sigil character names a glob too' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}

# ── Bug 27: ->symbol answers `%x` for the `$x` in `*$x{SLOT}` ────────────────
# NOT a mis-tokenization — the token stream for `*$x{SCALAR}` is right
# (Cast(*) Symbol($x) Structure::Subscript({SCALAR})).  It is ->symbol, the
# method documented to return "the ACTUAL symbol this token refers to", which
# resolves `$foo{…}` to `%foo` UNLESS a cast trumps the braces — and its cast
# set is `qw{ $ @ % }`, so `*` is missing.  But `*$x{SCALAR}` is perl's
# glob-slot syntax, `*{$x}{SCALAR}`, whose operand is the SCALAR $x:
#
#   $ perl -e 'our $n = "g"; *{$n} = \10; print ${*$n{SCALAR}}'
#   10
#
# and `*$x[0]{…}` is not valid perl at all, so braces after a `*` cast are
# never an element access.  Five sites in core Carp.pm depend on it
# (`*$_{HASH}`, `${*$_{SCALAR}}`), plus t/op/filetest.t, t/op/stat.t,
# t/op/gv.t and t/uni/parser.t.  One-token fix: add `*` to
# %cast_which_trumps_braces in PPI/Token/Symbol.pm.
{
    my $doc = PPI::Document->new(\'my $x = *$a{SCALAR};');
    my ($sym) = grep { $_->isa('PPI::Token::Symbol') && $_->content eq '$a' } $doc->tokens;
    is( ($sym ? $sym->symbol : '(no token)'), '$a',
        '`$a` under a `*` cast is the SCALAR $a — the glob-slot operand' );
}
{
    my $doc = PPI::Document->new(\'return *$_{HASH};');
    my ($sym) = grep { $_->isa('PPI::Token::Magic') && $_->content eq '$_' } $doc->tokens;
    is( ($sym ? $sym->symbol : '(no token)'), '$_',
        '`$_` under a `*` cast is $_, not %_ (core Carp.pm:34)' );
}

# ── Bug 28: `$x.2` — the `.` is absorbed into a Number::Float ────────────────
#
# The fourth sibling of bugs 12 (`)*name`), 15 (`)-1`) and 25 (`)-name`), and
# the same operator-vs-term decision.  perl starts a number at `.` only where a
# TERM is expected; after a complete term the `.` is concatenation, so `$_.2` is
# `$_ . 2` (perl -MO=Deparse says so).  PPI leaves NO operator in the stream at
# all, so a consumer sees two juxtaposed terms.  A space or a non-digit on the
# right fixes it — the bug needs the digit adjacent.
{
    my $doc = PPI::Document->new(\'my $x = $_.2;');
    ok( !(grep { $_->isa('PPI::Token::Number') && $_->content eq '.2' } $doc->tokens),
        '`$_.2` should lex as Operator(.) + Number(2), not one Number::Float' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
{
    my $doc = PPI::Document->new(\'my $z = $y[1].3;');
    ok( (grep { $_->isa('PPI::Token::Operator') && $_->content eq '.' } $doc->tokens),
        '`].3` should carry a concatenation operator' )
        or diag "got: " . join(' ', map { ref($_) =~ s/^PPI::Token:://r . "[" . $_->content . "]" }
                                    grep { $_->significant } $doc->tokens);
}
# The spaced spelling proves the intent: the SAME source with one blank added
# lexes the way perl reads both of them.
{
    my $doc = PPI::Document->new(\'my $x = $_ . 2;');
    ok( (grep { $_->isa('PPI::Token::Operator') && $_->content eq '.' } $doc->tokens),
        '`$_ . 2` lexes as a concatenation operator (the control)' );
}
# …and the NEGATIVE, which PPI gets right and must keep getting right: where a
# TERM is expected, `.5` really is the number one half.
{
    my $doc = PPI::Document->new(\'my $x = .5;');
    ok( (grep { $_->isa('PPI::Token::Number') && $_->content eq '.5' } $doc->tokens),
        '`= .5` is the NUMBER 0.5 (the negative control)' );
}
