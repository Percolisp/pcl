# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::PPIBugs;

# Pl::PPIBugs -- THE PPI-BUG REGISTRY, as DATA (s525p; parser-structure
# program step 0, member c1; design `docs/parser-review-s525.md` §7;
# measurement `docs/expect-state-checker-s525.md`).
#
# ONE ROW PER BUG, keyed by its `docs/ppi-upstream-bugs.md` section (`id`).
# The catalogue keeps the prose (impact, history); this module keeps what a
# program can check.  It is DATA: a list of hashes, one accessor, no logic.
# Nothing in the compiler reads it yet -- the 1a expect-state engine (task
# #3694) is its first consumer; today its consumer is the canary test
# `Pl/t/ppi-bugs-registry-01.t`.
#
# THE SCHEMA (review §7, as built):
#   id        the catalogue section number
#   kind      token      PPI gives a wrong token CLASS or wrong boundaries
#                        (the expect-state checker sees it, `when` says how)
#             structure  the tokens are right, PPI's tree is wrong (a Block
#                        that is a hash, a statement split in two)
#             source     the bug is in how PPI reads the TEXT (before tokens)
#             lexer-dies PPI::Document->new returns undef: no document at all
#   dump      the dump FORM `tokens` is compared in: tokens | structure |
#             statements | lexer | heredoc | symbol | slurp -- each defined
#             once, in Pl/t/ppi-bugs-registry-01.t (the forms say what a
#             `got` string means; `tokens` is the report file's
#             "Class=content" convention without the `PPI::Token::` prefix)
#   title     the catalogue heading
#   ppi       { confirmed => the PPI version the bug was last seen on,
#               fixed_in  => undef while it reproduces;
#                            'pcl' when PCL's PPI branch fixes it (s527a:
#                            vendor/PPI, docs/ppi-fix-at-source-s527.md);
#                            else the upstream PPI version that fixed it }
#   when      token rows: the checker's contradiction-table row that flags it
#             (Pl::PPIExpect: expect state, PPI class, content, predicate,
#             check = the contradiction kind); other rows: { check => the
#             checker kind that sees it or undef, shape => one line }
#   fix       { kind => relex | retag | handler, handler => today's repair
#               sub (undef = no workaround shipped) } -- a NAME, no engine yet;
#               a row fixed on the branch adds commit => the fix commit on
#               branch `pcl`, group => its PR group letter
#               (s527/upstream/PR-<letter>.md), pr => the upstream PR number
#               once there is one (undef until then)
#   repro     a minimal, RUNNABLE, valid perl program
#   perl      perl 5.40.3's output for `repro` (PROBED, s525p)
#   tokens    { got => PPI's dump of `repro` in the `dump` form (the bug),
#               want => the correct dump }
#   unblocks  test files the catalogue section names
#   tasks     task numbers the catalogue section names
#
# ADD A BUG: append a row (the next free catalogue §), with `got` taken from
# a real dump (`perl -MPl::PPIBugs` is not needed -- run the test, it prints
# the dump of any row whose `got` disagrees).  REMOVE A BUG (PPI fixed it):
# the canary test fails with "PPI <version> fixed §<id>: set fixed_in"; set
# `fixed_in` to that version -- the row then asserts `want` instead, and the
# repair named in `fix` can be deleted (the 1a engine skips a row whose
# fixed_in is <= the installed PPI).  A row is never deleted: a fixed bug
# stays as the guard that it stays fixed.
#
# FIXED ON PCL'S BRANCH (fixed_in => 'pcl', s527a).  PCL parses with the
# vendored PPI (Pl::VendorPPI), so the canary runs in two modes:
#   * on the VENDORED lib (the gate) a 'pcl' row asserts `want` -- the
#     branch keeps it fixed;
#   * on a SYSTEM PPI (PCL_SYSTEM_PPI=1 prove Pl/t/ppi-bugs-registry-01.t)
#     a 'pcl' row asserts `got`, and fails with
#         upstream PPI <version> fixed §<id>: set fixed_in
#     when the installed PPI gives `want`: upstream has released the fix, so
#     set fixed_in to that version (and the branch commit can be dropped at
#     the next rebase).  This is the "did upstream catch up" check; run it at
#     every vendor refresh (tools/vendor-ppi).

use strict;
use warnings;
use utf8;

our @BUGS = (
 { id => 1, kind => 'token', dump => 'tokens',
   title => '`$$$ref` (triple dereference) tokenized as `$$` + `$ref`',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => '*', class => 'Magic', content => '$$', pred => 'adj_deref_follows', check => 'pid-magic-is-cast-run' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::PExpr::_split_pid_magic_cast_run (s527b: PPI fixed on the branch; upstream PR #428 taken)', commit => 'eb16e9d2', group => 'Q', pr => 428 },
   repro => 'my $v = 7; my $ref = \\\\$v; print $$$ref, "\\n";',
   perl => "7\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$v', 'Operator==', 'Number=7', 'Structure=;', 'Word=my', 'Symbol=$ref', 'Operator==', 'Cast=\\', 'Cast=\\', 'Symbol=$v', 'Structure=;', 'Word=print', 'Magic=$$', 'Symbol=$ref', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=$v', 'Operator==', 'Number=7', 'Structure=;', 'Word=my', 'Symbol=$ref', 'Operator==', 'Cast=\\', 'Cast=\\', 'Symbol=$v', 'Structure=;', 'Word=print', 'Cast=$', 'Cast=$', 'Symbol=$ref', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/prefix-incr-deref-01.t', 'Pl/t/transpile-test-10.t', 'perl-tests/ref.t', 't/op/gv.t', 't/uni/gv.t'],
   tasks => [305, 463] },
 { id => 2, kind => 'token', dump => 'tokens',
   title => 'C99 hex-float literal `0x1.8p+1` split into 5 tokens',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Number::Float', content => qr/^\.\d/, pred => 'prev_adj_number', check => 'hexfloat-split' },
   fix => { kind => 'relex', handler => undef, commit => 'd93bd718', group => 'I', pr => undef },
   repro => 'print 0x1.8p+1, "\\n";',
   perl => "3\n",
   tokens => {
     got  => ['Word=print', 'Number::Hex=0x1', 'Number::Float=.8', 'Word=p', 'Operator=+', 'Number=1', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=print', 'Number::Float=0x1.8p+1', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['perl-tests/hexfp.t'],
   tasks => [] },
 { id => 3, kind => 'token', dump => 'tokens',
   title => '`< EXPR >` comparison chain misread as a glob/readline',
   ppi => { confirmed => '1.291', fixed_in => '1.291' },
   when => { check => undef, shape => 'FIXED in PPI 1.291: a `< EXPR >` comparison chain was read as a glob/readline' },
   fix => { kind => 'relex', handler => undef },
   repro => 'my $x = [1, 2, 3]; print "ok\\n" if $x->[0] < $x->[1] && $x->[2] > $x->[1];',
   perl => "ok\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$x', 'Operator==', 'Structure=[', 'Number=1', 'Operator=,', 'Number=2', 'Operator=,', 'Number=3', 'Structure=]', 'Structure=;', 'Word=print', 'Quote::Double="ok\\n"', 'Word=if', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=0', 'Structure=]', 'Operator=<', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=1', 'Structure=]', 'Operator=&&', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=2', 'Structure=]', 'Operator=>', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=1', 'Structure=]', 'Structure=;'],
     want => ['Word=my', 'Symbol=$x', 'Operator==', 'Structure=[', 'Number=1', 'Operator=,', 'Number=2', 'Operator=,', 'Number=3', 'Structure=]', 'Structure=;', 'Word=print', 'Quote::Double="ok\\n"', 'Word=if', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=0', 'Structure=]', 'Operator=<', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=1', 'Structure=]', 'Operator=&&', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=2', 'Structure=]', 'Operator=>', 'Symbol=$x', 'Operator=->', 'Structure=[', 'Number=1', 'Structure=]', 'Structure=;'],
   },
   unblocks => ['Pl/t/misc-fixes-02.t'],
   tasks => [] },
 { id => 4, kind => 'token', dump => 'tokens',
   title => '`7%-3` tokenized as the magic hash `%-` (modulo lost)',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Magic', content => qr/^%[-+]$/, pred => '', check => 'magic-hash-for-modulo' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser::_fix_modulo_magic (s527b: PPI fixed on the branch)', commit => '1cff9293', group => 'B', pr => undef },
   repro => 'print 7%-3, "\\n";',
   perl => "-2\n",
   tokens => {
     got  => ['Word=print', 'Number=7', 'Magic=%-', 'Number=3', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=print', 'Number=7', 'Operator=%', 'Number=-3', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/misc-fixes-02.t'],
   tasks => [] },
 { id => 5, kind => 'structure', dump => 'structure',
   title => '`{ LITERAL , ... }` (anon hash, comma-separated) misclassified as a Block',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'brace-class', shape => 'statement position: `{` + a string or word + `,` (yyl_leftcurly 6364-6505: an anon hash) built as Structure::Block' },
   fix => { kind => 'handler', handler => 'Pl::Parser::_bare_block_is_anon_hash' },
   repro => 'sub f { { \'a\' , \'foo\' } } print f()->{a}, "\\n";',
   perl => "foo\n",
   tokens => {
     got  => ['Word=sub', 'Word=f', 'Structure={ in Structure::Block < Statement::Sub', 'Structure={ in Structure::Block < Statement::Compound', 'Quote::Single=\'a\'', 'Operator=,', 'Quote::Single=\'foo\'', 'Structure=} in Structure::Block < Statement::Compound', 'Structure=} in Structure::Block < Statement::Sub', 'Word=print', 'Word=f', 'Structure=( in Structure::List < Statement', 'Structure=) in Structure::List < Statement', 'Operator=->', 'Structure={ in Structure::Subscript < Statement', 'Word=a', 'Structure=} in Structure::Subscript < Statement', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=sub', 'Word=f', 'Structure={ in Structure::Block < Statement::Sub', 'Structure={ in Structure::Constructor < Statement', 'Quote::Single=\'a\'', 'Operator=,', 'Quote::Single=\'foo\'', 'Structure=} in Structure::Constructor < Statement', 'Structure=} in Structure::Block < Statement::Sub', 'Word=print', 'Word=f', 'Structure=( in Structure::List < Statement', 'Structure=) in Structure::List < Statement', 'Operator=->', 'Structure={ in Structure::Subscript < Statement', 'Word=a', 'Structure=} in Structure::Subscript < Statement', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/transpile-test-04.t', 't/comp/term.t'],
   tasks => [] },
 { id => 6, kind => 'lexer-dies', dump => 'lexer',
   title => '`for ${*$f} (LIST) { }` — LEXER DIES: "Illegal state in \'for\' compound statement"',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'lexer-dies', shape => 'the lexer dies: `for ${*$f} (LIST)` -- a deref block as the foreach variable' },
   fix => { kind => 'handler', handler => undef },
   repro => 'no strict "refs"; our $v; my $f = "v"; for ${*$f} (5,11,33) { print "$v " } print "\\n";',
   perl => "5 11 33 \n",
   tokens => {
     got  => ['LEXER-DIES: Lexer failed: Illegal state in \'for\' compound statement'],
     want => ['DOCUMENT'],
   },
   unblocks => ['Pl/t/misc-fixes-02.t', 't/op/for.t'],
   tasks => [228, 253, 254] },
 { id => 7, kind => 'token', dump => 'tokens',
   title => '`(sub :lvalue { … })` — an anon sub\'s ATTRIBUTE at expression start becomes a LABEL',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'TERM', class => 'Label', content => qr/^sub\s*:$/, pred => '', check => 'label-not-at-statement-start' },
   fix => { kind => 'retag', handler => undef, deleted => 'Pl::Parser2::_normalize_anon_sub_attrs + _repair_swallowing_prototypes (s527b: PPI fixed on the branch)', commit => '6a5de5c1', group => 'S', pr => undef },
   repro => 'my @a = (sub :lvalue { 1 }); print ref($a[0]), "\\n";',
   perl => "CODE\n",
   tokens => {
     got  => ['Word=my', 'Symbol=@a', 'Operator==', 'Structure=(', 'Label=sub :', 'Word=lvalue', 'Structure={', 'Number=1', 'Structure=}', 'Structure=)', 'Structure=;', 'Word=print', 'Word=ref', 'Structure=(', 'Symbol=$a', 'Structure=[', 'Number=0', 'Structure=]', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=@a', 'Operator==', 'Structure=(', 'Word=sub', 'Operator=:', 'Attribute=lvalue', 'Structure={', 'Number=1', 'Structure=}', 'Structure=)', 'Structure=;', 'Word=print', 'Word=ref', 'Structure=(', 'Symbol=$a', 'Structure=[', 'Number=0', 'Structure=]', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/transpile-test-09.t', 't/op/sub_lval.t'],
   tasks => [268, 270] },
 { id => 8, kind => 'token', dump => 'tokens',
   title => 'A variable declaration\'s ATTRIBUTE is not a `Token::Attribute`',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'XOPERATOR', class => 'Operator', content => ':', pred => '', check => 'decl-attribute-as-operator' },
   fix => { kind => 'retag', handler => undef, kept => 'Pl::Parser2::_strip_decl_decorations (the declaration-attribute semantics: drop + announce; it now reads the Attribute tokens)', commit => '6a5de5c1', group => 'S', pr => undef },
   repro => 'my $x : shared = 1; print "$x\\n";',
   perl => "1\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$x', 'Operator=:', 'Word=shared', 'Operator==', 'Number=1', 'Structure=;', 'Word=print', 'Quote::Double="$x\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=$x', 'Operator=:', 'Attribute=shared', 'Operator==', 'Number=1', 'Structure=;', 'Word=print', 'Quote::Double="$x\\n"', 'Structure=;'],
   },
   unblocks => ['t/op/attrs.t', 't/uni/attrs.t'],
   tasks => [314, 322] },
 { id => 9, kind => 'token', dump => 'tokens',
   title => '`${ PUNCTUATION }` is a variable, but lexes as Cast + Block',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { expect => '*', class => 'Operator', content => '', pred => 'sole_in_deref_block', check => 'punct-name-in-braces' },
   fix => { kind => 'relex', handler => 'Pl::PExpr::braced_punct_magic_name' },
   repro => '"ab" =~ /(a)/; my @plus = @{+}; print "@plus\\n";',
   perl => "1 1\n",
   tokens => {
     got  => ['Quote::Double="ab"', 'Operator==~', 'Regexp::Match=/(a)/', 'Structure=;', 'Word=my', 'Symbol=@plus', 'Operator==', 'Cast=@', 'Structure={', 'Operator=+', 'Structure=}', 'Structure=;', 'Word=print', 'Quote::Double="@plus\\n"', 'Structure=;'],
     want => ['Quote::Double="ab"', 'Operator==~', 'Regexp::Match=/(a)/', 'Structure=;', 'Word=my', 'Symbol=@plus', 'Operator==', 'Magic=@+', 'Structure=;', 'Word=print', 'Quote::Double="@plus\\n"', 'Structure=;'],
   },
   unblocks => ['t/re/pat_rt_report.t'],
   tasks => [] },
 { id => 10, kind => 'structure', dump => 'statements',
   title => '`for` accepts only `[my] $scalar` as its loop variable — and mis-lexes the rest of the FILE',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'block-owner-split', shape => 'a `for` whose variable slot holds `my (LIST)` or `\\my %h`: the head and block leave the Compound' },
   fix => { kind => 'handler', handler => 'Pl::Parser2::_repair_nary_foreach' },
   repro => 'my @l = (1, 2, 3, 4); for my ($q, $r) (@l) { print "$q$r " } print "x\\n";',
   perl => "12 34 x\n",
   tokens => {
     got  => ['Statement::Variable: my @l = ( 1 , 2 , 3 , 4 ) ;', 'Statement::Compound: for my', 'Statement: ( $q , $r ) ( @l ) { print "$q$r " } print "x\\n" ;'],
     want => ['Statement::Variable: my @l = ( 1 , 2 , 3 , 4 ) ;', 'Statement::Compound: for my ( $q , $r ) ( @l ) { print "$q$r " }', 'Statement: print "x\\n" ;'],
   },
   unblocks => ['Pl/t/refaliasing-01.t', 't/op/const-optree.t', 't/op/for-many.t'],
   tasks => [327, 329] },
 { id => 11, kind => 'token', dump => 'tokens',
   title => '`/PATTERN/` after a paren-less WORD is tokenized as division',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { expect => 'XTERM', class => 'Operator', content => qr/^\/\/=?$/, pred => '', check => 'slash-as-division' },
   fix => { kind => 'relex', handler => 'Pl::Parser2::_repair_word_match' },
   repro => 'sub ok { print "ok $_[1]\\n" } $_ = "x"; ok /x/, "d";',
   perl => "ok d\n",
   tokens => {
     got  => ['Word=sub', 'Word=ok', 'Structure={', 'Word=print', 'Quote::Double="ok $_[1]\\n"', 'Structure=}', 'Magic=$_', 'Operator==', 'Quote::Double="x"', 'Structure=;', 'Word=ok', 'Operator=/', 'Word=x', 'Operator=/', 'Operator=,', 'Quote::Double="d"', 'Structure=;'],
     want => ['Word=sub', 'Word=ok', 'Structure={', 'Word=print', 'Quote::Double="ok $_[1]\\n"', 'Structure=}', 'Magic=$_', 'Operator==', 'Quote::Double="x"', 'Structure=;', 'Word=ok', 'Regexp::Match=/x/', 'Operator=,', 'Quote::Double="d"', 'Structure=;'],
   },
   unblocks => ['t/re/pat.t', 't/re/pat_advanced.t', 't/re/pat_re_eval.t'],
   tasks => [351, 872, 931, 940] },
 { id => 12, kind => 'token', dump => 'tokens',
   title => '`)*name` — a `*` after a term is lexed as a GLOB, not multiplication',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Symbol', content => qr/^\*/, pred => '', check => 'glob-for-multiply' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser2::_repair_glob_multiply (s527b: PPI fixed on the branch)', commit => '1cff9293', group => 'B', pr => undef },
   repro => 'my ($s, $k) = (0, "abc"); $s += length($k)*length($k); print "$s\\n";',
   perl => "9\n",
   tokens => {
     got  => ['Word=my', 'Structure=(', 'Symbol=$s', 'Operator=,', 'Symbol=$k', 'Structure=)', 'Operator==', 'Structure=(', 'Number=0', 'Operator=,', 'Quote::Double="abc"', 'Structure=)', 'Structure=;', 'Symbol=$s', 'Operator=+=', 'Word=length', 'Structure=(', 'Symbol=$k', 'Structure=)', 'Symbol=*length', 'Structure=(', 'Symbol=$k', 'Structure=)', 'Structure=;', 'Word=print', 'Quote::Double="$s\\n"', 'Structure=;'],
     want => ['Word=my', 'Structure=(', 'Symbol=$s', 'Operator=,', 'Symbol=$k', 'Structure=)', 'Operator==', 'Structure=(', 'Number=0', 'Operator=,', 'Quote::Double="abc"', 'Structure=)', 'Structure=;', 'Symbol=$s', 'Operator=+=', 'Word=length', 'Structure=(', 'Symbol=$k', 'Structure=)', 'Operator=*', 'Word=length', 'Structure=(', 'Symbol=$k', 'Structure=)', 'Structure=;', 'Word=print', 'Quote::Double="$s\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [138, 354] },
 { id => 13, kind => 'source', dump => 'slurp',
   title => 'Tokenization of a trailing `__END__`/`__DATA__` section depends on `$/`',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => undef, shape => 'source read with $/ undefined: a final __END__/__DATA__ line gains a newline' },
   fix => { kind => 'handler', handler => 'Pl::Parser::_trim_invented_tail' },
   repro => 'print "x\\n";
__END__
',
   perl => "x\n",
   tokens => {
     got  => ['serialize=print "x\\n";\\n__END__\\n\\n'],
     want => ['serialize=print "x\\n";\\n__END__\\n'],
   },
   unblocks => [],
   tasks => [] },
 { id => 14, kind => 'token', dump => 'tokens',
   title => '`<FH>` / `<glob>` after a list operator or a block is lexed as `<` … `>`',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { expect => 'TERM', class => 'Operator', content => qr/^<(?:<|=|<=|=>)?$/, pred => '', check => 'lt-as-comparison' },
   fix => { kind => 'relex', handler => 'Pl::Parser2::_repair_readline_cascade' },
   repro => 'print sort <DATA>;
__DATA__
b
a
',
   perl => "a\nb\n",
   tokens => {
     got  => ['Word=print', 'Word=sort', 'Operator=<', 'Word=DATA', 'Operator=>', 'Structure=;', 'Separator=__DATA__', 'Data=b\\na\\n'],
     want => ['Word=print', 'Word=sort', 'QuoteLike::Readline=<DATA>', 'Structure=;', 'Separator=__DATA__', 'Data=b\\na\\n'],
   },
   unblocks => ['Pl/t/readline-ternary-01.t'],
   tasks => [138, 472, 479, 563] },
 { id => 15, kind => 'token', dump => 'tokens',
   title => '`)` followed by `-1` — the operator is swallowed into a negative NUMBER',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Number', content => qr/^-/, pred => '', check => 'minus-in-number' },
   fix => { kind => 'relex', handler => 'Pl::PExpr::_fix_ppi_negative_number_bug', narrowed => 's527b: only the bareword-term and -N**2 arms are left; PPI splits after ) ] } symbols numbers quotes', commit => 'a737c524', group => 'A', pr => undef },
   repro => 'my $x = (1+2)-1; print "$x\\n";',
   perl => "2\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$x', 'Operator==', 'Structure=(', 'Number=1', 'Operator=+', 'Number=2', 'Structure=)', 'Number=-1', 'Structure=;', 'Word=print', 'Quote::Double="$x\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=$x', 'Operator==', 'Structure=(', 'Number=1', 'Operator=+', 'Number=2', 'Structure=)', 'Operator=-', 'Number=1', 'Structure=;', 'Word=print', 'Quote::Double="$x\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [] },
 { id => 16, kind => 'token', dump => 'tokens',
   title => 'perl 5.40\'s `^^` (logical XOR) is tokenized as two `^` operators',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'TERM', class => 'Operator', content => '^', pred => 'prev_adj_caret', check => 'logical-xor-split' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::PExpr::_fix_ppi_logical_xor_bug (s527a: PPI fixed on the branch)', commit => '21394cc3', group => 'C', pr => undef },
   repro => 'my ($p, $q) = (1, 0); my $r = $p ^^ $q; print "$r\\n";',
   perl => "1\n",
   tokens => {
     got  => ['Word=my', 'Structure=(', 'Symbol=$p', 'Operator=,', 'Symbol=$q', 'Structure=)', 'Operator==', 'Structure=(', 'Number=1', 'Operator=,', 'Number=0', 'Structure=)', 'Structure=;', 'Word=my', 'Symbol=$r', 'Operator==', 'Symbol=$p', 'Operator=^', 'Operator=^', 'Symbol=$q', 'Structure=;', 'Word=print', 'Quote::Double="$r\\n"', 'Structure=;'],
     want => ['Word=my', 'Structure=(', 'Symbol=$p', 'Operator=,', 'Symbol=$q', 'Structure=)', 'Operator==', 'Structure=(', 'Number=1', 'Operator=,', 'Number=0', 'Structure=)', 'Structure=;', 'Word=my', 'Symbol=$r', 'Operator==', 'Symbol=$p', 'Operator=^^', 'Symbol=$q', 'Structure=;', 'Word=print', 'Quote::Double="$r\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [] },
 { id => 17, kind => 'structure', dump => 'structure',
   title => 'A SUBSCRIPT after a deref or a KV slice is structured as something else',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'brace-class', shape => 'a `[` / `{` subscript after a deref block `${$r}` or a KV slice `%h{...}` built as Constructor/Block' },
   fix => { kind => 'handler', handler => 'Pl::PExpr::_parse_subscript_ix' },
   repro => 'my $r = [5]; my $v = ${$r}[0]; print "$v\\n";',
   perl => "5\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$r', 'Operator==', 'Structure=[ in Structure::Constructor < Statement::Variable', 'Number=5', 'Structure=] in Structure::Constructor < Statement::Variable', 'Structure=;', 'Word=my', 'Symbol=$v', 'Operator==', 'Cast=$', 'Structure={ in Structure::Block < Statement::Variable', 'Symbol=$r', 'Structure=} in Structure::Block < Statement::Variable', 'Structure=[ in Structure::Constructor < Statement::Variable', 'Number=0', 'Structure=] in Structure::Constructor < Statement::Variable', 'Structure=;', 'Word=print', 'Quote::Double="$v\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=$r', 'Operator==', 'Structure=[ in Structure::Constructor < Statement::Variable', 'Number=5', 'Structure=] in Structure::Constructor < Statement::Variable', 'Structure=;', 'Word=my', 'Symbol=$v', 'Operator==', 'Cast=$', 'Structure={ in Structure::Block < Statement::Variable', 'Symbol=$r', 'Structure=} in Structure::Block < Statement::Variable', 'Structure=[ in Structure::Subscript < Statement::Variable', 'Number=0', 'Structure=] in Structure::Subscript < Statement::Variable', 'Structure=;', 'Word=print', 'Quote::Double="$v\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [] },
 { id => 18, kind => 'structure', dump => 'statements',
   title => '`finally { … }` is not part of the `try` statement — and eats the next one',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { check => 'continuation-split', shape => '`finally BLOCK` after try/catch starts a new statement and swallows the next one' },
   fix => { kind => 'handler', handler => undef, deleted => 'Pl::Parser2::_repair_try_finally + the _lower_block finally join (s527c: PPI fixed on the branch)', commit => '946fb9e1', group => 'Q', pr => undef },
   repro => 'use feature \'try\'; no warnings;
try { die "e\\n" } catch ($e) { print "caught $e" } finally { print "fin\\n" }
print "after\\n";
',
   perl => "caught e\nfin\nafter\n",
   tokens => {
     got  => ['Statement::Include: use feature \'try\' ;', 'Statement::Include: no warnings ;', 'Statement::Compound: try { die "e\\n" } catch ( $e ) { print "caught $e" }', 'Statement: finally { print "fin\\n" } print "after\\n" ;'],
     want => ['Statement::Include: use feature \'try\' ;', 'Statement::Include: no warnings ;', 'Statement::Compound: try { die "e\\n" } catch ( $e ) { print "caught $e" } finally { print "fin\\n" }', 'Statement: print "after\\n" ;'],
   },
   unblocks => ['Pl/t/misc-fixes-02.t', 'Pl/t/try-catch-01.t', 't/op/try.t'],
   tasks => [] },
 { id => 19, kind => 'token', dump => 'tokens',
   title => 'A call to a sub named `x` after a list operator is lexed as the repetition operator',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { expect => 'TERM', class => 'Operator', content => qr/^x=?$/, pred => '', check => 'x-as-repetition' },
   fix => { kind => 'retag', handler => 'Pl::Parser2::_repair_word_x_call' },
   repro => 'sub x { "PKG" } print x(), "|\\n";',
   perl => "PKG|\n",
   tokens => {
     got  => ['Word=sub', 'Word=x', 'Structure={', 'Quote::Double="PKG"', 'Structure=}', 'Word=print', 'Operator=x', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double="|\\n"', 'Structure=;'],
     want => ['Word=sub', 'Word=x', 'Structure={', 'Quote::Double="PKG"', 'Structure=}', 'Word=print', 'Word=x', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double="|\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/bareword-call-01.t', 'Pl/t/method-name-word-01.t', 'Pl/t/misc-fixes-02.t', 't/op/lexsub.t'],
   tasks => [361] },
 { id => 20, kind => 'structure', dump => 'statements',
   title => 'Two of perl\'s three ways to enable `try` are not recognised, so the construct mis-lexes',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'statement-not-ended', shape => '`try` enabled by `use v5.40` / `use experimental "try"`: not recognised, the construct mis-lexes' },
   fix => { kind => 'handler', handler => 'Pl::Parser::_pcl_feature_include_cb' },
   repro => 'use v5.40;
try { die "e\\n" } catch ($e) { print "caught $e" }
print "after\\n";
',
   perl => "caught e\nafter\n",
   tokens => {
     got  => ['Statement::Include: use v5.40 ;', 'Statement: try { die "e\\n" } catch ( $e ) { print "caught $e" } print "after\\n" ;'],
     want => ['Statement::Include: use v5.40 ;', 'Statement::Compound: try { die "e\\n" } catch ( $e ) { print "caught $e" }', 'Statement: print "after\\n" ;'],
   },
   unblocks => ['Pl/t/feature-pragma-01.t'],
   tasks => [] },
 { id => 21, kind => 'token', dump => 'tokens',
   title => 'A term-initial `~~` is lexed as the smart-match operator',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'XTERM', class => 'Operator', content => '~~', pred => '', check => 'smartmatch-for-complements' },
   fix => { kind => 'relex', handler => 'Pl::Parser2::_repair_term_initial_complement', commit => '4affc7de', group => 'E', pr => undef },
   repro => 'my $y = 3; print ~~$y, "\\n";',
   perl => "3\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$y', 'Operator==', 'Number=3', 'Structure=;', 'Word=print', 'Operator=~~', 'Symbol=$y', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=$y', 'Operator==', 'Number=3', 'Structure=;', 'Word=print', 'Operator=~', 'Operator=~', 'Symbol=$y', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/misc-fixes-02.t', 'perl-tests/bop.t', 't/op/bop.t'],
   tasks => [339, 370, 371] },
 { id => 22, kind => 'token', dump => 'tokens',
   title => 'A filetest after a SCALAR filehandle is split into `-` + WORD',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => '*', class => 'Operator', content => '-', pred => 'adj_filetest_word', check => 'filetest-split' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::PExpr::_fuse_print_filehandle_filetest (s527b: PPI fixed on the branch)', commit => '5b822dea', group => 'N', pr => undef },
   repro => 'my $f = "/"; my $fh = \\*STDOUT; print $fh -e $f; print "\\n";',
   perl => "1\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$f', 'Operator==', 'Quote::Double="/"', 'Structure=;', 'Word=my', 'Symbol=$fh', 'Operator==', 'Cast=\\', 'Symbol=*STDOUT', 'Structure=;', 'Word=print', 'Symbol=$fh', 'Operator=-', 'Word=e', 'Symbol=$f', 'Structure=;', 'Word=print', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=$f', 'Operator==', 'Quote::Double="/"', 'Structure=;', 'Word=my', 'Symbol=$fh', 'Operator==', 'Cast=\\', 'Symbol=*STDOUT', 'Structure=;', 'Word=print', 'Symbol=$fh', 'Operator=-e', 'Symbol=$f', 'Structure=;', 'Word=print', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/filetest-stack-01.t'],
   tasks => [372] },
 { id => 23, kind => 'token', dump => 'tokens',
   title => 'A `$` scalar with a NON-ASCII name is split into Cast + Word — but `@`/`%`/`*`/`&` are not',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'XCAST', class => 'Word', content => '', pred => 'adj_nonascii', check => 'cast-then-word' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser::_merge_unicode_symbols (s527a: PPI fixed on the branch)', commit => 'c6b17b5e', group => 'G', pr => undef },
   repro => 'use utf8; binmode STDOUT, \':utf8\';
$Ｘ = 1; print "$Ｘ\\n";
',
   perl => "1\n",
   tokens => {
     got  => ['Word=use', 'Word=utf8', 'Structure=;', 'Word=binmode', 'Word=STDOUT', 'Operator=,', 'Quote::Single=\':utf8\'', 'Structure=;', 'Cast=$', 'Word=Ｘ', 'Operator==', 'Number=1', 'Structure=;', 'Word=print', 'Quote::Double="$Ｘ\\n"', 'Structure=;'],
     want => ['Word=use', 'Word=utf8', 'Structure=;', 'Word=binmode', 'Word=STDOUT', 'Operator=,', 'Quote::Single=\':utf8\'', 'Structure=;', 'Symbol=$Ｘ', 'Operator==', 'Number=1', 'Structure=;', 'Word=print', 'Quote::Double="$Ｘ\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/utf8-source-01.t', 't/mro/basic_utf8.t', 't/mro/package_aliases_utf8.t', 't/uni/caller.t', 't/uni/gv.t', 't/uni/method.t'],
   tasks => [410, 422] },
 { id => 24, kind => 'token', dump => 'tokens',
   title => 'A PUNCTUATION-named array `@?` is split into Cast + Operator',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'XCAST', class => 'Operator', content => '', pred => '', check => 'cast-then-punct' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser::_merge_punct_array_symbols (s527b: PPI fixed on the branch)', commit => 'ecd87059', also_narrowed => 'Pl::Parser2::_repair_punct_hash_name (inverse: a declared-term word before %X; and %^)', also_deleted => 'Pl::Parser2::_repair_punct_hash_cast, Pl::Parser::_reclass_subscripts_after, punct_container_chars', group => 'D', pr => undef },
   repro => '@? = (1, 2); print join(" ", @?), "\\n";',
   perl => "1 2\n",
   tokens => {
     got  => ['Cast=@', 'Operator=?', 'Operator==', 'Structure=(', 'Number=1', 'Operator=,', 'Number=2', 'Structure=)', 'Structure=;', 'Word=print', 'Word=join', 'Structure=(', 'Quote::Double=" "', 'Operator=,', 'Cast=@', 'Operator=?', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Magic=@?', 'Operator==', 'Structure=(', 'Number=1', 'Operator=,', 'Number=2', 'Structure=)', 'Structure=;', 'Word=print', 'Word=join', 'Structure=(', 'Quote::Double=" "', 'Operator=,', 'Magic=@?', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/punct-array-glob-01.t', 't/re/subst.t'],
   tasks => [415, 418, 449, 550, 653] },
 { id => 25, kind => 'token', dump => 'tokens',
   title => '`-name` after a token that ENDS A TERM is lexed as one negative-bareword Word',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Word', content => qr/^-/, pred => '', check => 'minus-in-word' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser2::_repair_minus_word (s527b: PPI fixed on the branch)', commit => 'a737c524', group => 'A', pr => undef },
   repro => 'my $z = length("abc")-length("a"); print "$z\\n";',
   perl => "2\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$z', 'Operator==', 'Word=length', 'Structure=(', 'Quote::Double="abc"', 'Structure=)', 'Word=-length', 'Structure=(', 'Quote::Double="a"', 'Structure=)', 'Structure=;', 'Word=print', 'Quote::Double="$z\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=$z', 'Operator==', 'Word=length', 'Structure=(', 'Quote::Double="abc"', 'Structure=)', 'Operator=-', 'Word=length', 'Structure=(', 'Quote::Double="a"', 'Structure=)', 'Structure=;', 'Word=print', 'Quote::Double="$z\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/minus-word-01.t'],
   tasks => [138, 457] },
 { id => 26, kind => 'token', dump => 'tokens',
   title => 'A glob whose NAME is punctuation or a digit run is split into two operators',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'TERM', class => 'Operator', content => qr/^\*\*?=?$/, pred => '', check => 'star-as-multiply' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser2::_repair_punct_glob_name + _glob_name_position (s527b: PPI fixed on the branch; the leaf lowers the Symbol through Pl::Environment::punct_glob_name)', commit => '6100d8b3', group => 'P', pr => undef },
   repro => 'our %X = (k => 1); *X = *-; print scalar(keys %X), "\\n";',
   perl => "0\n",
   tokens => {
     got  => ['Word=our', 'Symbol=%X', 'Operator==', 'Structure=(', 'Word=k', 'Operator==>', 'Number=1', 'Structure=)', 'Structure=;', 'Symbol=*X', 'Operator==', 'Operator=*', 'Operator=-', 'Structure=;', 'Word=print', 'Word=scalar', 'Structure=(', 'Word=keys', 'Symbol=%X', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=our', 'Symbol=%X', 'Operator==', 'Structure=(', 'Word=k', 'Operator==>', 'Number=1', 'Structure=)', 'Structure=;', 'Symbol=*X', 'Operator==', 'Symbol=*-', 'Structure=;', 'Word=print', 'Word=scalar', 'Structure=(', 'Word=keys', 'Symbol=%X', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/punct-glob-name-01.t', 'perl-tests/local.t', 't/op/method.t', 't/re/reg_namedcapture.t', 't/re/subst.t'],
   tasks => [138, 463, 562, 564] },
 { id => 27, kind => 'token', dump => 'symbol',
   title => '`->symbol` answers `%x` for the `$x` in `*$x{SLOT}` — the `*` cast is missing from its "cast trumps braces" set',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { check => undef, shape => 'Token::Symbol->symbol of the `$x` in `*$x{SLOT}` answers `%x` (the `*` cast is missing from its cast-trumps-braces set)' },
   fix => { kind => 'handler', handler => 'Pl::Parser::_brace_glob_slot_symbol', commit => 'dc242138', group => 'L', pr => undef, stays => 's527b: the expression lowering needs the braced spelling (Carp.pm *$_{HASH})' },
   repro => 'our $v = 9; our $a = "v"; my $x = *$a{SCALAR}; print $$x, "\\n";',
   perl => "9\n",
   tokens => {
     got  => ['Symbol=$v ->symbol=$v', 'Symbol=$a ->symbol=$a', 'Symbol=$x ->symbol=$x', 'Symbol=$a ->symbol=%a', 'Symbol=$x ->symbol=$x'],
     want => ['Symbol=$v ->symbol=$v', 'Symbol=$a ->symbol=$a', 'Symbol=$x ->symbol=$x', 'Symbol=$a ->symbol=$a', 'Symbol=$x ->symbol=$x'],
   },
   unblocks => ['Pl/t/glob-slot-operand-01.t', 'perl-tests/vec.t', 't/op/filetest.t', 't/op/gv.t', 't/op/stat.t', 't/uni/parser.t'],
   tasks => [663] },
 { id => 28, kind => 'token', dump => 'tokens',
   title => '`$x.2` — a `.` followed by a digit after a term is lexed as a Number::Float, so the concatenation OPERATOR disappears',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Number::Float', content => qr/^\.\d/, pred => '', check => 'dot-in-number' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser2::_repair_dot_number (s527b: PPI fixed on the branch)', commit => '8955ba30', group => 'F', pr => undef },
   repro => '$_ = 1; my $x = $_.2; print "$x\\n";',
   perl => "12\n",
   tokens => {
     got  => ['Magic=$_', 'Operator==', 'Number=1', 'Structure=;', 'Word=my', 'Symbol=$x', 'Operator==', 'Magic=$_', 'Number::Float=.2', 'Structure=;', 'Word=print', 'Quote::Double="$x\\n"', 'Structure=;'],
     want => ['Magic=$_', 'Operator==', 'Number=1', 'Structure=;', 'Word=my', 'Symbol=$x', 'Operator==', 'Magic=$_', 'Operator=.', 'Number=2', 'Structure=;', 'Word=print', 'Quote::Double="$x\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/minus-word-01.t'],
   tasks => [138, 480] },
 { id => 29, kind => 'lexer-dies', dump => 'lexer',
   title => 'A DEREFERENCE as the `foreach` loop variable fails the LEXER: "Illegal state in \'for\' compound statement"',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'lexer-dies', shape => 'the lexer dies: `for $$f (LIST)` -- a dereference as the foreach variable' },
   fix => { kind => 'handler', handler => undef },
   repro => 'our $z = 7; my $f = "z"; no strict "refs"; my @seen; for $$f (5, 11) { push @seen, $$f } print "@seen $z\\n";',
   perl => "5 11 7\n",
   tokens => {
     got  => ['LEXER-DIES: Lexer failed: Illegal state in \'for\' compound statement'],
     want => ['DOCUMENT'],
   },
   unblocks => ['t/op/decl-refs.t', 't/op/for.t'],
   tasks => [1580, 1648, 1649] },
 { id => 30, kind => 'token', dump => 'heredoc',
   title => 'An indented here-doc\'s INDENTATION is `^(\\s*)` of the whole terminator line, so a delimiter that begins with whitespace (or is empty) over-counts',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { check => undef, shape => '`<<~DELIM` whose delimiter begins with whitespace (or is empty): the indentation is over-counted' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser::_repair_indented_heredocs (s527b: PPI fixed on the branch)', commit => '3ed5f883', group => 'M', pr => undef },
   repro => 'print <<~\' EOF\';
  some data
   EOF
',
   perl => "some data\n",
   tokens => {
     got  => ['HereDoc=<<~\' EOF\'', 'body=  some data\\n'],
     want => ['HereDoc=<<~\' EOF\'', 'body=some data\\n'],
   },
   unblocks => ['Pl/t/heredoc-indent-01.t', 't/op/heredoc.t'],
   tasks => [435, 1501] },
 { id => 31, kind => 'token', dump => 'statements',
   title => '`sub _ { … }` — the NAME `_` after `sub` is lexed as `Token::Magic`, so no `Statement::Sub` is built and the FOLLOWING statement is swallowed',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'XSUBNAME', class => 'Magic', content => '_', pred => '', check => 'sub-name-as-magic' },
   fix => { kind => 'retag', handler => undef, commit => '8cefefce', group => 'H', pr => undef },
   repro => 'sub _ { "x" } print "y\\n";',
   perl => "y\n",
   tokens => {
     got  => ['Statement: sub _ { "x" } print "y\\n" ;'],
     want => ['Statement::Sub: sub _ { "x" }', 'Statement: print "y\\n" ;'],
   },
   unblocks => ['t/op/filetest.t'],
   tasks => [138, 1817] },
 { id => 32, kind => 'token', dump => 'tokens',
   title => '`$)` is tokenized as `Symbol:$` + `Structure:)` ANYWHERE once the `signatures` feature is on — and the stray `)` wrecks the enclosing structure',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => '*', class => 'Symbol', content => '$', pred => '', check => 'bare-sigil-symbol' },
   fix => { kind => 'relex', handler => undef, deleted => 'Pl::Parser::_signature_gid_offsets + _restore_signature_gid + _walk_token_offsets (s527b: PPI fixed on the branch)', commit => '61817c99', group => 'R', pr => undef },
   repro => 'use v5.36;
my $e = $);
print length($e) ? "gid\\n" : "none\\n";
',
   perl => "gid\n",
   tokens => {
     got  => ['Word=use', 'Number::Version=v5.36', 'Structure=;', 'Word=my', 'Symbol=$e', 'Operator==', 'Symbol=$', 'Structure=)', 'Structure=;', 'Word=print', 'Word=length', 'Structure=(', 'Symbol=$e', 'Structure=)', 'Operator=?', 'Quote::Double="gid\\n"', 'Operator=:', 'Quote::Double="none\\n"', 'Structure=;'],
     want => ['Word=use', 'Number::Version=v5.36', 'Structure=;', 'Word=my', 'Symbol=$e', 'Operator==', 'Magic=$)', 'Structure=;', 'Word=print', 'Word=length', 'Structure=(', 'Symbol=$e', 'Structure=)', 'Operator=?', 'Quote::Double="gid\\n"', 'Operator=:', 'Quote::Double="none\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/sig-param-shadow-01.t'],
   tasks => [] },
 { id => 33, kind => 'token', dump => 'tokens',
   title => 'A `*` right after a BLOCK\'s `}` is lexed as MULTIPLICATION when the block\'s last statement ends in a hash subscript',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'TERM', class => 'Operator', content => qr/^\*\*?=?$/, pred => 'next_is_open_curly', check => 'star-as-multiply' },
   fix => { kind => 'retag', handler => 'Pl::Parser::_desugar_loop_modifiers', commit => 'd8bcdfa1', group => 'J', pr => undef },
   repro => 'my %h; for (1) { $h{$_}; }
*{"main::g"} = sub { 7 };
print g(), "\\n";
',
   perl => "7\n",
   tokens => {
     got  => ['Word=my', 'Symbol=%h', 'Structure=;', 'Word=for', 'Structure=(', 'Number=1', 'Structure=)', 'Structure={', 'Symbol=$h', 'Structure={', 'Magic=$_', 'Structure=}', 'Structure=;', 'Structure=}', 'Operator=*', 'Structure={', 'Quote::Double="main::g"', 'Structure=}', 'Operator==', 'Word=sub', 'Structure={', 'Number=7', 'Structure=}', 'Structure=;', 'Word=print', 'Word=g', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=my', 'Symbol=%h', 'Structure=;', 'Word=for', 'Structure=(', 'Number=1', 'Structure=)', 'Structure={', 'Symbol=$h', 'Structure={', 'Magic=$_', 'Structure=}', 'Structure=;', 'Structure=}', 'Cast=*', 'Structure={', 'Quote::Double="main::g"', 'Structure=}', 'Operator==', 'Word=sub', 'Structure={', 'Number=7', 'Structure=}', 'Structure=;', 'Word=print', 'Word=g', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [2098] },
 { id => 34, kind => 'token', dump => 'tokens',
   title => 'After a FLOAT / EXP / HEX / OCTAL / BINARY number, `-N` is lexed as a NEGATIVE NUMBER — the subtraction operator disappears',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Number::Float', content => qr/^-/, pred => 'prev_nonint_number', check => 'minus-in-number' },
   fix => { kind => 'relex', handler => 'Pl::PExpr::_fix_ppi_negative_number_bug', narrowed => 's527b: only the bareword-term and -N**2 arms are left; PPI splits after ) ] } symbols numbers quotes', commit => 'a737c524', group => 'A', pr => undef },
   repro => 'print 1.5-0.5, " ", 1e3-1, " ", 0x10-1, "\\n";',
   perl => "1 999 15\n",
   tokens => {
     got  => ['Word=print', 'Number::Float=1.5', 'Number::Float=-0.5', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Number::Exp=1e3', 'Number=-1', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Number::Hex=0x10', 'Number=-1', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=print', 'Number::Float=1.5', 'Operator=-', 'Number::Float=0.5', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Number::Exp=1e3', 'Operator=-', 'Number=1', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Number::Hex=0x10', 'Operator=-', 'Number=1', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/minus-word-01.t', 'Pl/t/misc-fixes-02.t'],
   tasks => [2530] },
 { id => 35, kind => 'token', dump => 'tokens',
   title => 'A filetest right after a `BEGIN { … }` block on the same line is split into `-` + WORD',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => '*', class => 'Operator', content => '-', pred => 'adj_filetest_after_phaser', check => 'filetest-split' },
   fix => { kind => 'relex', handler => undef, kept => 'PCLSwitches::prefix_first_code_line (the `;` that closes a switch prefix ending in a block is an ordinary statement separator, not a token repair; it stays)', commit => '5b822dea', group => 'N', pr => undef },
   repro => 'BEGIN { 1 } -e _; print "x\\n";',
   perl => "x\n",
   tokens => {
     got  => ['Word=BEGIN', 'Structure={', 'Number=1', 'Structure=}', 'Operator=-', 'Word=e', 'Magic=_', 'Structure=;', 'Word=print', 'Quote::Double="x\\n"', 'Structure=;'],
     want => ['Word=BEGIN', 'Structure={', 'Number=1', 'Structure=}', 'Operator=-e', 'Magic=_', 'Structure=;', 'Word=print', 'Quote::Double="x\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [2670] },
 { id => 36, kind => 'structure', dump => 'structure',
   title => 'The `[i]` of a braced element `${name}[i]` is lexed as an anonymous-array CONSTRUCTOR',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'brace-class', shape => 'the `[i]` after a braced name `${name}` built as Constructor' },
   fix => { kind => 'handler', handler => 'Pl::Parser2::_brace_name_refs' },
   repro => '@a = (1, 2, 3); print ${a}[1], "\\n";',
   perl => "2\n",
   tokens => {
     got  => ['Symbol=@a', 'Operator==', 'Structure=( in Structure::List < Statement', 'Number=1', 'Operator=,', 'Number=2', 'Operator=,', 'Number=3', 'Structure=) in Structure::List < Statement', 'Structure=;', 'Word=print', 'Cast=$', 'Structure={ in Structure::Block < Statement', 'Word=a', 'Structure=} in Structure::Block < Statement', 'Structure=[ in Structure::Constructor < Statement', 'Number=1', 'Structure=] in Structure::Constructor < Statement', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Symbol=@a', 'Operator==', 'Structure=( in Structure::List < Statement', 'Number=1', 'Operator=,', 'Number=2', 'Operator=,', 'Number=3', 'Structure=) in Structure::List < Statement', 'Structure=;', 'Word=print', 'Cast=$', 'Structure={ in Structure::Block < Statement', 'Word=a', 'Structure=} in Structure::Block < Statement', 'Structure=[ in Structure::Subscript < Statement', 'Number=1', 'Structure=] in Structure::Subscript < Statement', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [2645] },
 { id => 37, kind => 'structure', dump => 'structure',
   title => '`print({$fh} LIST)` -- the parenthesised block filehandle -- is split into two statements, or its block is a CONSTRUCTOR',
   ppi => { confirmed => '1.291', fixed_in => undef },
   when => { check => 'filehandle-block-split', shape => '`print({$fh} LIST)`: the block filehandle becomes its own Statement::Compound, or a Constructor' },
   fix => { kind => 'handler', handler => 'Pl::Parser::_repair_paren_block_filehandle' },
   repro => 'my $fh = \\*STDOUT; print({$fh} "a", "b"); printf({$fh} "%s\\n", "c");',
   perl => "abc\n",
   tokens => {
     got  => ['Word=my', 'Symbol=$fh', 'Operator==', 'Cast=\\', 'Symbol=*STDOUT', 'Structure=;', 'Word=print', 'Structure=( in Structure::List < Statement', 'Structure={ in Structure::Block < Statement::Compound', 'Symbol=$fh', 'Structure=} in Structure::Block < Statement::Compound', 'Quote::Double="a"', 'Operator=,', 'Quote::Double="b"', 'Structure=) in Structure::List < Statement', 'Structure=;', 'Word=printf', 'Structure=( in Structure::List < Statement', 'Structure={ in Structure::Constructor < Statement', 'Symbol=$fh', 'Structure=} in Structure::Constructor < Statement', 'Quote::Double="%s\\n"', 'Operator=,', 'Quote::Double="c"', 'Structure=) in Structure::List < Statement', 'Structure=;'],
     want => ['Word=my', 'Symbol=$fh', 'Operator==', 'Cast=\\', 'Symbol=*STDOUT', 'Structure=;', 'Word=print', 'Structure=( in Structure::List < Statement', 'Structure={ in Structure::Block < Statement', 'Symbol=$fh', 'Structure=} in Structure::Block < Statement', 'Quote::Double="a"', 'Operator=,', 'Quote::Double="b"', 'Structure=) in Structure::List < Statement', 'Structure=;', 'Word=printf', 'Structure=( in Structure::List < Statement', 'Structure={ in Structure::Block < Statement', 'Symbol=$fh', 'Structure=} in Structure::Block < Statement', 'Quote::Double="%s\\n"', 'Operator=,', 'Quote::Double="c"', 'Structure=) in Structure::List < Statement', 'Structure=;'],
   },
   unblocks => ['Pl/t/misc-fixes-02.t', 'Pl/t/print-block-fh-01.t'],
   tasks => [3244] },
 { id => 38, kind => 'token', dump => 'tokens',
   title => 'A statement that begins with `*$name` after a bare block is lexed with a multiplication OPERATOR',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'TERM', class => 'Operator', content => qr/^\*\*?=?$/, pred => 'next_is_scalar', check => 'star-as-multiply' },
   fix => { kind => 'retag', handler => undef, deleted => 'Pl::Parser::_repair_statement_start_glob_cast (s527b: PPI fixed on the branch)', commit => '0a536af5', group => 'K', pr => undef },
   repro => 'use Symbol; my $o = Symbol::gensym(); my ($c, $b) = (0, 1);
{ $c = $b; }
*$o->{A} = 7; print *$o->{A}, "\\n";
',
   perl => "7\n",
   tokens => {
     got  => ['Word=use', 'Word=Symbol', 'Structure=;', 'Word=my', 'Symbol=$o', 'Operator==', 'Word=Symbol::gensym', 'Structure=(', 'Structure=)', 'Structure=;', 'Word=my', 'Structure=(', 'Symbol=$c', 'Operator=,', 'Symbol=$b', 'Structure=)', 'Operator==', 'Structure=(', 'Number=0', 'Operator=,', 'Number=1', 'Structure=)', 'Structure=;', 'Structure={', 'Symbol=$c', 'Operator==', 'Symbol=$b', 'Structure=;', 'Structure=}', 'Operator=*', 'Symbol=$o', 'Operator=->', 'Structure={', 'Word=A', 'Structure=}', 'Operator==', 'Number=7', 'Structure=;', 'Word=print', 'Cast=*', 'Symbol=$o', 'Operator=->', 'Structure={', 'Word=A', 'Structure=}', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
     want => ['Word=use', 'Word=Symbol', 'Structure=;', 'Word=my', 'Symbol=$o', 'Operator==', 'Word=Symbol::gensym', 'Structure=(', 'Structure=)', 'Structure=;', 'Word=my', 'Structure=(', 'Symbol=$c', 'Operator=,', 'Symbol=$b', 'Structure=)', 'Operator==', 'Structure=(', 'Number=0', 'Operator=,', 'Number=1', 'Structure=)', 'Structure=;', 'Structure={', 'Symbol=$c', 'Operator==', 'Symbol=$b', 'Structure=;', 'Structure=}', 'Cast=*', 'Symbol=$o', 'Operator=->', 'Structure={', 'Word=A', 'Structure=}', 'Operator==', 'Number=7', 'Structure=;', 'Word=print', 'Cast=*', 'Symbol=$o', 'Operator=->', 'Structure={', 'Word=A', 'Structure=}', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;'],
   },
   unblocks => ['Pl/t/misc-fixes-02.t', 'Pl/t/regex-pos-01.t', 'Pl/t/zlib-01.t'],
   tasks => [1511, 3342] },
 { id => 55, kind => 'token', dump => 'tokens',
   title => 'A method name after `->`, a postfix `++`/`--` and the `}` of a subscript or a dereference block END A TERM, and PPI lexes what follows as a term (`$o->w*w()` a glob, `$i++ / 2` a match, `$h{a}%-1` the magic hash)',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { expect => 'OP', class => 'Symbol', content => qr/^\*/, pred => '', check => 'glob-for-multiply' },
   fix => { kind => 'relex', handler => 'Pl::Parser2::_ends_term', commit => 'd8bcdfa1', group => 'J', pr => undef },
   repro => 'package O; sub w { 10 } package main; my $o = bless {}, "O"; sub w { 2 } my %h = (a => 5); my $i = 5; print $o->w*w(), " ", $i++*w(), " ", $h{a}%-1, "\\n";',
   perl => "20 10 0\n",
   tokens => {
     got  => ['Word=package', 'Word=O', 'Structure=;', 'Word=sub', 'Word=w', 'Structure={', 'Number=10', 'Structure=}', 'Word=package', 'Word=main', 'Structure=;', 'Word=my', 'Symbol=$o', 'Operator==', 'Word=bless', 'Structure={', 'Structure=}', 'Operator=,', 'Quote::Double="O"', 'Structure=;', 'Word=sub', 'Word=w', 'Structure={', 'Number=2', 'Structure=}', 'Word=my', 'Symbol=%h', 'Operator==', 'Structure=(', 'Word=a', 'Operator==>', 'Number=5', 'Structure=)', 'Structure=;', 'Word=my', 'Symbol=$i', 'Operator==', 'Number=5', 'Structure=;', 'Word=print', 'Symbol=$o', 'Operator=->', 'Word=w', 'Symbol=*w', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Symbol=$i', 'Operator=++', 'Symbol=*w', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Symbol=$h', 'Structure={', 'Word=a', 'Structure=}', 'Magic=%-', 'Number=1', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;']
,
     want => ['Word=package', 'Word=O', 'Structure=;', 'Word=sub', 'Word=w', 'Structure={', 'Number=10', 'Structure=}', 'Word=package', 'Word=main', 'Structure=;', 'Word=my', 'Symbol=$o', 'Operator==', 'Word=bless', 'Structure={', 'Structure=}', 'Operator=,', 'Quote::Double="O"', 'Structure=;', 'Word=sub', 'Word=w', 'Structure={', 'Number=2', 'Structure=}', 'Word=my', 'Symbol=%h', 'Operator==', 'Structure=(', 'Word=a', 'Operator==>', 'Number=5', 'Structure=)', 'Structure=;', 'Word=my', 'Symbol=$i', 'Operator==', 'Number=5', 'Structure=;', 'Word=print', 'Symbol=$o', 'Operator=->', 'Word=w', 'Operator=*', 'Word=w', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Symbol=$i', 'Operator=++', 'Operator=*', 'Word=w', 'Structure=(', 'Structure=)', 'Operator=,', 'Quote::Double=" "', 'Operator=,', 'Symbol=$h', 'Structure={', 'Word=a', 'Structure=}', 'Operator=%', 'Number=-1', 'Operator=,', 'Quote::Double="\\n"', 'Structure=;']
,
   },
   unblocks => [],
   tasks => [3940] },
 { id => 56, kind => 'token', dump => 'tokens',
   title => 'A `/` after the `}` of a block that opens a statement or follows a word (`BEGIN { } /x/`, `{ 1 } /x/`, `grep { 1 } /(x)/`) is lexed as a DIVISION, where perl expects a term and reads a match',
   ppi => { confirmed => '1.291', fixed_in => 'pcl' },
   when => { check => 'slash-as-division', shape => 'a `/` right after a statement block\'s `}`' },
   fix => { kind => 'relex', handler => undef, commit => 'c1058d05', group => 'O', pr => undef },
   repro => '$_ = "x"; { 1 } /x/ and print "match\\n";',
   perl => "match\n",
   tokens => {
     got  => ['Magic=$_', 'Operator==', 'Quote::Double="x"', 'Structure=;', 'Structure={', 'Number=1', 'Structure=}', 'Operator=/', 'Word=x', 'Operator=/', 'Operator=and', 'Word=print', 'Quote::Double="match\\n"', 'Structure=;'],
     want => ['Magic=$_', 'Operator==', 'Quote::Double="x"', 'Structure=;', 'Structure={', 'Number=1', 'Structure=}', 'Regexp::Match=/x/', 'Operator=and', 'Word=print', 'Quote::Double="match\\n"', 'Structure=;'],
   },
   unblocks => [],
   tasks => [3942] },
);

# The one accessor.
sub bugs { @BUGS }

1;
