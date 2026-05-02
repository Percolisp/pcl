#!/usr/bin/env perl
# block-analyzer-01.t: Unit tests for Pl::BlockAnalyzer

use v5.30;
use strict;
use warnings;
use Test::More;

# Add project root (CWD when tests are run) so Pl::BlockAnalyzer can be found.
use lib '.';

use PPI;
require Pl::BlockAnalyzer;

# ── Helper: parse a sub body block from Perl source ──────────────────────────
sub parse_block {
    my ($perl_src) = @_;
    my $doc = PPI::Document->new(\$perl_src);
    die "PPI parse failed for: $perl_src" unless $doc;
    my $sub_stmt = $doc->find_first('PPI::Statement::Sub');
    die "No sub found in: $perl_src" unless $sub_stmt;
    my $block = $sub_stmt->find_first('PPI::Structure::Block');
    die "No block found in: $perl_src" unless $block;
    # Keep $doc alive so PPI objects don't get freed.
    return ($block, $doc);
}

# ── Test 1: empty sub body ────────────────────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is ref($a), 'HASH',                    'analyze returns hashref';
    is ref($a->{declarations}), 'ARRAY',   'declarations is arrayref';
    is scalar(@{$a->{declarations}}), 0,   'no declarations in empty body';
    is ref($a->{vars}), 'HASH',            'vars is hashref';
}

# ── Test 2: single my declaration ────────────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { my $x = 1; }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is scalar(@{$a->{declarations}}), 1,    'one declaration';
    my $d = $a->{declarations}[0];
    is $d->{decl_type}, 'my',              'decl_type is my';
    is_deeply $d->{vars}, ['$x'],          'vars contains $x';
    is $d->{stmt_idx}, 0,                  'stmt_idx is 0';

    ok exists $a->{vars}{'$x'},            'vars{$x} exists';
    is $a->{vars}{'$x'}{scope}, 'local',   'scope is local';
    is $a->{vars}{'$x'}{decl_type}, 'my', 'decl_type in vars';
    is $a->{vars}{'$x'}{sigil}, '$',       'sigil is $';
    is $a->{vars}{'$x'}{captured}, 0,      'not captured';
}

# ── Test 3: multiple my declarations at different positions ──────────────────
{
    my ($block, $doc) = parse_block('sub foo { print $x; my $x = 1; my $y = 2; }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is scalar(@{$a->{declarations}}), 2,   'two declarations';
    is $a->{declarations}[0]{stmt_idx}, 1, 'first my at stmt 1 (after print)';
    is $a->{declarations}[1]{stmt_idx}, 2, 'second my at stmt 2';
    is $a->{declarations}[0]{vars}[0], '$x', 'first is $x';
    is $a->{declarations}[1]{vars}[0], '$y', 'second is $y';
}

# ── Test 4: list declaration ──────────────────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { my ($a, $b) = (1, 2); }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is scalar(@{$a->{declarations}}), 1,   'one declaration node';
    my $d = $a->{declarations}[0];
    is scalar(@{$d->{vars}}), 2,           'two vars in list decl';
    ok +(grep { $_ eq '$a' } @{$d->{vars}}), '$a in decl';
    ok +(grep { $_ eq '$b' } @{$d->{vars}}), '$b in decl';
}

# ── Test 5: array and hash declarations ───────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { my @arr = (); my %h = (); }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is scalar(@{$a->{declarations}}), 2, 'two declarations';
    is $a->{declarations}[0]{vars}[0], '@arr', '@arr declared';
    is $a->{declarations}[1]{vars}[0], '%h',   '%h declared';
    is $a->{vars}{'@arr'}{sigil}, '@', 'sigil @';
    is $a->{vars}{'%h'}{sigil}, '%',   'sigil %';
}

# ── Test 6: closure capture detection ────────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { my $x = 1; my $f = sub { $x }; }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is $a->{vars}{'$x'}{captured}, 1, '$x is captured by inner anon sub';
    is $a->{vars}{'$f'}{captured}, 0, '$f is not captured';
}

# ── Test 7: named sub inside body — does NOT trigger capture ─────────────────
{
    my ($block, $doc) = parse_block('sub foo { my $x = 1; sub bar { $x } }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    # Named inner sub bar is NOT a closure — its $x is a package global.
    # The capture detection skips named subs.
    is $a->{vars}{'$x'}{captured}, 0, 'named inner sub does not capture $x';
}

# ── Test 8: inner my decl inside while — hoisted to compound stmt ────────────
{
    my ($block, $doc) = parse_block('sub foo { while (1) { my $i = 0; } }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    # $i is declared inside the while body.  BlockAnalyzer maps it to the
    # while compound statement (stmt_idx 0 — the only statement in foo's body).
    is scalar(@{$a->{declarations}}), 1,    'one declaration entry';
    is $a->{declarations}[0]{stmt_idx}, 0,  'mapped to while statement (idx 0)';
    is $a->{declarations}[0]{vars}[0], '$i', 'var is $i';
    is $a->{vars}{'$i'}{scope}, 'local',    'scope is local';
}

# ── Test 9: our declaration ───────────────────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { our $x = 1; }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is scalar(@{$a->{declarations}}), 1, 'our counts as a declaration';
    is $a->{declarations}[0]{decl_type}, 'our', 'decl_type is our';
}

# ── Test 10: outer scope tracking ────────────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { my $x = 1; }');
    my $outer = { '$outer_var' => { type => 'my', cl_name => '$outer_var' } };
    my $a = Pl::BlockAnalyzer->analyze($block, $outer);

    ok exists $a->{vars}{'$x'},            'local var in vars';
    is ref($a->{outer_refs}), 'HASH',      'outer_refs is hashref';
}

# ── Test 11: state declaration ────────────────────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { state $count = 0; }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    is scalar(@{$a->{declarations}}), 1,    'state counts';
    is $a->{declarations}[0]{decl_type}, 'state', 'decl_type is state';
}

# ── Test 12: ppi_stmt field is the PPI object ─────────────────────────────────
{
    my ($block, $doc) = parse_block('sub foo { my $x = 1; print $x; }');
    my $a = Pl::BlockAnalyzer->analyze($block);

    my $d = $a->{declarations}[0];
    ok ref($d->{ppi_stmt}),                'ppi_stmt is a reference';
    ok $d->{ppi_stmt}->isa('PPI::Statement::Variable'),
                                           'ppi_stmt is PPI::Statement::Variable';
}

done_testing;
