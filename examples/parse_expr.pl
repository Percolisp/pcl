#!/usr/bin/env perl
# parse_expr.pl - Parse a Perl expression and dump its AST
#
# Usage: perl parse_expr.pl 'expression'
# Example: perl parse_expr.pl '$x + $y * 2'
#
# Part of PCL (Perl to Common Lisp) - demonstrates Pl::PExpr usage

use v5.30;
use strict;
use warnings;

use FindBin;
use lib "$FindBin::Bin/..";

use PPI;
use Pl::PExpr;

# Get expression from command line or use default
my $expr = shift // '$a + $b * $c';

# Parse with PPI
my $doc = PPI::Document->new(\$expr);
die "PPI parse failed for: $expr\n" unless $doc;

my @doc_children = $doc->children;
my $stmt = $doc_children[0];
unless ($stmt) {
    die "No statement found in: $expr\n";
}

my @stmt_children = $stmt->children;
my @tokens = grep { !$_->isa('PPI::Token::Whitespace') } @stmt_children;

# Parse expression to AST
my $parser = Pl::PExpr->new(
    e        => \@tokens,
    full_PPI => $doc,
);

my ($root_id, $declarations) = $parser->parse_expr_to_tree();

# Show declarations
if (@$declarations) {
    say "Declarations:";
    for my $decl (@$declarations) {
        say "  $decl->{type} $decl->{var}";
    }
    say "";
}

# Dump the tree
say "AST for: $expr";
say "-" x 40;
dump_tree($parser, $root_id);

sub dump_tree {
    my ($parser, $node_id, $indent) = @_;
    $indent //= 0;
    my $prefix = "  " x $indent;

    my $tree = $parser->node_tree;
    my $node = $tree->node_data($node_id);
    my $children = $tree->children_ids($node_id);

    my $desc;
    my $num_children = @$children;
    if (ref($node) && ref($node) =~ /^(?:HASH|PPIreference)$/ && $node->{type}) {
        $desc = $node->{type};
        $desc .= "($node->{op})" if $node->{op};
        $desc .= "($node->{func_name})" if $node->{func_name};
        $desc .= "($node->{const_name})" if $node->{const_name};
    } elsif (ref($node) && $node->isa('PPI::Token::Operator') && $num_children == 2) {
        # Binary operator with 2 children
        $desc = "binop(" . $node->content . ")";
    } elsif (ref($node) && $node->can('content')) {
        my $content = $node->content;
        my $type = ref($node) =~ s/^PPI:://r;
        $desc = "$type: $content";
    } else {
        $desc = ref($node) || $node // 'undef';
    }

    say "${prefix}[$node_id] $desc";

    for my $child_id (@$children) {
        dump_tree($parser, $child_id, $indent + 1);
    }
}

__END__

=head1 NAME

parse_expr.pl - Parse Perl expressions and display AST

=head1 SYNOPSIS

    perl parse_expr.pl '$x + $y * 2'
    perl parse_expr.pl 'my $x = $hash{key}'
    perl parse_expr.pl '$obj->method($a, $b)'

=head1 DESCRIPTION

Demonstrates usage of Pl::PExpr to parse Perl expressions into an
Abstract Syntax Tree (AST). Useful for understanding how PCL parses
Perl code.

Note: Without context from full Perl code, barewords are treated as
function calls. So 'PI+2' parses as 'PI(+2)' because the parser doesn't
know PI is a constant. Use the full transpiler for proper constant handling.

XXXXX Fix: Should (optionally) also print the parsed list/scalar contexts.



=head1 EXAMPLES

    $ perl parse_expr.pl '$a + $b * $c'
    AST for: $a + $b * $c
    ----------------------------------------
    [4] prefix_op(+)
      [0] Token::Symbol: $a
      [3] prefix_op(*)
        [1] Token::Symbol: $b
        [2] Token::Symbol: $c

    $ perl parse_expr.pl 'my $x = 10'
    Declarations:
      my $x

    AST for: my $x = 10
    ----------------------------------------
    [2] prefix_op(=)
      [0] Token::Symbol: $x
      [1] Token::Number: 10

=cut
