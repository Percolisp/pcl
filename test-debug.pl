#!/usr/bin/env perl
use lib ".";
use Pl::PExpr;
use Pl::Environment;
use PPI;
use Data::Dump qw/dump/;

# Test 1: shift @$e - fails
print "=== Test 1: shift \@\$e ===\n";
test_parse('shift @$e');

# Test 2: @$e - works
print "\n=== Test 2: \@\$e ===\n";
test_parse('@$e');

# Test 3: shift @arr - should work
print "\n=== Test 3: shift \@arr ===\n";
test_parse('shift @arr');

sub test_parse {
    my $code = shift;
    my $doc = PPI::Document->new(\$code);
    my @stmts = $doc->schildren;
    my $env = Pl::Environment->new();
    my @parts;
    for my $stmt (@stmts) {
        @parts = grep {
            ref($_) !~ /Whitespace|Comment/ && ref($_) ne "PPI::Token::Structure"
        } $stmt->children;
    }
    print "Parts: ", dump(\@parts), "\n";

    my $pexpr = Pl::PExpr->new(e => \@parts, environment => $env);
    my $id = eval { $pexpr->parse_expr_to_tree(\@parts) };
    if ($@) {
        my $err = $@;
        $err =~ s/ at .*//s;
        print "ERROR: $err\n";
    } else {
        print "Success, node ID: $id\n";
    }
}
