#          -*-Mode: CPerl -*-

# Test array/hash deref as function arguments and string interpolation
# with array/hash indexes

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 32;
BEGIN { use_ok('Pl::PExpr') };
BEGIN { use_ok('Pl::ExprToCL') };
BEGIN { use_ok('Pl::Environment') };
BEGIN { use_ok('Pl::Parser') };

# ----------------------------------------------------------------------
# Helper functions

sub parse_and_gen {
  my $code    = shift;

  my $doc     = PPI::Document->new(\$code);
  my @stmts   = $doc->children();
  my @parts   = $stmts[0]->children();

  my $env     = Pl::Environment->new();
  my $expr_o  = Pl::PExpr->new(e => \@parts, full_PPI => $doc);
  my $node_id = $expr_o->parse_expr_to_tree(\@parts);

  my $gen     = Pl::ExprToCL->new(expr_o => $expr_o, environment => $env);
  my $cl      = $gen->generate($node_id);

  return $cl;
}

# Statement-level parser for testing constructs like my/print with heredocs
sub parse_stmt_and_gen {
  my $code    = shift;

  my $parser  = Pl::Parser->new(code => $code);
  my $cl      = $parser->parse();

  return $cl;
}

# ----------------------------------------------------------------------
# Test 1: Array deref as argument to shift/pop/etc (Cast token fix)

{
  my $cl = parse_and_gen('shift @$lines');
  like($cl, qr/pl-shift.*pl-cast-@.*\$lines/,
       'shift @$lines: generates pl-shift with pl-cast-@');
}

{
  my $cl = parse_and_gen('pop @$arr');
  like($cl, qr/pl-pop.*pl-cast-@.*\$arr/,
       'pop @$arr: generates pl-pop with pl-cast-@');
}

{
  my $cl = parse_and_gen('defined @$ref');
  like($cl, qr/pl-defined.*pl-cast-@.*\$ref/,
       'defined @$ref: generates pl-defined with pl-cast-@');
}

{
  my $cl = parse_and_gen('scalar @$items');
  like($cl, qr/pl-scalar.*pl-cast-@.*\$items/,
       'scalar @$items: generates pl-scalar with pl-cast-@');
}

{
  my $cl = parse_and_gen('push @$stack, $item');
  like($cl, qr/pl-push.*pl-cast-@.*\$stack.*\$item/,
       'push @$stack, $item: generates pl-push with pl-cast-@');
}

# Hash deref
{
  my $cl = parse_and_gen('keys %$hash');
  like($cl, qr/pl-keys.*pl-cast-%.*\$hash/,
       'keys %$hash: generates pl-keys with pl-cast-%');
}

{
  my $cl = parse_and_gen('values %$ref');
  like($cl, qr/pl-values.*pl-cast-%.*\$ref/,
       'values %$ref: generates pl-values with pl-cast-%');
}

{
  my $cl = parse_and_gen('each %$data');
  like($cl, qr/pl-each.*pl-cast-%.*\$data/,
       'each %$data: generates pl-each with pl-cast-%');
}

# ----------------------------------------------------------------------
# Test 2: String interpolation with array index

{
  my $cl = parse_and_gen('"value is $arr[0]"');
  like($cl, qr/pl-string_concat.*"value is ".*pl-aref.*\@arr.*0/,
       'String interpolation with $arr[0]');
}

{
  my $cl = parse_and_gen('"first: $list[0], second: $list[1]"');
  like($cl, qr/pl-aref.*\@list.*0.*pl-aref.*\@list.*1/,
       'String interpolation with multiple array accesses');
}

{
  my $cl = parse_and_gen('"negative index: $arr[-1]"');
  like($cl, qr/pl-aref.*\@arr.*-1/,
       'String interpolation with negative array index');
}

{
  my $cl = parse_and_gen('"variable index: $arr[$i]"');
  like($cl, qr/pl-aref.*\@arr.*\$i/,
       'String interpolation with variable array index');
}

# ----------------------------------------------------------------------
# Test 3: String interpolation with hash key

{
  my $cl = parse_and_gen('"value is $hash{key}"');
  like($cl, qr/pl-string_concat.*"value is ".*pl-gethash.*%hash.*"key"/,
       'String interpolation with $hash{key}');
}

{
  my $cl = parse_and_gen('"name: $data{name}, age: $data{age}"');
  like($cl, qr/pl-gethash.*%data.*"name".*pl-gethash.*%data.*"age"/,
       'String interpolation with multiple hash accesses');
}

{
  my $cl = parse_and_gen('"dynamic key: $hash{$key}"');
  like($cl, qr/pl-gethash.*%hash.*\$key/,
       'String interpolation with variable hash key');
}

# ----------------------------------------------------------------------
# Test 4: Complex string interpolation

{
  my $cl = parse_and_gen('"line $. of $file: $lines[$i]"');
  like($cl, qr/pl-string_concat/,
       'Complex interpolation with multiple vars');
  like($cl, qr/\|\$\.\|/,
       'Complex interpolation: $. magic variable');
  like($cl, qr/pl-aref.*\@lines.*\$i/,
       'Complex interpolation: array access');
}

{
  my $cl = parse_and_gen('"error at $file:$line"');
  like($cl, qr/pl-string_concat.*\$file.*\$line/,
       'Simple variable interpolation');
}

# ----------------------------------------------------------------------
# Test 5: Edge cases

# Empty string
{
  my $cl = parse_and_gen('""');
  like($cl, qr/^""$/, 'Empty string');
}

# String with only variable
{
  my $cl = parse_and_gen('"$x"');
  like($cl, qr/\$x/, 'String with only variable');
}

# Escaped characters in string with interpolation
{
  my $cl = parse_and_gen('"value: $x\\n"');
  like($cl, qr/pl-string_concat/, 'String with variable and escape');
}

# ----------------------------------------------------------------------
# Test 6: Heredocs with special characters

{
  # Simple heredoc
  my $code = <<'PERL';
my $x = <<'EOF';
Hello World
EOF
PERL
  my $cl = parse_stmt_and_gen($code);
  like($cl, qr/"Hello World/, 'Simple heredoc');
}

{
  # Heredoc with special characters that need escaping in CL
  my $code = <<'PERL';
my $x = <<'EOF';
Special: "quotes" and \backslash\ and (parens) and 'single'
EOF
PERL
  my $cl = parse_stmt_and_gen($code);
  # Check that quotes are escaped
  like($cl, qr/\\"quotes\\"/, 'Heredoc: double quotes escaped');
  # Check that backslashes are escaped
  like($cl, qr/\\\\backslash\\\\/, 'Heredoc: backslashes escaped');
}

{
  # Heredoc with various punctuation
  my $code = <<'PERL';
my $msg = <<'END';
Symbols: @#$%^&*(){}[]|;:<>,.?/~`!
END
PERL
  my $cl = parse_stmt_and_gen($code);
  like($cl, qr/"Symbols:/, 'Heredoc with punctuation parses');
}

{
  # Heredoc with newlines
  my $code = <<'PERL';
my $multi = <<'TEXT';
Line 1
Line 2
Line 3
TEXT
PERL
  my $cl = parse_stmt_and_gen($code);
  like($cl, qr/Line 1/, 'Heredoc with multiple lines');
}

{
  # Heredoc used in expression
  my $code = <<'PERL';
print <<'MSG';
Error occurred
MSG
PERL
  my $cl = parse_stmt_and_gen($code);
  like($cl, qr/pl-print.*"Error occurred/, 'Heredoc as function argument');
}

# Tests complete
