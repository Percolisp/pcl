#          -*-Mode: CPerl -*-

# Test the statement-level parser prototype

use v5.32;
use strict;
use warnings;

use lib ".";

use Test::More tests => 5;

BEGIN { use_ok('Pl::Parser') };


# Test 1: Simple expression
{
  my $code = '$x = 10;';
  my $result = Pl::Parser->parse_code($code);

  like($result, qr/;; \$x = 10/, 'Comment shows Perl code');
  like($result, qr/pl-setf \$x 10/, 'Generated CL code');
}


# Test 2: Multiple statements
{
  my $code = <<'END';
$x = 1;
$y = 2;
$z = $x + $y;
END

  my $result = Pl::Parser->parse_code($code);

  like($result, qr/pl-setf \$x 1/, 'First statement');
  like($result, qr/pl-setf \$z.*pl-\+/, 'Third statement with addition');
}


diag "";
diag "Sample output from parser:";
diag "-" x 40;

my $sample = <<'END';
my $name = "World";
my $greeting = "Hello, " . $name;
$count = $count + 1;
$result = $x > 0 ? "positive" : "negative";
END

my $output = Pl::Parser->parse_code($sample);
diag $output;

diag "-" x 40;
diag "Parser prototype test complete.";
