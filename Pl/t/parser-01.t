#          -*-Mode: CPerl -*-

# Test the statement-level parser prototype

use v5.32;
use strict;
use warnings;

use lib ".";

use Test::More tests => 11;

BEGIN { use_ok('Pl::Parser') };


# Test 1: Simple expression
{
  my $code = '$x = 10;';
  my $result = Pl::Parser->parse_code($code);

  like($result, qr/;; \$x = 10/, 'Comment shows Perl code');
  like($result, qr/pl-scalar-= \$x 10/, 'Generated CL code');
}


# Test 2: Multiple statements
{
  my $code = <<'END';
$x = 1;
$y = 2;
$z = $x + $y;
END

  my $result = Pl::Parser->parse_code($code);

  like($result, qr/pl-scalar-= \$x 1/, 'First statement');
  like($result, qr/pl-scalar-= \$z.*pl-\+/, 'Third statement with addition');
}


# Test 3: Sub bodies are moved to forward-declaration positions
{
  my $code = <<'END';
sub greet { return "hello"; }
greet();
END

  my $result = Pl::Parser->parse_code($code);

  # Sub body should appear (moved to top), before the call
  like($result, qr/\(pl-sub pl-greet\b/, 'Sub body is present in output');
  # The sub body should appear before the call to greet()
  like($result, qr/pl-sub pl-greet.*\(pl-greet\)/s, 'Sub definition appears before call');
}


# Test 4: Sub body moved after in-package in package context
{
  my $code = <<'END';
package MyClass;
do_setup();
sub do_setup { print "setup\n"; }
END

  my $result = Pl::Parser->parse_code($code);

  # Sub body should appear in MyClass section, before the runtime call
  like($result, qr/\(in-package :MyClass\).*\(pl-sub pl-do_setup\b.*\(pl-do_setup\)/s,
       'Sub body appears in MyClass section before call');
  like($result, qr/\(pl-sub pl-do_setup\b/,
       'pl-sub for package sub');
}


# Test 5: Multiple subs in same package - all present
{
  my $code = <<'END';
sub foo { }
sub bar { }
sub foo { }
END

  my $result = Pl::Parser->parse_code($code);

  # Both foo and bar should have pl-sub definitions
  my @foo_defs = ($result =~ /\(pl-sub pl-foo\b/g);
  # The second definition of foo overwrites the first in Perl, but our
  # output may have both since PPI sees them as separate statements.
  # Just verify at least one is present.
  ok(scalar @foo_defs >= 1, 'Sub foo definition is present');
  like($result, qr/\(pl-sub pl-bar\b/, 'Sub bar definition is present');
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
