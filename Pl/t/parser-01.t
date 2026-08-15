#          -*-Mode: CPerl -*-
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Test the statement-level parser prototype

use v5.32;
use strict;
use warnings;

use lib ".";

use Test::More tests => 10;

BEGIN { use_ok('Pl::Parser2') };


# Test 1: Simple expression
{
  my $code = '$x = 10;';
  my $result = Pl::Parser2->parse_code($code);

  # (v1 echoed the perl source as a ";; $x = 10" comment; v2 emits no
  # source echoes — that row asserted only the echo, a v1 implementation
  # detail, and is dropped pending the #132 decision on v2 echoes.)
  like($result, qr/p-scalar-= \$x 10/, 'Generated CL code');
}


# Test 2: Multiple statements
{
  my $code = <<'END';
$x = 1;
$y = 2;
$z = $x + $y;
END

  my $result = Pl::Parser2->parse_code($code);

  like($result, qr/p-scalar-= \$x 1/, 'First statement');
  like($result, qr/p-scalar-= \$z.*p-\+/, 'Third statement with addition');
}


# Test 3: Sub bodies are moved to forward-declaration positions
{
  my $code = <<'END';
sub greet { return "hello"; }
greet();
END

  my $result = Pl::Parser2->parse_code($code);

  # Sub body should appear (moved to top), before the call
  like($result, qr/\(p-sub pl-greet\b/, 'Sub body is present in output');
  # The sub body should appear before the call to greet()
  like($result, qr/p-sub pl-greet.*\(pl-greet\)/s, 'Sub definition appears before call');
}


# Test 4: Sub body moved after in-package in package context
{
  my $code = <<'END';
package MyClass;
do_setup();
sub do_setup { print "setup\n"; }
END

  my $result = Pl::Parser2->parse_code($code);

  # Sub body should appear in MyClass section, before the runtime call.
  # (v1 qualified the call MyClass::pl-do_setup; v2 emits the unqualified
  # (pl-do_setup) — it resolves inside the :MyClass section, same target.)
  like($result, qr/\(in-package :MyClass\).*\(p-sub pl-do_setup\b.*\(pl-do_setup\)/s,
       'Sub body appears in MyClass section before call');
  like($result, qr/\(p-sub pl-do_setup\b/,
       'p-sub for package sub');
}


# Test 5: Multiple subs in same package - all present
{
  my $code = <<'END';
sub foo { }
sub bar { }
sub foo { }
END

  my $result = Pl::Parser2->parse_code($code);

  # Both foo and bar should have p-sub definitions
  my @foo_defs = ($result =~ /\(p-sub pl-foo\b/g);
  # The second definition of foo overwrites the first in Perl, but our
  # output may have both since PPI sees them as separate statements.
  # Just verify at least one is present.
  ok(scalar @foo_defs >= 1, 'Sub foo definition is present');
  like($result, qr/\(p-sub pl-bar\b/, 'Sub bar definition is present');
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

my $output = Pl::Parser2->parse_code($sample);
diag $output;

diag "-" x 40;
diag "Parser prototype test complete.";
