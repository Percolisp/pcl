#          -*-Mode: CPerl -*-

# Test the statement-level parser prototype

use v5.32;
use strict;
use warnings;

use lib ".";

use Test::More tests => 12;

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


# Test 3: Forward declarations for subs in main package
{
  my $code = <<'END';
sub greet { return "hello"; }
greet();
END

  my $result = Pl::Parser->parse_code($code);

  like($result, qr/Forward declarations:/, 'Forward declaration comment emitted');
  like($result, qr/\(unless \(fboundp 'pl-greet\)/, 'Forward declaration uses fboundp guard');
  like($result, qr/\(defun pl-greet \(&rest args\)/, 'Forward declaration is a stub defun');
}


# Test 4: Forward declarations for subs in a package
{
  my $code = <<'END';
package MyClass;
do_setup();
sub do_setup { print "setup\n"; }
END

  my $result = Pl::Parser->parse_code($code);

  # Forward decl should appear after (in-package :MyClass)
  like($result, qr/\(in-package :MyClass\)\n;; Forward declarations:/,
       'Forward declarations appear after in-package');
  like($result, qr/\(unless \(fboundp 'pl-do_setup\)/,
       'Forward declaration for package sub');
}


# Test 5: Multiple subs in same package get deduplicated forward declarations
{
  my $code = <<'END';
sub foo { }
sub bar { }
sub foo { }
END

  my $result = Pl::Parser->parse_code($code);

  # Should only have one forward decl for foo, even if sub defined twice
  my @foo_decls = ($result =~ /\(unless \(fboundp 'pl-foo\)/g);
  is(scalar @foo_decls, 1, 'Duplicate sub names are deduplicated in forward declarations');
  like($result, qr/\(unless \(fboundp 'pl-bar\)/, 'Forward declaration for bar');
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
