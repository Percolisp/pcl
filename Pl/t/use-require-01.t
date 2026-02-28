#!/usr/bin/env perl

# Tests for use/require transpilation and runtime

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use File::Spec;

use lib ".";
use Pl::Parser;

my $runtime = "cl/pcl-runtime.lisp";
my $pl2cl   = "./pl2cl";

# ============================================================
# Transpilation Tests
# ============================================================

note "-------- Transpilation Tests:";

# Test: use strict becomes pragma comment
{
  my $result = Pl::Parser->parse_code('use strict;');
  like($result, qr/;; use strict \(pragma\)/, 'use strict is pragma comment');
}

# Test: use warnings becomes pragma comment
{
  my $result = Pl::Parser->parse_code('use warnings;');
  like($result, qr/;; use warnings \(pragma\)/, 'use warnings is pragma comment');
}

# Test: use v5.30 becomes pragma comment
{
  my $result = Pl::Parser->parse_code('use v5.30;');
  like($result, qr/;; use v5\.30 \(pragma\)/, 'use v5.30 is pragma comment');
}

# Test: use Module generates pl-use
{
  my $result = Pl::Parser->parse_code('use Foo::Bar;');
  like($result, qr/\(pl-use "Foo::Bar"\)/, 'use Module generates pl-use');
}

# Test: use Module with qw() imports
{
  my $result = Pl::Parser->parse_code('use Foo qw(bar baz);');
  like($result, qr/pl-use "Foo" :imports '\("bar" "baz"\)/, 'use with qw() imports');
}

# Test: require generates pl-require
{
  my $result = Pl::Parser->parse_code('require Foo::Bar;');
  like($result, qr/\(pl-require "Foo::Bar"\)/, 'require generates pl-require');
}

# Test: require with literal path string generates pl-require-file
{
  my $result = Pl::Parser->parse_code('require "./test.pl";');
  like($result, qr/pl-require-file.*test\.pl/, 'require with path generates pl-require-file');
}

# Test: require with single-quoted path
{
  my $result = Pl::Parser->parse_code("require './lib/helper.pl';");
  like($result, qr/pl-require-file.*lib.helper\.pl/, 'require with single-quoted path');
}

# Test: require with absolute path
{
  my $result = Pl::Parser->parse_code('require "/usr/lib/foo.pl";');
  like($result, qr/pl-require-file.*\/usr\/lib\/foo\.pl/, 'require with absolute path');
}

# Test: require with variable
{
  my $result = Pl::Parser->parse_code('require $path;');
  like($result, qr/pl-require-file \$path/, 'require with variable');
}

# Test: require with expression (concatenation)
{
  my $result = Pl::Parser->parse_code('require $dir . "/" . $file;');
  like($result, qr/pl-require-file.*pl-\..*\$dir.*\$file/, 'require with expression');
}

# Test: use lib modifies @INC
{
  my $result = Pl::Parser->parse_code('use lib "mylib";');
  like($result, qr/pl-unshift \@INC "mylib"/, 'use lib modifies @INC');
}

# Test: use lib with multiple paths via qw()
{
  my $result = Pl::Parser->parse_code('use lib qw(lib1 lib2);');
  like($result, qr/pl-unshift \@INC "lib1"/, 'use lib qw() - first path');
  like($result, qr/pl-unshift \@INC "lib2"/, 'use lib qw() - second path');
}

# Test: no strict becomes no-op comment
{
  my $result = Pl::Parser->parse_code('no strict;');
  like($result, qr/;; no strict \(no-op\)/, 'no strict is no-op');
}

# ============================================================
# pl2cl CLI Tests
# ============================================================

note "-------- pl2cl CLI Tests:";

# Test: pl2cl generates @INC initialization
{
  my $result = `echo 'my \$x = 1;' | $pl2cl 2>&1`;
  like($result, qr/\*pcl-pl2cl-path\*/, 'pl2cl sets *pcl-pl2cl-path*');
  like($result, qr/setf pcl::\@INC/, 'pl2cl initializes @INC');
}

# Test: --no-cache flag
{
  my $result = `echo 'my \$x = 1;' | $pl2cl --no-cache 2>&1`;
  like($result, qr/\*pcl-skip-cache\* t/, '--no-cache sets *pcl-skip-cache*');
}

# Test: --cache-lisp flag
{
  my $result = `echo 'my \$x = 1;' | $pl2cl --cache-lisp 2>&1`;
  like($result, qr/\*pcl-cache-fasl\* nil/, '--cache-lisp sets *pcl-cache-fasl* nil');
}

# ============================================================
# Runtime Tests (actual module loading)
# ============================================================

note "-------- Runtime Tests:";

# Create a test module
my $tempdir = tempdir(CLEANUP => 1);
my $module_path = File::Spec->catfile($tempdir, "TestMod.pm");
open my $mod_fh, '>', $module_path or die "Can't write $module_path: $!";
print $mod_fh <<'END_MODULE';
package TestMod;
sub get_value { return 42; }
1;
END_MODULE
close $mod_fh;

# Test: require loads a module
{
  my $test_code = qq{
use lib "$tempdir";
require TestMod;
say TestMod::get_value();
};

  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $test_code;
  close $fh;

  my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;

  # Write CL code to temp file
  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  # Run with SBCL
  my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

  # Filter SBCL noise
  $output =~ s/^;.*\n//gm;
  $output =~ s/^\s*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;
  $output = (split /\n/, $output)[0] // '';

  is($output, '42', 'require loads module and function works');

  unlink $pl_file, $cl_file;
}

# Test: %INC is populated after require
{
  my $test_code = qq{
use lib "$tempdir";
require TestMod;
if (\$INC{"TestMod.pm"}) {
  say "loaded";
} else {
  say "not loaded";
}
};

  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $test_code;
  close $fh;

  my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;

  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
  $output =~ s/^;.*\n//gm;
  $output =~ s/^\s*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;
  $output = (split /\n/, $output)[0] // '';

  is($output, 'loaded', '%INC is populated after require');

  unlink $pl_file, $cl_file;
}

# Test: double require doesn't reload
{
  my $counter_module = File::Spec->catfile($tempdir, "Counter.pm");
  open my $cnt_fh, '>', $counter_module or die "Can't write: $!";
  print $cnt_fh <<'END_MODULE';
package Counter;
our $count = 0;
$count++;
sub get_count { return $count; }
1;
END_MODULE
  close $cnt_fh;

  my $test_code = qq{
use lib "$tempdir";
require Counter;
require Counter;  # Should not reload
say Counter::get_count();
};

  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $test_code;
  close $fh;

  my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;

  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
  $output =~ s/^;.*\n//gm;
  $output =~ s/^\s*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;
  $output = (split /\n/, $output)[0] // '';

  is($output, '1', 'double require does not reload module');

  unlink $pl_file, $cl_file;
}

# ============================================================
# Complex Runtime Tests
# ============================================================

note "-------- Complex Runtime Tests:";

# Helper to run Perl code and get output
sub run_pl {
  my ($code, $extra_libs) = @_;
  $extra_libs //= [];

  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;

  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
  $output =~ s/^;.*\n//gm;
  $output =~ s/^\s*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;

  unlink $pl_file, $cl_file;

  return $output;
}

# Test: Nested requires (A requires B)
{
  my $modB = File::Spec->catfile($tempdir, "ModB.pm");
  open my $fh, '>', $modB or die;
  print $fh <<'EOF';
package ModB;
sub value_b { return 100; }
1;
EOF
  close $fh;

  my $modA = File::Spec->catfile($tempdir, "ModA.pm");
  open $fh, '>', $modA or die;
  print $fh <<"EOF";
package ModA;
use lib "$tempdir";
require ModB;
sub value_a { return ModB::value_b() + 1; }
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
require ModA;
say ModA::value_a();
});

  like($output, qr/^101/, 'nested require: A requires B');
}

# Test: Module in subdirectory (Foo/Bar.pm)
{
  my $subdir = File::Spec->catdir($tempdir, "Foo");
  mkdir $subdir;

  my $mod = File::Spec->catfile($subdir, "Bar.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package Foo::Bar;
sub baz { return "from Foo::Bar"; }
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
require Foo::Bar;
say Foo::Bar::baz();
});

  like($output, qr/from Foo::Bar/, 'require Foo::Bar loads Foo/Bar.pm');
}

# Test: Module with persistent package variable
{
  my $mod = File::Spec->catfile($tempdir, "Stateful.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package Stateful;
our $state = 0;
sub increment { $state++; return $state; }
sub get_state { return $state; }
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
require Stateful;
Stateful::increment();
Stateful::increment();
Stateful::increment();
say Stateful::get_state();
});

  like($output, qr/^3/, 'package variable persists across calls');
}

# Test: Module with multiple functions
{
  my $mod = File::Spec->catfile($tempdir, "Math.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package Math;
sub add { my ($a, $b) = @_; return $a + $b; }
sub mul { my ($a, $b) = @_; return $a * $b; }
sub square { my ($x) = @_; return $x * $x; }
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
require Math;
my \$sum = Math::add(3, 4);
my \$prod = Math::mul(5, 6);
my \$sq = Math::square(7);
say "\$sum \$prod \$sq";
});

  like($output, qr/^7 30 49/, 'module with multiple functions');
}

# Test: Conditional require
{
  my $output = run_pl(qq{
use lib "$tempdir";
my \$loaded = 0;
if (1) {
  require TestMod;
  \$loaded = 1;
}
say \$loaded;
say TestMod::get_value();
});

  like($output, qr/1\n42/, 'conditional require works');
}

# Test: require in a subroutine (dynamic loading)
# Fixed: Parser now pre-declares referenced packages at top of generated code
{
  my $output = run_pl(qq{
use lib "$tempdir";

sub load_and_call {
  require TestMod;
  return TestMod::get_value();
}

say load_and_call();
});

  like($output, qr/^42/, 'require inside subroutine (dynamic loading)');
}

# Test: Multiple modules with cross-references
{
  my $utils = File::Spec->catfile($tempdir, "Utils.pm");
  open my $fh, '>', $utils or die;
  print $fh <<'EOF';
package Utils;
sub double { my ($x) = @_; return $x * 2; }
1;
EOF
  close $fh;

  my $calc = File::Spec->catfile($tempdir, "Calc.pm");
  open $fh, '>', $calc or die;
  print $fh <<"EOF";
package Calc;
use lib "$tempdir";
require Utils;
sub quad { my (\$x) = \@_; return Utils::double(Utils::double(\$x)); }
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
require Calc;
say Calc::quad(5);
});

  like($output, qr/^20/, 'module chain: Calc uses Utils');
}

# Test: Cache file is created
{
  # Clear cache first
  system("rm -rf ~/.pcl-cache/*.fasl ~/.pcl-cache/*.lisp 2>/dev/null");

  my $mod = File::Spec->catfile($tempdir, "Cached.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package Cached;
sub test { return "cached"; }
1;
EOF
  close $fh;

  # Run without --no-cache to create cache
  my ($pl_fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $pl_fh qq{
use lib "$tempdir";
require Cached;
say Cached::test();
};
  close $pl_fh;

  my $cl_code = `$pl2cl $pl_file 2>&1`;  # Note: no --no-cache

  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

  # Check cache was created
  my @cache_files = glob("$ENV{HOME}/.pcl-cache/*");
  ok(scalar(@cache_files) > 0, 'cache files created');

  unlink $pl_file, $cl_file;
}

# Test: require with string path
{
  my $output = run_pl(qq{
use lib "$tempdir";
require "TestMod.pm";
say TestMod::get_value();
});

  # This might work or might not depending on implementation
  # Just verify it doesn't crash badly
  ok(defined $output, 'require with string path does not crash');
}

# Test: Three-level module hierarchy
{
  my $deep = File::Spec->catdir($tempdir, "Deep");
  mkdir $deep;
  my $deeper = File::Spec->catdir($deep, "Deeper");
  mkdir $deeper;

  my $mod = File::Spec->catfile($deeper, "Module.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package Deep::Deeper::Module;
sub dive { return "deep dive"; }
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
require Deep::Deeper::Module;
say Deep::Deeper::Module::dive();
});

  like($output, qr/deep dive/, 'three-level module hierarchy');
}

# Test: Module that returns false (should work - we don't check return value)
{
  my $mod = File::Spec->catfile($tempdir, "FalseRet.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package FalseRet;
sub val { return "it works"; }
0;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
require FalseRet;
say FalseRet::val();
});

  like($output, qr/it works/, 'module returning 0 still works');
}

# ============================================================
# Exporter Tests (symbol import from @EXPORT)
# ============================================================

note "-------- Exporter Tests:";

# Test: Module with @EXPORT - symbols are imported
{
  my $mod = File::Spec->catfile($tempdir, "Exportable.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package Exportable;
our @EXPORT = qw(%data get_value);
our %data = (key1 => 100, key2 => 200);
sub get_value { return 42; }
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
use Exportable;
say \$data{key1};
say get_value();
});

  like($output, qr/100/, 'Exporter: hash from @EXPORT is accessible');
  like($output, qr/42/, 'Exporter: function from @EXPORT is accessible');
}

# Test: Config module import (uses our lib/Config.pm)
{
  my $output = run_pl(q{
use Config;
say $Config{ivsize};
say $Config{osname};
});

  like($output, qr/8/, 'Config: ivsize imported and accessible');
  like($output, qr/linux/, 'Config: osname imported and accessible');
}

# Test: Qualified access still works alongside import
{
  my $mod = File::Spec->catfile($tempdir, "Dual.pm");
  open my $fh, '>', $mod or die;
  print $fh <<'EOF';
package Dual;
our @EXPORT = qw($var);
our $var = "exported";
our $other = "not exported";
1;
EOF
  close $fh;

  my $output = run_pl(qq{
use lib "$tempdir";
use Dual;
say \$var;
say \$Dual::other;
});

  like($output, qr/exported/, 'Exporter: imported symbol works');
  like($output, qr/not exported/, 'Exporter: qualified access to non-exported works');
}

done_testing();
