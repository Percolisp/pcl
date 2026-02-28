#!/usr/bin/env perl

# Tests for file glob <*.txt> and glob() function
# Glob expands file patterns and returns matching files

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);

my $pl2cl = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';

# Create temp directory with test files
my $tmpdir = tempdir(CLEANUP => 1);
for my $name (qw(a.txt b.txt c.log d.txt sub)) {
  if ($name eq 'sub') {
    mkdir "$tmpdir/$name";
  } else {
    open my $fh, '>', "$tmpdir/$name" or die $!;
    close $fh;
  }
}

# Helper to run transpiled code and capture output
sub run_pcl {
  my ($code) = @_;

  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;

  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

  # Filter SBCL noise
  $output =~ s/^;.*\n//gm;
  $output =~ s/^\s*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;
  $output =~ s/STYLE-WARNING.*\n//g;

  unlink $pl_file, $cl_file;

  return $output;
}

# ============================================================
# Transpilation Tests
# ============================================================

# Test: Basic glob transpilation
{
  my $code = 'my @f = <*.txt>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = `$pl2cl $pl_file 2>&1`;

  like($cl, qr/pl-glob.*"\*\.txt"/, 'glob <*.txt> generates pl-glob call');

  unlink $pl_file;
}

# Test: Glob with path
{
  my $code = 'my @f = </tmp/*.log>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = `$pl2cl $pl_file 2>&1`;

  like($cl, qr/pl-glob.*"\/tmp\/\*\.log"/, 'glob </tmp/*.log> generates pl-glob with path');

  unlink $pl_file;
}

# Test: Readline still works (not confused with glob)
{
  my $code = 'my $line = <STDIN>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = `$pl2cl $pl_file 2>&1`;

  like($cl, qr/pl-readline/, '<STDIN> still generates pl-readline');
  unlike($cl, qr/pl-glob/, '<STDIN> does NOT generate pl-glob');

  unlink $pl_file;
}

# Test: Variable filehandle still works
{
  my $code = 'my $line = <$fh>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = `$pl2cl $pl_file 2>&1`;

  like($cl, qr/pl-readline.*\$fh/, '<$fh> generates pl-readline');
  unlike($cl, qr/pl-glob/, '<$fh> does NOT generate pl-glob');

  unlink $pl_file;
}

# ============================================================
# Runtime Tests
# ============================================================

# Test: Basic glob in list context
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob *.txt returns 3 .txt files');
}

# Test: Glob returns sorted list
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.txt>;
print join(",", sort \@files);
END_CODE

  like($output, qr/a\.txt.*b\.txt.*d\.txt/s, 'glob returns files (sorted)');
}

# Test: Glob with no matches returns empty
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.xyz>;
print "count:" . scalar(\@files);
END_CODE

  like($output, qr/count:0/, 'glob with no matches returns empty array');
}

# Test: Glob with ? wildcard
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/?.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob with ? wildcard works');
}

# Test: Multiple wildcards
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.*>;
print scalar(\@files);
END_CODE

  # Should match a.txt, b.txt, c.log, d.txt = 4 files
  like($output, qr/4/, 'glob *.* matches all files with extensions');
}

# Test: Glob in scalar context (returns one file)
{
  my $output = run_pcl(<<"END_CODE");
my \$file = <$tmpdir/*.txt>;
print defined \$file ? "got one" : "undef";
END_CODE

  like($output, qr/got one/, 'glob in scalar context returns first match');
}

# ============================================================
# Corner Cases
# ============================================================

# Test: Glob with brackets []
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[ab].txt>;
print scalar(\@files);
END_CODE

  like($output, qr/2/, 'glob with [ab] character class works');
}

# Test: Distinguish glob from readline by content
{
  my $code = q{
    my $x = <STDIN>;   # readline
    my @f = <*.pm>;    # glob
  };
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = `$pl2cl $pl_file 2>&1`;

  like($cl, qr/pl-readline.*STDIN/, 'STDIN recognized as readline');
  like($cl, qr/pl-glob.*\*\.pm/, '*.pm recognized as glob');

  unlink $pl_file;
}

# Test: Empty <> is readline (not glob)
{
  my $code = 'my $line = <>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = `$pl2cl $pl_file 2>&1`;

  like($cl, qr/pl-readline\)/, 'empty <> is readline');
  unlike($cl, qr/pl-glob/, 'empty <> is NOT glob');

  unlink $pl_file;
}

# ============================================================
# Additional Edge Cases
# ============================================================

# Test: Variable interpolation in glob pattern
{
  my $output = run_pcl(<<"END_CODE");
my \$dir = "$tmpdir";
my \@files = <\$dir/*.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob with variable interpolation works');
}

# Test: Hidden files (dotfiles)
{
  # Create a dotfile
  open my $fh, '>', "$tmpdir/.hidden" or die $!;
  close $fh;

  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/.*>;
print scalar(\@files);
END_CODE

  # Should match .hidden (and possibly . and ..)
  like($output, qr/[123]/, 'glob .* matches hidden files');

  unlink "$tmpdir/.hidden";
}

# Test: glob() function form (not angle brackets)
{
  my $code = 'my @f = glob("*.txt");';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = `$pl2cl $pl_file 2>&1`;

  like($cl, qr/pl-glob.*"\*\.txt"/, 'glob() function generates pl-glob call');

  unlink $pl_file;
}

# Test: glob() function runtime
{
  my $output = run_pcl(<<"END_CODE");
my \@files = glob("$tmpdir/*.txt");
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob() function returns correct count');
}

# Test: Relative path glob
{
  # Run from tmpdir context
  my $output = run_pcl(<<"END_CODE");
chdir("$tmpdir");
my \@files = <*.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob with relative path works');
}

# Test: No wildcards - literal filename
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/a.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/1/, 'glob with literal filename returns 1 match');
}

# Test: No wildcards - nonexistent file
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/nonexistent.txt>;
print "count:" . scalar(\@files);
END_CODE

  like($output, qr/count:0/, 'glob with nonexistent literal file returns empty');
}

# Test: Glob used directly in foreach
{
  my $output = run_pcl(<<"END_CODE");
my \$count = 0;
for my \$f (<$tmpdir/*.txt>) {
    \$count++;
}
print \$count;
END_CODE

  like($output, qr/3/, 'glob works directly in foreach');
}

# Test: Glob in boolean context (if)
{
  my $output = run_pcl(<<"END_CODE");
if (<$tmpdir/*.txt>) {
    print "found";
} else {
    print "empty";
}
END_CODE

  like($output, qr/found/, 'glob in boolean context works');
}

# Test: Glob with no matches in boolean context
{
  my $output = run_pcl(<<"END_CODE");
if (<$tmpdir/*.xyz>) {
    print "found";
} else {
    print "empty";
}
END_CODE

  like($output, qr/empty/, 'glob with no matches is false');
}

# Test: Multiple glob calls in same expression
{
  my $output = run_pcl(<<"END_CODE");
my \@all = (<$tmpdir/*.txt>, <$tmpdir/*.log>);
print scalar(\@all);
END_CODE

  like($output, qr/4/, 'multiple globs can be combined');
}

# Test: Glob result used with grep
{
  my $output = run_pcl(<<"END_CODE");
my \@files = grep { /\\/a\\.txt\$/ } <$tmpdir/*.txt>;
print scalar(\@files);
END_CODE

  # Should match only a.txt (ends with /a.txt)
  like($output, qr/1/, 'glob result can be filtered with grep');
}

# Test: Character range in brackets (now expanded by pl-glob)
{
  # Create c.txt for this test
  open my $fh, '>', "$tmpdir/c.txt" or die $!;
  close $fh;

  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[a-c].txt>;
print scalar(\@files);
END_CODE

  # Should match a.txt, b.txt, c.txt = 3 files
  like($output, qr/3/, 'glob with [a-c] character range works');

  unlink "$tmpdir/c.txt";
}

# Test: Negated character class [!d] (handled at transpile time)
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[!d].txt>;
print scalar(\@files);
END_CODE

  # Should match a.txt, b.txt (not d.txt) - 2 files since c.txt was cleaned up
  # Wait, we have a.txt, b.txt, d.txt from setup. c.log not c.txt.
  # So [!d].txt should match a.txt, b.txt = 2 files
  like($output, qr/2/, 'glob with [!d] negated class works');
}

# Test: Negated class with caret [^d]
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[^d].txt>;
print scalar(\@files);
END_CODE

  like($output, qr/2/, 'glob with [^d] negated class works');
}

done_testing();
