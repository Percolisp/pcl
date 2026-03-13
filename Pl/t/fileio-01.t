#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Test file I/O code generation

use v5.30;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 40;

BEGIN { use_ok('Pl::PExpr') };
BEGIN { use_ok('Pl::ExprToCL') };
BEGIN { use_ok('Pl::Environment') };


# Shared environment for tests (with pre-registered filehandles)
my $env = Pl::Environment->new();


# Helper: parse Perl expression and generate CL code
sub perl_to_cl {
  my $code    = shift;
  my $indent  = shift // 0;

  my $doc     = PPI::Document->new(\$code);
  my $expr    = _get_ppi_part($doc);
  my $expr_o  = Pl::PExpr->new(e => $expr, full_PPI => $doc, environment => $env);
  my $node_id = $expr_o->parse_expr_to_tree($expr);

  my $gen     = Pl::ExprToCL->new(expr_o => $expr_o, indent_level => $indent);
  return $gen->generate($node_id);
}


# Helper: test that Perl expression generates expected CL code
sub test_codegen {
  my $perl_code = shift;
  my $expected  = shift;
  my $desc      = shift // "Perl: $perl_code";
  my $indent    = shift // 0;

  my $result = perl_to_cl($perl_code, $indent);
  is($result, $expected, $desc);
}


# Get parts of statement from PPI Document
sub _get_ppi_part {
  my $doc     = shift;
  my $stmt_ix = shift // 0;

  my @stmts;
  my @parts;
  if (ref($doc) eq 'PPI::Document') {
    @stmts    = $doc->children();
    @parts    = $stmts[$stmt_ix]->children();
  } elsif (ref($doc) eq 'PPI::Statement') {
    @parts    = $doc->children();
  } else {
    die "Code is not PPI::Document?? Is: " . ref($doc);
  }

  return \@parts;
}


# ============================================================
diag "";
diag "-------- open function (3-arg):";

test_codegen('open($fh, "<", $file)',
            '(p-open $fh "<" $file)',
            'open 3-arg read');

test_codegen('open($fh, ">", $file)',
            '(p-open $fh ">" $file)',
            'open 3-arg write');

test_codegen('open($fh, ">>", $file)',
            '(p-open $fh ">>" $file)',
            'open 3-arg append');

# Note: open(my $fh, ...) has complex parsing where 'my' affects the whole expression
# Using variable instead for cleaner test
test_codegen('open($fh, "<", "data.txt")',
            '(p-open $fh "<" "data.txt")',
            'open with string filename');

test_codegen('open($fh, "+<", $file)',
            '(p-open $fh "+<" $file)',
            'open 3-arg read-write');

test_codegen('open($fh, "|-", "cmd")',
            '(p-open $fh "|-" "cmd")',
            'open pipe to command');

test_codegen('open($fh, "-|", "cmd")',
            '(p-open $fh "-|" "cmd")',
            'open pipe from command');


# ============================================================
diag "";
diag "-------- open with bareword filehandle:";

# First call registers FH in environment
test_codegen('open(FH, ">", "out.txt")',
            '(p-open FH ">" "out.txt")',
            'open registers bareword filehandle');

# Now FH is known
test_codegen('close(FH)',
            '(p-close FH)',
            'close uses registered filehandle');


# ============================================================
diag "";
diag "-------- close function:";

test_codegen('close($fh)',
            '(p-close $fh)',
            'close variable filehandle');


# ============================================================
diag "";
diag "-------- eof function:";

test_codegen('eof($fh)',
            '(p-eof $fh)',
            'eof with filehandle');

test_codegen('eof()',
            '(p-eof)',
            'eof without args');


# ============================================================
diag "";
diag "-------- tell function:";

test_codegen('tell($fh)',
            '(p-tell $fh)',
            'tell with filehandle');

test_codegen('tell()',
            '(p-tell)',
            'tell without args');


# ============================================================
diag "";
diag "-------- seek function:";

test_codegen('seek($fh, 0, 0)',
            '(p-seek $fh 0 0)',
            'seek to beginning');

test_codegen('seek($fh, $pos, 1)',
            '(p-seek $fh $pos 1)',
            'seek relative');

# Note: -10 is a negative number literal, not (p-- 10)
test_codegen('seek($fh, -10, 2)',
            '(p-seek $fh -10 2)',
            'seek from end');


# ============================================================
diag "";
diag "-------- binmode function:";

test_codegen('binmode($fh)',
            '(p-binmode $fh)',
            'binmode basic');

test_codegen('binmode($fh, ":utf8")',
            '(p-binmode $fh ":utf8")',
            'binmode with encoding');

test_codegen('binmode($fh, ":raw")',
            '(p-binmode $fh ":raw")',
            'binmode raw');


# ============================================================
diag "";
diag "-------- read function:";

test_codegen('read($fh, $buf, 1024)',
            '(p-read $fh $buf 1024)',
            'read bytes');

test_codegen('read($fh, $buf, $len, $offset)',
            '(p-read $fh $buf $len $offset)',
            'read with offset');


# ============================================================
diag "";
diag "-------- sysread/syswrite:";

test_codegen('sysread($fh, $buf, 1024)',
            '(p-sysread $fh $buf 1024)',
            'sysread');

test_codegen('syswrite($fh, $data)',
            '(p-syswrite $fh $data)',
            'syswrite');

test_codegen('syswrite($fh, $data, $len)',
            '(p-syswrite $fh $data $len)',
            'syswrite with length');


# ============================================================
diag "";
diag "-------- truncate function:";

test_codegen('truncate($fh, $size)',
            '(p-truncate $fh $size)',
            'truncate filehandle');

test_codegen('truncate($file, 0)',
            '(p-truncate $file 0)',
            'truncate by filename');


# ============================================================
diag "";
diag "-------- stat/lstat functions:";

test_codegen('stat($file)',
            '(p-stat $file)',
            'stat file');

test_codegen('stat($fh)',
            '(p-stat $fh)',
            'stat filehandle');

test_codegen('lstat($file)',
            '(p-lstat $file)',
            'lstat file');


# ============================================================
diag "";
diag "-------- fileno function:";

test_codegen('fileno($fh)',
            '(p-fileno $fh)',
            'fileno');


# ============================================================
diag "";
diag "-------- getc function:";

test_codegen('getc($fh)',
            '(p-getc $fh)',
            'getc with filehandle');

test_codegen('getc()',
            '(p-getc)',
            'getc from STDIN');


# ============================================================
diag "";
diag "-------- Directory operations:";

test_codegen('opendir($dh, $dir)',
            '(p-opendir $dh $dir)',
            'opendir');

test_codegen('readdir($dh)',
            '(p-readdir $dh)',
            'readdir');

test_codegen('closedir($dh)',
            '(p-closedir $dh)',
            'closedir');

test_codegen('rewinddir($dh)',
            '(p-rewinddir $dh)',
            'rewinddir');


diag "";
diag "All file I/O tests completed!";
