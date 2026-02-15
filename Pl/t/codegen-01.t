#          -*-Mode: CPerl -*-

# Test the expression to Common Lisp code generator

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use PPI;
use PPI::Dumper;

use Test::More tests => 155;

BEGIN { use_ok('Pl::PExpr') };
BEGIN { use_ok('Pl::ExprToCL') };
BEGIN { use_ok('Pl::Environment') };


# Helper: parse Perl expression and generate CL code
sub perl_to_cl {
  my $code    = shift;
  my $indent  = shift // 0;

  my $doc     = PPI::Document->new(\$code);
  my $expr    = _get_ppi_part($doc);
  my $env     = Pl::Environment->new();
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
diag "-------- Simple literals and variables:";

test_codegen('$x', '$x', 'Scalar variable');
test_codegen('@arr', '@arr', 'Array variable');
test_codegen('%hash', '%hash', 'Hash variable');
test_codegen('42', '42', 'Number literal');
test_codegen('"hello"', '"hello"', 'String literal');

# ============================================================
diag "";
diag "-------- Number literal formats:";

test_codegen('0x1234', '#x1234', 'Hex literal');
test_codegen('0b1010', '#b1010', 'Binary literal');
test_codegen('0777', '#o777', 'Octal literal');
test_codegen('1_000_000', '1000000', 'Underscored decimal');
test_codegen('0xFF_FF', '#xFFFF', 'Underscored hex');
test_codegen('0b1111_0000', '#b11110000', 'Underscored binary');


# ============================================================
diag "";
diag "-------- Arithmetic operators:";

test_codegen('$x + $y', '(pl-+ $x $y)', 'Addition');
test_codegen('$x - $y', '(pl-- $x $y)', 'Subtraction');
test_codegen('$x * $y', '(pl-* $x $y)', 'Multiplication');
test_codegen('$x / $y', '(pl-/ $x $y)', 'Division');
test_codegen('$x % $y', '(pl-% $x $y)', 'Modulo');
test_codegen('$x ** $y', '(pl-** $x $y)', 'Exponentiation');


# ============================================================
diag "";
diag "-------- String operators:";

test_codegen('$x . $y', '(pl-. $x $y)', 'String concatenation');
test_codegen('$x x 3', '(pl-str-x $x 3)', 'String repetition');


# ============================================================
diag "";
diag "-------- Comparison operators (numeric):";

test_codegen('$x == $y', '(pl-== $x $y)', 'Numeric equality');
test_codegen('$x != $y', '(pl-!= $x $y)', 'Numeric inequality');
test_codegen('$x < $y', '(pl-< $x $y)', 'Less than');
test_codegen('$x > $y', '(pl-> $x $y)', 'Greater than');
test_codegen('$x <= $y', '(pl-<= $x $y)', 'Less than or equal');
test_codegen('$x >= $y', '(pl->= $x $y)', 'Greater than or equal');
test_codegen('$x <=> $y', '(pl-<=> $x $y)', 'Spaceship');


# ============================================================
diag "";
diag "-------- Comparison operators (string):";

test_codegen('$x eq $y', '(pl-str-eq $x $y)', 'String equality');
test_codegen('$x ne $y', '(pl-str-ne $x $y)', 'String inequality');
test_codegen('$x lt $y', '(pl-str-lt $x $y)', 'String less than');
test_codegen('$x gt $y', '(pl-str-gt $x $y)', 'String greater than');


# ============================================================
diag "";
diag "-------- Logical operators:";

test_codegen('$x && $y', '(pl-&& $x $y)', 'Logical AND (&&)');
test_codegen('$x || $y', '(pl-|| $x $y)', 'Logical OR (||)');
test_codegen('$x // $y', '(pl-// $x $y)', 'Defined-or');
test_codegen('$x and $y', '(pl-and $x $y)', 'Logical and');
test_codegen('$x or $y', '(pl-or $x $y)', 'Logical or');


# ============================================================
diag "";
diag "-------- Bitwise operators (with exceptions):";

test_codegen('$x & $y', '(pl-bit-and $x $y)', 'Bitwise AND');
test_codegen('$x | $y', '(pl-bit-or $x $y)', 'Bitwise OR');
test_codegen('$x ^ $y', '(pl-bit-xor $x $y)', 'Bitwise XOR');


# ============================================================
diag "";
diag "-------- Assignment:";

test_codegen('$x = $y', '(pl-scalar-= $x $y)', 'Simple assignment');
test_codegen('$x += 5', '(pl-incf $x 5)', 'Add-assign');
test_codegen('$x -= 5', '(pl-decf $x 5)', 'Subtract-assign');


# ============================================================
diag "";
diag "-------- Prefix/postfix operators:";

test_codegen('!$x', '(pl-! $x)', 'Logical not');
test_codegen('++$x', '(pl-pre++ $x)', 'Pre-increment');
test_codegen('--$x', '(pl-pre-- $x)', 'Pre-decrement');
test_codegen('$x++', '(pl-post++ $x)', 'Post-increment');
test_codegen('$x--', '(pl-post-- $x)', 'Post-decrement');


# ============================================================
diag "";
diag "-------- File test operators:";

test_codegen('-f $file', '(pl--f $file)', 'File test -f');
test_codegen('-e $path', '(pl--e $path)', 'File test -e');
test_codegen('-d $dir', '(pl--d $dir)', 'File test -d');


# ============================================================
diag "";
diag "-------- Ternary operator:";

test_codegen('$x ? $y : $z', '(pl-if $x $y $z)', 'Simple ternary');


# ============================================================
diag "";
diag "-------- Function calls:";

test_codegen('foo()', '(pl-foo)', 'Function call no args');
test_codegen('foo($x)', '(pl-foo $x)', 'Function call one arg');
test_codegen('foo($x, $y)', '(pl-foo $x $y)', 'Function call two args');


# ============================================================
diag "";
diag "-------- Array/hash access:";

test_codegen('$arr[0]', '(pl-aref @arr 0)', 'Array access');
test_codegen('$hash{key}', '(pl-gethash %hash "key")', 'Hash access');
test_codegen('$ref->[0]', '(pl-aref-deref $ref 0)', 'Array ref access');
test_codegen('$ref->{key}', '(pl-gethash-deref $ref "key")', 'Hash ref access');


# ============================================================
diag "";
diag "-------- Slices:";

test_codegen('@arr[1,2,3]', '(pl-aslice @arr 1 2 3)', 'Array slice');
test_codegen('@hash{"a","b"}', '(pl-hslice %hash "a" "b")', 'Hash slice');


# ============================================================
diag "";
diag "-------- Initializers:";

test_codegen('[1, 2, 3]', '(make-pl-box (pl-array-init 1 2 3))', 'Array initializer (boxed ref)');
test_codegen('{a => 1, b => 2}', '(make-pl-box (pl-hash "a" 1 "b" 2))', 'Hash initializer (boxed ref)');


# ============================================================
diag "";
diag "-------- Method calls:";

test_codegen('$obj->method()', '(pl-method-call $obj \'method)', 'Method call no args');
test_codegen('$obj->method($x)', '(pl-method-call $obj \'method $x)', 'Method call with arg');


# ============================================================
diag "";
diag "-------- Complex expressions:";

test_codegen('$x + $y * $z',
             '(pl-+ $x (pl-* $y $z))',
             'Operator precedence');

test_codegen('($x + $y) * $z',
             '(pl-* (pl-+ $x $y) $z)',
             'Parenthesized expression');

test_codegen('$foo + (my $x = 1)',
             '(pl-+ $foo (pl-scalar-= $x 1))',
             'Inline my declaration in parens');


# ============================================================
diag "";
diag "-------- Assignment to array/hash elements:";

test_codegen('$arr[0] = 5',
             '(pl-setf (pl-aref @arr 0) 5)',
             'Assign to array element');

test_codegen('$hash{key} = "value"',
             '(pl-setf (pl-gethash %hash "key") "value")',
             'Assign to hash element');

test_codegen('$arr[$i] = $x + 1',
             '(pl-setf (pl-aref @arr $i) (pl-+ $x 1))',
             'Assign expression to array element');

test_codegen('$hash{$key} = $val',
             '(pl-setf (pl-gethash %hash $key) $val)',
             'Assign with variable key');


# ============================================================
diag "";
diag "-------- Chained array/hash references:";

test_codegen('$ref->[0][1]',
             '(pl-aref (pl-aref-deref $ref 0) 1)',
             'Chained array refs');

test_codegen('$ref->{a}{b}',
             '(pl-gethash (pl-gethash-deref $ref "a") "b")',
             'Chained hash refs');

test_codegen('$ref->[0]{key}',
             '(pl-gethash (pl-aref-deref $ref 0) "key")',
             'Array ref then hash access');

test_codegen('$ref->{key}[0]',
             '(pl-aref (pl-gethash-deref $ref "key") 0)',
             'Hash ref then array access');

test_codegen('$data->[0][1][2]',
             '(pl-aref (pl-aref (pl-aref-deref $data 0) 1) 2)',
             'Triple nested array refs');

test_codegen('$config->{db}{host}{port}',
             '(pl-gethash (pl-gethash (pl-gethash-deref $config "db") "host") "port")',
             'Triple nested hash refs');


# ============================================================
diag "";
diag "-------- Assignment to dereferenced values:";

test_codegen('$ref->[0] = 5',
             '(pl-setf (pl-aref-deref $ref 0) 5)',
             'Assign to array ref element');

test_codegen('$ref->{key} = "value"',
             '(pl-setf (pl-gethash-deref $ref "key") "value")',
             'Assign to hash ref element');

test_codegen('$data->[0][1] = $x',
             '(pl-setf (pl-aref (pl-aref-deref $data 0) 1) $x)',
             'Assign to nested array ref');

test_codegen('$cfg->{a}{b} = 100',
             '(pl-setf (pl-gethash (pl-gethash-deref $cfg "a") "b") 100)',
             'Assign to nested hash ref');


# ============================================================
diag "";
diag "-------- Mixed complex expressions:";

test_codegen('$x = $arr[0] + $hash{key}',
             '(pl-scalar-= $x (pl-+ (pl-aref @arr 0) (pl-gethash %hash "key")))',
             'Sum of array and hash values');

test_codegen('$result = $obj->method($arr[0])',
             '(pl-scalar-= $result (pl-method-call $obj \'method (pl-aref @arr 0)))',
             'Method call with array element arg');

test_codegen('$total = $prices->[$i] * $qty->{$item}',
             '(pl-scalar-= $total (pl-* (pl-aref-deref $prices $i) (pl-gethash-deref $qty $item)))',
             'Multiply two dereferenced values');

test_codegen('$hash{$arr[0]} = $ref->{key}',
             '(pl-setf (pl-gethash %hash (pl-aref @arr 0)) (pl-gethash-deref $ref "key"))',
             'Complex LHS and RHS');

test_codegen('$x++ + $y--',
             '(pl-+ (pl-post++ $x) (pl-post-- $y))',
             'Post-increment and decrement in expression');

test_codegen('++$arr[0]',
             '(pl-pre++ (pl-aref-box @arr 0))',
             'Pre-increment array element (l-value context)');


# ============================================================
diag "";
diag "-------- Compound assignment operators:";

test_codegen('$x *= 2',
             '(pl-*= $x 2)',
             'Multiply-assign');

test_codegen('$x /= 2',
             '(pl-/= $x 2)',
             'Divide-assign');

test_codegen('$str .= "suffix"',
             '(pl-.= $str "suffix")',
             'Concat-assign');

test_codegen('$x ||= 10',
             '(pl-or-assign $x 10)',
             'Or-assign');

test_codegen('$x //= "default"',
             '(pl-//= $x "default")',
             'Defined-or-assign');

test_codegen('$arr[0] += 5',
             '(pl-incf (pl-aref @arr 0) 5)',
             'Add-assign to array element');


# ============================================================
diag "";
diag "-------- More precedence tests:";

test_codegen('$a && $b || $c',
             '(pl-|| (pl-&& $a $b) $c)',
             'AND has higher precedence than OR');

test_codegen('$a || $b && $c',
             '(pl-|| $a (pl-&& $b $c))',
             'AND binds tighter');

test_codegen('$a ? $b : $c ? $d : $e',
             '(pl-if $a $b (pl-if $c $d $e))',
             'Nested ternary (right-associative)');

test_codegen('$x = $a ? $b : $c',
             '(pl-scalar-= $x (pl-if $a $b $c))',
             'Assignment with ternary (assignment has lower precedence)');


# ============================================================
diag "";
diag "-------- Indentation tests:";

test_codegen('$x + $y',
             '  (pl-+ $x $y)',
             'Indent level 1 (2 spaces)', 1);

test_codegen('$arr[0] = 5',
             '    (pl-setf (pl-aref @arr 0) 5)',
             'Indent level 2 (4 spaces)', 2);

test_codegen('$x',
             '      $x',
             'Indent level 3 (6 spaces)', 3);


# ============================================================
diag "";
diag "-------- Print/say with filehandles:";

test_codegen('print "hello"',
             '(pl-print "hello")',
             'print without filehandle');

test_codegen('print STDERR "error"',
             "(pl-print :fh 'STDERR \"error\")",
             'print with STDERR filehandle');

test_codegen('print STDOUT "out"',
             "(pl-print :fh 'STDOUT \"out\")",
             'print with STDOUT filehandle');

test_codegen('print $fh "data"',
             '(pl-print :fh $fh "data")',
             'print with variable filehandle');

test_codegen('print STDERR "Some", "thing\n"',
             "(pl-print :fh 'STDERR \"Some\" \"thing\n\")",
             'print with filehandle and multiple args');

test_codegen('say "hello"',
             '(pl-say "hello")',
             'say without filehandle');

test_codegen('say STDERR $msg',
             "(pl-say :fh 'STDERR \$msg)",
             'say with filehandle');

test_codegen('print STDERR "a", $b, "c"',
             "(pl-print :fh 'STDERR \"a\" \$b \"c\")",
             'print with filehandle and mixed args');


diag "";
diag "-------- Regression tests (session 3):";

# Regression: &subname should generate function call
# Was outputting literal &foo which is invalid CL
test_codegen('&foo',
             '(pl-foo)',
             'Regression: &subname generates funcall');

# Regression: delete $a[idx] should use pl-delete-array
# Was passing value instead of array+index
test_codegen('delete $a[1]',
             '(pl-delete-array @a 1)',
             'Regression: delete $a[idx] uses array function');

# Hash delete passes hash and key separately (not the value)
test_codegen('delete $h{key}',
             '(pl-delete %h "key")',
             'Regression: delete $h{key} passes hash and key separately');

# exists $a[idx] should use pl-exists-array
test_codegen('exists $a[1]',
             '(pl-exists-array @a 1)',
             'Regression: exists $a[idx] uses array function');

# exists $h{key} should use pl-exists with hash and key
test_codegen('exists $h{key}',
             '(pl-exists %h "key")',
             'Regression: exists $h{key} passes hash and key separately');


diag "";
diag "-------- Regression tests (session 5):";

# Hash slice delete: delete @hash{keys} -> pl-delete-hash-slice with %hash
test_codegen('delete @foo{4,5}',
             '(pl-delete-hash-slice %foo 4 5)',
             'Regression: delete hash slice uses %hash and slice function');

# Array slice delete: delete @arr[indices] -> pl-delete-array-slice
test_codegen('delete @arr[1,2,3]',
             '(pl-delete-array-slice @arr 1 2 3)',
             'Regression: delete array slice uses slice function');

# Hash slice: @hash{keys} accesses %hash, not @hash
test_codegen('@hash{"a","b"}',
             '(pl-hslice %hash "a" "b")',
             'Regression: hash slice @h{} uses %hash sigil');

# Block filehandle syntax: print {$expr} LIST
test_codegen('print {$fh} "text"',
             '(pl-print :fh $fh "text")',
             'print with block filehandle - simple var');

test_codegen('print {STDERR} "text"',
             "(pl-print :fh 'STDERR \"text\")",
             'print with block filehandle - bareword');

test_codegen('print {$hash{key}} "text"',
             '(pl-print :fh (pl-gethash %hash "key") "text")',
             'print with block filehandle - hash access');

# Variable filehandle: $scalar followed by a term = filehandle
test_codegen('print $fh $data',
             '(pl-print :fh $fh $data)',
             'print with variable filehandle - scalar arg');

test_codegen('say $fh @arr',
             '(pl-say :fh $fh @arr)',
             'say with variable filehandle - array arg');

test_codegen('print $fh "hello"',
             '(pl-print :fh $fh "hello")',
             'print with variable filehandle - string arg');

test_codegen('printf $fh "%s", $x',
             '(pl-printf :fh $fh "%s" $x)',
             'printf with variable filehandle');

# NOT a filehandle: binary operator after $fh
test_codegen('print $fh . "bar"',
             '(pl-print (pl-. $fh "bar"))',
             'print NOT filehandle - concat operator');

test_codegen('print $fh + 1',
             '(pl-print (pl-+ $fh 1))',
             'print NOT filehandle - addition');

# NOT a filehandle: comma after $fh
test_codegen('print $fh, "hello"',
             '(pl-print $fh "hello")',
             'print NOT filehandle - comma separates args');

# NOT a filehandle: nothing after $fh
test_codegen('print $fh',
             '(pl-print $fh)',
             'print NOT filehandle - no args after');


diag "";
diag "-------- KV slice:";

test_codegen('my @kv = %h{"a","b"}',
             '(pl-array-= @kv (pl-kv-hslice %h "a" "b"))',
             'KV hash slice %h{keys}');

test_codegen('delete %h{"a","c"}',
             '(pl-delete-kv-hash-slice %h "a" "c")',
             'delete KV hash slice');

diag "";
diag "-------- split scalar context:";

test_codegen('my $n = split(/,/, $str)',
             '(pl-scalar-= $n (length (pl-split (pl-regex "/,/") $str)))',
             'split in scalar context returns length');

diag "";
diag "-------- q{} and qq{} string quoting:";

test_codegen('q{hello world}',
             '"hello world"',
             'q{} literal string');

test_codegen('q(paren text)',
             '"paren text"',
             'q() literal string');

test_codegen('q[bracket text]',
             '"bracket text"',
             'q[] literal string');

test_codegen('q/slash text/',
             '"slash text"',
             'q// literal string');

test_codegen('qq{hello $name}',
             '(pl-string-concat "hello " $name)',
             'qq{} interpolated string');

test_codegen('qq/hello $name/',
             '(pl-string-concat "hello " $name)',
             'qq// interpolated string');

# $#array lvalue
test_codegen('--$#ary',
             '(pl-set-array-length @ary (1- (pl-array-last-index @ary)))',
             '$#array lvalue - pre-decrement');

test_codegen('++$#ary',
             '(pl-set-array-length @ary (1+ (pl-array-last-index @ary)))',
             '$#array lvalue - pre-increment');

test_codegen('$#ary = 5',
             '(pl-set-array-length @ary 5)',
             '$#array lvalue - assignment');

test_codegen('$#ary++',
             '(let ((_prev (pl-array-last-index @ary))) (pl-set-array-length @ary (1+ _prev)) _prev)',
             '$#array lvalue - post-increment');

diag "";
diag "-------- Unary minus on bareword:";

test_codegen('-bareword',
             '"-bareword"',
             'unary minus on bareword produces string');

test_codegen('-SomeWord',
             '"-SomeWord"',
             'unary minus on capitalized bareword produces string');

# --- CL reader safety: pipe-quoting special variables ---
test_codegen('$| = 1',
             '(pl-setf |$\\|| 1)',
             '$| (autoflush) is pipe-quoted for CL reader safety');

test_codegen('$; eq "x"',
             '(pl-str-eq |$;| "x")',
             '$; (subscript separator) is pipe-quoted');

test_codegen('$, = ","',
             '(pl-setf |$,| ",")',
             '$, (output field separator) is pipe-quoted');

test_codegen('$^X',
             '|$^X|',
             '$^X (Perl executable path) is pipe-quoted');

# Negative hex/binary/octal literals
test_codegen('-0x80001218',
             '(- #x80001218)',
             'negative hex literal emits (- #x...)');

test_codegen('-0b1010',
             '(- #b1010)',
             'negative binary literal emits (- #b...)');

test_codegen('-0777',
             '(- #o777)',
             'negative octal literal emits (- #o...)');

# Version strings
test_codegen('v1.20.300',
             '(pl-version-string 1 20 300)',
             'version string v1.20.300 emits pl-version-string call');

# $] special variable
test_codegen('$]',
             '|$]|',
             '$] (Perl version) is pipe-quoted');

# Positive hex still works
test_codegen('0xFF',
             '#xFF',
             'positive hex literal still works');

test_codegen('0b1010',
             '#b1010',
             'positive binary literal still works');

diag "";
diag "All tests completed!";
