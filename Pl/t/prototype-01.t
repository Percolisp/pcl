#          -*-Mode: CPerl -*-

# Test prototype extraction and & (block argument) handling

use v5.32;
use strict;
use warnings;

use lib ".";

use PPI;
use Test::More;

BEGIN { use_ok('Pl::Parser') };
BEGIN { use_ok('Pl::Environment') };
BEGIN { use_ok('Pl::PExpr') };
BEGIN { use_ok('Pl::ExprToCL') };


# ============================================================
diag "";
diag "-------- Old-style Prototype Extraction:";

# Helper to parse a sub and extract prototype info
sub get_prototype_info {
  my $code = shift;

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  $parser->parse();

  return $env;
}

# Test basic prototypes
{
  my $env = get_prototype_info('sub foo ($$) { }');
  my $proto = $env->get_prototype('foo');
  ok($proto, 'foo prototype extracted');
  is($proto->{is_proto}, 1, 'foo is old-style prototype');
  is($proto->{min_params}, 2, 'foo has 2 required params');
  ok(!$proto->{has_block_arg}, 'foo has no block arg');
}

{
  my $env = get_prototype_info('sub bar (&) { }');
  my $proto = $env->get_prototype('bar');
  ok($proto, 'bar prototype extracted');
  is($proto->{is_proto}, 1, 'bar is old-style prototype');
  ok($proto->{has_block_arg}, 'bar has block arg');
}

{
  my $env = get_prototype_info('sub baz (&;@) { }');
  my $proto = $env->get_prototype('baz');
  ok($proto, 'baz prototype extracted');
  ok($proto->{has_block_arg}, 'baz has block arg (with optional list)');
  is($proto->{proto_string}, '&;@', 'baz proto_string preserved');
}

{
  my $env = get_prototype_info('sub quux ($&) { }');
  my $proto = $env->get_prototype('quux');
  ok($proto, 'quux prototype extracted');
  ok($proto->{has_block_arg}, 'quux has block arg (not first)');
}


# ============================================================
diag "";
diag "-------- Prototype from use'd Module:";

# Create a test module with prototyped subs
my $test_module_dir = "/tmp/pcl_proto_test_$$";
mkdir $test_module_dir;

# Write a module with a prototyped sub
{
  open my $fh, '>', "$test_module_dir/ProtoTest.pm" or die $!;
  print $fh <<'END_MODULE';
package ProtoTest;
use strict;
use warnings;

sub try_it (&;@) {
    my ($block, @rest) = @_;
    return $block->(@rest);
}

sub normal_sub {
    return 42;
}

1;
END_MODULE
  close $fh;
}

# Test that we can extract prototypes from the module
{
  my $code = qq{
    use lib "$test_module_dir";
    use ProtoTest;
    try_it { print "hello" };
  };

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  eval { $parser->parse() };
  ok(!$@, 'Parser handles use with prototyped module') or diag $@;

  # Verify prototype was imported
  my $proto = $env->get_prototype('try_it');
  ok($proto, 'try_it prototype imported from module');
  ok($proto && $proto->{has_block_arg}, 'try_it has block arg');
  is($proto && $proto->{proto_string}, '&;@', 'try_it proto_string correct');
}


# ============================================================
diag "";
diag "-------- Block-to-Sub Conversion with & Prototype:";

# Helper to transpile and check output
sub transpile {
  my $code = shift;

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  $parser->parse();

  return join("\n", @{$parser->output});
}

# Test that sub with & prototype can be called with block
{
  my $cl = transpile(<<'END_PERL');
sub wrapper (&) {
    my $block = shift;
    $block->();
}

wrapper { print "inside"; };
END_PERL

  # The call 'wrapper { print "inside" }' should be converted to
  # 'wrapper(sub { print "inside" })' internally
  like($cl, qr/wrapper/, 'wrapper call present');
  # For now, just check it transpiles without error
  ok(defined $cl, 'Code with & prototype transpiles');
}

# Test that module-imported prototype triggers block conversion
{
  my $code = qq{
    use lib "$test_module_dir";
    use ProtoTest;
    my \$result = try_it { return 42; };
  };

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  $parser->parse();
  my $cl = join("\n", @{$parser->output});

  # Verify try_it is called as a function (not method call on block)
  like($cl, qr/try_it|try-it/i, 'try_it function call present');
  # Should have a function reference or lambda for the block
  like($cl, qr/lambda|defun|func[_-]ref/i, 'block converted to function');
}


# ============================================================
diag "";
diag "-------- Multiple & Arguments and Edge Cases:";

# Test multiple blocks in prototype (&;&)
{
  my $env = get_prototype_info('sub double_block (&;&) { }');
  my $proto = $env->get_prototype('double_block');
  ok($proto, 'double_block prototype extracted');
  ok($proto->{has_block_arg}, 'double_block has block arg');
  is($proto->{proto_string}, '&;&', 'double_block proto_string correct');
}

# Test & at end of prototype
{
  my $env = get_prototype_info('sub block_last ($$&) { }');
  my $proto = $env->get_prototype('block_last');
  ok($proto, 'block_last prototype extracted');
  ok($proto->{has_block_arg}, 'block_last has block arg (at end)');
}

# Test & mixed with other types
{
  my $env = get_prototype_info('sub mixed_proto (&$@) { }');
  my $proto = $env->get_prototype('mixed_proto');
  ok($proto, 'mixed_proto extracted');
  ok($proto->{has_block_arg}, 'mixed_proto has block arg');
  is($proto->{min_params}, 2, 'mixed_proto has 2 required params (& and $)');
}

# Test that new-style signatures don't have has_block_arg
{
  my $env = get_prototype_info('sub new_style ($x, $y) { }');
  my $proto = $env->get_prototype('new_style');
  ok($proto, 'new_style prototype extracted');
  ok(!$proto->{has_block_arg}, 'new_style signature has no block arg');
  ok(!$proto->{is_proto}, 'new_style is not old-style prototype');
}


# ============================================================
diag "";
diag "-------- Chained Prototype Calls:";

# Test that block is properly parsed in assignment context
{
  my $cl = transpile(<<'END_PERL');
sub execute (&) {
    my $block = shift;
    return $block->();
}

my $result = execute { return 42; };
END_PERL

  like($cl, qr/execute/, 'execute call present');
  like($cl, qr/\$result/, 'result variable present');
  ok(defined $cl, 'Chained prototype call transpiles');
}


# ============================================================
diag "";
diag "-------- Recursive Module Import:";

# Create a module that uses another module with prototypes
{
  open my $fh, '>', "$test_module_dir/ProtoHelper.pm" or die $!;
  print $fh <<'END_MODULE';
package ProtoHelper;
use strict;
use warnings;

sub helper_block (&) {
    my $block = shift;
    return $block->();
}

1;
END_MODULE
  close $fh;
}

{
  open my $fh, '>', "$test_module_dir/ProtoUser.pm" or die $!;
  print $fh <<'END_MODULE';
package ProtoUser;
use strict;
use warnings;
use ProtoHelper;

sub user_block (&;@) {
    my ($block, @rest) = @_;
    return helper_block { $block->(@rest) };
}

1;
END_MODULE
  close $fh;
}

# Test that recursive module imports work
{
  my $code = qq{
    use lib "$test_module_dir";
    use ProtoUser;
    my \$x = user_block { return 1; };
  };

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  eval { $parser->parse() };
  ok(!$@, 'Recursive module import works') or diag $@;

  my $proto = $env->get_prototype('user_block');
  ok($proto, 'user_block prototype imported from nested module');
  ok($proto && $proto->{has_block_arg}, 'user_block has block arg');
}


# ============================================================
diag "";
diag "-------- Complex Prototype Patterns:";

# Test all reference types in one prototype
{
  my $env = get_prototype_info('sub all_refs(\$\@\%) { }');
  my $proto = $env->get_prototype('all_refs');
  ok($proto, 'all_refs prototype extracted');
  is($proto->{proto_string}, '\$\@\%', 'all_refs proto_string preserved');
  is(scalar @{$proto->{params}}, 3, 'all_refs has 3 params');
  is($proto->{params}[0]{name}, '\$', 'first param is \$');
  is($proto->{params}[1]{name}, '\@', 'second param is \@');
  is($proto->{params}[2]{name}, '\%', 'third param is \%');
}

# Test optional reference params
{
  my $env = get_prototype_info('sub opt_ref(\@;\%) { }');
  my $proto = $env->get_prototype('opt_ref');
  ok($proto, 'opt_ref prototype extracted');
  is($proto->{min_params}, 1, 'opt_ref has 1 required param (before ;)');
  is($proto->{proto_string}, '\@;\%', 'opt_ref proto_string preserved');
}

# Test glob reference \*
{
  my $env = get_prototype_info('sub glob_ref(\*) { }');
  my $proto = $env->get_prototype('glob_ref');
  ok($proto, 'glob_ref prototype extracted');
  is($proto->{params}[0]{name}, '\*', 'glob_ref has \* param');
}

# Test complex mixed prototype
{
  my $env = get_prototype_info('sub complex($\@$$\%;@) { }');
  my $proto = $env->get_prototype('complex');
  ok($proto, 'complex prototype extracted');
  is($proto->{min_params}, 5, 'complex has 5 required params');
  is($proto->{params}[0]{name}, '$', 'param 0 is $');
  is($proto->{params}[1]{name}, '\@', 'param 1 is \@');
  is($proto->{params}[2]{name}, '$', 'param 2 is $');
  is($proto->{params}[3]{name}, '$', 'param 3 is $');
  is($proto->{params}[4]{name}, '\%', 'param 4 is \%');
}

# Test underscore prototype (_)
{
  my $env = get_prototype_info('sub with_underscore(_) { }');
  my $proto = $env->get_prototype('with_underscore');
  ok($proto, 'with_underscore prototype extracted');
  is($proto->{params}[0]{name}, '_', 'underscore param recognized');
}

# Test empty prototype ()
{
  my $env = get_prototype_info('sub no_args() { }');
  my $proto = $env->get_prototype('no_args');
  ok($proto, 'no_args prototype extracted');
  is($proto->{min_params}, 0, 'no_args has 0 params');
  is(scalar @{$proto->{params}}, 0, 'no_args params array is empty');
}


# ============================================================
diag "";
diag "-------- Module Import with Reference Prototypes:";

# Create a module with reference prototypes
{
  open my $fh, '>', "$test_module_dir/RefProto.pm" or die $!;
  print $fh <<'END_MODULE';
package RefProto;
use strict;
use warnings;

sub modify_array(\@) {
    my $arr_ref = shift;
    push @$arr_ref, 'added';
    return scalar @$arr_ref;
}

sub modify_hash(\%) {
    my $hash_ref = shift;
    $hash_ref->{modified} = 1;
    return keys %$hash_ref;
}

sub modify_scalar(\$) {
    my $scalar_ref = shift;
    $$scalar_ref .= '_modified';
    return $$scalar_ref;
}

sub mixed_refs($\@\%$) {
    my ($prefix, $arr_ref, $hash_ref, $suffix) = @_;
    return $prefix . scalar(@$arr_ref) . scalar(keys %$hash_ref) . $suffix;
}

1;
END_MODULE
  close $fh;
}

# Test that reference prototypes are imported from module
{
  my $code = qq{
    use lib "$test_module_dir";
    use RefProto;
  };

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  eval { $parser->parse() };
  ok(!$@, 'RefProto module loads without error') or diag $@;

  # Check modify_array prototype
  my $proto = $env->get_prototype('modify_array');
  ok($proto, 'modify_array prototype imported');
  is($proto->{proto_string}, '\@', 'modify_array has \@ prototype');
  ok($proto->{params}[0]{name} eq '\@', 'modify_array param is \@');

  # Check modify_hash prototype
  $proto = $env->get_prototype('modify_hash');
  ok($proto, 'modify_hash prototype imported');
  is($proto->{proto_string}, '\%', 'modify_hash has \% prototype');

  # Check modify_scalar prototype
  $proto = $env->get_prototype('modify_scalar');
  ok($proto, 'modify_scalar prototype imported');
  is($proto->{proto_string}, '\$', 'modify_scalar has \$ prototype');

  # Check mixed_refs prototype
  $proto = $env->get_prototype('mixed_refs');
  ok($proto, 'mixed_refs prototype imported');
  is($proto->{proto_string}, '$\@\%$', 'mixed_refs has complex prototype');
  is($proto->{min_params}, 4, 'mixed_refs has 4 required params');
}

# Test that auto-boxing works with imported prototypes
{
  my $code = qq{
    use lib "$test_module_dir";
    use RefProto;
    my \@arr = (1, 2, 3);
    my \%hash = (a => 1);
    my \$scalar = "test";
    modify_array(\@arr);
    modify_hash(\%hash);
    modify_scalar(\$scalar);
  };

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  $parser->parse();
  my $cl = join("\n", @{$parser->output});

  like($cl, qr/pl-modify_array \(pl-backslash \@arr\)/,
       'modify_array call auto-boxes array from imported module');
  like($cl, qr/pl-modify_hash \(pl-backslash %hash\)/,
       'modify_hash call auto-boxes hash from imported module');
  like($cl, qr/pl-modify_scalar \(pl-backslash \$scalar\)/,
       'modify_scalar call auto-boxes scalar from imported module');
}

# Test mixed_refs with imported prototype
{
  my $code = qq{
    use lib "$test_module_dir";
    use RefProto;
    my \@data = (1, 2);
    my \%info = (x => 1);
    mixed_refs("pre", \@data, \%info, "post");
  };

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  $parser->parse();
  my $cl = join("\n", @{$parser->output});

  like($cl, qr/pl-mixed_refs "pre" \(pl-backslash \@data\) \(pl-backslash %info\) "post"/,
       'mixed_refs auto-boxes only reference params (positions 2 and 3)');
}


# ============================================================
diag "";
diag "-------- Prototype Override and Shadowing:";

# Test that local sub definition overrides imported prototype
{
  my $code = qq{
    use lib "$test_module_dir";
    use RefProto;

    # Local definition with different prototype
    sub modify_array(\\\$) {
        my \$ref = shift;
        return \$\$ref;
    }

    my \$s = "test";
    modify_array(\$s);
  };

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  $parser->parse();
  my $cl = join("\n", @{$parser->output});

  # The local definition should take precedence
  my $proto = $env->get_prototype('modify_array');
  is($proto->{proto_string}, '\$', 'Local prototype overrides imported');

  # Call should auto-box scalar, not array
  like($cl, qr/pl-modify_array \(pl-backslash \$s\)/,
       'Call uses local prototype, not imported');
}


# ============================================================
diag "";
diag "-------- Edge Cases with Prototypes:";

# Test calling function before definition (prototype not yet known)
{
  my $cl = transpile(<<'END_PERL');
# Call before definition - no auto-boxing expected
early_call(@arr);

sub early_call(\@) {
    my $ref = shift;
}
END_PERL

  # When called before definition, prototype isn't known yet
  # so no auto-boxing should happen
  unlike($cl, qr/early_call \(pl-backslash/,
         'Call before prototype definition does not auto-box');
}

# Test that non-symbol arguments are not auto-boxed
{
  my $cl = transpile(<<'END_PERL');
sub takes_array_ref(\@) { }
takes_array_ref([1, 2, 3]);  # Anonymous array ref, not @array
END_PERL

  # Anonymous array ref should not be wrapped
  unlike($cl, qr/pl-backslash \(vector/,
         'Anonymous array ref is not auto-boxed');
}

# Test function call result as argument (not auto-boxed)
{
  my $cl = transpile(<<'END_PERL');
sub takes_scalar_ref(\$) { }
sub get_scalar { return "value"; }
takes_scalar_ref(get_scalar());
END_PERL

  # Function call result should not be auto-boxed
  unlike($cl, qr/pl-backslash \(pl-get_scalar/,
         'Function call result is not auto-boxed');
}


# ============================================================
diag "";
diag "-------- Reference Prototype Auto-boxing:";

# Test \@ prototype auto-boxes array argument
{
  my $cl = transpile(<<'END_PERL');
sub ref_array(\@) {
    my $arr_ref = shift;
    print @$arr_ref;
}
my @arr = (1, 2, 3);
ref_array(@arr);
END_PERL

  like($cl, qr/pl-backslash \@arr/, '\@ prototype auto-boxes array arg');
}

# Test \% prototype auto-boxes hash argument
{
  my $cl = transpile(<<'END_PERL');
sub ref_hash(\%) {
    my $hash_ref = shift;
}
my %h = (a => 1);
ref_hash(%h);
END_PERL

  like($cl, qr/pl-backslash %h/, '\% prototype auto-boxes hash arg');
}

# Test \$ prototype auto-boxes scalar argument
{
  my $cl = transpile(<<'END_PERL');
sub ref_scalar(\$) {
    my $scalar_ref = shift;
}
my $s = "test";
ref_scalar($s);
END_PERL

  like($cl, qr/pl-backslash \$s/, '\$ prototype auto-boxes scalar arg');
}

# Test mixed prototype with reference
{
  my $cl = transpile(<<'END_PERL');
sub mixed_ref($\@$) {
    my ($x, $arr_ref, $y) = @_;
}
my @data = (1, 2);
mixed_ref(1, @data, 2);
END_PERL

  # Only @data (second arg) should be auto-boxed
  like($cl, qr/pl-mixed_ref 1 \(pl-backslash \@data\) 2/,
       'Mixed prototype auto-boxes only the reference param');
}

# Test that explicit reference is not double-wrapped
{
  my $cl = transpile(<<'END_PERL');
sub ref_array(\@) { }
my @arr = (1, 2);
ref_array(\@arr);
END_PERL

  # When explicitly passing \@arr, it should not be wrapped again
  # (The backslash is part of the call, not auto-added)
  unlike($cl, qr/pl-backslash \(pl-backslash/,
         'Explicit reference is not double-wrapped');
}


# ============================================================
diag "";
diag "-------- Cleanup:";

# Cleanup extra test modules
unlink "$test_module_dir/ProtoHelper.pm";
unlink "$test_module_dir/ProtoUser.pm";
unlink "$test_module_dir/RefProto.pm";

# Cleanup test module
unlink "$test_module_dir/ProtoTest.pm";
rmdir $test_module_dir;
ok(1, 'Cleanup done');


done_testing();
