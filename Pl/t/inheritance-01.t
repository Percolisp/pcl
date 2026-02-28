#          -*-Mode: CPerl -*-

# Test OO inheritance using @ISA and SUPER::

use v5.32;
use strict;
use warnings;

use lib ".";

use PPI;
use Test::More;

BEGIN { use_ok('Pl::Parser') };
BEGIN { use_ok('Pl::Environment') };
BEGIN { use_ok('Pl::ExprToCL') };


# ============================================================
diag "";
diag '-------- Basic @ISA Parsing:';

# Helper to parse and get output
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

# Test @ISA with qw()
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = qw(Parent);
END_PERL

  like($cl, qr/defclass child \(parent\)/, '@ISA qw() generates CLOS class with parent');
  like($cl, qr/defvar \@ISA/, '@ISA array is still created');
  like($cl, qr/pl-push \@ISA "Parent"/, 'Parent pushed to @ISA array');
}

# Test @ISA with list syntax
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = ('Parent1', 'Parent2');
END_PERL

  like($cl, qr/defclass child \(parent1 parent2\)/, '@ISA list generates CLOS class with parents');
}

# Test nested package names
{
  my $cl = transpile(<<'END_PERL');
package My::Child;
our @ISA = qw(My::Parent);
END_PERL

  like($cl, qr/defclass my-child \(my-parent\)/, 'Nested package names converted to CLOS class names');
}


# ============================================================
diag "";
diag "-------- Package with CLOS Class:";

# Test that package generates CLOS class
{
  my $cl = transpile(<<'END_PERL');
package Animal;
sub speak { "generic sound" }
END_PERL

  like($cl, qr/defclass animal \(\) \(\)/, 'Package generates empty CLOS class');
}


# ============================================================
diag "";
diag "-------- SUPER:: Method Calls:";

# Test SUPER:: method call generation
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = qw(Parent);

sub greet {
    my $self = shift;
    return $self->SUPER::greet() . " and Child";
}
END_PERL

  like($cl, qr/pl-super-call/, 'SUPER:: generates pl-super-call');
  like($cl, qr/pl-super-call .* 'greet "Child"/, 'SUPER:: call includes current package name');
}

# Test SUPER:: with arguments
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = qw(Parent);

sub process {
    my ($self, $x, $y) = @_;
    return $self->SUPER::process($x, $y);
}
END_PERL

  like($cl, qr/pl-super-call .* 'process "Child"/, 'SUPER:: with args generates correct call');
}


# ============================================================
diag "";
diag "-------- Multiple Inheritance:";

# Test multiple parents
{
  my $cl = transpile(<<'END_PERL');
package Bat;
our @ISA = qw(Mammal Flyer);
END_PERL

  like($cl, qr/defclass bat \(mammal flyer\)/, 'Multiple inheritance generates CLOS class with multiple parents');
}


# ============================================================
diag "";
diag "-------- Method Call (Basic):";

# Test regular method calls still work
{
  my $cl = transpile(<<'END_PERL');
my $obj = Dog->new();
$obj->speak();
END_PERL

  like($cl, qr/pl-method-call/, 'Regular method call generates pl-method-call');
  unlike($cl, qr/pl-super-call/, 'Regular method call does not generate pl-super-call');
}


# ============================================================
diag "";
diag "-------- Environment Tracking:";

# Test that @ISA is tracked in environment
{
  my $code = <<'END_PERL';
package Child;
our @ISA = qw(Parent1 Parent2);
END_PERL

  my $env = Pl::Environment->new();
  my $parser = Pl::Parser->new(
    code        => $code,
    environment => $env,
  );

  $parser->parse();

  my $isa = $env->get_isa('Child');
  is_deeply($isa, ['Parent1', 'Parent2'], '@ISA tracked in environment');
}


# ============================================================
diag "";
diag "-------- Complex Inheritance Hierarchy:";

# Test multi-level inheritance hierarchy
{
  my $cl = transpile(<<'END_PERL');
package Animal;
sub new {
    my $class = shift;
    return bless {}, $class;
}
sub speak { "..." }

package Mammal;
our @ISA = qw(Animal);
sub speak { "mammal sound" }

package Dog;
our @ISA = qw(Mammal);
sub speak { "woof" }

package main;
my $dog = Dog->new();
END_PERL

  # Check that each class is generated
  like($cl, qr/defclass animal \(\) \(\)/, 'Animal CLOS class generated');
  like($cl, qr/defclass mammal \(animal\)/, 'Mammal inherits from Animal');
  like($cl, qr/defclass dog \(mammal\)/, 'Dog inherits from Mammal');
}


# ============================================================
diag "";
diag "-------- Diamond Inheritance:";

# Test diamond inheritance pattern
{
  my $cl = transpile(<<'END_PERL');
package Animal;
sub desc { "animal" }

package Mammal;
our @ISA = qw(Animal);

package Flyer;
our @ISA = qw(Animal);

package Bat;
our @ISA = qw(Mammal Flyer);
END_PERL

  like($cl, qr/defclass animal \(\)/, 'Animal base class');
  like($cl, qr/defclass mammal \(animal\)/, 'Mammal inherits Animal');
  like($cl, qr/defclass flyer \(animal\)/, 'Flyer inherits Animal');
  like($cl, qr/defclass bat \(mammal flyer\)/, 'Bat has multiple parents (diamond)');
}


# ============================================================
diag "";
diag "-------- Override and SUPER:: Combined:";

# Test override with SUPER:: call
{
  my $cl = transpile(<<'END_PERL');
package Parent;
sub greet { "Hello from Parent" }

package Child;
our @ISA = qw(Parent);

sub greet {
    my $self = shift;
    my $parent_msg = $self->SUPER::greet();
    return $parent_msg . " and Child";
}

package main;
my $c = Child->new();
my $msg = $c->greet();
END_PERL

  # Check SUPER:: call is generated
  like($cl, qr/pl-super-call/, 'SUPER:: in override generates pl-super-call');
  # Check it references the correct method
  like($cl, qr/pl-super-call .* 'greet/, 'SUPER::greet references greet method');
}


# ============================================================
diag "";
diag "-------- Edge Cases:";

# Test empty @ISA
{
  my $cl = transpile(<<'END_PERL');
package Standalone;
our @ISA = ();
END_PERL

  # Should still emit the @ISA initialization
  like($cl, qr/defvar \@ISA/, 'Empty @ISA still creates array');
}

# Test @ISA with single quoted strings
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = ('Parent');
END_PERL

  like($cl, qr/defclass child \(parent\)/, 'Single-quoted @ISA works');
}


# ============================================================
diag "";
diag "-------- SUPER:: in Nested Packages:";

# Test SUPER:: in deeply nested package names
{
  my $cl = transpile(<<'END_PERL');
package My::Deep::Child;
our @ISA = qw(My::Deep::Parent);

sub process {
    my $self = shift;
    return $self->SUPER::process();
}
END_PERL

  like($cl, qr/defclass my-deep-child \(my-deep-parent\)/, 'Nested package inheritance');
  like($cl, qr/pl-super-call .* 'process "My::Deep::Child"/, 'SUPER:: in nested package uses full package name');
}


# ============================================================
diag "";
diag "-------- Multiple SUPER:: Calls:";

# Test multiple SUPER:: calls in the same method
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = qw(Parent);

sub combined {
    my $self = shift;
    my $a = $self->SUPER::method_a();
    my $b = $self->SUPER::method_b();
    return $a . $b;
}
END_PERL

  # Should have two SUPER:: calls
  my @matches = ($cl =~ /pl-super-call/g);
  is(scalar @matches, 2, 'Multiple SUPER:: calls in one method');
  like($cl, qr/pl-super-call .* 'method_a/, 'First SUPER:: call to method_a');
  like($cl, qr/pl-super-call .* 'method_b/, 'Second SUPER:: call to method_b');
}


# ============================================================
diag "";
diag "-------- Constructor Inheritance Pattern:";

# Test constructor inheritance (Child using Parent's new)
{
  my $cl = transpile(<<'END_PERL');
package Parent;
sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->{created_by} = 'Parent';
    return $self;
}

package Child;
our @ISA = qw(Parent);
# No new() defined - inherits from Parent

sub child_method {
    my $self = shift;
    return "child: " . $self->{created_by};
}

package main;
my $c = Child->new();
END_PERL

  # Child should not have its own new, but call should work via inheritance
  like($cl, qr/defclass child \(parent\)/, 'Child inherits from Parent');
  # The method call Child->new() should use pl-method-call
  like($cl, qr/pl-method-call.*"Child".*'new/s, 'Child->new() generates method call');
}


# ============================================================
diag "";
diag "-------- SUPER:: with Constructor Override:";

# Test constructor override calling SUPER::new
{
  my $cl = transpile(<<'END_PERL');
package Parent;
sub new {
    my $class = shift;
    return bless { parent_init => 1 }, $class;
}

package Child;
our @ISA = qw(Parent);

sub new {
    my $class = shift;
    my $self = $class->SUPER::new();
    $self->{child_init} = 1;
    return $self;
}
END_PERL

  like($cl, qr/pl-super-call .* 'new "Child"/, 'SUPER::new in constructor override');
}


# ============================================================
diag "";
diag "-------- Three-Level Inheritance Chain:";

# Test SUPER:: at each level of a three-level hierarchy
{
  my $cl = transpile(<<'END_PERL');
package GrandParent;
sub greet { "GrandParent" }

package Parent;
our @ISA = qw(GrandParent);
sub greet {
    my $self = shift;
    return $self->SUPER::greet() . "->Parent";
}

package Child;
our @ISA = qw(Parent);
sub greet {
    my $self = shift;
    return $self->SUPER::greet() . "->Child";
}
END_PERL

  like($cl, qr/defclass grandparent \(\)/, 'GrandParent class');
  like($cl, qr/defclass parent \(grandparent\)/, 'Parent inherits GrandParent');
  like($cl, qr/defclass child \(parent\)/, 'Child inherits Parent');

  # Check SUPER:: calls reference correct packages
  like($cl, qr/pl-super-call .* 'greet "Parent"/, 'Parent SUPER:: uses "Parent"');
  like($cl, qr/pl-super-call .* 'greet "Child"/, 'Child SUPER:: uses "Child"');
}


# ============================================================
diag "";
diag "-------- MRO Order (First Parent Wins):";

# Test that first parent in @ISA is preferred
{
  my $cl = transpile(<<'END_PERL');
package Left;
sub method { "Left" }

package Right;
sub method { "Right" }

package Child;
our @ISA = qw(Left Right);
# No method override - should inherit from Left first
END_PERL

  # CLOS will use C3 MRO, with Left before Right
  like($cl, qr/defclass child \(left right\)/, 'Child MRO: Left before Right');
}


# ============================================================
diag "";
diag "-------- Class Method vs Instance Method:";

# Test class methods with inheritance
{
  my $cl = transpile(<<'END_PERL');
package Parent;
sub class_method {
    my $class = shift;
    return "class: $class";
}

sub instance_method {
    my $self = shift;
    return "instance";
}

package Child;
our @ISA = qw(Parent);

package main;
Child->class_method();
my $obj = Child->new();
$obj->instance_method();
END_PERL

  # Both should generate pl-method-call
  like($cl, qr/pl-method-call.*"Child".*'class_method/s, 'Class method call on Child');
  like($cl, qr/pl-method-call.*\$obj.*'instance_method/s, 'Instance method call');
}


# ============================================================
diag "";
diag "-------- Chained Method Calls with Inheritance:";

# Test method chaining where methods return $self
{
  my $cl = transpile(<<'END_PERL');
package Builder;
sub new {
    my $class = shift;
    return bless { data => [] }, $class;
}
sub add {
    my ($self, $item) = @_;
    push @{$self->{data}}, $item;
    return $self;
}

package ExtendedBuilder;
our @ISA = qw(Builder);
sub add_twice {
    my ($self, $item) = @_;
    $self->add($item)->add($item);
    return $self;
}

package main;
my $b = ExtendedBuilder->new()->add("x")->add_twice("y");
END_PERL

  # Check chained method calls
  like($cl, qr/pl-method-call.*pl-method-call/s, 'Chained method calls present');
  like($cl, qr/defclass extendedbuilder \(builder\)/, 'ExtendedBuilder inherits Builder');
}


# ============================================================
diag "";
diag "-------- SUPER:: with Various Argument Patterns:";

# Test SUPER:: with different argument patterns
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = qw(Parent);

sub no_args {
    my $self = shift;
    return $self->SUPER::no_args();
}

sub with_scalar {
    my ($self, $x) = @_;
    return $self->SUPER::with_scalar($x);
}

sub with_list {
    my ($self, @items) = @_;
    return $self->SUPER::with_list(@items);
}

sub with_hash {
    my ($self, %opts) = @_;
    return $self->SUPER::with_hash(%opts);
}

sub pass_through {
    my $self = shift;
    return $self->SUPER::pass_through(@_);
}
END_PERL

  like($cl, qr/pl-super-call .* 'no_args "Child"\)/, 'SUPER:: with no args');
  like($cl, qr/pl-super-call .* 'with_scalar "Child" \$x/, 'SUPER:: with scalar arg');
  like($cl, qr/pl-super-call .* 'with_list "Child" \@items/, 'SUPER:: with list arg');
  like($cl, qr/pl-super-call .* 'with_hash "Child" %opts/, 'SUPER:: with hash arg');
  like($cl, qr/pl-super-call .* 'pass_through "Child" \@_/, 'SUPER:: with @_ pass-through');
}


# ============================================================
diag "";
diag "-------- Multiple Packages with Different Inheritance:";

# Test multiple unrelated inheritance trees in one file
{
  my $cl = transpile(<<'END_PERL');
# Tree 1
package Animal;
sub speak { "..." }

package Dog;
our @ISA = qw(Animal);
sub speak { "woof" }

# Tree 2 (unrelated)
package Vehicle;
sub move { "..." }

package Car;
our @ISA = qw(Vehicle);
sub move { "drive" }

# Tree 3 (shares nothing)
package Standalone;
sub action { "solo" }
END_PERL

  # Check all classes are generated correctly
  like($cl, qr/defclass animal \(\)/, 'Animal (tree 1)');
  like($cl, qr/defclass dog \(animal\)/, 'Dog inherits Animal');
  like($cl, qr/defclass vehicle \(\)/, 'Vehicle (tree 2)');
  like($cl, qr/defclass car \(vehicle\)/, 'Car inherits Vehicle');
  like($cl, qr/defclass standalone \(\)/, 'Standalone (no inheritance)');
}


# ============================================================
diag "";
diag '-------- Package Without Explicit @ISA:';

# Test that package without @ISA gets empty CLOS class
{
  my $cl = transpile(<<'END_PERL');
package NoParent;
sub method { "I have no parent" }
END_PERL

  like($cl, qr/defclass noparent \(\) \(\)/, 'Package without @ISA gets empty parent list');
}


# ============================================================
diag "";
diag "-------- Accessor Pattern with Inheritance:";

# Test typical accessor/mutator pattern with inheritance
{
  my $cl = transpile(<<'END_PERL');
package Base;
sub new {
    my ($class, %args) = @_;
    return bless \%args, $class;
}

sub name {
    my $self = shift;
    if (@_) {
        $self->{name} = shift;
        return $self;
    }
    return $self->{name};
}

package Extended;
our @ISA = qw(Base);

sub name {
    my $self = shift;
    if (@_) {
        my $val = shift;
        return $self->SUPER::name(uc($val));
    }
    return $self->SUPER::name();
}
END_PERL

  like($cl, qr/defclass extended \(base\)/, 'Extended inherits Base');
  # Two SUPER::name calls (getter and setter paths)
  my @super_calls = ($cl =~ /pl-super-call[^)]+name/g);
  is(scalar @super_calls, 2, 'Two SUPER::name calls (getter and setter)');
}


# ============================================================
diag "";
diag "-------- SUPER:: in Block Form Package:";

# Test SUPER:: works in block-scoped package
{
  my $cl = transpile(<<'END_PERL');
package Parent;
sub greet { "parent" }

package Child {
    our @ISA = qw(Parent);

    sub greet {
        my $self = shift;
        return $self->SUPER::greet() . " from child block";
    }
}
END_PERL

  like($cl, qr/pl-super-call .* 'greet "Child"/, 'SUPER:: works in block-scoped package');
}


# ============================================================
diag "";
diag '-------- @ISA with Double-Quoted Strings:';

# Test @ISA with double-quoted strings
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = ("Parent");
END_PERL

  like($cl, qr/defclass child \(parent\)/, 'Double-quoted @ISA works');
}


# ============================================================
diag "";
diag '-------- Mixed Quote Styles in @ISA:';

# Test @ISA with mixed quote styles
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = ('Parent1', "Parent2");
END_PERL

  like($cl, qr/defclass child \(parent1 parent2\)/, 'Mixed quote styles in @ISA');
}


# ============================================================
diag "";
diag "-------- Very Deep Nesting (4 levels):";

# Test 4-level inheritance
{
  my $cl = transpile(<<'END_PERL');
package Level0;
sub method { 0 }

package Level1;
our @ISA = qw(Level0);

package Level2;
our @ISA = qw(Level1);

package Level3;
our @ISA = qw(Level2);

sub method {
    my $self = shift;
    return $self->SUPER::method() + 3;
}
END_PERL

  like($cl, qr/defclass level0 \(\)/, 'Level0 base');
  like($cl, qr/defclass level1 \(level0\)/, 'Level1 inherits Level0');
  like($cl, qr/defclass level2 \(level1\)/, 'Level2 inherits Level1');
  like($cl, qr/defclass level3 \(level2\)/, 'Level3 inherits Level2');
}


# ============================================================
diag "";
diag "-------- Private Method Pattern:";

# Test private method naming convention with inheritance
{
  my $cl = transpile(<<'END_PERL');
package Base;
sub _private { "base private" }
sub public {
    my $self = shift;
    return $self->_private();
}

package Derived;
our @ISA = qw(Base);
sub _private { "derived private" }
END_PERL

  # Both _private methods should be defined
  like($cl, qr/pl-sub pl-_private/, '_private method defined');
  like($cl, qr/defclass derived \(base\)/, 'Derived inherits Base');
}


# ============================================================
diag "";
diag "-------- SUPER:: not at Start of Method:";

# Test SUPER:: called after some other code
{
  my $cl = transpile(<<'END_PERL');
package Child;
our @ISA = qw(Parent);

sub process {
    my $self = shift;
    my $prepared = $self->prepare();
    my $result = $self->SUPER::process($prepared);
    return $self->finalize($result);
}
END_PERL

  # Should have both regular method calls and SUPER::
  like($cl, qr/pl-method-call .* 'prepare/, 'Regular method call before SUPER::');
  like($cl, qr/pl-super-call .* 'process/, 'SUPER:: call in middle');
  like($cl, qr/pl-method-call .* 'finalize/, 'Regular method call after SUPER::');
}


# ============================================================
diag "";
diag "-------- can() Method:";

# Test can() method call syntax
{
  my $cl = transpile(<<'END_PERL');
package MyClass;
sub foo { "foo" }

package main;
my $obj = MyClass->new();
if ($obj->can("foo")) {
    print "has foo";
}
END_PERL

  like($cl, qr/pl-method-call .* 'can/, 'can() generates method call');
}

# Test can() with string argument
{
  my $cl = transpile(<<'END_PERL');
my $method = $obj->can("process");
END_PERL

  like($cl, qr/pl-method-call .* 'can "process"/, 'can() with string arg');
}

# Test can() as class method
{
  my $cl = transpile(<<'END_PERL');
if (MyClass->can("new")) {
    print "can construct";
}
END_PERL

  like($cl, qr/pl-method-call.*"MyClass".*'can/s, 'Class->can() syntax');
}


# ============================================================
diag "";
diag "-------- isa() Method:";

# Test isa() method call syntax
{
  my $cl = transpile(<<'END_PERL');
package Dog;
our @ISA = qw(Animal);

package main;
my $dog = Dog->new();
if ($dog->isa("Animal")) {
    print "is an animal";
}
END_PERL

  like($cl, qr/pl-method-call .* 'isa "Animal"/, 'isa() generates method call with class name');
}

# Test isa() with inheritance check
{
  my $cl = transpile(<<'END_PERL');
if ($obj->isa("Parent")) {
    $obj->parent_method();
}
END_PERL

  like($cl, qr/pl-method-call .* 'isa "Parent"/, 'isa() with parent class');
}

# Test isa() as class method
{
  my $cl = transpile(<<'END_PERL');
if (Child->isa("Parent")) {
    print "Child inherits from Parent";
}
END_PERL

  like($cl, qr/pl-method-call.*"Child".*'isa/s, 'Class->isa() syntax');
}


# ============================================================
diag "";
diag "-------- can() and isa() Combined:";

# Test using both can() and isa() together
{
  my $cl = transpile(<<'END_PERL');
package Handler;
sub process { }

package AdvancedHandler;
our @ISA = qw(Handler);
sub extra { }

package main;
my $h = AdvancedHandler->new();
if ($h->isa("Handler") && $h->can("extra")) {
    $h->extra();
}
END_PERL

  like($cl, qr/pl-method-call .* 'isa "Handler"/, 'isa() in combined check');
  like($cl, qr/pl-method-call .* 'can "extra"/, 'can() in combined check');
}


# ============================================================
diag "";
diag "-------- UNIVERSAL::can and UNIVERSAL::isa:";

# Test UNIVERSAL::can function call syntax
{
  my $cl = transpile(<<'END_PERL');
if (UNIVERSAL::can($obj, "method")) {
    print "can do method";
}
END_PERL

  like($cl, qr/UNIVERSAL::pl-can/, 'UNIVERSAL::can generates qualified call');
}

# Test UNIVERSAL::isa function call syntax
{
  my $cl = transpile(<<'END_PERL');
if (UNIVERSAL::isa($obj, "Class")) {
    print "is a Class";
}
END_PERL

  like($cl, qr/UNIVERSAL::pl-isa/, 'UNIVERSAL::isa generates qualified call');
}


done_testing();
