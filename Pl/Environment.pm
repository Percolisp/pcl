package Pl::Environment;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.30;
use strict;
use warnings;

use Moo;

=head1 NAME

Pl::Environment - Environment information for Perl expression parser

=head1 SYNOPSIS

    use Pl::Environment;
    
    my $env = Pl::Environment->new(
        prototypes => { my_func => '$$' },
    );

    my $proto = $env->get_prototype('my_func');

=head1 DESCRIPTION

Pl::Environment provides information about the Perl environment that affects
expression parsing. This includes:

- Subroutine prototypes
- Bareword filehandles
- Lvalue subroutines

The environment is INPUT to the parser - it does not manage scope or track
declarations. That's the job of a statement parser (future work).

=cut

=head1 ATTRIBUTES

=head2 prototypes

Hash reference mapping subroutine names to their signature info.

    prototypes => {
        my_func => {
            params     => [ { name => '$x', default_cl => undef },
                            { name => '$y', default_cl => '10' } ],
            min_params => 1,
            is_proto   => 0,
        }
    }

Signature info includes:
- params: Array of parameter hashes with name and optional default_cl (compiled CL)
- min_params: Minimum number of required arguments
- is_proto: True if old-style prototype ($$), false if new-style signature ($x, $y)

=cut

has prototypes => (
    is => 'rw',
    default => sub { _builtin_prototypes() },
);

# Built-in function prototypes for functions that take bareword filehandles.
# The '*' prototype means "accepts bareword as filehandle".
# NOTE: We don't set min_params here - that's handled by Config.pm's known_no_of_params.
# These prototypes only provide the '*' type info for post-processing bareword filehandles.
sub _builtin_prototypes {
    return {
        # File I/O - these take filehandle as first arg
        'open'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'close'     => { params => [{proto_type => '*'}], is_proto => 1 },
        'binmode'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'eof'       => { params => [{proto_type => '*'}], is_proto => 1 },
        'tell'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'seek'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'truncate'  => { params => [{proto_type => '*'}], is_proto => 1 },
        'flock'     => { params => [{proto_type => '*'}], is_proto => 1 },
        'read'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'sysread'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'syswrite'  => { params => [{proto_type => '*'}], is_proto => 1 },
        'sysseek'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'fileno'    => { params => [{proto_type => '*'}], is_proto => 1 },
        'getc'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'stat'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'lstat'     => { params => [{proto_type => '*'}], is_proto => 1 },
        # Directory operations
        'opendir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'readdir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'closedir'  => { params => [{proto_type => '*'}], is_proto => 1 },
        'rewinddir' => { params => [{proto_type => '*'}], is_proto => 1 },
        'seekdir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'telldir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        # Note: print/say/printf are NOT included here - they have special handling
        # in PExpr.pm that deals with their complex filehandle detection logic.
    };
}

=head2 filehandles

Hash reference of bareword filehandles.

    filehandles => { STDIN => 1, STDOUT => 1, FH => 1 }

Used to distinguish barewords in print statements:

    print FH "data";    # FH is filehandle
    print foo "data";   # foo is function

=cut

has filehandles => (
    is => 'rw',
    coerce => sub {
        my $fh = shift // {};
        return { STDIN => 1, STDOUT => 1, STDERR => 1, DATA => 1, ARGV => 1, %$fh };
    },
    default => sub { { STDIN => 1, STDOUT => 1, STDERR => 1, DATA => 1, ARGV => 1 } },
);

=head2 filehandle_scope

Tracks scope level for each filehandle. Standard filehandles are at scope 0.
When pop_scope() is called, filehandles added at that scope level are removed.

=cut

has filehandle_scope => (
    is => 'rw',
    default => sub { { STDIN => 0, STDOUT => 0, STDERR => 0, DATA => 0, ARGV => 0 } },
);

=head2 scope_level

Current scope nesting level. Starts at 0.

=cut

has scope_level => (
    is => 'rw',
    default => 0,
);

=head2 in_subroutine

Counter tracking subroutine nesting depth. 0 = top level, >0 = inside sub.
Used to determine whether shift/pop should default to @_ or @ARGV.

=cut

has in_subroutine => (
    is => 'rw',
    default => 0,
);

=head2 lvalue_subs

Hash reference of subroutines declared with :lvalue attribute.

    lvalue_subs => { get_value => 1 }

Lvalue subs can appear on the left side of assignment:

    get_value() = 42;

=cut

has lvalue_subs => (
    is => 'rw',
    default => sub { {} },
);

=head2 package_stack

Array reference representing the current package scope stack.
The default is ['main']. When entering a package block, push the
new package name. When leaving, pop back to the previous package.

    package MyClass { ... }  # push 'MyClass', then pop after block

=cut

has package_stack => (
    is => 'rw',
    default => sub { ['main'] },
);

=head2 known_packages

Hash reference of all declared package/class names.

    known_packages => { Counter => 1, Point => 1 }

Used to distinguish class names from function calls in method calls:

    Counter->new();  # Counter is a known package, use as class name

=cut

has known_packages => (
    is => 'rw',
    default => sub { {} },
);

=head2 referenced_packages

Hash of package names referenced in code (e.g., from Foo::bar() calls).
Used to pre-declare packages that might not be defined until runtime.

=cut

has referenced_packages => (
    is => 'rw',
    default => sub { {} },
);

=head2 our_variables

Hash of package variables declared with 'our'.
Keys are "Package::$varname", values are 1.

    our_variables => { 'Counter::$count' => 1 }

=cut

has our_variables => (
    is => 'rw',
    default => sub { {} },
);

=head2 isa_declarations

Hash of @ISA declarations per package.
Keys are package names, values are arrayrefs of parent package names.

    isa_declarations => { 'Child' => ['Parent1', 'Parent2'] }

=cut

has isa_declarations => (
    is => 'rw',
    default => sub { {} },
);

=head2 declared_subs

Array of subs declared in this file, with their package names.
Each entry is { name => 'subname', package => 'PackageName' }.
Used to emit forward declarations so top-level code can call subs
defined later in the file.

=cut

has declared_subs => (
    is => 'rw',
    default => sub { [] },
);

=head2 source_file

The source filename being parsed. Used for __FILE__ token expansion.
Defaults to '-' (stdin) if not set.

=cut

has source_file => (
    is => 'rw',
    default => '-',
);

=head1 METHODS

=head2 get_prototype($name)

Returns the signature info hash for a subroutine, or undef if not found.

    my $sig_info = $env->get_prototype('my_func');
    # Returns: { params => [...], min_params => N, is_proto => 0/1 }

=cut

sub get_prototype {
    my $self = shift;
    my $name = shift;

    return $self->prototypes->{$name};
}

=head2 has_prototype($name)

Returns true if the subroutine has a known prototype/signature.

    if ($env->has_prototype('my_func')) { ... }

=cut

sub has_prototype {
    my $self = shift;
    my $name = shift;

    return exists $self->prototypes->{$name};
}

=head2 get_min_params($name)

Returns the minimum number of required parameters for a subroutine,
or undef if not found.

    my $min = $env->get_min_params('my_func');

=cut

sub get_min_params {
    my $self = shift;
    my $name = shift;

    my $sig_info = $self->prototypes->{$name};
    return undef unless $sig_info;
    return $sig_info->{min_params};
}

=head2 add_prototype($name, $sig_info)

Adds or updates a subroutine signature info.

    $env->add_prototype('my_func', {
        params     => [ { name => '$x' }, { name => '$y', default_cl => '10' } ],
        min_params => 1,
        is_proto   => 0,
    });

=cut

sub add_prototype {
    my $self     = shift;
    my $name     = shift;
    my $sig_info = shift;

    $self->prototypes->{$name} = $sig_info;
}

=head2 is_filehandle($name)

Returns true if $name is a known filehandle.

    if ($env->is_filehandle('FH')) { ... }

=cut

sub is_filehandle {
    my $self = shift;
    my $name = shift;
    
    return exists $self->filehandles->{$name};
}

=head2 add_filehandle($name)

Adds a bareword filehandle at the current scope level.

    $env->add_filehandle('FH');

=cut

sub add_filehandle {
    my $self = shift;
    my $name = shift;

    $self->filehandles->{$name} = 1;
    $self->filehandle_scope->{$name} = $self->scope_level;
}

=head2 push_scope()

Enters a new scope level. Called when entering a block.

    $env->push_scope();

=cut

sub push_scope {
    my $self = shift;
    $self->scope_level($self->scope_level + 1);
}

=head2 pop_scope()

Leaves the current scope level. Removes filehandles added at this level.
Called when leaving a block.

    $env->pop_scope();

=cut

sub pop_scope {
    my $self = shift;
    my $level = $self->scope_level;

    # Remove filehandles added at this scope level
    my $fh = $self->filehandles;
    my $fh_scope = $self->filehandle_scope;

    for my $name (keys %$fh_scope) {
        if ($fh_scope->{$name} == $level) {
            delete $fh->{$name};
            delete $fh_scope->{$name};
        }
    }

    # Decrease scope level (but never below 0)
    $self->scope_level($level - 1) if $level > 0;
}

=head2 is_lvalue_sub($name)

Returns true if $name is a known lvalue subroutine.

    if ($env->is_lvalue_sub('get_value')) { ... }

=cut

sub is_lvalue_sub {
    my $self = shift;
    my $name = shift;
    
    return exists $self->lvalue_subs->{$name};
}

=head2 add_lvalue_sub($name)

Adds an lvalue subroutine.

    $env->add_lvalue_sub('get_value');

=cut

sub add_lvalue_sub {
    my $self = shift;
    my $name = shift;

    $self->lvalue_subs->{$name} = 1;
}

=head2 current_package()

Returns the current package name (top of stack).

    my $pkg = $env->current_package();  # 'main' by default

=cut

sub current_package {
    my $self = shift;
    return $self->package_stack->[-1];
}

=head2 push_package($name)

Pushes a new package onto the stack.

    $env->push_package('MyClass');

=cut

sub push_package {
    my $self = shift;
    my $name = shift;
    push @{$self->package_stack}, $name;
}

=head2 pop_package()

Pops the current package from the stack.
Never pops below 'main'.

    $env->pop_package();

=cut

sub pop_package {
    my $self  = shift;
    my $stack = $self->package_stack;
    pop @$stack if @$stack > 1;  # Never pop 'main'
}

=head2 add_package($name)

Adds a package to the known packages set.

    $env->add_package('Counter');

=cut

sub add_package {
    my $self = shift;
    my $name = shift;
    $self->known_packages->{$name} = 1;
}

=head2 is_package($name)

Returns true if $name is a known package/class.

    if ($env->is_package('Counter')) { ... }

=cut

sub is_package {
    my $self = shift;
    my $name = shift;
    return exists $self->known_packages->{$name};
}

=head2 add_referenced_package($name)

Records that a package is referenced in code (e.g., via Foo::bar() call).
Only records if the package is not already declared via known_packages.

=cut

sub add_referenced_package {
    my $self = shift;
    my $name = shift;
    return if exists $self->known_packages->{$name};
    $self->referenced_packages->{$name} = 1;
}

=head2 get_undeclared_packages()

Returns list of packages that are referenced but not declared.
Used to emit pre-declarations at the top of generated code.

=cut

sub get_undeclared_packages {
    my $self = shift;
    my @pkgs = grep { !exists $self->known_packages->{$_} }
               keys %{$self->referenced_packages};
    return [sort @pkgs];
}

=head2 add_our_variable($pkg, $var)

Records that a variable was declared with 'our' in the given package.

    $env->add_our_variable('Counter', '$count');

=cut

sub add_our_variable {
    my ($self, $pkg, $var) = @_;
    $self->our_variables->{"${pkg}::${var}"} = 1;
}

=head2 is_our_variable($pkg, $var)

Returns true if $var was declared with 'our' in $pkg.

    if ($env->is_our_variable('Counter', '$count')) { ... }

=cut

sub is_our_variable {
    my ($self, $pkg, $var) = @_;
    return exists $self->our_variables->{"${pkg}::${var}"};
}

=head2 set_isa($pkg, \@parents)

Records the @ISA declaration for a package.

    $env->set_isa('Child', ['Parent1', 'Parent2']);

=cut

sub set_isa {
    my ($self, $pkg, $parents) = @_;
    $self->isa_declarations->{$pkg} = $parents;
}

=head2 get_isa($pkg)

Returns the @ISA for a package, or empty arrayref if not set.

    my $parents = $env->get_isa('Child');  # ['Parent1', 'Parent2']

=cut

sub get_isa {
    my ($self, $pkg) = @_;
    return $self->isa_declarations->{$pkg} // [];
}

=head2 add_declared_sub($name, $package)

Records that a sub was declared in the given package.
Used to emit forward declarations.

    $env->add_declared_sub('greet', 'main');

=cut

sub add_declared_sub {
    my ($self, $name, $package) = @_;
    push @{$self->declared_subs}, { name => $name, package => $package };
}

=head2 get_declared_subs()

Returns arrayref of all declared subs with their packages.

    my $subs = $env->get_declared_subs();
    # [ { name => 'foo', package => 'main' }, { name => 'bar', package => 'MyClass' } ]

=cut

sub get_declared_subs {
    my $self = shift;
    return $self->declared_subs;
}

=head2 merge($other_env)

Merges another environment into this one.

    $env->merge($other_env);

All prototypes, filehandles, and lvalue subs from $other_env
are added to this environment. In case of conflicts, $other_env wins.

=cut

sub merge {
    my $self  = shift;
    my $other = shift;

    # Merge prototypes
    for my $name (keys %{$other->prototypes}) {
        $self->prototypes->{$name} = $other->prototypes->{$name};
    }
    
    # Merge filehandles
    for my $name (keys %{$other->filehandles}) {
        $self->filehandles->{$name} = 1;
    }
    
    # Merge lvalue subs
    for my $name (keys %{$other->lvalue_subs}) {
        $self->lvalue_subs->{$name} = 1;
    }

    # Merge known packages
    for my $name (keys %{$other->known_packages}) {
        $self->known_packages->{$name} = 1;
    }
}

=head2 clone()

Creates a shallow copy of this environment.

    my $env2 = $env->clone();

Useful for creating a modified environment without affecting the original.

=cut

sub clone {
    my $self = shift;

    return Pl::Environment->new(
        prototypes       => { %{$self->prototypes} },
        filehandles      => { %{$self->filehandles} },
        filehandle_scope => { %{$self->filehandle_scope} },
        scope_level      => $self->scope_level,
        lvalue_subs      => { %{$self->lvalue_subs} },
        package_stack    => [ @{$self->package_stack} ],
        known_packages   => { %{$self->known_packages} },
    );
}

=head1 SCOPE MANAGEMENT

Scope management for filehandles is handled via push_scope/pop_scope.

Scope management requires tracking:
- Block entry/exit
- my/our/local declarations
- Package changes

This is the job of a statement parser, not an expression parser.

For V1, the environment is INPUT to the parser - typically created by:
1. Scanning declarations in the file
2. Loading from a configuration
3. Building from known built-ins

=cut

=head1 USAGE WITH PARSER

    use Pl::PExpr;
    use Pl::Environment;

    # Create environment
    my $env = Pl::Environment->new(
        prototypes => { my_func => '$$' },
    );
    
    # Parse with environment
    my $parser = Pl::PExpr->new(
        e => $expr,
        environment => $env,
    );
    
    my $tree_id = $parser->parse_expr_to_tree($expr);

The parser can then use the environment to:
- Apply prototype-based parsing rules
- Handle bareword filehandles in print
- Validate lvalue contexts

=cut

1;

=head1 AUTHOR

Perl Expression Parser Project

=head1 SEE ALSO

L<Pl::PExpr>, L<Pl::PExpr::Config>

=cut
