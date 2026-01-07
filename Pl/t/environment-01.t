#          -*-Mode: CPerl -*-

# Test Pl::Environment module

use v5.32;
use strict;
use warnings;

use lib ".";

use Data::Dump qw/dump/;

use Test::More tests => 56;
BEGIN { use_ok('Pl::Environment') };

my $env;

# ----------------------------------------------------------------------
diag "";
diag "-------- Basic Construction:";

$env = Pl::Environment->new();
isa_ok($env, 'Pl::Environment', "new() creates Environment object");

# Check defaults
is(ref($env->prototypes), 'HASH', "prototypes is hash ref");
is(ref($env->filehandles), 'HASH', "filehandles is hash ref");
is(ref($env->lvalue_subs), 'HASH', "lvalue_subs is hash ref");

# Default filehandles
ok($env->is_filehandle('STDIN'), "STDIN is default filehandle");
ok($env->is_filehandle('STDOUT'), "STDOUT is default filehandle");
ok($env->is_filehandle('STDERR'), "STDERR is default filehandle");

# ----------------------------------------------------------------------
diag "";
diag "-------- Prototypes:";

$env = Pl::Environment->new();

is($env->get_prototype('my_func'), undef, "Unknown prototype is undef");
ok(!$env->has_prototype('my_func'), "has_prototype returns false for unknown");

$env->add_prototype('my_func', '$$');
is($env->get_prototype('my_func'), '$$', "Prototype stored correctly");
ok($env->has_prototype('my_func'), "has_prototype returns true after add");

$env->add_prototype('push', '\@@');
is($env->get_prototype('push'), '\@@', "Complex prototype stored");

# Update prototype
$env->add_prototype('my_func', '$$$');
is($env->get_prototype('my_func'), '$$$', "Prototype can be updated");

# ----------------------------------------------------------------------
diag "";
diag "-------- Filehandles:";

$env = Pl::Environment->new();

ok(!$env->is_filehandle('FH'), "FH not defined initially");

$env->add_filehandle('FH');
ok($env->is_filehandle('FH'), "FH added as filehandle");

$env->add_filehandle('LOG');
ok($env->is_filehandle('LOG'), "LOG added as filehandle");

# Default filehandles still present
ok($env->is_filehandle('STDIN'), "STDIN still present");

# ----------------------------------------------------------------------
diag "";
diag "-------- Lvalue Subs:";

$env = Pl::Environment->new();

ok(!$env->is_lvalue_sub('get_value'), "get_value not defined initially");

$env->add_lvalue_sub('get_value');
ok($env->is_lvalue_sub('get_value'), "get_value added as lvalue sub");

$env->add_lvalue_sub('set_value');
ok($env->is_lvalue_sub('set_value'), "set_value added as lvalue sub");

# ----------------------------------------------------------------------
diag "";
diag "-------- Construction with Initial Data:";

$env = Pl::Environment->new(
    prototypes => { func1 => '$', func2 => '$$' },
    filehandles => { FH1 => 1, FH2 => 1 },
    lvalue_subs => { lv1 => 1 },
);

# Check prototypes
is($env->get_prototype('func1'), '$', "func1 prototype from constructor");
is($env->get_prototype('func2'), '$$', "func2 prototype from constructor");

# Check filehandles (including defaults)
ok($env->is_filehandle('FH1'), "FH1 from constructor");
ok($env->is_filehandle('FH2'), "FH2 from constructor");
ok($env->is_filehandle('STDIN'), "STDIN still present with custom filehandles");

# Check lvalue subs
ok($env->is_lvalue_sub('lv1'), "lv1 from constructor");

# ----------------------------------------------------------------------
diag "";
diag "-------- Merge:";

my $env1 = Pl::Environment->new(
    prototypes => { func1 => '$' },
);

my $env2 = Pl::Environment->new(
    prototypes => { func2 => '$$' },
    filehandles => { FH => 1 },
);

$env1->merge($env2);

# Check that env1 now has both
is($env1->get_prototype('func1'), '$', "env1 still has func1 prototype");
is($env1->get_prototype('func2'), '$$', "env1 now has func2 from env2");
ok($env1->is_filehandle('FH'), "env1 now has FH from env2");

# Check that env2 is unchanged
ok(!$env2->has_prototype('func1'), "env2 doesn't have func1");

# ----------------------------------------------------------------------
diag "";
diag "-------- Clone:";

$env1 = Pl::Environment->new(
    prototypes => { func => '$' },
    filehandles => { FH => 1 },
    lvalue_subs => { lv => 1 },
);

my $env_clone = $env1->clone();

isa_ok($env_clone, 'Pl::Environment', "clone() returns Environment");

# Check cloned data
is($env_clone->get_prototype('func'), '$', "Clone has prototype");
ok($env_clone->is_filehandle('FH'), "Clone has filehandle");
ok($env_clone->is_lvalue_sub('lv'), "Clone has lvalue sub");

# Modify clone - should not affect original
$env_clone->add_prototype('new_func', '$$');
ok($env_clone->has_prototype('new_func'), "Clone has new prototype");
ok(!$env1->has_prototype('new_func'), "Original doesn't have new prototype");

# Modify original - should not affect clone
$env1->add_prototype('orig_func', '$$$');
ok($env1->has_prototype('orig_func'), "Original has orig_func");
ok(!$env_clone->has_prototype('orig_func'), "Clone doesn't have orig_func");

# ----------------------------------------------------------------------
diag "";
diag "-------- Filehandle Scope Management:";

$env = Pl::Environment->new();

# Initial state
is($env->scope_level, 0, "Initial scope level is 0");
ok($env->is_filehandle('STDERR'), "STDERR exists at scope 0");
ok(!$env->is_filehandle('FH'), "FH doesn't exist initially");

# Enter scope 1 and add filehandle
$env->push_scope();
is($env->scope_level, 1, "After push_scope, level is 1");

$env->add_filehandle('FH');
ok($env->is_filehandle('FH'), "FH exists after add_filehandle");
ok($env->is_filehandle('STDERR'), "STDERR still exists at scope 1");

# Enter scope 2 and add another filehandle
$env->push_scope();
is($env->scope_level, 2, "After second push_scope, level is 2");

$env->add_filehandle('FH2');
ok($env->is_filehandle('FH2'), "FH2 exists at scope 2");
ok($env->is_filehandle('FH'), "FH still exists at scope 2");

# Leave scope 2
$env->pop_scope();
is($env->scope_level, 1, "After pop_scope, level is 1");
ok(!$env->is_filehandle('FH2'), "FH2 removed after pop_scope");
ok($env->is_filehandle('FH'), "FH still exists (added at scope 1)");
ok($env->is_filehandle('STDERR'), "STDERR still exists");

# Leave scope 1
$env->pop_scope();
is($env->scope_level, 0, "Back to scope 0");
ok(!$env->is_filehandle('FH'), "FH removed after second pop_scope");
ok($env->is_filehandle('STDERR'), "STDERR persists (scope 0)");

# pop_scope at level 0 should not go negative
$env->pop_scope();
is($env->scope_level, 0, "pop_scope at 0 stays at 0");
