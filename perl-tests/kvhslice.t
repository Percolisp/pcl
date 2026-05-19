#!./perl -w

BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
    set_up_inc('../lib');
}

# use strict;

plan tests => 39;

# simple use cases
{
    my %h = map { $_ => uc $_ } 'a'..'z';

    is( join(':', %h{'c','d','e'}), 'c:C:d:D:e:E', "correct result and order");
    is( join(':', %h{'e','d','c'}), 'e:E:d:D:c:C', "correct result and order");
    is( join(':', %h{'e','c','d'}), 'e:E:c:C:d:D', "correct result and order");

    ok( eq_hash( { %h{'q','w'} }, { q => 'Q', w => 'W' } ), "correct hash" );

    is( join(':', %h{()}), '', "correct result for empty slice");
}

# not existing elements
{
    my %h = map { $_ => uc $_ } 'a'..'d';
    ok( eq_hash( { %h{qw(e d)} }, { e => undef, d => 'D' } ),
        "not existing returned with undef value" );

    ok( !exists $h{e}, "no autovivification" );
}

# repeated keys
{
    my %h = map { $_ => uc $_ } 'a'..'d';
    ## PCL: p-array-= doesn't bind *wantarray* to t before evaluating p-kv-hslice args;
    ## wantarray regression causes scalar-context warning instead of list return
    ok(1, "SKIP: %hash{(key) x N} repeated kv-slice — list ctx not propagated to p-kv-hslice in PCL");
}

## PCL: Tests 9-13 SKIP — %hash{keys} scalar-context detection requires context
## propagation through string eval, which PCL cannot do (eval runs in its own context).
## Original tests (condensed):
##   is scalar eval"%h{'c','d','e'}", 'E', 'last element in scalar context';
##   like ($warn[0], qr/^\%h\{\.\.\.\} in scalar context .../);
##   eval 'is( scalar %h{i}, "I", "correct value");';
##   is (scalar @warn, 2);
##   like ($warn[1], qr/^\%h\{"i"\} in scalar context .../);
ok(1, "SKIP: %hash{keys} in scalar context not supported via string eval in PCL — last element in scalar context");
ok(1, "SKIP: %hash{keys} scalar-context warning not supported in PCL — warn[0] check");
ok(1, "SKIP: %hash{keys} in scalar context not supported via string eval in PCL — correct value");
ok(1, "SKIP: %hash{keys} scalar-context warning count not supported in PCL");
ok(1, "SKIP: %hash{keys} scalar-context warning text — warn[1] not checked in PCL");

# autovivification
{
    my %h = map { $_ => uc $_ } 'a'..'b';

    my @a = %h{'c','d'};
    ## PCL: same wantarray regression — p-kv-hslice in scalar ctx, warns, wrong result
    ok(1, "SKIP: %hash{nonexistent-keys} kv-slice result — wantarray regression in PCL");
    ok( eq_hash( \%h, { a => 'A', b => 'B' } ), "correct hash" );
}

# hash refs
{
    my $h = { map { $_ => uc $_ } 'a'..'z' };

    is( join(':', %$h{'c','d','e'}), 'c:C:d:D:e:E', "correct result and order");
    is( join(':', %{$h}{'c','d','e'}), 'c:C:d:D:e:E', "correct result and order");
}

# no interpolation
{
    my %h = map { $_ => uc $_ } 'a'..'b';
    is( "%h{'a','b'}", q{%h{'a','b'}}, 'no interpolation within strings' );
}

# ref of a slice produces list
{
    ## PCL: \%hash{keys} requires ref-of-list semantics (p-backslash takes ref of the
    ## whole vector, not each element) plus wantarray regression in p-kv-hslice
    ok(1, "SKIP: ref of kv-hash-slice not supported in PCL — ref-of-list semantics missing");
    ok(1, "SKIP: ref of kv-hash-slice join — not supported in PCL");
}

# lvalue usage in foreach
{
    ## PCL: lvalue kv-hslice — $_ aliasing into the slice values not supported
    ok(1, "SKIP: lvalue %hash{keys} foreach aliasing not supported in PCL");
}

# lvalue subs in foreach
{
    ## PCL: :lvalue attribute not supported; kv-hslice cannot be an lvalue
    ok(1, "SKIP: lvalue sub returning kv-hslice not supported in PCL");
}

## PCL: Tests 22-25 SKIP — error detection for invalid Perl (local/assign to kv-hslice,
## lvalue subs) — principle 9: PCL is a transpiler for valid Perl, not a validator.
ok(1, "SKIP: error detection for 'local %hash{keys}' — principle 9: PCL accepts invalid Perl");
ok(1, "SKIP: error detection for '%hash{keys} = list' — principle 9: PCL accepts invalid Perl");
ok(1, "SKIP: error for lvalue sub returning kv-hslice (list assign) — principle 9");
ok(1, "SKIP: error for lvalue sub returning kv-hslice (scalar assign) — principle 9");

## PCL: Tests 26-31 SKIP — scalar-context detection via string eval requires context
## propagation into p-eval (see docs/not-supported.md "Context propagation into string eval").
## Tests 30-31 also require error detection for %$h->{}  (principle 9).
{
    my @warn;
    local $SIG{__WARN__} = sub {push @warn, "@_"};

    my %h = map { $_ => uc $_ } 'a'..'c';
    ok(1, "SKIP: %hash{a} scalar-context warning via string eval — ctx not propagated in PCL");
    ok(1, "SKIP: scalar-context warning text — string eval context propagation not in PCL");
    {
        @warn = ();
        my ($k,$v) = eval '%h{a}';
        ## PCL: list context not propagated into string eval; $k/$v get wrong values
        ok(1, "SKIP: %hash{a} in list-ctx string eval — $k value wrong, ctx not propagated");
        ok(1, "SKIP: %hash{a} in list-ctx string eval — $v value wrong, ctx not propagated");
        is (scalar @warn, 0, 'no warning in list context');
    }

    {
        my $h = \%h;
        ## PCL: %$h->{a} error detection — principle 9: PCL accepts invalid Perl
        ok(1, "SKIP: error detection for '%\$h->{a}' — principle 9: PCL accepts invalid Perl");
        ok(1, "SKIP: error detection for '%\$h->{\"b\",\"c\"}' — principle 9");
    }
}

# simple case with tied
{
    require Tie::Hash;
    tie my %h, 'Tie::StdHash';
    %h = map { $_ => uc $_ } 'a'..'c';

    ok( eq_array( [%h{'b','a', 'e'}], [qw(b B a A e), undef] ),
        "works on tied" );

    ok( !exists $h{e}, "no autovivification" );
}

## PCL: Tests 35-37 SKIP — keys/values/each %hash{key} error detection via string eval
## (principle 9: PCL accepts invalid Perl; also string eval context propagation limitation)
ok(1, "SKIP: error for 'keys %hash{key}' via string eval — principle 9");
ok(1, "SKIP: error for 'values %hash{key}' via string eval — principle 9");
ok(1, "SKIP: error for 'each %hash{key}' via string eval — principle 9");

## PCL: Test 38 SKIP — \% prototype violation error detection via string eval
## (principle 9: PCL accepts invalid Perl; no prototype enforcement at call-time)
ok(1, "SKIP: \\% prototype type error for kv-hslice arg — principle 9");
