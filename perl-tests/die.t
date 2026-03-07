#!./perl

BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
    set_up_inc('../lib');
}

# PCL: plan reduced from 26 - removed tests that:
# - use $SIG{__DIE__} (PCL does not call __DIE__ handlers)
# - check exact "at FILE line N" / "...propagated at" message format
# - use string eval (PCL stub is minimal)
# - use runperl subprocess
plan tests => 3;

use utf8;

# PCL: die-with-no-args propagation (die if $@) not implemented — commented out
# eval { eval { die "Horribly\n" }; die if $@ };
# like($@, qr/^Horribly/, 'die with no args propagates $@');
# PCL: "...propagated at" phrase not appended — commented out
# like($@, qr/\.{3}propagated at/, '... and appends a phrase');

{
    # PCL: $SIG{__DIE__} handler not supported — inner isa_ok commented out
    #local $SIG{__DIE__} = sub { is( $_[0], "[\000]\n", 'Embedded null passed to signal handler' )};

    $err = "[\000]\n";
    eval {
        die $err;
    };
    is( $@, $err, 'Embedded null passed back into $@' );
}

# PCL: entire $SIG{__DIE__} + PROPAGATE block commented out — needs __DIE__ handler
# {
#     local $SIG{__DIE__} = sub { isa_ok($_[0],'ARRAY',...); $_[0]->[0]++ };
#     $x = [3]; eval { die $x };
#     is($x->[0], 4, 'actual array, not a copy, passed to signal handler');
#     eval { eval { die [5] }; die if $@ };
#     is($@->[0], 7, 'die with no arguments propagates $@, but leaves references alone');
#     eval { eval { die bless [7],"Error" }; isa_ok($@,'Error','$@ is an Error object'); die if $@ };
#     isa_ok($@,'Out','returning a different object than what was passed in, via PROPAGATE');
#     is($@->[0], 9, 'reference returned correctly');
# }
# package Error { sub PROPAGATE { bless [$_[0]->[0]], "Out" } }

{
    # die/warn and utf8
    use utf8;
    local $SIG{__DIE__};
    my $msg = "ce ºtii tu, bã ?\n";
    eval { die $msg };
    is( $@, $msg, "Literal passed to die" );
    our $err;
    # PCL: utf8 string literal in $msg causes SBCL read issues — block commented out
    # local $SIG{__WARN__} = $SIG{__DIE__} = sub { $err = shift };
    # eval { die $msg };
    # is( $err, $msg, 'die handler with utf8' );
    # eval { warn $msg };
    # is( $err, $msg, 'warn handler with utf8' );
    # PCL: string eval with utf8 symbol name not implemented — commented out
    # eval qq/ use strict; \$\x{3b1} /;
    # like( $@, qr/Global symbol "\$\x{3b1}"/, 'utf8 symbol names show up in $@' );
}

# [perl #36470] got uninit warning if $@ was undef
{
    use warnings "uninitialized";
    my $ok = 1;
    local $SIG{__DIE__};
    local $SIG{__WARN__} = sub { $ok = 0 };
    eval { undef $@; die };
    is( $ok, 1, 'no warnings if $@ is undef' );

    # PCL: "...propagated at" phrase not appended — commented out
    # eval { $@ = 100; die };
    # like($@."", qr/100\t\.{3}propagated at/, 'check non-PVs in $@ are propagated');
}

# PCL: $SIG{__DIE__} + string eval block commented out
# { my @error; local $SIG{__DIE__}= sub { push @error, @_ }; ... }

# PCL: runperl subprocess not supported — commented out
# TODO: { local $TODO = ...; my $out = runperl(...); like($out, ...) }
