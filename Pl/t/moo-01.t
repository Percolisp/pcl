#!/usr/bin/env perl
#          -*-Mode: CPerl -*-

# Moo end-to-end guard (s304, task #80).
#
# Moo is the best single integration test PCL has: one `use Moo` exercises
# module loading, Exporter-aliased imports, glob-installed subs, constant
# subs under `use strict` (bareword-after-binary-op disambiguation),
# Sub::Quote/Sub::Defer string eval, method dispatch, and the p-sub calling
# convention.  Two v2 bugs found through it in s304:
#   - the signature fast path emitted a bare &optional lambda list that did
#     NOT flatten aggregate args (f(@_) delegation bound the args vector to
#     the first param) — Moo::_Utils::_name_coderef -> Sub::Util::set_subname
#     returned undef, so `use Moo` installed no subs at all;
#   - `use strict` was invisible to v2's ahead-of-stream sub lowering, so
#     `$module =~ _module_name_rx` (a glob-installed constant sub) parsed as
#     the literal string "_module_name_rx" and extends/with croaked
#     '"Foo" is not a module name!'.
#
# One transpiled program prints tag=value lines; we assert one per line.
# trig_absent/trig_flow guard the s305 fix for task #81: the Moo constructor's
# `exists $args->{w} and (assign), (trigger)` guard used to parse as
# `(exists and assign), trigger` — comma binds tighter than `and` (perlop) —
# so the trigger fired unconditionally at construction.

use v5.30;
use strict;
use warnings;
use lib ".";
use File::Temp qw(tempfile);

use Test::More;

my $pl2cl   = "./pl2cl";
my $runtime = "cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found"  unless -x $pl2cl;
plan skip_all => "sbcl not found"   unless `which sbcl 2>/dev/null`;
plan skip_all => "Moo not installed" unless eval { require Moo; 1 };

my $driver = <<'PERL';
package Pt {
    use Moo;
    has x => (is => 'ro', required => 1);
    has y => (is => 'rw', default => 7);
    has size => (is => 'rw', lazy => 1, builder => '_build_size',
                 predicate => 'has_size', clearer => 'clear_size');
    sub _build_size { my $self = shift; $self->x + $self->y }
}
package Pt3 {
    use Moo;
    extends 'Pt';
    has z => (is => 'rw', default => 3);
}
package Greeter {
    use Moo::Role;
    requires 'name';
    has greeting => (is => 'rw', default => 'hello');
    sub greet { my $self = shift; $self->greeting . " " . $self->name }
}
package Tw {
    use Moo;
    has w => (is => 'rw',
              trigger => sub { push @main::trighist, "t:" . ($_[1] // "u") });
}
package Person {
    use Moo;
    has name => (is => 'ro', required => 1);
    has calls => (is => 'rw', default => sub { [] });
    with 'Greeter';
    before greet => sub { push @{ $_[0]->calls }, 'before' };
    after  greet => sub { push @{ $_[0]->calls }, 'after' };
    around greet => sub {
        my ($orig, $self) = (shift, shift);
        return "<" . $self->$orig(@_) . ">";
    };
}

my $p = Pt->new(x => 2);
print "ro=", $p->x, "\n";
print "default=", $p->y, "\n";
$p->y(11);
print "rw=", $p->y, "\n";
print "lazy=", $p->size, "\n";
print "pred=", ($p->has_size ? 1 : 0), "\n";
$p->clear_size;
print "cleared=", ($p->has_size ? 1 : 0), "\n";
my $noreq = eval { Pt->new(); 1 };
print "required=", (defined $noreq ? "missing-ok" : "enforced"), "\n";

my $q = Pt3->new(x => 1, y => 2);
print "inherit=", join(",", $q->x, $q->y, $q->z), "\n";
print "isa=", ($q->isa('Pt') ? 1 : 0), "\n";

my $person = Person->new(name => 'ada');
print "does=", ($person->does('Greeter') ? 1 : 0), "\n";
print "modifiers=", $person->greet, "\n";
print "order=", join(",", @{ $person->calls }), "\n";
print "role_attr=", $person->greeting, "\n";

our @trighist;
@trighist = ();
my $t1 = Tw->new;
print "trig_absent=", join(",", @trighist), "\n";
@trighist = ();
my $t2 = Tw->new(w => 'i');
$t2->w('s');
print "trig_flow=", join(",", @trighist), "\n";
PERL

my %expect = (
    ro        => '2',
    default   => '7',
    rw        => '11',
    lazy      => '13',
    pred      => '1',
    cleared   => '0',
    required  => 'enforced',
    inherit   => '1,2,3',
    isa       => '1',
    does      => '1',
    modifiers => '<hello ada>',
    order     => 'before,after',
    role_attr => 'hello',
    trig_absent => '',
    trig_flow   => 't:i,t:s',
);

plan tests => scalar(keys %expect);

my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $driver;
close $fh;
my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;
my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

my %got;
for my $line (split /\n/, $out) {
    $got{$1} = $2 if $line =~ /^(\w+)=(.*)$/;
}

for my $tag (sort keys %expect) {
    is($got{$tag} // '<missing>', $expect{$tag}, "moo: $tag => $expect{$tag}")
        or diag("full output:\n$out");
}
