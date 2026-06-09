#!/usr/bin/env perl
# shim-gaps.pl — TEMPORARY diagnostic.
#
# PCL ships hand-written shims for some core modules under lib/ (because the real
# module is XS, or uses a construct PCL can't yet run — e.g. real Errno.pm builds
# its constants via a dynamic symbol-table loop).  A hand-written shim drifts from
# the real module: it omits exports/subs the real one has.  Since PCL now runs the
# real Exporter, a missing export turns into a hard `"X is not exported"` die at
# `use` time (this is what silently broke Errno/EBADF).
#
# This script compares each lib/*.pm shim against the REAL module (introspected in
# a clean perl with PCL's lib/ removed from @INC) and reports, per module:
#   - EXPORT GAP : names in the real @EXPORT/@EXPORT_OK that the shim does not export
#   - SUB GAP    : sub/constant names defined in the real package but absent in the shim
# Run from the project root:  perl tools/shim-gaps.pl
#
# It only reports; it changes nothing.  Use the output as a punch-list to complete
# the shims (or to decide a shim should load the real module instead).

use strict;
use warnings;
use File::Find;
use FindBin qw($RealBin);

my $root    = "$RealBin/..";
my $lib_dir = "$root/lib";

# Collect (package, shim-file) for every lib/*.pm.
my @shims;
find(sub {
    return unless /\.pm$/;
    my $file = $File::Find::name;
    open my $fh, '<', $file or return;
    my $pkg;
    while (<$fh>) { if (/^\s*package\s+([\w:]+)\s*;/) { $pkg = $1; last } }
    close $fh;
    push @shims, [ $pkg, $file ] if $pkg;
}, $lib_dir);

# Introspect a module in a clean perl: print @EXPORT+@EXPORT_OK, then a marker,
# then every CODE symbol (sub/constant) in the package's stash.  @INC is the real
# one when $use_shim is false; with PCL's lib/ prepended when true.
sub introspect {
    my ($pkg, $use_shim) = @_;
    my $inc = $use_shim ? "-I$lib_dir" : '';
    my $code = <<'PERL';
        my $pkg = shift @ARGV;
        eval "require $pkg; 1" or do { print "LOADFAIL\n"; exit };
        no strict 'refs';
        my %ex; $ex{$_}=1 for (@{"${pkg}::EXPORT"}, @{"${pkg}::EXPORT_OK"});
        print "EXPORT\t$_\n" for sort keys %ex;
        print "--SUBS--\n";
        for my $n (sort keys %{"${pkg}::"}) {
            print "SUB\t$n\n" if defined &{"${pkg}::$n"};
        }
PERL
    my @out = `perl $inc -e '$code' $pkg 2>/dev/null`;
    my (%exp, %sub, $in_subs, $loadfail);
    for (@out) {
        chomp;
        $loadfail = 1, last if $_ eq 'LOADFAIL';
        if    ($_ eq '--SUBS--')      { $in_subs = 1 }
        elsif (/^EXPORT\t(.+)$/)      { $exp{$1} = 1 }
        elsif (/^SUB\t(.+)$/ && $in_subs) { $sub{$1} = 1 }
    }
    return ($loadfail, \%exp, \%sub);
}

my $total_gaps = 0;
for my $s (sort { $a->[0] cmp $b->[0] } @shims) {
    my ($pkg, $file) = @$s;
    my ($real_fail, $real_exp, $real_sub) = introspect($pkg, 0);
    if ($real_fail) {
        print "== $pkg ==\n  (no real module installed — PCL-specific shim, skipped)\n\n";
        next;
    }
    my ($shim_fail, $shim_exp, $shim_sub) = introspect($pkg, 1);
    if ($shim_fail) {
        print "== $pkg ==\n  (shim failed to load standalone; check manually)\n\n";
        next;
    }
    my @exp_gap = grep { !$shim_exp->{$_} } sort keys %$real_exp;
    # Sub gap: real exportable names the shim's package doesn't define at all.
    my @sub_gap = grep { !$shim_sub->{$_} && $real_exp->{$_} } sort keys %$real_sub;
    next unless @exp_gap || @sub_gap;
    $total_gaps += @exp_gap + @sub_gap;
    print "== $pkg ==  ($file)\n";
    print "  EXPORT GAP (", scalar @exp_gap, "): @exp_gap\n" if @exp_gap;
    print "  SUB GAP    (", scalar @sub_gap, "): @sub_gap\n" if @sub_gap;
    print "\n";
}
print "Total shim gaps: $total_gaps\n";
