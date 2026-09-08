#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser2;
use FindBin;
use lib "$FindBin::Bin";
use PCLCore;

# The sbcl command line comes from the ONE builder every runner shares
# (tools/lib/PCLSbcl.pm via PCLCore::sbcl_prefix, task #344): the saved core
# with the runtime already compiled in, and the 512 MB control stack.  This
# file used to spell `sbcl --noinform --non-interactive --load
# cl/pcl-runtime.lisp` itself, which recompiles the whole runtime on EVERY row
# -- 2.89 CPU-s a row against 0.007 s from the core (measured s473u, #1544) --
# and ran on the default 2 MB stack, which is exactly the drift #344 exists to
# stop.
my @sbcl_rt = PCLCore::sbcl_prefix("$FindBin::Bin/../../cl/pcl-runtime.lisp");

sub run_pl {
    my $code = shift;
        my $cl_code = Pl::Parser2->parse_code($code);

    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;

    my $output = `sbcl @sbcl_rt --load "$filename" 2>&1`;
    unlink $filename;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s+//;

    return $output;
}

sub parse_pl {
    my $code = shift;
        return Pl::Parser2->parse_code($code);
}

plan tests => 18;

# ── Codegen checks ──────────────────────────────────────────────────────────

{
    my $cl = parse_pl('our %h; local $h{key};');
    like($cl, qr/p-local-hash-elem/, 'local $h{key} generates p-local-hash-elem');
    like($cl, qr/"key"/, 'bareword key is auto-quoted');
}

{
    my $cl = parse_pl('our @a; local $a[2];');
    like($cl, qr/p-local-array-slice/, 'local $a[2] generates p-local-array-slice');
    like($cl, qr/p-local-array-slice \@a 2/, 'array element uses correct index');
}

{
    my $cl = parse_pl('our %h; local @h{"x","y"};');
    like($cl, qr/p-local-hash-elem.*"x".*p-local-hash-elem.*"y"/s,
         'local @h{x,y} generates two nested p-local-hash-elem calls');
}

# ── Runtime: hash element ────────────────────────────────────────────────────

{
    my $out = run_pl(q{
our %h = (key => 10);
{
    local $h{key} = 42;
    say $h{key};
}
say $h{key};
});
    is($out, "42\n10\n", 'local $h{key}=val: sets inside scope, restores outside');
}

{
    my $out = run_pl(q{
our %h = (key => 10);
sub peek { return $h{key}; }
sub test {
    local $h{key} = 99;
    return peek();
}
say test();
say peek();
});
    is($out, "99\n10\n", 'local $h{key} provides dynamic scoping to called sub');
}

{
    my $out = run_pl(q{
our %h = (a => 1, b => 2);
{
    local $h{a};
    say defined($h{a}) ? "defined:$h{a}" : "undef";
}
say $h{a};
});
    is($out, "undef\n1\n", 'bare local $h{key} sets to undef temporarily, restores on exit');
}

{
    my $out = run_pl(q{
our %h = (x => 5);
eval {
    local $h{x} = 99;
    die "oops\n";
};
say $h{x};
});
    is($out, "5\n", 'local $h{key} restores after die/eval (non-local exit)');
}

{
    my $out = run_pl(q{
our %h;
{
    local $h{new} = "hello";
    say $h{new};
}
say defined($h{new}) ? "still there" : "gone";
});
    is($out, "hello\ngone\n", 'local $h{key} where key did not exist: removed on exit');
}

{
    my $out = run_pl(q{
our %h = (k => 1);
{
    local $h{k} = 2;
    {
        local $h{k} = 3;
        say $h{k};
    }
    say $h{k};
}
say $h{k};
});
    is($out, "3\n2\n1\n", 'nested local $h{key}: LIFO restoration');
}

{
    my $out = run_pl(q{
our %h = (k => "original");
{
    local $h{k} = "temp";
    $h{other} = "side";
}
say $h{k};
say $h{other};
});
    is($out, "original\nside\n", 'local $h{k} only restores that key, not others');
}

# ── Runtime: array element ───────────────────────────────────────────────────

{
    my $out = run_pl(q{
our @a = (10, 20, 30);
{
    local $a[1] = 99;
    say $a[1];
}
say $a[1];
});
    is($out, "99\n20\n", 'local $a[N]=val: sets inside scope, restores outside');
}

{
    my $out = run_pl(q{
our @a = (10, 20, 30);
eval {
    local $a[0] = 55;
    die "bail\n";
};
say $a[0];
});
    is($out, "10\n", 'local $a[N] restores after die/eval');
}

{
    my $out = run_pl(q{
our @a = (1, 2, 3);
{
    local $a[-1] = 99;
    say $a[2];
}
say $a[2];
});
    is($out, "99\n3\n", 'local $a[-1] (negative index) sets last element, restores');
}

{
    my $out = run_pl(q{
our @a = (1, 2, 3);
{
    local $a[1] = 5;
    {
        local $a[1] = 9;
        say $a[1];
    }
    say $a[1];
}
say $a[1];
});
    is($out, "9\n5\n2\n", 'nested local $a[N]: LIFO restoration');
}

# ── Runtime: hash slice ──────────────────────────────────────────────────────

{
    my $out = run_pl(q{
our %h = (a => 1, b => 2, c => 3);
{
    local @h{"a","b"} = (10, 20);
    say "$h{a} $h{b} $h{c}";
}
say "$h{a} $h{b} $h{c}";
});
    is($out, "10 20 3\n1 2 3\n", 'local @h{a,b}=vals: sets slice, restores on exit');
}

{
    my $out = run_pl(q{
our %h = (x => "old");
{
    local @h{"x","y"} = ("new_x", "new_y");
    say $h{x};
    say $h{y};
}
say $h{x};
say defined($h{y}) ? "y:$h{y}" : "y:gone";
});
    is($out, "new_x\nnew_y\nold\ny:gone\n",
       'local @h{x,y}: existing key restored, new key removed on exit');
}

done_testing();
