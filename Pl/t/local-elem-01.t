#!/usr/bin/env perl

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser;

sub run_pl {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    my $cl_code = $parser->parse();

    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;

    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
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
    my $parser = Pl::Parser->new(code => $code);
    return $parser->parse();
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
    like($cl, qr/p-local-array-elem/, 'local $a[2] generates p-local-array-elem');
    like($cl, qr/p-local-array-elem \@a 2/, 'array element uses correct index');
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
