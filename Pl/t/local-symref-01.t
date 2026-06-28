#!/usr/bin/env perl

# Regression: `local` through a *symbolic* reference (a string naming a package
# variable) saves/restores that package variable; `local` through a *hard*
# reference dies with "Can't localize through a reference".
# Covers local ${...}, $$x, @{...}, @$x, %{...}, %$x  (op/localref.t).

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
    $output =~ s/^\s+//;
    return $output;
}

plan tests => 8;

# ── scalar: local ${aa} / ${"aa"} / ${$x} / $$x all localize $aa ──
{
    my $out = run_pl(q{
$aa = 1;
{ local ${aa};   $aa = 3; print "in1=$aa\n"; }
print "out1=$aa\n";
{ local ${"aa"}; $aa = 4; print "in2=$aa\n"; }
print "out2=$aa\n";
$x = "aa";
{ local ${$x};   $aa = 5; print "in3=$aa\n"; }
print "out3=$aa\n";
{ local $$x;     $aa = 7; print "in4=$aa\n"; }
print "out4=$aa\n";
});
    is($out, "in1=3\nout1=1\nin2=4\nout2=1\nin3=5\nout3=1\nin4=7\nout4=1\n",
       'local on symbolic scalar deref saves/restores $aa');
}

# ── array: local @{$x} / @$x localize @aa ──
{
    my $out = run_pl(q{
@aa = ('a','b');
$x = "aa";
{ local @{$x}; @aa = ('i','j'); print "in=@aa\n"; }
print "out=@aa\n";
{ local @$x;   @aa = ('m','n'); print "in=@aa\n"; }
print "out=@aa\n";
});
    is($out, "in=i j\nout=a b\nin=m n\nout=a b\n",
       'local on symbolic array deref saves/restores @aa');
}

# ── hash: local %{$x} / %$x localize %aa ──
{
    my $out = run_pl(q{
%aa = (a=>'b');
$x = "aa";
{ local %{$x}; %aa = (i=>'j'); print "in=$aa{i}\n"; }
print "out=$aa{a}\n";
{ local %$x;   %aa = (m=>'n'); print "in=$aa{m}\n"; }
print "out=$aa{a}\n";
});
    is($out, "in=j\nout=b\nin=n\nout=b\n",
       'local on symbolic hash deref saves/restores %aa');
}

# ── hard reference: local through a real ref is fatal ──
{
    my $out = run_pl(q{
$aa = 1;
$x = \$aa;
eval { local $$x; };
print $@ =~ /Can't localize through a reference/ ? "scalar-die\n" : "scalar-NODIE\n";
$x = \@aa;
eval { local @$x; };
print $@ =~ /Can't localize through a reference/ ? "array-die\n" : "array-NODIE\n";
$x = \%aa;
eval { local %$x; };
print $@ =~ /Can't localize through a reference/ ? "hash-die\n" : "hash-NODIE\n";
});
    is($out, "scalar-die\narray-die\nhash-die\n",
       'local through a hard reference dies');
}

# ── ${\$aa} and ${\'x'} (anon hard refs) also die ──
{
    my $out = run_pl(q{
$aa = 1;
eval { local ${\$aa}; };
print $@ =~ /Can't localize through a reference/ ? "ok1\n" : "no1\n";
eval { local @{[]}; };
print $@ =~ /Can't localize through a reference/ ? "ok2\n" : "no2\n";
});
    is($out, "ok1\nok2\n", 'local through anonymous hard refs dies');
}

# ── transpile-level: the macro is emitted (not silently dropped) ──
{
    my $parser = Pl::Parser->new(code => '$aa=1; { local ${aa}; $aa=3; }');
    my $cl = $parser->parse();
    like($cl, qr/p-local-deref-scalar/, 'local ${aa} emits p-local-deref-scalar');
}
{
    my $parser = Pl::Parser->new(code => '@aa=(1); { local @{$x}; }');
    my $cl = $parser->parse();
    like($cl, qr/p-local-deref-array/, 'local @{$x} emits p-local-deref-array');
}
{
    my $parser = Pl::Parser->new(code => '%aa=(); { local %$x; }');
    my $cl = $parser->parse();
    like($cl, qr/p-local-deref-hash/, 'local %$x emits p-local-deref-hash');
}
