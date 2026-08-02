#!/usr/bin/env perl
# difftest-ops.pl — differential test of PCL against real `perl` (the oracle)
# for operator precedence / associativity / ternary nesting / named-unary forms.
#
# For each generated snippet:  run it through real `perl` AND through PCL
# (./runpcl), normalize, and report only the MISMATCHES (incl. PCL parse errors).
# Every mismatch is a bug; the snippet doubles as a ready-made regression test.
#
# Usage:  perl tools/difftest-ops.pl [--jobs N] [--limit N] [--show-ok]

use strict;
use warnings;
use File::Basename qw(dirname);
use Cwd qw(abs_path);
use File::Path qw(make_path remove_tree);
use Getopt::Long;

my $JOBS = 8;
my $LIMIT = 0;
my $SHOW_OK = 0;
GetOptions('jobs=i' => \$JOBS, 'limit=i' => \$LIMIT, 'show-ok' => \$SHOW_OK);

my $ROOT  = abs_path(dirname(abs_path($0)) . "/..");
my $RUNPL = "$ROOT/runpcl";
my $WORK  = "/tmp/difftest_$$";
make_path($WORK);

# ---------------------------------------------------------------------------
# Snippet generators.  Each snippet ends by printing exactly "[VALUE]\n" so the
# comparison is robust against surrounding noise; undef prints as [undef].
# ---------------------------------------------------------------------------
my @snips;   # {desc, code, mods?}
# add(desc, code [, {ModName => module_code, ...}]) — mods are written as
# $WORK/ModName.pm at generation time (two-file module axis); the snippet
# reaches them via its own `use lib "$WORK"` line.
sub add { push @snips, { desc => $_[0], code => $_[1], mods => $_[2] }; }

# wrap an expression EXPR into a full program that prints its scalar value
sub prog {
    my ($expr, $prelude) = @_;
    $prelude //= '';
    return "${prelude}my \$r = $expr;\n"
         . "\$r = 'undef' unless defined \$r;\n"
         . "print \"[\$r]\\n\";\n";
}

# --- Axis 1: binary operator precedence pairs (no parens): 2 OP1 3 OP2 4 -----
# Shifts (<<,>>) excluded: large-shift behaviour is a documented non-support.
my @num_ops = qw( ** * % + - . & | ^ == != <=> < > <= >= && || // );
for my $o1 (@num_ops) {
    for my $o2 (@num_ops) {
        add("binop  2 $o1 3 $o2 4", prog("2 $o1 3 $o2 4"));
    }
}

# --- Axis 2: string/relational ops with mixed numeric-string operands --------
my @str_ops = qw( . x eq ne lt gt le ge cmp );
for my $o1 (@str_ops) {
    for my $o2 (qw( . eq lt == + )) {
        add("strop  '2' $o1 '10' $o2 '3'", prog("'2' $o1 '10' $o2 '3'"));
    }
}

# --- Axis 3: ternary nesting / associativity --------------------------------
# Iterate condition truth values so every branch leaf is reached.
my @tern_shapes = (
    [ 'C1 ? C2 ? a : b : c',                  'true-nest',    2 ],
    [ 'C1 ? a : C2 ? b : c',                  'false-nest',   2 ],
    [ 'C1 ? C2 ? a : b : C3 ? c : d',         'both-nest',    3 ],
    [ 'C1 ? C2 ? C3 ? a : b : c : d',         'deep-true',    3 ],
    [ 'C1 ? a : C2 ? b : C3 ? c : d',         'chain3',       3 ],
    [ 'C1 ? a : b == c ? d : e',              'binop-in-fls', 1 ],
    [ 'C1 ? a == b ? c : d : e',              'binop-in-tru', 1 ],
);
my @leaf = map { "\"L$_\"" } ('a'..'h');   # distinct quoted leaf values
for my $sh (@tern_shapes) {
    my ($tmpl, $name, $ncond) = @$sh;
    for my $bits (0 .. (2**$ncond - 1)) {
        my $expr = $tmpl;
        # substitute conditions C1.. with 0/1 from $bits
        for my $i (1 .. $ncond) {
            my $v = ($bits >> ($i-1)) & 1;
            $expr =~ s/\bC$i\b/$v/g;
        }
        # substitute leaf letters a,b,c... with distinct quoted strings; for the
        # binop-in-* shapes a/b/c are operands of == so use numbers there.
        if ($name =~ /^binop-in/) {
            my @nums = (1,2,2,8,9);   # a b c d e ; pick so == is decidable
            my $k = 0;
            $expr =~ s/\b([a-e])\b/$nums[$k++]/g;
        } else {
            my %seen; my $k = 0;
            $expr =~ s/\b([a-h])\b/ "\"L$1\"" /ge;
        }
        add("ternary[$name] $tmpl  bits=$bits", prog($expr));
    }
}

# --- Axis 4: named-unary combined with binary / ternary ---------------------
add('ref-eq-ternary',  prog('ref $h eq "HASH" ? "yes" : "no"', 'my $h = {};'));
add('ref-ne-ternary',  prog('ref $h ne "HASH" ? "yes" : "no"', 'my $h = [];'));
add('length-cmp',      prog('length $s == 3 ? "three" : "other"', 'my $s = "abc";'));
add('length-plus',     prog('length $s + 1', 'my $s = "abcd";'));
add('defined-ternary', prog('defined $x ? "def" : "undef"', 'my $x;'));
add('not-defined',     prog('! defined $x', 'my $x = 5;'));
add('scalar-cmp',      prog('scalar(@a) > 2 ? "big" : "small"', 'my @a = (1,2,3);'));
add('uc-eq',           prog('uc $s eq "AB" ? "y" : "n"', 'my $s = "ab";'));
add('ref-in-cond',     prog('ref $h eq "HASH" && length $k > 0 ? "y" : "n"',
                                                'my $h={}; my $k="x";'));
add('chained-named',   prog('lc uc $s', 'my $s = "AbC";'));

# --- Axis 5: context sensitivity (scalar / list / count) --------------------
# The SAME expression is evaluated in list, count (goatse `()=`), and (where
# well-defined) scalar context; PCL must match perl in every one.  These catch
# the list-vs-scalar / flatten / wantarray bugs that recur in CPAN sweeps.
# Each program prints exactly one [payload] so the existing extractor works.
sub prog_list {           # list context: join elements, undef -> U
    my ($expr, $prelude) = @_; $prelude //= '';
    return "${prelude}my \@__r = ($expr);\n"
         . "print '[', join(',', map { defined \$_ ? \$_ : 'U' } \@__r), \"]\\n\";\n";
}
sub prog_count {          # count context: number of list elements via () =
    my ($expr, $prelude) = @_; $prelude //= '';
    return "${prelude}my \$__n = () = ($expr);\n"
         . "print \"[\$__n]\\n\";\n";
}
sub prog_scalar {         # scalar context
    my ($expr, $prelude) = @_; $prelude //= '';
    return "${prelude}my \$__s = scalar($expr);\n"
         . "\$__s = 'U' unless defined \$__s;\n"
         . "print \"[\$__s]\\n\";\n";
}
# [ expr, prelude, scalar-is-well-defined? ] — skip scalar where perl warns
# (split) or it is unspecified (sort).  list+count are always well-defined.
my @ctx_exprs = (
    [ '@a',                    'my @a = (10,20,30);',          1 ],
    [ 'reverse @a',            'my @a = (1,2,3);',             1 ],
    [ 'reverse "abc"',         '',                             1 ],
    [ 'map { $_*2 } @a',       'my @a = (1,2,3);',             1 ],
    [ 'map { ($_,$_) } @a',    'my @a = (1,2);',               1 ],
    [ 'grep { $_ > 1 } @a',    'my @a = (1,2,3);',             1 ],
    [ 'sort { $a <=> $b } @a', 'my @a = (3,1,2);',             0 ],
    [ 'split /,/, $s',         'my $s = "a,b,c";',             0 ],
    [ '(1,2,3)',               '',                             1 ],
    [ '(10,20,30)[1,2]',       '',                             1 ],
    [ '@a[0,2]',               'my @a = (5,6,7);',             1 ],
    [ '@h{qw(a c)}',           'my %h = (a=>1,b=>2,c=>3);',    1 ],
    [ 'sort keys %h',          'my %h = (b=>2,a=>1,c=>3);',    0 ],
    [ '$s =~ /(\d)(\d)/',      'my $s = "42";',                1 ],
    [ 'unpack("A1A1","xy")',   '',                             1 ],
    [ '("x") x 3',             '',                             1 ],
    [ 'wantarray ? "L" : defined(wantarray) ? "S" : "V"', '',  1 ],
);
for my $ce (@ctx_exprs) {
    my ($expr, $pre, $scalar_ok) = @$ce;
    add("ctx-list   $expr",  prog_list($expr, $pre));
    add("ctx-count  $expr",  prog_count($expr, $pre));
    add("ctx-scalar $expr",  prog_scalar($expr, $pre)) if $scalar_ok;
}

# --- Axis 6: builtins x call forms (paren / named / CORE:: / $_ default) -----
# Catches @_/$_ defaulting and named-unary parsing differences across call
# syntaxes (e.g. session 240's CORE::shift / CORE::ref bugs).  ARG is a Perl
# literal; each form prints one [result].
sub add_builtin {
    my ($name, $arg, $us) = @_;   # $us = defaults to $_ when called with no arg
    add("bi $name($arg)",        prog("$name($arg)"));
    add("bi $name $arg",         prog("$name $arg"));
    add("bi CORE::$name($arg)",  prog("CORE::$name($arg)"));
    add("bi CORE::$name $arg",   prog("CORE::$name $arg"));
    if ($us) {
        add("bi $name \$_",        prog($name,         "\$_ = $arg;"));
        add("bi CORE::$name \$_",  prog("CORE::$name", "\$_ = $arg;"));
    }
}
add_builtin('length',   '"abcd"', 1);
add_builtin('uc',       '"abc"',  1);
add_builtin('lc',       '"ABC"',  1);
add_builtin('ucfirst',  '"abc"',  1);
add_builtin('lcfirst',  '"ABC"',  1);
# NB: fc() is gated behind `use feature 'fc'`; without it perl treats a bare
# `fc` as the string "fc", so it is omitted here (PCL always treats it builtin).
add_builtin('ord',      '"A"',    1);
add_builtin('chr',      '65',     1);
add_builtin('hex',      '"ff"',   1);
add_builtin('oct',      '"0x1f"', 1);
add_builtin('abs',      '-5',     1);
add_builtin('int',      '3.7',    1);
add_builtin('sqrt',     '16',     1);
add_builtin('quotemeta','"a.b"',  1);
add_builtin('ref',      '[1]',    1);
add_builtin('defined',  '0',      1);

# --- Axis 7: deref / sigil / slices / postfix deref -------------------------
# Ref names avoid $r / @__r (used by prog / prog_list).
my $SR = 'my $x = 42; my $sr = \$x;';
my $AR = 'my @a = (10,20,30); my $ar = \@a;';
my $HR = 'my %h = (a=>1,b=>2,c=>3); my $hr = \%h;';
# [ expr, prelude, kind ]  kind: 's' scalar(prog), 'l' list(prog_list)
my @deref = (
    [ '$$sr',            $SR, 's' ],
    [ '${$sr}',          $SR, 's' ],
    [ '$sr->$*',         $SR, 's' ],
    [ '@$ar',            $AR, 'l' ],
    [ '@{$ar}',          $AR, 'l' ],
    [ '$ar->@*',         $AR, 'l' ],
    [ '$$ar[1]',         $AR, 's' ],
    [ '${$ar}[1]',       $AR, 's' ],
    [ '$ar->[1]',        $AR, 's' ],
    [ '$ar->[-1]',       $AR, 's' ],
    [ '$#$ar',           $AR, 's' ],
    [ '$#{$ar}',         $AR, 's' ],
    [ '@$ar[0,2]',       $AR, 'l' ],
    [ '@{$ar}[0,2]',     $AR, 'l' ],
    [ '$ar->@[0,2]',     $AR, 'l' ],
    [ '$$hr{a}',         $HR, 's' ],
    [ '${$hr}{a}',       $HR, 's' ],
    [ '$hr->{a}',        $HR, 's' ],
    [ '@$hr{qw(a c)}',   $HR, 'l' ],
    [ '$hr->@{qw(a c)}', $HR, 'l' ],
    [ 'sort keys %$hr',  $HR, 'l' ],
    [ 'sort keys %{$hr}',$HR, 'l' ],
    [ '$ar->[0] + $ar->[1]', $AR, 's' ],
);
for my $d (@deref) {
    my ($expr, $pre, $kind) = @$d;
    add("deref $expr", $kind eq 'l' ? prog_list($expr, $pre) : prog($expr, $pre));
}

# --- Axis 8: OO dispatch ----------------------------------------------------
# Small hierarchy Dog -> Animal exercising override / inherited / SUPER:: /
# can / isa / ref / method-name-in-var / class-method / chained.
my $OO = join('',
    'package Animal; sub new { bless { name => $_[1] }, $_[0] }',
    ' sub speak { "generic" } sub name { $_[0]{name} }',
    ' package Dog; our @ISA = ("Animal");',
    ' sub speak { "woof" } sub fetch { "fetch" }',
    ' sub describe { my $s = shift; $s->SUPER::speak() . "+" . $s->speak() }',
    ' package main; my $d = Dog->new("Rex"); my $m = "speak";');
for my $oc (
    [ '$d->speak',                  ],   # inherited override
    [ '$d->speak()',                ],
    [ '$d->name',                   ],   # inherited method
    [ '$d->fetch',                  ],   # own method
    [ '$d->$m',                     ],   # method name in scalar
    [ '$d->$m()',                   ],
    [ 'Dog->speak',                 ],   # class-method call
    [ 'Dog->new("X")->name',        ],   # chained
    [ '$d->describe',               ],   # SUPER::
    [ 'ref $d',                     ],
    [ '$d->isa("Animal") ? 1 : 0',  ],
    [ '$d->isa("Cat") ? 1 : 0',     ],
    [ '$d->can("fetch") ? "y":"n"', ],
    [ '$d->can("meow") ? "y":"n"',  ],
    [ 'Dog->can("speak") ? "y":"n"',],
    [ '$d->isa("Dog") ? 1 : 0',     ],
) {
    my $expr = $oc->[0];
    add("oo $expr", prog($expr, $OO));
}

# --- Axis 9: string builtins & sprintf formatting ---------------------------
# substr (2/3/4-arg, negative offset+len), index/rindex, x-repeat, join,
# and a broad sprintf format sweep — all scalar-context results.
my @strfns = (
    'substr("hello",1)', 'substr("hello",1,3)', 'substr("hello",-3)',
    'substr("hello",-3,2)', 'substr("hello",1,-1)', 'substr("hello",-2,-1)',
    'substr("hello",0,0)', 'substr("hello",5)',
    'index("hello world","o")', 'index("hello world","o",5)',
    'index("abc","z")', 'index("abc","")',
    'rindex("hello world","o")', 'rindex("hello world","o",4)',
    'rindex("abc","z")',
    '"ab" x 3', '"ab" x 0', '"-" x 5', 'join("",("a") x 3)',
    'join("-",1,2,3)', 'join("",1..4)',
    'sprintf("%d",42)', 'sprintf("%5d",42)', 'sprintf("%-5d|",42)',
    'sprintf("%05d",42)', 'sprintf("%x",255)', 'sprintf("%X",255)',
    'sprintf("%#x",255)', 'sprintf("%o",8)', 'sprintf("%b",5)',
    'sprintf("%e",1234.5)', 'sprintf("%.2f",3.14159)', 'sprintf("%g",0.0001)',
    'sprintf("%g",1000000)', 'sprintf("%g",100000000)', 'sprintf("%+d",42)',
    'sprintf("% d",42)', 'sprintf("%c",65)', 'sprintf("%s","hi")',
    'sprintf("%3s","hi")', 'sprintf("%-3s|","hi")', 'sprintf("%.2s","hello")',
    'sprintf("%%")', 'sprintf("%2\$s %1\$s","a","b")',
    'sprintf("%*d",4,7)', 'sprintf("%.*f",2,3.14159)',
    'sprintf("%d %d",1,2)', 'sprintf("[%6.2f]",3.14159)',
    'reverse("abc")', 'lc("MiXeD")', 'uc("MiXeD")',
    'ucfirst("hello world")', 'lcfirst("HELLO")',
    'length("héllo")', 'ord("A")', 'chr(97)',
    'quotemeta("a.b*c")', 'sprintf("%s",undef)',
);
add("strfn $_", prog($_)) for @strfns;

# --- Axis 10: regex match / substitution / transliteration ------------------
my @regex = (
    [ '"hello" =~ /l+/ ? "y" : "n"' ],
    [ '"hello" =~ /z/ ? "y" : "n"' ],
    [ '"hello" !~ /z/ ? "y" : "n"' ],
    [ 'join(",", "2024-01-02" =~ /(\d+)-(\d+)-(\d+)/)' ],
    [ '(my $x="aaa") =~ s/a/b/g' ],                # returns subst count
    [ 'do { my $x="aaa"; $x =~ s/a/b/g; $x }' ],
    [ 'do { my $x="a.b.c"; $x =~ s/\./_/g; $x }' ],
    [ 'do { my $s="Hello"; $s =~ tr/a-z/A-Z/; $s }' ],
    [ 'do { my $s="Hello"; ($s =~ tr/l//) }' ],     # count, no change
    [ 'do { my $s="hello"; $s =~ tr/a-z//cd; $s }' ],
    [ '"a1b2c3" =~ /(\w)(\d)/ ? "$1$2" : "no"' ],
    [ '"FooBar" =~ /(?<x>Foo)/ ? $+{x} : "no"' ],
    [ 'scalar(my @m = ("a1b2c3" =~ /(\d)/g))' ],    # global match count
    [ 'join(",", "a1b2c3" =~ /(\d)/g)' ],
    [ 'do { my $c = () = "mississippi" =~ /s/g; $c }' ],
    [ '"Hello World" =~ /world/i ? "y" : "n"' ],
    [ 'do { my $s="  trim  "; $s =~ s/^\s+|\s+$//g; "[$s]" }' ],
    [ 'join("|", split //, "abc")' ],
    [ 'join("|", split /,/, "a,,b", -1)' ],
    [ 'join("|", split /(,)/, "a,b")' ],            # capturing split
);
add("re $_->[0]", prog($_->[0])) for @regex;

# --- Axis 11: numeric edge cases --------------------------------------------
# Negative modulo (Perl follows right-operand sign), bit ops, ~ (u64),
# string<->number coercion, and magic string auto-increment.
my @nums = (
    '10 % 3', '-10 % 3', '10 % -3', '-10 % -3', '0 % 5',
    'int(7/2)', 'int(-7/2)', 'int(2.999)', 'int(-2.999)',
    '7 <=> 3', '3 <=> 7', '5 <=> 5',
    'abs(-3.5)', 'abs(3.5)', 'abs(-0)',
    '2 ** 10', '4 ** 0.5', '10 ** -2', '2 ** -1',
    '0.1 + 0.2', '1/4', '3/2',
    '0xff', '0b1010', '017', '1_000_000',
    'sprintf("%d", 3.9)', 'sprintf("%d", -3.9)',
    '1e3', '1.5e-3', '1234567890123',
    '5 & 3', '5 | 2', '5 ^ 1', '~0', '~5',
    '1 << 4', '256 >> 2', '1 << 30',
    '"10abc" + 5', '"3.14xyz" * 2', '"  42  " + 0', '"0x10" + 0',
    'ord("")', '"" + 0', '"abc" + 0',
    '"inf" + 0', '9**9**2 > 0 ? "pos" : "neg"',
    '5 == 5.0 ? "eq" : "ne"', '0.1+0.2 == 0.3 ? "eq" : "ne"',
    'do { my $s="Az"; $s++; $s }',
    'do { my $s="Zz"; $s++; $s }',
    'do { my $s="a9"; $s++; $s }',
    'do { my $s="Zz9"; $s++; $s }',
    'do { my $s="aa"; $s++; $s }',
    'do { my $s="a1"; $s--; $s }',          # numeric (no magic on --)
    'join(",", "aa".."ae")',
    'join(",", "Az".."Ba")',
    'join(",", 1..5)', 'join(",", reverse 1..3)',
    'join(",", "a".."e")',
);
add("num $_", prog($_)) for @nums;

# --- Axis 12: compound assignment & increment/decrement ---------------------
# Test BOTH the value the assignment expression yields AND the final variable.
# `do { my $x=I; my $r = ($x OP RHS); "$r|$x" }` -> [returnval|finalval].
my @asn = (
    [ 5,    '+= 3'  ], [ 5,    '-= 3'  ], [ 5,    '*= 3'  ], [ 7,    '/= 2'  ],
    [ 7,    '%= 3'  ], [ 2,    '**= 3' ], [ 5,    '|= 2'  ], [ 5,    '&= 3'  ],
    [ 5,    '^= 1'  ], [ 1,    '<<= 4' ], [ 256,  '>>= 2' ],
    [ '"a"','.= "b"'], [ '"ab"','x= 3' ],
    [ 0,    '||= 7' ], [ 3,    '||= 7' ], [ 0,    '//= 7' ], [ 5,    '&&= 9' ],
    [ 0,    '&&= 9' ],
);
for my $a (@asn) {
    my ($init, $op) = @$a;
    add("asn my \$x=$init; \$x $op",
        prog(qq{do { my \$x=$init; my \$r = (\$x $op); "\$r|\$x" }}));
}
# pre/post increment & decrement: value of the expression + the variable after
for my $form ('$x++','++$x','$x--','--$x') {
    add("incdec $form",
        prog(qq{do { my \$x=5; my \$r = $form; "\$r|\$x" }}));
}
# chained / nested assignment.  (We deliberately do NOT test forms that modify
# the same variable twice in one statement, e.g. `$x += $x += 1` or `$i = $i++`:
# perlop declares those UNDEFINED behavior — "Perl will not guarantee what the
# result is" — so any PCL-vs-perl divergence there is meaningless, not a bug.)
add('asn chain $a=$b=4',   prog('do { my ($a,$b); $a=$b=4; "$a|$b" }'));

# --- Axis 13: array/hash builtins — return value AND mutation ----------------
# push/pop/shift/unshift/splice return values + the resulting array; scalar(@a),
# scalar(%h), keys/values/exists/delete, wantarray of slices.
my @aryops = (
    [ 'push',     'do { my @a=(1,2); my $r=push @a,3,4; "$r|@a" }' ],
    [ 'pop',      'do { my @a=(1,2,3); my $r=pop @a; "$r|@a" }' ],
    [ 'shift',    'do { my @a=(1,2,3); my $r=shift @a; "$r|@a" }' ],
    [ 'unshift',  'do { my @a=(2,3); my $r=unshift @a,0,1; "$r|@a" }' ],
    [ 'splice3',  'do { my @a=(1,2,3,4,5); my @r=splice(@a,1,2); "@r|@a" }' ],
    [ 'splice4',  'do { my @a=(1,2,3,4,5); my @r=splice(@a,1,2,9,9,9); "@r|@a" }' ],
    [ 'splice-neg','do { my @a=(1,2,3,4,5); my @r=splice(@a,-2); "@r|@a" }' ],
    [ 'scalar-ary','do { my @a=(1,2,3); scalar(@a) }' ],
    [ 'scalar-hash','do { my %h=(a=>1,b=>2); scalar(%h) }' ],
    [ 'keys-count','do { my %h=(a=>1,b=>2,c=>3); scalar(keys %h) }' ],
    [ 'values-sum','do { my %h=(a=>1,b=>2,c=>3); my $s=0; $s+=$_ for values %h; $s }' ],
    [ 'exists',   'do { my %h=(a=>1); exists $h{a} ? "y":"n" }' ],
    [ 'exists-no','do { my %h=(a=>1); exists $h{z} ? "y":"n" }' ],
    [ 'delete',   'do { my %h=(a=>1,b=>2); my $r=delete $h{a}; "$r|".join(",",sort keys %h) }' ],
    [ 'exists-ary','do { my @a=(1,2,3); exists $a[1] ? "y":"n" }' ],
    [ 'delete-ary','do { my @a=(1,2,3); my $r=delete $a[1]; "$r|".(defined $a[1]?"def":"undef") }' ],
    [ 'wantarray-slice','do { my @a=(5,6,7,8); "@a[1..2]" }' ],
    [ 'grep-scalar','do { my @a=(1,2,3,4); my $n=grep { $_%2==0 } @a; $n }' ],
    [ 'map-scalar', 'do { my @a=(1,2,3); my $n=map { ($_,$_) } @a; $n }' ],
    [ 'reverse-list','do { my @a=(1,2,3); "@{[reverse @a]}" }' ],
    [ 'sort-default','do { my @a=(10,9,100,2); join(",",sort @a) }' ],  # string sort!
    [ 'sort-num',   'do { my @a=(10,9,100,2); join(",",sort {$a<=>$b} @a) }' ],
    [ 'wantarray-ctx','do { sub c { wantarray ? "L":"S" } my @x=c(); my $y=c(); "$x[0]|$y" }' ],
);
add("ah $_->[0]", prog($_->[1])) for @aryops;

# --- Axis 14: list construction / slices / swap / nested -------------------
my @listops = (
    [ 'list-repeat',   'do { my @a=(1,2) x 3; "@a" }' ],
    [ 'list-repeat0',  'do { my @a=(1,2) x 0; scalar(@a) }' ],
    [ 'swap',          'do { my ($a,$b)=(1,2); ($a,$b)=($b,$a); "$a|$b" }' ],
    [ 'list-assign-ct','do { my $n=(my ($a,$b,$c)=(1,2,3,4,5)); $n }' ],  # =count RHS
    [ 'array-in-list', 'do { my @a=(2,3); my @b=(1,@a,4); "@b" }' ],
    [ 'hash-slice-asn','do { my %h; @h{qw(a b c)}=(1,2,3); join(",",map "$_=$h{$_}",sort keys %h) }' ],
    [ 'nested-aref',   'do { my $r=[[1,2],[3,4]]; $r->[1][0] }' ],
    [ 'nested-href',   'do { my $r={a=>{b=>42}}; $r->{a}{b} }' ],
    [ 'aoh',           'do { my @a=({n=>1},{n=>2}); $a[1]{n} }' ],
    [ 'wantarray-flat','do { my @a=((1,2),(3,4)); scalar(@a) }' ],
    [ 'range-rev',     'do { join(",", reverse 1..3) }' ],
    [ 'last-expr-list','do { my @a=(1,2,3); my $x=(4,5,6)[-1]; $x }' ],
    [ 'neg-slice',     'do { my @a=(1,2,3,4,5); "@a[-2,-1]" }' ],
    [ 'qw-count',      'do { my @a=qw(a b c d); scalar(@a) }' ],
);
add("list $_->[0]", prog($_->[1])) for @listops;

# --- Axis 15: closures & lexical capture ------------------------------------
# Counter/accumulator closures, independent vs shared state, capture-by-
# reference (a mutation after capture is visible), nested closures, escape from
# an inner block.  NOTE: the foreach per-iteration capture
# `for my $i (...) { push @s, sub { $i } }` is a DOCUMENTED not-supported case
# (each iteration should bind a fresh $i — see CLAUDE.md / closure.t), so it is
# deliberately omitted here to avoid a permanent known-divergence in the report.
my @closures = (
    'do { my $n=0; my $c=sub { $n++ }; $c->(); $c->(); $c->() }',            # 2 post-inc
    'do { my $n=0; my $c=sub { ++$n }; $c->(); $c->() }',                    # 2 pre-inc
    'do { sub mk { my $n=shift; sub { $n++ } } my $a=mk(10); my $b=mk(20);'
      . ' $a->(); my $x=$a->(); my $y=$b->(); "$x|$y" }',                    # 11|20 independent
    'do { my $x=1; my $f=sub { $x }; $x=99; $f->() }',                       # 99 capture-by-ref
    'do { my $x=0; my $inc=sub { $x++ }; $inc->(); $inc->(); $x }',          # 2 visible outside
    'do { my $f=sub { my $x=shift; sub { my $y=shift; $x+$y } }; $f->(10)->(5) }', # 15 nested
    'do { my @a=(1,2,3); my $sum=0; my $f=sub { $sum+=$_ for @a }; $f->(); $sum }', # 6
    'do { my $g; { my $h=42; $g=sub { $h }; } $g->() }',                     # 42 escapes block
    'do { my $acc=""; my $add=sub { $acc.=shift }; $add->("a"); $add->("b"); $acc }', # ab
    'do { my $c=do { my $n=5; sub { $n-- } }; $c->(); $c->(); $c->() }',     # 3
    'do { my @subs=map { my $k=$_; sub { $k*$k } } 1..3; join(",",map { $_->() } @subs) }', # 1,4,9
);
add("closure $_", prog($_)) for @closures;

# --- Axis 16: local / dynamic scoping ---------------------------------------
# local on package scalars/arrays/hashes and their elements; the dynamic value
# seen by a called sub during the extent; restoration afterwards.  (local on a
# lexical is a perl compile error, so every target here is a package global or
# an aggregate element, which the oracle accepts.)
my @locals = (
    'do { our $G=1; sub rd { $G } my $r; { local $G=9; $r=rd() } "$r|$G" }',  # 9|1
    'do { our @A=(1,2,3); my $r; { local $A[1]=99; $r="@A" } "$r|@A" }',       # 1 99 3|1 2 3
    'do { our %H=(a=>1); my $r; { local $H{a}=9; $r=$H{a} } "$r|$H{a}" }',     # 9|1
    'do { our %H=(a=>1); my $r; { local $H{b}=5; $r=$H{b} } "$r|".(exists $H{b}?"y":"n") }', # 5|n
    'do { local $_="hi"; uc }',                                               # HI
    'do { our $G=10; sub inc { $G++ } { local $G=0; inc(); inc() } $G }',      # 10 restored
    'do { our @A=(1,2,3); my $r; { local @A=(7,8); $r="@A" } "$r|@A" }',       # 7 8|1 2 3
    'do { our $sep=","; sub j { join($sep,@_) } my $r;'
      . ' { local $sep="-"; $r=j(1,2,3) } "$r|".j(4,5) }',                     # 1-2-3|4,5
    'do { our $G="x"; sub g { $G } my @r; for my $v (qw(a b)) { local $G=$v; push @r,g() } "@r|$G" }', # a b|x
);
add("local $_", prog($_)) for @locals;

# --- Axis 17: sort variants -------------------------------------------------
# Default string sort vs numeric, descending, reverse-sort, multi-key tie-break,
# sort-by-hash-value, Schwartzian transform, named comparator, sorted slice,
# scalar-context count, sort feeding/fed-by map.
my @sorts = (
    'join(",", sort 3,1,20,2)',                          # string sort -> 1,2,20,3
    'join(",", sort { $a <=> $b } 3,1,20,2)',            # numeric
    'join(",", sort { $b <=> $a } 3,1,20,2)',            # numeric desc
    'join(",", reverse sort { $a <=> $b } 3,1,2)',       # 3,2,1
    'join(",", sort { length($a) <=> length($b) or $a cmp $b } qw(bb a ccc aa))', # a,aa,bb,ccc
    'do { my %h=(a=>3,b=>1,c=>2); join(",", sort { $h{$a} <=> $h{$b} } keys %h) }', # b,c,a
    'do { my @a=map {$_->[1]} sort {$a->[0]<=>$b->[0]} map {[length,$_]} qw(ccc a bb); "@a" }', # a bb ccc
    'join(",", sort { $a cmp $b } qw(banana apple cherry))',
    'do { sub byn { $a <=> $b } join(",", sort byn 3,1,2) }',  # named comparator
    'join(",", (sort { $a <=> $b } 5,3,8,1)[0,1])',      # sorted slice -> 1,3
    'scalar(my @s = sort 3,1,2)',                        # count in scalar
    'join(",", sort { $a <=> $b } map { $_*2 } 3,1,2)',  # 2,4,6
    'join(",", sort { $a <=> $b } grep { $_ % 2 } 1..6)',# 1,3,5
);
add("sort $_", prog($_)) for @sorts;

# --- Axis 18: regex features (flags, lookaround, non-greedy, backrefs) -------
# Word boundary, lookahead/lookbehind, non-greedy, /g list & count, optional,
# quantifier capture, /i /m /s flags, backreference, /e with nested calls,
# alternation, capturing/limit split.  \n in the double-quoted subject strings
# becomes a real newline in the generated program (needed for /m and /s).
my @regex2 = (
    '"Hello World" =~ /\bWorld\b/ ? "y":"n"',
    '"foobar" =~ /foo(?=bar)/ ? $& : "no"',                 # lookahead -> foo
    '"foobar" =~ /(?<=foo)bar/ ? $& : "no"',                # lookbehind -> bar
    '"aaa" =~ /a+?/ ? $& : "no"',                           # non-greedy -> a
    '"<a><b>" =~ /<(.+?)>/ ? $1 : "no"',                    # non-greedy group -> a
    'do { my $s="a1b2c3"; my @m = $s =~ /(\d)/g; "@m" }',   # 1 2 3
    '"color colour" =~ /colou?r/ ? $& : "no"',              # color
    '"2024-01-02" =~ /(\d{4})-(\d{2})-(\d{2})/ ? "$1/$2/$3" : "no"',
    'do { my $s="aXbXc"; my $c = () = $s =~ /X/g; $c }',    # 2 match count
    '"ABCabc" =~ /abc/i ? "y":"n"',
    '"line1\nline2" =~ /^line2/m ? "y":"n"',                # /m -> y
    '"a\nb" =~ /a.b/s ? "y":"n"',                           # /s dot eats newline -> y
    '"a\nb" =~ /a.b/ ? "y":"n"',                            # no /s -> n
    '"abcabc" =~ /(abc)\1/ ? "y":"n"',                      # backreference -> y
    'do { my $s="the quick"; $s =~ s/(\w+)/ucfirst($1)/ge; $s }', # The Quick
    '"cat dog bird" =~ /(cat|dog)/ ? $1 : "no"',            # cat
    'join("|", split /\s*,\s*/, "a, b ,c")',                # a|b|c
    'do { my @p = "key=val" =~ /(\w+)=(\w+)/; "@p" }',      # key val
    '"FooBar" =~ /(?<x>Foo)(?<y>Bar)/ ? "$+{x}-$+{y}" : "no"', # Foo-Bar
);
add("re2 $_", prog($_)) for @regex2;

# --- Axis 19: autovivification & nested data structures ---------------------
# Deep hash/array autoviv, push to an autovivified arrayref, mixed AoH/HoA,
# iterating/building structures in a loop.  Each prints one scalar [payload].
my @autoviv = (
    'do { my %h; $h{a}{b}=1; $h{a}{c}=2; join(",",map "$_=$h{a}{$_}",sort keys %{$h{a}}) }',
    'do { my %h; push @{$h{list}}, 1,2,3; "@{$h{list}}" }',
    'do { my @a; $a[2][1]=9; $a[2][0]=8; "$a[2][0]$a[2][1]" }',
    'do { my %h; $h{x}[0]=1; $h{x}[1]=2; "@{$h{x}}" }',
    'do { my $r={}; $r->{a}{b}{c}=42; $r->{a}{b}{c} }',
    'do { my @a=([1,2],[3,4]); $a[0][1]+$a[1][0] }',
    'do { my %h=(a=>[1,2,3]); scalar @{$h{a}} }',
    'do { my $r=[{n=>1},{n=>2}]; $r->[0]{n}+$r->[1]{n} }',
    'do { my %h; $h{$_}=$_*$_ for 1..3; join(",",map "$_=$h{$_}",sort keys %h) }',
    'do { my %seen; $seen{$_}++ for qw(a b a c a b); join(",",map "$_:$seen{$_}",sort keys %seen) }',
    'do { my @m; $m[$_]=$_**2 for 0..3; "@m" }',
    'do { my %h; push @{$h{$_%2?"odd":"even"}}, $_ for 1..6; "@{$h{odd}}|@{$h{even}}" }',
);
add("av $_", prog($_)) for @autoviv;

# --- Axis 20: numeric stringification & sprintf precision -------------------
# Float printing (Perl uses %.15g for stringification), large integers crossing
# the IV/NV boundary, sprintf rounding (banker's vs away-from-zero), %g/%e/%f.
my @numstr = (
    '0.1+0.2', '1/3', '2/3', '10/3', '1e20', '1e-20',
    '0.0000001', '1000000000000000',
    'sprintf("%.17g",0.1)', 'sprintf("%g",0.1)', 'sprintf("%.15g",1/3)',
    'sprintf("%e",0)', 'sprintf("%.0f",2.5)', 'sprintf("%.0f",3.5)',
    'sprintf("%.0f",0.5)', 'sprintf("%d",2**31)', 'sprintf("%x",2**32)',
    '0.1*3', '3*0.1', 'int(0.1*3*10)', '1.0', '1.5', '100.0',
    '"3.14"+0', '0.5**10', '10**-5', 'sqrt(2)',
    'sprintf("%s",0.1+0.2)', 'sprintf("%s",1/3)', '1234567.89', '-1234567.89',
    '1_000_000 * 1_000_000', '2**53', '2**53 + 1',
);
add("ns $_", prog($_)) for @numstr;

# --- Axis 21: short-circuit / defined-or, with side-effect ordering ---------
# ||/&&///, ||=/&&=///=, and the exact set of operands actually evaluated
# (tracked via a @log the closure pushes to) — catches eager-evaluation bugs.
my @sc = (
    'do { my $x; my $y = $x // "default"; $y }',
    'do { my $x=0; my $y = $x || "fb"; $y }',
    'do { my $x=0; my $y = $x // "fb"; $y }',
    'do { my @log; my $f=sub { push @log,$_[0]; $_[0] }; my $r=$f->(0)||$f->(1)||$f->(2); "$r|@log" }',
    'do { my @log; my $f=sub { push @log,$_[0]; $_[0] }; my $r=$f->(1)&&$f->(2)&&$f->(3); "$r|@log" }',
    'do { my $c=0; $c ||= 5; $c ||= 9; $c }',
    'do { my $h={}; $h->{x} //= 10; $h->{x} //= 20; $h->{x} }',
    'do { my @a=(1,2,3); my $x = $a[5] // "none"; $x }',
    'do { my $s = "" || 0 || "third"; $s }',
    'do { my $n = 0 // 5; $n }',
    'do { my $x = undef; $x //= do { 42 }; $x }',
    'do { my ($a,$b)=(0,2); my $r = $a || $b; $r }',
);
add("sc $_", prog($_)) for @sc;

# --- Axis 22: symbolic references (no strict refs) --------------------------
# @{NAME}/${NAME}/%{NAME} where NAME is a string (in a scalar, a literal, or a
# computed expression) is a symbolic reference to a package variable.  Exercises
# the deref + slice + element + lvalue paths across all three spellings; the
# read-back is via the SAME symbolic ref so the round-trip is self-consistent.
# (Session 245 fixed p-aref string resolution + the @{EXPR}[slice] codegen.)
my @symref = (
    # scalar symbolic deref: name via scalar / literal / computed expr
    [ 'scalar-var',   'do { $sv=7; my $n="sv"; ${$n} }' ],
    [ 'scalar-lit',   'do { $sv=7; ${"sv"} }' ],
    [ 'scalar-expr',  'do { $sv=7; ${"s"."v"} }' ],
    # whole-array deref
    [ 'arr-var',      'do { @av=(1,2,3); my $n="av"; join(",",@{$n}) }' ],
    [ 'arr-lit',      'do { @av=(1,2,3); join(",",@{"av"}) }' ],
    [ 'arr-expr',     'do { @av=(1,2,3); join(",",@{"a"."v"}) }' ],
    # array slice rvalue
    [ 'aslice-var',   'do { @av=(10,20,30,40); my $n="av"; join(",",@{$n}[1,3]) }' ],
    [ 'aslice-lit',   'do { @av=(10,20,30,40); join(",",@{"av"}[1,3]) }' ],
    [ 'aslice-expr',  'do { @av=(10,20,30,40); join(",",@{"a"."v"}[1,3]) }' ],
    # array slice lvalue (write + read back via the same symbolic ref)
    [ 'aslice-set-var',  'do { my $n="bv"; @{$n}[1,2]=("X","Y"); join(",",map { defined $_?$_:"u" } @{$n}[0,1,2]) }' ],
    [ 'aslice-set-lit',  'do { @{"bv"}[1,2]=("X","Y"); join(",",map { defined $_?$_:"u" } @{"bv"}[0,1,2]) }' ],
    [ 'aslice-set-expr', 'do { @{"b"."v"}[1,2]=("X","Y"); join(",",map { defined $_?$_:"u" } @{"b"."v"}[0,1,2]) }' ],
    # array element write with a variable / computed index
    [ 'aelem-vidx',   'do { my $n="cv"; my $i=2; @{$n}[$i]="Z"; @{$n}[$i+1]="W"; join(",",map { defined $_?$_:"u" } @{$n}[0..3]) }' ],
    # cross-check: write via symbolic ref, read via the plain package array
    [ 'set-read-plain', 'do { my $n="dv"; @{$n}[0,1]=("p","q"); "$dv[0]$dv[1]" }' ],
    # hash slice rvalue / lvalue
    [ 'hslice-var',   'do { %hv=(a=>1,b=>2,c=>3); my $n="hv"; join(",",@{$n}{qw(a c)}) }' ],
    [ 'hslice-lit',   'do { %hv=(a=>1,b=>2,c=>3); join(",",@{"hv"}{qw(a c)}) }' ],
    [ 'hslice-set',   'do { my $n="iv"; @{$n}{qw(x y)}=(8,9); join(",",@{$n}{qw(x y)}) }' ],
);
add("symref[$_->[0]] $_->[1]", prog($_->[1])) for @symref;

# --- Axis 23: postfix dereference + nested deref chains ---------------------
# The ->@* / ->%* / ->$* / ->@[..] / ->%{..} / ->$#* postfix-deref family, and
# chained derefs through nested AoH/HoA structures.  Full-hash flatten (->%*)
# is excluded — its key order is unspecified, which would be false-positive
# noise; explicit-key slices preserve their argument order.
my $NEST = 'my $d = { list => [10,20,30], map => { x => 1, y => 2 } };';
my $AOA  = 'my $m = [[1,2],[3,4]];';
my @postfix = (
    [ '$ar->@*',              $AR,   'l' ],   # 10,20,30
    [ '$ar->$#*',             $AR,   's' ],   # 2
    [ '$ar->@[0,2]',          $AR,   'l' ],   # 10,30
    [ '$ar->@[-1,-2]',        $AR,   'l' ],   # 30,20
    [ 'scalar $ar->@*',       $AR,   's' ],   # 3
    [ '$hr->@{qw(a c)}',      $HR,   'l' ],   # 1,3
    [ '$hr->%{qw(a c)}',      $HR,   'l' ],   # a,1,c,3 (kv-slice, key order = args)
    [ '$d->{list}[1]',        $NEST, 's' ],   # 20
    [ '$d->{list}->[1]',      $NEST, 's' ],   # 20
    [ '$d->{list}->@*',       $NEST, 'l' ],   # 10,20,30
    [ '$d->{list}->@[0,2]',   $NEST, 'l' ],   # 10,30
    [ 'scalar $d->{list}->@*',$NEST, 's' ],   # 3
    [ '$d->{map}{x}',         $NEST, 's' ],   # 1
    [ '$d->{map}->@{qw(x y)}',$NEST, 'l' ],   # 1,2
    [ '$m->[1][0]',           $AOA,  's' ],   # 3
    [ '$m->[1]->@*',          $AOA,  'l' ],   # 3,4
    [ '$#{$d->{list}}',       $NEST, 's' ],   # 2
);
for my $p (@postfix) {
    my ($expr, $pre, $kind) = @$p;
    add("postfix $expr", $kind eq 'l' ? prog_list($expr, $pre) : prog($expr, $pre));
}

# --- Axis 18: call shapes x sub-definition shapes (s304 #80 class) ----------
# Every caller argument shape against every sub-definition shape: PCL's
# uniform convention is "callers pass containers raw, the CALLEE flattens";
# the &optional signature fast path silently broke that for aggregate args
# (f(@a) / f(@_) delegation) and no gate test covered it.  Perl itself
# rejects some combinations (($$) prototype vs list args) — the oracle skip
# handles those.
{
    my @subdefs = (
        [ 'sig',      'sub f2 { my ($x, $y) = @_; return "$x/$y"; }' ],
        [ 'shiftrun', 'sub f2 { my $x = shift; my $y = shift; return "$x/$y"; }' ],
        [ 'slurpy',   'sub f2 { my ($x, @r) = @_; return "$x/" . join("+", @r); }' ],
        [ 'elems',    'sub f2 { return "$_[0]/$_[1]"; }' ],
        [ 'proto',    'sub f2 ($$) { my ($x, $y) = @_; return "$x/$y"; }' ],
    );
    my @callshapes = (
        [ 'scalars',   'f2(1, 2)',       '' ],
        [ 'array',     'f2(@aa)',        'my @aa = (1, 2);' ],
        [ 'mix-post',  'f2(9, @a1)',     'my @a1 = (8);' ],
        [ 'mix-pre',   'f2(@a1, 7)',     'my @a1 = (6);' ],
        [ 'hash',      'f2(%hh)',        'my %hh = (k => 3);' ],
        [ 'empty',     'f2()',           '' ],
        [ 'one-arg',   'f2(5)',          '' ],
        [ 'delegate',  'g2(3, 4)',       'sub g2 { return f2(@_); }' ],
        [ 'coderef',   '$cr->(@aa)',     'my @aa = (1, 2); my $cr = \&f2;' ],
        [ 'amp-call',  '&f2(@aa)',       'my @aa = (1, 2);' ],
        [ 'nested',    'f2(f2(1,2), 3)', '' ],
    );
    for my $sd (@subdefs) {
        for my $cs (@callshapes) {
            add("call $sd->[0]/$cs->[0]",
                prog($cs->[1], "$cs->[2]\n$sd->[1]\n"));
        }
    }
}

# --- Axis 19: pragma visibility (use strict on/off x bareword shapes) -------
# PExpr's bareword-after-binary-op disambiguation is strict_subs-gated;
# s304's _premerge_strict_pragma made the pragma visible to v2's
# ahead-of-stream lowering.  Same shape with and without strict must both
# match perl (which resolves barewords against subs LIVE in the stash, so
# some no-strict rows are expected known divergences — they cluster).
{
    my @bareshapes = (
        [ 'const-match',  'BEGIN { my $rx = qr/^\w+$/; *_cnst = sub () { $rx }; }',
                          '"Pt" =~ _cnst ? "match" : "nomatch"' ],
        [ 'const-concat', 'BEGIN { *_cval = sub () { "CV" }; }',
                          '_cval . "-tail"' ],
        [ 'const-num',    'BEGIN { *_cnum = sub () { 42 }; }',
                          '_cnum + 1' ],
        [ 'use-constant', 'use constant KC => 7;',
                          'KC * 2' ],
        [ 'alias-call',   'sub real9 { "R9" } BEGIN { *ali9 = \&real9; }',
                          'ali9() . "!"' ],
        [ 'alias-bare',   'sub real8 { "R8" } BEGIN { *ali8 = \&real8; }',
                          'ali8 . "!"' ],
    );
    for my $bs (@bareshapes) {
        my ($name, $pre, $expr) = @$bs;
        add("pragma strict/$name",   prog($expr, "use strict; use warnings;\n$pre\n"));
        add("pragma nostrict/$name", prog($expr, "no strict;\n$pre\n"));
    }
}

# --- Axis 20: shadows / captures / interpolation (rename-engine shapes) -----
# The promotion/rename passes (shadow renames, capture promotion, interp
# rewrite) are what the v1-parity oracle kept catching by hand in E1.
sub prog_stmts {   # statements that assign $R; prints [$R]
    my ($stmts) = @_;
    return "my \$R;\n$stmts\n\$R = 'undef' unless defined \$R;\nprint \"[\$R]\\n\";\n";
}
{
    my @shapes = (
        [ 'shadow-basic',
          'my $x = 1; { my $x = 2; $R .= "i$x"; } $R .= "o$x";' ],
        [ 'shadow-deep',
          'my $x = 1; { my $x = 2; { my $x = 3; $R .= $x; } $R .= $x; } $R .= $x;' ],
        [ 'shadow-if',
          'my $x = "a"; if (1) { my $x = "b"; $R = $x; } $R .= $x;' ],
        [ 'capture-read',
          'my $x = 5; my $c = sub { $x }; $x = 6; $R = $c->();' ],
        [ 'capture-write',
          'my $x = 5; my $c = sub { $x++ }; $c->(); $c->(); $R = $x;' ],
        [ 'capture-shadow',
          'my $x = 1; my $c; { my $x = 2; $c = sub { $x }; } $x = 9; $R = $c->() . "/$x";' ],
        [ 'foreach-capture',
          'my @f; for my $i (1..3) { push @f, sub { $i } } $R = join ",", map { $_->() } @f;' ],
        [ 'interp-shadow',
          'my $x = "out"; { my $x = "in"; $R = "v=$x"; } $R .= "+$x";' ],
        [ 'interp-array',
          'my @a = (1,2); my $x = 3; $R = "@a-$x-$a[1]-${x}s";' ],
        [ 'interp-hash',
          'my %h = (k=>7); $R = "h=$h{k}!";' ],
        [ 'interp-capture',
          'my $x = 5; my $c = sub { "cap=$x" }; $x = 8; $R = $c->();' ],
        [ 'state-in-sub',
          'use feature "state"; sub tick9 { state $n = 0; return ++$n; } tick9(); $R = tick9();' ],
        [ 'local-our',
          'our $g9 = 1; sub rd9 { $g9 } { local $g9 = 2; $R = rd9(); } $R .= "/$g9";' ],
    );
    add("rename $_->[0]", prog_stmts($_->[1])) for @shapes;
}

# --- Axis 21: string-eval shapes (E3 eval-mode coverage) --------------------
# Runtime `eval "..."` now transpiles through v2 (per-eval v1 retry); these
# pin the capture protocol end-to-end: read/write-back, closures built inside
# the eval, subs the eval defines, cross-eval globals, $a/$b in sort.
{
    my @shapes = (
        [ 'read',        'my $x = 5; $R = eval q{$x + 1};' ],
        [ 'write-back',  'my $x = 5; eval q{$x = 9}; $R = $x;' ],
        [ 'closure',     'my $x = "S"; my $c = eval q{sub { "got:$x" }}; $R = $c->();' ],
        [ 'defsub-clo',  'eval q{sub mk9 { my $s = shift; return sub { $s }; }}; $R = mk9("z")->();' ],
        [ 'glob-persist','eval q{$gp9 = 4}; $R = eval q{$gp9 + 1};' ],
        [ 'sort-ab',     '$R = eval q{join ",", sort { $b <=> $a } (1,3,2)};' ],
        [ 'array-read',  'my @a = (1,2); $R = eval q{join "-", @a};' ],
        [ 'hash-write',  'my %h; eval q{$h{k} = 3}; $R = $h{k};' ],
        [ 'inner-my',    '$R = eval q{my $z = 7; $z * 2};' ],
        [ 'die-caught',  '$R = eval q{die "boom\n"}; $R = defined $R ? $R : "E:" . $@;' ],
        [ 'shadow',      'my $x = 1; { my $x = 2; $R = eval q{$x}; }' ],
    );
    add("eval $_->[0]", prog_stmts($_->[1])) for @shapes;
}

# --- Axis 22: two-file module mode (the Moo-chain class) --------------------
# Import/export mechanisms cross the module boundary the single-file axes
# cannot reach: Exporter (direct and glob-aliased import), BEGIN-installed
# constant subs under strict, and the cross-file calling convention.
{
    my $L = "use lib \"$WORK\";\n";
    add('module exporter-direct',
        $L . prog_stmts('use TFm1 qw(util1); $R = util1(7);'),
        { TFm1 => <<'PM' });
package TFm1;
use strict; use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(util1);
sub util1 { return "U1:$_[0]"; }
1;
PM
    add('module exporter-globalias',
        $L . prog_stmts('use TFm2 qw(util2); $R = util2();'),
        { TFm2 => <<'PM' });
package TFm2;
use strict; use warnings;
use Exporter ();
BEGIN { *import = \&Exporter::import }
our @EXPORT_OK = qw(util2);
sub util2 { return "U2"; }
1;
PM
    add('module exporter-default',
        $L . prog_stmts('use TFm3; $R = util3("d");'),
        { TFm3 => <<'PM' });
package TFm3;
use strict; use warnings;
use Exporter 'import';
our @EXPORT = qw(util3);
sub util3 { return "U3:$_[0]"; }
1;
PM
    add('module const-sub-strict',
        $L . prog_stmts('use TFm4 (); $R = TFm4::chk("Word") . "/" . TFm4::chk("no space");'),
        { TFm4 => <<'PM' });
package TFm4;
use strict; use warnings;
BEGIN { my $rx = qr/\A\w+\z/; *_mrx = sub () { $rx }; }
sub chk { return $_[0] =~ _mrx ? "ok" : "no"; }
1;
PM
    add('module sig-cross-file-array',
        $L . prog_stmts('use TFm5 (); my @a = (1, 2); $R = TFm5::pair(@a);'),
        { TFm5 => <<'PM' });
package TFm5;
use strict; use warnings;
sub pair { my ($x, $y) = @_; return "$x/$y"; }
1;
PM
    add('module sig-cross-file-delegate',
        $L . prog_stmts('use TFm6 (); $R = TFm6::outer(3, 4);'),
        { TFm6 => <<'PM' });
package TFm6;
use strict; use warnings;
sub pair6 { my ($x, $y) = @_; return "$x/$y"; }
sub outer { return pair6(@_); }
1;
PM
    add('module proto-cross-file',
        $L . prog_stmts('use TFm7 (); $R = TFm7::two(5, 6);'),
        { TFm7 => <<'PM' });
package TFm7;
use strict; use warnings;
sub two ($$) { my ($x, $y) = @_; return $x + $y; }
1;
PM
    add('module import-into-caller',
        $L . prog_stmts('use TFm8; $R = defined &gift8 ? gift8() : "missing";'),
        { TFm8 => <<'PM' });
package TFm8;
use strict; use warnings;
sub import { my $pkg = caller; no strict 'refs'; *{"${pkg}::gift8"} = sub { "G8" }; }
1;
PM
}

$LIMIT and @snips = @snips[0 .. $LIMIT-1];

# ---------------------------------------------------------------------------
# Oracle: run each snippet through real perl (skip if perl rejects it).
# ---------------------------------------------------------------------------
my $n = @snips;
print STDERR "Generated $n snippets. Computing perl oracle...\n";
my @active;
for my $i (0 .. $#snips) {
    my $s = $snips[$i];
    my $f = "$WORK/s$i.pl";
    open my $fh, '>', $f or die $!;
    print $fh $s->{code};
    close $fh;
    if ($s->{mods}) {
        for my $mname (sort keys %{ $s->{mods} }) {
            open my $mfh, '>', "$WORK/$mname.pm" or die $!;
            print $mfh $s->{mods}{$mname};
            close $mfh;
        }
    }
    $s->{file} = $f;
    my $out = `perl $f 2>$WORK/s$i.perr`;
    if ($? != 0) { $s->{skip} = 1; next; }   # perl rejected → not valid Perl
    $s->{perl} = extract($out);
    push @active, $i;
}
print STDERR scalar(@active), " valid (", $n - @active, " skipped as invalid Perl)\n";
print STDERR "Running PCL on $JOBS workers...\n";

# ---------------------------------------------------------------------------
# Run PCL (./runpcl) over the active snippets with a simple fork pool.
# ---------------------------------------------------------------------------
my %pid2i;
my @queue = @active;
my $done = 0;
sub reap {
    my $pid = wait;
    return if $pid < 0;
    my $i = delete $pid2i{$pid};
    return unless defined $i;
    my $r = do { local $/; open my $f,'<',"$WORK/s$i.pcl" or return; <$f> };
    $snips[$i]{pcl} = extract($r // '');
    $done++;
    print STDERR "\r  $done/", scalar(@active), " " if $done % 5 == 0;
}
while (@queue || %pid2i) {
    while (@queue && keys(%pid2i) < $JOBS) {
        my $i = shift @queue;
        my $pid = fork;
        if (!$pid) {   # child
            my $out = `$RUNPL $snips[$i]{file} 2>&1`;
            open my $fh,'>',"$WORK/s$i.pcl"; print $fh $out; close $fh;
            exit 0;
        }
        $pid2i{$pid} = $i;
    }
    reap();
}
print STDERR "\n";

# ---------------------------------------------------------------------------
# normalize a program's stdout to the [...] payload (or a marker for failures)
# ---------------------------------------------------------------------------
sub extract {
    my ($out) = @_;
    $out //= '';
    return 'PARSE-ERROR'   if $out =~ /PARSE ERROR|unknown type|Transpile failed/i;
    return 'CL-ERROR'      if $out =~ /UNDEFINED-FUNCTION|SIMPLE-ERROR|debugger invoked|unhandled|Unhandled/i;
    my $val;
    if ($out =~ /\[([^\]]*)\]/) { $val = $1; }
    else { $out =~ s/\s+\z//; $out =~ s/\A\s+//; $val = $out eq '' ? '(empty)' : "?:$out"; }
    # Normalize reference stringifications: the hex address always differs
    # between perl and PCL, so compare only the ref TYPE (ARRAY/HASH/REF/...).
    $val =~ s/\b((?:ARRAY|HASH|CODE|REF|SCALAR|GLOB|Regexp|[\w:]+)\()0x[0-9a-fA-F]+\)/${1}0xADDR)/g;
    return $val;
}

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
my (@mism, $ok);
for my $i (@active) {
    my $s = $snips[$i];
    my $p = $s->{perl} // '?';
    my $c = $s->{pcl}  // '(no-output)';
    if ($p eq $c) { $ok++; next; }
    push @mism, $s;
}

# classify a mismatch into a root-cause cluster signature
sub signature {
    my ($s) = @_;
    my ($p, $c) = ($s->{perl}, $s->{pcl} // '(no-output)');
    return 'PCL parse error (perl accepts)'   if $c eq 'PARSE-ERROR';
    return 'PCL runtime error'                if $c eq 'CL-ERROR';
    return "false comparison: '' vs undef"    if $p eq '' && $c eq 'undef';
    return 'float (**) vs exact bigint'
        if $p =~ /e[+-]\d/ && $c =~ /^-?\d+$/;
    return 'numeric format (float vs int/precision)'
        if $p =~ /^-?[\d.]+(e[+-]\d+)?$/ && $c =~ /^-?[\d.]+(e[+-]\d+)?$/;
    return "other: perl=[$p] pcl=[$c]";
}

my %cluster;
for my $s (@mism) { push @{ $cluster{ signature($s) } }, $s; }

print "\n", "="x72, "\n";
printf "RESULT: %d valid snippets, %d match, %d MISMATCH in %d clusters\n",
    scalar(@active), $ok, scalar(@mism), scalar(keys %cluster);
print "="x72, "\n";

for my $sig (sort { @{$cluster{$b}} <=> @{$cluster{$a}} } keys %cluster) {
    my @g = @{ $cluster{$sig} };
    printf "\n### [%d] %s\n", scalar(@g), $sig;
    my $shown = 0;
    for my $s (@g) {
        last if $shown++ >= 4;
        (my $code1 = $s->{code}) =~ s/\n/ /g; $code1 =~ s/\s+/ /g;
        $code1 =~ s/ \$r = 'undef'.*//;   # trim the boilerplate tail
        printf "  %-46s perl=[%s] pcl=[%s]\n",
            ($s->{desc} =~ /^(.{0,46})/)[0], $s->{perl}, $s->{pcl} // '(none)';
    }
    printf "  ... and %d more\n", @g - 4 if @g > 4;
}
print "\n";

remove_tree($WORK);
exit(scalar(@mism) ? 1 : 0);
