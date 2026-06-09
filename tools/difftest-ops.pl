#!/usr/bin/env perl
# difftest-ops.pl — differential test of PCL against real `perl` (the oracle)
# for operator precedence / associativity / ternary nesting / named-unary forms.
#
# For each generated snippet:  run it through real `perl` AND through PCL
# (./runpl), normalize, and report only the MISMATCHES (incl. PCL parse errors).
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
my $RUNPL = "$ROOT/runpl";
my $WORK  = "/tmp/difftest_$$";
make_path($WORK);

# ---------------------------------------------------------------------------
# Snippet generators.  Each snippet ends by printing exactly "[VALUE]\n" so the
# comparison is robust against surrounding noise; undef prints as [undef].
# ---------------------------------------------------------------------------
my @snips;   # {desc, code}
sub add { push @snips, { desc => $_[0], code => $_[1] }; }

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
    $s->{file} = $f;
    my $out = `perl $f 2>$WORK/s$i.perr`;
    if ($? != 0) { $s->{skip} = 1; next; }   # perl rejected → not valid Perl
    $s->{perl} = extract($out);
    push @active, $i;
}
print STDERR scalar(@active), " valid (", $n - @active, " skipped as invalid Perl)\n";
print STDERR "Running PCL on $JOBS workers...\n";

# ---------------------------------------------------------------------------
# Run PCL (./runpl) over the active snippets with a simple fork pool.
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
    if ($out =~ /\[([^\]]*)\]/) { return $1; }
    $out =~ s/\s+\z//; $out =~ s/\A\s+//;
    return $out eq '' ? '(empty)' : "?:$out";
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
