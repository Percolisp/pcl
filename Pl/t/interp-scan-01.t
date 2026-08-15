#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# interp-scan-01.t — task #237 (Fable half): the shared variable-reference
# event scanner (Pl::InterpScan) and its intuit_more/regcurly port, verified
# against LIVE PERL.
#
# Three layers, one row table:
#   1. PROBE TABLE — every recorded verdict is re-derived from the running
#      perl each time this file runs (stringified qr / dq result against
#      marker fixtures), so a perl-behavior drift fails loudly here instead
#      of silently invalidating the scanner.  The verdicts were first taken
#      s382 on perl 5.40.3 against its own toke.c/regcomp.c source.
#   2. CLASSIFIER — Pl::InterpScan::intuit_more must return the same
#      subscript-vs-regex-syntax verdict the probe demonstrated.
#   3. SCANNER — event shapes (form/canon/chain/spans) for the same texts.
#
# The findings these rows pin down (details in docs/interp-scan.md):
#   * {2} {2,} {,3} { 2, 3 } are QUANTIFIERS after a var in a pattern
#     (regcurly), never subscripts; {k} {$i} {-3} {} are subscripts.
#   * Only the FIRST bracket group is classified — continuations always
#     bind ($m[0][abc] dies on the bareword; $h2{k}{2,3} dies Not-a-HASH).
#   * ${x}[0] / @{x}[0] / ${ar}->@* — braces CLOSE the reference in BOTH
#     modes; ${m}[0] under strict dies on the SCALAR $m.
#   * The weigher's symbol-table hook is real: [\n@foo] flips verdict with
#     @foo's existence.
#   * In patterns: $ before ()|/end is an anchor, @+/@- stay literal, $"
#     and $] DO interpolate, \c does not hide a following $.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib "$RealBin/../..";
use Pl::InterpScan qw(scan intuit_more);

# Package names the probe fixture declares — the known_name oracle for the
# classifier must agree with what gv_fetchpvn_flags sees in the probe.
my %KNOWN = map { $_ => 1 } qw(x m h2 ar hr sref i sep foo);
my %OPT = (in_regex => 1, known_name => sub { $KNOWN{$_[0]} });

# ── The row table ──────────────────────────────────────────────────────────
# [ id, mode, body, expect_re, iv ]
#   mode:  rx / rxpd (qr, postderef feature) / dq / dqpd
#   expect_re: the live-perl output must match (OK <result> | DIE <error>)
#   iv:    [pos_after_var, verdict] — intuit_more assertion (regex rows)
my @ROWS = (
  # [ ... ] after $x in a pattern
  ['rx_plain',      'rx', '$x',            qr/^OK \(\?\^:SX\)$/],
  ['rx_abc',        'rx', '$x[abc]',       qr/SX\[abc\]/,        [2, 0]],
  ['rx_caret',      'rx', '$x[^a]',        qr/SX\[\^a\]/,        [2, 0]],
  ['rx_empty_br',   'rx', '$x[]',          qr/^DIE.*Unmatched \[/, [2, 0]],
  ['rx_0',          'rx', '$x[0]',         qr/:AX0\)/,           [2, 1]],
  ['rx_42',         'rx', '$x[42]',        qr/^OK \(\?\^:\)$/,   [2, 1]],
  ['rx_89',         'rx', '$x[89]',        qr/^OK \(\?\^:\)$/,   [2, 1]],
  ['rx_100',        'rx', '$x[100]',       qr/SX\[100\]/,        [2, 0]],
  ['rx_123',        'rx', '$x[123]',       qr/SX\[123\]/,        [2, 0]],
  ['rx_neg1',       'rx', '$x[-1]',        qr/:AX2\)/,           [2, 1]],
  ['rx_neg12',      'rx', '$x[-12]',       qr/SX\[-12\]/,        [2, 0]],
  ['rx_ivar',       'rx', '$x[$i]',        qr/:AX1\)/,           [2, 1]],
  ['rx_foovar',     'rx', '$x[$foo]',      qr/:AX0\)/,           [2, 1]],
  ['rx_uscore',     'rx', '$x[$_]',        qr/:AX0\)/,           [2, 1]],
  ['rx_punctvar',   'rx', '$x[$-]',        qr/:AX0\)/,           [2, 1]],
  ['rx_az',         'rx', '$x[a-z]',       qr/SX\[a-z\]/,        [2, 0]],
  ['rx_09',         'rx', '$x[0-9]',       qr/SX\[0-9\]/,        [2, 0]],
  ['rx_w',          'rx', '$x[\w]',        qr/SX\[\\w\]/,        [2, 0]],
  ['rx_n',          'rx', '$x[\n]',        qr/SX\[\\n\]/,        [2, 0]],
  ['rx_kw',         'rx', '$x[eq]',        qr/^DIE.*syntax error/, [2, 1]],
  ['rx_dollar_end', 'rx', '$x[abc$]',      qr/^DIE.*Unmatched \[.*abc5\./, [2, 0]],
  ['rx_mixed',      'rx', '$x[\w$sep]',    qr/SX\[\\w;\]/,       [2, 0]],
  ['rx_arrfoo',     'rx', '$x[@foo]',      qr/:AX2\)/,           [2, 1]],
  ['rx_kn_known',   'rx', '$x[\n@foo]',    qr/^DIE.*(Bareword|syntax)/, [2, 1]],
  ['rx_kn_unknown', 'rx', '$x[\n@main::zqx]', qr/^OK \(\?\^:SX\[\\n\]\)$/, [2, 0]],
  # { ... } after $x in a pattern
  ['rx_q2',         'rx', '$x{2}',         qr/SX\{2\}/,          [2, 0]],
  ['rx_q2c',        'rx', '$x{2,}',        qr/SX\{2,\}/,         [2, 0]],
  ['rx_q23',        'rx', '$x{2,3}',       qr/SX\{2,3\}/,        [2, 0]],
  ['rx_qc3',        'rx', '$x{,3}',        qr/SX\{,3\}/,         [2, 0]],
  ['rx_qsp',        'rx', '$x{ 2, 3 }',    qr/SX\{ 2, 3 \}/,     [2, 0]],
  ['rx_qempty',     'rx', '$x{}',          qr/^DIE.*syntax error/, [2, 1]],
  ['rx_hk',         'rx', '$x{k}',         qr/:HXk\)/,           [2, 1]],
  ['rx_hk1',        'rx', '$x{k1}',        qr/:HXk1\)/,          [2, 1]],
  ['rx_hkq',        'rx', '$x{\'k\'}',     qr/:HXk\)/,           [2, 1]],
  ['rx_hvar',       'rx', '$x{$i}',        qr/:HX1\)/,           [2, 1]],
  ['rx_hneg',       'rx', '$x{-3}',        qr/:HXm3\)/,          [2, 1]],
  ['rx_h2x',        'rx', '$x{2x}',        qr/^DIE.*syntax error/, [2, 1]],
  # chains: first group classified, continuations bind unconditionally
  ['rx_chain_nn',   'rx', '$m[0][1]',      qr/:M01\)/,           [2, 1]],
  ['rx_chain_ncls', 'rx', '$m[0][abc]',    qr/^DIE.*Bareword/],
  ['rx_chain_hh',   'rx', '$h2{k}{v}',     qr/:V2\)/],
  ['rx_chain_hq',   'rx', '$h2{k}{2,3}',   qr/^OK \(\?\^:\)$/],
  ['rx_chain_mixed','rx', '$m[0]{2,3}',    qr/^DIE.*Not a HASH reference/],
  ['rx_arrow_n',    'rx', '$ar->[0]',      qr/:R0\)/,            [3, 1]],
  ['rx_arrow_bw',   'rx', '$ar->[abc]',    qr/^DIE.*Bareword/,   [3, 1]],
  ['rx_arrow_h',    'rx', '$hr->{k}',      qr/:RK\)/,            [3, 1]],
  # braces close the reference
  ['rx_braced_n',   'rx', '${x}[0]',       qr/SX\[0\]/],
  ['rx_braced_q',   'rx', '${x}{2}',       qr/SX\{2\}/],
  ['dq_braced_n',   'dq', '${x}[0]',       qr/^OK SX\[0\]$/],
  ['dq_braced_h',   'dq', '${x}{k}',       qr/^OK SX\{k\}$/],
  ['dq_braced_m',   'dq', '${m}[0]',       qr/^DIE.*Global symbol "\$m"/],
  ['dq_braced_ws',  'dq', '${ x }',        qr/^OK SX$/],
  ['rxpd_braced_pd','rxpd', '${ar}->@*',   qr/ARRAY\(0x.*->\@\*/],
  ['dqpd_braced_pd','dqpd', '${ar}->@*',   qr/ARRAY\(0x.*->\@\*/],
  # slices
  ['rx_slice_n',    'rx', '@x[1]',         qr/:AX1\)/,           [2, 1]],
  ['rx_slice_cls',  'rx', '@x[abc]',       qr/AX0 AX1 AX2\[abc\]/, [2, 0]],
  ['rx_slice_h',    'rx', '@x{k}',         qr/:HXk\)/,           [2, 1]],
  ['rx_slice_cont', 'rx', '@x[1][0]',      qr/^DIE.*syntax error/],
  ['dq_slice_cont', 'dq', '@x[1][0]',      qr/^DIE.*syntax error/],
  # other reference forms in patterns
  ['rx_lastidx',    'rx', '$#x',           qr/^OK \(\?\^:2\)$/],
  ['rx_deref',      'rx', '$$sref',        qr/:SREF\)/],
  ['rx_pid_end',    'rx', 'a$$',           qr/^OK \(\?\^:a\d+\)$/],
  ['rx_bslash_c',   'rx', '\c$x',          qr/\\cSX/],
  ['rx_punct_dq',   'rx', 'a$"b',          qr/^OK \(\?\^:a b\)$/],
  ['rx_plus_sub',   'rx', '$+[0]',         qr/^OK \(\?\^:2\)$/],
  ['rx_named_cap',  'rx', '$+{L}',         qr/:aa\)/],
  # starts that stay literal in patterns
  ['rx_anchor_alt', 'rx', 'a$|b',          qr/^OK \(\?\^:a\$\|b\)$/],
  ['rx_anchor_par', 'rx', '(a$)',          qr/\(a\$\)/],
  ['rx_at_minus',   'rx', 'a@-',           qr/^OK \(\?\^:a\@-\)$/],
  ['rx_at_plus',    'rx', 'a@+',           qr/^OK \(\?\^:a\@\+\)$/],
  # postderef
  ['rx_pd_off',     'rx',   '$ar->@*',     qr/ARRAY\(0x.*->\@\*/],
  ['rx_pd_on',      'rxpd', '$ar->@*',     qr/:R0 R1\)/],
  ['dqpd_plain_pd', 'dqpd', '$ar->@*',     qr/^OK R0 R1$/],
  # dq mode: always-subscript, and the shapes StringInterpolation narrows
  ['dq_h2',         'dq', '$x{2}',         qr/^OK HX2$/],
  ['dq_q23',        'dq', '$x{2,3}',       qr/^OK\s*$/],
  ['dq_abc',        'dq', '$x[abc]',       qr/^DIE.*Bareword/],
  ['dq_chain',      'dq', '$m[0][1]',      qr/^OK M01$/],
  ['dq_deref_el',   'dq', '$$ar[0]',       qr/^OK R0$/],
  ['dq_deref_sl',   'dq', '@$ar[0]',       qr/^OK R0$/],
  ['dq_atbraced_n', 'dq', '@{x}[0]',       qr/^OK AX0 AX1 AX2\[0\]$/],
  ['dq_at_minus',   'dq', 'a@-b',          qr/^OK a0 0b$/],
  ['dq_at_minus_i', 'dq', 'a@-[1]b',       qr/^OK a0b$/],
  ['dq_at_plus_i',  'dq', 'a$+[1]b',       qr/^OK a2b$/],
  ['dq_named_cap',  'dq', '$+{L}',         qr/^OK aa$/],
  ['dq_main',       'dq', '$::i',          qr/^OK 1$/],
  ['dq_digit_tail', 'dq', '$1a',           qr/^OK aaa$/],
  ['dq_qualified',  'dq', '$main::x',      qr/^OK SX$/],
  ['dq_dangling',   'dq', 'a$Foo::.b',     qr/^OK a\.b$/],
  ['dq_ddd',        'dq', '$$$sref',       qr/^DIE.*SCALAR ref/],
);

# ── Layer 1: the live-perl oracle ──────────────────────────────────────────
my $probe = <<'PROBE';
use strict;
no warnings;
our @x = ('AX0', 'AX1', 'AX2');
our $x = 'SX';
our %x = (1 => 'HX1', 2 => 'HX2', k => 'HXk', k1 => 'HXk1', '-3' => 'HXm3');
our @m = (['M00', 'M01'], ['M10', 'M11']);
our %h2 = (k => {v => 'V2'});
our $ar = ['R0', 'R1'];
our $hr = {k => 'RK'};
our $sref = \'SREF';
our $i = 1;
our $sep = ';';
our @foo = ('F0', 'F1');
our $foo = 'SF';
open my $fh, '<', $ARGV[0] or die "rows: $!";
while (my $line = <$fh>) {
  chomp $line;
  my ($id, $mode, $body) = split /\t/, $line, 3;
  my $code =
      $mode eq 'rx'   ? qq{"aab" =~ /(?<L>a+)/; qr/$body/}
    : $mode eq 'rxpd' ? qq{use feature 'postderef_qq'; qr/$body/}
    : $mode eq 'dqpd' ? qq{use feature 'postderef_qq'; "$body"}
    :                   qq{"aab" =~ /(?<L>a+)/; "$body"};
  my $v = eval $code;
  my $out = defined $v ? "OK $v" : "DIE $@";
  $out =~ tr/\n/ /;
  print "$id\t$out\n";
}
PROBE

my ($rfh, $rows_file) = tempfile(SUFFIX => '.tsv', UNLINK => 1);
print $rfh join("\t", @{$_}[0, 1, 2]), "\n" for @ROWS;
close $rfh;
my ($pfh, $probe_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $pfh $probe;
close $pfh;

my %observed;
for my $line (split /\n/, `perl $probe_file $rows_file`) {
  my ($id, $out) = split /\t/, $line, 2;
  $observed{$id} = $out;
}

for my $r (@ROWS) {
  my ($id, undef, undef, $expect) = @$r;
  like($observed{$id} // '(no output)', $expect, "perl oracle: $id");
}

# ── Layer 2: the classifier matches the probed verdicts ────────────────────
for my $r (@ROWS) {
  my ($id, undef, $body, undef, $iv) = @$r;
  next unless $iv;
  my ($pos, $verdict) = @$iv;
  is(intuit_more($body, $pos, %OPT) ? 1 : 0, $verdict,
     "intuit_more: $id");
}

# regcurly directly (position past the '$x' prefix)
is(!!Pl::InterpScan::regcurly('$x{ 2, 3 }', 2), 1, 'regcurly: blanks ok');
is(!!Pl::InterpScan::regcurly('$x{,3}', 2),     1, 'regcurly: max-only ok');
is(!!Pl::InterpScan::regcurly('$x{}', 2),       '', 'regcurly: needs a number');
is(!!Pl::InterpScan::regcurly('$x{k}', 2),      '', 'regcurly: word is no bound');

# ── Layer 3: scanner event shapes ──────────────────────────────────────────
# sig: sigil,form,name,canon,#chain,slice,span,postderef-what
sub sigs {
  my ($text, %opt) = @_;
  return join ' | ', map {
    join(',', $_->{sigil}, $_->{form}, $_->{name} // '-', $_->{canon} // '-',
         scalar @{ $_->{chain} }, $_->{slice},
         $_->{span}[0] . '-' . $_->{span}[1],
         $_->{postderef} ? $_->{postderef}{what} : '-');
  } @{ scan($text, %opt) };
}

my @SHAPES = (
  # regex mode — the #237 target shapes
  ['\G$_[1]',    'rx', '$,plain,_,@_,1,0,2-7,-'],
  ['$x[abc]',    'rx', '$,plain,x,$x,0,0,0-2,-'],
  ['$x[^a]',     'rx', '$,plain,x,$x,0,0,0-2,-'],
  ['$x{2,3}',    'rx', '$,plain,x,$x,0,0,0-2,-'],
  ['$x{2}',      'rx', '$,plain,x,$x,0,0,0-2,-'],
  ['$h2{k}',     'rx', '$,plain,h2,%h2,1,0,0-6,-'],
  ['$m[0][1]',   'rx', '$,plain,m,@m,2,0,0-8,-'],
  ['${x}[0]',    'rx', '$,braced,x,$x,0,0,0-4,-'],
  ['@x[1]',      'rx', '@,plain,x,@x,1,1,0-5,-'],
  ['@x{k}',      'rx', '@,plain,x,%x,1,1,0-5,-'],
  ['$ar->[0]',   'rx', '$,plain,ar,$ar,1,0,0-8,-'],
  ['$+[0]',      'rx', '$,magic,+,-,1,0,0-5,-'],
  ['$#x',        'rx', '$#,plain,x,@x,0,0,0-3,-'],
  ['$$sref',     'rx', '$,deref,sref,$sref,0,0,0-6,-'],
  ['\c$x',       'rx', '$,plain,x,$x,0,0,2-4,-'],
  ['a$|b',       'rx', ''],
  ['a@-',        'rx', ''],
  ['x$',         'rx', ''],
  ['$ar->@*',    'rxp', '$,plain,ar,$ar,0,0,0-7,@*'],
  # dq mode — always-subscript plus the perl-vs-StringInterpolation shapes
  ['$x{2}',      'dq', '$,plain,x,%x,1,0,0-5,-'],
  ['$x[abc]',    'dq', '$,plain,x,@x,1,0,0-7,-'],
  ['$$ar[0]',    'dq', '$,deref,ar,$ar,1,0,0-7,-'],
  ['@$ar[0]',    'dq', '@,deref,ar,$ar,1,1,0-7,-'],
  ['@{x}[0]',    'dq', '@,braced,x,@x,0,0,0-4,-'],
  ['a@-[1]b',    'dq', '@,magic,-,-,1,1,1-6,-'],
  ['${ x }s',    'dq', '$,braced,x,$x,0,0,0-6,-'],
  ['a$Foo::.b',  'dq', '$,plain,Foo,$Foo,0,0,1-7,-'],
  ['$1a',        'dq', '$,magic,1,-,0,0,0-2,-'],
  ['@{[uc($_)]}','dq', '@,expr,-,-,0,0,0-11,-'],
  ['$h{k}{v}[2]','dq', '$,plain,h,%h,3,0,0-11,-'],
  ['\$x',        'dq', ''],
  ['\\\\$x',     'dq', '$,plain,x,$x,0,0,2-4,-'],
  ['\c$x',       'dq', ''],
  ['${^CAPTURE}','dq', '$,magic,^CAPTURE,-,0,0,0-11,-'],
  ['$^W',        'dq', '$,magic,^W,-,0,0,0-3,-'],
  ['$::i',       'dq', '$,plain,::i,$::i,0,0,0-4,-'],
  ['$Foo::bar[3]','dq','$,plain,Foo::bar,@Foo::bar,1,0,0-12,-'],
  ['$$',         'dq', '$,magic,$,-,0,0,0-2,-'],
  ['$r->@*',     'dq', '$,plain,r,$r,0,0,0-2,-'],
  ['$r->@*',     'dqp','$,plain,r,$r,0,0,0-6,@*'],
);

for my $s (@SHAPES) {
  my ($text, $mode, $want) = @$s;
  my %opt = %OPT;
  %opt = ()                                    if $mode eq 'dq';
  %opt = (postderef_qq => 1)                   if $mode eq 'dqp';
  %opt = (%OPT, postderef_qq => 1)             if $mode eq 'rxp';
  is(sigs($text, %opt), $want, "scan($mode): $text");
}

# name_span is the rename splice target — exact positions matter
{
  my $ev = scan('$Foo::bar[3]')->[0];
  is_deeply($ev->{name_span}, [1, 9], 'name_span: $Foo::bar');
  $ev = scan('${ x }s')->[0];
  is_deeply($ev->{name_span}, [3, 4], 'name_span: ${ x }');
  $ev = scan('@{[uc($_)]}')->[0];
  is_deeply($ev->{expr_span}, [2, 10], 'expr_span: @{[...]}');
  $ev = scan('$h{k}{v}[2]')->[0];
  is_deeply([map { $_->{open} } @{ $ev->{chain} }], ['{', '{', '['],
            'chain opens: $h{k}{v}[2]');
  is_deeply($ev->{chain}[0]{guts_span}, [3, 4], 'chain guts: {k}');
}

done_testing();
