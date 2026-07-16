#!/usr/bin/env perl
# Pl/t/clform-01.t — Pl::CLForm printer + the E2 emitter-conversion scaffold
# (docs/v2-endgame-plan.md E2).  Pure-perl (no SBCL spawn): unit tests for
# to_flat (the exact flat renderer every converted ExprToCL emitter is
# byte-parity-verified through) and shape guards for the pilot conversion
# (gen_ternary → form-producing), including converted-in-converted nesting
# and raw (unconverted-child) embedding.
use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin;
use lib "$FindBin::Bin/../..";
use Pl::CLForm qw(raw raw_wrap to_flat);
use Pl::Parser2;

# --- to_flat: the exact flat rendering contract -----------------------------

is(to_flat('$x'), '$x', 'plain atom passes through');
is(to_flat(['p-+', '$x', '3']), '(p-+ $x 3)', 'list form, single spaces');
is(to_flat(['p-if', '$a', ['p-+', '$x', '1'], '"s"']),
   '(p-if $a (p-+ $x 1) "s")', 'nested forms');
is(to_flat(['let', ['list', ['list', '$x', '1']], '$x']),
   '(let (($x 1)) $x)', "'list' head renders headless parens");
is(to_flat(raw('(p-anything (already text))')),
   '(p-anything (already text))', 'raw atom embedded verbatim');
is(to_flat(['p-if', '$a', raw("(lambda ()\n  1)"), '2']),
   "(p-if \$a (lambda ()\n  1) 2)",
   'multi-line raw text embedded verbatim, not re-flattened');
ok(!eval { to_flat(raw_wrap('(let ((x 1))', 1)); 1 },
   'raw_wrap inside an expression form dies');

# --- pilot conversion: gen_ternary emits via the form path ------------------

my $cl = Pl::Parser2->parse_code(
  'my $x = $a ? ($b ? 1 : 2) : 3; my $y = $a ? foo(7) : [8]; print $x;');

like($cl, qr/\(p-if \$a \(p-if \$b 1 2\) 3\)/,
     'converted-in-converted: nested ternary forms compose');
like($cl, qr/\(p-if \$a \(let \(\(\*wantarray\* nil\)\) \(pl-foo 7\)\) \(make-p-box \(p-array-init 8\)\)\)/,
     'unconverted children arrive as raw atoms, bytes preserved');

# --- converted: gen_string_concat / gen_array_str_interp --------------------

my $sc = Pl::Parser2->parse_code(
  'my @arr = (1,2); my $a = 3; my $s = "x $a @arr @{[1+2]} @arr[0..1] y"; print $s;');

like($sc, qr/\(p-string-concat "x " \$a " " \(p-join \|\$"\| \@arr\) " " \(p-join \|\$"\| \(p-cast-@ \(make-p-box \(p-array-init \(p-\+ 1 2\)\)\)\)\) " " \(p-join \|\$"\| \(p-aslice \@arr \(p-\.\. 0 1\)\)\) " y"\)/,
     'string_concat form: scalar, @arr join, @{[...]} cast, slice join, literals');

# --- converted: gen_funcall_form (E2.1, generic path) ------------------------

my $fc = Pl::Parser2->parse_code(<<'EOT');
sub two ($$) { my ($a, $b) = @_; return $a + $b }
my @a = (1, 2, 3);
my $r = two(@a, 3);
my $j = join(",", 1, 2);
print STDERR "e";
print;
warn "w";
EOT

like($fc, qr/\(pl-two \(p-scalar \@a\) 3\)/,
     'funcall form: prototype $-slot imposes (p-scalar @a), literal skipped');
like($fc, qr/\(let \(\(\*wantarray\* t\)\) \(p-join "," 1 2\)\)/,
     'funcall form: join gets its list-context bind');
like($fc, qr/\(p-print :fh 'STDERR "e"\)/,
     'funcall form: print filehandle marker passes through untouched');
like($fc, qr/\(p-print \$_\)/,
     'funcall form: bare print gets the explicit $_ default');
like($fc, qr/\(p-warn :loc "- line \d+" "w"\)/,
     'funcall form: warn carries the :loc source-location marker');

done_testing();
