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

# --- converted: gen_funcall_form introspection family (E2.1) -----------------
# exists / delete / defined / tied / pos — element/slice/ref/sub shapes now
# emit via the form path (removed from %FUNCALL_FORM_DECLINES).

my $ix = Pl::Parser2->parse_code(<<'EOT');
my %h = (a => 1); my @a = (1, 2, 3);
my $r = { x => 1 }; my $ar = [1, 2];
my $e1 = exists $h{a};   my $e2 = exists $a[0];
my $e3 = exists $r->{x}; my $e4 = exists $ar->[0];
my $ef = exists &foo;
delete $h{a};   delete $a[0];
delete @h{'a','b'}; delete @a[0,1];
delete $r->{x};
my $d1 = defined &foo; my $d2 = defined FILE;
print $e1;
EOT

like($ix, qr/\(p-exists %h "a"\)/,          'exists $h{k} → (p-exists %h key)');
like($ix, qr/\(p-exists-array \@a 0\)/,      'exists $a[i] → (p-exists-array @a i)');
like($ix, qr/\(p-exists \(unbox \$r\) "x"\)/, 'exists $r->{k} → (p-exists (unbox ref) key)');
like($ix, qr/\(p-exists-array \(unbox \$ar\) 0\)/,
     'exists $ar->[i] → (p-exists-array (unbox ref) i)');
like($ix, qr/\(p-sub-exists "main" "foo"\)/, 'exists &sub → (p-sub-exists pkg name)');
like($ix, qr/\(p-delete %h "a"\)/,           'delete $h{k} → (p-delete %h key)');
like($ix, qr/\(p-delete-array \@a 0\)/,       'delete $a[i] → (p-delete-array @a i)');
like($ix, qr/\(p-delete-hash-slice %h "a" "b"\)/,
     'delete @h{...} → (p-delete-hash-slice %h keys)');
like($ix, qr/\(p-delete-array-slice \@a 0 1\)/,
     'delete @a[...] → (p-delete-array-slice @a idxs)');
like($ix, qr/\(p-delete \(unbox \$r\) "x"\)/, 'delete $r->{k} → (p-delete (unbox ref) key)');
like($ix, qr/\(p-sub-defined "main" "foo"\)/, 'defined &sub → (p-sub-defined pkg name)');
like($ix, qr/\(p-defined-fh 'FILE\)/,         'defined BAREWORD → (p-defined-fh name)');

# --- converted: gen_funcall_form lvalue family (E2.1) ------------------------
# undef / chop / chomp — args generated under lvalue context (element args get
# the box), plus undef &sub.

my $lv = Pl::Parser2->parse_code(<<'EOT');
my %h = (a => 1); my @a = (1, 2, 3); my $s = "x\n"; my $x = 5;
undef $h{a}; undef $a[0]; undef $x; undef &foo; undef;
chomp $s; chop $s; chomp @a;
print $s;
EOT

like($lv, qr/\(p-undef \(p-gethash-box %h "a"\)\)/,
     'undef $h{k} → box (lvalue context)');
like($lv, qr/\(p-undef \(p-aref-box \@a 0\)\)/,
     'undef $a[i] → box (lvalue context)');
like($lv, qr/\(p-undef \$x\)/,           'undef $scalar → (p-undef $x)');
like($lv, qr/\(p-undef-sub "main" "foo"\)/, 'undef &sub → (p-undef-sub pkg name)');
like($lv, qr/\(p-undef\)/,               'bare undef → (p-undef)');
like($lv, qr/\(p-chomp \$s\)/,           'chomp $s → (p-chomp $s)');
like($lv, qr/\(p-chop \$s\)/,            'chop $s → (p-chop $s)');
like($lv, qr/\(p-chomp \@a\)/,           'chomp @a → (p-chomp @a)');

done_testing();
