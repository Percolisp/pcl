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

# --- converted: gen_funcall_form goto family (E2.1) --------------------------
# goto &sub / goto &$cref (tail-calls) and computed goto EXPR.  (The goto
# LABEL → (go :LABEL) / throw-wrap shape is covered by corpus byte-parity;
# a standalone label hits an unrelated Parser2 statement gate here.)

my $gt = Pl::Parser2->parse_code(<<'EOT');
sub bar { 1 }
my $c = sub { 2 };
sub foo { goto &bar; }
sub baz { goto &$c; }
sub cc  { my $l = "L"; goto $l; }
EOT

like($gt, qr/\(p-goto-sub #'pl-bar\)/,        'goto &sub → (p-goto-sub #(quote)pl-bar)');
like($gt, qr/\(p-goto-sub \(p-get-coderef \$c\)\)/,
     'goto &$cref → (p-goto-sub (p-get-coderef $c))');
like($gt, qr/\(p-goto-computed \$l\)/,        'goto EXPR → (p-goto-computed $l)');

# --- converted: gen_funcall_form do family (E2.1) ---------------------------
# do { BLOCK } (func_ref / inline_lambda) and do &CODE.  body_cl embeds as a
# raw atom (structural inline_lambda conversion is E2's last step).

my $do = Pl::Parser2->parse_code(<<'EOT');
my $x = do { my $a = 1; $a + 2 };
sub f { 9 } my $ref = \&f;
my $z = do &$ref;
print $x + $z;
EOT

like($do, qr/\(funcall \(lambda \(\)/,   'do { BLOCK } → (funcall (lambda () …))');
like($do, qr/\(p-do \(p-get-coderef \$ref\)\)/,
     'do &$cref → generic tail (p-do (p-get-coderef $ref))');
like($do, qr/\(let \(\(\*wantarray\* nil\)\) \(funcall \(lambda/,
     'do block gets its scalar-context wantarray bind');

# --- converted: gen_funcall_form grep/map (E2.1) ----------------------------
# EXPRESSION form gets the (lambda ($_) EXPR) wrap in the form path; the
# BLOCK form rides the generic tail (lambda child stays a raw atom until the
# inline_lambda emitter converts — E2's last step).

my $gm = Pl::Parser2->parse_code(<<'EOT');
my @a = (1, 2, 3, 4);
my @d = grep $_ > 2, @a;
my @e = map $_ + 1, @a;
my @b = grep { $_ > 2 } @a;
print "@b @d @e";
EOT

like($gm, qr/\(p-grep \(lambda \(\$_\) \(p-> \$_ 2\)\) \@a\)/,
     'grep EXPR, LIST → (p-grep (lambda ($_) EXPR) @a)');
like($gm, qr/\(p-map \(lambda \(\$_\) \(p-\+ \$_ 1\)\) \@a\)/,
     'map EXPR, LIST → (p-map (lambda ($_) EXPR) @a)');
like($gm, qr/\(p-grep \(lambda \(\$_\)/,
     'grep { BLOCK } @a → generic-tail funcall with lambda child');

# --- converted: gen_leaf_form Number family (E2.1 leaf pilot) ----------------
# Number leaves reached through a CONVERTED parent (here a funcall) go via
# gen_leaf_form: atoms stay atoms, radix/version/inf become forms/atoms.

my $nm = Pl::Parser2->parse_code(<<'EOT');
sub f { $_[0] }
my $c = 1;
my @z = (f(42), f(0xFF), f(-0x10), f(0b1010), f(0o17), f(0777),
         f(1_000_000), f(3.14), f(1e9999), f(-1e9999), f(v1.2.3));
print "@z";
EOT

like($nm, qr/\(pl-f 42\)/,               'decimal atom: (pl-f 42)');
like($nm, qr/\(pl-f #xFF\)/,             'hex → #xFF atom');
like($nm, qr/\(pl-f \(- #x10\)\)/,       'negative hex → (- #x10)');
like($nm, qr/\(pl-f #b1010\)/,           'binary → #b1010');
like($nm, qr/\(pl-f #o17\)/,             '0o octal → #o17');
like($nm, qr/\(pl-f #o777\)/,            'legacy octal → #o777');
like($nm, qr/\(pl-f 1000000\)/,          'underscores stripped: 1_000_000 → 1000000');
like($nm, qr/\(pl-f \(p-double-inf\)\)/, 'float overflow → (p-double-inf)');
like($nm, qr/\(pl-f \(p-double-inf t\)\)/, 'negative float overflow → (p-double-inf t)');
like($nm, qr/\(pl-f \(p-version-string 1 2 3\)\)/, 'v-string → (p-version-string 1 2 3)');

# --- converted: gen_leaf_form Symbol/Magic family (E2.1 leaf) ----------------
# Reached through a CONVERTED parent (funcall): genuine atoms become native
# atoms; compound sym forms (stash/typeglob/&sub) decline to raw (still v1
# bytes — asserted indirectly: the atom cases carry the frontier weight).

my $sm = Pl::Parser2->parse_code(<<'EOT');
sub f { $_[0] }
my $x = 5; my @a = (1); my %h = (k => 1);
my $r = f($x) + f($a[0]) + f($h{k}) + f($@) + f($.);
print $r;
EOT

like($sm, qr/\(pl-f \$x\)/,             'scalar symbol atom: (pl-f $x)');
like($sm, qr/\(pl-f \(p-aref \@a 0\)\)/, 'array-access child still lowers');
like($sm, qr/\(pl-f \(p-gethash %h "k"\)\)/, 'hash-access child still lowers');
like($sm, qr/\(pl-f \$\@\)/,            'magic $@ atom: (pl-f $@)');
like($sm, qr/\(pl-f \|\$\.\|\)/,        'magic $. → |$.| atom');

# --- converted: gen_leaf_form Quote/HereDoc/Word/Operator (E2.1 leaf) --------
# Pure atom leaves through a converted parent become native atoms.

my $ql = Pl::Parser2->parse_code(<<'EOT');
sub f { $_[0] }
my $r = f("hello") . f('single') . f(qq{plain}) . f(q{lit});
print $r;
EOT

like($ql, qr/\(pl-f "hello"\)/,  'double-quote literal → "hello" atom');
like($ql, qr/\(pl-f "single"\)/, 'single-quote literal → "single" atom');
like($ql, qr/\(pl-f "plain"\)/,  'qq{} literal → "plain" atom');
like($ql, qr/\(pl-f "lit"\)/,    'q{} literal → "lit" atom');

# --- converted: gen_leaf_form regex leaves (E2.1 leaf) ----------------------
# NON-interpolated m// / qr// become (p-regex "…") / (pcl::p-qr "…") forms;
# interpolated patterns and s/// / tr/// DECLINE to the text path unchanged.
my $rl = Pl::Parser2->parse_code(<<'EOT');
my $x = "abc";
my $m  = $x =~ /a.c/;
my $q  = qr/\d+/i;
my $mi = $x =~ /$x/;
$x =~ s/a/b/;
$x =~ tr/a/b/;
print $m;
EOT
like($rl, qr/\(p-=~ \$x \(p-regex "\/a\.c\/"\)\)/,
     'non-interp m// → (p-regex "/a.c/")');
like($rl, qr{\(pcl::p-qr "qr/\\\\d\+/i"\)},
     'non-interp qr// → (pcl::p-qr "qr/\\d+/i")');
like($rl, qr/\(pcl::p-regex-from-parts \$x /,
     'interpolated /$x/ declines → (pcl::p-regex-from-parts …)');
like($rl, qr/\(p-subst "a" "b"\)/,  's/// declines → (p-subst …)');
like($rl, qr/\(p-tr /,              'tr/// declines → (p-tr …)');

# --- converted: arr_init / hash_init form handlers (E2.1 internal nodes) -----

my $in = Pl::Parser2->parse_code(<<'EOT');
my $a = [1, 2, 3];
my $e = [];
my $h = { x => 1, y => 2 };
my $eh = {};
print "$a $h $e $eh";
EOT

like($in, qr/\(make-p-box \(p-array-init 1 2 3\)\)/, '[1,2,3] → (make-p-box (p-array-init 1 2 3))');
like($in, qr/\(make-p-box \(make-array 0 :adjustable t :fill-pointer 0\)\)/,
     '[] → empty adjustable vector');
like($in, qr/\(make-p-box \(p-hash "x" 1 "y" 2\)\)/,
     '{x=>1,y=>2} → (make-p-box (p-hash "x" 1 "y" 2))');
like($in, qr/\(make-p-box \(p-hash \)\)/,
     '{} declines to text (trailing-space byte preserved)');

# --- converted: a_acc / h_acc form handlers (E2.1 internal nodes) ------------

my $ac = Pl::Parser2->parse_code(<<'EOT');
my @a = (1, 2, 3); my %h = (x => 1);
my $x = $a[0] + $h{x};
$a[1] = 5; $h{y} = 6;
my $m = $h{'p', 'q'};
my $ref = [[10, 20]];
my $y = $ref->[0][1];
print "$x $m $y";
EOT

like($ac, qr/\(p-aref \@a 0\)/,          'rvalue array access → (p-aref @a 0)');
like($ac, qr/\(p-gethash %h "x"\)/,      'rvalue hash access → (p-gethash %h "x")');
like($ac, qr/\(setf \(p-aref \@a 1\) 5\)/, 'lvalue array access → p-aref target');
like($ac, qr/\(setf \(p-gethash %h "y"\) 6\)/, 'lvalue hash access → p-gethash target');
like($ac, qr/\(p-gethash %h \(p-join \|\$;\| \(vector "p" "q"\)\)\)/,
     'multi-key $h{a,b} → (p-join |$;| (vector …))');
like($ac, qr/\(p-aref \(p-aref-deref \$ref 0\) 1\)/,
     'nested container stays structural');

# --- converted: ref-access + slice family form handlers (E2.1) --------------

my $sl = Pl::Parser2->parse_code(<<'EOT');
my @a = (1, 2, 3, 4); my %h = (a => 1, b => 2);
my $ar = [10, 20, 30]; my $hr = { x => 1 };
my @s1 = @a[0, 2];
my @s2 = @h{'a', 'b'};
my %k1 = %h{'a', 'b'};
my $r1 = $ar->[1];
my $r2 = $hr->{x};
my $r3 = $hr->{'p', 'q'};
print "@s1 @s2 $r1 $r2 $r3";
EOT

like($sl, qr/\(p-aslice \@a 0 2\)/,          'array slice → (p-aslice @a 0 2)');
like($sl, qr/\(p-hslice %h "a" "b"\)/,       'hash slice → (p-hslice %h "a" "b")');
like($sl, qr/\(p-kv-hslice %h "a" "b"\)/,    'kv hash slice → (p-kv-hslice %h "a" "b")');
like($sl, qr/\(p-aref-deref \$ar 1\)/,       'array-ref access → (p-aref-deref $ar 1)');
like($sl, qr/\(p-gethash-deref \$hr "x"\)/,  'hash-ref access → (p-gethash-deref $hr "x")');
like($sl, qr/\(p-gethash-deref \$hr \(p-join \|\$;\| \(vector "p" "q"\)\)\)/,
     'multi-key hash-ref → (p-join |$;| (vector …))');

# --- converted: progn + small I/O nodes (E2.1) ------------------------------

my $ms = Pl::Parser2->parse_code(<<'EOT');
my @a = (1, 2, 3);
my @e = ();
my $s = ("a", "b", "c");
print STDERR "err\n";
my $out = `echo hi`;
my $line = <$fh>;
my @lines = <STDIN>;
print "@a $s $out $line @lines @e";
EOT

like($ms, qr/\(vector 1 2 3\)/,            'list-context progn → (vector 1 2 3)');
like($ms, qr/\(vector \)/,                 'empty () declines to text (trailing space)');
like($ms, qr/\(p-print :fh 'STDERR /,      'filehandle marker → :fh \x27STDERR');
like($ms, qr/\(p-backtick "echo hi"\)/,    'backtick → (p-backtick "echo hi")');
like($ms, qr/\(let \(\(\*wantarray\* nil\)\) \(p-readline \$fh\)\)/,
     'readline <$fh> scalar-context bound');
like($ms, qr/\(let \(\(\*wantarray\* t\)\) \(p-readline 'STDIN\)\)/,
     'readline <STDIN> list-context bound + bareword quote');

# --- converted: gen_glob_form (E2.1) ----------------------------------------
# literal / interpolated (p-. concat) / negated-char-class (glob + remove-if
# filter) patterns, all with the wantarray bind.
my $gl = Pl::Parser2->parse_code(<<'EOT');
my $dir = "d";
my @a = <*.txt>;
my @b = <$dir/*.c>;
my @c = <[!._]*>;
my $one = <*.log>;
print "@a";
EOT
like($gl, qr/\(let \(\(\*wantarray\* t\)\) \(p-glob "\*\.txt"\)\)/,
     'glob literal → (p-glob "*.txt") list-context bound');
like($gl, qr/\(p-glob \(p-\. \$dir "\/\*\.c"\)\)/,
     'glob interpolated → (p-glob (p-. …))');
like($gl, qr/\(remove-if \(lambda \(--f--\) \(let \(\(--name-- \(file-namestring \(pathname --f--\)\)\)\) \(and \(> \(length --name--\) 0\) \(find \(char --name-- 0\) "\._"\)\)\)\) \(p-glob "\?\*"\)\)/,
     'glob [!chars] → glob "?"-simplified + remove-if filter');
like($gl, qr/\(let \(\(\*wantarray\* nil\)\) \(p-glob "\*\.log"\)\)/,
     'glob scalar-context → (let ((*wantarray* nil)) (p-glob …))');

# --- converted: gen_methodcall_form (E2.1, internal-node frontier) -----------
# invocant disambiguation (class string / __PACKAGE__ / resolve-invocant /
# paren-scalar base), dynamic method, SUPER::, and args all via the form path.

my $mc = Pl::Parser2->parse_code(<<'EOT');
package B; our @ISA = ('A');
sub g { my $self = shift; $self->SUPER::g(1, 2); }
package main;
my $o = B->new();
my $m = "g";
my $r = \42;
my $a = B->g(3);
my $b = $o->g();
my $c = $o->$m();
my $d = __PACKAGE__->g();
my $e = Unknownpkg->h();
my $f = ($r // 0)->foo(7);
print $b;
EOT

like($mc, qr/\(p-method-call "B" "new"\)/,
     'class name invocant → "B" string literal');
like($mc, qr/\(p-method-call \$o "g"\)/,
     'static method name → quoted string arg');
like($mc, qr/\(p-method-call \$o \$m\)/,
     'dynamic method $obj->$m → variable, not quoted');
like($mc, qr/\(p-method-call "main" "g"\)/,
     '__PACKAGE__ invocant → current package string');
like($mc, qr/\(p-method-call \(p-resolve-invocant "Unknownpkg"\) "h"\)/,
     'unknown bareword invocant → (p-resolve-invocant "name")');
like($mc, qr/\(p-method-call \(p-\/\/ \$r 0\) "foo" 7\)/,
     'paren-scalar base → scalar-context deref invocant + args');
like($mc, qr/\(p-super-call \$self "g" "B" 1 2\)/,
     'SUPER::g → (p-super-call obj "g" "B" args)');

# --- converted: gen_prefix_op_form (E2.1, PARTIAL — text-inspecting ops decline)
# converts !/~/-/not, sigil casts, unary +, &, *, $#{…}, the cast-over-postfix
# fixup; DECLINES \ / ++ / -- (magic-lvalue operand-text inspection deferred).

my $pf = Pl::Parser2->parse_code(<<'EOT');
my $x = 5; my @a = (1, 2, 3); my $ref = \@a; my $cref = sub { 1 };
my $n = -$x;
my $b = !$x;
my $c = ~$x;
my $d = not $x;
my $e = $#a;
my $f = $#{$ref};
my @g = @$ref;
my $h = +($x + 1);
my $bs = \$x;
my $pp = ++$x;
print "$n$b$c";
EOT

like($pf, qr/\(p-- \$x\)/,                 'unary minus → (p-- $x)');
like($pf, qr/\(p-! \$x\)/,                 'logical not → (p-! $x)');
like($pf, qr/\(p-bit-not \$x\)/,           'bit complement → (p-bit-not $x)');
like($pf, qr/\(p-not \$x\)/,               'low-prec not → (p-not $x)');
like($pf, qr/\(p-array-last-index \@a\)/,  '$#a → (p-array-last-index @a)');
like($pf, qr/\(p-array-last-index \$ref\)/,'$#{$ref} → (p-array-last-index $ref)');
like($pf, qr/\(p-cast-@ \$ref\)/,          '@$ref → (p-cast-@ $ref)');
# declines: the text emitter still owns these (byte-identical fallback).
like($pf, qr/\(p-backslash \$x\)/,         '\\$x declines → text (p-backslash $x)');
like($pf, qr/\(p-pre\+\+ \$x\)/,           '++$x declines → text (p-pre++ $x)');

# --- converted: gen_postfix_op_form (E2.1) ----------------------------------
# plain ++/-- and the chained-comparison container; DECLINES $#array++ (arylen
# setter form inspects operand text → deferred, AST-detected via _operand_is_arylen).

my $po = Pl::Parser2->parse_code(<<'EOT');
my $x = 5; my @a = (1, 2, 3); my %h = (k => 1);
my $p1 = $x++;
my $p2 = $x--;
my $p3 = $h{k}++;
my $p4 = $a[0]--;
my $c = ($x < 3 < 10);
$#a++;
print "$p1$p2";
EOT

like($po, qr/\(p-post\+\+ \$x\)/,          '$x++ → (p-post++ $x)');
like($po, qr/\(p-post-- \$x\)/,            '$x-- → (p-post-- $x)');
like($po, qr/\(p-post\+\+ \(p-gethash-box %h "k"\)\)/,
     '$h{k}++ → (p-post++ (p-gethash-box …)) lvalue container');
like($po, qr/\(p-post-- \(p-aref-box \@a 0\)\)/,
     '$a[0]-- → (p-post-- (p-aref-box …)) lvalue container');
like($po, qr/\(p-chain-cmp \$x '< 3 '< 10\)/,
     'chained comparison → (p-chain-cmp term \x27op …)');
like($po, qr/\(let \(\(_prev \(p-array-last-index \@a\)\)\) \(p-set-array-length \@a \(1\+ _prev\)\) _prev\)/,
     '$#a++ declines → arylen setter text form');

# --- converted: gen_tree_val_form (E2.1) ------------------------------------
# vector / progn / flatten branches, and the single-child list-context regex
# special case: a (p-=~ …) ANYWHERE in the child (even nested inside a larger
# expression) suppresses the (vector …) wrap in favour of a bare
# (let ((*wantarray* t)) child) — the byte-exact to_flat($child) grep.  This
# is the error-prone case, so the regex-inside-expression shapes are pinned
# explicitly here.

my $tv = Pl::Parser2->parse_code(<<'EOT');
my $x = "abc"; my @a = (1, 2, 3);
my @m1 = ($x =~ /(\w)(\w)/);
my @m2 = ($x !~ /z/);
my @m3 = (/foo/);
my @m4 = (1 + ($x =~ /y/));
my @m5 = (($a[0] =~ /1/), $a[1]);
my @m6 = ($x);
my @m7 = ($x, $x);
my @m8 = (10 .. 12);
print "@m1";
EOT

# regex directly the single child → let-wrap, NEVER (vector …)
like($tv, qr/\(p-array-= \@m1 \(let \(\(\*wantarray\* t\)\) \(let \(\(\*wantarray\* t\)\) \(p-=~ \$x /,
     'list-ctx ($x =~ /re/) → let-wrap, no vector');
unlike($tv, qr/\@m1 \(vector/,
     '=~ as the sole child of @m1 is NOT wrapped in (vector …)');
# !~ is boolean (emits p-!~), NOT a p-=~ match → IS vector-wrapped
like($tv, qr/\(p-array-= \@m2 \(vector \(let \(\(\*wantarray\* t\)\) \(p-!~ \$x /,
     'list-ctx ($x !~ /re/) → (vector (p-!~ …)) — not the regex special case');
# bare /foo/ lowers to (p-=~ $_ …) → let-wrap
like($tv, qr/\(p-array-= \@m3 \(let \(\(\*wantarray\* t\)\) \(let \(\(\*wantarray\* t\)\) \(p-=~ \$_ /,
     'list-ctx bare (/foo/) → let-wrap ($_ match)');
# CRITICAL: regex NESTED inside a larger expression still suppresses vector —
# a naive "child is a =~ node" AST predicate would wrongly emit (vector …) here.
like($tv, qr/\(p-array-= \@m4 \(let \(\(\*wantarray\* t\)\) \(p-\+ 1 /,
     'list-ctx (1 + ($x =~ /y/)) → let-wrap (regex nested in expression)');
unlike($tv, qr/\(p-array-= \@m4 \(vector /,
     'nested-regex single child is NOT vector-wrapped');
# multi-child with a regex element is a genuine multi-value list → (vector …);
# the regex element is itself a single-child tree_val, so it keeps its own
# (let ((*wantarray* t)) (p-=~ …)) let-wrap inside the vector.
like($tv, qr/\(p-array-= \@m5 \(vector \(let \(\(\*wantarray\* t\)\) \(p-=~ /,
     'multi-child list with a regex element → (vector (let … (p-=~ …)) …)');
# plain single scalar / multi / range branches
like($tv, qr/\(p-array-= \@m6 \(vector \$x\)/,   'single non-regex child → (vector $x)');
like($tv, qr/\(p-array-= \@m7 \(vector \$x \$x\)/,'multi child → (vector $x $x)');
like($tv, qr/\(p-array-= \@m8 \(p-\.\. 10 12\)\)/,'single range child → bare (p-.. …), no vector');

# empty () declines to the text emitter (trailing-space (vector )/(progn ) shape)
my $tve = Pl::Parser2->parse_code('my @e = (); print scalar(@e);');
like($tve, qr/\(vector \)/, 'empty () declines → text (vector ) trailing space');

# --- converted: gen_ref_funcall_form (E2.1) ---------------------------------
my $rf = Pl::Parser2->parse_code(<<'EOT');
my $c = sub { $_[0] + 1 };
my $r1 = $c->(5);
my @l = $c->(1, 2, 3);
$c->();
print $r1;
EOT
like($rf, qr/\(let \(\(\*wantarray\* nil\)\) \(p-funcall-ref \$c 5\)\)/,
     'scalar-ctx code-ref call → (let ((*wantarray* nil)) (p-funcall-ref $c 5))');
like($rf, qr/\(let \(\(\*wantarray\* t\)\) \(p-funcall-ref \$c 1 2 3\)\)/,
     'list-ctx code-ref call → (let ((*wantarray* t)) (p-funcall-ref …))');
like($rf, qr/\(let \(\(\*wantarray\* :void\)\) \(p-funcall-ref \$c\)\)/,
     'void-ctx code-ref call → (let ((*wantarray* :void)) (p-funcall-ref $c))');

# --- converted: gen_binary_op_form (E2.1) -----------------------------------
# arithmetic / comparison / logical / string / . / x / .. / isa all convert to
# forms; so do `=` assignment (LHS-shape dispatch inspects the flat left text)
# and =~/!~ (s///-vs-match wantarray wrap decided AST-level).

my $bo = Pl::Parser2->parse_code(<<'EOT');
my ($x, $y) = (5, 3); my @a = (1, 2, 3); my $o = bless {}, 'Foo';
my $s1 = $x + $y * 2;
my $s2 = $x . $y . "z";
my $b1 = $x == $y;
my $b2 = $x <= $y && $y > 1;
my $b3 = $x eq "5" || $y ne "3";
my $sh = $x << 2;
my $rep = "a" x 3;
my @lr = (1, 2) x 2;
my @rng = (1 .. 5);
my $isa = $o isa Foo;
print $s1;
EOT

like($bo, qr/\(p-\+ \$x \(p-\* \$y 2\)\)/,      'arith precedence → (p-+ $x (p-* $y 2))');
like($bo, qr/\(p-\. \(p-\. \$x \$y\) "z"\)/,    'string concat . → nested (p-. …)');
like($bo, qr/\(p-== \$x \$y\)/,                 'numeric compare → (p-== $x $y)');
like($bo, qr/\(p-&& \(p-<= \$x \$y\) \(p-> \$y 1\)\)/, 'logical && over comparisons');
like($bo, qr/\(p-\|\| \(p-str-eq \$x "5"\) \(p-str-ne \$y "3"\)\)/, 'string eq/ne under ||');
like($bo, qr/\(p-<< \$x 2\)/,                   'bit shift → (p-<< $x 2)');
like($bo, qr/\(p-str-x "a" 3\)/,                'scalar x → (p-str-x "a" 3)');
like($bo, qr/\(p-list-x \(vector 1 2\) 2\)/,    'list (…) x N → (p-list-x (vector …) N)');
like($bo, qr/\(p-\.\. 1 5\)/,                   'list-context range → (p-.. 1 5)');
like($bo, qr/\(p-isa \$o "Foo"\)/,              'isa bareword RHS → (p-isa $o "Foo")');

# flip-flop (scalar-context ..) still emits the p-flipflop macro with an id
my $ff = Pl::Parser2->parse_code('while (<STDIN>) { my $f = /a/ .. /b/; print $f; }');
like($ff, qr/\(p-flipflop \d+ /, 'scalar-context .. → (p-flipflop ID …)');

# `=` assignment: LHS-shape dispatch (@→p-array-=, %→p-hash-=, sigil-$→
# p-scalar-=, element→p-setf) now runs through gen_binary_op_form at byte
# parity; =~/!~ likewise convert.
my $bd = Pl::Parser2->parse_code(<<'EOT');
my ($x, $y) = (1, 2); my @a; my %h;
$x = $y; @a = (1, 2); %h = (k => 1);
my $m = $x =~ /1/; my $nm = $x !~ /z/;
print $x;
EOT
# (a bare lexical scalar `$x = $y` is lowered by Parser2's NATIVE assignment
# path to (p-my-= …), never reaching gen_binary_op; the array/hash `=` DO
# reach gen_binary_op[_form] → (p-array-=)/(p-hash-=).)
like($bd, qr/\(p-my-= \$x \$y\)/,       'scalar = uses the native (p-my-= …) path');
like($bd, qr/\(p-array-= \@a /,         '= array assign → (p-array-= …)');
like($bd, qr/\(p-hash-= %h /,           '= hash assign → (p-hash-= …)');
like($bd, qr/\(p-=~ \$x /,              '=~ still emits (p-=~ …)');
like($bd, qr/\(p-!~ \$x /,              '!~ still emits (p-!~ …)');

# `=` LHS-shape dispatch — the distinctive form-path branches that DO reach
# gen_binary_op_form (all byte-parity with the pre-conversion text emitter):
# list-assign, dereferenced container assign, $#arr length set, typeglob assign.
# (Bare-statement element stores $h{k}=… / $a[i]=… are lowered by Parser2's
# native path to (setf (p-gethash …) …), not through gen_binary_op — so they
# are not asserted here.)
my $ba = Pl::Parser2->parse_code(<<'EOT');
our $g; my ($x, $y); my @a; my $ar = [1]; my $hr = {};
($x, $y) = (1, 2);
@$ar = (3, 4); %$hr = (k => 1);
$#a = 2;
*g = \&main::foo;
print $x;
EOT
like($ba, qr/\(p-list-= \(vector \$x \$y\) /, '(LIST) = … → (p-list-= (vector …) …)');
like($ba, qr/\(p-array-deref-= \(p-cast-\@ \$ar\) /, '@$ref = … → (p-array-deref-= …)');
like($ba, qr/\(p-hash-deref-= \(p-cast-% \$hr\) /,  '%$ref = … → (p-hash-deref-= …)');
like($ba, qr/\(p-set-array-length \@a 2\)/,        '$#arr = N → (p-set-array-length …)');
like($ba, qr/\(p-glob-assign "main" "g" /,         '*g = … → (p-glob-assign "pkg" "name" …)');

# --- converted: =~ / !~ context wrap (AST subst/tr detection) ----------------
# match RHS gets a *wantarray* wrap pinned to the node context; s///-/tr///-RHS
# (a scalar count) skips it — decided AST-level (RHS node type), not by grepping
# the generated $right.
my $rx = Pl::Parser2->parse_code(<<'EOT');
my $x = "abc"; my @m;
my $b  = $x =~ /a/;
my $nb = $x !~ /z/;
@m = $x =~ /(\w)(\w)/;
$x =~ s/a/b/;
$x =~ tr/a/b/;
print "$b$nb";
EOT
like($rx, qr/\(let \(\(\*wantarray\* nil\)\) \(p-=~ \$x \(p-regex "\/a\/"\)\)\)/,
     'scalar-ctx match → (let ((*wantarray* nil)) (p-=~ …))');
like($rx, qr/\(let \(\(\*wantarray\* nil\)\) \(p-!~ \$x /,
     'scalar-ctx !~ match → nil-wrapped');
like($rx, qr/\(let \(\(\*wantarray\* t\)\) \(p-=~ \$x \(p-regex "\/\(\\\\w\)\(\\\\w\)\/"\)\)\)/,
     'list-ctx match → (let ((*wantarray* t)) (p-=~ …))');
like($rx, qr/\(p-=~ \$x \(p-subst "a" "b"\)\)/,
     's/// RHS → (p-=~ … (p-subst …)) with NO wantarray wrap');
like($rx, qr/\(p-=~ \$x \(p-tr "a" "b"\)\)/,
     'tr/// RHS → (p-=~ … (p-tr …)) with NO wantarray wrap');
unlike($rx, qr/\(let \(\(\*wantarray\* \w+\)\) \(p-=~ \$x \(p-subst/,
     's/// RHS is never wantarray-wrapped');

# --- converted: gen_anon_sub_form (E2.1) ------------------------------------
# The anon_sub internal node's real site is the s///e replacement code block:
# s/PAT/CODE/e wraps CODE in (lambda () …).  (Plain sub { … } uses Parser2's
# native (lambda (&rest %_args) …) lowering, a different path.)
my $an = Pl::Parser2->parse_code('my $x = "ab"; $x =~ s/(a)/$1 . "z"/e; print $x;');
like($an, qr/\(lambda \(\) \(p-\. \$1 "z"\)\)/,
     's///e replacement → (lambda () CODE) via gen_anon_sub_form');

done_testing();
