#!/usr/bin/env perl
# Transpile tests part 7.  NEW TESTS GO HERE (or a future -08) — the
# BIGGEST test file bounds the parallel suite's wall time (one SBCL spawn
# per test), so start a new file instead of growing the current largest.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    # Shell-escape embedded single quotes ('…' -> '\''), or any tick in the
    # snippet truncates the -e arg.
    (my $sh_code = $full_code) =~ s/'/'\\''/g;
    my $output = `perl -e '$sh_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# #25 suite family: high capture groups ($10+) and $^N.  $^N is the
# participating group with the rightmost CLOSING paren — the nested case
# is where it differs from $+ (highest-numbered opener).
test_transpile("capture groups past \$9", '
"abcdefghij" =~ /(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)/;
print "ten:$10:$+\n";
"xy" =~ /(x)(y)/;
print "stale:", (defined $10 ? "still" : "cleared"), "\n";
');

test_transpile("\$^N rightmost-closer rule incl. nested groups", '
"ab" =~ /(a(b))/;      print "nested:", $^N, "\n";
"ab" =~ /(a)(b)/;      print "flat:", $^N, "\n";
"b"  =~ /(?:(a)|(b))/; print "alt:$^N\n";
');

# Task #114 (range.t 162, RT #130841): a range whose byte size wraps
# size_t must croak in perl's "panic: memory wrap|Out of memory" family,
# not a PCL-specific overflow message.
test_transpile("huge range croak speaks perl's memory-wrap family", '
my $max_iv = (~0 >> 1);
eval { my @range = 1..($max_iv - 1); };
print "err:", (($@ =~ /panic: memory wrap|Out of memory/) ? "wrap-family" : "other:$@"), "\n";
');

# #25 suite family: the full filetest set (-l/-p/-u/-g/-k/-o/-O/-R/-W/-X/
# -T/-B/-M/-A/-C/-t...), plus the op_info case-sensitivity fix: the
# precedence lookup lowercased operators, so -M/-A (no lowercase twin in
# the table) fell through as parse errors while -C/-T/-S only worked by
# landing on the OTHER filetest's identical entry.
test_transpile("filetest family incl. -M/-A case-sensitive lookup", '
my $d = "/tmp/pcl-ft-$$";
mkdir $d;
open my $fh, ">", "$d/f.txt"; print $fh "hello\n"; close $fh;
symlink("f.txt", "$d/ln");
print "l:", (-l "$d/ln" ? 1 : 0), (-l "$d/f.txt" ? 1 : 0), "\n";
print "TB:", (-T "$d/f.txt" ? 1 : 0), (-B "$d/f.txt" ? 1 : 0), "\n";
print "own:", (-o "$d/f.txt" ? 1 : 0), (-O "$d/f.txt" ? 1 : 0), "\n";
print "age:", int(-M "$d/f.txt"), int(-A "$d/f.txt"), int(-C "$d/f.txt"), "\n";
print "miss:", (-l "$d/nosuch" ? 1 : 0), (-u "$d/nosuch" ? 1 : 0), "\n";
print "W:$^W\n";
unlink "$d/ln", "$d/f.txt"; rmdir $d;
');

# #25 suite family: perl's `Bareword::` package-name-string form —
# `Foo::` eq "Foo" in term position, as a method-call invocant, as a
# funcall argument (a separate parse route), as a hash key, and after a
# fat comma.  Normalized in cleanup_for_parsing (all parse routes) +
# _make_string_of_token_word (the autoquote paths).
test_transpile("Bareword:: package-name string form", '
package Foo { sub new { bless {}, shift } sub who { ref($_[0]) || $_[0] } }
print Foo::, "|", main::, "\n";
print Foo::->new->who, "|", Foo::->who, "\n";
sub takes { return $_[0] }
print takes(Foo::), "\n";
my %h = (Foo => 1, "Foo::" => 2);
print $h{Foo::}, "|";
my %g = (Foo:: => 5);
print join(",", keys %g), "\n";
');

# lock: a no-op on unthreaded perl (op/lock.t crashed on undefined pl-lock)
test_transpile("lock is a no-op that returns its argument", '
my @a = (1,2,3);
lock @a;
my $x = 5;
print lock($x) + 1, ":", scalar(@a), "\n";
');

# #25: real fileno (fd-stream backed), glob/glob-ref filehandle
# designators, and pure-perl POSIX::dup on top of them.
test_transpile("fileno returns real fds; glob designators; POSIX::dup", '
require POSIX;
print "std:", fileno(\*STDIN), fileno(*STDOUT), fileno(STDERR), "\n";
open my $fh, "<", "/etc/hostname" or die;
print "real:", (fileno($fh) > 2 ? "y" : "n"), "\n";
my $fd1 = POSIX::dup(fileno(\*STDOUT));
my $fd2 = POSIX::dup(fileno(\*STDOUT));
print "dup:", ($fd1 > 2 ? "y" : "n"), ($fd2 > $fd1 ? "y" : "n"), "\n";
');

# #25: passwd-database family (new) + the grent family's latent context
# bugs (wantarray was a &key nothing passed — list context never spread;
# scalar getgrnam returned the name where perl returns the GID).
test_transpile("getpw*/getgr* families are context-sensitive", '
my @e = getpwuid($<);
print "n:", scalar(@e), "|", ($e[2] == $< ? "uid-ok" : "uid-bad"), "\n";
my $name = getpwuid($<);
my $uid  = getpwnam($e[0]);
print "scalar:", ($name eq $e[0] ? "y" : "n"), ($uid == $< ? "y" : "n"), "\n";
setpwent();
my @first = getpwent();
my @second = getpwent();
print "ent:", scalar(@first), ":", ($first[0] ne $second[0] ? "advances" : "stuck"), "\n";
endpwent();
my @g = getgrnam("root");
my $gid = getgrnam("root");
print "gr:", scalar(@g), ":$gid\n";
');

# overload::constant is the %^H compile-time mechanism (blessed
# not-supported) — must be a harmless no-op, and runtime operator
# overloading must be unaffected.
test_transpile("overload::constant no-op; runtime overloading intact", '
package S { use overload q{""} => sub { "STR" }; sub new { bless {}, shift } }
overload::constant(integer => sub { $_[1] });
my $o = S->new;
print "x:$o:done\n";
');

# #25 / task #129: an ANONYMOUS sub's signature.  With the feature in
# scope PPI hands `($x)` back as a Structure, so `sub` looked like a CALL
# and the whole enclosing statement died "Fell through. Missing case"
# (op/signatures.t).  Covers defaults (absent-only for `=`, undef/false
# for //= and ||=), a slurpy tail, `()` arity 0, and both arity dies.
test_transpile("anon sub signatures: binding, defaults, slurpy, arity", '
use feature "signatures";
my $f = sub ($x, $y = 3, @rest) { "$x/$y/[@rest]" };
print $f->(1), ";", $f->(1,2), ";", $f->(1,2,3,4), "\n";
my $g = sub ($a, $b //= "D", $c ||= "E") { "$a$b$c" };
print $g->(1, undef, 0), ";", $g->(1, 2, 3), "\n";
my $z = sub () { 42 };
print $z->(), "\n";
print eval { $f->() } ? "no-die" : ($@ =~ /^Too few arguments/ ? "few" : "?$@"), ";",
      eval { $z->(9) } ? "no-die" : ($@ =~ /^Too many arguments/ ? "many" : "?$@"), "\n";
');

# Same signature reached through the OTHER parse routes: a glob
# assignment inside a package block (the op/signatures.t killer), a
# funcall argument, an array element, and a string eval — the eval
# inherits the feature from a scope PPI never sees, so the desugar has
# to read the Prototype token it gets there too.  A REAL prototype
# ((&@), ($$)) binds nothing and must still be dropped.
# (Signature syntax with the feature genuinely OFF is read as a
# signature, not a prototype — a deliberate deviation shared with named
# subs; see docs/not-supported.md §Signature syntax.)
test_transpile("anon signature parse routes incl. eval; prototypes untouched", '
use feature "signatures";
package T200 { *t201 = sub ($x) { $x * 2 } }
print T200::t201(21), "\n";
sub apply { my ($c, @a) = @_; return $c->(@a) }
print apply(sub ($p, $q) { "$p-$q" }, "a", "b"), "\n";
my @subs = (sub ($n) { $n + 1 }, sub ($n) { $n * 10 });
print join(",", map { $_->(4) } @subs), "\n";
print eval q{my $e = sub ($v, $w = 9) { "$v/$w" }; $e->(1) . ";" . $e->(1,2)}, "\n";
');

# A real PROTOTYPE on an anon sub binds nothing and must stay untouched
# (it is a syntax error to write one with signatures enabled, so this
# runs in its own feature-free snippet).
test_transpile("anon-sub prototypes are not signatures", '
my $pair = sub ($$) { "$_[0]-$_[1]" };
my $blk  = sub (&@) { my $f = shift; join ",", map { $f->($_) } @_ };
my $none = sub () { 7 };
print $pair->(1,2), " ", $blk->(sub { $_[0]*2 }, 3, 4), " ", $none->(), "\n";
');

# V-string family (op/ver.t, s316l): underscores are digit separators;
# a scalar holding a v-string answers ref \$v = "VSTRING" until any write
# flattens it (s///, y///); dotless vNN is a bareword in autoquote position
# (fat comma, hash subscript) and a CALL when such a sub is declared (the
# "poetry optimization"), but dotted forms always engage v-stringness;
# v48 is the string "0", which is FALSE.
test_transpile("v-string literals: underscores, VSTRING ref, flatten, autoquote, poetry", '
sub v77 { "ok" }
my $x = v77;
my $v = v1.2_3;
print sprintf("%vd", $v), " ", ref(\$v), " ", $x, "\n";
my %h = (v65 => 42);
print( (exists $h{v65} ? "y" : "n"), (v48 ? "t" : "f") );
%h = (v65.66 => 1);
print exists $h{chr(65).chr(66)} ? "y" : "n";
%h = (65.66.67 => 1);
print( (exists $h{chr(65).chr(66).chr(67)} ? "y" : "n"), "\n" );
$v = 1.2.3;
print ref(\$v), "\n";
$v =~ s/\x01/\x01/;
print ref(\$v), " ", sprintf("%vd", $v), "\n";
my $t = v102;
$t =~ y/f/g/;
print ref(\$t), " $t\n";
');

# $| write magic (op/ver.t 48): every write clamps to 0/1 by truthiness,
# so --$| toggles; local $| resets to 0 and keeps the clamp in scope.
test_transpile("\$| clamps writes to 0/1; local keeps the magic", '
$| = 1; --$|; --$|; print "p:$|\n";
$| = 5;  print "q:$|\n";
{ local $|; print "r:$|\n"; $| = 2; print "s:$|\n"; }
print "t:$|\n";
');

# s/// replacement is double-quoted context: \n / \x41 / \x{42} become
# characters at transpile time; \1 stays a backref; \\ is one literal
# backslash (was emitted raw, so PCL printed the escape text verbatim).
test_transpile("s/// replacement processes dq escapes", '
my $s = "ab"; $s =~ s/a/\n/;      print join(",", map { ord } split //, $s), "\n";
$s = "cd";    $s =~ s/c/\x41/;    print $s, "\n";
$s = "ef";    $s =~ s/e/\x{42}/;  print $s, "\n";
$s = "gh";    $s =~ s/(g)/<$1\t>/; print $s, "\n";
$s = "ij";    $s =~ s/(i)/\1\1/;  print $s, "\n";
$s = "kl";    $s =~ s/k/\\\\/;    print $s, "\n";
');

# A signature's slurpy %hash must receive an EVEN number of leftover args —
# perl dies "Odd name/value argument for subroutine" (op/signatures.t).
# Both lowering paths: named (p-check-arity hash-start) and anon (desugar
# prologue die).
test_transpile("slurpy %hash signature dies on odd leftover args", '
use feature "signatures"; no warnings;
sub tC ($a, %h) { "C" . join(",", map { "$_=$h{$_}" } sort keys %h) }
sub tD (%h) { "D" . scalar(keys %h) }
my $c = sub ($a, %h) { "A" . scalar(keys %h) };
for my $t ([\&tC,1], [\&tC,2], [\&tC,3], [\&tD,0], [\&tD,1], [\&tD,2], [$c,1], [$c,2]) {
  my ($f, $n) = @$t;
  my $r = eval { $f->(1..$n) };
  print defined $r ? $r : "die", "\n";
}
');

# The :prototype(...) attribute (perl 5.20+) declares a prototype while the
# paren list stays a signature.  Desugared at the shared PPI entry into
# __pcl_set_prototype registration; prototype()/Sub::Util::set_prototype read
# the same runtime registry.  The anon form also guards attribute-skipping in
# _desugar_anon_signatures (an attribute between `sub` and the signature used
# to derail the whole statement — op/signatures.t t118).
test_transpile(":prototype attribute on named and anon subs", '
use feature "signatures"; no warnings;
sub tP :prototype($) ($a) { $a || "z" }
print( (prototype(\&tP) // "undef"), "\n");
print tP(456), "\n";
sub tQ :prototype(@) ($a) { $a }
print( (prototype(\&tQ) // "undef"), "\n");
print( (prototype(\&tR) // "undef"), "\n");
sub tR ($a) { $a }
my $c = sub :prototype($$) ($x, $y) { $x + $y };
print( (prototype($c) // "undef"), "\n");
print $c->(2, 3), "\n";
use Sub::Util ();
my $d = Sub::Util::set_prototype("\$\$", sub { 9 });
print( (prototype($d) // "undef"), "\n");
print $d->(), "\n";
');

# Three signature-family fixes (op/signatures.t):
# - __SUB__ in a NAMED sub (body or sig default) rewrites to (\&name) at the
#   shared PPI entry — the runtime pl-__SUB__ is a no-op stub, so the
#   recursive-default t122 pattern silently returned "".
# - A signatured anon sub NESTED in another signature's default desugars too
#   (fixpoint loop; one pass missed tree-spliced inner subs — t135).
# - Signature text may contain comments, spaced sigils and repeated commas
#   (t086/t087) — normalized once in _signature_param_specs.
test_transpile("signature defaults: __SUB__ recursion, nested sig subs, comments in sig", '
use feature "signatures", "current_sub"; no warnings;
sub t122 ($c = 5, $r = $c > 0 ? __SUB__->($c - 1) : "") { $c.$r }
print t122(), "\n";
print t122(1), "\n";
sub body ($n) { $n > 0 ? $n . __SUB__->($n - 1) : "x" }
print body(3), "\n";
sub t135 ($a = sub ($a, $t = sub ($p) { $p."p" }) { $t->($a)."z" }) {
    $a->("a")."/".$a->("b", sub { $_[0]."q" } )
}
print t135(), "\n";
sub t086
    ( #foo)))
    $ #foo)))
    a#foo)))
    , #foo)))
    ,#foo)))
    $ #foo)))
    b #foo)))
    = #foo)))
    333 #foo)))
    , #foo)))
    ) #foo)))
    { $a.$b }
print t086(456), "\n";
print t086(456, 789), "\n";
print( (eval { t086() } // "die-few"), "\n");
print( (eval { t086(1,2,3) } // "die-many"), "\n");
');

# lib/Try/Tiny.pm shim: finally runs by DIRECT CALL (success path no args,
# failure path gets the error, still runs when catch itself dies, exceptions
# inside finally are warned not propagated), $@ is untouched after try, and
# try/catch keep their context split.  Upstream runs finallys from a
# DESTROY scope guard PCL cannot fire.
test_transpile("Try::Tiny shim: finally direct-call semantics", '
use Try::Tiny;
my @log;
my $r = try { "ok-val" } finally { push @log, "fin:@_" };
push @log, "r=$r";
try { die "boom\n" } catch { push @log, "caught:$_" } finally { push @log, "fin2:$_[0]" };
$@ = "keepme";
try { 42 };
push @log, "at:$@";
my $out = eval {
  try { die "a\n" } catch { die "b\n" } finally { push @log, "fin3:$_[0]" };
  1;
};
push @log, "rethrow:$@" if !$out;
{
  local $SIG{__WARN__} = sub { push @log, "warned" if $_[0] =~ /CAN NOT BE PROPAGATED/ };
  try { 1 } finally { die "finboom\n" };
}
print map { s/\n/./gr . "\n" } @log;
');

# Task #131 (defelem arg-aliasing): a hash/array ELEMENT passed to a user
# sub aliases through @_ — writes to $_[0] reach the caller's element, an
# absent element vivifies only when written (never on a read-only call),
# and a write past the end extends the array.  Named subs, coderef calls,
# and method calls; ordinary copy idioms (my ($x) = @_, shift) still copy.
test_transpile("element args alias through \@_ (defelem)", '
no warnings;
my %h = (k => 1, j => 2);
my @a = (10, 20);
sub w { $_[0] = 99 }
w($h{k}); w($a[1]); print "w:$h{k},$a[1]\n";
w($h{new}); print "viv:", (exists $h{new} ? $h{new} : "none"), "\n";
sub r { my ($x) = @_; $x = 0; }
r($h{ro}); print "ro:", (exists $h{ro} ? "BAD" : "ok"), ",$h{k}\n";
my $c = sub { $_[0]++ };
$c->($h{j}); $c->($h{j}); print "inc:$h{j}\n";
$c->($a[5]); print "ext:", scalar(@a), ":$a[5]\n";
package P { sub m2 { $_[1] = 7 } }
P->m2($h{k}); print "meth:$h{k}\n";
');

# op/signatures.t t017/t021: a PARENLESS call to a known non-prototyped
# sub in a signature default is a LIST OPERATOR — it swallows the rest of
# the signature into its argument list (ONE param).  A :prototype($) sub
# is unary and does NOT swallow (the attribute registers at parse time);
# an explicitly parenthesized call does not either.
test_transpile("parenless list-op call swallows the sig default", '
use feature "signatures"; no warnings;
our $a;
sub t018 { join("/", @_) }
sub t017 ($p = t018 222, $a = 333) { $p // "z" }
print t017(), " a:$a\n";
$a = 123;
print t017(456), " a:$a\n";
print( (defined eval("t017(456, 789)") ? "two-params" : "one-param"), "\n");
sub t020 :prototype($) { $_[0]."z" }
sub t021 ($p = t020 222, $a = 333) { "$p/$a" }
print t021(), "|", t021(456,789), "\n";
sub t019 ($p = t018(222), $a = 333) { "$p:$a" }
print t019(1), "|", t019(1,2), "\n";
');

# Task #134 (op/signatures.t t126/t127): `state $s = INIT` in a signature
# default runs INIT only on the FIRST defaulted call; later defaulted calls
# see $s's current value.  Both the whole-expression form and the do-block
# form; passing an arg must not touch the state or run INIT.
test_transpile("state in signature default initializes once", '
use feature "signatures"; use feature "state"; no warnings;
our $z;
sub t126 ($c = (state $s = $z++)) { $c }
sub t127 ($c = do { state $s = $z++; $s++ }) { $c }
$z = 222;
print t126(456), " ", t126(), " z=$z\n";
print t126(), " z=$z\n";
print t127(), " ", t127(), " z=$z\n";
print t127(456), " ", t127(), " z=$z\n";
');

# Task #137 (op/lex_assign.t t133): assignment binds TIGHTER than `,` and
# `or` — `$a = readlink 'x', 'y'` is `($a = readlink 'x'), 'y'` (named
# unary takes ONE arg; $a stays undef), and `$a = 0 or f()` is
# `($a = 0) or f()` ($a must be 0, not f()'s value).  A parenless LIST-OP
# call still swallows the comma: `$a = two 1, 2` passes both args.  The
# v2 native statement split folded such tails into the RHS.
test_transpile("assignment binds tighter than comma/or at statement level", '
sub two { $_[0] + $_[1] }
my $a; $a = readlink "pcl-nx", "pcl-ny";
print "comma:", (defined $a ? "leaked" : "undef"), "\n";
my $b; $b = 0 or print "short\n";
print "or:$b\n";
my $c; $c = two 1, 2;
print "listop:$c\n";
my $d; $d = 5, 7;
print "plain:$d\n";
');

# Task #137 (op/lex_assign.t t144-146, op/waitpid.t): process-group
# builtins + POSIX WNOHANG + short-list utime.  Values vary by process,
# so assert relations, not absolutes; getpriority with a bogus WHICH is
# perl-style -1 + $! (EINVAL), never a die.
test_transpile("getpgrp/setpgrp/getpriority/WNOHANG/short utime", '
use POSIX qw(WNOHANG);
print "wnohang:", WNOHANG, "\n";
print "wait:", waitpid(0, WNOHANG), "\n";
print "pgrp:", (getpgrp() == getpgrp(0) ? "eq" : "ne"), "\n";
print "setpgrp:", join("_", setpgrp(0)), "\n";
print "prio:", (getpriority(0, 0) == getpriority(0, $$) ? "eq" : "ne"), "\n";
$! = 0;
my $bad = getpriority(12345, 0);
print "prio-bad:$bad:", ($! ? "err" : "noerr"), "\n";
my $u = utime "pcl-nonexistent-file";
print "utime:$u\n";
');

# Task #138: assignment binds TIGHTER than `,`/`=>`/`or`/`and`/`xor`, so a
# depth-0 tail after the init is a SEPARATE thing — `my $x = 1, $y = 2` is
# `(my $x = 1), ($y = 2)`.  Every statement handler that sliced "everything
# after the `=`" folded the tail into the value.  The discriminator in each
# case is the parenless list operator, whose comma really IS part of the init
# (`my $c = h 1, 2` passes both args) — hence the shared classifier rather
# than a per-site scan.
test_transpile("my-decl init stops at a depth-0 comma/or tail", '
sub h { print "h(@_)\n"; return 7 }
my $x = 1, $y = 2;
print "x=$x y=$y\n";
my $w = 0 or print "or-tail ran\n";
print "w=$w\n";
my $c = h 1, 2;
print "c=$c\n";
my $d = 3, h(9);
print "d=$d\n";
sub f { my $t = 1, $u = 2 }
print "tailval=", f(), "\n";
');

# The worst one: _extract_params matched `my (LIST) = @_` with >= 4 tokens and
# the caller DELETED the whole statement, so the tail vanished from the output
# entirely — silent statement-text deletion, not merely a wrong value.
test_transpile("my (LIST) = \@_ param fast path keeps its comma tail", '
sub g { print "g ran\n"; return 9 }
sub p1 { my ($a) = @_, g(); print "a=$a\n" }
p1(5);
sub p2 { my ($a, $b) = @_; print "ab=$a$b\n" }
p2(1, 2);
');

# Only the ASSIGNMENT is once-guarded, so a state decl tail runs on EVERY
# call; an `or` tail re-tests the cell each time.  A parenless list-op init
# (`\substr $s, $i, 1`) must stay whole — both the named-sub route and the
# anon-sub source-rewrite route.
test_transpile("state init: comma tail runs every call, list-op init whole", '
sub st { state $s = 1, print("tail ran\n"); $s++; print "s=$s\n" }
st(); st();
my $an = sub { state $t = 0 or print "or ran t=$t\n"; $t = 7 if !$t; print "t=$t\n" };
$an->(); $an->();
my $str = "hello"; my $i = 0;
sub sb { state $c = \substr $str, $i, 1; print "c=$$c\n" }
sb(); sb();
');

# `for (my $i = 0, $j = 9; ...)`: the folded init started $i at 9, so the loop
# ran ZERO times.  The two-`my` form already had its own carve-out; this is
# its one-`my` sibling.
test_transpile("C-style for init stops at a depth-0 comma", '
for (my $i = 0, $j = 9; $i < 2; $i++) { print "i=$i j=$j\n" }
for (my $k = 0; $k < 2; $k++) { print "k=$k\n" }
for (my $m = 0, my $n = 5; $m < 2; $m++) { print "m=$m n=$n\n" }
');

# `local $x = A, B` — the tail runs INSIDE the localization (perl evaluates it
# after the local takes effect), and only the localized name is restored.
test_transpile("local init stops at a depth-0 comma", '
$l = "outer"; $m = "outerm";
sub L { local $l = 1, $m = 2; print "in: l=$l m=$m\n" }
L(); print "out: l=$l m=$m\n";
sub L2 { local $n = join ",", 1, 2; print "n=$n\n" }
L2();
');

# Task #140: the `OP=` operator set had four hand-rolled copies, and two of
# them omitted the string-bitwise trio `&.= |.= ^.=` (feature 'bitwise').
# Both omissions were live: the foreach-range split fired on
# `for ($y |.= "a" .. 3)` — assignment binds LOOSER than `..`, so the range
# is the assignment's RHS and the loop runs ONCE, not over 0..3 — and the
# state-decl normalization never rewrote `state ($u) |.= "a"`.
test_transpile("state (\$x) OP= covers the string-bitwise trio", '
use feature "bitwise";
sub s1 { state ($t) //= 3; print "t=$t\n"; $t++ }
s1(); s1();
sub s2 { state ($u) |.= "a"; print "u=$u\n" }
s2(); s2();
sub s3 { state ($v) .= "b"; print "v=$v\n" }
s3(); s3();
');

test_transpile("foreach-range split stops at every assignment operator", '
use feature "bitwise";
my $n = 0;
for ($n |.= "a" .. 3) { print "n-iter\n" }
my $m = 0;
for ($m += 1 .. 3) { print "m-iter\n" }
for (1 .. 3) { print "plain\n" }
');

# vec() with BITS=64: BOTH p-vec and p-vec-set had per-width branches for
# 1/2/4/8/16/32 and no 64, so the reader fell through to a 0 default and the
# writer extended the string to the right length then wrote NOTHING — an
# all-zero result, silently, even though both docstrings listed 64 as legal
# (t/op/64bitint.t t80/t81).  Each side is now one loop over bits/8 bytes, so
# a missing width cannot recur; the other widths are asserted here too.
test_transpile("vec 64-bit round-trip, and the narrower widths unchanged", '
my $q = 0x1234567890abcdef;
my $x = "";
vec($x, 1, 64) = $q;
printf "len=%d bytes=%s\n", length($x), join("", map { sprintf "%02x", ord } split //, $x);
printf "r64=%s eq=%s\n", vec($x,1,64), (vec($x,1,64) == $q ? "YES" : "NO");
printf "r0=%s r2=%s\n", vec($x,0,64), vec($x,2,64);
my $y = "";
vec($y,0,16) = 0xBEEF; vec($y,3,8) = 0x41; vec($y,9,4) = 0xC;
printf "w16=%s w8=%s w4=%s w32=%s\n", vec($y,0,16), vec($y,3,8), vec($y,9,4), vec($y,0,32);
');

# rand/srand: perl ships its OWN drand48 since [perl #115928] so a seed
# replays the same sequence everywhere — t/op/rand.t asserts the exact value.
# PCL had discarded the seed entirely, and treated a supplied rand(0) as a
# zero range instead of perlfunc\'s "omitted or zero, uses 1".
test_transpile("srand replays perl\'s drand48 sequence; rand(0) is rand(1)", '
srand(1);
print "first=", int(rand(1000)), "\n";
srand(42); my @a = map { sprintf "%.6f", rand } 1..3;
srand(42); my @b = map { sprintf "%.6f", rand } 1..3;
print "replay=", ("@a" eq "@b" ? "YES" : "NO"), "\n";
srand(7); my $z = rand(0);
srand(7); my $o = rand(1);
print "zero_is_one=", ($z == $o ? "YES" : "NO"), "\n";
print "in_range=", (do { srand(3); my $r = rand; $r >= 0 && $r < 1 }) ? "YES" : "NO", "\n";
');

done_testing();
