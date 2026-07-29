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

done_testing();
