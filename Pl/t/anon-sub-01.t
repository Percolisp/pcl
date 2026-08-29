#          -*-Mode: CPerl -*-
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Test anonymous subroutines in map/grep/sort

use v5.32;
use strict;
use warnings;

use lib ".";


use PPI;
use PPI::Dumper;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

use Test::More tests => 59;
BEGIN { use_ok('Pl::PExpr') };

my $code;
my $doc;
my $expr_o;
my $expr;
my $node_id;

# ----------------------------------------------------------------------
# Helper functions

sub parse_expr {
  my $code    = shift;

  my $doc     = PPI::Document->new(\$code);
  my $expr    = _get_ppi_part($doc);
  my $expr_o  = Pl::PExpr->new(
      e => $expr,
      full_PPI => $doc,
  );

  my $node_id = $expr_o->parse_expr_to_tree($expr);

  return ($expr_o, $node_id);
}

sub _get_ppi_part {
  my $doc     = shift;
  my $stmt_ix = shift // 0;

  my @stmts;
  my @parts;
  if (ref($doc) eq 'PPI::Document') {
    @stmts    = $doc->children();
    @parts    = $stmts[$stmt_ix]->children();
  } elsif (ref($doc) eq 'PPI::Statement') {
    @parts    = $doc->children();
  } else {
    die "Code is not PPI::Document?? Is: " . ref($doc);
  }

  return \@parts;
}

sub verify_funcall {
  my ($expr_o, $node_id, $func_name, $msg) = @_;
  
  my $node = $expr_o->get_a_node($node_id);
  ok($expr_o->is_internal_node_type($node), "$msg - is internal node");
  is($node->{type}, 'funcall', "$msg - is funcall");
  
  my $children = $expr_o->get_node_children($node_id);
  my $func_node = $expr_o->get_a_node($children->[0]);
  is($func_node->content(), $func_name, "$msg - function name is $func_name");
  
  return $children;
}

sub verify_has_anon_sub {
  my ($expr_o, $children, $msg) = @_;
  
  # Second child should be anon_sub (first arg to map/grep/sort)
  ok(scalar(@$children) >= 2, "$msg - has at least 2 children");
  
  my $block_node = $expr_o->get_a_node($children->[1]);
  ok($expr_o->is_internal_node_type($block_node), "$msg - first arg is internal node");
  is($block_node->{type}, 'anon_sub', "$msg - first arg is anon_sub");
}

# ----------------------------------------------------------------------
# Tests

diag "";
diag "-------- map with Block:";

# map { $_ * 2 } @array
($expr_o, $node_id) = parse_expr('map { $_ * 2 } @array');
my $children = verify_funcall($expr_o, $node_id, 'map', "map with block");
verify_has_anon_sub($expr_o, $children, "map has anon_sub");

# Check that it has 3 children: map, block, @array
is(scalar(@$children), 3, "map has 3 children (func, block, array)");


diag "";
diag "-------- grep with Block:";

# grep { $_ > 5 } @list
($expr_o, $node_id) = parse_expr('grep { $_ > 5 } @list');
$children = verify_funcall($expr_o, $node_id, 'grep', "grep with block");
verify_has_anon_sub($expr_o, $children, "grep has anon_sub");

is(scalar(@$children), 3, "grep has 3 children (func, block, list)");


diag "";
diag "-------- sort with Block:";

# sort { $a <=> $b } @numbers
($expr_o, $node_id) = parse_expr('sort { $a <=> $b } @numbers');
$children = verify_funcall($expr_o, $node_id, 'sort', "sort with block");
verify_has_anon_sub($expr_o, $children, "sort has anon_sub");

is(scalar(@$children), 3, "sort has 3 children (func, block, numbers)");


diag "";
diag "-------- Hash vs Block Disambiguation:";

# Hash constructor (has =>)
($expr_o, $node_id) = parse_expr('{ key => value }');
my $node = $expr_o->get_a_node($node_id);
is($node->{type}, 'hash_init', "Hash with => is hash_init, not anon_sub");

# Block in map (no =>)
($expr_o, $node_id) = parse_expr('map { $_ * 2 } @x');
$children = $expr_o->get_node_children($node_id);
my $block = $expr_o->get_a_node($children->[1]);
is($block->{type}, 'anon_sub', "Block without => becomes anon_sub in map");


diag "";
diag "-------- Block Metadata:";

# TODO: Block metadata tracking not yet implemented
# The get_metadata call would need to be on $expr_o->node_tree
# and the block_context would need to be set during parsing
SKIP: {
  skip "Block metadata not yet implemented", 1;
  ($expr_o, $node_id) = parse_expr('map { $_ } @x');
  $children = $expr_o->get_node_children($node_id);
  my $block_id = $children->[1];
  my $metadata = $expr_o->node_tree->get_metadata($block_id, 'block_context');
  is($metadata, 'map', "Block metadata records function context");
}


diag "";
diag "-------- Non-Block Functions:";

# Regular function calls should not create anon_sub
($expr_o, $node_id) = parse_expr('push @array, $value');
$children = verify_funcall($expr_o, $node_id, 'push', "push is regular funcall");

# First arg should NOT be anon_sub
my $first_arg = $expr_o->get_a_node($children->[1]);
ok(!($expr_o->is_internal_node_type($first_arg) && $first_arg->{type} eq 'anon_sub'),
   "push first arg is not anon_sub");


diag "";
diag "-------- task #516: `(sub {...})->()` — the paren-scalar-base family's";
diag "-------- fourth member, `->(`";

# `->` on a PARENTHESISED expression dereferences ONE scalar value; the paren
# group is not a list.  ExprToCL's other three postfix-arrow emitters
# (`->method`, `->[i]`, `->{k}`) already ask _is_paren_scalar_base and lower
# such a base in SCALAR context; gen_ref_funcall_form did not.  So in LIST
# context the group lowered to `(vector (lambda ...))` and p-funcall-ref
# rejected the vector — "Not a CODE reference", fatal, taking the whole file
# with it.  In SCALAR context the progn collapsed on its own, which is why
# `my $s = (sub{...})->()` always worked and the SAME expression inside a
# `print` list did not.  Rows 1-7 all died on the base tree; 8, 9, 10 are the
# inverses that were already right and must stay right (verified by an inverse
# run on a 23e3005 worktree, s443h).

my $project_root = "$RealBin/../..";
my $pl2cl_bin    = "$project_root/pl2cl";
my $runtime_lisp = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime_lisp);

sub _write_pl {
    my ($src) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $pl_file;
}

sub _run_pcl {
    my ($src) = @_;
    my $cl = PCLCore::transpile("$pl2cl_bin --no-cache " . _write_pl($src));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    chomp $out;
    return $out;
}

sub both_agree {
    my ($src, $desc) = @_;
    my $perl = `perl @{[ _write_pl($src) ]} 2>&1`;
    chomp $perl;
    my $pcl  = _run_pcl($src);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")")
        or diag "pcl=[$pcl]";
}

my $F = 'sub f { return "F(@_)" }' . "\n";

# ---- the bug: `->(` on a paren group, in LIST context ---------------------

both_agree('print "c3 [", (sub { return "x" })->(), "]\n";',
           '#516 the repro: an immediately-called anon-sub LITERAL');

both_agree('print "[", (sub { my @a = @_; return "y@a" })->(1,2), "]\n";',
           '#516 ... with arguments');

both_agree('print "[", (sub { return sub { return "z" } })->()->(), "]\n";',
           '#516 ... chained: a sub that returns a sub');

both_agree($F . 'print "[", (\&f)->(7), "]\n";',
           '#516 ... a \&NAME code ref in parens');

both_agree($F . 'my $c = \&f; print "[", ($c)->(9), "]\n";',
           '#516 ... a parenthesised SCALAR (not anon-sub-specific)');

both_agree($F . 'sub mk { return \&f } print "[", (mk())->(3), "]\n";',
           '#516 ... a CALL that returns a code ref');

both_agree('my @r = (sub { return (1,2,3) })->(); print "[", scalar(@r), " @r]\n";',
           '#516 ... list context: the sub is called, its LIST returned');

# ---- the inverses: already right before the fix, must stay right ----------

both_agree('my $s = (sub { return (1,2,3) })->(); print "[$s]\n";',
           '#516 inverse: SCALAR context was already right');

both_agree('(sub { print "[void]\n" })->();',
           '#516 inverse: VOID context was already right');

both_agree('print "[", (sub{ "A" }, sub{ "B" })[1]->(), "]\n";',
           '#516 inverse: a LIST SLICE followed by ->() is a list, not a base');

# ---- the emission shape (what the vector was) ----------------------------

{
    my $cl = PCLCore::transpile("$pl2cl_bin --no-cache "
                 . _write_pl('print "a", (sub { return 1 })->(), "\n";'));
    unlike($cl, qr/\(p-funcall-ref\s*\(vector/,
           '#516 shape: the invocant of ->( is never wrapped in a (vector ...)');
}

diag "";
diag "-------- task #527: a MULTI-element paren base, for all four members";

# #516's residue.  _is_paren_scalar_base required EXACTLY ONE child, but perl's
# rule for the invocant of a postfix `->` is the comma operator's LAST element
# in scalar context -- every element still evaluated, for its side effects.  So
# `(1,2,$r)->[1]` was 2 (the group lowered as a LIST and the subscript indexed
# IT: silent wrong), while `->{k}` / `->(...)` / `->method` died "Not a HASH
# reference" / "Not a CODE reference" / "on unblessed reference".  The first
# FIVE rows diverged on the base tree (verified by an inverse run, s448p); the
# rest are the shapes that were already right and must stay right -- in
# particular the LIST SLICE `(1,2,3)[1]`, a different operator that never
# reaches this predicate (PExpr marks it list_ctx_subscript), and the
# SCALAR-context spelling, where the progn already collapsed on its own.

my $R = 'my $r = [10,20]; my $h = {a=>"HA"}; my $cr = sub { "CR(@_)" };' . "\n"
      . 'package O1; sub new { bless {n=>"obj"}, shift } sub m1 { "M1:" . $_[0]{n} }'
      . "\npackage main; my \$o = O1->new;\n";

both_agree($R . 'print "[", (1,2,$r)->[1], "]\n";',
           '#527 the repro: a multi-element paren base of ->[i]');

both_agree($R . 'print "[", (1,2,$h)->{a}, "]\n";',
           '#527 ... of ->{k}');

both_agree($R . 'print "[", (1,2,$cr)->(4), "]\n";',
           '#527 ... of ->(...)');

both_agree($R . 'print "[", (1,2,$o)->m1(), "]\n";',
           '#527 ... of ->method');

# perl evaluates EVERY element of the comma expression, not just the last.
# Spelled in LIST context (inside a print list) — that is the position the bug
# lived in; the SCALAR spelling `my $v = (f(),g(),$r)->[0]` collapsed the progn
# on its own and was already right, exactly as #516's inverse rows record.
both_agree('my @log; sub f { push @log, "f"; 1 } sub g { push @log, "g"; 2 }
            my $r = [10,20];
            print "[", (f(),g(),$r)->[0], " ", join(",",@log), "]\n";',
           '#527 ... and every element is evaluated for its side effects');

both_agree('my @log; sub f { push @log, "f"; 1 } sub g { push @log, "g"; 2 }
            my $r = [10,20];
            my $v = (f(),g(),$r)->[0];
            print "[$v ", join(",",@log), "]\n";',
           '#527 inverse: the SCALAR-context spelling was already right');

# ---- the inverses: already right before the fix, must stay right ----------

both_agree($R . 'print "[", ($r)->[1], " ", ($h)->{a}, " ", ($cr)->(9), " ", ($o)->m1(), "]\n";',
           '#527 inverse: the SINGLE-element paren base of all four');

both_agree($R . 'print "[", ($r//0)->[0], "]\n";',
           '#527 inverse: a single-element base that is an expression');

both_agree('print "[", ((1,2,3)[1]), " ", join(",", (10,20,30)[0,2]), "]\n";',
           '#527 inverse: the LIST SLICE (LIST)[i] is a different operator');

# ... and its qw spelling.  `qw(a b c)[2]` IS `(LIST)[2]`, but PExpr's
# qw-subscript branch never set the `list_ctx_subscript` marker its paren twin
# sets -- the shape got by on the base "happening" to be a multi-child node.
# The moment #527 taught _is_paren_scalar_base that a multi-element group IS a
# scalar base, all FOUR qw-slice sites in perl-tests (array.t, context.t,
# flip.t, list.t) started dereferencing the LAST WORD; setting the marker is
# the fix, and it also closes the single-word spelling, which answered EMPTY
# on the base tree where perl says the word (measured s448p).
both_agree('print "[", qw(foo bar snorfle)[2], " ", join("", qw(a b c)[2,0,1]),
                  " ", qw(only)[0], " ", join(",", qw(x y z)[0..1]), "]\n";',
           '#527 the qw-slice spelling of (LIST)[i], including a ONE-word list');

both_agree('sub cx { my $c = qw[void scalar list][wantarray + defined wantarray]; $c }
            my @l = cx(); my $s = cx();
            my @sl = qw(a b)[2,3];
            print "[$l[0] $s ", scalar(@sl), "]\n";',
           '#527 ... the corpus shapes: a wantarray-indexed qw slice and an out-of-range one');

# The BRACED spelling of the same base, `${ EXPR }[i]` -- t/op/gmagic.t:109 and
# :114, and the only diff the 1036-file A/B found for this fix.  Same rule: the
# block's value in scalar context, so a multi-element group is the comma
# operator.  All four rows below were EMPTY on the base tree; the first two are
# LVALUES.
both_agree('my $r = [1,2,3]; my $s = \$r; my $true = 1; my $h = {a=>"HA"};
            ${ (), $$s }[0] = 73;
            ( ! $true ? undef : $$s )->[0] = 74;
            print "[", $$s->[0], " ", ${ (), $$s }[1], " ", ${ 0, $$s }[2],
                  " ", ${ (), $h }{a}, "]\n";',
           '#527 the braced ${ EXPR }[i] spelling, as an lvalue and as a read');

both_agree('my @plain = (1,2,3); my @nested = ((1,2,3));
            print "[", scalar(@plain), scalar(@nested), "]\n";',
           '#527 inverse: a paren list with NO arrow stays a list');

# What the fix newly makes reachable: the base as an LVALUE, a chained arrow
# off it, a class NAME in it, and void context.  All four died on the base tree.
both_agree('package C9; sub hi { "HI" }
            package main;
            my $r = [10,20]; my $h = {a=>"HA"}; my $deep = { x => [5,6] };
            (1,2,$r)->[0] = 99;
            (0,$h)->{a} = "NEW";
            (1,2,$r)->[1];
            print "[$$r[0] $h->{a} ", (1,$deep)->{x}[1], " ", (1,2,"C9")->hi, "]\n";',
           '#527 the multi-element base as an lvalue, chained, a class name, and in void context');

diag "";
diag "-------- task #611: the scalar context must reach through a NESTED paren";

# #527's residue.  `_gen_scalar_deref_base_form` forces SCALAR_CTX on the base
# of a postfix `->` at EMIT time, but `annotate_contexts` had already stamped
# the whole subtree -- and `child_context` gives a progn's children LIST_CTX
# unconditionally ("progn (comma operator) forces list context").  A base that
# is itself a TRANSPARENT paren layer collapses to its child's form, so the
# inner group still lowered as `(vector 0 $h)` and the arrow dereferenced the
# vector: `((0,$h))->{k}` DIED "Not a HASH reference" while the depth-1
# spelling `(0,$h)->{k}` was right.  The fix is the SCALAR_CTX counterpart of
# the LIST_CTX push-down in gen_progn_form / gen_tree_val_form, applied to the
# LAST child only -- the comma operator's value in scalar context.
#
# Rows 1-2 diverged on the base tree (`->[i]` was a silent wrong, the other
# three died); rows 3-4 are the inverses that were already right.

my $N = 'my $r = [10,20,30]; my $h = {k=>"HK"}; my $cr = sub { "CR(@_)" };' . "\n"
      . 'package O6; sub new { bless {}, shift } sub m1 { "M1" }'
      . "\npackage main; my \$o = O6->new;\n";

both_agree($N . 'print "[", ((0,$r))->[1], " ", ((0,$h))->{k}, " ",
                        ((0,$cr))->("x"), " ", ((0,$o))->m1, "]\n";',
           '#611 the repro: one transparent paren layer, all four members');

both_agree($N . 'print "[", (((0,$r)))->[1], " ", (((0,$h)))->{k}, "]\n";',
           '#611 ... and two layers deep');

both_agree('my @log; sub s6 { push @log, $_[0]; return $_[1] }
            my $h = {k=>"HK"};
            print "[", ((s6(1,0), s6(2,$h)))->{k}, " ", join(",",@log), "]\n";',
           '#611 ... every element still evaluated, in order');

both_agree('print "[", join("|", ((1,2,3))[1]), " ", join("|", (1,2,3)[1,2]), "]\n";',
           '#611 inverse: a nested LIST SLICE is still a list slice');

both_agree('my @n = ((1,2,3)); my $c = ((1,2,3));
            print "[", scalar(@n), " $c]\n";',
           '#611 inverse: a nested paren list with NO arrow keeps its own context');
