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

use Test::More tests => 40;
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
