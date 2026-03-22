package Pl::Parser;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.30;
use strict;
use warnings;

use Moo;

use PPI;
use Data::Dump qw/dump/;
use File::Basename;
use File::Spec;
use Cwd qw(abs_path);

use Pl::PExpr;
use Pl::ExprToCL;
use Pl::Environment;

# Statement-level parser prototype.
# Iterates over PPI statements, delegates expressions to PExpr,
# generates Common Lisp via ExprToCL.

has filename => (
  is        => 'ro',
  predicate => 'has_filename',
);

has code => (
  is        => 'ro',
  predicate => 'has_code',
);

has ppi_doc => (
  is        => 'lazy',
);

has indent_level => (
  is        => 'rw',
  default   => 0,
);


has output => (
  is        => 'rw',
  default   => sub { [] },
);

# Output bucket system (replaces flat @output + post-processing)
# Each section = one package entry point; buckets are assembled in order:
#   preamble → declarations → definitions → runtime
has _sections => (
  is      => 'rw',
  default => sub { [] },
);
has _cur_section => (
  is      => 'rw',
  default => 0,
);
has _cur_bucket => (
  is      => 'rw',
  default => 'runtime',
);
has _block_depth => (
  is      => 'rw',
  default => 0,
);

has environment => (
  is        => 'lazy',
);

# @INC paths for module lookup (transpile-time)
# Computed at compile time: project_root/lib + Perl's @INC
my $_pcl_lib_dir = File::Spec->catdir(dirname(dirname(abs_path(__FILE__))), 'lib');
has inc_paths => (
  is        => 'rw',
  default   => sub { [$_pcl_lib_dir, @INC] },  # Include project lib/ + Perl's @INC
);

# Track modules currently being parsed (cycle detection)
has _parsing_modules => (
  is        => 'rw',
  default   => sub { {} },
);


# Flag to suppress output emission (for prototype-only parsing)
has collect_prototypes_only => (
  is        => 'rw',
  default   => 0,
);

# DEBUG/TEST FLAG: When set, truncate file at first PPI-unparseable line
# instead of dying. Used by run-perl-test.pl and sweep-perl-tests.pl to get
# partial results from files with one exotic line PPI can't handle.
# Do NOT enable in production — silently dropping code is dangerous.
has lenient_ppi => (
  is        => 'ro',
  default   => 0,
);


sub _build_environment {
  my $self = shift;
  return Pl::Environment->new(
    source_file => $self->has_filename ? $self->filename : '-',
  );
}


sub _build_ppi_doc {
  my $self = shift;

  if ($self->has_filename) {
    my $doc = PPI::Document->new($self->filename);
    return $doc if $doc;
    die "Failed to parse file: " . $self->filename unless $self->lenient_ppi;
    open(my $fh, '<', $self->filename)
      or die "Failed to parse file (and can't re-read): " . $self->filename;
    my $src = do { local $/; <$fh> };
    close $fh;
    return $self->_ppi_with_fallback($src);
  }
  elsif ($self->has_code) {
    my $code = $self->code;
    my $doc = PPI::Document->new(\$code);
    return $doc if $doc;
    die "Failed to parse code" unless $self->lenient_ppi;
    return $self->_ppi_with_fallback($code);
  }
  else {
    die "Must provide either 'filename' or 'code'";
  }
}

# DEBUG/TEST: binary-search for the first line PPI can't parse, truncate there.
# Only called when lenient_ppi is set.
sub _ppi_with_fallback {
  my ($self, $src) = @_;
  my @lines = split /\n/, $src;
  my ($lo, $hi) = (0, $#lines);
  while ($lo < $hi) {
    my $mid = int(($lo + $hi) / 2);
    my $partial = join("\n", @lines[0..$mid]);
    if (PPI::Document->new(\$partial)) { $lo = $mid + 1; }
    else                               { $hi = $mid;     }
  }
  warn "PCL [lenient-ppi]: truncating at line $lo due to PPI parse failure\n";
  my $partial = join("\n", @lines[0..($lo-1)]);
  return PPI::Document->new(\$partial);
}


# Main entry point: parse and generate CL
sub parse {
  my $self = shift;

  my $doc = $self->ppi_doc;

  # Reset package stack so second pass (parse_file shares environment between
  # passes) always starts in 'main'. Without this, the first pass's
  # push_package calls accumulate and the second pass sees a stale stack.
  $self->environment->package_stack(['main']);

  # Initialize bucket system
  $self->_sections([]);
  $self->_cur_bucket('runtime');
  $self->_open_section('pcl');

  # Initial (in-package :pcl) goes to preamble
  $self->_with_bucket('preamble', sub {
    $self->_emit("(in-package :pcl)");
    $self->_emit("");
  });

  $self->_process_children($doc);

  # Insert forward declarations for undeclared package variables
  $self->_insert_variable_forward_declarations();

  my @assembled = $self->_assemble_output();
  $self->output(\@assembled);
  return join("\n", @assembled);
}

# Transform Perl qualified sub name to CL format
# Perl: A::DESTROY -> CL: A::pl-DESTROY
# Perl: Hash::Util::func -> CL: |Hash::Util|::pl-func
# Perl: Class::DESTROY -> CL: |Class|::pl-DESTROY (avoid CL conflict)
# Perl: simple_sub -> CL: pl-simple_sub
sub _qualified_sub_to_cl {
  my ($self, $name) = @_;
  # Perl allows ' as package separator (old style): BASEOBJ'doit == BASEOBJ::doit
  # Convert to :: before processing
  $name =~ s/'/::/g;
  if ($name =~ /^(.+)::([^:]+)$/) {
    my ($pkg, $bare) = ($1, $2);
    # Pipe-quote if contains :: or conflicts with CL symbols
    my $cl_pkg = ($pkg =~ /::/ || lc($pkg) eq 'class' || lc($pkg) eq 'error' ||
                  lc($pkg) eq 'method' || lc($pkg) eq 'function')
                 ? "|$pkg|" : $pkg;
    return "${cl_pkg}::pl-$bare";
  }
  return "pl-$name";
}

# ============================================================
# Output bucket system helpers
# ============================================================

# Open a new output section for a package (called from _emit_package_preamble).
# Each section holds four named buckets assembled in order:
#   preamble → declarations → definitions → runtime
sub _open_section {
  my ($self, $pkg_name) = @_;
  push @{$self->_sections}, {
    pkg          => $pkg_name,
    preamble     => [],
    declarations => [],
    definitions  => [],
    runtime      => [],
  };
  $self->_cur_section($#{$self->_sections});
}

# Temporarily switch to a named bucket, run $code, then restore.
sub _with_bucket {
  my ($self, $bucket, $code) = @_;
  my $old = $self->_cur_bucket;
  $self->_cur_bucket($bucket);
  $code->();
  $self->_cur_bucket($old);
}

# Assemble all sections into a flat ordered list of lines.
# Also prepends missing package declarations to the first section's preamble.
sub _assemble_output {
  my $self = shift;

  # Collect packages declared in section 0's preamble only.
  # Packages declared only in later sections still need a predeclaration at
  # the top because section 0 code may reference them before they're defined.
  my %declared_pkgs;
  for my $line (@{$self->_sections->[0]{preamble}}) {
    if ($line =~ /^\s*\(defpackage\s+:(\S+)/) {
      my $pkg = $1;
      $pkg =~ s/^\|//; $pkg =~ s/\|$//;
      $declared_pkgs{$pkg} = 1;
    }
  }

  # Collect packages referenced in qualified names (Pkg::symbol)
  my %needed_packages;
  for my $section (@{$self->_sections}) {
    for my $bucket (qw(preamble declarations definitions runtime)) {
      for my $line (@{$section->{$bucket}}) {
        while ($line =~ /\b([A-Za-z][A-Za-z0-9_]*)::/g) {
          my $pkg = $1;
          next if lc($pkg) eq 'pcl';  # our runtime package
          $needed_packages{$pkg} = 1 unless $declared_pkgs{$pkg};
        }
        while ($line =~ /\|([^|]+)\|::/g) {
          my $pkg = $1;
          $needed_packages{$pkg} = 1 unless $declared_pkgs{$pkg};
        }
      }
    }
  }

  # Also add packages from environment's undeclared list
  my $env_pkgs = $self->environment->get_undeclared_packages();
  for my $pkg (@$env_pkgs) {
    $needed_packages{$pkg} = 1 unless $declared_pkgs{$pkg};
  }

  # Prepend missing package declarations to first section's preamble
  if (%needed_packages) {
    my @predecls;
    for my $pkg (sort keys %needed_packages) {
      my $cl_pkg = ($pkg =~ /::/ || lc($pkg) eq 'class' || lc($pkg) eq 'error' ||
                    lc($pkg) eq 'method' || lc($pkg) eq 'function')
                   ? ":|$pkg|" : ":$pkg";
      push @predecls, ";; Pre-declare package for dynamic loading";
      push @predecls, "(defpackage $cl_pkg (:use :cl :pcl))";
      push @predecls, "";
    }
    unshift @{$self->_sections->[0]{preamble}}, @predecls;
  }

  # Assemble: for each section emit preamble → declarations → definitions → runtime
  my @lines;
  for my $section (@{$self->_sections}) {
    push @lines, @{$section->{preamble}};
    push @lines, @{$section->{declarations}};
    push @lines, @{$section->{definitions}};
    push @lines, @{$section->{runtime}};
  }
  return @lines;
}


# DELETED: _insert_sub_forward_declarations (replaced by bucket routing)
# DELETED: _reorder_compile_runtime_forms   (replaced by bucket ordering)
# DELETED: _parse_output_chunks             (no longer needed)
# DELETED: _is_compile_time_form            (no longer needed)
# DELETED: _insert_package_predeclarations  (folded into _assemble_output)


# Insert defvar for package variables used without my/our declaration.
# Scans all output buckets; pushes defvars into first section's declarations.
sub _insert_variable_forward_declarations {
  my $self = shift;

  return if $self->collect_prototypes_only;

  # Variables defined in the pcl runtime (inherited via :use :pcl)
  my %runtime_vars = map { $_ => 1 } qw(
    $_ @_ %_args @ARGV @INC %ENV %INC %SIG $@
    $1 $2 $3 $4 $5 $6 $7 $8 $9
    $0 $$ $?
  );

  my %declared;    # variables with defvar already in preamble/declarations
  my %let_bound;   # variables bound by let/let*/foreach at FILE scope only
  my %referenced;  # all variable references at FILE scope only

  # Only scan section 0's preamble+declarations for "already declared" defvars.
  # Forward declarations are inserted into section 0. A defvar in a later section
  # (e.g. section 7) doesn't help section 0's runtime code which runs first.
  # defvars inside runtime code (e.g. from 'my @a' in bare blocks) must also NOT
  # suppress a forward declaration for the same reason.
  {
    my $s0 = $self->_sections->[0];
    for my $line (@{$s0->{preamble}}, @{$s0->{declarations}}) {
      if ($line =~ /\(defvar\s+([\$\@\%][a-zA-Z_]\w*)\b/) {
        $declared{$1} = 1;
      }
    }
  }

  # Collect all lines from all sections' all buckets
  my @all_lines;
  for my $section (@{$self->_sections}) {
    push @all_lines, @{$section->{preamble}};
    push @all_lines, @{$section->{declarations}};
    push @all_lines, @{$section->{definitions}};
    push @all_lines, @{$section->{runtime}};
  }

  # Track nesting inside sub definitions.
  # We only care about let_bound/referenced at file scope (sub_depth == 0),
  # because 'my $a' inside a sub should NOT prevent defvar for file-scope $a.
  my $sub_depth = 0;

  for my $line (@all_lines) {
    # Skip comment lines
    next if $line =~ /^\s*;;/;

    # Track entry/exit of sub definitions
    if ($line =~ /^\(p-sub\s|^\(defun\s/) {
      $sub_depth++;
    }

    if ($sub_depth == 0) {
      # Collect let/let*-bound variables.
      if ($line =~ /\(let\*?\s+\(/) {
        while ($line =~ /\(([\$\@\%][a-zA-Z_]\w*)\s+/g) {
          $let_bound{$1} = 1;
        }
      }
      # Collect foreach-bound variables
      if ($line =~ /\(p-foreach\s+\(([\$\@\%][a-zA-Z_]\w*)\b/) {
        $let_bound{$1} = 1;
      }
      # Collect all variable references
      while ($line =~ /([\$\@\%][a-zA-Z_]\w*)/g) {
        my $var = $1;
        next if $var =~ /::/;  # skip package-qualified
        $referenced{$var} = 1;
      }
    }

    # Track closing of sub definitions by counting parens
    if ($sub_depth > 0 && $line =~ /^\)/) {
      $sub_depth--;
    }
  }

  # Undeclared = referenced - declared - runtime - __lex__ let-bound
  # Note: do NOT exclude all let_bound vars. A variable may be let-bound inside a
  # bare block (e.g. 'my @a' -> (let ((@a ...))...)) but still referenced as a
  # package variable at an earlier point in load order. defvar is idempotent,
  # so emitting a forward declaration for regular let-bound vars is safe.
  # EXCEPTION: __lex__ variables (closure-renamed) must stay lexical (no defvar).
  # They are let-bound per-iteration and must create CL lexical (not dynamic)
  # bindings so each closure iteration captures its own independent binding.
  # Always declare $a and $b (Perl sort variables) — they are package-scoped globals
  # used by named sort comparator subs (sub cmp { $a <=> $b }) as dynamic variables.
  # defvar makes them CL special vars; lambda params (lambda ($a $b) ...) then create
  # dynamic bindings that named subs see.  They appear inside sub bodies (sub_depth>0)
  # so the file-scope scan above misses them; emit unconditionally before @undeclared.
  my $decls = $self->_sections->[0]{declarations};
  unless ($declared{'$a'}) {
    push @$decls, "(defvar \$a (make-p-box nil))";
    push @$decls, "(defvar \$b (make-p-box nil))";
    push @$decls, "";
    $declared{'$a'} = 1;
    $declared{'$b'} = 1;
  }

  my @undeclared;
  for my $var (sort keys %referenced) {
    next if $declared{$var};
    next if $runtime_vars{$var};
    next if $let_bound{$var} && $var =~ /__lex__/;
    next if $var =~ /^[\$\@\%]state__/;  # state vars use outer let binding, not defvar
    push @undeclared, $var;
  }

  return unless @undeclared;

  # Push undeclared defvars into first section's declarations bucket.
  # These will be assembled before any definitions or runtime code.
  push @$decls, ";; Forward declarations for package variables used without my/our.";
  push @$decls, ";; Perl globals auto-vivify as undef; CL needs defvar to avoid crashes.";
  for my $var (@undeclared) {
    my $sigil = substr($var, 0, 1);
    if ($sigil eq '$') {
      push @$decls, "(defvar $var (make-p-box nil))";
    } elsif ($sigil eq '@') {
      push @$decls, "(defvar $var (make-array 0 :adjustable t :fill-pointer 0))";
    } elsif ($sigil eq '%') {
      push @$decls, "(defvar $var (make-hash-table :test 'equal))";
    }
  }
  push @$decls, "";
}


# Transform package-qualified variable names for CL
# $Pkg::Var -> Pkg::$Var, $Pkg::Sub::Var -> |Pkg::Sub|::$Var
# Also: $::var -> main::$var (empty package = main)
sub _transform_pkg_var {
  my ($self, $var) = @_;
  # Handle package-qualified variables: $Pkg::var -> Pkg::$var
  # Note: Use (.*) not (.+) to allow empty package (main shorthand)
  if ($var =~ /^([\$\@\%])(.*)::([^:]+)$/) {
    my ($sigil, $pkg, $name) = ($1, $2, $3);
    # Empty package means main (e.g., $::foo = $main::foo)
    $pkg = 'main' if $pkg eq '';
    my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
    return "${cl_pkg}::${sigil}${name}";
  }
  return $var;
}

# Extract individual key/index token groups from a PPI::Structure::Subscript,
# splitting on top-level comma operators.
# Returns a list of arrayrefs, one per key/index expression.
sub _subscript_key_groups {
  my ($self, $sub) = @_;

  # Flatten subscript children: unwrap Statement::Expression wrappers
  my @tokens;
  for my $child ($sub->children()) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) =~ /^PPI::Statement/) {
      for my $gc ($child->children()) {
        next if ref($gc) eq 'PPI::Token::Whitespace';
        push @tokens, $gc;
      }
    } else {
      push @tokens, $child;
    }
  }

  # Split on top-level comma (simple split; does not handle nested commas)
  my @groups;
  my @current;
  for my $tok (@tokens) {
    if (ref($tok) eq 'PPI::Token::Operator' && $tok->content eq ',') {
      push @groups, [@current] if @current;
      @current = ();
    } else {
      push @current, $tok;
    }
  }
  push @groups, [@current] if @current;
  return @groups;
}

# Return a list of individual CL key expression strings for a subscript.
# Unlike _subscript_key_groups + _subscript_key_expr, this expands qw// into
# individual strings — needed for delete local @h{qw/a b/} where each key
# must get its own p-local-hash-elem scope.
sub _subscript_key_cl_list {
  my ($self, $sub, $open, $stmt) = @_;

  # Collect non-whitespace children
  my @tokens;
  for my $child ($sub->children()) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) =~ /^PPI::Statement/) {
      for my $gc ($child->children()) {
        next if ref($gc) eq 'PPI::Token::Whitespace';
        push @tokens, $gc;
      }
    } else {
      push @tokens, $child;
    }
  }

  # Special case: single qw// token — expand into individual quoted strings
  if (@tokens == 1 && ref($tokens[0]) =~ /QuoteLike::Words/) {
    my $raw = $tokens[0]->content;      # e.g. "qw/b d/" or "qw(b d)"
    $raw =~ s/^qw\s*\S//;              # strip "qw" + opening delimiter
    $raw =~ s/\S$//;                   # strip closing delimiter
    my @words = grep { $_ ne '' } split /\s+/, $raw;
    return map { "\"$_\"" } @words;
  }

  # General case: split on commas and evaluate each group
  my @groups = $self->_subscript_key_groups($sub);
  return map { $self->_subscript_key_expr($_, $open, $stmt) } @groups;
}

# Parse one key group to a CL expression string.
# For hash subscripts ({...}), auto-quote single bareword tokens (Perl hash key rule).
sub _subscript_key_expr {
  my ($self, $group, $open, $stmt) = @_;
  if ($open eq '{' && @$group == 1 && ref($group->[0]) eq 'PPI::Token::Word') {
    my $word = $group->[0]->content;
    # Only auto-quote if it's not a keyword
    unless ($word =~ /^(?:if|unless|while|until|for|foreach|sub|my|our|local|state|undef|defined|not|and|or)$/) {
      return "\"$word\"";
    }
  }
  return $self->_parse_expression($group, $stmt) // 'nil';
}

# Process children of a PPI node (Document or Block)
sub _process_children {
  my $self     = shift;
  my $parent   = shift;

  my @children = $parent->children;
  my %skip;

  for my $i (0 .. $#children) {
    next if $skip{$i};
    my $child = $children[$i];

    # Lookahead: bare block compound statement followed by continue { }
    if (ref($child) eq 'PPI::Statement::Compound') {
      my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
      if ($continue) {
        $self->_process_compound_statement($child, $continue);
        $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
        next;
      }
    }

    $self->_process_element($child);
  }
}

# Look ahead for a continue { } statement after a bare block compound statement.
# PPI splits "{ ... } continue { ... }" into two sibling statements for bare blocks.
# PPI may also include trailing statements in the continue PPI::Statement.
# Returns ($continue_block, \@trailing_children) if found, () otherwise.
sub _find_continue_sibling {
  my ($self, $children, $i, $skip) = @_;

  my $child = $children->[$i];

  # Check if this is a bare block (first significant child is a Block, not a keyword)
  my $is_bare_block = 0;
  for my $cc ($child->children) {
    my $ref = ref($cc);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Label';
    if ($ref eq 'PPI::Structure::Block') {
      $is_bare_block = 1;
    }
    last;
  }
  return () unless $is_bare_block;

  # Look ahead for continue { } as next non-whitespace sibling
  for my $j ($i+1 .. $#$children) {
    my $sibling = $children->[$j];
    next if ref($sibling) eq 'PPI::Token::Whitespace';
    if (ref($sibling) eq 'PPI::Statement') {
      my @sib_kids = $sibling->children;
      my $k = 0;
      $k++ while $k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Token::Whitespace';
      if ($k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Token::Word'
          && $sib_kids[$k]->content eq 'continue') {
        my $cont_idx = $k;
        $k++;
        $k++ while $k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Token::Whitespace';
        if ($k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Structure::Block') {
          my $continue_block = $sib_kids[$k];
          # Collect trailing children after the continue block (PPI quirk:
          # PPI may include subsequent statements in the same PPI::Statement)
          my @trailing;
          for my $t ($k+1 .. $#sib_kids) {
            push @trailing, $sib_kids[$t];
          }
          $skip->{$j} = 1;
          return ($continue_block, \@trailing);
        }
      }
    }
    last;  # Only check immediate next non-whitespace sibling
  }
  return ();
}

# Process trailing PPI tokens that were orphaned when a continue { } statement
# was consumed by the bare block lookahead. PPI may include subsequent code
# (e.g., "$ok = 1;") in the same PPI::Statement as the continue block.
sub _process_trailing_tokens {
  my ($self, $trailing) = @_;

  # Filter out whitespace-only trailing content
  my @significant = grep { ref($_) ne 'PPI::Token::Whitespace' } @$trailing;
  return unless @significant;

  # Create a synthetic PPI::Statement containing the trailing tokens
  # and process it as an expression statement
  my $synth = PPI::Statement->new();
  for my $token (@$trailing) {
    $synth->add_element($token->clone());
  }
  $self->_process_expression_statement($synth);
}

# Process a single PPI element
sub _process_element {
  my $self    = shift;
  my $element = shift;

  my $ref = ref($element);

  # Skip whitespace and POD
  return if $ref eq 'PPI::Token::Whitespace';
  return if $ref eq 'PPI::Token::Pod';

  # Emit Perl comments as Lisp comments
  if ($ref eq 'PPI::Token::Comment') {
    my $comment = $element->content;
    chomp $comment;
    $self->_emit(";; $comment");
    return;
  }

  # Handle different statement types
  if ($ref eq 'PPI::Statement') {
    # Simple expression statement
    $self->_process_expression_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Expression') {
    $self->_process_expression_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Variable') {
    # Variable declaration: my $x = 10;
    $self->_process_variable_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Compound') {
    # Control flow: if, while, for, etc.
    $self->_process_compound_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Sub') {
    # Subroutine declaration
    $self->_process_sub_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Package') {
    # Package declaration
    $self->_process_package_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Include') {
    # use/require
    $self->_process_include_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Scheduled') {
    # BEGIN, END, CHECK, INIT blocks
    $self->_process_scheduled_block($element);
  }
  elsif ($ref eq 'PPI::Statement::End' || $ref eq 'PPI::Statement::Data') {
    # __END__ / __DATA__ — register DATA filehandle with embedded text
    my ($data_tok) = grep { ref($_) eq 'PPI::Token::Data'
                            || ref($_) eq 'PPI::Token::End' }
                           $element->children;
    my $data = $data_tok ? $data_tok->content : '';
    $data =~ s/\\/\\\\/g;
    $data =~ s/"/\\"/g;
    $self->_with_bucket('preamble', sub {
      $self->_emit(";; $ref — register DATA filehandle");
      $self->_emit("(setf (gethash 'DATA *p-filehandles*)");
      $self->_emit("  (make-string-input-stream \"$data\"))");
    });
    return;
  }
  elsif ($ref =~ /^PPI::Statement/) {
    # Other statement types - treat as expression for now
    $self->_process_expression_statement($element);
  }
  else {
    # Unknown - emit comment
    $self->_emit(";; UNKNOWN: $ref");
  }
}


# Process a simple expression statement
sub _process_expression_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;  # Remove trailing semicolon
  $perl_code =~ s/\n/ /g;   # Collapse newlines

  # Get the expression parts (skip semicolon, whitespace, and comments)
  my @parts = grep {
    my $ref = ref($_);
    $ref ne 'PPI::Token::Whitespace'
      && $ref ne 'PPI::Token::Comment'
      && !($ref eq 'PPI::Token::Structure' && $_->content eq ';')
  } $stmt->children;

  return unless @parts;

  # Special case: "import PACKAGE" is syntactic sugar for "PACKAGE->import()"
  # PPI parses this as two barewords, so we detect and convert it
  # Use funcall+intern to avoid read-time package dependency
  if (@parts == 2
      && ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content eq 'import'
      && ref($parts[1]) eq 'PPI::Token::Word') {
    my $pkg = $parts[1]->content;
    $self->_emit(";; $perl_code");
    $self->_emit("(funcall (intern \"PL-IMPORT\" :$pkg))");
    $self->_emit("");
    return;
  }

  # Handle: delete local SYMBOL SUBSCRIPT (standalone delete+local statement)
  # PPI::Statement: Word("delete"), Word("local"), Symbol, Subscript
  # Opens a p-local-X-elem scope that wraps the rest of the block (closed at block end).
  if (@parts >= 4
      && ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content eq 'delete'
      && ref($parts[1]) eq 'PPI::Token::Word' && $parts[1]->content eq 'local'
      && ref($parts[2]) eq 'PPI::Token::Symbol'
      && ref($parts[3]) eq 'PPI::Structure::Subscript') {
    my $sym       = $parts[2]->content;
    my $sub       = $parts[3];
    my $open      = $sub->start->content;
    my $base      = substr($sym, 1);
    my $new_sigil = ($open eq '{') ? '%' : '@';
    my $cl_var    = $self->_transform_pkg_var("${new_sigil}${base}");
    my @key_cls = $self->_subscript_key_cl_list($sub, $open, $stmt);
    if (@key_cls) {
      my $macro   = ($open eq '{') ? 'p-local-hash-elem'  : 'p-local-array-elem';
      my $del_fn  = ($open eq '{') ? 'p-delete'            : 'p-delete-array';
      $self->_emit(";; $perl_code");
      for my $key_cl (@key_cls) {
        $self->_emit("($macro $cl_var $key_cl");
        $self->_emit("  ($del_fn $cl_var $key_cl)");
        $self->indent_level($self->indent_level + 1);
        $self->{_local_let_depth} //= 0;
        $self->{_local_let_depth}++;
      }
      $self->_emit("");
      return;
    }
  }

  # Check for statement modifiers: EXPR if/unless/while/until/for COND
  my $modifier_idx = -1;
  my $modifier;
  for my $i (0 .. $#parts) {
    if (ref($parts[$i]) eq 'PPI::Token::Word') {
      my $word = $parts[$i]->content;
      if ($word =~ /^(if|unless|while|until|for|foreach)$/) {
        $modifier_idx = $i;
        $modifier = $word;
        last;
      }
    }
  }

  my $cl_code;
  if ($modifier_idx > 0) {
    # Split into expression and condition
    my @expr_parts = @parts[0 .. $modifier_idx - 1];
    my @cond_parts = @parts[$modifier_idx + 1 .. $#parts];

    # Unwrap PPI::Structure::Condition to get the inner expression children.
    # PPI wraps postfix-if conditions in Condition nodes: `if ($x > 1)` → Condition(...)
    if (@cond_parts == 1 && ref($cond_parts[0]) eq 'PPI::Structure::Condition') {
      @cond_parts = grep {
        ref($_) ne 'PPI::Token::Whitespace'
      } $cond_parts[0]->children;
    }

    my $expr_cl = $self->_parse_expression(\@expr_parts, $stmt);
    my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt);

    # Generate appropriate control structure
    # Note: 'for' and 'foreach' modifiers use p-foreach (iterate over list),
    # not p-for (C-style for loop)
    my $cl_modifier = $modifier;
    if ($modifier eq 'for' || $modifier eq 'foreach') {
      $cl_modifier = 'foreach';
      # For foreach modifier, need ($_ list) syntax
      $cl_code = "(p-foreach (\$_ $cond_cl) $expr_cl)";
    }
    else {
      $cl_code = "(p-$cl_modifier $cond_cl $expr_cl)";
    }
  }
  else {
    # No modifier - parse normally
    $cl_code = $self->_parse_expression(\@parts, $stmt);
  }

  # Emit as comment + code
  $self->_emit(";; $perl_code");
  $self->_emit($cl_code) if defined $cl_code;
  $self->_emit("");
}


# Process variable declaration: my $x = 10;
sub _process_variable_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;
  $perl_code =~ s/\n/ /g;

  # Get expression parts (skip semicolon and whitespace)
  my @parts = grep {
    my $ref = ref($_);
    $ref ne 'PPI::Token::Whitespace'
      && !($ref eq 'PPI::Token::Structure' && $_->content eq ';')
  } $stmt->children;

  return unless @parts;

  # Check declarator type
  my $declarator = '';
  if (ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content =~ /^(my|our|state|local)$/) {
    $declarator = $1;
  }

  # Handle 'our' declarations - package variables
  if ($declarator eq 'our') {
    $self->_process_our_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Handle 'local' declarations - dynamic scoping
  if ($declarator eq 'local') {
    $self->_process_local_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Handle top-level 'my' declarations - need p-my for BEGIN block visibility
  # Inside subs, my uses regular let bindings (handled elsewhere)
  # Exception: if the var was renamed by _with_declarations (closure capture at pkg level),
  # skip _process_my_toplevel_declaration and fall through to the rename handling below.
  if ($declarator eq 'my' && $self->environment->in_subroutine == 0) {
    my $scope_renames = $self->{_current_scope_new_renames} // {};
    my $var_for_check;
    for my $p (@parts) {
      my $ref = ref($p);
      last if $ref eq 'PPI::Structure::List';
      if ($ref eq 'PPI::Token::Symbol') { $var_for_check = $p->content; last; }
    }
    unless (defined $var_for_check && exists $scope_renames->{$var_for_check}) {
      $self->_process_my_toplevel_declaration($stmt, \@parts, $perl_code);
      return;
    }
    # Fall through: this var was renamed for closure capture — handle via rename path
  }

  # Check if this is a state declaration inside a sub
  my $is_state = ($declarator eq 'state');
  my $state_vars = $self->{_current_state_vars} // {};

  # Package-level state is the same as package-level my (runs once at load time)
  if ($is_state && $self->environment->in_subroutine == 0) {
    $self->_process_my_toplevel_declaration($stmt, \@parts, $perl_code);
    return;
  }

  if ($is_state && %$state_vars) {
    # State declaration inside a sub - generate init guard
    $self->_process_state_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Check for bare declaration without assignment (my $x; or my @arr;)
  # These have: declarator, variable, no operator
  my $has_operator = grep { ref($_) eq 'PPI::Token::Operator' } @parts;
  if (!$has_operator) {
    # Bare declaration - just emit as comment, runtime will auto-declare
    # For state, we also need init guard for bare declarations
    if ($is_state && %$state_vars) {
      $self->_process_state_declaration($stmt, \@parts, $perl_code);
      return;
    }
    $self->_emit(";; $perl_code (bare declaration)");
    $self->_emit("");
    return;
  }

  # Special case: scalar 'my $var = EXPR' inside a sub where $var was renamed by
  # _with_declarations (captured by a closure). Parse only the RHS with the rename
  # for $var temporarily absent so that '$var' in the RHS refers to the outer scope.
  # This handles 'my $i = $i + 1' shadowing correctly (outer $i → 5, not the new lex box).
  if ($declarator eq 'my') {
    my $scope_renames = $self->{_current_scope_new_renames} // {};

    # Find the declared scalar variable (skip if list declaration)
    my $var_name;
    for my $p (@parts) {
      my $ref = ref($p);
      last if $ref eq 'PPI::Structure::List';  # list decl — handled below
      if ($ref eq 'PPI::Token::Symbol') { $var_name = $p->content; last; }
    }

    if (defined $var_name && exists $scope_renames->{$var_name}) {
      # Find '=' and split into RHS tokens
      my $eq_idx = -1;
      for my $i (0 .. $#parts) {
        if (ref($parts[$i]) eq 'PPI::Token::Operator' && $parts[$i]->content eq '=') {
          $eq_idx = $i; last;
        }
      }
      if ($eq_idx >= 0) {
        my @rhs_parts = @parts[$eq_idx + 1 .. $#parts];
        my $new_name  = $scope_renames->{$var_name};

        # Temporarily remove new rename so RHS sees the outer/old binding for $var_name
        my $old_rn      = $self->{_current_scope_old_renames} // {};
        my $env_renames = $self->environment->state_var_renames // {};
        my %temp = %$env_renames;
        if (defined $old_rn->{$var_name}) {
          $temp{$var_name} = $old_rn->{$var_name};
        } else {
          delete $temp{$var_name};
        }
        $self->environment->state_var_renames(\%temp);

        my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt);

        # Re-apply new rename
        $self->environment->state_var_renames($env_renames);

        $self->_emit(";; $perl_code");
        $self->_emit("(p-my-= $new_name $rhs_cl)") if defined $rhs_cl && $rhs_cl ne '';
        $self->_emit("");
        return;
      }
    }
  }

  # Parse with PExpr (handles declarator extraction)
  my $cl_code = $self->_parse_expression(\@parts, $stmt);

  $self->_emit(";; $perl_code");
  $self->_emit($cl_code) if defined $cl_code;
  $self->_emit("");
}

# Process 'our' variable declaration - package-level variable
sub _process_our_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  my $pkg = $self->environment->current_package;

  # Find variable(s) and optional initializer
  my @vars;
  my $init_idx = -1;

  for my $i (0 .. $#$parts) {
    my $p = $parts->[$i];
    my $ref = ref($p);

    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
      push @vars, $p->content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # List declaration: our ($x, $y)
      push @vars, $self->_find_symbols_in_list($p);
    }
    elsif ($ref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i;
      last;
    }
  }

  return unless @vars;

  # Register in environment
  for my $var (@vars) {
    $self->environment->add_our_variable($pkg, $var);
  }

  # Special handling for @ISA - inheritance declaration
  if (@vars == 1 && $vars[0] eq '@ISA' && $init_idx >= 0) {
    $self->_process_isa_declaration($stmt, $parts, $init_idx, $perl_code);
    return;
  }

  # Compile-time declarations (defvar) go to declarations bucket.
  # Separate declaration from initialization (runtime) to match Perl:
  # 'our $x = 1; BEGIN { $x = 2 }' → at runtime $x becomes 1 (init overwrites BEGIN)
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
  });

  if ($init_idx >= 0) {
    # Has initializer - parse the RHS
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

    if (@vars == 1) {
      # Single variable: our $x = value, our @arr = (), our %hash = ()
      my $var = $vars[0];
      my $sigil = substr($var, 0, 1);

      # Check for empty list initializer ()
      my $is_empty_list = (@rhs_parts == 1 &&
                           ref($rhs_parts[0]) eq 'PPI::Structure::List' &&
                           $self->_is_empty_structure($rhs_parts[0]));

      if ($sigil eq '@') {
        # Array: declare at compile time, initialize at runtime
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-direct");
          $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
        });
        unless ($is_empty_list) {
          # Parse full statement so PExpr sees '@arr = ...' and propagates
          # LIST context to the RHS (e.g. split() gets LIST_CTX, not SCALAR_CTX)
          my $cl_code = $self->_parse_expression($parts, $stmt);
          $self->_emit($cl_code) if defined $cl_code;
        }
      }
      elsif ($sigil eq '%') {
        # Hash: declare at compile time, initialize at runtime
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-direct");
          $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
        });
        unless ($is_empty_list) {
          # Parse full statement so PExpr sees '%hash = ...' and propagates
          # LIST context to the RHS
          my $cl_code = $self->_parse_expression($parts, $stmt);
          $self->_emit($cl_code) if defined $cl_code;
        }
      }
      else {
        # Scalar: declare with nil box at compile time, set value at runtime
        my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-direct");
          $self->_emit("  (defvar $var (make-p-box nil)))");
        });
        $self->_emit("(setf (p-box-value $var) $init_cl)");
      }
    }
    else {
      # Multiple variables: our ($x, $y) = (1, 2)
      # First declare all at compile time, then assign at runtime
      $self->_with_bucket('declarations', sub {
        for my $var (@vars) {
          my $sigil = substr($var, 0, 1);
          $self->_emit("(p-eval-direct");
          if ($sigil eq '@') {
            $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
          } elsif ($sigil eq '%') {
            $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
          } else {
            $self->_emit("  (defvar $var (make-p-box nil)))");
          }
        }
      });
      # Now do the assignment at runtime
      my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
      my $vars_vector = "(vector " . join(" ", @vars) . ")";
      $self->_emit("(p-list-= $vars_vector $init_cl)");
    }
  }
  else {
    # Bare declaration: our $x; or our @arr; or our %hash;
    $self->_with_bucket('declarations', sub {
      for my $var (@vars) {
        my $sigil = substr($var, 0, 1);
        $self->_emit("(p-eval-direct");
        if ($sigil eq '@') {
          $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
        } elsif ($sigil eq '%') {
          $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
        } else {
          $self->_emit("  (defvar $var (make-p-box nil)))");
        }
      }
    });
  }

  $self->_emit("");
}

# Process top-level 'my' declaration - lexical at file scope
# Uses eval-when for BEGIN block visibility: declaration at compile time,
# initialization at runtime. Inside subs, 'my' uses regular let bindings.
sub _process_my_toplevel_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  # Find variable(s) and optional initializer
  my @vars;
  my $init_idx = -1;

  for my $i (0 .. $#$parts) {
    my $p = $parts->[$i];
    my $ref = ref($p);

    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
      push @vars, $p->content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # List declaration: my ($x, $y)
      push @vars, $self->_find_symbols_in_list($p);
    }
    elsif ($ref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i;
      last;
    }
  }

  return unless @vars;

  # Compile-time declarations (defvar) go to the declarations bucket
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $sigil = substr($var, 0, 1);
      $self->_emit("(p-eval-direct");
      if ($sigil eq '@') {
        $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
      } elsif ($sigil eq '%') {
        $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
      } else {
        $self->_emit("  (defvar $var (make-p-box nil)))");
      }
    }
  });

  # Handle initialization at runtime (stays in current bucket)
  if ($init_idx >= 0) {
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

    # Check for empty list initializer ()
    my $is_empty_list = (@rhs_parts == 1 &&
                         ref($rhs_parts[0]) eq 'PPI::Structure::List' &&
                         $self->_is_empty_structure($rhs_parts[0]));

    unless ($is_empty_list) {
      # Check for: my VARS = delete local SYMBOL SUBSCRIPT
      # This must be handled before normal expression parsing because:
      #  - The local save/restore must scope to the enclosing block
      #  - The delete is done inside the local scope, and result assigned to VARS
      my @clean_rhs = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;
      if (@clean_rhs >= 4
          && ref($clean_rhs[0]) eq 'PPI::Token::Word'  && $clean_rhs[0]->content eq 'delete'
          && ref($clean_rhs[1]) eq 'PPI::Token::Word'  && $clean_rhs[1]->content eq 'local'
          && ref($clean_rhs[2]) eq 'PPI::Token::Symbol'
          && ref($clean_rhs[3]) eq 'PPI::Structure::Subscript') {
        my $sym  = $clean_rhs[2]->content;
        my $sub  = $clean_rhs[3];
        my $open = $sub->start->content;
        my $base = substr($sym, 1);
        my $new_sigil = ($open eq '{') ? '%' : '@';
        my $cl_var    = $self->_transform_pkg_var("${new_sigil}${base}");
        my @key_cls = $self->_subscript_key_cl_list($sub, $open, $stmt);
        if (@key_cls) {
          my $macro   = ($open eq '{') ? 'p-local-hash-elem'  : 'p-local-array-elem';
          my $del_fn  = ($open eq '{') ? 'p-delete'            : 'p-delete-array';
          $self->_emit(";; $perl_code");
          # Pre-evaluate original values BEFORE opening local scopes,
          # so the result of "delete local" is the ORIGINAL value (not the fresh nil box).
          my $get_fn = ($open eq '{') ? 'p-gethash' : 'p-aref';
          $self->{_local_counter} //= 0;
          my @del_tmp_vars;
          for my $key_cl (@key_cls) {
            my $tmp = "pcl-del-" . $self->{_local_counter}++;
            push @del_tmp_vars, $tmp;
            $self->_emit("(let (($tmp ($get_fn $cl_var $key_cl)))");
            $self->indent_level($self->indent_level + 1);
            $self->{_local_let_depth} //= 0;
            $self->{_local_let_depth}++;
          }
          # Open local save/restore scope for each key (nested, closed at block end)
          for my $key_cl (@key_cls) {
            $self->_emit("($macro $cl_var $key_cl");
            $self->_emit("  ($del_fn $cl_var $key_cl)");
            $self->indent_level($self->indent_level + 1);
            $self->{_local_let_depth} //= 0;
            $self->{_local_let_depth}++;
          }
          # Emit the assignment inside the local scope using pre-saved values
          if (@vars == 1 && @key_cls == 1) {
            # my $c = delete local $a[N]  or  my $c = delete local $h{k}
            my $var = $vars[0];
            $self->_emit("(box-set $var $del_tmp_vars[0])");
          } else {
            # my ($x,$y) = delete local @a[N,M]  or  my ($x,$y) = delete local @h{k1,k2}
            my $lhs_cl = "(vector " . join(' ', @vars) . ")";
            my $rhs_cl = "(let ((*wantarray* t)) (vector " . join(' ', @del_tmp_vars) . "))";
            $self->_emit("(p-list-= $lhs_cl $rhs_cl)");
          }
          $self->_emit("");
          return;
        }
      }

      if (@vars == 1) {
        my $var = $vars[0];
        my $sigil = substr($var, 0, 1);

        if ($sigil eq '$') {
          # Scalar: parse RHS and use box-set to properly unbox source
          my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
          $self->_emit("(box-set $var $init_cl)");
        } else {
          # Array/hash: parse full statement through expression parser for proper list context
          # This generates (p-array-= @arr (vector ...)) or (p-hash-= %h (p-hash ...))
          my $cl_code = $self->_parse_expression($parts, $stmt);
          $self->_emit($cl_code) if defined $cl_code;
        }
      } else {
        # Multiple variables: parse full statement through expression parser
        my $cl_code = $self->_parse_expression($parts, $stmt);
        $self->_emit($cl_code) if defined $cl_code;
      }
    }
  }

  $self->_emit("");
}

# Helper to check if a PPI structure is empty (for () detection)
sub _is_empty_structure {
  my ($self, $struct) = @_;
  my @children = $struct->children;
  # Filter out whitespace
  @children = grep { ref($_) ne 'PPI::Token::Whitespace' } @children;
  return @children == 0;
}

# Process @ISA declaration - emit CLOS class with parents for MRO
sub _process_isa_declaration {
  my ($self, $stmt, $parts, $init_idx, $perl_code) = @_;

  my $pkg = $self->environment->current_package;

  # Extract parent class names from RHS
  my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
  @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

  my @parents = $self->_extract_parent_classes(\@rhs_parts);

  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
  });

  if (@parents) {
    # Emit CLOS class with parent classes for MRO tracking
    my $cl_class = $self->_pkg_to_clos_class($pkg);
    # Package-qualify parent class symbols so they resolve correctly regardless
    # of which CL package the defclass is emitted in.
    # Animal -> Animal::animal (reads as ANIMAL::ANIMAL)
    # Foo::Bar -> |Foo::Bar|::foo--bar (pipe-quoting preserves :: in pkg name)
    my $parents_cl = join(' ', map {
      my $cls = $self->_pkg_to_clos_class($_);
      my $pkg_prefix = ($_ =~ /::/) ? "|$_|" : $_;
      "$pkg_prefix\:\:$cls"
    } @parents);

    # Store parent list in environment for later use
    $self->environment->set_isa($pkg, \@parents);

    # Redefine the CLOS class with parents in preamble (package-setup form)
    $self->_with_bucket('preamble', sub {
      $self->_emit(";; Redefine CLOS class with parents for MRO");
      $self->_emit("(defclass $cl_class ($parents_cl) ())");
    });
  }

  # Declare @ISA in declarations bucket, initialize at runtime
  $self->_with_bucket('declarations', sub {
    $self->_emit("(defvar \@ISA (make-array 0 :adjustable t :fill-pointer 0))");
  });
  for my $parent (@parents) {
    $self->_emit("(p-push \@ISA \"$parent\")");
  }

  $self->_emit("");
}

# Extract parent class names from an @ISA initializer expression
# Handles: qw(Parent1 Parent2), ('Parent1', 'Parent2'), ("Parent")
sub _extract_parent_classes {
  my ($self, $parts) = @_;
  my @parents;

  for my $part (@$parts) {
    my $ref = ref($part);

    if ($ref eq 'PPI::Token::QuoteLike::Words') {
      # qw(Parent1 Parent2)
      my $content = $part->content;
      $content =~ s/^qw\s*[\(\[\{<]//;
      $content =~ s/[\)\]\}>]$//;
      push @parents, split(/\s+/, $content);
    }
    elsif ($ref eq 'PPI::Token::Quote::Single'
	   || $ref eq 'PPI::Token::Quote::Double') {
      # 'Parent' or "Parent"
      push @parents, $part->string;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # (list) - recurse into children
      for my $child ($part->schildren) {
        if ($child->isa('PPI::Statement::Expression')) {
          push @parents, $self->_extract_parent_classes([$child->schildren]);
        }
        elsif ($child->isa('PPI::Token::Quote')) {
          push @parents, $child->string;
        }
      }
    }
  }

  return grep { defined $_ && $_ ne '' } @parents;
}

# Process 'local' variable declaration - dynamic scoping
# Emits a (let ...) that stays open until block end
sub _process_local_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  # Handle local *foo and local *foo = RHS (typeglob localization)
  # Use p-local-glob which saves/restores all slots via unwind-protect.
  # @parts includes the 'local' keyword as first element — skip it.
  my @non_ws = grep {
    my $r = ref($_);
    $r ne 'PPI::Token::Whitespace'
    && !($r eq 'PPI::Token::Word' && $_->content eq 'local')
  } @$parts;

  # ── Pre-unwrap: local(*foo) — single symbol in parens. Unwrap before typeglob check.
  if (@non_ws >= 1 && ref($non_ws[0]) eq 'PPI::Structure::List') {
    my @flat;
    for my $child ($non_ws[0]->children) {
      my $cr = ref($child);
      next if $cr eq 'PPI::Token::Whitespace';
      next if $cr eq 'PPI::Token::Structure';
      if ($cr =~ /^PPI::Statement/) {
        for my $gc ($child->children) {
          next if ref($gc) eq 'PPI::Token::Whitespace';
          push @flat, $gc;
        }
      } else {
        push @flat, $child;
      }
    }
    if (@flat == 1 && ref($flat[0]) eq 'PPI::Token::Symbol') {
      # local(*foo) or local($scalar) with no subscript — unwrap the parens
      splice(@non_ws, 0, 1, @flat);
    }
  }

  if (@non_ws && ref($non_ws[0]) eq 'PPI::Token::Symbol'
      && $non_ws[0]->content =~ /^\*(.+)$/) {
    my $glob_content = $non_ws[0]->content;  # e.g. "*foo" or "*Pkg::foo"
    my ($pkg, $name);
    if ($glob_content =~ /^\*(.*)::([^:]+)$/) {
      ($pkg, $name) = ($1 || 'main', $2);
    } else {
      $glob_content =~ /^\*(\w+)$/;
      $name = $1;
      $pkg  = $self->environment ? $self->environment->current_package : 'main';
      $pkg //= 'main';
    }
    $self->_emit(";; $perl_code");
    # Find initializer (after '=')
    my $has_init = grep { ref($_) eq 'PPI::Token::Operator' && $_->content eq '=' } @non_ws;
    if ($has_init) {
      my @rhs_parts;
      my $past_eq = 0;
      for my $p (@non_ws) {
        if (!$past_eq && ref($p) eq 'PPI::Token::Operator' && $p->content eq '=') {
          $past_eq = 1;
          next;
        }
        push @rhs_parts, $p if $past_eq;
      }
      my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
      $self->_emit("(p-local-glob \"$pkg\" \"$name\"");
      $self->indent_level($self->indent_level + 1);
      $self->_emit("(p-glob-assign \"$pkg\" \"$name\" $rhs_cl)");
    } else {
      $self->_emit("(p-local-glob \"$pkg\" \"$name\"");
      $self->indent_level($self->indent_level + 1);
    }
    $self->{_local_let_depth} //= 0;
    $self->{_local_let_depth}++;
    $self->_emit("");
    return;
  }

  # ── Unwrap local(ELEM) parens form: local($a[N]) / local($h{key}) / local(@a[N,M])
  # PPI gives Structure::List when parens are used; unwrap it so the handler below fires.
  if (@non_ws >= 1 && ref($non_ws[0]) eq 'PPI::Structure::List') {
    my @flat;
    for my $child ($non_ws[0]->children) {
      my $cr = ref($child);
      next if $cr eq 'PPI::Token::Whitespace';
      next if $cr eq 'PPI::Token::Structure';   # skip '(' and ')'
      if ($cr =~ /^PPI::Statement/) {
        for my $gc ($child->children) {
          next if ref($gc) eq 'PPI::Token::Whitespace';
          push @flat, $gc;
        }
      } else {
        push @flat, $child;
      }
    }
    if (@flat == 2
        && ref($flat[0]) eq 'PPI::Token::Symbol'
        && ref($flat[1]) eq 'PPI::Structure::Subscript') {
      splice(@non_ws, 0, 1, @flat);
    }
    elsif (@flat == 1
           && ref($flat[0]) eq 'PPI::Token::Symbol') {
      # local(*foo), local($scalar), etc. — single symbol in parens
      splice(@non_ws, 0, 1, @flat);
    }
  }

  # ── Handle local $hash{key}, local @arr[N], local @hash{@keys}, local @arr[N,M]
  # PPI gives: Symbol("$hash") + Structure::Subscript("{key}")
  if (@non_ws >= 2
      && ref($non_ws[0]) eq 'PPI::Token::Symbol'
      && ref($non_ws[1]) eq 'PPI::Structure::Subscript') {

    my $sym   = $non_ws[0]->content;           # e.g. "$hash", "@arr"
    my $sub   = $non_ws[1];
    my $open  = $sub->start()->content();       # '{' or '['
    my $sigil = substr($sym, 0, 1);             # '$' (single) or '@' (slice)
    my $base  = substr($sym, 1);               # "hash" or "arr"

    # The CL variable: hash subscripts access %hash, array subscripts access @arr
    my $new_sigil = ($open eq '{') ? '%' : '@';
    my $cl_var    = $self->_transform_pkg_var("${new_sigil}${base}");

    # Extract individual key/index expressions from the subscript
    my @key_groups = $self->_subscript_key_groups($sub);
    if (@key_groups) {
      # Check for initializer (= expr) after the subscript
      my ($has_init, @rhs_parts);
      for my $i (2 .. $#non_ws) {
        if (ref($non_ws[$i]) eq 'PPI::Token::Operator' && $non_ws[$i]->content eq '=') {
          $has_init = 1;
          @rhs_parts = @non_ws[($i + 1) .. $#non_ws];
          last;
        }
      }
      my $init_cl = $has_init ? ($self->_parse_expression(\@rhs_parts, $stmt) // 'nil') : undef;

      $self->_emit(";; $perl_code");

      # Parse all key CL expressions up front (need them for both macro open and init)
      my @key_cls = map { $self->_subscript_key_expr($_, $open, $stmt) } @key_groups;

      # Choose the macro based on subscript type
      my $macro      = ($open eq '{') ? 'p-local-hash-elem'      : 'p-local-array-elem';
      my $macro_init = ($open eq '{') ? 'p-local-hash-elem-init'  : 'p-local-array-elem-init';

      if (defined $init_cl && @key_cls == 1) {
        # Single element with initializer: use the *-init macro which evaluates
        # init-form BEFORE installing the fresh box, preventing stale-read bugs
        # like local($a[2]) = $a[2] reading the fresh undef box.
        my $key_cl = $key_cls[0];
        $self->_emit("($macro_init $cl_var $key_cl $init_cl");
        $self->indent_level($self->indent_level + 1);
        $self->{_local_let_depth} //= 0;
        $self->{_local_let_depth}++;
      } elsif (defined $init_cl) {
        # Slice with initializer: pre-evaluate RHS before any macros open,
        # then emit nested macro opens, then assign using the saved value.
        $self->{_local_counter} //= 0;
        my $tmp = "pcl-local-init-" . $self->{_local_counter}++;
        my $ctx = "(let ((*wantarray* t)) ";
        my $ctx_close = ")";
        $self->_emit("(let (($tmp ${ctx}$init_cl${ctx_close}))");
        $self->indent_level($self->indent_level + 1);
        $self->{_local_let_depth}++;
        for my $key_cl (@key_cls) {
          $self->_emit("($macro $cl_var $key_cl");
          $self->indent_level($self->indent_level + 1);
          $self->{_local_let_depth}++;
        }
        my $keys_str = join(' ', @key_cls);
        if ($open eq '{') {
          $self->_emit("(let ((*wantarray* t)) (p-setf (p-hslice $cl_var $keys_str) $tmp))");
        } else {
          $self->_emit("(let ((*wantarray* t)) (p-setf (p-aslice $cl_var $keys_str) $tmp))");
        }
      } else {
        # No initializer: emit one macro call per key (nested open forms)
        for my $key_cl (@key_cls) {
          $self->_emit("($macro $cl_var $key_cl");
          $self->indent_level($self->indent_level + 1);
          $self->{_local_let_depth} //= 0;
          $self->{_local_let_depth}++;
        }
      }

      $self->_emit("");
      return;
    }
  }

  # Find variable and optional initializer
  my @vars;
  my $init_idx = -1;

  for my $i (0 .. $#$parts) {
    my $p = $parts->[$i];
    my $ref = ref($p);

    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
      push @vars, $self->_transform_pkg_var($p->content);
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # Use undef-aware extraction so local(undef, $a, undef, $b) keeps skip markers
      push @vars, $self->_find_symbols_and_undefs_in_list($p);
    }
    elsif ($ref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i;
      last;
    }
  }

  return unless @vars;

  $self->_emit(";; $perl_code");

  # Build let bindings
  my @bindings;
  if ($init_idx >= 0 && @vars == 1) {
    # local $x = value
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;
    my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';

    my $var = $vars[0];
    my $sigil = substr($var, 0, 1);

    if ($sigil eq '@') {
      # local @arr = EXPR: evaluate EXPR with old @arr, make an independent copy.
      # CL 'let' evaluates init form with old bindings, so @arr in $init_cl reads old value.
      push @bindings, "($var (p-copy-array (let ((*wantarray* t)) $init_cl)))";
    }
    elsif ($sigil eq '%') {
      # local %h = EXPR: evaluate EXPR with old %h, make an independent copy.
      push @bindings, "($var (p-copy-hash (let ((*wantarray* t)) $init_cl)))";
    }
    else {
      push @bindings, "($var (make-p-box $init_cl))";
    }
  }
  else {
    # Bare local or multiple vars - just shadow with nil/empty.
    # Skip undef markers (they are skip slots, not real variables).
    for my $var (@vars) {
      next if $var eq '(p-undef)';  # undef slot: no binding needed
      my $sigil = substr($var, 0, 1);
      if ($sigil eq '@') {
        push @bindings, "($var (make-array 0 :adjustable t :fill-pointer 0))";
      }
      elsif ($sigil eq '%') {
        push @bindings, "($var (make-hash-table :test 'equal))";
      }
      else {
        push @bindings, "($var (make-p-box nil))";
      }
    }
  }

  my $bindings_str = join("\n        ", @bindings);
  $self->_emit("(let ($bindings_str)");
  $self->indent_level($self->indent_level + 1);

  # Track that we have an open let that needs closing
  $self->{_local_let_depth} //= 0;
  $self->{_local_let_depth}++;

  # For multi-var local with initializer: local($a, $b) = @_
  # The let bindings start empty; emit the assignment as first body form.
  # Include undef markers in the LHS vector so p-list-= can skip them.
  if ($init_idx >= 0 && @vars > 1) {
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;
    my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
    # RHS must be evaluated in list context so list-producing expressions
    # (qw(), function calls, etc.) return vectors. Wrap in let wantarray=t.
    $rhs_cl = "(let ((*wantarray* t)) $rhs_cl)";
    my $lhs_cl = "(vector " . join(" ", @vars) . ")";
    $self->_emit("(p-list-= $lhs_cl $rhs_cl)");
  }

  $self->_emit("");
}

# Process state variable declaration with init guard
sub _process_state_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  # Find the variable name(s) and initializer
  my @vars;
  my $found_assign = 0;  # 1 = '=', 2 = '//='
  my @init_parts;

  for my $part (@$parts) {
    my $ref = ref($part);

    if ($ref eq 'PPI::Token::Word' && $part->content eq 'state') {
      next;  # Skip 'state' keyword
    }
    elsif ($ref eq 'PPI::Token::Symbol' && !$found_assign) {
      push @vars, $part->content;
    }
    elsif ($ref eq 'PPI::Structure::List' && !$found_assign) {
      # state ($t, $u) — extract symbols from the list
      push @vars, $self->_find_symbols_in_list($part);
    }
    elsif ($ref eq 'PPI::Token::Operator' && $part->content eq '=' && !$found_assign) {
      $found_assign = 1;
    }
    elsif ($ref eq 'PPI::Token::Operator' && $part->content eq '//=' && !$found_assign) {
      $found_assign = 2;  # defined-or-assign: same semantics as = for state vars
    }
    elsif ($found_assign) {
      push @init_parts, $part;
    }
  }

  # Parse the initializer expression
  my $init_cl = 'nil';
  if (@init_parts) {
    $init_cl = $self->_parse_expression(\@init_parts, $stmt) // 'nil';
  }

  $self->_emit(";; $perl_code");

  # Generate init guard for each state variable, using the unique CL name.
  my $renames = $self->environment->state_var_renames // {};
  for my $var (@vars) {
    my $cl_var   = $renames->{$var} // $var;
    my $init_flag = "${cl_var}__init";
    my $sigil = substr($var, 0, 1);
    $self->_emit("(unless $init_flag");
    $self->indent_level($self->indent_level + 1);
    if ($sigil eq '$') {
      # Scalar: use ensure-boxed for the init value
      $self->_emit("(setf $cl_var (ensure-boxed $init_cl))");
    } elsif ($sigil eq '@') {
      # Array: only initialize if there's an explicit init expression
      $self->_emit("(p-array-= $cl_var (list $init_cl))") if @init_parts;
    } elsif ($sigil eq '%') {
      # Hash: only initialize if there's an explicit init expression
      $self->_emit("(p-hash-= $cl_var (list $init_cl))") if @init_parts;
    }
    $self->_emit("(setf $init_flag t))");
    $self->indent_level($self->indent_level - 1);
  }

  $self->_emit("");
}


# Process compound statement (if/while/for/bare block)
sub _process_compound_statement {
  my $self = shift;
  my $stmt = shift;
  my $external_continue = shift;  # Optional: continue block from sibling lookahead

  # Get the first keyword to determine statement type
  # Also detect any label (LABEL:) before the keyword
  my $first_word;
  my $first_block;
  my $label;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Label') {
      # Label like "OUTER:" - extract just the name
      $label = $child->content;
      $label =~ s/:$//;  # Remove trailing colon
    }
    elsif ($ref eq 'PPI::Token::Word') {
      $first_word = $child->content;
      last;
    }
    elsif ($ref eq 'PPI::Structure::Block' && !$first_block) {
      $first_block = $child;
      last;  # Found the block - don't scan further (avoid picking up 'continue' as first_word)
    }
  }

  if (!$first_word && $first_block) {
    # Bare block: { ... } possibly with continue { ... }
    # Scan remaining children for continue block (when PPI keeps it as child)
    my $continue_block = $external_continue;  # May have been found by sibling lookahead
    my $found_continue = 0;
    for my $child ($stmt->children) {
      my $ref = ref($child);
      if ($ref eq 'PPI::Token::Word' && $child->content eq 'continue') {
        $found_continue = 1;
      }
      elsif ($ref eq 'PPI::Structure::Block' && $found_continue) {
        $continue_block = $child;
        last;
      }
    }
    $self->_process_bare_block($first_block, $label, $continue_block);
  }
  elsif (!$first_word) {
    # Neither block nor keyword found - emit as comment
    my $perl_code = $stmt->content;
    $perl_code =~ s/\n/ /g;
    $self->_emit(";; COMPOUND (unknown) not handled: $perl_code");
    $self->_emit("");
  }
  elsif ($first_word eq 'if' || $first_word eq 'unless') {
    $self->_process_if_statement($stmt, $first_word);
  }
  elsif ($first_word eq 'while' || $first_word eq 'until') {
    $self->_process_while_statement($stmt, $first_word, $label);
  }
  elsif ($first_word eq 'for' || $first_word eq 'foreach') {
    $self->_process_for_statement($stmt, $first_word, $label);
  }
  else {
    # Unknown compound - emit as comment
    my $perl_code = $stmt->content;
    $perl_code =~ s/\n/ /g;
    $self->_emit(";; COMPOUND ($first_word) not yet implemented:");
    $self->_emit(";; $perl_code");
    $self->_emit("");
  }
}


# Process a bare block: { ... } possibly with continue { ... }
sub _process_bare_block {
  my $self  = shift;
  my $block = shift;
  my $label = shift;  # Optional label (e.g., TEST1: { ... })
  my $continue_block = shift;  # Optional continue block

  $self->_emit(";; { ... }");

  # Wrap in *package* save/restore so that any (in-package ...) calls inside
  # the block don't leak *package* to subsequent top-level forms after the block.
  # This is a no-op when no package changes happen inside.
  $self->_emit("(let ((*package* *package*))");
  $self->indent_level($self->indent_level + 1);

  # Wrap the entire bare block in a let for any my-declarations inside it.
  # Each bare block is its own lexical scope in Perl, so inner my-vars must
  # NOT be hoisted to the enclosing sub's let (see _find_all_declarations which
  # now stops recursing at Block boundaries).
  $self->_with_declarations($block, sub {

  # Save current section so that package changes inside the block don't
  # permanently redirect subsequent code (including after-block statements)
  # to a different CL package section. The closers and post-block code
  # must go to the same section as the block opening.
  my $saved_section = $self->_cur_section;
  # Save the transpile-time package stack so __PACKAGE__ and variable name
  # generation see the correct package after the block exits.
  my $saved_pkg_stack = [@{$self->environment->package_stack}];

  if ($label) {
    # Labeled bare block: use (block LABEL ...)
    # In Perl, a bare block is a single-iteration loop - last/next/redo all work.
    # With continue: wrap tagbody in catch for labeled next, then run continue after
    $self->_emit("(block $label");
    $self->indent_level($self->indent_level + 1);
    # Wrap contents in LAST-LABEL catch so p-last-dynamic can throw to exit the block.
    # Mirrors how p-next/p-redo use throw for dynamic (cross-function) labeled exits.
    # e.g. Test::More's skip() calls (last SKIP) from inside a called function.
    $self->_emit("(catch 'pcl::LAST-$label");
    $self->indent_level($self->indent_level + 1);
    if ($continue_block) {
      # Use pcl:: prefix to match the package used by p-next macro's throw
      $self->_emit("(catch 'pcl::NEXT-$label");
      $self->indent_level($self->indent_level + 1);
    }
    $self->_emit("(tagbody");
    $self->indent_level($self->indent_level + 1);
    $self->_emit(":redo");
    # Use pcl:: prefix to match the package used by p-redo macro's throw
    $self->_emit("(catch 'pcl::REDO-$label");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(progn");
    $self->indent_level($self->indent_level + 1);
    $self->_block_depth($self->_block_depth + 1);
    $self->_process_block($block);
    $self->_block_depth($self->_block_depth - 1);
    $self->_cur_section($saved_section);
    $self->environment->package_stack($saved_pkg_stack);
    $self->_emit("(go :next)))");  # close progn + catch'REDO + tagbody... no:
    # Actually: close progn ), close catch ), NOT tagbody
    $self->indent_level($self->indent_level - 2);
    # Back to tagbody content level
    $self->_emit("(go :redo)");
    $self->_emit(":next)");  # close tagbody
    $self->indent_level($self->indent_level - 1);
    if ($continue_block) {
      $self->_emit(")");  # close catch for NEXT
      $self->indent_level($self->indent_level - 1);
    }
    if ($continue_block) {
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_process_block($continue_block);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    $self->_emit(")");  # close LAST-LABEL catch
    $self->indent_level($self->indent_level - 1);
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  } else {
    # Unlabeled bare block: (block nil (tagbody :redo ... :next))
    # Supports redo, next, last without labels
    # Continue block runs after tagbody (after next/normal exit, not after last)
    $self->_emit("(block nil");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(tagbody :redo");
    $self->indent_level($self->indent_level + 1);
    $self->_block_depth($self->_block_depth + 1);
    $self->_process_block($block);
    $self->_block_depth($self->_block_depth - 1);
    $self->_cur_section($saved_section);
    $self->environment->package_stack($saved_pkg_stack);
    $self->_emit(":next)");
    $self->indent_level($self->indent_level - 1);
    if ($continue_block) {
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_process_block($continue_block);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  }); # end _with_declarations

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close (let ((*package* *package*)) ...)
  $self->_emit("");
}


# Process if/elsif/else statement
sub _process_if_statement {
  my $self     = shift;
  my $stmt     = shift;
  my $keyword  = shift;  # 'if' or 'unless'

  # Emit the original Perl as comment
  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Collect the if/elsif/else chain and conditions for declaration scanning
  my @clauses;  # Each: { type => 'if'|'elsif'|'else', cond => ..., block => ... }
  my @conditions;  # All condition elements for declaration scanning

  my $current_type;
  my $current_cond;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word') {
      my $word = $child->content;
      if ($word eq 'if' || $word eq 'elsif' || $word eq 'unless') {
        $current_type = $word;
      }
      elsif ($word eq 'else') {
        $current_type = 'else';
      }
    }
    elsif ($ref eq 'PPI::Structure::Condition') {
      $current_cond = $child;
      push @conditions, $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      push @clauses, {
        type  => $current_type,
        cond  => $current_cond,
        block => $child,
      };
      $current_cond = undef;
    }
  }

  # Use common helper to wrap with declarations
  $self->_with_declarations(\@conditions, sub {
    $self->_generate_if_clauses(\@clauses);
  });

  $self->_emit("");
}


# Generate CL for if/elsif/else chain
sub _generate_if_clauses {
  my $self    = shift;
  my $clauses = shift;

  return unless @$clauses;

  my $first = $clauses->[0];
  my $rest  = [@$clauses[1 .. $#$clauses]];

  # Generate condition
  my $cond_cl = $self->_parse_condition($first->{cond});

  # Emit comment for this clause
  my $cond_perl = $first->{cond} ? $first->{cond}->content : "";
  $cond_perl =~ s/^\s*\(\s*//;  # Remove leading paren
  $cond_perl =~ s/\s*\)\s*$//;  # Remove trailing paren
  $cond_perl =~ s/\n/\n;; /g;   # Add ;; to continuation lines
  $self->_emit(";; $first->{type} ($cond_perl)");

  # Handle 'unless' by negating
  if ($first->{type} eq 'unless') {
    $cond_cl = "(p-not $cond_cl)";
  }

  $self->_emit("(p-if $cond_cl");
  $self->indent_level($self->indent_level + 1);

  # Then block
  $self->_emit("(progn");
  $self->indent_level($self->indent_level + 1);
  $self->_with_declarations($first->{block}, sub {
    $self->_process_block($first->{block});
  });
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");

  # Else/elsif part
  if (@$rest) {
    my $next = $rest->[0];
    if ($next->{type} eq 'else') {
      # Simple else
      $self->_emit(";; else");
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_with_declarations($next->{block}, sub {
        $self->_process_block($next->{block});
      });
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    else {
      # elsif - recursive
      $self->_generate_if_clauses($rest);
    }
  }
  else {
    # No else clause - emit nil
    $self->_emit("nil");
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
}


# Process a block's contents
sub _process_block {
  my $self  = shift;
  my $block = shift;

  # Enter new scope for filehandles
  $self->environment->push_scope();

  # Track local let depth at block start
  my $start_depth = $self->{_local_let_depth} // 0;

  my @children = $block->children;
  my %skip;
  for my $i (0 .. $#children) {
    next if $skip{$i};
    my $child = $children[$i];
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    # Lookahead: bare block followed by continue { } as sibling
    if ($ref eq 'PPI::Statement::Compound') {
      my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
      if ($continue) {
        $self->_process_compound_statement($child, $continue);
        $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
        next;
      }
    }

    $self->_process_element($child);
  }

  # Close any let forms opened by local declarations in this block
  my $end_depth = $self->{_local_let_depth} // 0;
  while ($end_depth > $start_depth) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")  ;; end local");
    $self->{_local_let_depth}--;
    $end_depth--;
  }

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();
}


# Counter for anonymous block functions
my $anon_block_counter = 0;

# Counter for unique state variable names
my $state_var_counter = 0;

# Counter for unique lexical variable names (used in symbol-macrolet inside subs)
my $lex_var_counter = 0;

# Parse a block as a named function for eval/sub blocks
# Returns the generated function name
# $params is arrayref: [] for eval/sub
# $is_anon_sub: 1 for sub{} anonymous subs (adds &rest %_args + @_ binding)
# $return_lambda: 1 = return a (lambda ...) CL string instead of emitting (defun ...)
# Note: grep/map/sort now use parse_block_to_cl_string with inline lambdas
sub parse_block_as_function {
  my $self          = shift;
  my $block         = shift;  # PPI::Structure::Block
  my $params        = shift // [];  # Parameter names
  my $is_anon_sub   = shift // 0;   # 1 = anonymous sub (receives call args via @_)
  my $return_lambda = shift // 0;   # 1 = return lambda string, don't emit defun

  # Generate unique function name (used only for defun path)
  my $func_name = sprintf("--anon-block-%d--", ++$anon_block_counter);

  # For $return_lambda: redirect all _emit calls to a temp section so we
  # can collect the output as a string to return inline.
  my ($saved_sections, $saved_cur_section, $saved_cur_bucket, $saved_indent);
  if ($return_lambda) {
    $saved_sections    = $self->_sections;
    $saved_cur_section = $self->_cur_section;
    $saved_cur_bucket  = $self->_cur_bucket;
    $saved_indent      = $self->indent_level;
    $self->_sections([{
      pkg => '_lambda_', preamble => [], declarations => [], definitions => [], runtime => [],
    }]);
    $self->_cur_section(0);
    $self->_cur_bucket('runtime');
    $self->indent_level(0);
  }

  # For anonymous subs, detect state variables and wrap in outer let.
  # This mirrors the logic in _process_sub_statement for named subs.
  my %state_renames;
  my %anon_state_vars_set;
  if ($is_anon_sub && $block) {
    my @all_decls = @{$self->_find_all_declarations($block)};
    my %seen;
    my @state_vars = grep { !$seen{$_}++ }
                     map  { $_->{var} }
                     grep { $_->{type} eq 'state' } @all_decls;
    if (@state_vars) {
      %anon_state_vars_set = map { $_ => 1 } @state_vars;
      my @bindings;
      for my $var (@state_vars) {
        my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
        $bare //= $var; $sigil //= '$';
        my $bare_slug = $bare; $bare_slug =~ s/[^a-zA-Z0-9]/_/g;
        my $unique = sprintf("%sstate__anon__%s__%d",
                             $sigil, $bare_slug, ++$state_var_counter);
        $state_renames{$var} = $unique;
        # Initialize to sigil-appropriate empty container
        my $init_val = $sigil eq '$' ? '(make-p-box nil)'
                     : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                     :                 '(make-hash-table :test (quote equal))';
        push @bindings, "($unique $init_val)";
        push @bindings, "(${unique}__init nil)";
      }
      $self->_emit("(let (" . join(" ", @bindings) . ")");
      $self->indent_level($self->indent_level + 1);
    }
  }

  # Anonymous subs accept arguments like named subs: (&rest %_args)
  # with @_ bound inside the body via p-flatten-args.
  my $params_cl = $is_anon_sub ? '&rest %_args' : join(' ', @$params);

  # Emit the function definition
  if ($return_lambda) {
    $self->_emit("(lambda ($params_cl)");
  } else {
    $self->_emit("(defun $func_name ($params_cl)");
  }
  $self->indent_level($self->indent_level + 1);

  if ($is_anon_sub) {
    $self->_emit("(let ((\@_ (p-flatten-args %_args)))");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(catch :p-return");
    $self->indent_level($self->indent_level + 1);
  }

  $self->_emit("(block nil");
  $self->indent_level($self->indent_level + 1);

  # Enter new scope for filehandles; count as a subroutine so 'my'
  # declarations use the let-binding path, not eval-when+defvar.
  $self->environment->push_scope();
  $self->environment->in_subroutine($self->environment->in_subroutine + 1);

  # Wrap body in let for any 'my' declarations, then process contents.
  # For anon subs with state vars, set the rename map so _process_state_declaration
  # uses the unique CL names, and set _current_state_vars so it triggers.
  {
    local $self->{_current_state_vars} = \%anon_state_vars_set;
    my $saved_renames = $self->environment->state_var_renames;
    if (%state_renames) {
      # Merge with existing renames (parent closure renames must still apply)
      my %merged = (%{$saved_renames // {}}, %state_renames);
      $self->environment->state_var_renames(\%merged);
    }
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    });
    $self->environment->state_var_renames($saved_renames);
  }

  $self->environment->in_subroutine($self->environment->in_subroutine - 1);

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close block nil

  if ($is_anon_sub) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close catch :p-return
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close let @_
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close defun/lambda

  # Close outer state let if we opened one
  if (%state_renames) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close state let
  }

  if ($return_lambda) {
    # Collect all emitted lines from temp section and return as lambda string.
    # Definitions (BEGIN blocks) are hoisted to the real definitions bucket
    # rather than inlined in the lambda string — otherwise (eval-when ...) ends
    # up as the first argument to p-funcall-ref, making NIL the function ref.
    my $temp = $self->_sections->[0];
    my @hoisted_defs = @{$temp->{definitions}};
    my @lines = (
      @{$temp->{preamble}},
      @{$temp->{declarations}},
      @{$temp->{runtime}},
    );
    $self->_sections($saved_sections);
    $self->_cur_section($saved_cur_section);
    $self->_cur_bucket($saved_cur_bucket);
    $self->indent_level($saved_indent);
    # Re-emit hoisted definitions (BEGIN blocks, etc.) into the real sections
    if (@hoisted_defs) {
      my $section = $self->_sections->[$self->_cur_section];
      push @{$section->{'definitions'}}, @hoisted_defs;
    }
    return @lines ? join("\n", @lines) : "(lambda () nil)";
  }

  $self->_emit("");  # Blank line after function

  return $func_name;
}

# Parse a block and return its body as CL code string (for inline lambdas)
# Returns the CL code string for the block body
sub parse_block_to_cl_string {
  my $self   = shift;
  my $block  = shift;  # PPI::Structure::Block

  # Save current bucket state and indent; set up a fresh temp section
  my $saved_sections    = $self->_sections;
  my $saved_cur_section = $self->_cur_section;
  my $saved_cur_bucket  = $self->_cur_bucket;
  my $saved_indent      = $self->indent_level;
  my $saved_local_depth = $self->{_local_let_depth} // 0;

  $self->_sections([{
    pkg => '_temp_', preamble => [], declarations => [], definitions => [], runtime => [],
  }]);
  $self->_cur_section(0);
  $self->_cur_bucket('runtime');
  $self->indent_level(1);  # Start with some indent for readability

  # Enter new scope for filehandles
  $self->environment->push_scope();

  # Process block contents
  my $has_content = 0;
  for my $child ($block->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    $self->_process_element($child);
    $has_content = 1;
  }

  # Close any local forms opened inside this block (e.g. local $h{key})
  # Same logic as _process_block, but emitting into the temp section.
  my $end_depth = $self->{_local_let_depth} // 0;
  while ($end_depth > $saved_local_depth) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
    $self->{_local_let_depth}--;
    $end_depth--;
  }
  $self->{_local_let_depth} = $saved_local_depth;

  # Leave scope
  $self->environment->pop_scope();

  # Collect all lines from the temp section (assembled order)
  my $temp = $self->_sections->[0];
  my @body_lines = (
    @{$temp->{preamble}},
    @{$temp->{declarations}},
    @{$temp->{definitions}},
    @{$temp->{runtime}},
  );

  # Restore original state
  $self->_sections($saved_sections);
  $self->_cur_section($saved_cur_section);
  $self->_cur_bucket($saved_cur_bucket);
  $self->indent_level($saved_indent);

  # Return body as string (or "nil" if empty)
  if (@body_lines) {
    return join("\n", @body_lines);
  } else {
    return "nil";
  }
}


# Parse a block that contains hash key-value pairs: {key => val, ...}
# Used for map({key=>$_}, LIST) where the block is a hash constructor.
# Returns CL string: "(make-p-box (p-hash key val ...))"
sub parse_hash_block_to_cl_string {
  my $self  = shift;
  my $block = shift;  # PPI::Structure::Block

  my @raw = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@raw == 1 && $raw[0]->isa('PPI::Statement')) {
    @raw = grep { ref($_) !~ /Whitespace|Comment/ } $raw[0]->children();
  }

  my $result;
  eval {
    my $expr_o = Pl::PExpr->new(
      e           => \@raw,
      environment => $self->environment,
      parser      => $self,
    );
    my $pair_ids = $expr_o->parse_list(\@raw);
    my ($top_node, $top_id) = $expr_o->make_node_insert('hash_init');
    for my $id (@$pair_ids) {
      $expr_o->add_child_to_node($top_id, $id);
    }
    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => 0,
    );
    $result = $gen->generate($top_id);
  };
  die $@ if $@ && $@ =~ /^PCL:/;
  return $result // '(make-p-box (p-hash))';
}


# Find all variable declarations recursively in a PPI element
# Returns arrayref of { type => 'my'|'our'|..., var => '$x' }
sub _find_all_declarations {
  my $self = shift;
  my $elem = shift;
  my @decls;

  # Handle arrays of elements
  my @children;
  if (ref($elem) eq 'ARRAY') {
    @children = @$elem;
  } elsif (ref($elem) && $elem->can('children')) {
    @children = $elem->children;
  } else {
    return [];
  }

  my $pending_decl;
  for my $child (@children) {
    my $ref = ref($child);

    # Found a declarator keyword
    if ($ref eq 'PPI::Token::Word' && $child->content =~ /^(my|our|state|local)$/) {
      $pending_decl = $1;
    }
    # Found a variable after declarator
    elsif ($pending_decl && $ref eq 'PPI::Token::Symbol') {
      push @decls, { type => $pending_decl, var => $child->content };
      $pending_decl = undef;
    }
    # Found a list after declarator: my ($x, $y)
    elsif ($pending_decl && $ref eq 'PPI::Structure::List') {
      # Find all symbols inside the list
      my @list_vars = $self->_find_symbols_in_list($child);
      for my $var (@list_vars) {
        push @decls, { type => $pending_decl, var => $var };
      }
      $pending_decl = undef;
      # Don't recurse into this list - we already processed it
      next;
    }
    # Not a symbol or list after declarator - reset
    elsif ($pending_decl && $ref !~ /Whitespace/) {
      $pending_decl = undef;
    }

    # Recurse into nested elements, including bare blocks, but NOT into:
    #   - Named sub definitions (PPI::Statement::Sub)
    #   - BEGIN/END/etc blocks (PPI::Statement::Scheduled)
    #   - Anonymous sub bodies: PPI::Structure::Block whose previous
    #     non-whitespace sibling is the 'sub' keyword
    if ($ref && $child->can('children')
        && $ref ne 'PPI::Statement::Sub'
        && $ref ne 'PPI::Statement::Scheduled'
        && !($ref eq 'PPI::Structure::Block' && do {
               my $prev = $child->sprevious_sibling;
               $prev && ref($prev) eq 'PPI::Token::Word'
                     && $prev->content eq 'sub'
             })) {
      push @decls, @{$self->_find_all_declarations($child)};
    }
  }

  return \@decls;
}

# Helper: find all symbol names in a list structure like ($x, $y, @z)
sub _find_symbols_in_list {
  my $self = shift;
  my $list = shift;
  my @vars;

  for my $child ($list->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Symbol') {
      push @vars, $child->content;
    }
    elsif ($ref && $child->can('children')) {
      push @vars, $self->_find_symbols_in_list($child);
    }
  }

  return @vars;
}

# Like _find_symbols_in_list but also includes undef placeholders.
# Used by local() to preserve undef skip slots in list assignment.
sub _find_symbols_and_undefs_in_list {
  my $self = shift;
  my $list = shift;
  my @vars;

  for my $child ($list->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Symbol') {
      push @vars, $self->_transform_pkg_var($child->content);
    }
    elsif ($ref eq 'PPI::Token::Word' && $child->content eq 'undef') {
      push @vars, '(p-undef)';  # skip marker for p-list-=
    }
    elsif ($ref && $child->can('children')) {
      push @vars, $self->_find_symbols_and_undefs_in_list($child);
    }
  }

  return @vars;
}

# Return a hashref of all symbol names referenced inside any nested 'sub { }' blocks
# found within $elements. These are the variables "captured" by closures.
# Only direct PPI children's sub-blocks are searched; the caller intersects
# the result with _find_all_declarations to know which to rename.
sub _vars_referenced_in_closures {
  my ($self, $elements) = @_;
  my @elems = ref($elements) eq 'ARRAY' ? @$elements : ($elements);
  my %captured;
  for my $elem (@elems) {
    next unless ref($elem) && $elem->can('find');
    my $sub_kws = $elem->find(
      sub { $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'sub' }
    ) || [];   # PPI returns 0 (not undef) when nothing found — use || not //
    for my $kw (@$sub_kws) {
      # Skip NAMED subs (sub foo { ... }) — they are global defuns, not closures.
      # Only anonymous subs (sub { ... }) capture variables from the enclosing scope.
      # To detect named subs: the first non-whitespace sibling after 'sub' is a Word (the name).
      my $first = $kw->next_sibling;
      $first = $first->next_sibling while $first && $first->isa('PPI::Token::Whitespace');
      next if $first && $first->isa('PPI::Token::Word');  # named sub — skip

      # Walk forward to find the block (skipping prototypes/attributes for anon subs)
      my $sib = $kw->next_sibling;
      $sib = $sib->next_sibling while $sib && !$sib->isa('PPI::Structure::Block');
      next unless $sib;
      my $syms = $sib->find('PPI::Token::Symbol') || [];  # same: || not //
      $captured{$_->content} = 1 for @$syms;
    }
  }
  return \%captured;
}

# Common helper: wrap emitted code with let for any 'my' declarations
# Usage: $self->_with_declarations($ppi_elements, sub { ... emit code ... });
# $ppi_elements can be a single PPI element or arrayref of elements to scan
sub _with_declarations {
  my $self = shift;
  my $elements = shift;  # PPI element(s) to scan for declarations
  my $emit_body = shift; # Callback to emit the body code

  # Collect declarations from all elements
  my @all_decls;
  my @elems = ref($elements) eq 'ARRAY' ? @$elements : ($elements);
  for my $elem (@elems) {
    next unless defined $elem;
    push @all_decls, @{$self->_find_all_declarations($elem)};
  }

  # Get unique 'my' variables (skip state vars - they're handled at sub level)
  my $state_vars = $self->{_current_state_vars} // {};
  my %seen;
  my @my_vars = grep { !$seen{$_}++ }
                grep { !$state_vars->{$_} }  # skip state vars
                map { $_->{var} }
                grep { $_->{type} eq 'my' } @all_decls;

  # When inside a subroutine, rename 'my' vars that are captured by nested closures.
  # Fresh names (e.g. $i__lex__3) are never defvar'd, so the CL 'let' creates a
  # LEXICAL binding. Lambdas then capture the correct per-call copy, not a dynamic ref.
  my %new_renames;  # original perl name → unique CL name
  my %old_renames;  # original perl name → previous rename entry (undef if absent)
  if (@my_vars) {
    my $captured  = $self->_vars_referenced_in_closures($elements);
    my $existing  = $self->environment->state_var_renames // {};
    for my $var (@my_vars) {
      next unless $captured->{$var};
      my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
      $sigil //= '$'; $bare //= $var;
      (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
      my $unique = sprintf('%s%s__lex__%d', $sigil, $slug, ++$lex_var_counter);
      $new_renames{$var} = $unique;
      $old_renames{$var} = $existing->{$var};  # undef if no prior rename
    }
  }

  # Wrap in let if we have declarations
  if (@my_vars) {
    # Build let bindings using the (possibly renamed) CL variable names
    my $bindings = join(" ", map {
      my $let_var = $new_renames{$_} // $_;
      my $sigil = substr($let_var, 0, 1);
      my $init = $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
               : $sigil eq '%' ? "(make-hash-table :test #'equal)"
               :                 '(make-p-box nil)';
      "($let_var $init)"
    } @my_vars);
    $self->_emit("(let ($bindings)");
    $self->indent_level($self->indent_level + 1);

    # Track renamed/original vars as let-bound so _emit replaces p-scalar-= with
    # p-my-= (box-set), preventing the proclaim-special side-effect that would
    # turn future let bindings from lexical to dynamic and break closure capture.
    my $old_let_vars = $self->{_let_bound_vars};
    my @bound_names = map { $new_renames{$_} // $_ } @my_vars;
    $self->{_let_bound_vars} = { %{$old_let_vars // {}}, map { $_ => 1 } @bound_names };

    # Apply new renames to environment so ExprToCL emits the unique CL names.
    # Also expose them via _current_scope_new_renames for _process_variable_statement
    # to split RHS parsing (handles 'my $i = $i + 1' shadowing correctly).
    my $saved_env_renames;
    my $saved_scope_renames = $self->{_current_scope_new_renames};
    if (%new_renames) {
      $saved_env_renames = $self->environment->state_var_renames // {};
      my %merged = (%$saved_env_renames, %new_renames);
      $self->environment->state_var_renames(\%merged);
      $self->{_current_scope_new_renames} = \%new_renames;
      $self->{_current_scope_old_renames} = \%old_renames;
    }

    $emit_body->();

    # Restore rename map
    if (%new_renames) {
      $self->environment->state_var_renames($saved_env_renames);
      $self->{_current_scope_new_renames} = $saved_scope_renames;
      delete $self->{_current_scope_old_renames};
    }

    $self->{_let_bound_vars} = $old_let_vars;
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  } else {
    $emit_body->();
  }
}

# Parse a condition (from PPI::Structure::Condition)
# Returns: ($cl_code, $declarations_arrayref) in list context
#          $cl_code in scalar context
sub _parse_condition {
  my $self = shift;
  my $cond = shift;

  # Find ALL declarations recursively (including nested ones like my $x = my $y = 3)
  my $all_decls = $self->_find_all_declarations($cond);

  # Get the expression inside the condition
  my @parts;
  for my $child ($cond->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';

    if ($ref eq 'PPI::Statement::Expression') {
      # Get children of the expression
      push @parts, grep {
        ref($_) ne 'PPI::Token::Whitespace'
      } $child->children;
    }
    else {
      push @parts, $child;
    }
  }

  my ($result, $decls) = $self->_parse_expression(\@parts, $cond);
  $result //= "nil";

  # Merge: use our recursive findings (which catches nested decls)
  return wantarray ? ($result, $all_decls) : $result;
}


# Process while/until loop
sub _process_while_statement {
  my $self    = shift;
  my $stmt    = shift;
  my $keyword = shift;
  my $label   = shift;  # Optional loop label

  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Find condition, block, and optional continue block
  my ($cond, $block, $continue_block);
  my $found_body = 0;
  my $found_continue = 0;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Structure::Condition') {
      $cond = $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      if (!$found_body) {
        $block = $child;
        $found_body = 1;
      } elsif ($found_continue) {
        $continue_block = $child;
      }
    }
    elsif ($ref eq 'PPI::Token::Word' && $child->content eq 'continue') {
      $found_continue = 1;
    }
  }

  # Get condition CL code
  my $cond_cl = $cond ? $self->_parse_condition($cond) : "t";
  $cond_cl //= "t";

  # while (<FH>) with no explicit assignment → implicitly assign to $_
  # PPI: condition has a single Expression containing a single QuoteLike::Readline
  if ($cond && $cond_cl =~ /^\(p-readline\b/) {
    my @non_ws = grep { !ref($_) || ref($_) ne 'PPI::Token::Whitespace' } $cond->children;
    if (@non_ws == 1 && ref($non_ws[0]) eq 'PPI::Statement::Expression') {
      my @expr_ch = grep { ref($_) ne 'PPI::Token::Whitespace' } $non_ws[0]->children;
      if (@expr_ch == 1 && ref($expr_ch[0]) eq 'PPI::Token::QuoteLike::Readline') {
        $cond_cl = "(p-setf \$_ $cond_cl)";
      }
    }
  }

  # Handle 'until' by negating
  if ($keyword eq 'until') {
    $cond_cl = "(p-not $cond_cl)";
  }

  # Build the loop form with optional label
  my $label_arg = $label ? " :label $label" : "";

  # Use common helper to wrap with declarations
  $self->_with_declarations($cond, sub {
    $self->_emit("(p-while $cond_cl$label_arg");
    $self->indent_level($self->indent_level + 1);
    if ($block) {
      $self->_with_declarations($block, sub {
        $self->_process_block($block);
      });
    }
    if ($continue_block) {
      $self->_emit(":continue (progn");
      $self->indent_level($self->indent_level + 1);
      $self->_process_block($continue_block);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  });

  $self->_emit("");
}


# Process for/foreach loop
sub _process_for_statement {
  my $self    = shift;
  my $stmt    = shift;
  my $keyword = shift;
  my $label   = shift;  # Optional loop label

  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Check for C-style for vs foreach style, and detect continue block
  my $c_style_for;
  my $block;
  my $continue_block;
  my $found_continue = 0;

  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Structure::For') {
      $c_style_for = $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      if ($found_continue) {
        $continue_block = $child;
      } elsif (!$block || $c_style_for) {
        $block = $child;
      }
    }
    elsif ($ref eq 'PPI::Token::Word' && $child->content eq 'continue') {
      $found_continue = 1;
    }
  }

  if ($c_style_for) {
    $self->_process_c_style_for($c_style_for, $block, $stmt, $label);
  }
  else {
    $self->_process_foreach_loop($stmt, $block, $label, $continue_block);
  }
}


# Process C-style for loop: for (init; cond; incr) { }
sub _process_c_style_for {
  my $self   = shift;
  my $for_struct = shift;
  my $block  = shift;
  my $stmt   = shift;
  my $label  = shift;  # Optional loop label

  # Collect the three statements from the for structure
  my @statements;
  for my $child ($for_struct->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    if ($ref =~ /^PPI::Statement/) {
      push @statements, $child;
    }
  }

  my $init_cl = '';
  my $cond_cl = 't';  # Default to true
  my $incr_cl = '';

  # Process init statement (may be variable declaration or expression)
  if (@statements >= 1) {
    my $init_stmt = $statements[0];
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $init_stmt->children;

    if (@parts) {
      # Strip 'my'/'our'/'local' keywords for parsing (declarations handled by _with_declarations)
      if (ref($init_stmt) eq 'PPI::Statement::Variable') {
        @parts = grep { !(ref($_) eq 'PPI::Token::Word' && $_->content =~ /^(my|our|local)$/) } @parts;
      }
      $init_cl = $self->_parse_expression(\@parts, $stmt) // '' if @parts;
    }
  }

  # Process condition
  if (@statements >= 2) {
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $statements[1]->children;
    $cond_cl = $self->_parse_expression(\@parts, $stmt) // 't' if @parts;
  }

  # Process increment
  if (@statements >= 3) {
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $statements[2]->children;
    $incr_cl = $self->_parse_expression(\@parts, $stmt) // '' if @parts;
  }

  # Build label argument if present
  my $label_arg = $label ? " :label $label" : "";

  # Use common helper - scan init and condition for declarations
  my @decl_sources = grep { defined } @statements[0..1];
  $self->_with_declarations(\@decl_sources, sub {
    $self->_emit("(p-for ($init_cl)");
    $self->_emit("        ($cond_cl)");
    $self->_emit("        ($incr_cl)$label_arg");
    $self->indent_level($self->indent_level + 1);
    if ($block) {
      $self->_with_declarations($block, sub {
        $self->_process_block($block);
      });
    }
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  });

  $self->_emit("");
}


# Process foreach-style loop: for/foreach VAR (LIST) { }
sub _process_foreach_loop {
  my $self  = shift;
  my $stmt  = shift;
  my $block = shift;
  my $label = shift;  # Optional loop label
  my $continue_block = shift;  # Optional continue block

  my $loop_var;
  my @list_parts;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Symbol' && !$loop_var) {
      $loop_var = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # The list expression
      for my $lc ($child->children) {
        next if ref($lc) eq 'PPI::Token::Whitespace';
        if (ref($lc) eq 'PPI::Statement::Expression') {
          push @list_parts, grep {
            ref($_) ne 'PPI::Token::Whitespace'
          } $lc->children;
        }
        else {
          push @list_parts, $lc;
        }
      }
    }
  }

  $loop_var //= '$_';
  my $list_cl = @list_parts
    ? ($self->_parse_expression(\@list_parts, $stmt) // "(list)")
    : "(list)";

  # Convert (progn ...) to (vector ...) for foreach list context
  # This handles: foreach (1, 2, 3) which parses as a progn
  # Use \s* to handle potential leading whitespace from indentation
  $list_cl =~ s/^\s*\(progn\b/(vector/;

  # Wrap single scalar values in a vector for foreach
  # This handles: foreach (42) or foreach ($x) where $x is a scalar
  # Skip if it's already a vector, array, hash, range, or function call
  if ($list_cl !~ /^\s*\(/ && $list_cl !~ /^[@%]/) {
    $list_cl = "(vector $list_cl)";
  }

  # Build label argument if present
  my $label_arg = $label ? " :label $label" : "";

  $self->_emit("(p-foreach ($loop_var $list_cl)$label_arg");
  $self->indent_level($self->indent_level + 1);
  if ($block) {
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    });
  }
  if ($continue_block) {
    $self->_emit(":continue (progn");
    $self->indent_level($self->indent_level + 1);
    $self->_process_block($continue_block);
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
  $self->_emit("");
}


# Process subroutine declaration
sub _process_sub_statement {
  my $self = shift;
  my $stmt = shift;

  my $name = '';
  my $prototype = '';
  my $block;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word' && $child->content ne 'sub') {
      $name = $child->content unless $name;
    }
    elsif ($ref eq 'PPI::Token::Prototype') {
      $prototype = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::Signature') {
      # Perl 5.20+ signature (when 'use feature "signatures"' is used)
      $prototype = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      $block = $child;
    }
  }

  # At file scope, route named sub definitions to the declarations bucket.
  # declarations is assembled before definitions (BEGIN blocks, use/require),
  # which matches Perl: all named subs are compiled before any BEGIN runs,
  # so \&foo inside BEGIN can always find the sub already defined.
  # Inside subs (in_subroutine > 0), nested named subs emit in-place.
  my $old_bucket = $self->_cur_bucket;
  $self->_cur_bucket('declarations') if $self->environment->in_subroutine == 0;

  # Emit Perl code as comment
  my $perl_code = $stmt->content;
  $perl_code =~ s/\{.*\}$/{ ... }/s;  # Abbreviate body
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Parse prototype/signature
  # Default: -1 means "unknown/list" - sub takes any number of args
  # Only explicit prototypes/signatures set specific min_params
  my $sig_info = { params => [], min_params => -1, is_proto => 0 };
  if ($prototype) {
    $sig_info = $self->parse_prototype_or_signature($prototype, $stmt);
  }

  # Store in environment for later use by PExpr
  if ($name) {
    $self->environment->add_prototype($name, $sig_info);
    # Also record for forward declarations
    my $pkg = $self->environment->current_package();
    $self->environment->add_declared_sub($name, $pkg);
  }

  # Build parameter list for defun
  my @param_names;
  my @optional_params;
  my $in_optional = 0;

  for my $param (@{$sig_info->{params}}) {
    my $pname = $param->{name};

    # For old-style prototypes, skip ALL params - body uses @_ directly
    # (We still store proto_type for auto-boxing at call sites)
    next if $sig_info->{is_proto};

    if (defined $param->{default_cl}) {
      # Parameter with default goes to &optional
      push @optional_params, { name => $pname, default => $param->{default_cl} };
      $in_optional = 1;
    }
    elsif ($pname =~ /^[\@\%]/) {
      # Slurpy parameter - use &rest
      push @optional_params, { name => $pname, rest => 1 };
    }
    elsif ($in_optional) {
      # After seeing optional, all are optional
      push @optional_params, { name => $pname, default => 'nil' };
    }
    else {
      # Required parameter
      push @param_names, $pname;
    }
  }

  # Build the parameter string
  # CL order: required &optional &rest &key
  my $params_cl = join(' ', @param_names);

  if (@optional_params) {
    my @opt_strs;
    my $rest_param;

    for my $opt (@optional_params) {
      if ($opt->{rest}) {
        $rest_param = $opt->{name};
      }
      else {
        push @opt_strs, "($opt->{name} $opt->{default})";
      }
    }

    # &optional comes before &rest
    if (@opt_strs) {
      $params_cl .= ' &optional ' . join(' ', @opt_strs);
    }

    # &rest before &key
    if ($rest_param) {
      $params_cl .= ' &rest ' . $rest_param;
    }
  }

  # If no explicit parameters, add &rest %_args to capture arguments
  # Then convert to @_ vector so shift/pop work correctly
  # wantarray is handled via *wantarray* dynamic variable (set by caller)
  my $needs_args_conversion = 0;
  if (!@param_names && !@optional_params) {
    $params_cl = '&rest %_args';
    $needs_args_conversion = 1;
  }

  # Find state declarations in the block (they need special handling)
  my @state_vars;
  if ($block) {
    my @all_decls = @{$self->_find_all_declarations($block)};
    my %seen;
    @state_vars = grep { !$seen{$_}++ }
                  map { $_->{var} }
                  grep { $_->{type} eq 'state' } @all_decls;
  }

  # If we have state vars, wrap defun in a let for persistent storage.
  # Use unique CL names ($state--subname--varname--N) to avoid colliding with
  # any defvar declarations at file scope, which would make the symbol SPECIAL
  # and turn the lexical let into a dynamic binding that evaporates after load.
  my %state_renames;
  if (@state_vars) {
    my $sub_slug = $name ? $name : 'anon';
    $sub_slug =~ s/[^a-zA-Z0-9]/-/g;
    my @bindings;
    for my $var (@state_vars) {
      # Strip sigil for the slug part, keep sigil for the CL name
      my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
      $bare //= $var; $sigil //= '$';
      my $bare_slug = $bare; $bare_slug =~ s/[^a-zA-Z0-9]/-/g;
      my $unique = sprintf("%sstate__%s__%s__%d",
                           $sigil, $sub_slug, $bare_slug, ++$state_var_counter);
      $state_renames{$var} = $unique;
      # Initialize state var to an appropriate empty container by sigil.
      # $ → box(nil) so p-pre++/p-post++ work even before init guard fires.
      # @ → empty adjustable vector; % → empty hash table.
      my $init_val = $sigil eq '$' ? '(make-p-box nil)'
                   : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                   :                 '(make-hash-table :test (quote equal))';
      push @bindings, "($unique $init_val)";
      push @bindings, "(${unique}__init nil)";
    }
    $self->_emit("(let (" . join(" ", @bindings) . ")");
    $self->indent_level($self->indent_level + 1);
  }

  # User-defined subs get p- prefix to avoid conflicts with CL built-ins
  # Use p-sub macro to wrap in eval-when for BEGIN block visibility
  # Wrap body in (block nil ...) so p-return works
  # Handle qualified names: A::foo -> A::p-foo (not p-A::foo)

  # When inside a bare block (block_depth > 0), a simple 'package Foo;'
  # changes the environment's package stack but NOT the CL section.
  # The sub must carry a fully-qualified name so SBCL interns it in the
  # correct CL package at read time, regardless of *package*.
  my $effective_name = $name;
  if ($name && $name !~ /::/ && $self->_block_depth > 0) {
    my $pkg = $self->environment->current_package();
    $effective_name = "$pkg\::$name" if $pkg ne 'main';
  }
  my $cl_sub_name = $self->_qualified_sub_to_cl($effective_name);

  # All named subs get a p-declare-sub stub in the declarations bucket.
  # This ensures forward references (e.g. \&foo in a BEGIN block before
  # 'sub foo {}' in source) always resolve, regardless of source order.
  # The declarations bucket is assembled before definitions in the output.
  # p-declare-sub is idempotent: it only creates the stub if the real
  # definition hasn't loaded yet.
  if ($name) {
    push @{$self->_sections->[$self->_cur_section]{declarations}},
         "(p-declare-sub $cl_sub_name)";
  }

  # Forward declaration: sub name; or sub name ($); or sub name : attrs;
  # The p-declare-sub stub in declarations is sufficient; nothing more needed.
  unless ($block) {
    $self->_cur_bucket($old_bucket);
    return;
  }

  $self->_emit("(p-sub $cl_sub_name ($params_cl)");
  $self->indent_level($self->indent_level + 1);

  # If using %_args, convert to @_ vector
  if ($needs_args_conversion) {
    $self->_emit("(let ((\@_ (p-flatten-args %_args)))");
    $self->indent_level($self->indent_level + 1);
  }

  $self->_emit("(block nil");
  $self->indent_level($self->indent_level + 1);

  # Track that we're inside a subroutine (for shift/pop @_ vs @ARGV)
  $self->environment->in_subroutine($self->environment->in_subroutine + 1);

  if ($block) {
    # Wrap sub body with let for local variable declarations.
    # Pass state_vars so _with_declarations knows to skip them.
    # Also set rename map in environment so ExprToCL remaps $x -> $state--sub--x--N.
    local $self->{_current_state_vars} = { map { $_ => 1 } @state_vars };
    my $saved_renames = $self->environment->state_var_renames;
    $self->environment->state_var_renames(\%state_renames) if %state_renames;
    # Save package stack: inline 'package NAME;' inside a sub body must not leak
    my $saved_pkg_stack = [@{$self->environment->package_stack}];
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    });
    # Restore package stack in case of inline package switches inside the sub
    $self->environment->package_stack($saved_pkg_stack);
    $self->environment->state_var_renames($saved_renames);
  }
  else {
    $self->_emit("nil");
  }

  # Leaving subroutine
  $self->environment->in_subroutine($self->environment->in_subroutine - 1);

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close block

  if ($needs_args_conversion) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close let
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close defun

  # Close state vars let
  if (@state_vars) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  $self->_emit("");

  # Restore previous bucket
  $self->_cur_bucket($old_bucket);
}


# Process package declaration
sub _process_package_statement {
  my $self = shift;
  my $stmt = shift;

  my $pkg_name = $stmt->namespace // 'main';

  # Register package as a known class/package for method call resolution
  $self->environment->add_package($pkg_name);

  # Check for block form: package Foo { ... }
  my $block;
  for my $child ($stmt->schildren) {
    if (ref($child) eq 'PPI::Structure::Block') {
      $block = $child;
      last;
    }
  }

  if ($block) {
    # Block form: push package, process block, pop
    if ($self->environment->in_subroutine > 0) {
      # Inside a function body: emit package setup INLINE (no new section, no
      # in-package).  Using (in-package ...) here would change the CL reader's
      # package context for the rest of the file, corrupting all subsequent code.
      # Instead we:
      #   1. Emit (p-defpackage) and (defclass) inline.
      #   2. Temporarily increment _block_depth so _process_sub_statement emits
      #      fully-qualified names like |Point|::p-new (not just p-new).
      #      p-sub already handles qualified names via symbol-package.
      #   3. At runtime (when the enclosing function is called) the eval-when
      #      :execute semantics kick in and the package + methods are created in
      #      the right order within the same call.
      my $saved_pkg_stack = [@{$self->environment->package_stack}];

      my $cl_pkg = ($pkg_name =~ /::/ || lc($pkg_name) eq 'class' ||
                    lc($pkg_name) eq 'error' || lc($pkg_name) eq 'method' ||
                    lc($pkg_name) eq 'function')
                   ? ":|$pkg_name|" : ":$pkg_name";
      my $cl_class = $self->_pkg_to_clos_class($pkg_name);

      $self->_emit(";;; inline package $pkg_name");
      $self->_emit("(p-defpackage $cl_pkg)");
      $self->_emit(";; CLOS class for MRO");
      $self->_emit("(defclass $cl_class () ())");
      $self->_emit("");

      $self->environment->push_package($pkg_name);
      # Increment _block_depth so sub names become fully qualified (e.g. |Point|::p-new)
      $self->_block_depth($self->_block_depth + 1);

      for my $child ($block->schildren) {
        $self->_process_element($child);
      }

      $self->_block_depth($self->_block_depth - 1);
      $self->environment->pop_package();
      $self->environment->package_stack($saved_pkg_stack);
    }
    else {
      # Top-level block form: push package, process block, pop
      $self->_emit_package_preamble($pkg_name);
      $self->environment->push_package($pkg_name);

      # Process the block contents
      for my $child ($block->schildren) {
        $self->_process_element($child);
      }

      $self->environment->pop_package();
      # Switch back to previous package: open a new section with in-package in preamble
      my $prev_pkg = $self->environment->current_package();
      my $cl_prev  = $prev_pkg =~ /::/ ? ":|$prev_pkg|" : ":$prev_pkg";
      $self->_open_section($prev_pkg);
      $self->_cur_bucket('runtime');
      $self->_with_bucket('preamble', sub {
        $self->_emit("(in-package $cl_prev)");
        $self->_emit(";;; end package $pkg_name");
        $self->_emit("");
      });
    }
  }
  else {
    # Simple form: package Foo;
    # This changes the current package until another package declaration
    if ($self->environment->in_subroutine > 0) {
      # Inside a sub body: just update the environment.
      # Do NOT emit (in-package) or open a new section — that would break the
      # SBCL reader's package context and corrupt the section/bucket structure.
      # The environment's current_package is used by codegen (e.g. 1-arg bless).
      $self->environment->push_package($pkg_name);
    } else {
      $self->_emit_package_preamble($pkg_name);
      $self->environment->push_package($pkg_name);
      # Note: no pop - package remains active until next package declaration
    }
  }
}


# Emit CL package preamble (defpackage + in-package)
# Uses pipe-quoting for package names with :: or that conflict with CL symbols
# Also emits a CLOS class for MRO tracking (inheritance)
sub _emit_package_preamble {
  my $self     = shift;
  my $pkg_name = shift;

  # Pipe-quote if contains :: or conflicts with CL symbols
  my $cl_pkg = ($pkg_name =~ /::/ || lc($pkg_name) eq 'class' || lc($pkg_name) eq 'error' ||
                lc($pkg_name) eq 'method' || lc($pkg_name) eq 'function')
               ? ":|$pkg_name|" : ":$pkg_name";

  my $cl_class = $self->_pkg_to_clos_class($pkg_name);

  if ($self->_block_depth > 0) {
    # Inside a runtime block: emit package setup inline to the current bucket.
    # Opening a new section here would place its preamble/declarations outside
    # the block in the linear assembly, causing scope and symbol-table confusion.
    $self->_emit(";;; package $pkg_name");
    $self->_emit("(p-defpackage $cl_pkg)");
    $self->_emit("(in-package $cl_pkg)");
    $self->_emit(";; CLOS class for MRO");
    $self->_emit("(defclass $cl_class () ())");
    $self->_emit("");
    return;
  }

  # Open a new section for this package; preamble goes in its preamble bucket
  $self->_open_section($pkg_name);
  $self->_cur_bucket('runtime');  # subsequent code defaults to runtime

  $self->_with_bucket('preamble', sub {
    $self->_emit(";;; package $pkg_name");
    $self->_emit("(p-defpackage $cl_pkg)");
    $self->_emit("(in-package $cl_pkg)");
    $self->_emit(";; CLOS class for MRO");
    $self->_emit("(defclass $cl_class () ())");
    $self->_emit("");
  });
}

# Convert Perl package name to CLOS class name
# Foo::Bar -> foo-bar
# Pipe-quote names that might conflict with CL symbols (e.g., class, method)
sub _pkg_to_clos_class {
  my ($self, $pkg) = @_;
  my $class = lc($pkg);
  $class =~ s/::/-/g;
  # Pipe-quote to avoid CL symbol conflicts (especially 'class', 'error')
  if ($class eq 'class' || $class eq 'method' || $class eq 'function' ||
      $class eq 'error' || $class eq 'warning' || $class eq 'condition' ||
      $class eq 'standard-class' || $class eq 'standard-object') {
    return "|$class|";
  }
  return $class;
}


# PCL runtime symbols that might conflict with user sub names
my %PCL_SYMBOLS = map { $_ => 1 } qw(
  p-x p-y p-print p-say p-length p-push p-pop p-shift p-unshift
  p-keys p-values p-exists p-delete p-sort p-reverse p-map p-grep
  p-join p-split p-ref p-bless p-die p-warn p-open p-close
  p-read p-write p-int p-abs p-substr p-index p-lc p-uc
);

# Check if a sub name conflicts with PCL runtime
sub _is_pcl_symbol {
  my $self = shift;
  my $name = shift;
  return exists $PCL_SYMBOLS{"p-$name"};
}


# Process use/require statements
sub _process_include_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;

  my $type = $stmt->type // 'use';    # 'use', 'require', 'no'
  my $module = $stmt->module // '';

  # Handle 'use constant' specially
  if ($module eq 'constant') {
    $self->_process_use_constant($stmt, $perl_code);
    return;
  }

  # Handle 'use vars' - declare package globals with defvar
  if ($module eq 'vars') {
    $self->_process_use_vars($stmt, $perl_code);
    return;
  }

  # Handle 'no' statements
  if ($type eq 'no') {
    # 'no integer' - turn off integer pragma in current scope
    if ($module eq 'integer') {
      $self->environment->set_pragma('use_integer', 0);
    }
    $self->_emit(";; $perl_code (no-op)");
    $self->_emit("");
    return;
  }

  # Handle version declarations (use v5.30, use 5.030, etc.)
  if ($perl_code =~ /^use\s+v?5[\d.]+$/) {
    $self->_emit(";; $perl_code (pragma)");
    $self->_emit("");
    return;
  }

  # Handle require with path expression (e.g., require "./test.pl", require $path,
  # require $path . "/" . $file)
  # PPI returns empty module for these - we need to parse the expression
  if ($module eq '' && $type eq 'require') {
    # Collect all tokens after 'require' (excluding whitespace at start/end and semicolon)
    my @tokens;
    my $found_require = 0;
    for my $child ($stmt->children) {
      if ($child->isa('PPI::Token::Word') && $child->content eq 'require') {
        $found_require = 1;
        next;
      }
      next unless $found_require;
      next if $child->isa('PPI::Token::Structure');  # Skip semicolon
      push @tokens, $child;
    }

    # Skip leading/trailing whitespace
    shift @tokens while @tokens && $tokens[0]->isa('PPI::Token::Whitespace');
    pop @tokens while @tokens && $tokens[-1]->isa('PPI::Token::Whitespace');

    if (@tokens) {
      # Check if it's a version number (require 5.007, require v5.10, etc.) - no-op
      if (@tokens == 1 && ($tokens[0]->isa('PPI::Token::Number')
                           || ($tokens[0]->isa('PPI::Token::Word')
                               && $tokens[0]->content =~ /^v\d/))) {
        $self->_emit(";; $perl_code (version requirement, no-op)");
        $self->_emit("");
        return;
      }

      # Check if it's a simple string literal (compile-time)
      if (@tokens == 1 && $tokens[0]->isa('PPI::Token::Quote')) {
        my $path = $tokens[0]->string;
        $self->_emit(";; $perl_code");
        $self->_emit("(p-eval-direct");
        $self->_emit("  (p-require-file \"$path\"))");
        $self->_emit("");
        return;
      }

      # Otherwise, parse as expression (runtime)
      # Use the parser's _parse_expression method
      my $expr_cl = $self->_parse_expression(\@tokens);
      if ($expr_cl) {
        $self->_emit(";; $perl_code");
        $self->_emit("(p-require-file $expr_cl)");
        $self->_emit("");
        return;
      }
    }

    # Fallback
    $self->_emit(";; $perl_code (require without path)");
    $self->_emit("");
    return;
  }

  # Handle use with empty module (version pragmas handled above)
  if ($module eq '') {
    $self->_emit(";; $perl_code (pragma)");
    $self->_emit("");
    return;
  }

  # Handle pragmas - emit as comment (no CL equivalent)
  if ($module =~ /^(strict|warnings|warnings::register|feature|utf8|open|parent|base|Exporter|bytes|locale|integer|builtin|overloading|XSLoader|DynaLoader|Carp|re|version)$/) {
    # 'use integer' - enable integer pragma in current scope
    if ($module eq 'integer') {
      $self->environment->set_pragma('use_integer', 1);
    }
    $self->_emit(";; $perl_code (pragma)");
    $self->_emit("");
    return;
  }

  # Handle 'use lib' - modify @INC
  if ($module eq 'lib') {
    $self->_process_use_lib($stmt, $perl_code);
    return;
  }

  # require inside a sub body must stay inline (not hoisted) so that:
  # 1. eval { require Foo } can catch load failures properly
  # 2. Perl semantics: require inside a sub runs at call time, not compile time
  if ($type eq 'require' && $self->environment->in_subroutine > 0) {
    $self->_emit(";; $perl_code");
    $self->_emit("(p-require \"$module\")");
    $self->_emit("");
    return;
  }

  # General use/require — emit to definitions bucket (before runtime code)
  $self->_with_bucket('definitions', sub {
    if ($type eq 'use') {
      my @imports = $self->_parse_use_import_list($stmt);

      # Extract prototypes from module at transpile time
      # This allows prototypes in other files to work..
      my $module_env = $self->_extract_module_prototypes($module);
      if ($module_env) {
        $self->_merge_module_prototypes($module_env, \@imports);
      }

      $self->_emit(";; $perl_code");
      if (@imports) {
        my $list = join(' ', map { qq{"$_"} } @imports);
        $self->_emit("(p-eval-direct");
        $self->_emit("  (p-use \"$module\" :imports '($list)))");
      } else {
        $self->_emit("(p-eval-direct");
        $self->_emit("  (p-use \"$module\"))");
      }
    }
    elsif ($type eq 'require') {
      $self->_emit(";; $perl_code");
      $self->_emit("(p-eval-direct");
      $self->_emit("  (p-require \"$module\"))");
    }
    else {
      # Unknown type
      $self->_emit(";; $perl_code");
      $self->_emit(";; (include type '$type' not yet implemented)");
    }
    $self->_emit("");
  });
}


# Process scheduled blocks: BEGIN, END, CHECK, INIT
sub _process_scheduled_block {
  my $self = shift;
  my $stmt = shift;

  my $type = $stmt->type;  # 'BEGIN', 'END', 'CHECK', 'INIT', 'UNITCHECK'
  my $perl_code = $stmt->content;
  $perl_code =~ s/\n.*//s;  # First line only for comment

  # Find the block
  my ($block) = grep { $_->isa('PPI::Structure::Block') } $stmt->schildren;
  unless ($block) {
    $self->_emit(";; $type { } (no block found)");
    return;
  }

  if ($type eq 'BEGIN') {
    # BEGIN blocks execute at compile time — route to definitions bucket.
    # NOT at :load-toplevel - BEGIN should only run once, not again when loading fasl.
    $self->_with_bucket('definitions', sub {
      $self->_emit(";; $perl_code");
      $self->_emit("(eval-when (:compile-toplevel :execute)");
      $self->indent_level($self->indent_level + 1);
      $self->_process_children($block);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
      $self->_emit("");
    });
  }
  elsif ($type eq 'END') {
    # END blocks execute at program exit — route to definitions bucket.
    # Push a lambda to *end-blocks* (push gives LIFO = correct reverse order)
    $self->_with_bucket('definitions', sub {
      $self->_emit(";; $perl_code");
      $self->_emit("(push (lambda ()");
      $self->indent_level($self->indent_level + 2);
      $self->_process_children($block);
      $self->indent_level($self->indent_level - 2);
      $self->_emit("  ) *end-blocks*)");
      $self->_emit("");
    });
  }
  elsif ($type eq 'CHECK' || $type eq 'UNITCHECK') {
    # CHECK runs after compile, before execute — route to definitions bucket.
    $self->_with_bucket('definitions', sub {
      $self->_emit(";; $perl_code");
      $self->_emit("(eval-when (:load-toplevel)");
      $self->indent_level($self->indent_level + 1);
      $self->_process_children($block);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
      $self->_emit("");
    });
  }
  elsif ($type eq 'INIT') {
    # INIT runs just before main code starts — keep in runtime (source-order sensitive)
    $self->_emit(";; $perl_code (runs at load time, before main)");
    $self->_process_children($block);
    $self->_emit("");
  }
  else {
    $self->_emit(";; $type { } (unrecognized scheduled block)");
  }
}


# Process 'use lib' statements
sub _process_use_lib {
  my ($self, $stmt, $perl_code) = @_;

  # use lib is compile-time @INC manipulation — route to definitions bucket
  # so it appears before any 'require' or 'use' in the same section
  $self->_with_bucket('definitions', sub {
    $self->_emit(";; $perl_code");
    $self->_emit("(p-eval-direct");

    # Extract path arguments from the statement
    for my $child ($stmt->schildren) {
      if ($child->isa('PPI::Token::Quote')) {
        my $path = $child->string;
        $self->_emit("  (p-unshift \@INC \"$path\")");
      }
      elsif ($child->isa('PPI::Token::QuoteLike::Words')) {
        # qw(path1 path2)
        my $content = $child->content;
        $content =~ s/^qw\s*[\(\[\{<]//;
        $content =~ s/[\)\]\}>]$//;
        for my $path (split /\s+/, $content) {
          $self->_emit("  (p-unshift \@INC \"$path\")") if $path;
        }
      }
    }
    $self->_emit(")");  # Close eval-when
    $self->_emit("");
  });

  # Also add to transpiler's inc_paths for module finding
  for my $child ($stmt->schildren) {
    if ($child->isa('PPI::Token::Quote')) {
      unshift @{$self->inc_paths}, $child->string;
    }
    elsif ($child->isa('PPI::Token::QuoteLike::Words')) {
      my $content = $child->content;
      $content =~ s/^qw\s*[\(\[\{<]//;
      $content =~ s/[\)\]\}>]$//;
      for my $path (split /\s+/, $content) {
        unshift @{$self->inc_paths}, $path if $path;
      }
    }
  }
}


# Find a module file in @INC paths
# Returns the path to the .pm file, or undef if not found
sub _find_module_file {
  my ($self, $module) = @_;

  # Convert Module::Name to Module/Name.pm
  my $file = $module;
  $file =~ s/::/\//g;
  $file .= '.pm';

  for my $inc (@{$self->inc_paths}) {
    my $path = "$inc/$file";
    return $path if -f $path;
  }

  return undef;
}


# Extract prototypes from a module by parsing it at transpile time.
# Returns the Environment from parsing, or undef on failure.
#
# NOTE: This is RECURSIVE. When the module being parsed contains 'use'
# statements, those trigger _extract_module_prototypes() calls for their
# modules, and so on. The _parsing_modules hash (shared across all
# recursive calls) prevents infinite loops from circular dependencies.
# Results are memoized in a state hash to avoid re-parsing modules.
sub _extract_module_prototypes {
  my ($self, $module) = @_;

  # Memoization cache (persists across all calls)
  state $cache = {};

  # Return cached result if already parsed
  return $cache->{$module} if exists $cache->{$module};

  # Skip known core modules that don't have prototypes affecting codegen
  if ($module =~ /^(Test2?::|Carp|Scalar::Util|List::Util|Time::HiRes|
                    XSLoader|DynaLoader|Exporter|base|parent|strict|warnings|
                    utf8|bytes|overload|mro|B::|POSIX|File::|IO::|Data::Dumper)/x) {
    return $cache->{$module} = undef;
  }

  # Cycle detection
  return undef if $self->_parsing_modules->{$module};
  local $self->_parsing_modules->{$module} = 1;

  my $module_path = $self->_find_module_file($module);
  return $cache->{$module} = undef unless $module_path;

  my $doc = PPI::Document->new($module_path);
  return $cache->{$module} = undef unless $doc;

  my $module_env = Pl::Environment->new();

  my $module_parser = Pl::Parser->new(
    filename                => $module_path,
    environment             => $module_env,
    inc_paths               => $self->inc_paths,
    _parsing_modules        => $self->_parsing_modules,
    collect_prototypes_only => 1,
  );

  # parse() may recursively call _extract_module_prototypes() for any
  # 'use' statements found in the module
  eval { $module_parser->parse($doc) };
  if ($@) {
    warn "Failed to extract prototypes from $module: $@";
    return $cache->{$module} = undef;
  }

  return $cache->{$module} = $module_env;
}


# Merge prototypes from another environment (only exported ones)
sub _merge_module_prototypes {
  my ($self, $module_env, $imports) = @_;

  # If specific imports requested, only import those
  if ($imports && @$imports) {
    for my $name (@$imports) {
      my $proto = $module_env->get_prototype($name);
      if ($proto) {
        $self->environment->add_prototype($name, $proto);
      }
    }
    return;
  }

  # Otherwise import @EXPORT (we'd need to track this in Environment)
  # For now, import all prototypes that affect code generation:
  # - has_block_arg: requires &{} wrapping
  # - reference params (\@, \%, \$): require auto-boxing
  # This is a simplification - full implementation would track @EXPORT
  for my $name (keys %{$module_env->prototypes}) {
    my $proto = $module_env->get_prototype($name);
    next unless $proto;

    # Check if this prototype affects code generation
    my $needs_import = 0;
    $needs_import = 1 if $proto->{has_block_arg};

    # Check for reference parameters (proto_type starts with \)
    if ($proto->{params} && @{$proto->{params}}) {
      for my $param (@{$proto->{params}}) {
        my $ptype = $param->{proto_type} // $param->{name};
        if ($ptype && $ptype =~ /^\\/) {
          $needs_import = 1;
          last;
        }
      }
    }

    if ($needs_import) {
      $self->environment->add_prototype($name, $proto);
    }
  }
}


# Parse import list from use statement (e.g., qw(foo bar) or ('foo', 'bar'))
sub _parse_use_import_list {
  my ($self, $stmt) = @_;
  my @imports;

  for my $child ($stmt->schildren) {
    if ($child->isa('PPI::Token::QuoteLike::Words')) {
      # qw(foo bar baz)
      my $content = $child->content;
      $content =~ s/^qw\s*[\(\[\{<]//;
      $content =~ s/[\)\]\}>]$//;
      push @imports, split /\s+/, $content;
    }
    elsif ($child->isa('PPI::Structure::List')) {
      # ('foo', 'bar') import list
      for my $item ($child->schildren) {
        if ($item->isa('PPI::Statement::Expression')) {
          for my $expr_child ($item->schildren) {
            if ($expr_child->isa('PPI::Token::Quote')) {
              push @imports, $expr_child->string;
            }
          }
        }
        elsif ($item->isa('PPI::Token::Quote')) {
          push @imports, $item->string;
        }
      }
    }
  }

  return grep { defined $_ && $_ ne '' } @imports;
}


# Process 'use vars' - declare package globals with defvar
# use vars '@foo', use vars qw($a @b %c)
sub _process_use_vars {
  my ($self, $stmt, $perl_code) = @_;

  # Collect variable names from the argument list
  # Handles: use vars '@foo'       (single string)
  #          use vars qw(@a $b %c) (qw() list)
  #          use vars ('@a', '$b') (list)
  my @vars;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::QuoteLike::Words') {
      # qw(@a $b)
      my $content = $child->content;
      $content =~ s/^qw[^\w\s]//;  # strip leading qw(
      $content =~ s/[^\w\s]$//;    # strip trailing )
      push @vars, split /\s+/, $content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # ('@a', '$b')
      for my $item ($child->children) {
        next if ref($item) =~ /Whitespace|Separator/;
        if (ref($item) eq 'PPI::Token::Quote::Single' || ref($item) eq 'PPI::Token::Quote::Double') {
          push @vars, $item->string;
        }
      }
    }
    elsif ($ref eq 'PPI::Token::Quote::Single' || $ref eq 'PPI::Token::Quote::Double') {
      # use vars '@foo'  (single arg as string)
      push @vars, $child->string;
    }
  }

  # Filter to actual sigiled variables
  @vars = grep { /^[\$\@\%]/ } @vars;
  return unless @vars;

  my $pkg = $self->environment->current_package;
  for my $var (@vars) {
    $self->environment->add_our_variable($pkg, $var);
  }

  # Route defvars to declarations bucket
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $sigil = substr($var, 0, 1);
      my $name = substr($var, 1);
      my $cl_var;
      if ($sigil eq '$') {
        $cl_var = "\$$name";
      } elsif ($sigil eq '@') {
        $cl_var = "\@$name";
      } else {
        $cl_var = "\%$name";
      }
      my $init = $sigil eq '$' ? '(make-p-box nil)'
               : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
               :                 '(make-hash-table :test #\'equal)';
      $self->_emit("(p-eval-direct");
      $self->_emit("  (defvar $cl_var $init))");
    }
    $self->_emit("");
  });
}


# Process 'use constant' declarations
sub _process_use_constant {
  my $self      = shift;
  my $stmt      = shift;
  my $perl_code = shift;

  # Get children after 'constant'
  my @children = $stmt->schildren;
  # Skip: 'use', 'constant'
  my $i = 0;
  while ($i < @children && $children[$i]->content ne 'constant') {
    $i++;
  }
  $i++;  # Skip 'constant' itself

  # What's next determines the form
  return if $i >= @children;

  my $next = $children[$i];

  if ($next->isa('PPI::Structure::Constructor')) {
    # Hash-style: use constant { PI => 3.14, E => 2.71 };
    $self->_process_constant_hash($next, $perl_code);
  }
  elsif ($next->isa('PPI::Token::Word')) {
    # Single: use constant PI => 3.14;
    my $name = $next->content;
    # Get the value (skip => operator)
    my @value_parts;
    for my $j ($i+1 .. $#children) {
      my $child = $children[$j];
      next if $child->isa('PPI::Token::Operator') && $child->content eq '=>';
      next if $child->isa('PPI::Token::Structure');  # Skip ;
      push @value_parts, $child;
    }
    $self->_process_single_constant($name, \@value_parts, $perl_code);
  }
}


# Process hash-style constant declaration
sub _process_constant_hash {
  my $self      = shift;
  my $struct    = shift;
  my $perl_code = shift;

  # Constants are compile-time — route to definitions bucket
  $self->_with_bucket('definitions', sub {
    $self->_emit(";; $perl_code");

    # Get the expression inside the braces
    my @contents = $struct->schildren;

    for my $content (@contents) {
      next unless $content->isa('PPI::Statement::Expression');

      # Parse the expression children for name => value pairs
      my @parts = $content->schildren;
      my $i = 0;
      while ($i < @parts) {
        # Get name
        my $name_tok = $parts[$i];
        last unless $name_tok && $name_tok->isa('PPI::Token::Word');
        my $name = $name_tok->content;
        $i++;

        # Skip =>
        $i++ while $i < @parts && $parts[$i]->isa('PPI::Token::Operator') && $parts[$i]->content eq '=>';

        # Collect value tokens until , or end
        my @value_parts;
        while ($i < @parts) {
          my $part = $parts[$i];
          last if $part->isa('PPI::Token::Operator') && $part->content eq ',';
          push @value_parts, $part;
          $i++;
        }

        # Skip comma
        $i++ if $i < @parts && $parts[$i]->isa('PPI::Token::Operator') && $parts[$i]->content eq ',';

        # Process this constant
        $self->_emit_constant($name, \@value_parts);
      }
    }
    $self->_emit("");
  });
}


# Process single constant declaration
sub _process_single_constant {
  my $self        = shift;
  my $name        = shift;
  my $value_parts = shift;
  my $perl_code   = shift;

  # Constants are compile-time — route to definitions bucket
  $self->_with_bucket('definitions', sub {
    $self->_emit(";; $perl_code");
    $self->_emit_constant($name, $value_parts);
    $self->_emit("");
  });
}


# Emit a single constant definition
# Constants are implemented as zero-arg functions (like Perl does internally)
sub _emit_constant {
  my $self        = shift;
  my $name        = shift;
  my $value_parts = shift;

  # Compile the value expression to CL
  my $cl_value = $self->_compile_constant_value($value_parts);

  # Emit as a function (Perl implements constants as subs)
  # Use p-sub for compile-time visibility (BEGIN blocks can use constants)
  my $cl_sub_name = $self->_qualified_sub_to_cl($name);
  $self->_emit("(p-sub $cl_sub_name () $cl_value)");

  # Register as a zero-arg prototype so bareword is recognized as function call
  $self->environment->add_prototype($name, {
    params     => [],
    min_params => 0,
    is_proto   => 0,
  });
}


# Compile a constant's value expression to CL
sub _compile_constant_value {
  my $self  = shift;
  my $parts = shift;

  # Simple case: single literal
  if (@$parts == 1) {
    my $tok = $parts->[0];
    if ($tok->isa('PPI::Token::Number')) {
      return $tok->content;
    }
    elsif ($tok->isa('PPI::Token::Quote')) {
      # String - get the actual string value
      my $str = $tok->string // $tok->content;
      return '"' . $str . '"';
    }
  }

  # Complex expression: use PExpr to parse
  my $result;
  eval {
    my $expr_o = Pl::PExpr->new(
      e           => $parts,
      environment => $self->environment,
      parser      => $self,
    );
    my $node_id = $expr_o->parse_expr_to_tree($parts);
    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => 0,
    );
    $result = $gen->generate($node_id);
  };

  die $@ if $@ && $@ =~ /^PCL:/;
  return $result // '0';  # Fallback
}


# Parse an expression using PExpr and generate CL
sub _parse_expression {
  my $self  = shift;
  my $parts = shift;
  my $stmt  = shift;  # Original statement for full_PPI

  # Call the internal version that returns declarations too
  my ($result, $decls) = $self->_parse_expression_internal($parts, $stmt);

  # In scalar context, just return result (backwards compatible)
  return $result unless wantarray;

  # In list context, return result and declarations
  return ($result, $decls);
}

# Internal: parse expression and return both CL code and declarations
sub _parse_expression_internal {
  my $self  = shift;
  my $parts = shift;
  my $stmt  = shift;

  my $result;
  my @decls;

  eval {
    my $expr_o = Pl::PExpr->new(
      e           => $parts,
      full_PPI    => $stmt,
      environment => $self->environment,
      parser      => $self,
    );

    # Capture declarations in list context
    my ($node_id, $decl_list) = $expr_o->parse_expr_to_tree($parts);
    @decls = @{$decl_list // []};

    # Annotate AST with context information (scalar/list)
    $expr_o->annotate_contexts($node_id);

    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => $self->indent_level,
    );

    $result = $gen->generate($node_id);
  };

  if ($@) {
    my $error = $@;
    # Hard errors (e.g. unsupported features) must propagate — don't swallow.
    die $error if $error =~ /^PCL:/;
    $error =~ s/ at \/.*//s;  # Remove file/line info
    $error =~ s/\n.*//s;      # First line only
    return ("(progn ;; PARSE ERROR: $error\n nil)", []);
  }

  return ($result // ";; (no output)", \@decls);
}


# Emit a line to output
sub _emit {
  my $self = shift;
  my $line = shift;

  # Don't emit if we're just extracting prototypes
  return if $self->collect_prototypes_only;

  # For let-bound 'my' variables, replace (p-scalar-= $var ...) with
  # (p-my-= $var ...) to avoid p-scalar-='s (proclaim 'special) side-effect.
  # proclaim at runtime contaminates future compilations: the next time code
  # using the same name is compiled, the let creates a dynamic binding instead
  # of a lexical one, breaking closure capture.
  # p-my-= is a semantic macro (expands to box-set) that expresses intent for
  # other compiler backends reading the generated IR.
  if ($line && $self->{_let_bound_vars}) {
    for my $var (keys %{$self->{_let_bound_vars}}) {
      my $pat = quotemeta("(p-scalar-= $var");
      $line =~ s/$pat(?=[\s)])/(p-my-= $var/g;
    }
  }

  my $indent = "  " x $self->indent_level;
  my $section = $self->_sections->[$self->_cur_section];
  push @{$section->{$self->_cur_bucket}}, $indent . $line;
}


# Convenience class methods
#
# These use two-pass parsing:
# 1. First pass with collect_prototypes_only => 1 to find all 'use' statements
#    and extract prototypes from them (recursively). This ensures prototypes
#    are known even if 'use' appears after code that calls the imported subs.
# 2. Second pass for real transpilation, with prototypes already in environment.

sub parse_file {
  my $class    = shift;
  my $filename = shift;
  my %opts     = @_;

  # First pass: collect prototypes from all 'use'd modules
  my $proto_parser = $class->new(
    filename                => $filename,
    collect_prototypes_only => 1,
    %opts,
  );
  $proto_parser->parse;

  # Second pass: transpile with prototypes already known
  my $parser = $class->new(
    filename    => $filename,
    environment => $proto_parser->environment,
    %opts,
  );
  return $parser->parse;
}


sub parse_code {
  my $class = shift;
  my $code  = shift;
  my %opts  = @_;

  # First pass: collect prototypes
  my $proto_parser = $class->new(
    code                    => $code,
    collect_prototypes_only => 1,
    %opts,
  );
  $proto_parser->parse;

  # Second pass: transpile with prototypes already known
  my $parser = $class->new(
    code        => $code,
    environment => $proto_parser->environment,
    %opts,
  );
  return $parser->parse;
}


# ============================================================
# Parse a subroutine prototype or signature string.
#
# Input: prototype string like "($x, $y = 10, @rest)" or "($$;$)"
# Returns: {
#   params     => [ { name => '$x', default_cl => undef },
#                   { name => '$y', default_cl => '10' },
#                   { name => '@rest', default_cl => undef } ],
#   min_params => 1,   # minimum required parameters
#   is_proto   => 0,   # 1 if old-style prototype, 0 if signature
# }
#
# This is a separate sub so it can be moved to its own module later.
# ============================================================
sub parse_prototype_or_signature {
  my $self      = shift;
  my $proto_str = shift;
  my $context   = shift;  # PPI context for parsing defaults (e.g., the sub statement)

  # Remove surrounding parens and whitespace
  $proto_str =~ s/^\s*\(\s*//;
  $proto_str =~ s/\s*\)\s*$//;

  return { params => [], min_params => 0, is_proto => 0 } if $proto_str eq '';

  # Detect if this is an old-style prototype (no variable names, just sigils)
  # Old-style: ($$), (\@$), ($;$@)
  # New-style: ($x, $y), ($x = 10)
  my $is_proto = ($proto_str !~ /[\$\@\%]\w/);

  if ($is_proto) {
    return $self->_parse_old_prototype($proto_str);
  } else {
    return $self->_parse_signature($proto_str, $context);
  }
}


# Parse old-style prototype like "$$", "\@$", "$;$$"
sub _parse_old_prototype {
  my $self      = shift;
  my $proto_str = shift;

  my @params;
  my $min_params = 0;
  my $in_optional = 0;
  my $param_idx = 0;  # Counter for unique parameter names

  # Split into characters, handling backslash escapes
  my $i = 0;
  while ($i < length($proto_str)) {
    my $char = substr($proto_str, $i, 1);

    if ($char eq ';') {
      # Semicolon marks start of optional parameters
      $in_optional = 1;
      $i++;
      next;
    }
    elsif ($char eq '\\') {
      # Reference type: \@, \$, \%, \*
      my $next = substr($proto_str, $i + 1, 1);
      my $name = '$_proto_arg' . $param_idx++;
      push @params, {
        name => $name,
        default_cl => undef,
        proto_type => "\\$next"  # Preserve original for auto-boxing
      };
      $min_params++ unless $in_optional;
      $i += 2;
    }
    elsif ($char =~ /[\$\@\%\&\*_]/) {
      # Generate unique name with appropriate sigil
      my $sigil = ($char eq '@' || $char eq '%') ? $char : '$';
      my $name = $sigil . '_proto_arg' . $param_idx++;
      push @params, {
        name => $name,
        default_cl => undef,
        proto_type => $char  # Preserve original for special handling
      };
      $min_params++ unless $in_optional || $char eq '@' || $char eq '%';
      $i++;
    }
    else {
      # Skip unknown/whitespace
      $i++;
    }
  }

  # Check if prototype has & (block argument)
  my $has_block_arg = ($proto_str =~ /&/);

  return {
    params        => \@params,
    min_params    => $min_params,
    is_proto      => 1,
    has_block_arg => $has_block_arg,
    proto_string  => $proto_str,
  };
}


# Parse new-style signature like "$x, $y = 10, @rest"
sub _parse_signature {
  my $self      = shift;
  my $sig_str   = shift;
  my $context   = shift;

  my @params;
  my $min_params = 0;
  my $seen_optional = 0;

  # Split on commas, but be careful with nested parens in defaults
  my @param_strs = $self->_split_signature_params($sig_str);

  for my $param_str (@param_strs) {
    $param_str =~ s/^\s+//;
    $param_str =~ s/\s+$//;
    next if $param_str eq '';

    my ($name, $default_expr);

    if ($param_str =~ /^([\$\@\%]\w+)\s*=\s*(.+)$/) {
      # Parameter with default: $x = 10
      $name = $1;
      $default_expr = $2;
      $seen_optional = 1;
    }
    elsif ($param_str =~ /^([\$\@\%]\w+)$/) {
      # Simple parameter: $x
      $name = $1;
      $default_expr = undef;
    }
    else {
      # Unknown format, skip
      next;
    }

    my $default_cl = undef;
    if (defined $default_expr) {
      # Compile the default expression to CL
      $default_cl = $self->_compile_default_expr($default_expr, $context);
    }

    push @params, {
      name       => $name,
      default_cl => $default_cl,
    };

    # Count mandatory params (before any optional, and not slurpy)
    if (!$seen_optional && !defined $default_expr && $name !~ /^[\@\%]/) {
      $min_params++;
    }
  }

  return {
    params     => \@params,
    min_params => $min_params,
    is_proto   => 0,
  };
}


# Split signature string on commas, respecting nested parens
sub _split_signature_params {
  my $self = shift;
  my $str  = shift;

  my @result;
  my $current = '';
  my $depth = 0;

  for my $char (split //, $str) {
    if ($char eq ',' && $depth == 0) {
      push @result, $current;
      $current = '';
    }
    else {
      $depth++ if $char eq '(' || $char eq '[' || $char eq '{';
      $depth-- if $char eq ')' || $char eq ']' || $char eq '}';
      $current .= $char;
    }
  }
  push @result, $current if $current ne '';

  return @result;
}


# Compile a default expression to CL
sub _compile_default_expr {
  my $self    = shift;
  my $expr    = shift;
  my $context = shift;

  my $result;
  eval {
    my $doc = PPI::Document->new(\$expr);
    my @stmts = $doc->children;
    return undef unless @stmts;

    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace'
    } $stmts[0]->children;

    return undef unless @parts;

    my $expr_o = Pl::PExpr->new(
      e           => \@parts,
      full_PPI    => $doc,
      environment => $self->environment,
      parser      => $self,
    );

    my $node_id = $expr_o->parse_expr_to_tree(\@parts);

    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => 0,
    );

    $result = $gen->generate($node_id);
  };

  if ($@) {
    die $@ if $@ =~ /^PCL:/;
    warn "Failed to compile default expression '$expr': $@";
    return undef;
  }

  return $result;
}


1;
