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
    die "Failed to parse file: " . $self->filename unless $doc;
    return $doc;
  }
  elsif ($self->has_code) {
    my $code = $self->code;
    my $doc = PPI::Document->new(\$code);
    die "Failed to parse code" unless $doc;
    return $doc;
  }
  else {
    die "Must provide either 'filename' or 'code'";
  }
}


# Main entry point: parse and generate CL
sub parse {
  my $self = shift;

  my $doc = $self->ppi_doc;
  $self->output([]);

  # Always start with in-package :pcl
  $self->_emit("(in-package :pcl)");
  $self->_emit("");

  $self->_process_children($doc);

  # Insert pre-declarations for referenced but undeclared packages
  $self->_insert_package_predeclarations();

  # Insert forward declarations for subs
  $self->_insert_sub_forward_declarations();

  # Insert forward declarations for undeclared package variables
  $self->_insert_variable_forward_declarations();

  return join("\n", @{$self->output});
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

# Insert forward declarations and reorder compile-time forms before runtime.
#
# Two-phase approach:
#   Phase 1: Extract defvar blocks, emit defpackage/stubs after each in-package
#   Phase 2: Within each package section, reorder compile-time forms (subs,
#            eval-when, defclass, etc.) before runtime forms, preserving source
#            order within each group.
#
# This ensures:
#   - defvar proclamations precede defun (so `local` creates dynamic bindings)
#   - `use` imports are processed before sub definitions that reference them
#     (avoids CL symbol conflicts from premature interning)
#   - Sub definitions appear before runtime top-level calls
sub _insert_sub_forward_declarations {
  my $self = shift;

  # Skip if we're just collecting prototypes
  return if $self->collect_prototypes_only;

  my $subs = $self->environment->get_declared_subs();

  # Phase 1 requires declared subs; Phase 2 runs unconditionally
  # (use constant generates pl-sub without registering as declared sub)
  unless (@$subs) {
    $self->_reorder_compile_runtime_forms();
    return;
  }

  # Collect packages that need to be pre-declared for qualified subs
  my %needed_packages;

  # Group subs by package
  my %by_package;
  for my $sub (@$subs) {
    my $name = $sub->{name};
    # Normalize Perl's old-style ' package separator to ::
    $name =~ s/'/::/g;
    # Check if sub name is qualified (e.g., A::DESTROY)
    if ($name =~ /^(.+)::([^:]+)$/) {
      my $sub_pkg = $1;
      $needed_packages{$sub_pkg} = 1;
    }
    push @{$by_package{$sub->{package}}}, $name;
  }

  my $output = $self->output;

  # --- Phase 1: Extract defvars, determine stubs, scan for needed packages ---

  # Find top-level pl-sub names (to determine which subs need stubs)
  my %top_level_subs;  # "pkg::cl_name" => 1
  {
    my $cur_pkg = 'pcl';
    for my $i (0 .. $#$output) {
      my $line = $output->[$i];
      if ($line =~ /^\(in-package\s+:([^\)]+)\)/) {
        $cur_pkg = $1;
        $cur_pkg =~ s/^\|//;
        $cur_pkg =~ s/\|$//;
      }
      if ($line =~ /^\(pl-sub\s+(\S+)\s/) {
        $top_level_subs{"$cur_pkg\::$1"} = 1;
      }
    }
  }

  # Scan full output for package references that need pre-declaration.
  # When a form references a package (e.g., TestMod::pl-get_value), the
  # package must exist before the CL reader encounters it.
  for my $line (@$output) {
    # Simple package: TestMod::pl-get_value
    while ($line =~ /\b([A-Z][A-Za-z0-9_]+)::/g) {
      my $pkg = $1;
      next if $pkg eq 'PCL';
      $needed_packages{$pkg} = 1;
    }
    # Pipe-quoted package: |Foo::Bar|::pl-something
    while ($line =~ /\|([^|]+)\|::/g) {
      my $pkg = $1;
      $needed_packages{$pkg} = 1;
    }
  }

  # Extract (eval-when ... (defvar ...)) blocks from the output.
  # CL's defvar proclaims a variable as "special" (dynamically scoped).
  # This proclamation must happen BEFORE any defun that uses `let` to bind
  # the variable, otherwise `let` creates a lexical (not dynamic) binding,
  # breaking Perl's `local` dynamic scoping semantics.
  my %defvar_blocks;  # cl_pkg_name => [ lines... ]
  {
    my $dvpkg = 'pcl';
    my @dv_removals;  # [ [start, end], ... ]
    for my $j (0 .. $#$output) {
      if ($output->[$j] =~ /^\(in-package\s+:([^\)]+)\)/) {
        $dvpkg = $1;
        $dvpkg =~ s/^\|//;
        $dvpkg =~ s/\|$//;
      }
      # Match 2-line eval-when/defvar blocks
      if ($output->[$j] =~ /^\(eval-when\s+\(:compile-toplevel/ &&
          $j + 1 <= $#$output &&
          $output->[$j + 1] =~ /^\s+\(defvar\s/) {
        push @{$defvar_blocks{$dvpkg}}, $output->[$j], $output->[$j + 1];
        push @dv_removals, [$j, $j + 1];
      }
    }
    # Remove in reverse order
    for my $r (reverse @dv_removals) {
      splice @$output, $r->[0], $r->[1] - $r->[0] + 1;
    }
  }

  # Find (in-package ...) lines and insert defvars + defpackages + stubs
  my @insertions;  # [ [position, lines_to_insert], ... ]

  for my $i (0 .. $#$output) {
    my $line = $output->[$i];
    if ($line =~ /^\(in-package\s+:([^\)]+)\)/) {
      my $pkg_spec = $1;
      # Extract package name: :pcl -> pcl, :|Foo::Bar| -> Foo::Bar
      my $cl_pkg_name = $pkg_spec;
      $cl_pkg_name =~ s/^\|//;
      $cl_pkg_name =~ s/\|$//;

      # Map CL package to Perl package for by_package lookup
      my $pkg_name = $cl_pkg_name eq 'pcl' ? 'main' : $cl_pkg_name;

      # We insert if there are subs OR defvars for this package
      my $sub_names = $by_package{$pkg_name};
      my $defvars = delete $defvar_blocks{$cl_pkg_name};

      if ($sub_names || $defvars) {
        my @decls;

        # First, declare any packages needed for qualified sub names
        # Pipe-quote if contains :: or conflicts with CL symbols
        for my $pkg (sort keys %needed_packages) {
          my $cl_pkg = ($pkg =~ /::/ || lc($pkg) eq 'class' ||
                        lc($pkg) eq 'method' || lc($pkg) eq 'function')
                       ? ":|$pkg|" : ":$pkg";
          push @decls, "(defpackage $cl_pkg (:use :cl :pcl))";
        }
        # Clear so we don't emit again
        %needed_packages = ();

        # Insert defvar declarations so variables are proclaimed special
        # before any let bindings in subs
        if ($defvars) {
          push @decls, @$defvars;
        }

        # Emit stubs only for subs NOT found at top level (nested subs).
        # Top-level subs will be reordered before runtime code by Phase 2.
        if ($sub_names) {
          my %seen;
          my @unique_names = sort grep { !$seen{$_}++ } @$sub_names;

          my @stubs;
          for my $name (@unique_names) {
            my $cl_name = $self->_qualified_sub_to_cl($name);
            my $body_key = "$cl_pkg_name\::$cl_name";
            unless ($top_level_subs{$body_key}) {
              push @stubs, $cl_name;
            }
          }
          if (@stubs) {
            for my $cl_name (@stubs) {
              push @decls, "(pl-declare-sub $cl_name)";
            }
          }
          delete $by_package{$pkg_name};
        }

        push @decls, "";
        push @insertions, [$i + 1, \@decls];
      }
    }
  }

  # Insert in reverse order so positions remain valid
  for my $ins (reverse @insertions) {
    my ($pos, $lines) = @$ins;
    splice @$output, $pos, 0, @$lines;
  }

  # --- Phase 2: Reorder compile-time forms before runtime forms ---
  # Within each package section, move compile-time forms (subs, eval-when,
  # defclass, etc.) before runtime forms, preserving source order within
  # each group. This ensures `use` imports are processed before sub defs
  # that reference imported functions, while still keeping subs before
  # runtime top-level calls.
  $self->_reorder_compile_runtime_forms();
}


# Reorder output lines so compile-time forms appear before runtime forms
# within each package section, preserving source order within each group.
sub _reorder_compile_runtime_forms {
  my $self = shift;
  my $output = $self->output;

  # Find package section boundaries (in-package lines)
  my @section_starts;
  for my $i (0 .. $#$output) {
    if ($output->[$i] =~ /^\(in-package\s/) {
      push @section_starts, $i;
    }
  }

  return unless @section_starts;

  # Process all sections. The pcl section contains user code when no
  # preamble is present (parse_code), and pl2cl injects its preamble
  # after parsing, so we must reorder the pcl section too.
  for my $s (0 .. $#section_starts) {
    my $start = $section_starts[$s] + 1;  # skip in-package line itself
    my $end = ($s < $#section_starts)
            ? $section_starts[$s + 1] - 1
            : $#$output;

    next if $start > $end;

    # Parse section into chunks (comment + form + trailing blanks)
    my @chunks = _parse_output_chunks($output, $start, $end);

    # Classify and partition into compile-time vs runtime
    my (@compile_time, @runtime);
    for my $chunk (@chunks) {
      if ($chunk->{first_form} eq '') {
        # Blank/comment-only chunk: attach to whichever group has items,
        # defaulting to compile_time (preamble)
        if (@runtime) {
          push @runtime, $chunk;
        } else {
          push @compile_time, $chunk;
        }
      } elsif (_is_compile_time_form($chunk)) {
        push @compile_time, $chunk;
      } else {
        push @runtime, $chunk;
      }
    }

    # Skip if no reordering needed
    next unless @compile_time && @runtime;

    # Check if already in correct order (compile-time all before runtime)
    my $last_ct_line = 0;
    my $first_rt_line = $end + 1;
    for my $chunk (@compile_time) {
      my $pos = $chunk->{start_pos};
      $last_ct_line = $pos if $pos > $last_ct_line;
    }
    for my $chunk (@runtime) {
      my $pos = $chunk->{start_pos};
      $first_rt_line = $pos if $pos < $first_rt_line;
    }
    next if $last_ct_line < $first_rt_line;

    # Reassemble: compile-time chunks first, then runtime chunks
    my @new_lines;
    for my $chunk (@compile_time, @runtime) {
      push @new_lines, @{$chunk->{lines}};
    }

    # Replace section in output
    my $old_len = $end - $start + 1;
    splice @$output, $start, $old_len, @new_lines;

    # Update section_starts for subsequent sections
    my $delta = scalar(@new_lines) - $old_len;
    for my $j ($s + 1 .. $#section_starts) {
      $section_starts[$j] += $delta;
    }
  }
}


# Parse output lines[$start..$end] into chunks.
# Each chunk = { lines => [...], first_form => "...", start_pos => N }
# A chunk is: optional leading blanks/comments + paren-balanced form + trailing blanks
sub _parse_output_chunks {
  my ($output, $start, $end) = @_;
  my @chunks;
  my $i = $start;

  while ($i <= $end) {
    my @chunk_lines;
    my $first_form = '';
    my $chunk_start = $i;

    # Collect leading blank and comment lines
    while ($i <= $end && ($output->[$i] =~ /^\s*$/ || $output->[$i] =~ /^;;/)) {
      push @chunk_lines, $output->[$i];
      $i++;
    }

    # If no form follows, save comments/blanks as a chunk
    if ($i > $end) {
      push @chunks, { lines => \@chunk_lines, first_form => '', start_pos => $chunk_start }
        if @chunk_lines;
      last;
    }

    # Collect the paren-balanced form
    $first_form = $output->[$i];
    my $depth = 0;
    while ($i <= $end) {
      my $line = $output->[$i];
      push @chunk_lines, $line;

      my $clean = $line;
      $clean =~ s/;.*//;         # strip comments
      $clean =~ s/"[^"]*"//g;    # strip strings (simple)
      $depth += ($clean =~ tr/(//);
      $depth -= ($clean =~ tr/)//);

      $i++;
      last if $depth <= 0;
    }

    # Collect trailing blank lines
    while ($i <= $end && $output->[$i] =~ /^\s*$/) {
      push @chunk_lines, $output->[$i];
      $i++;
    }

    push @chunks, { lines => \@chunk_lines, first_form => $first_form, start_pos => $chunk_start };
  }

  return @chunks;
}


# Classify whether a chunk is a compile-time form.
# Compile-time forms include sub definitions, use/require (eval-when with
# :load-toplevel), defpackage, defclass, forward stubs, and END block
# registrations. BEGIN blocks (eval-when WITHOUT :load-toplevel) stay in
# source order — they should run sequentially in interpreted mode.
sub _is_compile_time_form {
  my ($chunk) = @_;
  my $line = $chunk->{first_form};
  return 0 unless $line;

  # Direct compile-time forms
  return 1 if $line =~ /^\(pl-sub\s/;

  # eval-when: only reorder use/defvar (has :load-toplevel).
  # BEGIN blocks use (:compile-toplevel :execute) without :load-toplevel
  # and should stay in source order for interpreted mode.
  # Bare `require './file.pl'` also gets eval-when :load-toplevel but must
  # stay in source order (e.g., chdir before require).
  if ($line =~ /^\(eval-when\s/) {
    return 0 unless $line =~ /:load-toplevel/;
    # Bare require with file path — keep in source order
    for my $l (@{$chunk->{lines}}) {
      return 0 if $l =~ /\bpl-require-file\b/;
    }
    return 1;
  }

  return 1 if $line =~ /^\(defpackage\s/;
  return 1 if $line =~ /^\(in-package\s/;
  return 1 if $line =~ /^\(defclass\s/;
  return 1 if $line =~ /^\(defconstant\s/;
  return 1 if $line =~ /^\(pl-declare-sub\s/;
  return 1 if $line =~ /^\(unless\s+\(fboundp\s/;
  return 1 if $line =~ /^\(push \(lambda/;   # END blocks

  # let-wrapped subs (state variable closures)
  if ($line =~ /^\(let[\s*]/) {
    for my $l (@{$chunk->{lines}}) {
      return 1 if $l =~ /\(pl-sub\s/;
    }
  }

  return 0;
}


# Insert defvar for package variables used without my/our declaration.
# In Perl, package globals auto-vivify as undef. In CL, unbound symbols crash.
# We scan the generated output for variable references, subtract those already
# declared (defvar) or locally bound (let/foreach), and emit defvar for the rest.
sub _insert_variable_forward_declarations {
  my $self = shift;

  return if $self->collect_prototypes_only;

  my $output = $self->output;

  # Variables defined in the pcl runtime (inherited via :use :pcl)
  my %runtime_vars = map { $_ => 1 } qw(
    $_ @_ %_args @ARGV @INC %ENV %INC
    $1 $2 $3 $4 $5 $6 $7 $8 $9
  );

  my %declared;    # variables with defvar
  my %let_bound;   # variables bound by let/let*/foreach
  my %referenced;  # all variable references

  for my $line (@$output) {
    # Skip comment lines
    next if $line =~ /^\s*;;/;

    # Collect defvar'd variables: (defvar $var ...)
    if ($line =~ /\(defvar\s+([\$\@\%][a-zA-Z_]\w*)\b/) {
      $declared{$1} = 1;
    }

    # Collect let/let*-bound variables.
    # Generated patterns:  (let (($x (make-pl-box nil)) (@arr ...) (%h ...))
    # Each binding is ($var init), so match ( followed by variable followed by space
    if ($line =~ /\(let\*?\s+\(/) {
      while ($line =~ /\(([\$\@\%][a-zA-Z_]\w*)\s+/g) {
        $let_bound{$1} = 1;
      }
    }

    # Collect foreach-bound variables: (pl-foreach ($i ...)
    if ($line =~ /\(pl-foreach\s+\(([\$\@\%][a-zA-Z_]\w*)\b/) {
      $let_bound{$1} = 1;
    }

    # Collect all variable references (identifiers starting with sigil)
    while ($line =~ /([\$\@\%][a-zA-Z_]\w*)/g) {
      my $var = $1;
      next if $var =~ /::/;  # skip package-qualified
      $referenced{$var} = 1;
    }
  }

  # Undeclared = referenced - declared - let_bound - runtime
  my @undeclared;
  for my $var (sort keys %referenced) {
    next if $declared{$var};
    next if $let_bound{$var};
    next if $runtime_vars{$var};
    push @undeclared, $var;
  }

  return unless @undeclared;

  # Find insertion point: after the first (in-package ...) line.
  # When pl2cl adds a preamble with (in-package :main), it goes between
  # (in-package :pcl) and our code, so these defvars end up in the right package.
  my $insert_pos;
  for my $i (0 .. $#$output) {
    if ($output->[$i] =~ /^\(in-package\s+/) {
      $insert_pos = $i + 1;
      last;
    }
  }
  return unless defined $insert_pos;

  # Skip past sub forward declarations and blank lines
  while ($insert_pos < @$output &&
         ($output->[$insert_pos] =~ /^\(unless\s|^;; Forward|^;; but top|^\s*$/)) {
    $insert_pos++;
  }

  # Build declarations
  my @decls;
  push @decls, ";; Forward declarations for package variables used without my/our.";
  push @decls, ";; Perl globals auto-vivify as undef; CL needs defvar to avoid crashes.";
  for my $var (@undeclared) {
    my $sigil = substr($var, 0, 1);
    if ($sigil eq '$') {
      push @decls, "(defvar $var (make-pl-box nil))";
    } elsif ($sigil eq '@') {
      push @decls, "(defvar $var (make-array 0 :adjustable t :fill-pointer 0))";
    } elsif ($sigil eq '%') {
      push @decls, "(defvar $var (make-hash-table :test 'equal))";
    }
  }
  push @decls, "";

  splice @$output, $insert_pos, 0, @decls;
}

# Insert defpackage forms for packages referenced but not declared
# This enables dynamic require inside functions to work
sub _insert_package_predeclarations {
  my $self = shift;

  # Skip if we're just collecting prototypes
  return if $self->collect_prototypes_only;

  my $pkgs = $self->environment->get_undeclared_packages();
  return unless @$pkgs;

  my @predecls;
  for my $pkg (@$pkgs) {
    # Pipe-quote if contains :: or conflicts with CL symbols
    my $cl_pkg = ($pkg =~ /::/ || lc($pkg) eq 'class' || lc($pkg) eq 'error' ||
                  lc($pkg) eq 'method' || lc($pkg) eq 'function')
                 ? ":|$pkg|" : ":$pkg";
    push @predecls, ";; Pre-declare package for dynamic loading";
    push @predecls, "(defpackage $cl_pkg (:use :cl :pcl))";
    push @predecls, "";
  }

  # Insert after line 1 (after "in-package :pcl" and blank line)
  splice @{$self->output}, 2, 0, @predecls;
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
  elsif ($ref eq 'PPI::Statement::End') {
    # __END__ - stop processing (ignore everything after)
    $self->_emit(";; __END__");
    return;
  }
  elsif ($ref eq 'PPI::Statement::Data') {
    # __DATA__ - stop processing (DATA filehandle not yet supported)
    $self->_emit(";; __DATA__ (DATA filehandle not implemented)");
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
    # Note: 'for' and 'foreach' modifiers use pl-foreach (iterate over list),
    # not pl-for (C-style for loop)
    my $cl_modifier = $modifier;
    if ($modifier eq 'for' || $modifier eq 'foreach') {
      $cl_modifier = 'foreach';
      # For foreach modifier, need ($_ list) syntax
      $cl_code = "(pl-foreach (\$_ $cond_cl) $expr_cl)";
    }
    else {
      $cl_code = "(pl-$cl_modifier $cond_cl $expr_cl)";
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

  # Handle top-level 'my' declarations - need pl-my for BEGIN block visibility
  # Inside subs, my uses regular let bindings (handled elsewhere)
  if ($declarator eq 'my' && $self->environment->in_subroutine == 0) {
    $self->_process_my_toplevel_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Check if this is a state declaration inside a sub
  my $is_state = ($declarator eq 'state');
  my $state_vars = $self->{_current_state_vars} // {};

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

    if ($ref eq 'PPI::Token::Symbol') {
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

  $self->_emit(";; $perl_code");

  # Wrap defvar in eval-when so variables exist at compile time for BEGIN blocks.
  # Separate declaration (compile-time) from initialization (runtime) to match Perl:
  # 'our $x = 1; BEGIN { $x = 2 }' → at runtime $x becomes 1 (init overwrites BEGIN)

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
        $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
        $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
        unless ($is_empty_list) {
          my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
          # Convert progn to pl-array-init for array initialization
          # This matches what ExprToCL does for @array = (...) assignments
          if ($init_cl =~ /^\(progn\s+(.+)\)$/) {
            $init_cl = "(pl-array-init $1)";
          }
          # Use pl-array-= to properly populate array from list
          $self->_emit("(pl-array-= $var $init_cl)");
        }
      }
      elsif ($sigil eq '%') {
        # Hash: declare at compile time, initialize at runtime
        $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
        $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
        unless ($is_empty_list) {
          my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
          # Convert progn to pl-hash for hash initialization
          # This matches what ExprToCL does for %hash = (...) assignments
          if ($init_cl =~ /^\(progn\s+(.+)\)$/) {
            $init_cl = "(pl-hash $1)";
          }
          # Use pl-hash-= to properly populate hash from list
          $self->_emit("(pl-hash-= $var $init_cl)");
        }
      }
      else {
        # Scalar: declare with nil box at compile time, set value at runtime
        my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
        $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
        $self->_emit("  (defvar $var (make-pl-box nil)))");
        $self->_emit("(setf (pl-box-value $var) $init_cl)");
      }
    }
    else {
      # Multiple variables: our ($x, $y) = (1, 2)
      # First declare all at compile time, then assign at runtime
      for my $var (@vars) {
        my $sigil = substr($var, 0, 1);
        $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
        if ($sigil eq '@') {
          $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
        } elsif ($sigil eq '%') {
          $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
        } else {
          $self->_emit("  (defvar $var (make-pl-box nil)))");
        }
      }
      # Now do the assignment at runtime
      my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
      my $vars_vector = "(vector " . join(" ", @vars) . ")";
      $self->_emit("(pl-list-= $vars_vector $init_cl)");
    }
  }
  else {
    # Bare declaration: our $x; or our @arr; or our %hash;
    # Just declare at compile time, no runtime init
    for my $var (@vars) {
      my $sigil = substr($var, 0, 1);
      $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
      if ($sigil eq '@') {
        $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
      } elsif ($sigil eq '%') {
        $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
      } else {
        $self->_emit("  (defvar $var (make-pl-box nil)))");
      }
    }
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

    if ($ref eq 'PPI::Token::Symbol') {
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

  $self->_emit(";; $perl_code");

  # Emit compile-time declaration for all variables
  for my $var (@vars) {
    my $sigil = substr($var, 0, 1);
    $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
    if ($sigil eq '@') {
      $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
    } elsif ($sigil eq '%') {
      $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
    } else {
      $self->_emit("  (defvar $var (make-pl-box nil)))");
    }
  }

  # Handle initialization at runtime
  if ($init_idx >= 0) {
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

    # Check for empty list initializer ()
    my $is_empty_list = (@rhs_parts == 1 &&
                         ref($rhs_parts[0]) eq 'PPI::Structure::List' &&
                         $self->_is_empty_structure($rhs_parts[0]));

    unless ($is_empty_list) {
      if (@vars == 1) {
        my $var = $vars[0];
        my $sigil = substr($var, 0, 1);

        if ($sigil eq '$') {
          # Scalar: parse RHS and use box-set to properly unbox source
          my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
          $self->_emit("(box-set $var $init_cl)");
        } else {
          # Array/hash: parse full statement through expression parser for proper list context
          # This generates (pl-array-= @arr (vector ...)) or (pl-hash-= %h (pl-hash ...))
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

  $self->_emit(";; $perl_code");

  if (@parents) {
    # Emit CLOS class with parent classes for MRO tracking
    my $cl_class = $self->_pkg_to_clos_class($pkg);
    my $parents_cl = join(' ', map { $self->_pkg_to_clos_class($_) } @parents);

    # Store parent list in environment for later use
    $self->environment->set_isa($pkg, \@parents);

    # Redefine the CLOS class with parents (overwrites the empty one from preamble)
    $self->_emit(";; Redefine CLOS class with parents for MRO");
    $self->_emit("(defclass $cl_class ($parents_cl) ())");
  }

  # Also emit the standard array initialization for @ISA
  # This keeps @ISA available as an array at runtime
  $self->_emit("(defvar \@ISA (make-array 0 :adjustable t :fill-pointer 0))");
  for my $parent (@parents) {
    $self->_emit("(pl-push \@ISA \"$parent\")");
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

  # Find variable and optional initializer
  my @vars;
  my $init_idx = -1;

  for my $i (0 .. $#$parts) {
    my $p = $parts->[$i];
    my $ref = ref($p);

    if ($ref eq 'PPI::Token::Symbol') {
      push @vars, $self->_transform_pkg_var($p->content);
    }
    elsif ($ref eq 'PPI::Structure::List') {
      push @vars, map { $self->_transform_pkg_var($_) } $self->_find_symbols_in_list($p);
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
      push @bindings, "($var (make-array 0 :adjustable t :fill-pointer 0))";
    }
    elsif ($sigil eq '%') {
      push @bindings, "($var (make-hash-table :test 'equal))";
    }
    else {
      push @bindings, "($var (make-pl-box $init_cl))";
    }
  }
  else {
    # Bare local or multiple vars - just shadow with nil/empty
    for my $var (@vars) {
      my $sigil = substr($var, 0, 1);
      if ($sigil eq '@') {
        push @bindings, "($var (make-array 0 :adjustable t :fill-pointer 0))";
      }
      elsif ($sigil eq '%') {
        push @bindings, "($var (make-hash-table :test 'equal))";
      }
      else {
        push @bindings, "($var (make-pl-box nil))";
      }
    }
  }

  my $bindings_str = join("\n        ", @bindings);
  $self->_emit("(let ($bindings_str)");
  $self->indent_level($self->indent_level + 1);

  # Track that we have an open let that needs closing
  $self->{_local_let_depth} //= 0;
  $self->{_local_let_depth}++;

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
  my $init_expr;
  my $found_eq = 0;
  my @init_parts;

  for my $part (@$parts) {
    my $ref = ref($part);

    if ($ref eq 'PPI::Token::Word' && $part->content eq 'state') {
      next;  # Skip 'state' keyword
    }
    elsif ($ref eq 'PPI::Token::Symbol' && !$found_eq) {
      push @vars, $part->content;
    }
    elsif ($ref eq 'PPI::Token::Operator' && $part->content eq '=') {
      $found_eq = 1;
    }
    elsif ($found_eq) {
      push @init_parts, $part;
    }
  }

  # Parse the initializer expression
  my $init_cl = 'nil';
  if (@init_parts) {
    $init_cl = $self->_parse_expression(\@init_parts, $stmt) // 'nil';
  }

  $self->_emit(";; $perl_code");

  # Generate init guard for each state variable
  for my $var (@vars) {
    my $init_flag = "$var--init";
    $self->_emit("(unless $init_flag");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(setf $var (ensure-boxed $init_cl))");
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
  if ($label) {
    # Labeled bare block: use (block LABEL ...)
    # In Perl, a bare block is a single-iteration loop - last/next/redo all work.
    # With continue: wrap tagbody in catch for labeled next, then run continue after
    $self->_emit("(block $label");
    $self->indent_level($self->indent_level + 1);
    if ($continue_block) {
      # Use pcl:: prefix to match the package used by pl-next macro's throw
      $self->_emit("(catch 'pcl::NEXT-$label");
      $self->indent_level($self->indent_level + 1);
    }
    $self->_emit("(tagbody");
    $self->indent_level($self->indent_level + 1);
    $self->_emit(":redo");
    # Use pcl:: prefix to match the package used by pl-redo macro's throw
    $self->_emit("(catch 'pcl::REDO-$label");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(progn");
    $self->indent_level($self->indent_level + 1);
    $self->_process_block($block);
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
    $self->_process_block($block);
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
    $cond_cl = "(pl-not $cond_cl)";
  }

  $self->_emit("(pl-if $cond_cl");
  $self->indent_level($self->indent_level + 1);

  # Then block
  $self->_emit("(progn");
  $self->indent_level($self->indent_level + 1);
  $self->_process_block($first->{block});
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
      $self->_process_block($next->{block});
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

# Parse a block as a named function for eval/sub blocks
# Returns the generated function name
# $params is arrayref: [] for eval/sub
# Note: grep/map/sort now use parse_block_to_cl_string with inline lambdas
sub parse_block_as_function {
  my $self   = shift;
  my $block  = shift;  # PPI::Structure::Block
  my $params = shift // [];  # Parameter names

  # Generate unique function name
  my $func_name = sprintf("--anon-block-%d--", ++$anon_block_counter);

  # Build parameter list
  my $params_cl = join(' ', @$params);

  # Emit the function definition
  $self->_emit("(defun $func_name ($params_cl)");
  $self->indent_level($self->indent_level + 1);

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

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();

  # Emit nil if block was empty
  $self->_emit("nil") unless $has_content;

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
  $self->_emit("");  # Blank line after function

  return $func_name;
}

# Parse a block and return its body as CL code string (for inline lambdas)
# Returns the CL code string for the block body
sub parse_block_to_cl_string {
  my $self   = shift;
  my $block  = shift;  # PPI::Structure::Block

  # Save current output and indent, collect to temporary output
  my $saved_output = $self->output;
  my $saved_indent = $self->indent_level;
  $self->output([]);
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

  # Leave scope
  $self->environment->pop_scope();

  # Collect the body lines
  my @body_lines = @{$self->output};

  # Restore original output
  $self->output($saved_output);
  $self->indent_level($saved_indent);

  # Return body as string (or "nil" if empty)
  if (@body_lines) {
    return join("\n", @body_lines);
  } else {
    return "nil";
  }
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

    # Recurse into children
    if ($ref && $child->can('children')) {
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

  # Wrap in let if we have declarations
  if (@my_vars) {
    my $bindings = join(" ", map { "($_ (make-pl-box nil))" } @my_vars);
    $self->_emit("(let ($bindings)");
    $self->indent_level($self->indent_level + 1);
  }

  # Emit the body
  $emit_body->();

  # Close let if we opened it
  if (@my_vars) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
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

  # Handle 'until' by negating
  if ($keyword eq 'until') {
    $cond_cl = "(pl-not $cond_cl)";
  }

  # Build the loop form with optional label
  my $label_arg = $label ? " :label $label" : "";

  # Use common helper to wrap with declarations
  $self->_with_declarations($cond, sub {
    $self->_emit("(pl-while $cond_cl$label_arg");
    $self->indent_level($self->indent_level + 1);
    $self->_process_block($block) if $block;
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
    $self->_emit("(pl-for ($init_cl)");
    $self->_emit("        ($cond_cl)");
    $self->_emit("        ($incr_cl)$label_arg");
    $self->indent_level($self->indent_level + 1);
    $self->_process_block($block) if $block;
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

  $self->_emit("(pl-foreach ($loop_var $list_cl)$label_arg");
  $self->indent_level($self->indent_level + 1);
  $self->_process_block($block) if $block;
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

  # If we have state vars, wrap defun in a let for persistent storage
  if (@state_vars) {
    # Create bindings: ($var nil) ($var--init nil) for each state var
    my @bindings;
    for my $var (@state_vars) {
      push @bindings, "($var nil)";
      push @bindings, "($var--init nil)";
    }
    $self->_emit("(let (" . join(" ", @bindings) . ")");
    $self->indent_level($self->indent_level + 1);
  }

  # User-defined subs get pl- prefix to avoid conflicts with CL built-ins
  # Use pl-sub macro to wrap in eval-when for BEGIN block visibility
  # Wrap body in (block nil ...) so pl-return works
  # Handle qualified names: A::foo -> A::pl-foo (not pl-A::foo)
  my $cl_sub_name = $self->_qualified_sub_to_cl($name);
  $self->_emit("(pl-sub $cl_sub_name ($params_cl)");
  $self->indent_level($self->indent_level + 1);

  # If using %_args, convert to @_ vector
  if ($needs_args_conversion) {
    $self->_emit("(let ((\@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))");
    $self->indent_level($self->indent_level + 1);
  }

  $self->_emit("(block nil");
  $self->indent_level($self->indent_level + 1);

  # Track that we're inside a subroutine (for shift/pop @_ vs @ARGV)
  $self->environment->in_subroutine($self->environment->in_subroutine + 1);

  if ($block) {
    # Wrap sub body with let for local variable declarations
    # Pass state_vars so _with_declarations knows to skip them
    local $self->{_current_state_vars} = { map { $_ => 1 } @state_vars };
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    });
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
    $self->_emit_package_preamble($pkg_name);
    $self->environment->push_package($pkg_name);

    # Process the block contents
    for my $child ($block->schildren) {
      $self->_process_element($child);
    }

    $self->environment->pop_package();
    # Switch back to previous package
    my $prev_pkg = $self->environment->current_package();
    my $cl_prev  = $prev_pkg =~ /::/ ? ":|$prev_pkg|" : ":$prev_pkg";
    $self->_emit("(in-package $cl_prev)");
    $self->_emit(";;; end package $pkg_name");
    $self->_emit("");
  }
  else {
    # Simple form: package Foo;
    # This changes the current package until another package declaration
    $self->_emit_package_preamble($pkg_name);
    $self->environment->push_package($pkg_name);
    # Note: no pop - package remains active until next package declaration
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

  $self->_emit(";;; package $pkg_name");
  $self->_emit("(defpackage $cl_pkg");
  $self->_emit("  (:use :cl :pcl))");
  $self->_emit("(in-package $cl_pkg)");

  # Emit a CLOS class for this package (for MRO tracking)
  my $cl_class = $self->_pkg_to_clos_class($pkg_name);
  $self->_emit(";; CLOS class for MRO");
  $self->_emit("(defclass $cl_class () ())");
  $self->_emit("");
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
  pl-x pl-y pl-print pl-say pl-length pl-push pl-pop pl-shift pl-unshift
  pl-keys pl-values pl-exists pl-delete pl-sort pl-reverse pl-map pl-grep
  pl-join pl-split pl-ref pl-bless pl-die pl-warn pl-open pl-close
  pl-read pl-write pl-int pl-abs pl-substr pl-index pl-lc pl-uc
);

# Check if a sub name conflicts with PCL runtime
sub _is_pcl_symbol {
  my $self = shift;
  my $name = shift;
  return exists $PCL_SYMBOLS{"pl-$name"};
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

  # Handle 'no' statements (turn off pragma - no CL equivalent)
  if ($type eq 'no') {
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
      # Check if it's a simple string literal (compile-time)
      if (@tokens == 1 && $tokens[0]->isa('PPI::Token::Quote')) {
        my $path = $tokens[0]->string;
        $self->_emit(";; $perl_code");
        $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
        $self->_emit("  (pl-require-file \"$path\"))");
        $self->_emit("");
        return;
      }

      # Otherwise, parse as expression (runtime)
      # Use the parser's _parse_expression method
      my $expr_cl = $self->_parse_expression(\@tokens);
      if ($expr_cl) {
        $self->_emit(";; $perl_code");
        $self->_emit("(pl-require-file $expr_cl)");
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
  if ($module =~ /^(strict|warnings|feature|utf8|open|parent|base|Exporter|bytes|locale|integer)$/) {
    $self->_emit(";; $perl_code (pragma)");
    $self->_emit("");
    return;
  }

  # Handle 'use lib' - modify @INC
  if ($module eq 'lib') {
    $self->_process_use_lib($stmt, $perl_code);
    return;
  }

  # General use/require
  # Wrap in eval-when so they execute at compile time (needed for FASL caching)
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
      $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
      $self->_emit("  (pl-use \"$module\" :imports '($list)))");
    } else {
      $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
      $self->_emit("  (pl-use \"$module\"))");
    }
  }
  elsif ($type eq 'require') {
    $self->_emit(";; $perl_code");
    $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
    $self->_emit("  (pl-require \"$module\"))");
  }
  else {
    # Unknown type
    $self->_emit(";; $perl_code");
    $self->_emit(";; (include type '$type' not yet implemented)");
  }
  $self->_emit("");
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
    # BEGIN blocks execute at compile time (or during interpretation).
    # NOT at :load-toplevel - BEGIN should only run once, not again when loading fasl.
    # Subs and variables before this BEGIN are wrapped in eval-when
    # (via pl-sub/pl-our/pl-my macros), making them visible to BEGIN.
    $self->_emit(";; $perl_code");
    $self->_emit("(eval-when (:compile-toplevel :execute)");
    $self->indent_level($self->indent_level + 1);

    # Process block contents
    $self->_process_children($block);

    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
    $self->_emit("");
  }
  elsif ($type eq 'END') {
    # END blocks execute at program exit, in reverse order
    # Push a lambda to *end-blocks* (push gives LIFO = correct reverse order)
    $self->_emit(";; $perl_code");
    $self->_emit("(push (lambda ()");
    $self->indent_level($self->indent_level + 2);

    # Process block contents
    $self->_process_children($block);

    $self->indent_level($self->indent_level - 2);
    $self->_emit("  ) *end-blocks*)");
    $self->_emit("");
  }
  elsif ($type eq 'CHECK' || $type eq 'UNITCHECK') {
    # CHECK runs after compile, before execute - use load-toplevel only
    $self->_emit(";; $perl_code");
    $self->_emit("(eval-when (:load-toplevel)");
    $self->indent_level($self->indent_level + 1);

    $self->_process_children($block);

    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
    $self->_emit("");
  }
  elsif ($type eq 'INIT') {
    # INIT runs just before main code starts
    # In CL, code at toplevel runs in order, so just emit normally
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

  $self->_emit(";; $perl_code");
  $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");

  # Extract path arguments from the statement
  for my $child ($stmt->schildren) {
    if ($child->isa('PPI::Token::Quote')) {
      my $path = $child->string;
      $self->_emit("  (pl-unshift \@INC \"$path\")");
    }
    elsif ($child->isa('PPI::Token::QuoteLike::Words')) {
      # qw(path1 path2)
      my $content = $child->content;
      $content =~ s/^qw\s*[\(\[\{<]//;
      $content =~ s/[\)\]\}>]$//;
      for my $path (split /\s+/, $content) {
        $self->_emit("  (pl-unshift \@INC \"$path\")") if $path;
      }
    }
  }
  $self->_emit(")");  # Close eval-when
  $self->_emit("");

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
}


# Process single constant declaration
sub _process_single_constant {
  my $self        = shift;
  my $name        = shift;
  my $value_parts = shift;
  my $perl_code   = shift;

  $self->_emit(";; $perl_code");
  $self->_emit_constant($name, $value_parts);
  $self->_emit("");
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
  # Use pl-sub for compile-time visibility (BEGIN blocks can use constants)
  my $cl_sub_name = $self->_qualified_sub_to_cl($name);
  $self->_emit("(pl-sub $cl_sub_name () $cl_value)");

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
    $error =~ s/ at \/.*//s;  # Remove file/line info
    $error =~ s/\n.*//s;      # First line only
    return (";; PARSE ERROR: $error", []);
  }

  return ($result // ";; (no output)", \@decls);
}


# Emit a line to output
sub _emit {
  my $self = shift;
  my $line = shift;

  # Don't emit if we're just extracting prototypes
  return if $self->collect_prototypes_only;

  my $indent = "  " x $self->indent_level;
  push @{$self->output}, $indent . $line;
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

  # First pass: collect prototypes from all 'use'd modules
  my $proto_parser = $class->new(
    filename                => $filename,
    collect_prototypes_only => 1,
  );
  $proto_parser->parse;

  # Second pass: transpile with prototypes already known
  my $parser = $class->new(
    filename    => $filename,
    environment => $proto_parser->environment,
  );
  return $parser->parse;
}


sub parse_code {
  my $class = shift;
  my $code  = shift;

  # First pass: collect prototypes
  my $proto_parser = $class->new(
    code                    => $code,
    collect_prototypes_only => 1,
  );
  $proto_parser->parse;

  # Second pass: transpile with prototypes already known
  my $parser = $class->new(
    code        => $code,
    environment => $proto_parser->environment,
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
    warn "Failed to compile default expression '$expr': $@";
    return undef;
  }

  return $result;
}


1;
