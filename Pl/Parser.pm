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

use Pl::PExpr qw(SCALAR_CTX LIST_CTX VOID_CTX INHERIT_CTX);
use Pl::ExprToCL;
use Pl::Environment;

# File-level counters (shared across all Parser instances within a load)
my $anon_block_counter = 0;
my $state_var_counter  = 0;
my $lex_var_counter    = 0;

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


sub _preprocess_source {
  my ($src) = @_;
  # hex()/oct() on a hex-float mantissa (below) can exceed 0xffffffff (32-bit),
  # which makes Perl emit spurious 'Hexadecimal number > 0xffffffff non-portable'
  # / 'Integer overflow' warnings *from our own toolchain* while transpiling source
  # that legitimately contains large hex literals (hexfp.t, sprintf.t). These
  # conversions are intentional and correct on 64-bit, so silence the noise.
  no warnings 'portable', 'overflow';
  # Convert C99/Perl hex/binary/octal float literals (0x1.8p-1, 0b10p-2, 010.1p0)
  # to decimal before PPI sees them. PPI doesn't understand the 'p' exponent marker
  # and misparses 0x1.8p-1 as: 0x1 . p - 1 (hex-num, concat, bareword, minus, num).
  # Perl allows underscore separators anywhere in these literals.
  #
  # CRITICAL: only convert *numeric literals*, never text that merely looks like one
  # inside a quoted string (e.g. the string '0x1p+0' must stay '0x1p+0', not become
  # '1'). Each substitution therefore matches a quoted string as its FIRST alternative
  # and passes it through unchanged, so the float pattern is never seen inside a string.
  # (A float pattern inside a comment is harmless to convert — PPI discards comments —
  # so comments are not specially skipped.)
  my $str_re = qr{'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"};
  # Hex float: 0x[hex_][.[hex_]]p[+-][decimal_]
  $src =~ s{($str_re)|0x([0-9a-fA-F_]*)\.?([0-9a-fA-F_]*)[pP]([+-]?[\d_]+)}{
    if (defined $1) { $1 } else {
      my ($int_str, $frac_str, $exp_str) = ($2, $3, $4);
      $int_str  =~ s/_//g;
      $frac_str =~ s/_//g;
      $exp_str  =~ s/_//g;
      my $mantissa = ($int_str ne '' ? hex($int_str) : 0);
      $mantissa += hex($frac_str) / (16 ** length($frac_str)) if $frac_str ne '';
      sprintf("%.17g", $mantissa * (2 ** $exp_str));
    }
  }gex;
  # Binary float: 0b1.1p0, 0b10p-2
  $src =~ s{($str_re)|0b([01_]+)(?:\.([01_]*))?[pP]([+-]?[\d_]+)}{
    if (defined $1) { $1 } else {
      my ($int_str, $frac_str, $exp_str) = ($2, $3 // '', $4);
      $int_str  =~ s/_//g;
      $frac_str =~ s/_//g;
      $exp_str  =~ s/_//g;
      my $mantissa = oct("0b$int_str");
      $mantissa += oct("0b$frac_str") / (2 ** length($frac_str)) if $frac_str ne '';
      sprintf("%.17g", $mantissa * (2 ** $exp_str));
    }
  }gex;
  # Octal float: 010.1p0, 00p0.  Lookbehind prevents matching digits inside a larger number.
  $src =~ s{($str_re)|(?<!\w)0([0-7_]+)(?:\.([0-7_]*))?[pP]([+-]?[\d_]+)}{
    if (defined $1) { $1 } else {
      my ($int_str, $frac_str, $exp_str) = ($2, $3 // '', $4);
      $int_str  =~ s/_//g;
      $frac_str =~ s/_//g;
      $exp_str  =~ s/_//g;
      my $mantissa = oct("0$int_str");
      $mantissa += oct("0$frac_str") / (8 ** length($frac_str)) if $frac_str ne '';
      sprintf("%.17g", $mantissa * (2 ** $exp_str));
    }
  }gex;
  # Strip type annotations from foreach loop variables: `for my Dog $spot` → `for my $spot`.
  # Perl allows `for my ClassName $var` but PPI can't parse the ClassName and stops,
  # producing a broken AST. PCL ignores type constraints anyway, so just drop them.
  $src =~ s/\b(for(?:each)?\s+(?:my|our))\s+[A-Za-z_]\w*(?:::[A-Za-z_]\w*)*\s+(\$)/$1 $2/g;
  return $src;
}

sub _build_ppi_doc {
  my $self = shift;

  if ($self->has_filename) {
    open(my $fh, '<', $self->filename)
      or die "Failed to open file: " . $self->filename;
    my $src = _preprocess_source(do { local $/; <$fh> });
    close $fh;
    my $doc = PPI::Document->new(\$src);
    return $doc if $doc;
    die "Failed to parse file: " . $self->filename unless $self->lenient_ppi;
    return $self->_ppi_with_fallback($src);
  }
  elsif ($self->has_code) {
    my $code = _preprocess_source($self->code);
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

  # Reset parse state so second pass (parse_file shares environment between
  # passes) always starts clean. Without these resets:
  #   - package_stack accumulates from the first pass
  #   - state_var_renames contains first-pass renames, which contaminate
  #     second-pass code (e.g. $f renamed to $state__toplevel__f__34 from
  #     first pass bleeds into second-pass foreach/bare-block uses of $f)
  #   - Counters must restart at 0 so defvar names match usage names in the
  #     second pass (both are generated fresh, in the same order)
  $self->environment->package_stack(['main']);
  $self->environment->state_var_renames({});
  $anon_block_counter = 0;
  $state_var_counter  = 0;
  $lex_var_counter    = 0;

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

  # Close any local let forms opened at file level (e.g. local $^W at file scope).
  # _process_block closes them for block-scoped locals, but file-level locals
  # (outside any { }) need to be closed here after all children are processed.
  my $file_local_depth = $self->{_local_let_depth} // 0;
  while ($file_local_depth > 0) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")  ;; end local (file scope)");
    $self->{_local_let_depth}--;
    $file_local_depth--;
  }

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
    # Register package so it gets pre-declared
    $self->environment->add_referenced_package($pkg) if $self->environment;
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
        while ($line =~ /\b([A-Za-z_][A-Za-z0-9_]*)::/g) {
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
      push @predecls, "(pcl:p-defpackage $cl_pkg)";
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
    # If the runtime section contains bare top-level labels (standalone :WORD lines
    # not inside a tagbody), wrap the contiguous run of expression-only forms that
    # contains each label in a (tagbody ...).  Forms that contain p-sub / eval-when /
    # defvar definitions are kept outside the tagbody to preserve top-level semantics.
    my @rt = @{$section->{runtime}};
    push @lines, _wrap_runtime_labels(\@rt);
  }
  return @lines;
}


# Wrap the minimal set of top-level runtime lines that participate in a
# goto/label pair in individual (tagbody ...) forms.
#
# Real generated labels are marked with a ";; pcl-label" sentinel so we can
# distinguish them from ":word" patterns inside CL string literals.
#
# @rt elements can be multi-line strings.  A goto counts as "top-level" only
# when it is reachable by CL's lexically-scoped (go ...) — meaning it must
# NOT be inside a lambda or named function scope.  We detect this by:
#   - Skipping @rt elements that start with whitespace (indented = nested).
#   - Skipping @rt elements that are p-sub/eval-when definitions.
#   - Skipping a (go :LABEL) match if the text preceding it within the same
#     @rt element contains "lambda" (i.e. the goto is inside a lambda body).
#
# Algorithm:
#   1. Find each ":LABEL  ;; pcl-label" element (real label, first occurrence).
#   2. Find the last qualifying (go :LABEL) for each label.
#   3. Build [min(label_pos, last_goto_pos), max(...)] ranges, merge overlaps.
#   4. Wrap each range in (tagbody ...), hoisting definition elements out.
#   5. Everything outside the ranges is emitted as independent top-level forms.
sub _wrap_runtime_labels {
  my $rt_ref = shift;
  my @rt = @$rt_ref;

  # Quick exit: no real label sentinels.
  return @rt unless grep { /^:[A-Za-z][A-Za-z0-9_]*\s*;; pcl-label/ } @rt;

  # Definition elements must stay outside any tagbody (same test used in pass 2).
  my $is_definition = sub {
    $_[0] =~ /^\((?:p-sub|eval-when|defvar|defpackage|in-package|p-defpackage|p-BEGIN)\b/;
  };

  # Collect real labels: name → first-occurrence index.
  my %label_first;
  for my $i (0 .. $#rt) {
    if ($rt[$i] =~ /^:([A-Za-z][A-Za-z0-9_]*)\s*;; pcl-label/) {
      $label_first{$1} //= $i;
    }
  }

  # Find the last qualifying (go :LABEL) for each known label.
  # "Qualifying" = not inside a lambda or function definition scope.
  my %last_goto;
  for my $i (0 .. $#rt) {
    next if $rt[$i] =~ /^\s/;         # starts indented → inside a nested form
    next if $is_definition->($rt[$i]);# definition → different function scope
    while ($rt[$i] =~ /\(go\s+:([A-Za-z][A-Za-z0-9_]*)\)/g) {
      my $lbl    = $1;
      my $prefix = substr($rt[$i], 0, $-[0]);
      next if $prefix =~ /\blambda\b/;  # goto is inside a lambda → not reachable
      $last_goto{$lbl} = $i if exists $label_first{$lbl};
    }
  }

  # Drop labels that have no qualifying goto (nothing to wrap).
  delete $label_first{$_} for grep { !exists $last_goto{$_} } keys %label_first;
  return @rt unless %label_first;

  # Build minimal [start, end] ranges (covers both the label and its goto).
  my @ranges;
  for my $lbl (keys %label_first) {
    my ($lpos, $gpos) = ($label_first{$lbl}, $last_goto{$lbl});
    push @ranges, [ ($lpos < $gpos ? $lpos : $gpos),
                    ($lpos > $gpos ? $lpos : $gpos) ];
  }

  # Merge overlapping / adjacent ranges.
  @ranges = sort { $a->[0] <=> $b->[0] } @ranges;
  my @merged;
  for my $r (@ranges) {
    if (@merged && $r->[0] <= $merged[-1][1] + 1) {
      $merged[-1][1] = $r->[1] if $r->[1] > $merged[-1][1];
    } else {
      push @merged, [ $r->[0], $r->[1] ];
    }
  }

  # Assemble result.
  my @result;
  my $pos = 0;
  for my $region (@merged) {
    my ($start, $end) = @$region;

    # Independent forms before this region.
    push @result, @rt[$pos .. $start - 1] if $start > $pos;

    # Wrap [start, end] in (tagbody ...), hoisting definition lines out.
    my @tb;
    for my $i ($start .. $end) {
      if ($is_definition->($rt[$i])) {
        push @result, "(tagbody", @tb, ")" if @tb;
        @tb = ();
        push @result, $rt[$i];
      } else {
        push @tb, $rt[$i];
      }
    }
    push @result, "(tagbody", @tb, ")" if @tb;

    $pos = $end + 1;
  }

  # Independent forms after the last region.
  push @result, @rt[$pos .. $#rt] if $pos <= $#rt;

  return @result;
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

  my %declared;           # variables with defvar already in preamble/declarations
  my %let_bound;          # variables bound by let/let*/foreach at FILE scope only (union)
  my %foreach_let_bound;  # only from (p-foreach ($var ...)) lines
  my %other_let_bound;    # only from other (let/let* ...) forms
  my %referenced;         # all variable references at FILE scope only
  my %cross_pkg_vars;     # pkg::$var references needing defvar in their package

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
      # Collect let/let*-bound variables (non-foreach).
      if ($line =~ /\(let\*?\s+\(/) {
        while ($line =~ /\(([\$\@\%][a-zA-Z_]\w*)\s+/g) {
          $let_bound{$1} = 1;
          $other_let_bound{$1} = 1;
        }
      }
      # Collect foreach-bound variables (tracked separately to identify pure loop vars).
      if ($line =~ /\(p-foreach\s+\(([\$\@\%][a-zA-Z_]\w*)\b/) {
        $let_bound{$1} = 1;
        $foreach_let_bound{$1} = 1;
      }
      # Collect all variable references
      while ($line =~ /([\$\@\%][a-zA-Z_]\w*)/g) {
        my $var = $1;
        next if $var =~ /::/;  # skip package-qualified (handled separately below)
        $referenced{$var} = 1;
      }
    }

    # Collect cross-package variable references at ANY nesting depth
    # (including inside overload handler lambdas, sub bodies, etc.).
    # e.g. o::$str, o::$num inside overload lambdas need defvar in their package.
    # Skip special packages (ENV, INC, SIG, pcl, main) and already-defvar'd forms.
    # Also handles pipe-quoted multi-component package names: |do::not::overwrite|::$this
    unless ($line =~ /^\s*\(defvar\s/) {
      my %skip_pkg = map { $_ => 1 } qw(ENV INC SIG pcl main);
      while ($line =~ /(?:\b([a-zA-Z_]\w*)|\|([^|]+)\|)::([\$\@\%][a-zA-Z_]\w*)/g) {
        my ($pkg, $var) = (defined($1) ? $1 : "|$2|", $3);
        my $bare_pkg = defined($1) ? $1 : $2;
        next if $skip_pkg{$bare_pkg};
        next if $var =~ /^[\$\@\%][0-9]/;  # special: $1, $2...
        $cross_pkg_vars{"$pkg\::$var"} = [$pkg, $var];
      }
    }

    # Track closing of sub definitions by counting parens
    if ($sub_depth > 0 && $line =~ /^\)/) {
      $sub_depth--;
    }
  }

  # Undeclared = referenced - declared - runtime - pure-foreach-my vars
  # Note: do NOT exclude all let_bound vars. A variable may be let-bound inside a
  # bare block (e.g. 'my @a' -> (let ((@a ...))...)) but still referenced as a
  # package variable at an earlier point in load order. defvar is idempotent,
  # so emitting a forward declaration for regular let-bound vars is safe.
  # EXCEPTION: 'for my $var' variables are Perl lexicals, never package globals.
  # Once defvar'd, ALL let-bindings of that name become dynamic (special) — closures
  # inside the loop would capture the symbol rather than the per-iteration value.
  # Safe to skip defvar when the var is foreach-only (not also bound by other let forms).
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

  my %lex_foreach = %{$self->{_lexical_foreach_vars} // {}};

  my @undeclared;
  for my $var (sort keys %referenced) {
    next if $declared{$var};
    next if $runtime_vars{$var};
    # __lex__ variables are renamed 'my' vars from _with_declarations: they must stay
    # lexical (no defvar) so per-closure-call let-bindings are lexical not dynamic.
    next if $let_bound{$var} && $var =~ /__lex__/;
    # 'for my $var' is a Perl lexical — never defvar it as a package global.
    # Safe to skip when it's foreach-only (not also bound by other let forms).
    next if $lex_foreach{$var} && $foreach_let_bound{$var} && !$other_let_bound{$var};
    next if $var =~ /^[\$\@\%]state__/;  # state vars use outer let binding, not defvar
    push @undeclared, $var;
  }

  # Emit defvars for cross-package variable references (e.g. o::$str used in
  # overload handlers). These are declared in the global section 0 defvar block;
  # CL defvar doesn't require the current package to match — pkg::$var works.
  # Already-defvar'd vars are skipped (defvar is idempotent but avoid duplicates).
  if (%cross_pkg_vars) {
    my %already_cross_declared;
    for my $section (@{$self->_sections}) {
      for my $line (@{$section->{preamble}}, @{$section->{declarations}}) {
        if ($line =~ /\(defvar\s+(?:(\w+)|\|([^|]+)\|)::([\$\@\%]\w+)\b/) {
          my $pkg = defined($1) ? $1 : "|$2|";
          $already_cross_declared{"$pkg\::$3"} = 1;
        }
      }
    }
    my @cross_decls;
    for my $key (sort keys %cross_pkg_vars) {
      next if $already_cross_declared{$key};
      my ($pkg, $var) = @{$cross_pkg_vars{$key}};
      my $sigil = substr($var, 0, 1);
      if ($sigil eq '$') {
        push @cross_decls, "(defvar $pkg\::$var (make-p-box nil))";
      } elsif ($sigil eq '@') {
        push @cross_decls, "(defvar $pkg\::$var (make-array 0 :adjustable t :fill-pointer 0))";
      } elsif ($sigil eq '%') {
        push @cross_decls, "(defvar $pkg\::$var (make-hash-table :test 'equal))";
      }
    }
    if (@cross_decls) {
      push @$decls, ";; Cross-package variable declarations (e.g. pkg::var used in closures).";
      push @$decls, @cross_decls;
      push @$decls, "";
    }
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
  # Handle package stash access: %Pkg:: or $Pkg:: (symbol table reference)
  # Must be checked BEFORE the package-qualified variable regex
  if ($var =~ /^([\$\%])(.*)::$/) {
    my ($sigil, $pkg) = ($1, $2);
    $pkg = 'main' if $pkg eq '';
    return "(p-stash \"$pkg\")";
  }
  # Handle package-qualified variables: $Pkg::var -> Pkg::$var
  # Note: Use (.*) not (.+) to allow empty package (main shorthand)
  if ($var =~ /^([\$\@\%])(.*)::([^:]+)$/) {
    my ($sigil, $pkg, $name) = ($1, $2, $3);
    # Empty package means main (e.g., $::foo = $main::foo)
    $pkg = 'main' if $pkg eq '';
    my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
    return "${cl_pkg}::${sigil}${name}";
  }
  # Pipe-quote if the name contains characters CL can't read as a bare symbol
  # (e.g. $" → |$"|, $\ → |$\\|, $| → |$\||, $; → |$;|)
  if ($var =~ /["|;,()\[\]{}'`\\]/) {
    (my $inner = $var) =~ s/([|\\])/\\$1/g;
    return "|$inner|";
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
  my ($self, $group, $open, $stmt, $ctx) = @_;
  $ctx //= 0;  # default SCALAR_CTX
  if ($open eq '{' && @$group == 1 && ref($group->[0]) eq 'PPI::Token::Word') {
    my $word = $group->[0]->content;
    # Only auto-quote if it's not a keyword
    unless ($word =~ /^(?:if|unless|while|until|for|foreach|sub|my|our|local|state|undef|defined|not|and|or)$/) {
      return "\"$word\"";
    }
  }
  return $self->_parse_expression($group, $stmt, $ctx) // 'nil';
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
    # CORE::state $x = ... is a variable declaration that PPI sees as a plain statement
    my ($first) = grep { ref($_) ne 'PPI::Token::Whitespace' } $element->children;
    if (defined $first && ref($first) eq 'PPI::Token::Word'
        && $first->content =~ /^CORE::(my|our|state|local)$/) {
      $first->{content} = $1;  # strip CORE:: prefix so _process_variable_statement recognizes it
      $self->_process_variable_statement($element);
    } else {
      $self->_process_expression_statement($element);
    }
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

  # Handle '...' (yada yada / unimplemented placeholder) as a statement.
  # In Perl, a bare '...' statement dies with "Unimplemented".
  if (@parts == 1 && ref($parts[0]) eq 'PPI::Token::Operator' && $parts[0]->content eq '...') {
    $self->_emit(";; $perl_code (yada yada)");
    # Perl dies "Unimplemented at $0 line N.\n".  The file part is the runtime
    # program name ($0), not the compile-time source file, so build the :loc
    # location at runtime from $0 and the literal source line.
    my $yada_line = (ref($parts[0]) && $parts[0]->can('line_number'))
      ? ($parts[0]->line_number // 0) : 0;
    $self->_emit(qq{(p-die "Unimplemented" :loc (format nil "~A line ~D" (to-string (unbox \$0)) $yada_line))});
    return;
  }

  # Handle '++ state $y' / '-- state $y': PPI treats these as generic expression
  # statements (not PPI::Statement::Variable) because they start with an operator.
  # We detect the 'state' keyword here, register the state declaration (emits
  # defvar + rename), strip 'state' from parts, then parse the remaining expression.
  for my $i (0 .. $#parts) {
    if (ref($parts[$i]) eq 'PPI::Token::Word' && $parts[$i]->content eq 'state') {
      # Build synthetic parts for just the state declaration (state keyword + vars)
      my @decl_parts = @parts[$i .. $#parts];
      if ($self->environment->in_subroutine == 0) {
        $self->_process_toplevel_state_declaration($stmt, \@decl_parts, $perl_code);
      } else {
        $self->_process_state_declaration($stmt, \@decl_parts, $perl_code);
      }
      # Remove the 'state' token from parts so the remaining expression parses cleanly.
      # The state_var_renames lookup in ExprToCL will apply the rename.
      splice(@parts, $i, 1);
      last;
    }
  }

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
    # Stash element — not supported; skip save/restore so subsequent tests can run.
    if ($cl_var =~ /^\(p-stash /) {
      $self->_emit(";; $perl_code (delete local on stash — not supported, skipped)");
      return;
    }
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

    my $expr_cl = $self->_parse_expression(\@expr_parts, $stmt, VOID_CTX);

    # Generate appropriate control structure
    # Note: 'for' and 'foreach' modifiers use p-foreach (iterate over list),
    # not p-for (C-style for loop)
    my $cl_modifier = $modifier;
    if ($modifier eq 'for' || $modifier eq 'foreach') {
      $cl_modifier = 'foreach';
      # The list must be in LIST_CTX (= 1) so split() returns elements not count
      my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt, 1);
      $cl_code = "(p-foreach (\$_ $cond_cl) $expr_cl)";
    }
    else {
      my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt);
      # Apply Perl's auto-defined() insertion for while-modifier loops.
      # while ($x = readdir/each/readline/glob) terminates on undef, not on false.
      if ($cl_modifier eq 'while') {
        my $auto_pat = 'p-each|p-readdir|p-readline|p-glob';
        if ($cond_cl =~ /^\(p-(?:scalar|my)-=\s+(\$\S+)\s+\((?:$auto_pat)\b/) {
          my $var = $1;
          $cond_cl = "(progn $cond_cl (p-defined $var))";
        } elsif ($cond_cl =~ /^\(p-setf\s+\(p-(?:gethash|aref)\b.*\((?:$auto_pat)\b/) {
          $cond_cl = "(p-defined $cond_cl)";
        } elsif ($cond_cl =~ /^\((?:$auto_pat)\b/) {
          # Bare call: assign to $_ and defined-check
          $cond_cl = "(progn (p-setf \$_ $cond_cl) (p-defined \$_))";
        }
      }
      $cl_code = "(p-$cl_modifier $cond_cl $expr_cl)";
    }
  }
  else {
    # No modifier - bare expression statement; result is discarded (void context)
    $cl_code = $self->_parse_expression(\@parts, $stmt, VOID_CTX);
  }

  # Emit as comment + code.
  # When inside a sub body and NOT at tail position, wrap in void context so that
  # dynamic operators like /g regex don't inherit the caller's list context.
  if (defined $cl_code
      && $self->environment->in_subroutine > 0
      && !$self->environment->tail_position) {
    $cl_code = "(let ((*wantarray* :void)) $cl_code)";
  }
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

  # Handle top-level 'my' declarations - need eval-when+defvar for BEGIN block visibility
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

  # Package-level state: needs init-once guard (unlike `my` which runs once at load)
  # Each `state $var` declaration at top-level gets a unique name and init flag
  # so multiple `state $var` in different loops don't share the same variable.
  if ($is_state && $self->environment->in_subroutine == 0) {
    $self->_process_toplevel_state_declaration($stmt, \@parts, $perl_code);
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

  # Special case: 'my $x = state $y = EXPR' inside a sub.
  # The state $y part needs its init guard; the whole expr must return $y's current value.
  # Detected when declarator is 'my', there's a state var in state_vars, and RHS has 'state'.
  if ($declarator eq 'my' && %$state_vars) {
    my ($eq_idx, $state_idx) = (-1, -1);
    for my $i (0 .. $#parts) {
      my $pref = ref($parts[$i]);
      if ($pref eq 'PPI::Token::Operator' && $parts[$i]->content eq '=' && $eq_idx < 0) {
        $eq_idx = $i;
      }
      if ($eq_idx >= 0 && $pref eq 'PPI::Token::Word' && $parts[$i]->content eq 'state') {
        $state_idx = $i; last;
      }
    }
    if ($state_idx > $eq_idx && $eq_idx >= 0) {
      # Find the state var and its init
      my $state_var_name;
      my $state_eq_idx = -1;
      my $rhs_state_parts = [grep { ref($_) ne 'PPI::Token::Whitespace' }
                              @parts[($state_idx + 1) .. $#parts]];
      for my $i (0 .. $#$rhs_state_parts) {
        my $pref = ref($rhs_state_parts->[$i]);
        if ($pref eq 'PPI::Token::Symbol' && !defined $state_var_name) {
          $state_var_name = $rhs_state_parts->[$i]->content;
        }
        if ($pref eq 'PPI::Token::Operator' && $rhs_state_parts->[$i]->content eq '=' && $state_eq_idx < 0) {
          $state_eq_idx = $i;
        }
      }
      if (defined $state_var_name) {
        my $renames  = $self->environment->state_var_renames // {};
        my $cl_state = $renames->{$state_var_name} // $state_var_name;
        my $flag     = "${cl_state}__init";

        # Find the LHS variable name
        my $lhs_name;
        for my $p (@parts[0 .. ($eq_idx - 1)]) {
          if (ref($p) eq 'PPI::Token::Symbol') { $lhs_name = $p->content; last; }
        }
        $lhs_name //= '$__unused';

        # Parse init expression if present
        my $init_cl = 'nil';
        if ($state_eq_idx >= 0) {
          my @init_parts = grep { ref($_) ne 'PPI::Token::Whitespace' }
                           @$rhs_state_parts[($state_eq_idx + 1) .. $#$rhs_state_parts];
          $init_cl = $self->_parse_expression(\@init_parts, $stmt) // 'nil' if @init_parts;
        }

        $self->_emit(";; $perl_code");
        $self->_emit("(p-my-= $lhs_name");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(progn");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(unless $flag");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(box-set $cl_state $init_cl)");
        $self->_emit("(setf $flag t))");
        $self->indent_level($self->indent_level - 1);
        $self->_emit("$cl_state))");
        $self->indent_level($self->indent_level - 2);
        $self->_emit("");
        return;
      }
    }
  }

  # For 'my @arr = EXPR' inside a block where init was pre-computed into the let binding
  # (self-referential init: my @bee = @bee), skip the body assignment — it's already done.
  if ($declarator eq 'my') {
    my $binding_inits = $self->{_my_binding_init_vars} // {};
    if (%$binding_inits) {
      my $decl_var;
      for my $p (@parts) {
        my $ref = ref($p);
        last if $ref eq 'PPI::Token::Operator' && $p->content eq '=';
        last if $ref eq 'PPI::Structure::List';  # multi-var: don't skip
        if ($ref eq 'PPI::Token::Symbol') { $decl_var = $p->content; last; }
      }
      if (defined $decl_var && $binding_inits->{$decl_var}) {
        $self->_emit(";; $perl_code (init in let binding)");
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
  # When inside a sub, _insert_variable_forward_declarations won't see our variables
  # at file scope, so we must emit defvars explicitly here.
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    if ($self->environment->in_subroutine > 0) {
      for my $var (@vars) {
        my $sigil = substr($var, 0, 1);
        my $init = $sigil eq '$' ? '(make-p-box nil)'
                 : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                 :                 '(make-hash-table :test #\'equal)';
        my $cl_var = "${pkg}::${var}";
        $self->_emit("(defvar $cl_var $init)");
      }
    }
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
          $self->_emit("(p-eval-always");
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
          $self->_emit("(p-eval-always");
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
          $self->_emit("(p-eval-always");
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
          $self->_emit("(p-eval-always");
          if ($sigil eq '@') {
            $self->_emit("  (defvar $var (make-array 0 :adjustable t :fill-pointer 0)))");
          } elsif ($sigil eq '%') {
            $self->_emit("  (defvar $var (make-hash-table :test 'equal)))");
          } else {
            $self->_emit("  (defvar $var (make-p-box nil)))");
          }
        }
      });
      # Now do the assignment at runtime.
      # our (...) = (...) is a list assignment, so the RHS is LIST context
      # (so '1..3' generates a range, not a flip-flop).
      my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt, 1) // 'nil';  # 1 = LIST_CTX
      my $vars_vector = "(vector " . join(" ", @vars) . ")";
      $self->_emit("(p-list-= $vars_vector $init_cl)");
    }
  }
  else {
    # Bare declaration: our $x; or our @arr; or our %hash;
    $self->_with_bucket('declarations', sub {
      for my $var (@vars) {
        my $sigil = substr($var, 0, 1);
        $self->_emit("(p-eval-always");
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

# Process top-level 'state' declaration - like my but with init-once guard.
# Each declaration gets a unique renamed CL variable so that multiple `state $x`
# in different loops at the same file scope don't share the same binding.
sub _process_toplevel_state_declaration {
  my ($self, $stmt, $parts, $perl_code) = @_;

  # Parse variable(s) and optional initializer
  my @vars;
  my $init_idx   = -1;
  my $postfix_op = '';  # '++' or '--' after variable (no '=')
  for my $i (0 .. $#$parts) {
    my $p    = $parts->[$i];
    my $pref = ref($p);
    if ($pref eq 'PPI::Token::Symbol' || $pref eq 'PPI::Token::Magic') {
      push @vars, $p->content;
    }
    elsif ($pref eq 'PPI::Structure::List') {
      push @vars, $self->_find_symbols_in_list($p);
    }
    elsif ($pref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i; last;
    }
    elsif ($pref eq 'PPI::Token::Operator' && $p->content =~ /^(\+\+|--)$/ && @vars) {
      $postfix_op = $p->content; last;
    }
  }
  return unless @vars;

  # Assign each variable a unique renamed CL name and an init flag.
  my %renames_for_this;
  my $env_renames = $self->environment->state_var_renames // {};
  for my $var (@vars) {
    my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
    ($sigil, $bare) = ('$', $var) unless defined $bare;
    (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
    my $n = ++$state_var_counter;
    my $new_name = sprintf('%sstate__toplevel__%s__%d', $sigil, $slug, $n);
    $renames_for_this{$var} = $new_name;
  }

  # Persist renames so subsequent code in this file uses the new names.
  $self->environment->state_var_renames({ %$env_renames, %renames_for_this });

  # Emit declarations (defvar for each renamed var + init flag)
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $cl_var = $renames_for_this{$var};
      my $sigil  = substr($cl_var, 0, 1);
      $self->_emit("(p-eval-always");
      if ($sigil eq '@') {
        $self->_emit("  (defvar $cl_var (make-array 0 :adjustable t :fill-pointer 0)))");
      } elsif ($sigil eq '%') {
        $self->_emit("  (defvar $cl_var (make-hash-table :test 'equal)))");
      } else {
        $self->_emit("  (defvar $cl_var (make-p-box nil)))");
      }
      $self->_emit("(p-eval-always (defvar ${cl_var}__init nil))");
    }
  });

  # Emit inline init guard (only runs init expression once)
  if ($init_idx >= 0) {
    my @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' }
                    @$parts[($init_idx + 1) .. $#$parts];
    my $init_cl = 'nil';
    $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil' if @rhs_parts;

    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $cl_var  = $renames_for_this{$var};
      my $sigil   = substr($cl_var, 0, 1);
      my $flag    = "${cl_var}__init";
      $self->_emit("(unless $flag");
      $self->indent_level($self->indent_level + 1);
      if ($sigil eq '$') {
        $self->_emit("(box-set $cl_var $init_cl)");
      } elsif ($sigil eq '@') {
        $self->_emit("(p-array-= $cl_var (let ((*wantarray* t)) (list $init_cl)))");
      } elsif ($sigil eq '%') {
        $self->_emit("(p-hash-= $cl_var (let ((*wantarray* t)) (list $init_cl)))");
      }
      $self->_emit("(setf $flag t))");
      $self->indent_level($self->indent_level - 1);
    }
    $self->_emit("");
  }

  # Emit post-increment/decrement when no initializer (state $z++)
  if ($postfix_op && $init_idx < 0) {
    my $cl_op = ($postfix_op eq '++') ? 'p-post++' : 'p-post--';
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $cl_var = $renames_for_this{$var};
      $self->_emit("($cl_op $cl_var)");
    }
    $self->_emit("");
  }

  # The value of a `state $x = EXPR` expression is the CURRENT value of $x
  # (not the init-guard result). Emit the variable as the trailing form so the
  # statement yields the right value in tail/expression position — e.g. as a
  # map/grep block return or a sub's implicit return. Only meaningful for a
  # single declared variable; list forms are left as-is.
  if (!$postfix_op && @vars == 1) {
    $self->_emit($renames_for_this{$vars[0]});
  }
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
      $self->_emit("(p-eval-always");
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
        # Stash element — not supported; emit comment and skip
        if ($cl_var =~ /^\(p-stash /) {
          $self->_emit(";; $perl_code (delete local on stash — not supported, skipped)");
          return;
        }
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
          # Array/hash: check if init was moved to the let binding (self-referential init)
          if (($self->{_my_binding_init_vars} // {})->{$var}) {
            $self->_emit(";; $perl_code (init in let binding)");
          } else {
            # Parse full statement through expression parser for proper list context
            # This generates (p-array-= @arr (vector ...)) or (p-hash-= %h (p-hash ...))
            my $cl_code = $self->_parse_expression($parts, $stmt);
            $self->_emit($cl_code) if defined $cl_code;
          }
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
  # When inside a sub (inline package with no (in-package) context change),
  # qualify @ISA with the package name so it lands in the right package.
  my $isa_sym = ($self->environment->in_subroutine > 0)
    ? "${pkg}::\@ISA"
    : "\@ISA";
  $self->_with_bucket('declarations', sub {
    $self->_emit("(defvar $isa_sym (make-array 0 :adjustable t :fill-pointer 0))");
  });
  for my $parent (@parents) {
    $self->_emit("(p-push $isa_sym \"$parent\")");
  }

  $self->_emit("");
}

# Process 'use base' / 'use parent' - equivalent to push @ISA, ...
# Also sets up CLOS inheritance for MRO.
sub _process_use_base {
  my ($self, $stmt, $perl_code, $module) = @_;

  # Extract parent class names from the argument list
  my @parents;
  my $skip_next = 0;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    # 'use parent -norequire, qw(...)' — skip the -norequire flag
    if ($ref eq 'PPI::Token::Operator' && $child->content eq '-') {
      $skip_next = 1; next;
    }
    if ($skip_next && $ref eq 'PPI::Token::Word') { $skip_next = 0; next; }
    $skip_next = 0;
    if ($ref eq 'PPI::Token::QuoteLike::Words') {
      my $content = $child->content;
      $content =~ s/^qw[^\w\s]//;
      $content =~ s/[^\w\s]$//;
      push @parents, split /\s+/, $content;
    }
    elsif ($ref eq 'PPI::Token::Quote::Single' || $ref eq 'PPI::Token::Quote::Double') {
      push @parents, $child->string;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      for my $item ($child->children) {
        next if ref($item) =~ /Whitespace|Separator/;
        if (ref($item) eq 'PPI::Token::Quote::Single' || ref($item) eq 'PPI::Token::Quote::Double') {
          push @parents, $item->string;
        }
      }
    }
  }
  @parents = grep { /\S/ } @parents;
  return unless @parents;

  my $pkg = $self->environment->current_package;

  # Redefine CLOS class with parents for MRO
  my $cl_class = $self->_pkg_to_clos_class($pkg);
  my $parents_cl = join(' ', map {
    my $cls = $self->_pkg_to_clos_class($_);
    my $pkg_prefix = ($_ =~ /::/) ? "|$_|" : $_;
    "$pkg_prefix\:\:$cls"
  } @parents);
  $self->environment->set_isa($pkg, \@parents);
  $self->_with_bucket('preamble', sub {
    $self->_emit(";; $perl_code");
    $self->_emit("(defclass $cl_class ($parents_cl) ())");
  });

  # Declare @ISA in declarations bucket, push parents at load time
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

    # Stash slice/element: $Pkg::{key} or @Pkg::{keys} — stash manipulation
    # is not supported. Emit just the body (no save/restore) so the file
    # doesn't crash and subsequent tests can run.
    if ($cl_var =~ /^\(p-stash /) {
      $self->_emit(";; $perl_code (stash element local — not supported, running body only)");
      return;
    }

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
      # Array subscripts use LIST_CTX so that 1..2 generates a range vector instead
      # of a flip-flop (which would trigger $SIG{__WARN__} via uninitialized $..).
      my $sub_ctx = ($open eq '[') ? 1 : 0;
      my @key_cls = map { $self->_subscript_key_expr($_, $open, $stmt, $sub_ctx) } @key_groups;

      # Choose the macro based on subscript type.
      # p-local-array-slice handles both scalar and vector (range) indices.
      my $macro      = ($open eq '{') ? 'p-local-hash-elem'      : 'p-local-array-slice';
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

  # Detect 'local our $var' — the 'our' qualifier means a package variable.
  # Emit a defvar so the variable is declared as special before local binds it.
  my $has_our = (@non_ws && ref($non_ws[0]) eq 'PPI::Token::Word'
                          && $non_ws[0]->content eq 'our');
  if ($has_our) {
    my $pkg = $self->environment ? $self->environment->current_package : 'main';
    $self->_with_bucket('declarations', sub {
      for my $p (@non_ws[1..$#non_ws]) {
        next if ref($p) ne 'PPI::Token::Symbol';
        my $var = $p->content;
        last if $var =~ /^=/; # stop at '=' (won't happen but be safe)
        my $sigil = substr($var, 0, 1);
        my $init  = $sigil eq '$' ? '(make-p-box nil)'
                  : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                  :                 "(make-hash-table :test #'equal)";
        my $cl_var = "${pkg}::${var}";
        $self->_emit("(defvar $cl_var $init)");
        $self->environment->add_our_variable($pkg, $var) if $self->environment;
      }
    });
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
  my $use_let_star = 0;
  if ($init_idx >= 0 && @vars == 1) {
    # local $x = value
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

    my $var = $vars[0];
    # For qualified vars (e.g. A::@ISA), the sigil is embedded after '::'.
    # For simple vars (e.g. @arr), it is the first character.
    my ($sigil) = ($var =~ /::([%\@\$])/) ? ($1) : (substr($var, 0, 1));

    # Use LIST_CTX for array/hash RHS so '..' generates a range, not a flip-flop
    my $rhs_ctx = ($sigil eq '@' || $sigil eq '%') ? 1 : 0;
    my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt, $rhs_ctx) // 'nil';

    if ($var eq '$!' || $var eq '|$!|') {
      # local $! = N: bind *p-stored-errno* (auto-restored by let) and set C errno
      push @bindings, "(pcl::*p-stored-errno* (pcl::%pcl-local-errno-init $init_cl))";
    }
    elsif ($sigil eq '@') {
      # local @arr = EXPR: evaluate EXPR with old @arr, make an independent copy.
      # Special case: when EXPR is (p-array-= VAR RHS), p-array-= mutates VAR in-place
      # during let binding evaluation. CL saves the symbol-value POINTER before binding,
      # but p-array-= has already mutated the pointed-to vector. On let exit, CL restores
      # the pointer to the (now-mutated) old vector — giving the wrong restored value.
      #
      # Fix: detect (p-array-= VAR RHS) and bypass the in-place mutation:
      #   Same var (local @bee = local(@bee) = RHS): use inner RHS for p-copy-array.
      #   Different var (local @bim = local(@bee) = RHS): give @bee its own let binding.
      (my $init_cl_trimmed = $init_cl) =~ s/^\s+|\s+$//gs;
      if ($init_cl_trimmed =~ /^\(p-array-= (\S+) (.+)\)$/s) {
        my ($mutated_var, $inner_rhs) = ($1, $2);
        if ($mutated_var eq $var) {
          # Same-var: skip the p-array-= mutation; copy the RHS directly.
          push @bindings, "($var (p-copy-array (let ((*wantarray* t)) $inner_rhs)))";
        } else {
          # Different-var: bind BOTH vars so CL saves/restores each independently.
          $self->{_local_counter} //= 0;
          my $tmp = "pcl-local-inner-" . $self->{_local_counter}++;
          unshift @bindings, "($tmp (let ((*wantarray* t)) $inner_rhs))";
          push @bindings, "($mutated_var (p-copy-array $tmp))";
          push @bindings, "($var (p-copy-array $tmp))";
          $use_let_star = 1;
        }
      } else {
        push @bindings, "($var (p-copy-array (let ((*wantarray* t)) $init_cl)))";
      }
    }
    elsif ($sigil eq '%') {
      # local %h = EXPR: evaluate EXPR with old %h, make an independent copy.
      push @bindings, "($var (p-copy-hash (let ((*wantarray* t)) $init_cl)))";
    }
    else {
      push @bindings, "($var (p-box-for-local $init_cl))";
    }
  }
  else {
    # Bare local or multiple vars - just shadow with nil/empty.
    # Skip undef markers (they are skip slots, not real variables).
    for my $var (@vars) {
      next if $var eq '(p-undef)';  # undef slot: no binding needed
      my ($sigil) = ($var =~ /::([%\@\$])/) ? ($1) : (substr($var, 0, 1));
      if ($var eq '$!' || $var eq '|$!|') {
        # bare local $!: save/restore *p-stored-errno*, clear to 0 (Perl undef $! = 0)
        push @bindings, "(pcl::*p-stored-errno* 0)";
      }
      elsif ($sigil eq '@') {
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

  # For multi-var local with initializer: local($a, $b, @arr) = @_
  # Pre-evaluate the RHS BEFORE the let bindings so that variables in the RHS
  # (e.g. @arr) still refer to their OLD values, not the freshly-bound empty ones.
  # Example: local (undef, @bee) = @bee  — @bee on RHS must see old @bee.
  # Use let* with the RHS as the first binding, then the fresh variable slots.
  my ($rhs_tmp_cl);
  if ($init_idx >= 0 && @vars > 1) {
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;
    my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
    $rhs_cl = "(let ((*wantarray* t) (*p-in-list-assign-rhs* t)) $rhs_cl)";
    $self->{_local_counter} //= 0;
    $rhs_tmp_cl = "pcl-local-rhs-" . $self->{_local_counter}++;
    unshift @bindings, "($rhs_tmp_cl $rhs_cl)";
  }

  my $bindings_str = join("\n        ", @bindings);
  my $let_form = ($rhs_tmp_cl || $use_let_star) ? "let*" : "let";
  $self->_emit("($let_form ($bindings_str)");
  $self->indent_level($self->indent_level + 1);

  # Track that we have an open let that needs closing
  $self->{_local_let_depth} //= 0;
  $self->{_local_let_depth}++;

  if ($rhs_tmp_cl) {
    my $lhs_cl = "(vector " . join(" ", @vars) . ")";
    $self->_emit("(p-list-= $lhs_cl $rhs_tmp_cl)");
  }
  elsif ($init_idx >= 0 && @vars == 1) {
    # Single array/hash local with init: emit the var as the default return value.
    # local @arr = EXPR as last expression in a sub should return the assigned list.
    # Subsequent statements override this as the actual return value.
    my ($sigil) = ($vars[0] =~ /::([%\@\$])/) ? ($1) : (substr($vars[0], 0, 1));
    if ($sigil eq '@' || $sigil eq '%') {
      $self->_emit("$vars[0]");
    }
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
  my $postfix_op   = '';  # '++' or '--' after variable (no '=')
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
    elsif ($ref eq 'PPI::Token::Operator' && $part->content =~ /^(\+\+|--)$/ && @vars && !$found_assign) {
      $postfix_op = $part->content; last;
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
      # Use box-set so tied init values call FETCH instead of copying the proxy
      $self->_emit("(box-set $cl_var $init_cl)");
    } elsif ($sigil eq '@') {
      # Array: only initialize if there's an explicit init expression
      # Force list context so qw(...) and other list exprs return all elements.
      $self->_emit("(p-array-= $cl_var (let ((*wantarray* t)) (list $init_cl)))") if @init_parts;
    } elsif ($sigil eq '%') {
      # Hash: only initialize if there's an explicit init expression
      $self->_emit("(p-hash-= $cl_var (let ((*wantarray* t)) $init_cl))") if @init_parts;
    }
    $self->_emit("(setf $init_flag t))");
    $self->indent_level($self->indent_level - 1);
  }

  # Emit post-increment/decrement when no initializer (state $z++)
  if ($postfix_op && !$found_assign) {
    my $cl_op = ($postfix_op eq '++') ? 'p-post++' : 'p-post--';
    for my $var (@vars) {
      my $cl_var = $renames->{$var} // $var;
      $self->_emit("($cl_op $cl_var)");
    }
  }

  # The value of a `state $x = EXPR` expression is the CURRENT value of $x
  # (not the init-guard result), so emit the variable as the trailing form for
  # tail/expression position (map/grep block, implicit sub return). Single
  # scalar/array/hash declarations only; list forms left as-is.
  if (!$postfix_op && @vars == 1) {
    my $cl_var = $renames->{$vars[0]} // $vars[0];
    $self->_emit($cl_var);
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
    if ($label) {
      # Standalone label statement: LABEL: → emit as tagbody tag.
      # The ;; pcl-label sentinel lets _wrap_runtime_labels distinguish
      # real generated labels from ":word" patterns inside string literals.
      $self->_emit(":$label  ;; pcl-label");
    } else {
      # Neither block nor keyword found - emit as comment
      my $perl_code = $stmt->content;
      $perl_code =~ s/\n/ /g;
      $self->_emit(";; COMPOUND (unknown) not handled: $perl_code");
      $self->_emit("");
    }
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
    # Always wrap tagbody in NEXT-LABEL catch so that (p-next LABEL) works even
    # without a continue block.  When next LABEL is thrown from an inner function
    # (e.g. eval { next $label }), the throw lands here; when there is a continue
    # block it runs after the catch returns, just as in the continue case.
    $self->_emit("(catch 'pcl::NEXT-$label");
    $self->indent_level($self->indent_level + 1);
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
    $self->_emit(")");  # close catch for NEXT
    $self->indent_level($self->indent_level - 1);
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


# ── Bare-if implicit return (B1) ─────────────────────────────────────────────
#
# Perl rule: the last expression *evaluated* is the return value of a sub.
# For `if (COND) { BODY }` with no else and COND false, COND itself is the
# last thing evaluated, so COND is returned (not undef).
#
# Fix: when the last statement of a sub block is an if/unless without else
# (block-form or postfix), save the condition into a fresh CL variable, and
# return that variable after the if form.

# Returns a fresh CL symbol name for the if-return let binding.
sub _fresh_ret_var {
  my $self = shift;
  $self->{_tail_ret_counter} //= 0;
  return '--pcl-if-ret--' . $self->{_tail_ret_counter}++;
}

# True if $stmt is a compound if/unless statement WITHOUT a final else.
sub _is_if_without_else {
  my ($self, $stmt) = @_;
  return 0 unless ref($stmt) eq 'PPI::Statement::Compound';
  my ($first_word, $has_else);
  for my $child ($stmt->children) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) eq 'PPI::Token::Word') {
      my $w = $child->content;
      $first_word //= $w;
      $has_else = 1 if $w eq 'else';
    }
  }
  return 0 if $has_else;
  return ($first_word // '') eq 'if' || ($first_word // '') eq 'unless';
}

# True if $stmt is a postfix if/unless modifier (PPI::Statement with if/unless word).
sub _is_postfix_if_without_else {
  my ($self, $stmt) = @_;
  return 0 unless ref($stmt) eq 'PPI::Statement';
  for my $child ($stmt->children) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) eq 'PPI::Token::Word') {
      my $w = $child->content;
      return 1 if $w eq 'if' || $w eq 'unless';
    }
  }
  return 0;
}

# Process a single statement in tail position.
# Wraps its result in (setf ret_var ...) so the outer let captures the value.
sub _process_tail_stmt {
  my ($self, $stmt, $ret_var) = @_;

  # Block-form if/unless without else: recurse with same ret_var (no new let)
  if ($self->_is_if_without_else($stmt)) {
    $self->_process_if_tail($stmt, $ret_var);
    return;
  }

  # Postfix if/unless: emit tail form
  if ($self->_is_postfix_if_without_else($stmt)) {
    my $perl_code = $stmt->content;
    $perl_code =~ s/;\s*$//;
    $perl_code =~ s/\n/ /g;
    my @parts = grep {
      my $r = ref($_);
      $r ne 'PPI::Token::Whitespace'
        && $r ne 'PPI::Token::Comment'
        && !($r eq 'PPI::Token::Structure' && $_->content eq ';')
    } $stmt->children;

    my ($modifier_idx, $modifier);
    for my $i (0 .. $#parts) {
      if (ref($parts[$i]) eq 'PPI::Token::Word') {
        my $w = $parts[$i]->content;
        if ($w eq 'if' || $w eq 'unless') {
          $modifier_idx = $i;
          $modifier = $w;
          last;
        }
      }
    }

    if (defined $modifier_idx && $modifier_idx > 0) {
      my @expr_parts = @parts[0 .. $modifier_idx - 1];
      my @cond_parts = @parts[$modifier_idx + 1 .. $#parts];

      if (@cond_parts == 1 && ref($cond_parts[0]) eq 'PPI::Structure::Condition') {
        @cond_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } $cond_parts[0]->children;
      }

      my $expr_cl = $self->_parse_expression(\@expr_parts, $stmt);
      my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt);

      $self->_emit(";; $perl_code");
      if ($modifier eq 'if') {
        $self->_emit("(p-if (setf $ret_var $cond_cl)");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(setf $ret_var $expr_cl)");
        $self->_emit("nil)");
        $self->indent_level($self->indent_level - 1);
      } else {  # unless
        $self->_emit("(p-unless (setf $ret_var $cond_cl)");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(setf $ret_var $expr_cl))");
        $self->indent_level($self->indent_level - 1);
      }
      $self->_emit("");
      return;
    }
    # Fall through to normal emit if we couldn't parse the modifier
  }

  # Simple expression statement: wrap result with setf
  if (ref($stmt) eq 'PPI::Statement' || ref($stmt) eq 'PPI::Statement::Expression') {
    my $perl_code = $stmt->content;
    $perl_code =~ s/;\s*$//;
    $perl_code =~ s/\n/ /g;
    my @parts = grep {
      my $r = ref($_);
      $r ne 'PPI::Token::Whitespace'
        && $r ne 'PPI::Token::Comment'
        && !($r eq 'PPI::Token::Structure' && $_->content eq ';')
    } $stmt->children;
    if (@parts) {
      my $cl = $self->_parse_expression(\@parts, $stmt);
      $self->_emit(";; $perl_code");
      $self->_emit("(setf $ret_var $cl)") if defined $cl;
      $self->_emit("");
    }
    return;
  }

  # Everything else (variable decl, loops, etc.): emit normally.
  # ret_var holds the condition value from the outer if — best-effort.
  $self->_process_element($stmt);
}

# Generate CL for an if/elsif/else chain in tail position.
# Mirrors _generate_if_clauses but wraps the condition (and each branch's
# last expr) so that ret_var always holds the correct return value.
sub _generate_if_tail_clauses {
  my ($self, $clauses, $ret_var) = @_;
  return unless @$clauses;

  my $first = $clauses->[0];
  my $rest  = [@$clauses[1 .. $#$clauses]];

  my $cond_cl = $self->_parse_condition($first->{cond});

  my $cond_perl = $first->{cond} ? $first->{cond}->content : "";
  $cond_perl =~ s/^\s*\(\s*//;
  $cond_perl =~ s/\s*\)\s*$//;
  $cond_perl =~ s/\n/\n;; /g;
  $self->_emit(";; $first->{type} ($cond_perl)");

  # Wrap condition to save its Perl value.
  # For 'unless': save first, then negate for the p-if test.
  my $wrapped_cond;
  if ($first->{type} eq 'unless') {
    $wrapped_cond = "(progn (setf $ret_var $cond_cl) (p-not $ret_var))";
  } else {
    $wrapped_cond = "(setf $ret_var $cond_cl)";
  }

  $self->_emit("(p-if $wrapped_cond");
  $self->indent_level($self->indent_level + 1);

  # Then block
  $self->_emit("(progn");
  $self->indent_level($self->indent_level + 1);
  $self->_with_declarations($first->{block}, sub {
    $self->_process_block_in_tail_context($first->{block}, $ret_var);
  });
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");

  # Else/elsif
  if (@$rest) {
    my $next = $rest->[0];
    if ($next->{type} eq 'else') {
      $self->_emit(";; else");
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_with_declarations($next->{block}, sub {
        $self->_process_block_in_tail_context($next->{block}, $ret_var);
      });
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    } else {
      # elsif: recurse with same ret_var (no new let)
      $self->_generate_if_tail_clauses($rest, $ret_var);
    }
  } else {
    # No else: nil placeholder.  ret_var already holds the last cond value.
    $self->_emit("nil");
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
}

# Process a tail-position if/unless compound statement.
# Collects clauses (same as _process_if_statement) and calls
# _generate_if_tail_clauses — no new let is opened here.
sub _process_if_tail {
  my ($self, $stmt, $ret_var) = @_;

  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  my @clauses;
  my @conditions;
  my $current_type;
  my $current_cond;

  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Word') {
      my $w = $child->content;
      if ($w eq 'if' || $w eq 'elsif' || $w eq 'unless') {
        $current_type = $w;
      } elsif ($w eq 'else') {
        $current_type = 'else';
      }
    } elsif ($ref eq 'PPI::Structure::Condition') {
      $current_cond = $child;
      push @conditions, $child;
    } elsif ($ref eq 'PPI::Structure::Block') {
      push @clauses, {
        type  => $current_type,
        cond  => $current_cond,
        block => $child,
      };
      $current_cond = undef;
    }
  }

  $self->_with_declarations(\@conditions, sub {
    $self->_generate_if_tail_clauses(\@clauses, $ret_var);
  });

  $self->_emit("");
}

# Process a block's contents where the last statement contributes to ret_var.
# Mirrors _process_block but dispatches the last significant statement
# through _process_tail_stmt instead of _process_element.
sub _process_block_in_tail_context {
  my ($self, $block, $ret_var) = @_;

  # Isolate _pending_let_closes so that our flush at the end does not
  # accidentally close let forms opened by an enclosing _emit_scoped_block.
  # Mirrors the save/reset/restore done by _process_block.
  my $saved_pending = $self->{_pending_let_closes};
  $self->{_pending_let_closes} = [];

  $self->environment->push_scope();
  my $start_depth = $self->{_local_let_depth} // 0;

  my @sig      = $block->schildren;
  my $last_sig = @sig ? $sig[-1] : undef;

  my @children = $block->children;
  my %skip;
  for my $i (0 .. $#children) {
    next if $skip{$i};
    my $child = $children[$i];
    my $ref   = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    # Fire the scoped-block hook before each significant statement.
    $self->{_stmt_pre_hook}->($self, $child) if $self->{_stmt_pre_hook};

    if ($ref eq 'PPI::Statement::Compound') {
      my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
      if ($continue) {
        $self->_process_compound_statement($child, $continue);
        $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
        next;
      }
    }

    if (defined $last_sig && $child == $last_sig) {
      $self->_process_tail_stmt($child, $ret_var);
    } else {
      $self->_process_element($child);
    }
  }

  # Flush only the let closes opened within this block's scope.
  while (@{$self->{_pending_let_closes} // []}) {
    pop @{$self->{_pending_let_closes}};
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  my $end_depth = $self->{_local_let_depth} // 0;
  while ($end_depth > $start_depth) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")  ;; end local");
    $self->{_local_let_depth}--;
    $end_depth--;
  }

  $self->environment->pop_scope();

  # Restore the outer pending closes (from enclosing _emit_scoped_block).
  $self->{_pending_let_closes} = $saved_pending;
}

# ─────────────────────────────────────────────────────────────────────────────


# Process a block's contents
sub _process_block {
  my $self  = shift;
  my $block = shift;

  # Isolate _pending_let_closes so that inner _process_block calls (e.g.
  # for if/while/bare block bodies) cannot accidentally flush pending let
  # closes that belong to an enclosing _emit_scoped_block context.
  # Each _process_block call owns its own slice; the outer value is restored
  # after this call finishes (including after flushing any closes we opened).
  my $saved_pending_block     = $self->{_pending_let_closes};
  $self->{_pending_let_closes} = [];

  # Enter new scope for filehandles
  $self->environment->push_scope();

  # Track local let depth at block start
  my $start_depth = $self->{_local_let_depth} // 0;

  # ── Tail-if detection ─────────────────────────────────────────────────────
  # When the last significant statement of a sub block is an if/unless without
  # else (or a postfix if/unless), wrap the block in a let-binding that captures
  # the condition value.  This implements Perl's "last expression evaluated"
  # return semantics for bare-if.
  my ($tail_ret_var, $tail_last_sig, $tail_sig);
  if ($self->environment->in_subroutine > 0) {
    my @sig = $block->schildren;
    # Skip BEGIN/END/INIT/CHECK blocks — they produce no runtime code,
    # so the tail is the last *runtime* significant statement.
    my $last;
    for my $s (reverse @sig) {
      unless (ref($s) eq 'PPI::Statement::Scheduled') {
        $last = $s;
        last;
      }
    }
    if ($last) {
      $tail_sig = $last;  # track for tail_position context propagation
      if ($self->_is_if_without_else($last) || $self->_is_postfix_if_without_else($last)) {
        $tail_ret_var  = $self->_fresh_ret_var();
        $tail_last_sig = $last;
      }
    }
  }
  if ($tail_ret_var) {
    $self->_emit("(let (($tail_ret_var nil))");
    $self->indent_level($self->indent_level + 1);
  }
  # ─────────────────────────────────────────────────────────────────────────

  my @children = $block->children;
  my %skip;

  # Hoist named sub definitions that are called BEFORE their definition in the block.
  # Perl compiles all named subs at compile time so they are callable anywhere in their
  # lexical scope.  In a let-bound block p-sub is emitted inline and evaluated
  # sequentially — calls to the sub BEFORE the p-sub form would use the forward stub
  # (returns nil).  Only hoist when actually needed (called before defined), to avoid
  # disturbing package context for subs already defined before their calls.
  # Only fires when _let_bound_vars is non-empty (otherwise subs already go to
  # declarations/definitions and are pre-hoisted).
  if (%{$self->{_let_bound_vars} // {}}) {
    # First pass: find which sub names appear as word tokens before their FIRST definition.
    # Only the first definition of a sub is ever hoisted — later redefinitions stay in order
    # so that last-definition-wins semantics are preserved.
    my %words_seen;    # token content => 1 if seen in a non-sub statement
    my %sub_defined;   # sub_name => 1 once we've seen the first definition
    my %needs_hoist;   # child index => 1 for subs that need hoisting
    for my $i (0 .. $#children) {
      my $child = $children[$i];
      my $ref = ref($child);
      next if $ref eq 'PPI::Token::Whitespace' || $ref eq 'PPI::Token::Comment';
      if ($ref eq 'PPI::Statement::Sub') {
        my ($sub_name, $has_block) = ('', 0);
        for my $sc ($child->children) {
          my $scref = ref($sc);
          if ($scref eq 'PPI::Token::Word' && $sc->content ne 'sub'
              && $sc->content ne 'my' && $sc->content ne 'our' && $sc->content ne 'state') {
            $sub_name ||= $sc->content;
          }
          $has_block = 1 if $scref eq 'PPI::Structure::Block';
        }
        if ($sub_name && $has_block) {
          # Hoist only if: (a) called before this definition, and (b) no earlier definition
          if ($words_seen{$sub_name} && !$sub_defined{$sub_name}) {
            $needs_hoist{$i} = 1;
          }
          $sub_defined{$sub_name} = 1;
        }
      } else {
        # Record all word tokens as potential calls
        my $words = $child->find('PPI::Token::Word') || [];
        for my $w (@$words) {
          $words_seen{$w->content} = 1;
        }
      }
    }
    # Second pass: hoist only subs that need it
    for my $i (sort { $a <=> $b } keys %needs_hoist) {
      $self->_process_element($children[$i]);
      $skip{$i} = 1;
    }
  }

  for my $i (0 .. $#children) {
    next if $skip{$i};
    my $child = $children[$i];
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    # Fire the scoped-block hook (set by _emit_scoped_block) before each
    # significant statement.  The hook opens (let ...) for any 'my'
    # declarations associated with this statement's PPI object.
    $self->{_stmt_pre_hook}->($self, $child) if $self->{_stmt_pre_hook};

    # Lookahead: bare block followed by continue { } as sibling
    if ($ref eq 'PPI::Statement::Compound') {
      my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
      if ($continue) {
        $self->_process_compound_statement($child, $continue);
        $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
        next;
      }
    }

    # Intercept last significant child when tail-if is active
    if ($tail_ret_var && defined $tail_last_sig && $child == $tail_last_sig) {
      $self->_process_tail_stmt($child, $tail_ret_var);
      next;
    }

    # Set tail_position so gen_funcall/gen_methodcall propagate *wantarray*
    # instead of overriding it — allowing context to flow from the call site.
    my $is_tail = defined $tail_sig && $child == $tail_sig;
    $self->environment->tail_position(1) if $is_tail;
    $self->_process_element($child);
    $self->environment->tail_position(0) if $is_tail;
  }

  # Flush let forms opened by _emit_scoped_block's hook (innermost first).
  # Must happen here, inside _process_block, so the closes land BEFORE any
  # tagbody/:next structure that $emit_body emits after _process_block returns.
  while (@{$self->{_pending_let_closes} // []}) {
    pop @{$self->{_pending_let_closes}};
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  # Close any let forms opened by local declarations in this block
  my $end_depth = $self->{_local_let_depth} // 0;
  while ($end_depth > $start_depth) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")  ;; end local");
    $self->{_local_let_depth}--;
    $end_depth--;
  }

  # Close the tail let (after local lets, so ret_var is the outermost form)
  if ($tail_ret_var) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit("$tail_ret_var)");
  }

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();

  # Restore the outer _pending_let_closes (saved at the top of this call).
  $self->{_pending_let_closes} = $saved_pending_block;
}


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
    $self->_emit("(let ((\@_ (p-flatten-args %_args))");
    $self->_emit("      (*pcl-caller-wantarray* *wantarray*))");
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
    }, 1);  # is_sub_body=1: enable two-phase scoped block
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
    # Definitions (BEGIN blocks) and declarations (our $var defvars) are hoisted
    # to the real buckets rather than inlined — otherwise (eval-when ...) ends up
    # as the first argument to p-funcall-ref, making NIL the function ref.
    my $temp = $self->_sections->[0];
    my @hoisted_defs  = @{$temp->{definitions}};
    my @hoisted_decls = @{$temp->{declarations}};
    my @lines = (
      @{$temp->{preamble}},
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
    # Re-emit hoisted declarations (our $var defvars, etc.) into the real sections
    if (@hoisted_decls) {
      my $section = $self->_sections->[$self->_cur_section];
      push @{$section->{'declarations'}}, @hoisted_decls;
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

  # Find last significant child so we can set tail_position correctly.
  # This prevents the VOID_CTX wrap (in _process_expression_statement) from
  # incorrectly wrapping the lambda's return value in map/grep/sort blocks.
  my @sig = grep {
    my $r = ref($_);
    $r ne 'PPI::Token::Whitespace' && $r ne 'PPI::Token::Comment'
  } $block->children;
  my $last_sig = @sig ? $sig[-1] : undef;

  # Process block contents
  my $has_content = 0;
  for my $child ($block->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    my $is_tail = defined $last_sig && $child == $last_sig;
    $self->environment->tail_position(1) if $is_tail;
    $self->_process_element($child);
    $self->environment->tail_position(0) if $is_tail;
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


# Return true if the elements contain any standalone bare label statements
# (PPI::Statement::Compound with only a PPI::Token::Label, no keyword/block).
# Only checks the immediate children — does not recurse into sub-blocks.
sub _has_bare_labels_shallow {
  my $self = shift;
  my $elements = shift;
  my @top;
  if (ref($elements) eq 'ARRAY') {
    @top = @$elements;
  } elsif (ref($elements) && $elements->can('children')) {
    @top = $elements->children;
  } else {
    return 0;
  }
  for my $elem (@top) {
    next unless ref($elem);
    # Direct bare label: the element itself is a compound with only a label
    if (ref($elem) eq 'PPI::Statement::Compound') {
      my @sig = grep { ref($_) && ref($_) !~ /Whitespace|Comment/ } $elem->children;
      return 1 if @sig == 1 && ref($sig[0]) eq 'PPI::Token::Label';
    }
    # When the elements arg is a block, its direct children are statements
    next unless $elem->can('children');
    for my $child ($elem->children) {
      next unless ref($child) eq 'PPI::Statement::Compound';
      my @sig = grep { ref($_) && ref($_) !~ /Whitespace|Comment/ } $child->children;
      return 1 if @sig == 1 && ref($sig[0]) eq 'PPI::Token::Label';
    }
  }
  return 0;
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

    # Recurse into nested elements, but NOT into:
    #   - Named sub definitions (PPI::Statement::Sub)
    #   - BEGIN/END/etc blocks (PPI::Statement::Scheduled)
    #   - Anonymous sub bodies: PPI::Structure::Block whose prev sibling is 'sub'
    #   - eval { } blocks: PPI::Structure::Block whose prev sibling is 'eval'
    #     (my vars inside eval { } are scoped to that eval; hoisting them to the
    #     enclosing let would shadow outer vars of the same name)
    # For bare blocks (no prev non-whitespace sibling): recurse but only keep
    #   'state' declarations — 'my' vars in bare blocks are scoped to the block
    #   by _process_bare_block/_with_declarations and must NOT be hoisted to the
    #   enclosing sub level (would shadow same-name package globals outside).
    if ($ref && $child->can('children')
        && $ref ne 'PPI::Statement::Sub'
        && $ref ne 'PPI::Statement::Scheduled'
        && !($ref eq 'PPI::Structure::Block' && do {
               my $prev = $child->sprevious_sibling;
               $prev && ref($prev) eq 'PPI::Token::Word'
                     && ($prev->content eq 'sub' || $prev->content eq 'eval')
             })) {
      my $is_bare_block = $ref eq 'PPI::Structure::Block' && do {
        my $prev = $child->sprevious_sibling;
        !$prev;
      };
      my $inner = $self->_find_all_declarations($child);
      if ($is_bare_block) {
        push @decls, grep { $_->{type} eq 'state' } @$inner;
      } else {
        push @decls, @$inner;
      }
    }
  }

  return \@decls;
}

# Return true if the block contains any directly-nested named sub statements.
# Used by _process_sub_statement to decide whether to use defvar (global) vs
# let (lexical) for state variables: if inner named subs exist, they must be
# hoisted to the definitions bucket and need to access state vars globally.
sub _block_has_inner_named_subs {
  my ($self, $block) = @_;
  for my $child ($block->children) {
    next unless ref($child) eq 'PPI::Statement::Sub';
    for my $c ($child->children) {
      my $ref = ref($c);
      next if $ref eq 'PPI::Token::Whitespace';
      next if $ref eq 'PPI::Token::Word' && $c->content =~ /^(sub|my|our|state)$/;
      return 1 if $ref eq 'PPI::Token::Word';  # first non-reserved word = name
      last;
    }
  }
  return 0;
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
    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
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

# Build the $outer scope hashref passed to BlockAnalyzer::analyze.
# Collects let-bound, state-renamed, constant, and our variables from the
# current environment so BlockAnalyzer can distinguish local from outer refs.
sub _current_outer_scope {
  my ($self) = @_;
  my %outer;
  for my $v (keys %{$self->{_let_bound_vars} // {}}) {
    $outer{$v} = { type => 'my', cl_name => $v };
  }
  my $renames = $self->environment->state_var_renames // {};
  for my $v (keys %$renames) {
    $outer{$v} = { type => 'state', cl_name => $renames->{$v} };
  }
  return \%outer;
}

# CL initialization expression for a let-binding by sigil.
sub _let_init {
  my ($sigil) = @_;
  return '(make-array 0 :adjustable t :fill-pointer 0)' if $sigil eq '@';
  return "(make-hash-table :test #'equal)"              if $sigil eq '%';
  return '(make-p-box nil)';
}

# Scoped block codegen — opens nested (let ...) forms at the exact statement
# where each 'my' declaration first appears, rather than hoisting them all to
# the top of the block.  Called by _with_declarations when $elements is a
# PPI::Structure::Block.  Sets _stmt_pre_hook so _process_block fires the hook
# before each significant statement.
sub _emit_scoped_block {
  my ($self, $analysis, $emit_body) = @_;

  my $decls      = $analysis->{declarations};
  my $vars       = $analysis->{vars};
  my $state_vars = $self->{_current_state_vars} // {};

  # Collect globally unique 'my' vars (excluding state vars and vars already
  # let-bound by an enclosing _emit_scoped_block, preserving order).
  my $already_bound = $self->{_let_bound_vars} // {};
  my (%seen_var);
  my @all_my_vars = grep { !$seen_var{$_}++ && !$state_vars->{$_}
                                             && !$already_bound->{$_} }
                    map  { @{$_->{vars}} }
                    grep { $_->{decl_type} eq 'my' } @$decls;

  # Nothing to scope? Emit body, but still isolate _pending_let_closes so that
  # inner _process_block calls (e.g. then/else blocks of a nested if) do not
  # accidentally flush pending closes that belong to an enclosing scoped block.
  unless (@all_my_vars) {
    my $saved_pending = $self->{_pending_let_closes};
    $self->{_pending_let_closes} = [];
    $emit_body->();
    $self->{_pending_let_closes} = $saved_pending;
    return;
  }

  # Compute renames: closure-captured vars → __lex__N, case-collision → __case__N.
  my (%new_renames, %old_renames, %cl_sym_seen);
  my $existing = $self->environment->state_var_renames // {};
  for my $var (@all_my_vars) {
    my $vinfo = $vars->{$var} // {};
    if ($vinfo->{captured}) {
      my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
      ($sigil, $bare) = ('$', $var) unless defined $bare;
      (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
      my $u = sprintf('%s%s__lex__%d', $sigil, $slug, ++$lex_var_counter);
      $new_renames{$var} = $u;
      $old_renames{$var} = $existing->{$var};
    }
    my $cl_name = $new_renames{$var} // $var;
    my $lc = lc($cl_name);
    if ($cl_sym_seen{$lc}) {
      my ($sigil, $bare) = ($cl_name =~ /^([\$\@\%])(.+)$/);
      ($sigil, $bare) = ('$', $cl_name) unless defined $bare;
      (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
      my $r = sprintf('%s%s__case__%d', $sigil, $slug, ++$lex_var_counter);
      $new_renames{$var} = $r;
      $old_renames{$var} //= $existing->{$var};
      $cl_sym_seen{lc($r)} = $var;
    } else {
      $cl_sym_seen{$lc} = $var;
    }
  }

  # Apply renames to the environment.
  my ($saved_env_renames, $saved_scope_renames);
  $saved_scope_renames = $self->{_current_scope_new_renames};
  if (%new_renames) {
    $saved_env_renames = $self->environment->state_var_renames // {};
    $self->environment->state_var_renames({ %$saved_env_renames, %new_renames });
    $self->{_current_scope_new_renames} = \%new_renames;
    $self->{_current_scope_old_renames} = \%old_renames;
  }

  # Build ppi_stmt_key → [vars] map for the hook.
  # Vars for the same ppi_stmt are batched into one let form.
  my (%vars_at_ppi, %seen_at_ppi);
  for my $d (@$decls) {
    next if $d->{decl_type} ne 'my';
    my $key = "$d->{ppi_stmt}";
    for my $var (@{$d->{vars}}) {
      next if $state_vars->{$var} || $seen_at_ppi{$key}{$var}++;
      push @{$vars_at_ppi{$key}}, $var;
    }
  }

  # Install the per-statement hook.  _process_block calls it with each child
  # element before dispatching.  The hook opens (let ...) for any 'my' vars
  # declared at that statement's position.
  #
  # Pending closes are stored on $self->{_pending_let_closes} (not a local var)
  # so that _process_block can flush them at the end of its statement loop,
  # BEFORE any tagbody/:next structure emitted by $emit_body closes things up.
  # _emit_scoped_block saves/restores so nested scopes don't interfere.
  my $saved_hook        = $self->{_stmt_pre_hook};
  my $old_let_vars      = $self->{_let_bound_vars};
  my $saved_pending     = $self->{_pending_let_closes};
  $self->{_let_bound_vars}     = { %{$old_let_vars // {}} };
  $self->{_pending_let_closes} = [];

  $self->{_stmt_pre_hook} = sub {
    my ($parser, $child) = @_;
    my $key = "$child";
    return unless $vars_at_ppi{$key};

    my @bindings;
    for my $var (@{$vars_at_ppi{$key}}) {
      my $lv    = $new_renames{$var} // $var;
      my $sigil = substr($lv, 0, 1);
      push @bindings, "($lv " . _let_init($sigil) . ")";
      $parser->{_let_bound_vars}{$lv} = 1;
    }
    return unless @bindings;
    $parser->_emit("(let (" . join(" ", @bindings) . ")");
    $parser->indent_level($parser->indent_level + 1);
    push @{$parser->{_pending_let_closes}}, 1;
  };

  $emit_body->();

  # Restore state.  Pending closes are flushed inside _process_block (at end
  # of the statement loop, before any tagbody/:next structure emitted by
  # $emit_body).  Nothing left to close here.
  $self->{_let_bound_vars}     = $old_let_vars;
  $self->{_pending_let_closes} = $saved_pending;
  $self->{_stmt_pre_hook}      = $saved_hook;
  if (%new_renames) {
    $self->environment->state_var_renames($saved_env_renames);
    $self->{_current_scope_new_renames} = $saved_scope_renames;
    delete $self->{_current_scope_old_renames};
  }
}

# Common helper: wrap emitted code with let for any 'my' declarations
# Usage: $self->_with_declarations($ppi_elements, sub { ... emit code ... });
# $ppi_elements can be a single PPI element or arrayref of elements to scan
sub _with_declarations {
  my $self = shift;
  my $elements = shift;  # PPI element(s) to scan for declarations
  my $emit_body = shift; # Callback to emit the body code
  my $is_sub_body = shift // 0;  # 1 only for direct sub body blocks

  # Phase 2: for DIRECT sub body blocks only, use the two-phase scoping fix
  # (_emit_scoped_block) which opens let-bindings at the exact statement where
  # each 'my' declaration first appears, rather than hoisting everything to
  # the top of the block.
  #
  # IMPORTANT: restrict to $is_sub_body=1, set only from _process_sub_statement.
  # if/else/while/bare blocks INSIDE subs must NOT use _emit_scoped_block:
  # those inner blocks share their parent sub's rename map, and running BlockAnalyzer
  # on them re-fires closure-capture detection and creates a spurious nested let that
  # shadows already-bound outer vars (e.g. breaks closure.t bizz() test).
  #
  # At the top level (in_subroutine=0), 'my' vars are defvar'd as dynamic variables,
  # and inline-let semantics interact badly with defvar + _process_my_toplevel_declaration.
  if (ref($elements) eq 'PPI::Structure::Block'
      && $self->environment->in_subroutine > 0
      && $is_sub_body) {
    require Pl::BlockAnalyzer;
    my $outer    = $self->_current_outer_scope();
    my $analysis = Pl::BlockAnalyzer->analyze($elements, $outer);

    # Supplemental hoisting: _find_all_declarations does a deep recursive search
    # and finds 'my' vars declared inside expressions (e.g. open(my $fh, '>', ...)).
    # BlockAnalyzer only sees statement-level PPI::Statement::Variable nodes.
    # Any var found by the deep search but not by BlockAnalyzer needs a hoisted
    # flat-let at the top of the block so it is visible to all subsequent statements.
    my $state_vars    = $self->{_current_state_vars} // {};
    my $already_bound = $self->{_let_bound_vars}     // {};
    my %stmt_level    = map  { $_ => 1 }
                        grep { !$state_vars->{$_} && !$already_bound->{$_} }
                        map  { @{$_->{vars}} }
                        grep { $_->{decl_type} eq 'my' } @{$analysis->{declarations}};
    my $deep_decls    = $self->_find_all_declarations($elements);
    my (@hoisted, %seen_hoist);
    for my $d (@$deep_decls) {
      next unless $d->{type} eq 'my';
      my $v = $d->{var};
      next if $seen_hoist{$v}++ || $stmt_level{$v}
           || $state_vars->{$v}  || $already_bound->{$v};
      push @hoisted, $v;
    }

    if (@hoisted) {
      # Open a hoisted flat-let for expression-level my vars, wrapping the
      # inline-let scoped block. This ensures vars like $fh (from open(my $fh,...))
      # are visible to all subsequent statements in the block.
      my $bindings = join(" ", map {
        my $sigil = substr($_, 0, 1);
        "($_ " . _let_init($sigil) . ")"
      } @hoisted);
      $self->_emit("(let ($bindings)");
      $self->indent_level($self->indent_level + 1);
      my $old_let = $self->{_let_bound_vars};
      $self->{_let_bound_vars} = { %{$old_let // {}}, map { $_ => 1 } @hoisted };
      $self->_emit_scoped_block($analysis, $emit_body);
      $self->{_let_bound_vars} = $old_let;
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    } else {
      $self->_emit_scoped_block($analysis, $emit_body);
    }
    return;
  }

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

    # CL reads unquoted symbols case-insensitively (upcase mode), so Perl variables
    # that differ only in case (e.g. $T and $t) map to the same CL symbol and cause
    # "variable occurs more than once in the LET" errors. Detect these collisions and
    # rename the later occurrence to $name__uc__ to make it distinct.
    my %cl_sym_seen;  # lc(cl_name) → first perl var with that cl symbol
    for my $var (@my_vars) {
      my $cl_name = $new_renames{$var} // $var;
      my $lc = lc($cl_name);
      if (exists $cl_sym_seen{$lc}) {
        # Collision: $var has same CL symbol as $cl_sym_seen{$lc}
        # Rename the later one (whichever $var this is)
        my ($sigil, $bare) = ($cl_name =~ /^([\$\@\%])(.+)$/);
        $sigil //= '$'; $bare //= $cl_name;
        (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
        my $renamed = sprintf('%s%s__case__%d', $sigil, $slug, ++$lex_var_counter);
        $new_renames{$var} = $renamed;
        $old_renames{$var} //= $existing->{$var};
        $cl_sym_seen{lc($renamed)} = $var;
      } else {
        $cl_sym_seen{$lc} = $var;
      }
    }
  }

  # Wrap in let if we have declarations
  if (@my_vars) {
    # Pre-scan: for 'my @arr = EXPR' / 'my (LIST) = EXPR' statements where the RHS
    # self-references a declared array/hash variable, pre-evaluate EXPR in the let
    # binding init position so the outer binding is still visible.
    # Single-var my @arr=EXPR: use full EXPR as init, mark to skip body assignment.
    # Multi-var  my(LIST)=EXPR: pre-init @arr to a copy of its outer value only;
    #   keep body so p-list-= can do the actual list destructuring from the correct source.
    my %arr_rhs_inits;   # let_var => cl_init_string
    my %skip_body_vars;  # perl_var => 1  (skip body emit for single-var case)
    {
      my %my_ah_set = map { $_ => 1 } grep { /^[@%]/ } @my_vars;
      if (%my_ah_set) {
        my @top_stmts;
        if (ref($elements) eq 'PPI::Structure::Block') {
          @top_stmts = grep { ref($_) eq 'PPI::Statement::Variable' } $elements->children;
        } elsif (ref($elements) eq 'ARRAY') {
          @top_stmts = grep { ref($_) eq 'PPI::Statement::Variable' } @$elements;
        }
        for my $chk_stmt (@top_stmts) {
          my @sp = grep { ref($_) ne 'PPI::Token::Whitespace' &&
                          !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';') }
                   $chk_stmt->children;
          next unless @sp >= 3;
          next unless ref($sp[0]) eq 'PPI::Token::Word' && $sp[0]->content eq 'my';
          my $eq_idx = -1;
          my @decl_ah;
          my $is_single = 0;
          if (ref($sp[1]) eq 'PPI::Token::Symbol') {
            my $v = $sp[1]->content;
            push @decl_ah, $v if $my_ah_set{$v};
            $is_single = 1;
            for my $i (2 .. $#sp) {
              if (ref($sp[$i]) eq 'PPI::Token::Operator' && $sp[$i]->content eq '=') {
                $eq_idx = $i; last;
              }
            }
          } elsif (ref($sp[1]) eq 'PPI::Structure::List') {
            # List children may be wrapped in PPI::Statement::Expression — use find()
            my $found = $sp[1]->find('PPI::Token::Symbol') || [];
            for my $lv (@$found) {
              my $v = $lv->content;
              push @decl_ah, $v if $my_ah_set{$v};
            }
            for my $i (2 .. $#sp) {
              if (ref($sp[$i]) eq 'PPI::Token::Operator' && $sp[$i]->content eq '=') {
                $eq_idx = $i; last;
              }
            }
          }
          next if $eq_idx < 0 || !@decl_ah;
          my @rhs_p = @sp[$eq_idx+1 .. $#sp];
          next unless @rhs_p;
          # Skip double-my (e.g. my @x = my @x = qw(...)) — existing code handles it
          next if grep { ref($_) eq 'PPI::Token::Word' && $_->content eq 'my' } @rhs_p;
          # Collect all Symbol tokens from RHS (including inside nested structures)
          my @rhs_syms;
          for my $rp (@rhs_p) {
            if (ref($rp) eq 'PPI::Token::Symbol') {
              push @rhs_syms, $rp->content;
            } elsif ($rp->can('find')) {
              my $found = $rp->find('PPI::Token::Symbol') || [];
              push @rhs_syms, map { $_->content } @$found;
            }
          }
          my %rhs_sym_set = map { $_ => 1 } @rhs_syms;
          my @self_ref = grep { $rhs_sym_set{$_} } @decl_ah;
          next unless @self_ref;
          if ($is_single) {
            my $var     = $self_ref[0];
            my $rhs_cl  = $self->_parse_expression(\@rhs_p, $chk_stmt) // 'nil';
            my $let_var = $new_renames{$var} // $var;
            my $sigil   = substr($var, 0, 1);
            my $copyfn  = $sigil eq '@' ? 'p-copy-array' : 'p-copy-hash';
            $arr_rhs_inits{$let_var} = "($copyfn (let ((*wantarray* t)) $rhs_cl))";
            $skip_body_vars{$var} = 1;
          } else {
            for my $var (@self_ref) {
              my $sigil   = substr($var, 0, 1);
              my $copyfn  = $sigil eq '@' ? 'p-copy-array' : 'p-copy-hash';
              my $outer   = $old_renames{$var} // $var;
              my $let_var = $new_renames{$var} // $var;
              $arr_rhs_inits{$let_var} = "($copyfn (let ((*wantarray* t)) $outer))";
            }
          }
        }
      }
    }

    # Build let bindings using the (possibly renamed) CL variable names
    my $bindings = join(" ", map {
      my $let_var = $new_renames{$_} // $_;
      my $sigil = substr($let_var, 0, 1);
      my $init = exists $arr_rhs_inits{$let_var} ? $arr_rhs_inits{$let_var}
               : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
               : $sigil eq '%' ? "(make-hash-table :test #'equal)"
               :                 '(make-p-box nil)';
      "($let_var $init)"
    } @my_vars);
    $self->_emit("(let ($bindings)");
    $self->indent_level($self->indent_level + 1);

    # Track renamed/original vars as let-bound so _emit replaces p-scalar-= with
    # p-my-= (box-set), preventing the proclaim-special side-effect that would
    # turn future let bindings from lexical to dynamic and break closure capture.
    #
    # HAZARD: _let_bound_vars is critical for correctness of closure capture.
    # p-scalar-= has a side effect: (unless (boundp ',place) (proclaim '(special ,place)))
    # which converts a variable to a CL special (dynamic) on its first write. Once
    # special, ALL let-bindings of that name become dynamic forever in this image —
    # closures capture the symbol (which is nil after the loop) rather than the
    # per-iteration value. p-my-= (box-set) skips this proclaim, preserving lexicality.
    # If you add a new let-binding path, you MUST update _let_bound_vars accordingly.
    my $old_let_vars = $self->{_let_bound_vars};
    my @bound_names = map { $new_renames{$_} // $_ } @my_vars;
    $self->{_let_bound_vars} = { %{$old_let_vars // {}}, map { $_ => 1 } @bound_names };

    # Apply new renames to environment so ExprToCL emits the unique CL names.
    # Also expose them via _current_scope_new_renames for _process_variable_statement
    # to split RHS parsing (handles 'my $i = $i + 1' shadowing correctly).
    #
    # Shadow removal: if any let-bound 'my' var is also in state_var_renames (e.g.
    # 'my $x' in 'foreach my $x' after an outer 'state $x' registered a rename),
    # remove it from the map so the let binding (not the state defvar) is used inside.
    my $saved_env_renames;
    my $saved_scope_renames = $self->{_current_scope_new_renames};
    my $cur_env_renames = $self->environment->state_var_renames // {};
    my %shadowed_state = map { $_ => 1 }
                         grep { exists $cur_env_renames->{$_} }
                         @my_vars;
    if (%new_renames || %shadowed_state) {
      $saved_env_renames = $cur_env_renames;
      my %merged = %$cur_env_renames;
      delete @merged{keys %shadowed_state};    # remove state renames shadowed by let
      %merged = (%merged, %new_renames);       # apply closure-capture renames
      $self->environment->state_var_renames(\%merged);
      if (%new_renames) {
        $self->{_current_scope_new_renames} = \%new_renames;
        $self->{_current_scope_old_renames} = \%old_renames;
      }
    }

    # Save/restore _my_binding_init_vars so nested _with_declarations calls don't interfere.
    # REPLACE (don't merge) with this block's skip set: inner blocks that create a new
    # let for @bee must NOT inherit the outer block's skip flag for @bee — the inner let
    # has its own init and should not inherit the skip from an outer block's let.
    my $old_skip_body = $self->{_my_binding_init_vars};
    $self->{_my_binding_init_vars} = \%skip_body_vars;

    $emit_body->();

    $self->{_my_binding_init_vars} = $old_skip_body;

    # Restore rename map
    if (%new_renames || %shadowed_state) {
      $self->environment->state_var_renames($saved_env_renames);
      if (%new_renames) {
        $self->{_current_scope_new_renames} = $saved_scope_renames;
        delete $self->{_current_scope_old_renames};
      }
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
  $cond_cl =~ s/^\s+//;  # generate() prepends indentation; strip it for regex checks below

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

  # Perl auto-defined insertion: while ($x = FUNC) terminates when FUNC returns undef,
  # not when it returns a false-but-defined value like "0".
  # Functions: each, readdir, readline (<FH>), glob.
  # Patterns:
  #   (p-scalar-= $var (p-each/readdir/readline/glob ...)) → (progn COND (p-defined $var))
  #   (p-my-= $var (p-each/readdir/readline/glob ...))     → same
  #   (p-setf (p-gethash/aref ...) (p-each/readdir/...))  → (p-defined COND)
  #   Bare (p-readdir/p-glob ...)   → (progn (p-setf $_ COND) (p-defined $_))
  #   Bare (p-each ...)             → same (sets $_ to each's return value)
  if ($keyword ne 'until') {
    my $auto_pat = 'p-each|p-readdir|p-readline|p-glob';
    if ($cond_cl =~ /^\(p-(?:scalar|my)-=\s+(\$\S+)\s+\((?:$auto_pat)\b/) {
      my $var = $1;
      $cond_cl = "(progn $cond_cl (p-defined $var))";
    } elsif ($cond_cl =~ /^\(p-setf\s+\(p-(?:gethash|aref)\b.*\((?:$auto_pat)\b/) {
      $cond_cl = "(p-defined $cond_cl)";
    } elsif ($cond_cl =~ /^\(p-setf\s+\$_\s+\((?:$auto_pat)\b/) {
      # (p-setf $_ (p-readline ...)) — implicit $_ assign, terminate on undef
      $cond_cl = "(progn $cond_cl (p-defined \$_))";
    } elsif ($cond_cl =~ /^\((?:$auto_pat)\b/) {
      # Bare call: assign to $_ and use defined-check (Perl's implicit $_ aliasing)
      $cond_cl = "(progn (p-setf \$_ $cond_cl) (p-defined \$_))";
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
    if (@parts) {
      $cond_cl = $self->_parse_expression(\@parts, $stmt) // 't';
      # Perl special case: for(; $k = each COLL ;) is treated as defined()
      # Prevents loop exit when each returns index 0 (falsy).
      if ($cond_cl =~ /^\(p-(?:scalar|my)-=\s+(\$\S+)\s+\(p-each\b/) {
        my $var = $1;
        $cond_cl = "(progn $cond_cl (p-defined $var))";
      }
    }
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


# If a foreach list is a single aliasable lvalue, return (FROM-HEAD, TO-HEAD) so
# the caller can rewrite the generated call head to its box-returning form, making
# the loop variable alias the live container (write-through).  Otherwise ().
# Two shapes are recognised (AST-level, per the codegen-style preference — inspect
# the PPI nodes, don't pattern-match the generated CL):
#   - a magic-lvalue builtin call: substr(...) / pos(...) / vec(...)
#     -> p-substr -> p-substr-lvalue-cell, etc.  (Word + argument-List)
#   - a single hash/array ELEMENT: $h{k} / $a[i]
#     -> p-gethash -> p-gethash-box  /  p-aref -> p-aref-box  (Symbol + Subscript)
# A two-part match guards against multi-element lists like `for (substr(...), $y)`.
# Slices (@a[...], @h{...}) and `values %h` are intentionally NOT handled here —
# they flatten through the shared copy machinery; see docs/foreach-aliasing.md.
sub _foreach_alias_rewrite {
  my ($list_parts) = @_;
  my @sig = grep { ref($_) ne 'PPI::Token::Whitespace' } @$list_parts;
  # A sole list element arrives wrapped in a PPI::Statement (or Expression) —
  # `for (substr($x,1,3))` gives one PPI::Statement('substr($x,1,3)'). Unwrap it.
  while (@sig == 1
         && (ref($sig[0]) eq 'PPI::Statement'
             || ref($sig[0]) eq 'PPI::Statement::Expression')) {
    @sig = grep { ref($_) ne 'PPI::Token::Whitespace' } $sig[0]->children;
  }
  return () unless @sig == 2;

  # Magic-lvalue builtin call: Word + argument-List.
  if (ref($sig[0]) eq 'PPI::Token::Word'
      && ref($sig[1]) eq 'PPI::Structure::List') {
    my %head = (substr => 'p-substr', pos => 'p-pos', vec => 'p-vec');
    my $h = $head{ $sig[0]->content } or return ();
    return ($h, "$h-lvalue-cell");
  }

  # Scalar element of a named hash/array: $-sigil Symbol + Subscript.
  # {k} -> hash element (p-gethash-box); [i] -> array element (p-aref-box).
  if (ref($sig[0]) eq 'PPI::Token::Symbol'
      && $sig[0]->content =~ /^\$/
      && ref($sig[1]) eq 'PPI::Structure::Subscript') {
    my $sub = $sig[1]->content;
    return ('p-gethash', 'p-gethash-box') if $sub =~ /^\{/;
    return ('p-aref',    'p-aref-box')    if $sub =~ /^\[/;
  }

  return ();
}

# Process foreach-style loop: for/foreach VAR (LIST) { }
sub _process_foreach_loop {
  my $self  = shift;
  my $stmt  = shift;
  my $block = shift;
  my $label = shift;  # Optional loop label
  my $continue_block = shift;  # Optional continue block

  my $loop_var;
  my $loop_var_is_my = 0;  # true when declared 'for my $var' (Perl lexical)
  my @list_parts;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word' && $child->content eq 'my') {
      $loop_var_is_my = 1;
    }
    elsif ($ref eq 'PPI::Token::Symbol' && !$loop_var) {
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
  # 'for my $var' declares a Perl lexical — it must never be defvar'd as a package global.
  # Record it so _insert_variable_forward_declarations can skip the defvar.
  if ($loop_var_is_my && $loop_var ne '$_') {
    $self->{_lexical_foreach_vars}{$loop_var} = 1;
  }

  # If the loop variable has been renamed for closure capture (e.g. $x → $x__lex__N),
  # the p-foreach form must use the renamed symbol so each iteration's lambda closes
  # over the correct per-iteration binding, not the outer let's initial binding.
  # IMPORTANT: only apply __lex__ renames here. State-variable renames (state__toplevel__
  # or state__subname__) must NOT be applied to loop variables — the loop variable is a
  # fresh lexical binding, not the state variable itself.
  my $renames = $self->environment->state_var_renames // {};
  my $candidate = $renames->{$loop_var};
  my $cl_loop_var = (defined $candidate && $candidate =~ /__lex__\d+$/)
                  ? $candidate : $loop_var;
  # Also track renamed name as lexical foreach var to skip defvar generation
  if ($cl_loop_var ne $loop_var) {
    $self->{_lexical_foreach_vars}{$cl_loop_var} = 1;
  }

  my $list_cl = @list_parts
    ? ($self->_parse_expression(\@list_parts, $stmt, 1) // "(list)")  # 1 = LIST_CTX
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

  # foreach-aliasing of a single lvalue: `for (substr($x,1,3)) { $_ = ... }` or
  # `for ($h{k}) { $_ = ... }` must bind $_ to the live lvalue (substr window /
  # hash or array slot) so writing $_ writes through — matching how `for (@a) {
  # $_ = ... }` aliases array elements.  A plain rvalue form ((p-substr ...) /
  # (p-gethash ...)) yields a VALUE in a fresh box, so the write is lost.  Detect
  # the shape at the AST level and rewrite the (one) generated call head to its
  # box-returning form; %p-flatten-for-list keeps the single box and p-foreach
  # binds $_ to it.  The outer call appears before its args, so the first
  # occurrence is the right one; the trailing space avoids matching e.g.
  # (p-substr-ref / (p-gethash-box .
  if (my ($from, $to) = _foreach_alias_rewrite(\@list_parts)) {
    $list_cl =~ s/\(\Q$from\E /($to /;
  }

  # Build label argument if present
  my $label_arg = $label ? " :label $label" : "";

  $self->_emit("(p-foreach ($cl_loop_var $list_cl)$label_arg");
  $self->indent_level($self->indent_level + 1);
  if ($block) {
    # The foreach loop variable creates a fresh lexical binding that shadows any
    # state_var_rename for the same name (e.g. 'foreach my $x' after 'state $x').
    # Temporarily remove the rename so body expressions use the loop's $x, not the defvar.
    my $saved_loop_var_rename;
    my $cur_renames = $self->environment->state_var_renames // {};
    if (exists $cur_renames->{$loop_var}) {
      $saved_loop_var_rename = delete $cur_renames->{$loop_var};
      $self->environment->state_var_renames({ %$cur_renames });
    }
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    });
    if (defined $saved_loop_var_rename) {
      $cur_renames = $self->environment->state_var_renames // {};
      $self->environment->state_var_renames({ %$cur_renames, $loop_var => $saved_loop_var_rename });
    }
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
  my $is_signature_syntax = 0;
  my $block;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word' && $child->content ne 'sub'
        && $child->content ne 'my' && $child->content ne 'our'
        && $child->content ne 'state') {
      # Concatenate: PPI may split "main::::foo" into "main::" + "::foo"
      $name .= $child->content unless $block;
    }
    elsif ($ref eq 'PPI::Token::Prototype') {
      $prototype = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::Signature') {
      # Perl 5.20+ signature (when 'use feature "signatures"' is used)
      $prototype = $child->content;
      $is_signature_syntax = 1;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      $block = $child;
    }
  }

  # At file scope, route named sub definitions to the declarations bucket.
  # declarations is assembled before definitions (BEGIN blocks, use/require),
  # which matches Perl: all named subs are compiled before any BEGIN runs,
  # so \&foo inside BEGIN can always find the sub already defined.
  # Inside subs (in_subroutine > 0), nested NAMED subs are hoisted to the
  # definitions bucket (at indent 0) so they are available before the outer
  # sub runs.  Their state variables use defvar (global special) instead of
  # let (lexical) so the inner sub can reference them from outside the let.
  #
  # Exception: when inside a let block (_let_bound_vars non-empty), the sub
  # must be emitted in-place so its lambda closes over the let-bound lexical
  # variables (e.g. $x__lex__N renamed for closure capture).  p-declare-sub
  # still goes to declarations for forward-reference support.
  my $is_nested_named = $name && $self->environment->in_subroutine > 0
                        && !%{$self->{_let_bound_vars} // {}};
  my $old_bucket = $self->_cur_bucket;
  my $old_indent = $self->indent_level;
  if ($self->environment->in_subroutine == 0 && !%{$self->{_let_bound_vars} // {}}) {
    $self->_cur_bucket('declarations');
  } elsif ($is_nested_named) {
    $self->_cur_bucket('definitions');
    $self->indent_level(0);
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
  if ($is_signature_syntax) {
    # Signature syntax (feature "signatures") is ALWAYS parsed as a signature,
    # never as an old-style prototype — even all-anonymous forms like ($) or
    # ($, $) that would otherwise look like a prototype.  Empty () => min 0.
    my $inner = $prototype;
    $inner =~ s/^\s*\(\s*//;
    $inner =~ s/\s*\)\s*$//;
    $sig_info = $self->_parse_signature($inner, $stmt);
  } elsif ($prototype) {
    $sig_info = $self->parse_prototype_or_signature($prototype, $stmt);
  }

  # A real Perl signature (feature "signatures"), not an old-style prototype.
  # When set, args are flattened into @_, arity is checked with Perl's exact
  # error message, and the named params are bound from @_ (see below).
  my $is_sig = $is_signature_syntax && !$sig_info->{is_proto};

  # Partition signature params into required / optional / slurpy and derive the
  # arity bounds.  Used both for the arity check and the @_-based binding.
  my (@sig_req, @sig_opt, $sig_slurpy);
  if ($is_sig) {
    for my $param (@{$sig_info->{params}}) {
      my $pname = $param->{name};
      if ($pname =~ /^[\@\%]/)          { $sig_slurpy = $pname; }
      elsif (defined $param->{default_cl}) { push @sig_opt, $param; }
      else                              { push @sig_req, $param; }
    }
  }
  my $sig_min  = scalar @sig_req;
  my $sig_max  = $sig_slurpy ? 'nil' : ($sig_min + scalar @sig_opt);
  my $sig_flex = (@sig_opt || $sig_slurpy) ? 't' : 'nil';

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

  # Real signatures: discard the CL-lambda param list built above and instead
  # capture every arg via &rest, then flatten + arity-check + bind from @_.
  # This makes foo(@arr) flatten correctly and gives Perl's exact arity error.
  if ($is_sig) {
    $params_cl = '&rest %_args';
    $needs_args_conversion = 0;   # we emit our own @_ binding below
  }

  # Perl package-qualified name for the arity error message ("main::foo").
  my $sig_qname;
  if ($is_sig) {
    my $pkg = $self->environment->current_package();
    my $bn  = $name ne '' ? $name : '__ANON__';
    $sig_qname = ($bn =~ /::/) ? $bn
               : ($pkg eq 'main' ? "main::$bn" : "$pkg\::$bn");
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
  #
  # Exception: when the block contains inner NAMED subs (which will be hoisted
  # to the definitions bucket), use defvar (global special) for state vars so
  # the hoisted inner subs can reference them without being inside the let scope.
  my %state_renames;
  my $use_defvar_state = 0;
  if (@state_vars) {
    my $sub_slug = $name ? $name : 'anon';
    $sub_slug =~ s/[^a-zA-Z0-9]/-/g;
    $use_defvar_state = $block && $self->_block_has_inner_named_subs($block);
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
      if ($use_defvar_state) {
        # Emit as global defvar so inner named subs can access outside the let.
        # Save/restore bucket+indent to emit at top level in declarations.
        my $saved_b = $self->_cur_bucket; my $saved_i = $self->indent_level;
        $self->_cur_bucket('declarations'); $self->indent_level(0);
        $self->_emit("(defvar $unique $init_val)");
        $self->_emit("(defvar ${unique}__init nil)");
        $self->_cur_bucket($saved_b); $self->indent_level($saved_i);
      } else {
        push @bindings, "($unique $init_val)";
        push @bindings, "(${unique}__init nil)";
      }
    }
    if (!$use_defvar_state) {
      $self->_emit("(let (" . join(" ", @bindings) . ")");
      $self->indent_level($self->indent_level + 1);
    }
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
    unshift @{$self->_sections->[$self->_cur_section]{declarations}},
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

  # Number of wrapper forms ((let ...)/(let* ...)) opened for a signature sub,
  # so the close section emits exactly that many ')'.
  my $sig_wrap_closes = 0;

  if ($is_sig) {
    # (let ((@_ (p-flatten-args %_args))) — flatten args (foo(@arr) spreads)
    $self->_emit("(let ((\@_ (p-flatten-args %_args)))");
    $self->indent_level($self->indent_level + 1);
    $sig_wrap_closes++;
    # Arity check BEFORE any binding (a too-few call must not index past @_).
    $self->_emit("(p-check-arity \"$sig_qname\" (length \@_) $sig_min $sig_max $sig_flex)");
    # Bind the named params positionally from @_ via a sequential let*
    # (so an optional default can reference an earlier param, e.g. $r = f($c)).
    #
    # Each scalar param is copied into a FRESH box via p-copy-scalar-arg: Perl
    # params are copies of @_ (`my ($x)=@_`), so the param must be its own
    # mutable box.  The param names are also registered in _let_bound_vars (see
    # below) so the body's `$x = ...` lowers to p-my-= (box-set) instead of
    # p-scalar-= — the latter's (proclaim special) would globalise the param and
    # make the write a silent no-op.  See docs/variable-declarations-spec.md §4.1.
    my @binds;
    my @sig_param_names;
    my @local_wraps;          # `local $G = …` defaults: localise $G for the body
    my $idx = 0;
    for my $p (@sig_req) {
      push @binds, "($p->{name} (p-copy-scalar-arg (aref \@_ $idx)))";
      push @sig_param_names, $p->{name};
      $idx++;
    }
    for my $p (@sig_opt) {
      push @binds,
        "($p->{name} (p-copy-scalar-arg (if (> (length \@_) $idx) (aref \@_ $idx) $p->{default_cl})))";
      push @sig_param_names, $p->{name};
      push @local_wraps, { var => $p->{local_var}, name => $p->{name}, idx => $idx }
        if $p->{local_var};
      $idx++;
    }
    if ($sig_slurpy) {
      my $fn = $sig_slurpy =~ /^\@/ ? 'p-sig-rest-array' : 'p-sig-rest-hash';
      push @binds, "($sig_slurpy ($fn \@_ $idx))";
    }
    if (@binds) {
      $self->_emit("(let* (" . join(' ', @binds) . ")");
      $self->indent_level($self->indent_level + 1);
      $sig_wrap_closes++;
    }
    # `local $G = RHS` default: localise $G to the param's value (= RHS) when the
    # default was taken, restored on sub exit via CL dynamic unwinding.  When an
    # arg was supplied the default did not run, so $G is rebound to itself (a
    # no-op rebinding that restores to the same box).  See spec §4.2.
    for my $lw (@local_wraps) {
      $self->_emit("(let (($lw->{var} (if (> (length \@_) $lw->{idx}) $lw->{var}"
                 . " (p-box-for-local (unbox $lw->{name})))))");
      $self->indent_level($self->indent_level + 1);
      $sig_wrap_closes++;
    }
    # Scalar params are lexical 'my'-style boxes: record them so _emit rewrites
    # their (p-scalar-= ...) to (p-my-= ...) for the duration of the body.
    $self->{_sig_param_names} = \@sig_param_names;
  }
  # If using %_args, convert to @_ vector
  elsif ($needs_args_conversion) {
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
    # Register scalar signature params so the body's `$param = ...` is rewritten
    # to p-my-= (box-set) by _emit, not p-scalar-=.  Kept in a SEPARATE set from
    # _let_bound_vars: the latter gates nested-named-sub hoisting (a sub inside a
    # `let` body stays inline to capture the lexicals), and params must NOT flip
    # that gate — an independently-called inner named sub must still hoist.
    local $self->{_sig_param_lexicals} = {
      %{$self->{_sig_param_lexicals} // {}},
      map { $_ => 1 } @{$self->{_sig_param_names} // []},
    };
    delete $self->{_sig_param_names};
    # Save package stack: inline 'package NAME;' inside a sub body must not leak
    my $saved_pkg_stack = [@{$self->environment->package_stack}];
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    }, 1);  # is_sub_body=1: enable two-phase scoped block
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

  if ($is_sig) {
    for (1 .. $sig_wrap_closes) {
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");  # close signature let / let*
    }
  }
  elsif ($needs_args_conversion) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close let
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close defun

  # Close state vars let (only when using lexical let, not defvar)
  if (@state_vars && !$use_defvar_state) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  $self->_emit("");

  # Restore previous bucket and indent (indent was saved for hoisted inner subs)
  $self->_cur_bucket($old_bucket);
  $self->indent_level($old_indent) if $is_nested_named;
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
    # Declare $a/$b as special in this package using fully-qualified names in the
    # top-level declarations bucket.  Using pkg::$a at top level (where the reader's
    # *package* is whatever the enclosing section uses) ensures SBCL sees these as
    # globally special before compiling any lambdas that reference them.
    # The inline `defvar $a` would declare MAIN::$A (wrong package), so we skip it.
    # Use same quoting as $cl_pkg: strip leading ':' to get the CL symbol prefix.
    # E.g. ':|Class|' → '|Class|::$a' so SBCL reads it as Class::$a not CLASS::$A.
    (my $cl_pkg_sym = $cl_pkg) =~ s/^://;
    my $pkg_a = $cl_pkg_sym . '::$a';
    my $pkg_b = $cl_pkg_sym . '::$b';
    $self->_with_bucket('declarations', sub {
      $self->_emit("(defvar $pkg_a (make-p-box nil))");
      $self->_emit("(defvar $pkg_b (make-p-box nil))");
      $self->_emit("");
    });
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
  # Declare $a/$b as special in this package so sort comparator lambdas
  # (lambda ($a $b) ...) create dynamic bindings visible to named comparator subs.
  $self->_with_bucket('declarations', sub {
    $self->_emit("(defvar \$a (make-p-box nil))");
    $self->_emit("(defvar \$b (make-p-box nil))");
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
  $perl_code =~ s/\n/ /g;   # Collapse newlines (multi-line use statements break CL ;; comments)

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
    # 'no strict' / 'no strict "subs"' - disable strict-subs mode
    if ($module eq 'strict') {
      my @args = map { $_->string } grep { $_->isa('PPI::Token::Quote') } $stmt->children;
      if (!@args || grep { /\bsubs\b/ } @args) {
        $self->environment->set_pragma('strict_subs', 0);
      }
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
        $self->_emit("(p-eval-always");
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

  # Handle 'use overload' - register operator overloading for the current package
  if ($module eq 'overload') {
    $self->_process_use_overload($stmt, $perl_code);
    return;
  }

  # Handle 'use base' / 'use parent' - set up @ISA inheritance
  if ($module eq 'base' || $module eq 'parent') {
    $self->_process_use_base($stmt, $perl_code, $module);
    return;
  }

  # Handle pragmas - emit as comment (no CL equivalent)
  if ($module =~ /^(strict|warnings|warnings::register|feature|utf8|open|Exporter|bytes|locale|integer|builtin|overloading|XSLoader|DynaLoader|Carp|re)$/) {
    # 'use integer' - enable integer pragma in current scope
    if ($module eq 'integer') {
      $self->environment->set_pragma('use_integer', 1);
    }
    # 'use strict' / 'use strict "subs"' - enable strict-subs mode for bareword disambiguation
    if ($module eq 'strict') {
      my @args = map { $_->string } grep { $_->isa('PPI::Token::Quote') } $stmt->children;
      if (!@args || grep { /\bsubs\b/ } @args) {
        $self->environment->set_pragma('strict_subs', 1);
      }
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

  # require inside a sub or block body must stay inline (not hoisted) so that:
  # 1. eval { require Foo } can catch load failures properly
  # 2. Perl semantics: require inside a block runs at runtime, not compile time
  # 3. require inside SKIP { } must not run when the block is skipped
  if ($type eq 'require' && ($self->environment->in_subroutine > 0 || $self->_block_depth > 0)) {
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
        $self->_emit("(p-eval-always");
        $self->_emit("  (p-use \"$module\" :imports '($list)))");
      } else {
        $self->_emit("(p-eval-always");
        $self->_emit("  (p-use \"$module\"))");
      }
    }
    elsif ($type eq 'require') {
      $self->_emit(";; $perl_code");
      $self->_emit("(p-eval-always");
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
      $self->_emit("(p-BEGIN");
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
      $self->_emit("(p-CHECK");
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


# Process 'use overload' — register operator overloading for the current package.
# Generates: (p-register-overloads (package-name *package*) PAIRS-VECTOR)
# where PAIRS-VECTOR is the transpiled form of the alternating op/handler list.
sub _process_use_overload {
  my ($self, $stmt, $perl_code) = @_;

  # Collect all tokens after the 'overload' keyword and before the semicolon.
  my @arg_tokens;
  my $past_module = 0;
  for my $child ($stmt->children) {
    if (!$past_module) {
      $past_module = 1
        if $child->isa('PPI::Token::Word') && $child->content eq 'overload';
      next;
    }
    next if $child->isa('PPI::Token::Structure');   # semicolon
    # Skip only leading whitespace (preserve whitespace within the list)
    next if !@arg_tokens && $child->isa('PPI::Token::Whitespace');
    push @arg_tokens, $child;
  }

  # Strip trailing whitespace
  pop @arg_tokens while @arg_tokens && $arg_tokens[-1]->isa('PPI::Token::Whitespace');

  if (!@arg_tokens) {
    $self->_emit(";; $perl_code (use overload - no handlers)");
    $self->_emit("");
    return;
  }

  # Parse the arg list in list context.
  # "+" => \&add, "0+" => \&numify, ...  →  (vector "+" #'pl-add "0+" #'pl-numify ...)
  my $args_cl = $self->_parse_expression(\@arg_tokens, $stmt, 1);  # 1 = LIST_CTX

  # Use the Perl package name as a literal string (not (package-name *package*))
  # because CL upcases package names ("MyStr" → "MYSTR") but p-bless stores the
  # original Perl class name as-is.  We need them to match at lookup time.
  my $pkg_name = $self->environment->current_package() // 'main';

  # Sanitize perl_code for comment — multi-line use overload would break CL
  (my $comment = $perl_code) =~ s/\n.*//s;  # keep only first line
  $self->_emit(";; $comment ...");
  $self->_emit("(p-register-overloads \"$pkg_name\" $args_cl)");
  $self->_emit("");
}


# Process 'use lib' statements
sub _process_use_lib {
  my ($self, $stmt, $perl_code) = @_;

  # use lib is compile-time @INC manipulation — route to definitions bucket
  # so it appears before any 'require' or 'use' in the same section
  $self->_with_bucket('definitions', sub {
    $self->_emit(";; $perl_code");
    $self->_emit("(p-eval-always");

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
      $self->_emit("(p-eval-always");
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
  my $self    = shift;
  my $parts   = shift;
  my $stmt    = shift;  # Original statement for full_PPI
  my $context = shift // 0;  # 0 = SCALAR_CTX (default), 1 = LIST_CTX

  # Call the internal version that returns declarations too
  my ($result, $decls) = $self->_parse_expression_internal($parts, $stmt, $context);

  # In scalar context, just return result (backwards compatible)
  return $result unless wantarray;

  # In list context, return result and declarations
  return ($result, $decls);
}

# Internal: parse expression and return both CL code and declarations
sub _parse_expression_internal {
  my $self    = shift;
  my $parts   = shift;
  my $stmt    = shift;
  my $context = shift // 0;  # 0 = SCALAR_CTX (default), 1 = LIST_CTX

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
    $expr_o->annotate_contexts($node_id, $context);

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
  if ($line && ($self->{_let_bound_vars} || $self->{_sig_param_lexicals})) {
    my %lex = (%{$self->{_let_bound_vars} // {}},
               %{$self->{_sig_param_lexicals} // {}});
    for my $var (keys %lex) {
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
  my $anon_counter = 0;

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
    elsif ($param_str =~ /^([\$\@\%])\s*=\s*(.*)$/) {
      # Anonymous placeholder with default: ($ = undef), ($ =)
      # Still counts toward arity; bound to a throwaway name.
      $name = $1 . '_sig_anon' . (++$anon_counter);
      my $rhs = $2;
      $rhs =~ s/\s+$//;
      $default_expr = ($rhs eq '') ? 'undef' : $rhs;
      $seen_optional = 1;
    }
    elsif ($param_str =~ /^([\$\@\%])$/) {
      # Anonymous mandatory placeholder: ($a, $) — the bare $ is a required slot.
      $name = $1 . '_sig_anon' . (++$anon_counter);
      $default_expr = undef;
    }
    else {
      # Unknown format, skip
      next;
    }

    # A `local $G = RHS` default localises $G for the sub's dynamic extent (and
    # the param's value is RHS).  PExpr would drop the `local` in expression
    # position (clobbering $G permanently), so peel it off here: compile only the
    # RHS as the default value and record the localised var for a body wrapper.
    # See docs/variable-declarations-spec.md §4.2.
    my $local_var;
    if (defined $default_expr
        && $default_expr =~ /^\s*\(?\s*local\s+(\$\w+)\s*=\s*(.+?)\s*\)?\s*$/s) {
      $local_var    = $1;
      $default_expr = $2;
    }

    # `our $VAR` inside a default declares a package global.  PExpr drops the
    # `our` keyword in expression position (so `(our $k)++` compiles to
    # `(p-post++ $k)`), but without an explicit declaration $VAR is never
    # defvar'd → unbound at runtime.  Register + emit the defvar here, mirroring
    # _process_our_declaration; the default expression keeps referencing $VAR.
    # See docs/variable-declarations-spec.md §4.3.
    if (defined $default_expr && $default_expr =~ /\bour\b/) {
      my $pkg = $self->environment->current_package;
      while ($default_expr =~ /\bour\s+([\$\@\%]\w+)/g) {
        my $ovar  = $1;
        my $sigil = substr($ovar, 0, 1);
        my $init  = $sigil eq '$' ? '(make-p-box nil)'
                  : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                  :                 '(make-hash-table :test #\'equal)';
        $self->environment->add_our_variable($pkg, $ovar);
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-always (defvar $ovar $init))");
        });
      }
    }

    my $default_cl = undef;
    if (defined $default_expr) {
      # Compile the default expression to CL
      $default_cl = $self->_compile_default_expr($default_expr, $context);
    }

    push @params, {
      name       => $name,
      default_cl => $default_cl,
      local_var  => $local_var,
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
