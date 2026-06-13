package Pl::ExprToCL;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.20;
use strict;
use warnings;

use Moo;

use Scalar::Util qw/looks_like_number/;
use Pl::PExpr qw(SCALAR_CTX LIST_CTX VOID_CTX INHERIT_CTX);

# Per-compilation flip-flop ID counter (increments across all ExprToCL instances)
my $g_flipflop_count = 0;
# Counter for unique gensyms in \(multi-term LIST) code generation
my $g_refgen_count = 0;

# Code generator that transforms Pl::PExpr AST into Common Lisp code.
# Follows conventions from CODEGEN_DESIGN.md:
# - Variables keep Perl sigils ($x, @arr, %hash)
# - All operators/functions use p- prefix
# - Pretty-printed output

has expr_o => (
  is       => 'ro',
  required => 1,
  doc      => 'Pl::PExpr object with parsed tree',
);

has environment => (
  is       => 'ro',
  doc      => 'Pl::Environment for package/class info',
);

has indent_level => (
  is       => 'rw',
  default  => 0,
);

has indent_str => (
  is       => 'ro',
  default  => '  ',
);

# L-value context tracking for array/hash element access
# When true, array/hash access should return boxes instead of values
has lvalue_context => (
  is       => 'rw',
  default  => 0,
);


has handlers => (
  is      => 'ro',
  lazy    => 1,
  builder => '_build_handlers',
);

sub _build_handlers {
  my $self = shift;
  return {
    'funcall'       => \&gen_funcall,
    'methodcall'    => \&gen_methodcall,
    'ref_funcall'   => \&gen_ref_funcall,
    'ternary'       => \&gen_ternary,
    'prefix_op'     => \&gen_prefix_op,
    'postfix_op'    => \&gen_postfix_op,
    'a_acc'         => \&gen_array_access,
    'h_acc'         => \&gen_hash_access,
    'a_ref_acc'     => \&gen_array_ref_access,
    'h_ref_acc'     => \&gen_hash_ref_access,
    'slice_a_acc'   => \&gen_array_slice,
    'slice_h_acc'   => \&gen_hash_slice,
    'kv_slice_h_acc' => \&gen_kv_hash_slice,
    'kv_slice_a_acc' => \&gen_kv_array_slice,
    'arr_init'      => \&gen_array_init,
    'hash_init'     => \&gen_hash_init,
    'progn'         => \&gen_progn,
    'tree_val'      => \&gen_tree_val,
    'filehandle'    => \&gen_filehandle,
    'readline'      => \&gen_readline,
    'glob'          => \&gen_glob,
    'backtick'      => \&gen_backtick,
    'anon_sub'      => \&gen_anon_sub,
    'func_ref'      => \&gen_func_ref,
    'inline_lambda' => \&gen_inline_lambda,
    'string_concat'    => \&gen_string_concat,
    'array_str_interp' => \&gen_array_str_interp,
    'glob_slot'        => \&gen_glob_slot,
  };
}

# Runtime built-in names (from pcl-runtime.lisp export list, p- prefix stripped).
# These get the p- prefix in generated code; user-defined subs get p-.
my %RUNTIME_NAMES = map { $_ => 1 } qw(
  ! != !~ $ % %= && * ** **= *= + - -d -e -f -r -s -w -x -z . .. ... .= / // //= /= < << <<=
  <= <=> == =~ > >= >> >>= abs and and-assign aref aref-box aref-deref array-= array-init
  array-last-index aslice atan2 backslash backtick binmode bit-and bit-and= bit-not bit-or
  bit-or= bit-xor bit-xor= bless box box-p box-value break caller can cast-$ cast-% cast-@
  chain-cmp chdir chmod chomp chop chr close closedir coderef-defined-p coderef-exists-p
  continue cos crypt cwd decf declare-sub defined defpackage delete delete-array delete-array-slice
  delete-hash-slice delete-kv-hash-slice die do each ensure-arrayref ensure-hashref env-get
  env-set eof eval eval-block eval-direct exception exception-object exists exists-array exit
  exp fc fileno flatten flatten-args for foreach funcall-ref get-class get-coderef getc getcwd
  getgrent getgrgid getgrnam endgrent setgrent
  gethash gethash-box gethash-deref glob glob-assign glob-copy glob-slot glob-undef-name gmtime
  grep hash hash-= hex hslice if incf index int isa join keys kv-aslice kv-hslice last last-dynamic lc
  lcfirst length let list-= list-x local-glob localtime log lstat make-typeglob map method-call
  alarm mkdir my my-= next not oct open opendir or or-assign ord our pack pipe pop pos post++ post-- pre++
  pre-- print printf prototype push quotemeta rand read readdir readline redo ref reftype regex
  rename require require-file reset resolve-invocant return reverse rewinddir rindex rmdir say
  evalbytes scalar scalar-= seek select set-array-length set_up_inc setf shift sin sleep sort splice split
  sprintf sqrt srand stat str-cmp str-eq str-ge str-gt str-le str-lt str-ne str-x str-x=
  string-concat study sub sub-defined sub-exists subst substr super-call sysread system syswrite
  tell tie tie-proxy tie-proxy-p tie-proxy-saved-value tie-proxy-tie-obj tied time times tr
  truncate typeglob typeglob-name typeglob-p typeglob-package uc ucfirst undef undef-sub unless
  unlink unpack unshift untie until use values vec version-string wantarray warn weaken isweak
  while xor ||
  overloaded overload-strval
);

# Only exceptions that need different CL names than p-<perl-op>
# If not listed here, the CL name is p-<perl-name> (runtime) or p-<perl-name> (user)
my %OP_EXCEPTIONS = (
  # Bitwise operators - avoid confusion with CL's & and |
  '&'   => 'p-bit-and',
  '|'   => 'p-bit-or',
  '^'   => 'p-bit-xor',
  '~'   => 'p-bit-not',
  # Dotted string bitwise operators (always string, never numeric)
  '&.'  => 'p-str-bit-and',
  '|.'  => 'p-str-bit-or',
  '^.'  => 'p-str-bit-xor',
  '~.'  => 'p-str-bit-not',

  # Assignment variants with clearer names
  '='   => 'p-setf',
  '+='  => 'p-incf',
  '-='  => 'p-decf',

  # Compound assignment - bitwise
  '&='  => 'p-bit-and=',
  '|='  => 'p-bit-or=',
  '^='  => 'p-bit-xor=',
  '&.=' => 'p-str-bit-and=',
  '|.=' => 'p-str-bit-or=',
  '^.=' => 'p-str-bit-xor=',

  # Compound assignment - logical
  '&&=' => 'p-and-assign',
  '||=' => 'p-or-assign',

  # Logical XOR (Perl 5.40+): same precedence as ||, high-prec version of 'xor'
  '^^'  => 'p-xor',

  # Reference operator
  '\\'  => 'p-backslash',

  # Note: Sigil cast operators (@, %, $) are handled in gen_prefix_op
  # They can't be in OP_EXCEPTIONS because % is also the modulo operator

  # Operators with names that could conflict with user subs
  # (these are valid Perl identifiers, so code can define sub x, sub eq, etc.)
  'x'   => 'p-str-x',
  'x='  => 'p-str-x=',
  'lt'  => 'p-str-lt',
  'gt'  => 'p-str-gt',
  'le'  => 'p-str-le',
  'ge'  => 'p-str-ge',
  'eq'  => 'p-str-eq',
  'ne'  => 'p-str-ne',
  'cmp' => 'p-str-cmp',
);

# Magic/special variables that need specific CL output
# Maps Perl variable name to its CL representation
my %SPECIAL_VARS = (
  '$!'  => '(p-errno-string)',
  '$?'  => '$?',
  '$.'  => '|$.|',
  '$0'  => '$0',
  '$@'  => '$@',
  '$^O' => '|$^O|',
  '$^V' => '|$^V|',
  '$^X' => '|$^X|',
  '$/'  => '|$/|',
  '$\\' => '|$\\\\|',   # $\ (ORS): backslash must be escaped INSIDE the |...| symbol;
                        # |$\| would escape the closing pipe -> unreadable form
  '$"'  => '|$"|',
  '$&'  => '|$&|',    # MATCH      - whole matched string
  '$`'  => '|$`|',    # PREMATCH   - text before the match
  q{$'} => q{|$'|},   # POSTMATCH  - text after the match
  # NB: do NOT map '$+' here — `$+{name}` is hash access on %+ (named captures);
  # a SPECIAL_VARS entry hijacks it.  Scalar `$+` (last-paren match) is rare; %+
  # is common.  The runtime |$+| var is still set by set-match-vars (harmless).
  '$|'  => '|$\||',
  '$;'  => '|$;|',
  '$,'  => '|$,|',
  '$]'  => '|$]|',
  # Format/write special variables (rarely used; declare to prevent CL read errors)
  '$~'  => '|$~|',    # FORMAT_NAME
  '$='  => '|$=|',    # FORMAT_LINES_PER_PAGE
  '$-'  => '|$-|',    # FORMAT_LINES_LEFT
  '$%'  => '|$%|',    # FORMAT_PAGE_NUMBER
  '$:'  => '|$:|',    # FORMAT_LINE_BREAK_CHARACTERS
  '$^L' => '|$^L|',   # FORMAT_FORMFEED
  '$^A' => '|$^A|',   # ACCUMULATOR (for formline/write)
  '$^'  => '|$^|',    # FORMAT_TOP_NAME
  # ${^...} caret variables — stub implementations (return undef)
  '${^WARNING_BITS}' => '(p-undef)',   # warning bits bitmask (Perl internal)
  '${^LAST_FH}'      => '(p-undef)',   # last filehandle used (Perl internal)
);

# Generate CL operator/function name from Perl name
# - Package-qualified names (Foo::bar) → |Foo|::p-func (user method)
# - Operator exceptions → from %OP_EXCEPTIONS (all runtime → p-)
# - Runtime built-in functions → p-<name>  (from %RUNTIME_NAMES)
# - User-defined functions → p-<name>
sub cl_name {
  my $self       = shift;
  my $perl_name  = shift;
  my $for_funcall = shift // 0;  # 1 = being used as a function call, not an operator

  # Guard against undefined input
  return 'p-UNDEFINED' unless defined $perl_name && length($perl_name);

  # Check for operator exceptions — but NOT when generating a function call name.
  # e.g. `x()` calls user sub x, not the string-repetition operator p-str-x.
  return $OP_EXCEPTIONS{$perl_name} if !$for_funcall && exists $OP_EXCEPTIONS{$perl_name};

  # Leading :: means main:: (e.g. ::is → main::is)
  $perl_name =~ s/^::/main::/;

  # Check for package-qualified name (Foo::bar or Foo::Bar::baz)
  if ($perl_name =~ /^(.+)::(.+)$/) {
    my ($pkg, $func) = ($1, $2);
    # CORE:: is Perl's built-in namespace — strip it and use the PCL built-in
    if ($pkg eq 'CORE') {
      return exists $RUNTIME_NAMES{$func} ? "p-$func" : "pl-$func";
    }
    # use overload introspection: overload::StrVal($obj), overload::Overloaded($obj)
    if ($pkg eq 'overload') {
      return 'p-overload-strval' if $func eq 'StrVal';
      return 'p-overloaded'     if $func eq 'Overloaded';
    }
    # Record package reference for pre-declaration
    $self->environment->add_referenced_package($pkg) if $self->environment;
    # Use pipe-quoting if package contains :: (e.g., |Foo::Bar|::pl-func)
    my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
    return "${cl_pkg}::pl-${func}";
  }

  # Runtime built-in → p-prefix; user-defined sub → pl-prefix
  if (exists $RUNTIME_NAMES{$perl_name}) {
    return "p-$perl_name";
  }
  # Inside a non-main package, qualify user-defined sub calls so SBCL's reader
  # resolves them in the right package (not MAIN, which is the load-time package).
  my $cur_pkg = ($self->environment && $self->environment->can('current_package'))
                  ? $self->environment->current_package()
                  : 'main';
  if ($cur_pkg && $cur_pkg ne 'main') {
    my $cl_pkg = $cur_pkg =~ /::/ ? "|$cur_pkg|" : $cur_pkg;
    return "${cl_pkg}::pl-${perl_name}";
  }
  return "pl-$perl_name";
}


# Helper: split a bare function name (from &funcname or &Pkg::funcname)
# into (package_string, plain_name_string).
# Used by exists/defined/undef &funcname codegen.
sub _split_func_sym {
  my ($self, $func_sym) = @_;
  my ($pkg, $name);
  if ($func_sym =~ /^(.+)::([^:]+)$/) {
    ($pkg, $name) = ($1, $2);
  } else {
    $pkg  = $self->environment ? ($self->environment->current_package // 'main') : 'main';
    $name = $func_sym;
  }
  return ($pkg, $name);
}


# Get context keyword for a node (:scalar or :list)
sub get_context_keyword {
  my $self    = shift;
  my $node_id = shift;

  my $ctx = $self->expr_o->get_node_context($node_id);
  # SCALAR_CTX = 0, LIST_CTX = 1
  return $ctx == 1 ? ':list' : ':scalar';
}


# Main entry point: generate CL code from AST
sub generate {
  my $self    = shift;
  my $node_id = shift // $self->expr_o->root;

  return ($self->indent_str x $self->indent_level) . $self->gen_node($node_id);
}


# Generate code for a single node
sub gen_node {
  my $self    = shift;
  my $node_id = shift;

  my $node    = $self->expr_o->get_a_node($node_id);
  my $kids    = $self->expr_o->get_node_children($node_id);

  # Internal node (PPIreference with type)
  if ($self->expr_o->is_internal_node_type($node)) {
    return $self->gen_internal_node($node, $node_id, $kids);
  }

  # Binary operator: PPI::Token::Operator with children
  # (Binary ops are stored as operator tokens, not PPIreference)
  if (ref($node) eq 'PPI::Token::Operator' && @$kids) {
    my $op = $node->content();
    return $self->gen_binary_op($op, $kids, $node_id);
  }

  # Word-form binary operators (e.g. 'isa') with children
  if (ref($node) eq 'PPI::Token::Word' && @$kids) {
    my $op = $node->content();
    return $self->gen_binary_op($op, $kids, $node_id);
  }

  # Leaf node (PPI token)
  return $self->gen_leaf($node);
}


# Generate code for internal nodes (operators, funcalls, etc.)
sub gen_internal_node {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $type    = $node->{type};

  # Dispatch based on node type
  my $handler = $self->handlers->{$type};

  if ($handler) {
    return $handler->($self, $node, $node_id, $kids);
  }

  # Assume it's a binary operator (operators stored with operator as type)
  return $self->gen_binary_op($type, $kids, $node_id);
}


# Generate code for leaf nodes (literals, variables)
# Parse a regex token content into (pattern_content, flags).
# Works for /pat/flags, m/pat/flags, m{pat}flags, qr/pat/flags, qr{pat}flags, etc.
# $is_qr: 1 if token starts with 'qr', 0 otherwise.
sub _parse_regex_content {
  my ($content, $is_qr) = @_;
  my $prefix_len = $is_qr ? 2 : 0;
  $prefix_len++ if !$is_qr && $content =~ /^m/;
  my $open_ch = substr($content, $prefix_len, 1);
  my %pairs = ('{' => '}', '(' => ')', '[' => ']', '<' => '>');
  my $close_ch = $pairs{$open_ch} // $open_ch;
  my $end_pos = rindex($content, $close_ch);
  my $pattern = substr($content, $prefix_len + 1, $end_pos - $prefix_len - 1);
  my $flags = substr($content, $end_pos + 1);
  return ($pattern, $flags);
}

# Check if a raw regex pattern content has unescaped $var or @var interpolation.
sub _has_regex_interpolation {
  my ($pattern) = @_;
  return $pattern =~ /(?<!\\)\$[a-zA-Z_{]|(?<!\\)\@[a-zA-Z_{]/;
}

# Build a CL expression that evaluates to the interpolated pattern string.
# Handles $scalar_var (simple scalar variables).
sub _gen_interp_regex_pattern {
  my ($pattern) = @_;
  my @parts;
  my $literal = '';
  my $i = 0;
  while ($i < length($pattern)) {
    my $c = substr($pattern, $i, 1);
    if ($c eq '\\') {
      $literal .= substr($pattern, $i, 2);
      $i += 2;
    } elsif ($c eq '$' && substr($pattern, $i) =~ /^\$\{([a-zA-Z_][a-zA-Z0-9_]*(?:::[a-zA-Z_][a-zA-Z0-9_]*)*)\}/) {
      my $varname = $1;
      if (length($literal)) {
        (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
        push @parts, qq{"$esc"};
        $literal = '';
      }
      push @parts, "\$$varname";
      $i += 3 + length($varname);
    } elsif ($c eq '$' && substr($pattern, $i) =~ /^\$([a-zA-Z_][a-zA-Z0-9_]*(?:::[a-zA-Z_][a-zA-Z0-9_]*)*)/) {
      my $varname = $1;
      if (length($literal)) {
        (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
        push @parts, qq{"$esc"};
        $literal = '';
      }
      my $cl_expr = "\$$varname";
      $i += 1 + length($varname);
      # Handle arrow dereferences: $var->[N] or $var->{key} (possibly chained)
      while ($i + 2 < length($pattern) && substr($pattern, $i, 2) eq '->') {
        my $bracket = substr($pattern, $i + 2, 1);
        if ($bracket eq '[') {
          my ($start, $depth, $j) = ($i + 3, 1, $i + 3);
          while ($j < length($pattern) && $depth > 0) {
            my $ch = substr($pattern, $j, 1);
            $depth++ if $ch eq '[';
            $depth-- if $ch eq ']';
            $j++;
          }
          last if $depth != 0;
          my $idx = substr($pattern, $start, $j - $start - 1);
          $cl_expr = "(p-aref $cl_expr $idx)";
          $i = $j;
        } elsif ($bracket eq '{') {
          my ($start, $depth, $j) = ($i + 3, 1, $i + 3);
          while ($j < length($pattern) && $depth > 0) {
            my $ch = substr($pattern, $j, 1);
            $depth++ if $ch eq '{';
            $depth-- if $ch eq '}';
            $j++;
          }
          last if $depth != 0;
          my $key = substr($pattern, $start, $j - $start - 1);
          $key =~ s/^["']//; $key =~ s/["']$//;
          $key =~ s/\\/\\\\/g; $key =~ s/"/\\"/g;
          $cl_expr = "(p-gethash $cl_expr \"$key\")";
          $i = $j;
        } else { last; }
      }
      push @parts, $cl_expr;
    } else {
      $literal .= $c;
      $i++;
    }
  }
  if (length($literal)) {
    (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
    push @parts, qq{"$esc"};
  }
  return @parts == 0 ? '""'
       : @parts == 1 ? $parts[0]
       : "(p-string-concat " . join(" ", @parts) . ")";
}

sub gen_leaf {
  my $self = shift;
  my $node = shift;

  my $ref  = ref($node);

  # Variable (like $x, @arr, %hash)
  if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
    my $content = $node->content() // '';
    # Normalize Perl 4 package separator: $pkg'var -> $pkg::var
    $content =~ s/^([\$\@\%\*&])([a-zA-Z_]\w*)'/$1$2::/;
    # Handle magic/special variables via dispatch table
    return $SPECIAL_VARS{$content} if exists $SPECIAL_VARS{$content};
    # Handle package-qualified variables: $Pkg::var -> Pkg::$var
    # Perl: $Config::debug  ->  CL: Config::$debug
    # Also: $::foo means $main::foo (empty package = main)
    # Note: Use [^:]+ at the end to avoid matching stash refs like $Pkg::Sub::
    if ($content =~ /^([\$\@\%])(.*)::([^:]+)$/) {
      my ($sigil, $pkg, $name) = ($1, $2, $3);
      # Empty package means main (e.g., $::foo = $main::foo)
      $pkg = 'main' if $pkg eq '';
      # Track referenced package
      $self->environment->add_referenced_package($pkg) if $self->environment;
      # Use pipe quoting for nested packages
      my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
      return "${cl_pkg}::${sigil}${name}";
    }
    # Handle package stash typeglob: *Pkg:: (no variable name) -> (p-stash "Pkg")
    # Perl: undef *Food:: or *Mover:: = *Mover2::
    # PCL: stash ops not fully supported but must be syntactically valid CL
    if ($content =~ /^\*(.*)::$/) {
      my $pkg = $1;
      $pkg = 'main' if $pkg eq '';
      $self->environment->add_referenced_package($pkg) if $self->environment;
      return "(p-stash \"$pkg\")";
    }
    # Handle package-qualified typeglobs: *Pkg::foo -> (p-make-typeglob "Pkg" "foo")
    # Also: *::foo means *main::foo (empty package = main)
    if ($content =~ /^\*(.*)::([^:]+)$/) {
      my ($pkg, $name) = ($1, $2);
      $pkg = 'main' if $pkg eq '';
      $self->environment->add_referenced_package($pkg) if $self->environment;
      return "(p-make-typeglob \"$pkg\" \"$name\")";
    }
    # Handle simple typeglob: *foo -> (p-make-typeglob "current-pkg" "foo")
    if ($content =~ /^\*(\w+)$/) {
      my $name = $1;
      my $pkg  = $self->environment ? $self->environment->current_package : 'main';
      $pkg //= 'main';
      return "(p-make-typeglob \"$pkg\" \"$name\")";
    }
    # Handle package stash access: $Pkg::Sub:: or %Pkg::Sub::
    # Perl: $YAML::Tiny:: or %YAML::Tiny:: -> CL: (p-stash "YAML::Tiny")
    # Also: $:: or %:: means main stash
    if ($content =~ /^([\$\%])(.*)::$/) {
      my ($sigil, $pkg) = ($1, $2);
      # Empty package means main (e.g., $:: = main stash)
      $pkg = 'main' if $pkg eq '';
      # Track referenced package
      $self->environment->add_referenced_package($pkg) if $self->environment;
      return "(p-stash \"$pkg\")";
    }
    # &foo (no parens) re-uses the CALLER'S @_ — unlike &foo() which passes an
    # empty list, or foo() which is a normal call.  At file top level @_ is the
    # global empty vector, so emitting @_ is always safe.
    # Note: &foo(@args) is handled as a funcall, not here; \&foo is a refgen.
    if ($content =~ /^&(.+)$/) {
      my $func_name = $1;
      my $cl_func = $self->cl_name($func_name, 1);
      return "($cl_func \@_)";
    }
    # Check if this var is a state variable that was renamed
    if ($self->environment) {
      my $renames = $self->environment->state_var_renames;
      return $renames->{$content} if $renames && exists $renames->{$content};
    }
    # Qualify `our` variables in non-main packages using the fully-qualified name.
    # When `our $var` is declared in `package Foo { }` the generated defvar uses
    # `Foo::$var`, but lambdas inside inline package blocks are read/compiled with
    # *package* = main (since only top-level in-package forms affect the reader).
    # Emitting `Foo::$var` makes the reference unambiguous regardless of context.
    if ($self->environment && $content =~ /^([\$\@\%])(\w+)$/) {
      my ($sigil, $name) = ($1, $2);
      my $pkg = $self->environment->current_package // 'main';
      if ($pkg ne 'main' && $self->environment->is_our_variable($pkg, $content)) {
        my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
        return "${cl_pkg}::${sigil}${name}";
      }
    }
    # Unknown ${^...} caret variables. Perl (perlvar: "alphanumeric strings
    # preceded by a caret") treats any ${^NAME} without assigned special meaning
    # as an ordinary, main-forced global scalar: undef until set, autovivifying
    # (e.g. `is ${^MPE}, undef` then `++${^MPE}` is 1). The reserved names we DO
    # model live in %SPECIAL_VARS above; everything else degrades to a normal
    # global here rather than aborting the whole transpile. We register the
    # symbol so _insert_variable_forward_declarations emits a file-level defvar.
    if ($content =~ /^\$\{\^/) {
      my $sym = "|$content|";
      $self->environment->add_caret_global($sym) if $self->environment;
      return $sym;
    }
    return $content;
  }

  # Array last index ($#arr)
  if ($ref eq 'PPI::Token::ArrayIndex') {
    my $content = $node->content();
    # $#arr     -> (p-array-last-index @arr)
    # $#Pkg::v  -> (p-array-last-index Pkg::@v)   — @ must go AFTER the pkg:: prefix
    $content =~ s/^\$#(.*)::(.+)$/$1\::\@$2/  # qualified: $#A::ISA → A::@ISA
        || $content =~ s/^\$#/\@/;            # simple: $#arr → @arr
    # Check state var rename (e.g., state @x → @state__sub__x__N)
    if ($self->environment) {
      my $renames = $self->environment->state_var_renames;
      $content = $renames->{$content} if $renames && exists $renames->{$content};
    }
    return "(p-array-last-index $content)";
  }

  # Number literal - convert Perl format to CL format
  # (includes subclasses: ::Hex, ::Binary, ::Octal, ::Float, ::Exp, ::Version)
  if ($ref =~ /^PPI::Token::Number/) {
    my $num = $node->content();

    # Version strings: v1.20.300 or 256.65.258 -> string of chr values
    if ($ref =~ /::Version$/ || $num =~ /^v(\d[\d.]*)$/) {
      my $vpart = $num;
      $vpart =~ s/^v//;   # strip leading 'v' if present
      my @parts = split /\./, $vpart;
      my $args = join(' ', @parts);
      return "(p-version-string $args)";
    }

    # Hex: 0x1234 or 0X1234 -> #x1234 (with optional leading -)
    if ($num =~ /^(-?)0[xX]([0-9a-fA-F_]+)$/) {
      my ($sign, $hex) = ($1, $2);
      $hex =~ s/_//g;  # Remove underscores
      return $sign ? "(- #x$hex)" : "#x$hex";
    }
    # Binary: 0b1010 or 0B1010 -> #b1010 (with optional leading -)
    if ($num =~ /^(-?)0[bB]([01_]+)$/) {
      my ($sign, $bin) = ($1, $2);
      $bin =~ s/_//g;
      return $sign ? "(- #b$bin)" : "#b$bin";
    }
    # Octal: 0o777 or 0O777 -> #o777 (Perl 5.34+ syntax, with optional leading -)
    if ($num =~ /^(-?)0[oO]([0-7_]+)$/) {
      my ($sign, $oct) = ($1, $2);
      $oct =~ s/_//g;
      return $sign ? "(- #o$oct)" : "#o$oct";
    }
    # Legacy octal: 0777 (but not 0 alone) -> #o777 (with optional leading -)
    if ($num =~ /^(-?)0([0-7_]+)$/ && $num ne '0') {
      my ($sign, $oct) = ($1, $2);
      $oct =~ s/_//g;
      return $sign ? "(- #o$oct)" : "#o$oct";
    }
    # Remove underscores from regular numbers (Perl allows 1_000_000)
    $num =~ s/_//g;
    # Check if float literal overflows double range (e.g. 1e9999 -> +Inf in Perl)
    if ($num =~ /[eE.]/) {
      my $val = eval($num);
      if (defined $val) {
        if ($val == 9**9**9)  { return '(p-double-inf)'; }
        if ($val == -(9**9**9)) { return '(p-double-inf t)'; }
      }
    }
    return $num;
  }

  # Compiled regex qr// (check before Quote to avoid catching QuoteLike::Regexp)
  if ($ref eq 'PPI::Token::QuoteLike::Regexp') {
    my $content = $node->content();
    my ($pattern, $flags) = _parse_regex_content($content, 1);
    if (_has_regex_interpolation($pattern)) {
      my $pat_expr = _gen_interp_regex_pattern($pattern);
      (my $esc_flags = $flags) =~ s/"/\\"/g;
      return qq{(pcl::p-regex-from-parts $pat_expr "$esc_flags")};
    }
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{(pcl::p-qr "$content")};
  }

  # Heredoc <<'EOF' or <<"EOF" or <<EOF
  if ($ref eq 'PPI::Token::HereDoc') {
    # PPI::Token::HereDoc has heredoc() method to get the content lines
    my @lines = $node->heredoc();
    my $content = join('', @lines);
    # Escape backslashes and double quotes for CL string literal
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }

  # String literals
  if ($ref =~ /^PPI::Token::Quote/) {
    my $content = $node->content();
    # Convert Perl escape sequences to actual characters for CL
    return $self->convert_perl_string($content);
  }

  # Bareword (function name, etc.)
  if ($ref eq 'PPI::Token::Word') {
    my $content = $node->can('content') ? ($node->content() // '') : '';
    # Handle __FILE__ and __LINE__ compile-time tokens
    if ($content eq '__FILE__') {
      my $source_file = $self->environment ? $self->environment->source_file : '-';
      $source_file //= '-';
      return qq{"$source_file"};
    }
    if ($content eq '__LINE__') {
      my $line = $node->line_number // 0;
      return $line;
    }
    # If marked as bareword string by handle_subcalls (unknown word used as a value,
    # e.g. !Bare where Bare is not a known function), emit as a string literal.
    if ($node->{_bareword_string}) {
      (my $escaped = $content) =~ s/"/\\"/g;
      return qq{"$escaped"};
    }
    return $content;
  }

  # Operator token (used as child of prefix_op, etc.)
  if ($ref eq 'PPI::Token::Operator') {
    return $node->content();
  }

  # Substitution s///
  if ($ref eq 'PPI::Token::Regexp::Substitute') {
    return $self->gen_substitution($node);
  }

  # Transliteration tr/// or y///
  if ($ref eq 'PPI::Token::Regexp::Transliterate') {
    return $self->gen_transliteration($node);
  }

  # Match regex m// or //
  if ($ref =~ /^PPI::Token::Regexp/) {
    my $content = $node->content();
    my ($pattern, $flags) = _parse_regex_content($content, 0);
    if (_has_regex_interpolation($pattern)) {
      my $pat_expr = _gen_interp_regex_pattern($pattern);
      (my $esc_flags = $flags) =~ s/"/\\"/g;
      return qq{(pcl::p-regex-from-parts $pat_expr "$esc_flags")};
    }
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{(p-regex "$content")};
  }

  # Cast (deref sigil)
  if ($ref eq 'PPI::Token::Cast') {
    return $node->content();
  }

  # Fallback
  if ($node->can('content')) {
    return $node->content();
  }

  return "(p-UNKNOWN-LEAF)";
}


# Return true if a PPI node is an integer literal (e.g. 3, 10, 0)
sub _is_integer_literal_node {
  my ($node) = @_;
  return 0 unless defined $node && ref($node) =~ /^PPI::Token::Number/;
  my $content = $node->can('content') ? ($node->content // '') : '';
  return $content =~ /^\d+$/;  # non-negative integer only (negatives are prefix-op)
}

sub _is_regex_match_node {
  # Returns true if the node is a regex-match expression (=~ or !~).
  # Such nodes are boolean-valued and not compared numerically with $. in flip-flops.
  my ($expr_o, $node) = @_;
  return 0 unless defined $node;
  return 0 unless $expr_o->is_internal_node_type($node);
  my $type = $node->{type} // '';
  return $type eq '=~' || $type eq '!~';
}

sub _is_string_literal_node {
  # Returns true for PPI string literal nodes (single/double/heredoc quotes etc.)
  # String literals in flip-flops use $.  comparison (may warn "isn't numeric").
  my ($node) = @_;
  return 0 unless defined $node;
  return ref($node) =~ /^PPI::Token::Quote/;
}

# Binary operator: (pcl:p-OP left right)
# Operators always use pcl: prefix to avoid conflicts with user-defined subs
sub gen_binary_op {
  my $self    = shift;
  my $op      = shift;
  my $kids    = shift;
  my $node_id = shift;  # Optional: for context-dependent operators like 'x'

  my $cl_op = $self->cl_op_name($op);

  # Special case: '..'/'..' — range in list context, flip-flop in scalar context
  if (($op eq '..' || $op eq '...') && defined $node_id) {
    my $ctx = $self->expr_o->get_node_context($node_id);
    my $left_node  = $self->expr_o->get_a_node($kids->[0]);
    my $right_node = $self->expr_o->get_a_node($kids->[1]);
    my $both_int  = _is_integer_literal_node($left_node) && _is_integer_literal_node($right_node);
    # Literal flip-flop: both operands are compile-time constants (ints or strings).
    # Perl compares them against $. (the line number), possibly warning "isn't numeric".
    # Variable/expression operands (including regex matches) use boolean evaluation.
    my $both_literal = ($both_int
                        || (_is_string_literal_node($left_node) && _is_string_literal_node($right_node)));
    # INHERIT_CTX with non-literal operands: range of booleans is degenerate; treat as scalar.
    # INHERIT_CTX with literals: emit a runtime wantarray check.
    my $effective_ctx = $ctx;
    if ($ctx == INHERIT_CTX && !$both_literal) {
      $effective_ctx = SCALAR_CTX;
    }
    if ($effective_ctx != LIST_CTX && $effective_ctx != INHERIT_CTX) {
      # Scalar (or demoted INHERIT) context: emit flip-flop
      my $ff_id = $g_flipflop_count++;
      my $left  = $self->gen_node($kids->[0]);
      my $right = $self->gen_node($kids->[1]);
      if ($both_int) {
        # Integer literals: clean $.  comparison, no warnings
        my $macro = ($op eq '...') ? 'p-flipflop-num-3' : 'p-flipflop-num';
        return "($macro $ff_id $left $right)";
      } elsif ($both_literal) {
        # String literals: $. comparison with numeric coercion, warns for non-numeric strings
        my $macro = ($op eq '...') ? 'p-flipflop-dyn-3' : 'p-flipflop-dyn';
        return "($macro $ff_id $left $right)";
      } else {
        # Variables, expressions, regex matches: boolean evaluation (no $. comparison)
        my $macro = ($op eq '...') ? 'p-flipflop-3' : 'p-flipflop';
        return "($macro $ff_id $left $right)";
      }
    }
    if ($effective_ctx == INHERIT_CTX) {
      # Literals in INHERIT_CTX: runtime wantarray check
      my $ff_id = $g_flipflop_count++;
      my $left  = $self->gen_node($kids->[0]);
      my $right = $self->gen_node($kids->[1]);
      my ($ff_macro, $range_fn);
      if ($both_int) {
        $ff_macro = ($op eq '...') ? 'p-flipflop-num-3' : 'p-flipflop-num';
      } else {
        $ff_macro = ($op eq '...') ? 'p-flipflop-dyn-3' : 'p-flipflop-dyn';
      }
      $range_fn  = ($op eq '...') ? 'p-...' : 'p-..';
      return "(if (eq *wantarray* t) ($range_fn $left $right) ($ff_macro $ff_id $left $right))";
    }
    # List context: range endpoints are always scalars, not lists
    $self->expr_o->set_node_context($kids->[0], SCALAR_CTX);
    $self->expr_o->set_node_context($kids->[1], SCALAR_CTX);
    # Fall through to normal range (p-.. / p-...)
  }

  # Special case: 'x' operator - use list repeat when LHS is parenthesized and in list context
  if ($op eq 'x' && defined $node_id) {
    my $lhs_node = $self->expr_o->get_a_node($kids->[0]);
    my $lhs_is_paren = $self->expr_o->is_internal_node_type($lhs_node) &&
                       ($lhs_node->{type} eq 'tree_val' || $lhs_node->{type} eq 'progn');
    my $ctx = $self->expr_o->get_node_context($node_id);
    if ($lhs_is_paren && $ctx == LIST_CTX) {
      # List repeat: (@x,1) x 4 — force LHS to list context so
      # gen_progn returns (vector ...) not (progn ...) / scalar last-val
      $self->expr_o->set_node_context($kids->[0], LIST_CTX);
      my $left  = $self->gen_node($kids->[0]);
      my $right = $self->gen_node($kids->[1]);
      return "(p-list-x $left $right)";
    }
    if ($lhs_is_paren && $ctx == INHERIT_CTX) {
      # Caller-context-dependent, e.g. `return (LIST) x $n`: list repeat in
      # list context, string repeat in scalar context.  Emit a runtime
      # *wantarray* check (mirrors the '..' INHERIT_CTX path above), generating
      # the parenthesized LHS in both list and scalar context.
      $self->expr_o->set_node_context($kids->[0], LIST_CTX);
      my $left_list   = $self->gen_node($kids->[0]);
      $self->expr_o->set_node_context($kids->[0], SCALAR_CTX);
      my $left_scalar = $self->gen_node($kids->[0]);
      my $right = $self->gen_node($kids->[1]);
      return "(if (eq *wantarray* t) (p-list-x $left_list $right) (p-str-x $left_scalar $right))";
    }
  }

  my $left  = $self->gen_node($kids->[0]);

  # Special case: isa operator - RHS bareword must be a string
  if ($op eq 'isa') {
    my $rhs_node = $self->expr_o->get_a_node($kids->[1]);
    my $right;
    if (ref($rhs_node) eq 'PPI::Token::Word' && !$self->expr_o->get_node_children($kids->[1])) {
      # Bareword class name → quoted string
      my $class_name = $rhs_node->content();
      $right = qq{"$class_name"};
    } else {
      $right = $self->gen_node($kids->[1]);
    }
    return "(p-isa $left $right)";
  }

  # Special case: hash assignment with list
  # %h = () or %h = (k=>v, ...) — pass flat vector so p-hash-= can count
  # input elements for scalar-context return (Perl: scalar(%h=(a,b,c,d)) = 4).
  if ($op eq '=' && $left =~ /^%/) {
    my $rhs_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($rhs_node)) {
      my $rhs_type = $rhs_node->{type};
      if ($rhs_type eq 'tree_val' || $rhs_type eq 'progn') {
        my $rhs_kids = $self->expr_o->get_node_children($kids->[1]);
        # p-hash-= reads *wantarray* at runtime to decide list vs scalar return.
        # Wrap it with the annotated context so runtime matches compile-time expectation.
        my $ctx = defined $node_id ? $self->expr_o->get_node_context($node_id) : 0;
        if (@$rhs_kids == 0) {
          my $result = "(p-hash-= $left (make-array 0 :adjustable t :fill-pointer 0))";
          return $ctx == LIST_CTX ? "(let ((*wantarray* t)) $result)"
               : $ctx == SCALAR_CTX ? "(let ((*wantarray* nil)) $result)"
               : $result;
        } else {
          # Flat vector — p-hash-= flattens, deduplicates, and counts internally
          my @parts = map { $self->gen_node($_) } @$rhs_kids;
          my $result = "(p-hash-= $left (vector " . join(" ", @parts) . "))";
          return $ctx == LIST_CTX ? "(let ((*wantarray* t)) $result)"
               : $ctx == SCALAR_CTX ? "(let ((*wantarray* nil)) $result)"
               : $result;
        }
      }
    }
  }

  my $right = $self->gen_node($kids->[1]);

  # Special case: keys(%h) = N is hash pre-sizing - no-op in CL
  # CL hash tables auto-resize, so just evaluate the RHS for side effects
  if ($op eq '=' && $left =~ /^\(p-keys /) {
    return "$right";
  }

  # Special case: $#arr = N  →  (p-set-array-length @arr N)
  if ($op eq '=' && $left =~ /^\(p-array-last-index (.+)\)$/) {
    my $arr = $1;
    return "(p-set-array-length $arr $right)";
  }

  # Typeglob assignment: *foo = RHS  →  (p-glob-assign "pkg" "name" rhs)
  # Runtime dispatch to the appropriate slot based on RHS type.
  if ($op eq '=' && $left =~ /^\(p-make-typeglob "([^"]+)" "([^"]+)"\)$/) {
    my ($pkg, $name) = ($1, $2);
    return "(p-glob-assign \"$pkg\" \"$name\" $right)";
  }

  # Dynamic typeglob assignment: *$var = RHS  →  (p-glob-assign-dynamic name-expr rhs)
  # e.g. *$::AUTOLOAD = sub { ... } assigns to the CODE slot of the glob named by $AUTOLOAD.
  if ($op eq '=' && $left =~ /^\(p-dynamic-typeglob (.+)\)$/) {
    my $name_expr = $1;
    return "(p-glob-assign-dynamic $name_expr $right)";
  }

  # For assignment, dispatch to type-specific forms based on LHS sigil.
  # Handles both local vars (@a, %h, $x) and qualified vars (Pkg::@a, Pkg::%h, Pkg::$x).
  if ($op eq '=') {
    if ($left =~ /^\(vector /) {
      my $ctx = defined $node_id ? $self->expr_o->get_node_context($node_id) : 0;
      my $result = "(p-list-= $left $right)";
      return $ctx == LIST_CTX ? "(let ((*wantarray* t)) $result)"
           : $ctx == SCALAR_CTX ? "(let ((*wantarray* nil)) $result)"
           : $result;
    } elsif ($left =~ /^\(p-cast-% /) {
      # %$ref = (list): assign to a dereferenced hash
      return "(p-hash-deref-= $left $right)";
    } elsif ($left =~ /^\(p-cast-@ /) {
      # @$ref = (list): assign to a dereferenced array
      return "(p-array-deref-= $left $right)";
    } elsif ($left =~ /^\(p-(?:gethash|aref) /) {
      # Single-element store: $h{k} = ... / $a[i] = ...  (via p-setf).  This
      # MUST precede the sigil regexes below: a package-qualified element form
      # like (p-gethash |Foo::Bar|::%H "x") contains "::%" (or "::@" for arrays)
      # which would otherwise be mis-detected as a whole %hash/@array LHS and
      # routed to p-hash-= / p-array-= (which expect a bare symbol place and
      # crash on the gethash/aref form).
      return "(p-setf $left $right)";
    } elsif ($left =~ /(?:^|::)@/) {
      return "(p-array-= $left $right)";
    } elsif ($left =~ /(?:^|::)%/) {
      return "(p-hash-= $left $right)";
    } elsif ($left =~ /(?:^|::)\$/) {
      return "(p-scalar-= $left $right)";
    }
    # Element access, slices, etc. - keep using p-setf
  }

  # 'use integer' pragma: truncate operands first, then operate.
  # Perl's 'use integer' truncates BOTH operands before the operation.
  # Uses p-int (exported, does truncate(to-number(val))) to stay in pcl namespace.
  if ($self->environment && $self->environment->has_pragma('use_integer')) {
    if ($op eq '/') {
      return "(truncate (p-int $left) (p-int $right))";
    } elsif ($op eq '%') {
      return "(rem (p-int $left) (p-int $right))";
    } elsif ($op eq '+') {
      return "(+ (p-int $left) (p-int $right))";
    } elsif ($op eq '-') {
      return "(- (p-int $left) (p-int $right))";
    } elsif ($op eq '*') {
      return "(* (p-int $left) (p-int $right))";
    } elsif ($op eq '&') {
      return "(p-to-s64 (logand (pcl::%pcl-to-integer (to-number $left)) (pcl::%pcl-to-integer (to-number $right))))";
    } elsif ($op eq '|') {
      return "(p-to-s64 (logior (pcl::%pcl-to-integer (to-number $left)) (pcl::%pcl-to-integer (to-number $right))))";
    } elsif ($op eq '^') {
      return "(p-to-s64 (logxor (pcl::%pcl-to-integer (to-number $left)) (pcl::%pcl-to-integer (to-number $right))))";
    } elsif ($op eq '<<') {
      return "(p-<<-int $left $right)";
    } elsif ($op eq '>>') {
      return "(p->>-int $left $right)";
    }
  }

  # Match operators read *wantarray* at runtime to choose between a boolean
  # (scalar) and a capture list (list).  When the surrounding expression pins
  # the match to a definite context, wrap it so the ambient *wantarray* from an
  # enclosing list construct does not leak in.  e.g. `join ':', split('a'=~/b/,…)`
  # — the match is split's scalar pattern arg, but join binds *wantarray* t.
  # Only a bare match is context-sensitive; s/// and tr/// return a scalar count,
  # so skip the wrapper for those (and keep their codegen string unchanged).
  if (($op eq '=~' || $op eq '!~') && $right !~ /^\(p-(?:subst|tr|translate)\b/) {
    my $ctx = defined $node_id ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;
    return "(let ((*wantarray* nil)) ($cl_op $left $right))" if $ctx == SCALAR_CTX;
    return "(let ((*wantarray* t)) ($cl_op $left $right))"   if $ctx == LIST_CTX;
  }

  return "($cl_op $left $right)";
}

# Generate CL name for an OPERATOR
# No prefix needed - pcl is in use list of all generated packages
sub cl_op_name {
  my $self = shift;
  my $op   = shift;

  # Check for operator exceptions first
  if (exists $OP_EXCEPTIONS{$op}) {
    return $OP_EXCEPTIONS{$op};
  }

  return "p-$op";
}


# String concatenation with multiple parts
# Handles array interpolation: "@arr" joins elements with $" (default: space)
sub gen_string_concat {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @parts;
  for my $kid_id (@$kids) {
    my $kid_node = $self->expr_o->get_a_node($kid_id);

    # Array/hash slices interpolate like arrays: their elements are joined with
    # $".  Force LIST context BEFORE generating — a slice inside a string used
    # in scalar context (my $s = "@a[1..2]") would otherwise inherit that scalar
    # context and reduce to its last element instead of joining all elements.
    my $is_slice = $self->expr_o->is_internal_node_type($kid_node)
                   && ($kid_node->{type} eq 'slice_a_acc'
                       || $kid_node->{type} eq 'slice_h_acc');
    $self->expr_o->set_node_context($kid_id, LIST_CTX) if $is_slice;
    my $generated = $self->gen_node($kid_id) // '';

    # Check if this is an array variable (@arr) - needs to be joined
    my $kid_content = (ref($kid_node) eq 'PPI::Token::Symbol' && $kid_node->can('content'))
                      ? ($kid_node->content() // '') : '';
    if ($kid_content =~ /^@/ || $is_slice) {
      # In Perl, "@arr" in string interpolation joins with $" (default space)
      # Use |$"| which is the CL variable for Perl's $" list separator
      push @parts, '(p-join |$"| ' . $generated . ')';
    } else {
      push @parts, $generated;
    }
  }
  return "(p-string-concat " . join(" ", @parts) . ")";
}


# Array interpolation in string: "@{[expr]}" or "@{$ref}" → (p-join |$"| (p-cast-@ EXPR))
sub gen_array_str_interp {
  my ($self, $node, $node_id, $kids) = @_;
  return '""' unless @$kids;
  my $expr = $self->gen_node($kids->[0]) // '""';
  return '(p-join |$"| (p-cast-@ ' . $expr . '))';
}


# Function call: (p-FUNC args...)
sub gen_funcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Check for __FILE__ and __LINE__ as zero-arg "functions"
  # (Parser wraps them in funcall when followed by operators)
  if (@$kids == 1) {
    my $func_node = $self->expr_o->get_a_node($kids->[0]);
    if (ref($func_node) eq 'PPI::Token::Word' && $func_node->can('content')) {
      my $content = $func_node->content() // '';
      if ($content eq '__FILE__') {
        my $source_file = $self->environment
	    ? $self->environment->source_file : '-';
        $source_file //= '-';
        return qq{"$source_file"};
      }
      if ($content eq '__LINE__') {
        my $line = $func_node->line_number // 0;
        return $line;
      }
      if ($content eq '__PACKAGE__') {
        my $pkg = $self->environment
	    ? $self->environment->current_package : 'main';
        $pkg //= 'main';
        return qq{"$pkg"};
      }
      # Perl: -bareword produces string "-bareword"
      # PPI tokenizes this as a single PPI::Token::Word
      if ($content =~ /^-[A-Za-z_]\w*$/) {
        return qq{"$content"};
      }
    }
  }

  # First child is function name
  my $func_name = $self->gen_node($kids->[0]);

  # PPI tokenizes "-funcname" as a single Word. When followed by arguments,
  # this means unary negation of the function call, not a call to "-funcname".
  # e.g. "-splice @a" → (p-- (p-splice @a))
  if ($func_name =~ /^-([A-Za-z_]\w*)$/) {
    my $real_func = $1;
    if (exists $RUNTIME_NAMES{$real_func}) {
      # Known built-in: generate unary minus of the call
      my $inner_cl = $self->cl_name($real_func, 1);
      my @arg_strs = map { $self->gen_node($_) } @{$kids}[1..$#$kids];
      my $args_str = @arg_strs ? ' ' . join(' ', @arg_strs) : '';
      return "(p-- ($inner_cl$args_str))";
    }
  }

  my $cl_func   = $self->cl_name($func_name, 1);

  # Special handling: SUPER::method(args) as indirect-object call
  # SUPER::m{@a} is indirect-object syntax: first arg is the invocant (from block)
  # Generate: (pcl::%pcl-super-indirect "m" "pkg" ARGS) where first arg is invocant
  if ($func_name =~ /^SUPER::(.+)$/) {
    my $method = $1;
    my $cur_pkg = ($self->environment && $self->environment->can('current_package'))
                    ? ($self->environment->current_package // 'main')
                    : 'main';
    if (@$kids >= 2) {
      my $arg_str = $self->gen_node($kids->[1]);
      return "(pcl::%pcl-super-indirect \"$method\" \"$cur_pkg\" $arg_str)";
    }
    # No args — call without invocant (will signal error at runtime)
    return "(pcl::%pcl-super-indirect \"$method\" \"$cur_pkg\" nil)";
  }

  # Special handling: require BAREWORD (module name) in expression context.
  # Statement-level `require Foo;` is handled in Parser.pm, but in expression
  # context (e.g. `$] >= 5.010 && require mro`) the bareword reaches here and
  # would otherwise be emitted as a function call (require (pl-mro)).  Detect a
  # single bareword/qualified module-name argument and load it by name.
  if ($func_name eq 'require' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    my $mod;
    if (ref($arg_node) eq 'PPI::Token::Word') {
      $mod = $arg_node->content;
    }
    # Bareword wrapped in a 0-arg funcall node (mro -> funcall(Word 'mro')).
    elsif ($self->expr_o->is_internal_node_type($arg_node)
           && $arg_node->{type} eq 'funcall') {
      my $ak = $self->expr_o->get_node_children($kids->[1]);
      if (@$ak == 1) {
        my $w = $self->expr_o->get_a_node($ak->[0]);
        $mod = $w->content if ref($w) eq 'PPI::Token::Word';
      }
    }
    if (defined $mod && $mod =~ /^\w+(?:::\w+)*$/) {
      return qq{(p-require "$mod")};
    }
  }

  # Special handling for next/last/redo/goto with label argument
  if (($func_name eq 'next' || $func_name eq 'last' || $func_name eq 'redo'
       || $func_name eq 'goto') && @$kids == 2) {
    # Check if the argument is a bareword label (funcall with single word child)
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);

    # goto &funcname — tail-call: call target with current @_ and return its result.
    # PPI tokenizes `goto &new1` as Word('goto') + Symbol('&new1').
    if ($func_name eq 'goto' &&
        ref($arg_node) eq 'PPI::Token::Symbol' &&
        $arg_node->content() =~ /^&(.+)$/) {
      my $target = $self->cl_name($1, 1);
      return "(p-goto-sub #'$target)";
    }

    # goto &$scalar — tail-call via dynamic coderef/name.
    # PPI tokenizes `goto &$cref` as Word('goto') + Cast('&') + Symbol('$cref').
    # PExpr processes the Cast as a prefix_op, generating (p-get-coderef $cref).
    if ($func_name eq 'goto' &&
        $self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'prefix_op') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 1) {
        my $op_node = $self->expr_o->get_a_node($arg_kids->[0]);
        if (ref($op_node) eq 'PPI::Token::Cast' && $op_node->content() eq '&') {
          my $fn_expr = $self->gen_node($kids->[1]);  # (p-get-coderef ...)
          return "(p-goto-sub $fn_expr)";
        }
      }
    }

    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids == 1) {
        my $label_node = $self->expr_o->get_a_node($arg_kids->[0]);
        if (ref($label_node) eq 'PPI::Token::Word') {
          my $label = $label_node->content();
          if ($func_name eq 'goto') {
            # goto LABEL → (go :label) within tagbody
            return "(go :$label)";
          }
          return "($cl_func $label)";
        }
      }
    }

    # goto EXPR (computed goto) — expression evaluates to a label name.
    # Not fully implementable in CL (go requires compile-time tags).
    # Generate a no-op rather than an undefined function call.
    if ($func_name eq 'goto') {
      my $arg_cl = $self->gen_node($kids->[1]);
      return "(p-goto-computed $arg_cl)";
    }
  }

  # Special handling for do { } blocks - evaluates block inline, returns last value
  if ($func_name eq 'do' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      if ($arg_node->{type} eq 'func_ref') {
        my $func_ref = $self->gen_node($kids->[1]);
        my $ctx = $self->expr_o->get_node_context($node_id);
        # INHERIT_CTX: don't override *wantarray*; p-return will restore it
        return "(funcall $func_ref)" if $ctx == INHERIT_CTX;
        my $wa  = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
        return "(let ((*wantarray* $wa)) (funcall $func_ref))";
      }
      elsif ($arg_node->{type} eq 'anon_sub') {
        my $block_kids = $self->expr_o->get_node_children($kids->[1]);
        my @body_parts;
        for my $kid_id (@$block_kids) {
          push @body_parts, $self->gen_node($kid_id);
        }
        return "(progn " . join(' ', @body_parts) . ")";
      }
      elsif ($arg_node->{type} eq 'inline_lambda') {
        # do { BLOCK } parsed as inline_lambda (avoids defun side-effect that
        # would corrupt a surrounding p-if when do{} sits in an elsif condition)
        my $body = $arg_node->{body_cl} // 'nil';
        my $ctx  = $self->expr_o->get_node_context($node_id);
        return "(progn $body)" if $ctx == INHERIT_CTX;
        my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
        return "(let ((*wantarray* $wa)) (progn $body))";
      }
    }
  }

  # Special handling for eval { } blocks
  # eval { block } catches exceptions and sets $@
  if ($func_name eq 'eval' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      my $ctx = $self->expr_o->get_node_context($node_id);
      my $wrap = sub {
        my ($inner) = @_;
        return $inner if $ctx == INHERIT_CTX;
        my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
        return "(let ((*wantarray* $wa)) $inner)";
      };
      if ($arg_node->{type} eq 'anon_sub') {
        # eval { block } with inline anon_sub - generate p-eval-block with body
        my $block_kids = $self->expr_o->get_node_children($kids->[1]);
        my @body_parts;
        for my $kid_id (@$block_kids) {
          push @body_parts, $self->gen_node($kid_id);
        }
        my $body = join(' ', @body_parts);
        return $wrap->("(p-eval-block $body)");
      }
      elsif ($arg_node->{type} eq 'inline_lambda') {
        # eval { block } parsed as inline_lambda (avoids defun side-effect)
        my $body = $arg_node->{body_cl} // 'nil';
        return $wrap->("(p-eval-block $body)");
      }
      elsif ($arg_node->{type} eq 'func_ref') {
        # eval { block } with named function (from Parser callback)
        my $func_ref = $self->gen_node($kids->[1]);
        return $wrap->("(p-eval-block (funcall $func_ref))");
      }
    }
    else {
      # eval STRING (string form): pass the caller's in-scope lexicals as an
      # alist so the eval body can capture them (the transpiler wraps the body
      # in a lambda whose params are those vars).  See docs/eval-lexical-capture.md.
      my $arg_cl = $self->gen_node($kids->[1]);
      my $alist  = $self->_eval_lexical_alist;
      return $alist ? "(p-eval $arg_cl $alist)" : "(p-eval $arg_cl)";
    }
  }

  # Special handling for grep/map expression form (without block)
  # grep EXPR, LIST  →  (p-grep (lambda ($_) EXPR) LIST)
  # The EXPR typically uses $_ which should be the lambda parameter
  if (($func_name eq 'grep' || $func_name eq 'map') && @$kids >= 2) {
    my $first_arg_node = $self->expr_o->get_a_node($kids->[1]);
    my $is_lambda_form = $self->expr_o->is_internal_node_type($first_arg_node) &&
                         ($first_arg_node->{type} eq 'inline_lambda' ||
                          $first_arg_node->{type} eq 'func_ref' ||
                          $first_arg_node->{type} eq 'anon_sub');

    if (!$is_lambda_form) {
      # Expression form: wrap first arg in lambda
      my $expr_cl = $self->gen_node($kids->[1]);
      my @rest_args;
      for my $i (2 .. $#$kids) {
        push @rest_args, $self->gen_node($kids->[$i]);
      }
      my $list_str = join(' ', @rest_args);
      return "($cl_func (lambda (\$_) $expr_cl) $list_str)";
    }
  }

  # Special handling for bless(REF, CLASSNAME)
  # The classname can be a bareword like MyClass or o:: which should be a string
  if ($func_name eq 'bless' && @$kids >= 2) {
    my $ref_arg = $self->gen_node($kids->[1]);
    my $cur_pkg = $self->environment ? $self->environment->current_package : 'main';
    my $class_arg = "\"$cur_pkg\"";  # Default class is the package at point of bless call

    if (@$kids >= 3) {
      my $class_node = $self->expr_o->get_a_node($kids->[2]);
      my $is_bareword = 0;

      # Check if it's a bareword (funcall with single word child that's just a Word)
      if ($self->expr_o->is_internal_node_type($class_node) &&
          $class_node->{type} eq 'funcall') {
        my $class_kids = $self->expr_o->get_node_children($kids->[2]);
        # Bareword funcalls have exactly 1 child (the word itself, no arguments)
        if (@$class_kids == 1) {
          my $word_node = $self->expr_o->get_a_node($class_kids->[0]);
          if (ref($word_node) eq 'PPI::Token::Word') {
            my $classname = $word_node->content();
            # Handle special tokens: __PACKAGE__, __FILE__, __LINE__, undef
            if ($classname eq '__PACKAGE__') {
              my $pkg = $self->environment
                  ? $self->environment->current_package : 'main';
              $pkg //= 'main';
              $class_arg = qq{"$pkg"};
              $is_bareword = 1;
            } elsif ($classname eq 'undef') {
              # undef keyword: not a bareword class name — fall through to gen_node,
              # which generates (p-undef); p-bless handles undef class at runtime
            } else {
              # Regular bareword class name - remove trailing :: if present (o:: -> o)
              $classname =~ s/::$//;
              $class_arg = qq{"$classname"};
              $is_bareword = 1;
            }
          }
        }
      }

      # Not a bareword - generate normally (could be string, shift, or other expression)
      if (!$is_bareword) {
        $class_arg = $self->gen_node($kids->[2]);
      }
    }
    return "(p-bless $ref_arg $class_arg)";
  }

  # Special handling for push/unshift: flatten @array arguments
  # In Perl, push(@x, @y) flattens @y, but push(@x, [1,2,3]) doesn't flatten the anon array
  # We detect @-sigiled expressions at code-gen time and wrap them with p-flatten
  if (($func_name eq 'push' || $func_name eq 'unshift') && @$kids >= 2) {
    my $target = $self->gen_node($kids->[1]);  # First arg is target array
    my @items;
    for my $i (2 .. $#$kids) {
      my $arg_node = $self->expr_o->get_a_node($kids->[$i]);
      my $arg = $self->gen_node($kids->[$i]);
      my $should_flatten = 0;

      # Check if this is an @-sigiled variable (e.g., @arr)
      if (ref($arg_node) eq 'PPI::Token::Symbol') {
        my $sigil = substr($arg_node->content(), 0, 1);
        $should_flatten = 1 if $sigil eq '@';
      }
      # Check if this is an array deref (e.g., @$ref, @{expr})
      # These are prefix_op nodes with @ Cast as first child
      elsif ($self->expr_o->is_internal_node_type($arg_node) &&
             $arg_node->{type} eq 'prefix_op') {
        my $arg_kids = $self->expr_o->get_node_children($kids->[$i]);
        if (@$arg_kids >= 1) {
          my $cast_node = $self->expr_o->get_a_node($arg_kids->[0]);
          if (ref($cast_node) eq 'PPI::Token::Cast' && $cast_node->content() eq '@') {
            $should_flatten = 1;
          }
        }
      }

      if ($should_flatten) {
        # Wrap with p-flatten to expand array elements
        $arg = "(p-flatten $arg)";
      }
      push @items, $arg;
    }
    my $items_str = @items ? ' ' . join(' ', @items) : '';
    return "($cl_func $target$items_str)";
  }

  # Special handling for readline(BAREWORD): treat arg as filehandle symbol, not function call
  if ($func_name eq 'readline' && @$kids == 2) {
    my $fh_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($fh_node) eq 'PPI::Token::Word' && $fh_node->can('content')) {
      my $fh_name = $fh_node->content() // '';
      return "(p-readline '$fh_name)";
    }
    # Funcall node with single word child (bareword wrapped in funcall)
    if ($self->expr_o->is_internal_node_type($fh_node) && $fh_node->{type} eq 'funcall') {
      my $fh_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$fh_kids == 1) {
        my $word_node = $self->expr_o->get_a_node($fh_kids->[0]);
        if (ref($word_node) eq 'PPI::Token::Word' && $word_node->can('content')) {
          my $fh_name = $word_node->content() // '';
          return "(p-readline '$fh_name)";
        }
      }
    }
  }

  # Special handling for tied(): needs the box, not the unboxed value.
  # p-aref normally unboxes (which would call FETCH on tied vars), so we use
  # p-aref-box to get the box at the array index without triggering FETCH.
  if ($func_name eq 'tied' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr = $self->gen_node($arg_kids->[0]);
        my $idx = $self->gen_node($arg_kids->[1]);
        $arr =~ s/(^|::)\$/${1}\@/;
        return "(p-tied (p-aref-box $arr $idx))";
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my $key  = $self->gen_node($arg_kids->[1]);
        $hash =~ s/(^|::)\$/${1}%/;
        return "(p-tied (p-gethash-box $hash $key))";
      }
    }
  }

  # Special handling for pos(): needs the box for identity tracking.
  # p-aref normally unboxes scalar elements, but pos() needs the box to track
  # the position in *p-match-pos*. Same pattern as tied().
  if ($func_name eq 'pos' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr = $self->gen_node($arg_kids->[0]);
        my $idx = $self->gen_node($arg_kids->[1]);
        $arr =~ s/(^|::)\$/${1}\@/;
        return "(p-pos (p-aref-box $arr $idx))";
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my $key  = $self->gen_node($arg_kids->[1]);
        $hash =~ s/(^|::)\$/${1}%/;
        return "(p-pos (p-gethash-box $hash $key))";
      }
    }
  }

  # Special handling for delete on arrays: delete $a[idx]
  # Need to pass array and index separately, not the dereferenced value
  if ($func_name eq 'delete' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'a_acc') {
      # Array access: delete $a[idx] -> (p-delete-array @arr idx)
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $arr = $self->gen_node($arg_kids->[0]);
        # Convert $a to @a for array (Symbol or Magic like $_)
        if ((ref($arr_node) eq 'PPI::Token::Symbol'
             || ref($arr_node) eq 'PPI::Token::Magic') && $arr =~ /(?:^|::)\$/) {
          $arr =~ s/(^|::)\$/${1}\@/;
        }
        my $idx = $self->gen_node($arg_kids->[1]);
        return "(p-delete-array $arr $idx)";
      }
    }
    # Hash access: delete $h{key} -> (p-delete %h key)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $hash = $self->gen_node($arg_kids->[0]);
        # Convert $h to %h for hash (Symbol or Magic like $_)
        if ((ref($hash_node) eq 'PPI::Token::Symbol'
             || ref($hash_node) eq 'PPI::Token::Magic') && $hash =~ /(?:^|::)\$/) {
          $hash =~ s/(^|::)\$/${1}%/;
        }
        my $key = $self->gen_node($arg_kids->[1]);
        return "(p-delete $hash $key)";
      }
    }
    # Hash slice: delete @foo{4,5} -> (p-delete-hash-slice %hash key1 key2 ...)
    # Also handles empty slice: delete @foo{()} -> (p-delete-hash-slice %hash)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'slice_h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 1) {
        my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $hash = $self->gen_node($arg_kids->[0]);
        # Convert @ to % for hash access (handle qualified names too).
        if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /(?:^|::)\@/) {
          $hash =~ s/(^|::)\@/${1}%/;
        }
        my @keys;
        for my $i (1 .. $#$arg_kids) {
          push @keys, $self->gen_node($arg_kids->[$i]);
        }
        my $keys_str = @keys ? ' ' . join(' ', @keys) : '';
        return "(p-delete-hash-slice $hash$keys_str)";
      }
    }
    # Array slice: delete @arr[1,2,3] -> (p-delete-array-slice @arr idx1 idx2 ...)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'slice_a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr = $self->gen_node($arg_kids->[0]);
        my @indices;
        for my $i (1 .. $#$arg_kids) {
          push @indices, $self->gen_node($arg_kids->[$i]);
        }
        my $idx_str = join(' ', @indices);
        return "(p-delete-array-slice $arr $idx_str)";
      }
    }
    # KV slice delete: delete %foo{6,7} -> (p-delete-kv-hash-slice %hash key1 key2 ...)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'kv_slice_h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my @keys;
        for my $i (1 .. $#$arg_kids) {
          push @keys, $self->gen_node($arg_kids->[$i]);
        }
        my $keys_str = join(' ', @keys);
        return "(p-delete-kv-hash-slice $hash $keys_str)";
      }
    }
    # KV array slice: delete %arr[6,7] -> (p-delete-kv-array-slice @arr idx1 idx2 ...)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'kv_slice_a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 1) {
        my $arr = $self->gen_node($arg_kids->[0]);
        $arr =~ s/(^|::)\%/${1}\@/;  # %arr -> @arr (KV sigil -> array container)
        my @indices;
        for my $i (1 .. $#$arg_kids) {
          push @indices, $self->gen_node($arg_kids->[$i]);
        }
        my $idx_str = @indices ? ' ' . join(' ', @indices) : '';
        return "(p-delete-kv-array-slice $arr$idx_str)";
      }
    }
    # Hash ref access: delete $ref->{key} -> (p-delete (unbox ref) key)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_ref_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $ref = $self->gen_node($arg_kids->[0]);
        my $key = $self->gen_node($arg_kids->[1]);
        return "(p-delete (unbox $ref) $key)";
      }
    }
  }

  # Special handling for exists on arrays and hashes
  # Need to pass container and key/index separately
  if ($func_name eq 'exists' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    # exists &funcname — subroutine existence check
    if (ref($arg_node) eq 'PPI::Token::Symbol') {
      my $sym = $arg_node->content();
      if ($sym =~ /^&(.+)$/) {
        my ($pkg, $name) = $self->_split_func_sym($1);
        return "(p-sub-exists \"$pkg\" \"$name\")";
      }
    }
    # exists &{$coderef} — coderef existence check (prefix_op with & cast)
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'prefix_op') {
      my $arg_kids2 = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids2 >= 2) {
        my $cast_node = $self->expr_o->get_a_node($arg_kids2->[0]);
        if (ref($cast_node) eq 'PPI::Token::Cast' && $cast_node->content() eq '&') {
          my $inner = $self->gen_node($arg_kids2->[1]);
          return "(p-coderef-exists-p $inner)";
        }
      }
    }
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        if ($arg_node->{type} eq 'a_acc') {
          # Array access: exists $a[idx] -> (p-exists-array @arr idx)
          my $arr_node = $self->expr_o->get_a_node($arg_kids->[0]);
          my $arr = $self->gen_node($arg_kids->[0]);
          # $a[i] -> @a : rewrite the scalar sigil to the array sigil, whether
          # bare ($a -> @a) or package-qualified (Foo::$a / |Foo::Bar|::$a ->
          # ...::@a). The `$` to rewrite is at the start or right after `::`.
          if ((ref($arr_node) eq 'PPI::Token::Symbol'
               || ref($arr_node) eq 'PPI::Token::Magic') && $arr =~ /(?:^|::)\$/) {
            $arr =~ s/(^|::)\$/${1}\@/;
          }
          my $idx = $self->gen_node($arg_kids->[1]);
          return "(p-exists-array $arr $idx)";
        }
        elsif ($arg_node->{type} eq 'h_acc') {
          # Hash access: exists $h{key} -> (p-exists %h key)
          my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
          my $hash = $self->gen_node($arg_kids->[0]);
          # $h{k} -> %h : rewrite the scalar sigil to the hash sigil, whether
          # bare ($h -> %h) or package-qualified (Foo::$h / |Foo::Bar|::$h ->
          # ...::%h). The `$` to rewrite is at the start or right after `::`.
          if ((ref($hash_node) eq 'PPI::Token::Symbol'
               || ref($hash_node) eq 'PPI::Token::Magic') && $hash =~ /(?:^|::)\$/) {
            $hash =~ s/(^|::)\$/${1}%/;
          }
          my $key = $self->gen_node($arg_kids->[1]);
          return "(p-exists $hash $key)";
        }
        elsif ($arg_node->{type} eq 'h_ref_acc') {
          # Hash-ref access: exists $r->{key} -> (p-exists (unbox REF) key)
          my $ref = $self->gen_node($arg_kids->[0]);
          my $key = $self->gen_node($arg_kids->[1]);
          return "(p-exists (unbox $ref) $key)";
        }
        elsif ($arg_node->{type} eq 'a_ref_acc') {
          # Array-ref access: exists $r->[idx] -> (p-exists-array (unbox REF) idx)
          my $ref = $self->gen_node($arg_kids->[0]);
          my $idx = $self->gen_node($arg_kids->[1]);
          return "(p-exists-array (unbox $ref) $idx)";
        }
      }
    }
  }

  # defined &funcname — subroutine defined check
  if ($func_name eq 'defined' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($arg_node) eq 'PPI::Token::Symbol') {
      my $sym = $arg_node->content();
      if ($sym =~ /^&(.+)$/) {
        my ($pkg, $name) = $self->_split_func_sym($1);
        return "(p-sub-defined \"$pkg\" \"$name\")";
      }
    }
    # defined &{$coderef} — coderef defined check (prefix_op with & cast)
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'prefix_op') {
      my $arg_kids2 = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids2 >= 2) {
        my $cast_node = $self->expr_o->get_a_node($arg_kids2->[0]);
        if (ref($cast_node) eq 'PPI::Token::Cast' && $cast_node->content() eq '&') {
          my $inner = $self->gen_node($arg_kids2->[1]);
          return "(p-coderef-defined-p $inner)";
        }
      }
    }
    # defined(FILEHANDLE) — bareword filehandle check (e.g. defined(FILE), defined(DIR))
    # Case 1: arg is a PPI::Token::Word leaf (all-caps bareword)
    if (ref($arg_node) eq 'PPI::Token::Word') {
      my $name = $arg_node->content();
      if ($name =~ /^[A-Z][A-Z0-9_]*$/) {
        return "(p-defined-fh '$name)";
      }
    }
    # Case 2: arg is an internal funcall node with a single uppercase-word child (no args)
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids2 = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids2 == 1) {
        my $fn_node = $self->expr_o->get_a_node($arg_kids2->[0]);
        if (ref($fn_node) eq 'PPI::Token::Word') {
          my $name = $fn_node->content();
          if ($name =~ /^[A-Z][A-Z0-9_]*$/) {
            return "(p-defined-fh '$name)";
          }
        }
      }
    }
  }

  # undef &funcname — undefine a sub (keeps it in exists table)
  if ($func_name eq 'undef' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($arg_node) eq 'PPI::Token::Symbol') {
      my $sym = $arg_node->content();
      if ($sym =~ /^&(.+)$/) {
        my ($pkg, $name) = $self->_split_func_sym($1);
        return "(p-undef-sub \"$pkg\" \"$name\")";
      }
    }
  }

  # Check for reference prototype that requires auto-boxing
  my $proto = $self->environment ? $self->environment->get_prototype($func_name) : undef;
  my @ref_params;
  if ($proto && $proto->{is_proto} && $proto->{params}) {
    # Use proto_type for auto-boxing checks (contains original like \@, \%, $, etc.)
    @ref_params = map { $_->{proto_type} // $_->{name} } @{$proto->{params}};
  }

  # Functions that modify their arguments (need l-value access to array/hash elements)
  # undef $hash{k} / undef $arr[i] must receive the box, not the unboxed value
  my %lvalue_funcs = map { $_ => 1 } qw(chop chomp undef);
  my $needs_lvalue = $lvalue_funcs{$func_name} // 0;

  # Rest are arguments
  # Temporarily clear tail_position while generating arguments: tail_position is
  # a flag that the current STATEMENT is the sub's last expression (so the outer
  # call can inherit caller context).  Arguments are NOT the tail call — they
  # need their own annotated context from child_context/annotate_contexts.
  my $saved_tail = $self->environment ? $self->environment->tail_position : 0;
  $self->environment->tail_position(0) if $self->environment && $saved_tail;

  my @args;
  for my $i (1 .. $#$kids) {
    # Set l-value context for functions that modify their arguments
    my $saved_lvalue = $self->lvalue_context;
    $self->lvalue_context(1) if $needs_lvalue;
    my $arg = $self->gen_node($kids->[$i]);
    $self->lvalue_context($saved_lvalue);

    # Check if this position has a reference prototype (\@, \%, \$)
    my $param_idx = $i - 1;  # 0-based index for params
    if ($param_idx < @ref_params) {
      my $param_type = $ref_params[$param_idx];
      if ($param_type =~ /^\\([@%\$])$/) {
        my $expected_sigil = $1;
        # Check if arg is an unref'd array/hash/scalar that needs wrapping
        my $arg_node = $self->expr_o->get_a_node($kids->[$i]);
        if (ref($arg_node) eq 'PPI::Token::Symbol') {
          my $arg_sigil = substr($arg_node->content(), 0, 1);
          # If arg sigil matches expected and it's not already a reference
          if ($arg_sigil eq $expected_sigil) {
            $arg = "(p-backslash $arg)";
          }
        }
      }
    }
    push @args, $arg;
  }

  # Restore tail_position before the tail-call context check below.
  $self->environment->tail_position($saved_tail) if $self->environment && $saved_tail;

  my $args_str = @args ? ' ' . join(' ', @args) : '';

  # die/warn: pass the source location so the runtime appends Perl's
  # " at FILE line N." suffix (when the message doesn't end in a newline).
  # The (:loc "...") marker is stripped by p-die/p-warn before concatenation.
  if ($cl_func eq 'p-die' || $cl_func eq 'p-warn') {
    my $word = $self->expr_o->get_a_node($kids->[0]);
    my $line = (ref($word) && $word->can('line_number')) ? ($word->line_number // 0) : 0;
    my $file = ($self->environment && $self->environment->source_file) || '-';
    $file =~ s/(["\\])/\\$1/g;
    $args_str = " :loc \"$file line $line\"$args_str";
  }

  my $call = "($cl_func$args_str)";

  # 'my'/'our' in expression context is an identity (returns the expression's value).
  # e.g. 'if (my $a = my $b = 3)' → (p-my-= $a (p-my-= $b 3)) with no wrapper needed.
  if (($func_name eq 'my' || $func_name eq 'our') && @args == 1) {
    return $args[0];
  }

  my $ctx = $self->expr_o->get_node_context($node_id);

  # split: p-split always returns a vector; no *wantarray* wrapper needed.
  # Arguments must NOT be evaluated in list context (e.g. =~ as pattern arg
  # would return captures vector instead of 1/0 if *wantarray* is t).
  if ($func_name eq 'split') {
    return $ctx == 0 ? "(length $call)" : $call;
  }

  # INHERIT_CTX or tail position: do not override *wantarray*; let the
  # caller's dynamic binding propagate through.  This must come BEFORE any
  # wantarray-sensitive built-in special cases (reverse/localtime/etc.) so that
  # when such a built-in IS the tail call of a sub, the caller's context flows
  # through rather than being frozen to the annotation-time context.
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;

  # reverse/localtime/gmtime/caller/unpack are wantarray-sensitive built-ins: they use
  # *wantarray* internally (or propagate it to do-file code).
  # Explicitly bind for all contexts so the outer dynamic scope can't leak into them.
  # unpack: scalar unpack() in list-context assignment (@a = scalar unpack()) must
  # force scalar context so p-unpack returns $result[0] not @result.
  if ($func_name =~ /^(reverse|localtime|gmtime|caller|unpack)$/) {
    return $ctx == LIST_CTX
        ? "(let ((*wantarray* t)) $call)"
        : "(let ((*wantarray* nil)) $call)";
  }

  # join always evaluates its list arguments in list context (args after sep),
  # regardless of the context in which join() itself is called.
  if ($func_name eq 'join') {
    return "(let ((*wantarray* t)) $call)";
  }
  if ($func_name eq 'do') {
    my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
    return "(let ((*wantarray* $wa)) $call)";
  }

  # User sub calls: always bind *wantarray* so the callee sees the correct
  # context regardless of what the surrounding scope has set.
  # Built-ins (in %RUNTIME_NAMES) don't call p-wantarray; only wrap them for
  # list context (to avoid disturbing wantarray-sensitive built-ins called
  # inside a scalar-context scope).
  if (!exists $RUNTIME_NAMES{$func_name}) {
    my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
    return "(let ((*wantarray* $wa)) $call)";
  }

  # Built-in in list context: still wrap so it gets list-context signal
  # (e.g. a wantarray-sensitive built-in called as the RHS of @arr = builtin())
  return $ctx == LIST_CTX ? "(let ((*wantarray* t)) $call)" : $call;
}


# Build the lexical-capture alist passed as the 2nd arg to (p-eval STRING ...).
# Each in-scope lexical becomes (cons "$name" $name), mapping its Perl name to
# its live CL container (box/array/hash).  The in-scope lexicals are the
# parser's _let_bound_vars (the rolling set of `my`/let-bound names, saved and
# restored around every closure).  Returns '' when there are none (top-level
# eval), so codegen emits a plain (p-eval STRING).
sub _eval_lexical_alist {
  my $self = shift;
  my $parser = ($self->expr_o && $self->expr_o->can('has_parser')
                && $self->expr_o->has_parser) ? $self->expr_o->parser : undef;
  return '' unless $parser;
  my $lb = $parser->{_let_bound_vars} // {};
  my @vars = sort keys %$lb;
  return '' unless @vars;
  # The alist KEY is the original Perl name; the VALUE is the live CL symbol.
  # Closure-captured lexicals are renamed to $name__lex__N (so per-call let
  # bindings stay lexical); strip that suffix so the key matches the bare name
  # the eval body uses (the eval string never sees the rename).
  my @pairs;
  for my $v (@vars) {
    (my $key = $v) =~ s/__lex__\d+$//;
    push @pairs, "(cons \"$key\" $v)";
  }
  return '(list ' . join(' ', @pairs) . ')';
}


# Method call: (p-method-call obj 'method args...)
sub gen_methodcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is object, second is method name
  my $obj_node = $self->expr_o->get_a_node($kids->[0]);
  my $obj;

  # Check if object is a class name (funcall with no args = bare word like Counter)
  # Disambiguation at compile time when possible, otherwise defer to runtime:
  #   - Known package (e.g., Counter->new()) → string "Counter"
  #   - Known function (e.g., foo->method() where foo is a sub) → function call
  #   - Unknown bareword → use p-resolve-invocant for runtime dispatch
  #     (In Perl, Foo->bar() checks if sub Foo exists before treating as class)
  if ($self->expr_o->is_internal_node_type($obj_node) &&
      $obj_node->{type} eq 'funcall') {
    my $obj_kids = $self->expr_o->get_node_children($kids->[0]);
    if (@$obj_kids == 1) {
      # Single child = potential bare class name or function call
      my $class_node = $self->expr_o->get_a_node($obj_kids->[0]);
      if (ref($class_node) eq 'PPI::Token::Word') {
        my $name = $class_node->content();
        if ($self->environment && $self->environment->is_package($name)) {
          # Known package → class name string
          $obj = '"' . $name . '"';
        } elsif ($self->environment && $self->environment->has_prototype($name)) {
          # Known function → function call
          $obj = $self->gen_node($kids->[0]);
        } else {
          # Unknown bareword → runtime dispatch (checks for sub first, then class)
          $obj = '(p-resolve-invocant "' . $name . '")';
        }
      } else {
        $obj = $self->gen_node($kids->[0]);
      }
    } else {
      $obj = $self->gen_node($kids->[0]);
    }
  } else {
    $obj = $self->gen_node($kids->[0]);
  }

  my $method_node = $self->expr_o->get_a_node($kids->[1]);
  my $method  = $self->gen_node($kids->[1]);

  # Check if method name is a variable (dynamic method call)
  my $is_dynamic_method = 0;
  if (ref($method_node) eq 'PPI::Token::Symbol' && $method_node->content() =~ /^\$/) {
    $is_dynamic_method = 1;
  } elsif ($self->expr_o->is_internal_node_type($method_node)) {
    # Computed method name, e.g. $obj->${ EXPR }(...) — the method is the
    # runtime value of an expression (a name string or a coderef).
    $is_dynamic_method = 1;
  }

  # Rest are arguments
  my @args;
  for my $i (2 .. $#$kids) {
    push @args, $self->gen_node($kids->[$i]);
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';

  # Check for SUPER:: method call (also handles old Perl 4 SUPER'method syntax)
  my $call;
  if ($method =~ /^SUPER(?:::|')(.+)$/) {
    my $real_method = $1;
    # Need current package for SUPER:: lookup
    my $current_pkg = $self->environment ? $self->environment->current_package : 'main';
    (my $rm_str = $real_method) =~ s/"/\\"/g;
    $call = "(p-super-call $obj \"$rm_str\" \"$current_pkg\"$args_str)";
  } elsif ($is_dynamic_method) {
    # Dynamic method call: $obj->$method_var
    # Method name is in a variable, pass the variable value
    $call = "(p-method-call $obj $method$args_str)";
  } else {
    # Use string literal to preserve case (CL symbols are upcased, breaking AUTOLOAD)
    (my $method_str = $method) =~ s/"/\\"/g;
    $call = "(p-method-call $obj \"$method_str\"$args_str)";
  }

  # Bind *wantarray* so the method body sees the correct call context.
  my $ctx = $self->expr_o->get_node_context($node_id);
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
  return "(let ((*wantarray* $wa)) $call)";
}


# Code ref call: (p-funcall-ref ref args...)
sub gen_ref_funcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is the code reference
  my $ref = $self->gen_node($kids->[0]);

  # Rest are arguments
  my @args;
  for my $i (1 .. $#$kids) {
    push @args, $self->gen_node($kids->[$i]);
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';
  my $call = "(p-funcall-ref $ref$args_str)";

  # Bind *wantarray* so the code-ref body sees the correct call context.
  my $ctx = $self->expr_o->get_node_context($node_id);
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
  return "(let ((*wantarray* $wa)) $call)";
}


# Ternary: (p-if cond then else)
sub gen_ternary {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $cond = $self->gen_node($kids->[0]);
  my $then  = $self->gen_node($kids->[1]);
  my $else  = $self->gen_node($kids->[2]);

  return "(p-if $cond $then $else)";
}


# Prefix operator: (p-OP operand) or (p-OP-pre operand)
sub gen_prefix_op {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is the operator, second is the operand
  my $op_node = $self->expr_o->get_a_node($kids->[0]);
  my $op      = $op_node->content();

  # Special case: \&func (reference to function)
  # Use p-backslash-sub to safely handle undefined functions (AUTOLOAD dispatch).
  if ($op eq '\\') {
    my $operand_id   = $kids->[1];
    my $operand_node = $self->expr_o->get_a_node($operand_id);
    if (ref($operand_node) eq 'PPI::Token::Symbol' &&
        $operand_node->content() =~ /^&(.+)$/) {
      my $func_name = $1;
      my $cl_func = $self->cl_name($func_name, 1);
      return "(p-backslash-sub '$cl_func)";
    }
    # \(LIST) — distribute \\ over each element. PExpr marks the operand node
    # with 'backslash_paren_list' when the source had explicit parens.
    if ($self->expr_o->node_tree->get_metadata($operand_id, 'backslash_paren_list')) {
      # For single-child tree_val with a scalar expression (not an array var,
      # range, or list-function), use p-backslash directly.  This handles
      # \(my $v = expr) correctly — without this check it generates a vector
      # of one ref instead of a plain scalar ref, breaking bless.
      my $inner_node = $self->expr_o->get_a_node($operand_id);
      if ($self->expr_o->is_internal_node_type($inner_node)
          && ($inner_node->{type} // '') eq 'tree_val') {
        my $tv_kids = $self->expr_o->get_node_children($operand_id);
        if (@$tv_kids == 1 && !$self->_is_list_node_for_refgen($tv_kids->[0])) {
          # Single scalar child: \(scalar_expr) == \scalar_expr
          my $saved_ctx = $self->expr_o->get_node_context($operand_id);
          $self->expr_o->set_node_context($operand_id, 0);
          my $scalar_expr = $self->gen_node($operand_id);
          $self->expr_o->set_node_context($operand_id, $saved_ctx);
          return "(p-backslash $scalar_expr)";
        }
        # Multi-term comma list: \(T1, T2, ...) — each @/% var gets one ref,
        # ranges spread into N scalar refs, scalars get one scalar ref.
        if (@$tv_kids > 1) {
          return $self->_gen_backslash_multi_term($tv_kids);
        }
      }
      my $raw_ctx   = $self->expr_o->get_node_context_raw($node_id);
      my $saved_ctx = $self->expr_o->get_node_context($node_id);
      $self->expr_o->set_node_context($operand_id, LIST_CTX);
      my $list_expr = $self->gen_node($operand_id);
      $self->expr_o->set_node_context($operand_id, $saved_ctx);
      # \(LIST) is a list operator: in explicit scalar/void context it yields a
      # ref to the LAST element (comma-operator semantics), e.g.
      # `bless \(map "$_", "x"), "C"` is a SCALAR ref, not an ARRAY ref.  When the
      # context is list or unannotated (list-natural), keep the full vector.
      if (defined $raw_ctx && ($raw_ctx == SCALAR_CTX || $raw_ctx == VOID_CTX)) {
        return "(p-list-scalar (p-refgen-list $list_expr))";
      }
      return "(p-refgen-list $list_expr)";
    }
  }

  # $#{ array } — last index of array (braced form of $#array)
  # PPI tokenises $#{@a} as Cast[$#] + Block{@a}; handle before cl_name()
  if ($op eq '$#') {
    my $arr_expr = $self->gen_node($kids->[1]);
    return "(p-array-last-index $arr_expr)";
  }

  # ${expr}++ / $$var++ / ${expr}-- / $$var-- (and @/% variants):
  # The shunting-yard parser incorrectly produces prefix_op($, postfix_op(X, ++))
  # because ++ has higher precedence (92) than Cast $ (90).
  # The correct semantics is postfix_op(prefix_op($, X), ++):
  #   (p-post++ (p-cast-$ X))  not  (p-cast-$ (p-post++ X))
  if ($op eq '$' || $op eq '@' || $op eq '%') {
    my $inner_id   = $kids->[1];
    my $inner_node = $self->expr_o->get_a_node($inner_id);
    if ($self->expr_o->is_internal_node_type($inner_node)
        && $inner_node->{type} eq 'postfix_op') {
      my $po_kids    = $self->expr_o->get_node_children($inner_id);
      my $po_op_node = $self->expr_o->get_a_node($po_kids->[1]);
      my $po_op      = $po_op_node->content();
      if ($po_op eq '++' || $po_op eq '--') {
        # Inner expression should NOT be in lvalue context: we want the VALUE
        # (the reference) to pass to p-cast-$, not the box.
        my $saved = $self->lvalue_context;
        $self->lvalue_context(0);
        my $inner_expr = $self->gen_node($po_kids->[0]);
        $self->lvalue_context($saved);
        return "(p-post$po_op (p-cast-$op $inner_expr))";
      }
    }
  }

  # Unary + is a pure no-op disambiguator in Perl (`map +(LIST), ...`,
  # `func +(...)`, `print +(...)`). It must NOT numify or collapse a list to a
  # scalar — it passes its operand through unchanged, inheriting the surrounding
  # context. Propagate our node's context to the operand so a parenthesised list
  # stays a list (fixes `map +($_, $h{$_}), LIST`). See docs/sweep-bug-catalog.md.
  if ($op eq '+') {
    my $operand_id = $kids->[1];
    my $my_ctx     = defined $node_id
                     ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;
    # A parenthesised SINGLE expression `+(EXPR)` is just EXPR — unwrap the
    # tree_val so it does not become a 1-element vector in list context (e.g.
    # `print +(2+3)`). A multi-term `+(A, B)` keeps the list (becomes a vector).
    my $on = $self->expr_o->get_a_node($operand_id);
    if ($self->expr_o->is_internal_node_type($on)
        && ($on->{type} // '') eq 'tree_val') {
      my $tv_kids = $self->expr_o->get_node_children($operand_id);
      $operand_id = $tv_kids->[0] if @$tv_kids == 1;
    }
    my $saved = $self->expr_o->get_node_context($operand_id);
    $self->expr_o->set_node_context($operand_id, $my_ctx);
    my $inner = $self->gen_node($operand_id);
    $self->expr_o->set_node_context($operand_id, $saved);
    return $inner;
  }

  # ++, --, \ and @ need l-value context for array/hash elements.
  # @ needs lvalue so subscripts return boxes → p-cast-@ can auto-vivify.
  # \ needs l-value to get a reference to the box, not a copy of the value.
  my $needs_lvalue = ($op eq '++' || $op eq '--' || $op eq '\\' || $op eq '@');
  my $saved_lvalue = $self->lvalue_context;
  $self->lvalue_context(1) if $needs_lvalue;
  my $operand = $self->gen_node($kids->[1]);
  $self->lvalue_context($saved_lvalue);

  # \$#array — reference to the arylen ($#array) magic. A plain
  # (p-backslash (p-array-last-index X)) backslashes a COPY of the integer, so
  # $$ref = N would not resize X. Emit a live magic-cell ref instead (getter =
  # p-array-last-index, setter = p-set-array-length). See docs/sweep-bug-catalog.md.
  if ($op eq '\\' && $operand =~ /^\(p-array-last-index (.+)\)$/) {
    return "(p-arylen-ref $1)";
  }

  # \substr / \pos / \vec — references to scalar magic lvalues. Like \$#array,
  # a plain (p-backslash (p-substr ...)) backslashes a COPY of the extracted
  # value, so $$ref = X would not write back. Emit live magic-cell refs instead
  # (getter reads, setter writes through). See docs/sweep-bug-catalog.md.
  if ($op eq '\\') {
    if ($operand =~ /^\(p-substr (.+)\)$/) { return "(p-substr-ref $1)"; }
    if ($operand =~ /^\(p-pos (.+)\)$/)    { return "(p-pos-ref $1)"; }
    if ($operand =~ /^\(p-vec (.+)\)$/)    { return "(p-vec-ref $1)"; }
  }

  # Get CL name for the operator
  my $cl_op = $self->cl_name($op);

  # Under 'use integer', ~ returns signed 64-bit complement
  if ($op eq '~' && $self->environment && $self->environment->has_pragma('use_integer')) {
    return "(p-to-s64 (lognot (pcl::%pcl-to-integer (to-number $operand))))";
  }

  # For ++ and --, distinguish prefix from postfix
  if ($op eq '++' || $op eq '--') {
    # Special case: $#array lvalue - emit setter form
    if ($operand =~ /^\(p-array-last-index (.+)\)$/) {
      my $arr = $1;
      my $delta_op = ($op eq '++') ? '1+' : '1-';
      return "(p-set-array-length $arr ($delta_op (p-array-last-index $arr)))";
    }
    $cl_op = "p-pre" . $op;
  }
  # Sigil cast operators (dereference) - use p-cast-X
  # (@{EXPR}[slice] / @{EXPR}{slice} never reach here: PExpr builds a
  # slice_a_acc/slice_h_acc node when a Cast+Block is followed by a trailing
  # subscript — see docs/symbolic-ref-slice-parse-fix.md.)
  elsif ($op eq '@' || $op eq '%' || $op eq '$') {
    $cl_op = "p-cast-$op";
  }
  # & Cast: &{expr} or &$var - dynamic coderef by name
  # \&{expr} becomes (p-backslash (p-get-coderef expr)), which returns the
  # function directly (p-backslash passes through non-box values).
  elsif ($op eq '&') {
    return "(p-get-coderef $operand)";
  }
  # * Cast: *$var (typeglob ref) — use distinct marker so assignment can detect it.
  # When on LHS of =, becomes (p-glob-assign-dynamic ...).
  # As rvalue, returns the typeglob object.
  elsif ($op eq '*') {
    return "(p-dynamic-typeglob $operand)";
  }

  return "($cl_op $operand)";
}


# Postfix operator: (p-OP-post operand)
sub gen_postfix_op {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Check if this is a chained comparison (odd number of kids >= 5:
  # term op term op term ...)
  if (scalar(@$kids) >= 5 && scalar(@$kids) % 2 == 1) {
    return $self->gen_chained_comparison($kids);
  }

  # Get operator first to check if we need l-value context
  my $op_node = $self->expr_o->get_a_node($kids->[1]);
  my $op      = $op_node->content();

  # ++ and -- modify their operand, need l-value context for array/hash elements
  my $needs_lvalue = ($op eq '++' || $op eq '--');
  my $saved_lvalue = $self->lvalue_context;
  $self->lvalue_context(1) if $needs_lvalue;
  my $operand = $self->gen_node($kids->[0]);
  $self->lvalue_context($saved_lvalue);

  # For ++ and --, use p-post++ / p-post-- naming
  my $cl_op;
  if ($op eq '++' || $op eq '--') {
    # Special case: $#array lvalue - emit setter form (return old value)
    if ($operand =~ /^\(p-array-last-index (.+)\)$/) {
      my $arr = $1;
      my $delta_op = ($op eq '++') ? '1+' : '1-';
      return "(let ((_prev (p-array-last-index $arr))) (p-set-array-length $arr ($delta_op _prev)) _prev)";
    }
    $cl_op = "p-post" . $op;
  } else {
    $cl_op = $self->cl_name($op) . '-post';
  }

  return "($cl_op $operand)";
}


# Chained comparison: $x < $y < $z      -> (p-chain-cmp $x '< $y '< $z)
#                     a == b != c == d   -> (p-chain-cmp a '== b '!= c '== d)
# Kids alternate: term, op, term, op, ..., term  (always odd count >= 5)
sub gen_chained_comparison {
  my $self = shift;
  my $kids = shift;

  my @parts;
  for my $i (0 .. $#$kids) {
    if ($i % 2 == 0) {
      push @parts, $self->gen_node($kids->[$i]);                              # term
    } else {
      push @parts, "'" . $self->expr_o->get_a_node($kids->[$i])->content();  # 'op
    }
  }
  return "(p-chain-cmp " . join(" ", @parts) . ")";
}


# Array access: (p-aref arr idx) or (p-aref-box arr idx) in l-value context
# In Perl, $arr[0] accesses @arr, so we convert $sigil to @sigil
sub gen_array_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $arr_node = $self->expr_o->get_a_node($kids->[0]);
  my $arr = $self->gen_node($kids->[0]);
  my $idx = $self->gen_node($kids->[1]);

  # Convert $varname to @varname (Perl $arr[i] accesses @arr)
  # Handle both plain $arr and package-qualified Pkg::$arr.
  # Only rewrite when the container is a bare variable (Symbol/Magic): for a
  # nested access like $a[$i]{...} the container is already a full (p-aref ...)
  # form whose inner package-qualified index (Pkg::$i) would be wrongly hit by
  # the `::$` alternative.
  if (ref($arr_node) eq 'PPI::Token::Symbol'
      || ref($arr_node) eq 'PPI::Token::Magic') {
    $arr =~ s/(^|::)\$/$1@/;
  }

  # Numeric-named arrays like @0, @1 are not valid Perl identifiers.
  # $0[n] parses as @0[n] but @0 is never a real variable; return undef.
  return '(p-undef)' if $arr =~ /^@\d+$/;

  # Apply rename map for @varname (closure/state variable captures)
  if ($self->environment) {
    my $renames = $self->environment->state_var_renames;
    $arr = $renames->{$arr} if $renames && exists $renames->{$arr};
  }

  # Use p-aref-box in l-value context for modifying operations
  my $func = $self->lvalue_context ? 'p-aref-box' : 'p-aref';
  return "($func $arr $idx)";
}


# Hash access: (p-gethash hash key) or (p-gethash-box hash key) in l-value context
# In Perl, $hash{key} accesses %hash, so we convert $sigil to %sigil
sub gen_hash_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $hash = $self->gen_node($kids->[0]);

  # $h{a, b, c} → key is join($;, a, b, c) (SUBSEP multi-key)
  my $key_node = $self->expr_o->get_a_node($kids->[1]);
  my $key;
  if ($self->expr_o->is_internal_node_type($key_node)
      && $key_node->{type} eq 'progn') {
    my $key_kids = $self->expr_o->get_node_children($kids->[1]);
    if (@$key_kids > 1) {
      my @parts = map { $self->gen_node($_) } @$key_kids;
      $key = "(p-join |\$;| (vector " . join(' ', @parts) . "))";
    } else {
      $key = $self->gen_node($kids->[1]);
    }
  } else {
    $key = $self->gen_node($kids->[1]);
  }

  # Convert $varname to %varname (Perl $hash{k} accesses %hash)
  # Handle both plain $hash and package-qualified Pkg::$hash.
  # Only rewrite when the container is a bare variable (Symbol/Magic): for a
  # nested access like $h{$k}[...] the container is already a full (p-gethash
  # ...) form whose inner package-qualified key (Pkg::$k) would be wrongly hit
  # by the `::$` alternative.
  my $hash_node = $self->expr_o->get_a_node($kids->[0]);
  if (ref($hash_node) eq 'PPI::Token::Symbol'
      || ref($hash_node) eq 'PPI::Token::Magic') {
    $hash =~ s/(^|::)\$/$1%/;
  }

  # Apply rename map for %varname (closure/state variable captures)
  if ($self->environment) {
    my $renames = $self->environment->state_var_renames;
    $hash = $renames->{$hash} if $renames && exists $renames->{$hash};
  }

  # Use p-gethash-box in l-value context for modifying operations
  my $func = $self->lvalue_context ? 'p-gethash-box' : 'p-gethash';
  return "($func $hash $key)";
}


# Array ref access: (p-aref-deref ref idx)
sub gen_array_ref_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # (LIST)[idx] or method()[idx]: force LIST_CTX on child 0 so the expression
  # is evaluated in list context — Perl always does this for X[N] subscripts.
  # 'list_ctx_subscript' is set by the Constructor path in PExpr.pm (covers
  # paren-list and method-call subscripts). Arrow-deref $arr->[N] does NOT
  # set this flag, so those keep their outer context (usually scalar).
  # Also handle qw[...][idx]: child 0 is a 'progn' (qw words), always LIST_CTX.
  my $is_list_subscript = $self->expr_o->node_tree->get_metadata($node_id, 'list_ctx_subscript');
  my $child0_node = $self->expr_o->get_a_node($kids->[0]);
  if ($is_list_subscript
      || ($self->expr_o->is_internal_node_type($child0_node)
          && $child0_node->{type} eq 'progn')) {
    $self->expr_o->set_node_context($kids->[0], 1);  # LIST_CTX = 1
  }

  my $ref = $self->gen_node($kids->[0]);
  my $idx = $self->gen_node($kids->[1]);

  return "(p-aref-deref $ref $idx)";
}


# Hash ref access: (p-gethash-deref ref key)
sub gen_hash_ref_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $ref = $self->gen_node($kids->[0]);

  # $href->{a, b} → key is join($;, a, b) (SUBSEP multi-key)
  my $key_node = $self->expr_o->get_a_node($kids->[1]);
  my $key;
  if ($self->expr_o->is_internal_node_type($key_node)
      && $key_node->{type} eq 'progn') {
    my $key_kids = $self->expr_o->get_node_children($kids->[1]);
    if (@$key_kids > 1) {
      my @parts = map { $self->gen_node($_) } @$key_kids;
      $key = "(p-join |\$;| (vector " . join(' ', @parts) . "))";
    } else {
      $key = $self->gen_node($kids->[1]);
    }
  } else {
    $key = $self->gen_node($kids->[1]);
  }

  return "(p-gethash-deref $ref $key)";
}


# Array slice: (p-aslice arr idx1 idx2 ...)
sub gen_array_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $arr = $self->gen_node($kids->[0]);
  my @indices;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @indices, $self->gen_node($kids->[$i]);
  }

  my $idx_str = join(' ', @indices);
  return $self->_slice_in_context("(p-aslice $arr $idx_str)", $node_id);
}

# An array/hash slice in scalar context yields its LAST element (list semantics),
# not the element count.  Wrap the slice vector accordingly: scalar → last elem,
# list/void → the vector itself, inherited (e.g. `return @a[...]`) → runtime check.
sub _slice_in_context {
  my ($self, $slice_cl, $node_id) = @_;
  # Use the RAW context: an unannotated slice is list-natural (e.g. inside string
  # interpolation or a freshly-parsed unit-test expression) and must keep its full
  # vector — defaulting it to scalar would wrongly reduce it to the last element.
  my $ctx = defined $node_id ? $self->expr_o->get_node_context_raw($node_id) : undef;
  return $slice_cl unless defined $ctx;            # unannotated → full slice vector
  return "(p-list-scalar $slice_cl)"  if $ctx == SCALAR_CTX;
  return "(p-slice-result $slice_cl)" if $ctx == INHERIT_CTX;
  return $slice_cl;  # LIST_CTX / VOID_CTX: keep the full slice vector
}


# Hash slice: (p-hslice hash key1 key2 ...)
# Note: @foo{keys} accesses %foo, so we convert @ sigil to %
sub gen_hash_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $hash_node = $self->expr_o->get_a_node($kids->[0]);
  my $hash = $self->gen_node($kids->[0]);
  # Convert @ to % for hash access (@ is context sigil, % is container sigil),
  # handling package-qualified names (Foo::@h / |Foo::Bar|::@h) too.
  if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /(?:^|::)\@/) {
    $hash =~ s/(^|::)\@/${1}%/;
  }
  my @keys;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @keys, $self->gen_node($kids->[$i]);
  }

  my $key_str = join(' ', @keys);
  return $self->_slice_in_context("(p-hslice $hash $key_str)", $node_id);
}

# KV hash slice: %hash{keys} - returns key-value pairs
sub gen_kv_hash_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $hash = $self->gen_node($kids->[0]);
  my @keys;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @keys, $self->gen_node($kids->[$i]);
  }

  my $key_str = join(' ', @keys);
  return "(p-kv-hslice $hash $key_str)";
}


# Typeglob slot access: *name{SLOT} -> (p-glob-slot <glob> "SLOT")
sub gen_glob_slot {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $glob_cl   = $self->gen_node($kids->[0]);
  # Computed slot {$type} / {EXPR}: the slot name is produced at runtime; the
  # slot expression is child 1, and p-glob-slot stringifies + upcases the result.
  if ($node->{slot_is_expr}) {
    my $slot_cl = $self->gen_node($kids->[1]);
    return "(p-glob-slot $glob_cl $slot_cl)";
  }
  my $slot_name = uc($node->{slot_name} // 'SCALAR');
  return "(p-glob-slot $glob_cl \"$slot_name\")";
}


# KV array slice: %arr[indices] - returns key-value pairs
sub gen_kv_array_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $arr = $self->gen_node($kids->[0]);
  # %arr[...] uses @ sigil for the array variable in CL (handle qualified names).
  $arr =~ s/(^|::)\%/${1}\@/;
  # %$ref[...] — $ref is a scalar holding an array ref; unbox to get the vector
  $arr = "(unbox $arr)" if $arr =~ /^\$/;
  my @indices;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @indices, $self->gen_node($kids->[$i]);
  }

  my $idx_str = join(' ', @indices);
  return "(p-kv-aslice $arr $idx_str)";
}

# Array initializer: (p-array-init ...)
# Uses p-array-init to flatten nested arrays (handles [(@x) x 2] etc.)
sub gen_array_init {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # The contents of an anonymous array constructor [ ... ] are ALWAYS evaluated
  # in list context, regardless of the context the [...] expression itself sits
  # in.  Two things leak otherwise: (1) the annotated context, and (2) the
  # tail_position flag — a wantarray-sensitive builtin (reverse/unpack/…) skips
  # its (let ((*wantarray* t)) …) wrapper in tail position, so the enclosing
  # scalar context leaks in.  E.g. `my $s = do { ...; "@{[reverse @a]}" }` ran
  # reverse in scalar context (reversing the joined string "123" -> "321")
  # instead of the list (3,2,1).  The bracket contents are never the tail call.
  my $saved_tail = $self->environment ? $self->environment->tail_position : 0;
  $self->environment->tail_position(0) if $self->environment && $saved_tail;
  my @elements;
  for my $kid_id (@$kids) {
    my $saved_ctx = $self->expr_o->get_node_context($kid_id);
    $self->expr_o->set_node_context($kid_id, LIST_CTX);
    push @elements, $self->gen_node($kid_id);
    $self->expr_o->set_node_context($kid_id, $saved_ctx);
  }
  $self->environment->tail_position($saved_tail) if $self->environment && $saved_tail;

  # Use p-array-init which flattens nested arrays
  # Wrap in make-p-box because [...] creates a REFERENCE to an anonymous array
  # (a scalar value), not the array itself. Without boxing, p-setf @arr
  # would flatten the inner array instead of storing it as a reference.
  if (@elements) {
    my $elem_str = join(' ', @elements);
    return "(make-p-box (p-array-init $elem_str))";
  } else {
    return "(make-p-box (make-array 0 :adjustable t :fill-pointer 0))";
  }
}


# Hash initializer: (p-hash ...)
sub gen_hash_init {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @pairs;
  for my $kid_id (@$kids) {
    push @pairs, $self->gen_node($kid_id);
  }

  my $pairs_str = join(' ', @pairs);
  # Wrap in make-p-box because {...} creates a REFERENCE to an anonymous hash
  return "(make-p-box (p-hash $pairs_str))";
}


# Progn (comma-separated list): (progn ...)
sub gen_progn {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $ctx = $self->expr_o->get_node_context($node_id);

  # In list context, each child element also contributes in list context
  # so that split() returns a list (not count), @arr expands, etc.
  if ($ctx == 1) {  # LIST_CTX = 1
    for my $kid_id (@$kids) {
      $self->expr_o->set_node_context($kid_id, 1);
    }
  }

  my @forms;
  for my $kid_id (@$kids) {
    push @forms, $self->gen_node($kid_id);
  }

  my $forms_str = join(' ', @forms);

  # In list context, generate a vector instead of progn
  # This handles: @a = (1,2,3), etc.
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(vector $forms_str)";
  }

  # In non-list context with multiple forms, check wantarray at runtime.
  # Covers SCALAR_CTX, VOID_CTX, and INHERIT_CTX — all cases where the caller
  # hasn't explicitly requested a list.  The runtime check handles map blocks
  # (whose body is compiled in VOID_CTX but whose lambda runs with *wantarray* t).
  # Wrap @array items with (p-flatten ...) so %p-collect-list in
  # %p-flatten-for-list can spread @arrays while keeping arrayrefs as scalars.
  if (@forms > 1 && $ctx != LIST_CTX) {
    my @flat_forms;
    for my $i (0 .. $#$kids) {
      my $form = $forms[$i];
      my $kid_node = $self->expr_o->get_a_node($kids->[$i]);
      if ($self->_is_array_expr_node($kid_node, $kids->[$i])) {
        $form = "(p-flatten $form)";
      }
      push @flat_forms, $form;
    }
    my $flat_str = join(' ', @flat_forms);
    return "(if *wantarray* (vector $flat_str) (progn $forms_str))";
  }

  return "(progn $forms_str)";
}

# Helper: true if a node represents an @array (should flatten in list context).
sub _is_array_expr_node {
  my ($self, $node, $node_id) = @_;
  if (ref($node) eq 'PPI::Token::Symbol') {
    return substr($node->content(), 0, 1) eq '@';
  }
  if ($self->expr_o->is_internal_node_type($node) && $node->{type} eq 'prefix_op') {
    my $kids = $self->expr_o->get_node_children($node_id);
    if (@$kids >= 1) {
      my $cast = $self->expr_o->get_a_node($kids->[0]);
      return ref($cast) eq 'PPI::Token::Cast' && $cast->content() eq '@';
    }
  }
  return 0;
}

# Returns true if a node (by ID) is a known list-returning expression.
# Used by gen_tree_val to decide whether to wrap in (vector ...) or not.
# Checks the AST structure — no string-matching on generated code.
sub _child_is_list_expr {
  my ($self, $node_id) = @_;
  my $node = $self->expr_o->get_a_node($node_id);

  # Array variable: @arr, @_  — already a vector
  if (ref($node) && $node->can('content')) {
    my $content = $node->content() // '';
    return 1 if $content =~ /^@/;
  }

  # Only internal (non-leaf) nodes below this point
  return 0 unless $self->expr_o->is_internal_node_type($node);

  my $type = $node->{type} // '';
  my $kids = $self->expr_o->get_node_children($node_id);

  # funcall: map, grep, sort, split, reverse, keys, values, each, etc.
  if ($type eq 'funcall' && @$kids >= 1) {
    my $func_node = $self->expr_o->get_a_node($kids->[0]);
    if (ref($func_node) && $func_node->can('content')) {
      my $fname = lc($func_node->content() // '');
      return 1 if $fname =~ /^(?:map|grep|sort|split|reverse|keys|values|each|unpack|readdir|localtime|caller|stat|lstat|getpwent|getgrent|getpwnam|getpwuid|getgrgid|getgrnam)$/;
    }
  }

  # tree_val (parenthesized expr): list if multiple children, or if single
  # child is itself list-returning.
  if ($type eq 'tree_val') {
    return 1 if @$kids > 1;
    return $self->_child_is_list_expr($kids->[0]) if @$kids == 1;
  }

  return 0;
}

# Returns true if the node is a list-generating expression for \(LIST) purposes:
# arrays, ranges, list-context functions (same as _child_is_list_expr but also
# covers the range operator .. since \(1..3) must spread into N scalar refs).
sub _is_list_node_for_refgen {
  my ($self, $node_id) = @_;
  return 1 if $self->_child_is_list_expr($node_id);
  # Range operator .. — binary op stored as PPI::Token::Operator with children
  my $node = $self->expr_o->get_a_node($node_id);
  if (ref($node) eq 'PPI::Token::Operator') {
    return 1 if ($node->content() // '') eq '..';
  }
  return 0;
}

# Generate \(T1, T2, ...) for multi-term comma lists.
# Perl rule: @/% vars → one ref each (ARRAY/HASH ref), ranges spread to N scalar refs,
# other terms → one scalar ref each.
sub _gen_backslash_multi_term {
  my ($self, $tv_kids) = @_;
  my $id = $g_refgen_count++;

  my @parts;  # each is ['single', CL_EXPR] or ['range', CL_EXPR]
  for my $kid_id (@$tv_kids) {
    my $kid_node = $self->expr_o->get_a_node($kid_id);
    my $is_range = ref($kid_node) eq 'PPI::Token::Operator'
                && ($kid_node->content() // '') eq '..';
    if ($is_range) {
      my $saved = $self->expr_o->get_node_context($kid_id);
      $self->expr_o->set_node_context($kid_id, LIST_CTX);
      my $kid_expr = $self->gen_node($kid_id);
      $self->expr_o->set_node_context($kid_id, $saved);
      push @parts, ['range', "(p-refgen-list $kid_expr)"];
    } else {
      # @/% vars, scalars, and everything else: one ref
      my $kid_expr = $self->gen_node($kid_id);
      push @parts, ['single', "(p-backslash $kid_expr)"];
    }
  }

  my $has_range = grep { $_->[0] eq 'range' } @parts;
  unless ($has_range) {
    # No ranges: simple vector of refs
    my $forms = join(' ', map { $_->[1] } @parts);
    return "(vector $forms)";
  }

  # Mix: use let + loop to concatenate variable-length parts
  my $result_var = "|--pcl-bsl-r$id--|";
  my $iter_var   = "|--pcl-bsl-x$id--|";
  my @stmts;
  for my $part (@parts) {
    if ($part->[0] eq 'range') {
      push @stmts, "(loop for $iter_var across $part->[1] do "
                 . "(vector-push-extend $iter_var $result_var))";
    } else {
      push @stmts, "(vector-push-extend $part->[1] $result_var)";
    }
  }
  my $stmts_str = join("\n  ", @stmts);
  return "(let (($result_var (make-array 4 :adjustable t :fill-pointer 0)))\n  "
       . "$stmts_str\n  $result_var)";
}


# Tree value (parenthesized expression): just generate the content
sub gen_tree_val {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $ctx = $self->expr_o->get_node_context($node_id);

  # If single child in scalar context, just return it
  # But in list context, we need (vector $x) for proper list assignment
  if (scalar(@$kids) == 1) {
    # In list context, propagate to child so split/funcs return lists not scalars
    if ($ctx == 1) {  # LIST_CTX = 1
      $self->expr_o->set_node_context($kids->[0], 1);
    }
    # Check at AST level (before codegen) whether child is a list-returning expr.
    # If so, we must NOT wrap it in (vector ...) — the child already returns a vector.
    my $child_is_list = ($ctx == 1) && $self->_child_is_list_expr($kids->[0]);
    my $child = $self->gen_node($kids->[0]);
    if ($ctx == 1) {  # LIST_CTX = 1
      # Special case: regex match already returns captures in list context
      # Don't wrap in vector, just ensure *wantarray* is set
      if ($child =~ /\(p-=~\s/) {
        return "(let ((*wantarray* t)) $child)";
      }
      return $child_is_list ? $child : "(vector $child)";
    }
    return $child;
  }

  # Multiple values
  my @forms;
  for my $kid_id (@$kids) {
    push @forms, $self->gen_node($kid_id);
  }

  my $forms_str = join(' ', @forms);

  # In list context, generate a vector instead of progn
  # This handles: @a = (1, 2, 3), foreach (1, 2, 3), etc.
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(vector $forms_str)";
  }

  # INHERIT context with multiple forms: the real context is only known at
  # runtime (e.g. this is a ternary branch inside `return`, where Perl treats
  # ($a, LIST) as a list in list context but as the comma operator in scalar
  # context).  p-flatten-args builds a flat vector (spreads raw vectors/
  # hashes, keeps boxes/refs).  Two restrictions, both load-bearing:
  # - only INHERIT_CTX: a statically scalar operand position (e.g. the
  #   comma exprs in cmpchain.t's `($e .= "a", $x) == ($e .= "b", $y)`) must
  #   stay a progn even when the *dynamic* *wantarray* happens to be t
  #   (the comparison sits inside join's list-context args).
  # - (eq *wantarray* t), not truthiness: :void takes the comma-operator
  #   branch (Sub::Defer: `*_subname = cond ? \&f : ($flag = 1, sub {...})`
  #   inside a :void-wrapped statement).
  if (@forms > 1 && $ctx == INHERIT_CTX) {
    return "(if (eq *wantarray* t) (p-flatten-args (list $forms_str)) (progn $forms_str))";
  }

  return "(progn $forms_str)";
}


# Generate filehandle marker for print/say
sub gen_filehandle {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Filehandle has one child - the actual handle name/variable
  if (@$kids) {
    my $fh = $self->gen_node($kids->[0]);
    # Quote bareword filehandles (FH, STDOUT, etc.) so CL doesn't try to evaluate them
    # Barewords are uppercase words without sigils
    if ($fh =~ /^[A-Z][A-Z0-9_]*$/) {
      return ":fh '$fh";
    }
    return ":fh $fh";
  }
  return ":fh nil";
}


# Generate readline operator <FH> or <$fh>
sub gen_readline {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Readline may have a filehandle child, or none (for <>)
  if (@$kids) {
    my $fh = $self->gen_node($kids->[0]);
    # Quote bareword filehandles (any word not starting with $ or ( )
    if ($fh =~ /^[A-Za-z_][A-Za-z0-9_]*$/) {
      return "(p-readline '$fh)";
    }
    return "(p-readline $fh)";
  }
  # Empty <> reads from ARGV or STDIN
  return "(p-readline)";
}


# Generate file glob <*.txt> or <$pattern>
# Handles negated character classes [!x] by generating grep filter,
# since SBCL's pathname wildcards don't support negation properly.
sub gen_glob {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Glob has a pattern child (string or interpolated)
  my $pattern_str;
  my $call;

  if (@$kids == 1) {
    $pattern_str = $self->gen_node($kids->[0]);
    $call = "(p-glob $pattern_str)";
  } elsif (@$kids > 1) {
    # Interpolated pattern - concatenate parts
    my @parts = map { $self->gen_node($_) } @$kids;
    my $concat = "(p-. " . join(' ', @parts) . ")";
    $pattern_str = $concat;
    $call = "(p-glob $concat)";
  } else {
    $pattern_str = '"*"';
    $call = "(p-glob)";
  }

  # Check for negated character class [!...] or [^...] in literal patterns
  # For these, generate a glob + filter since SBCL doesn't handle negation
  my $needs_filter = 0;
  my $negated_chars = '';
  my $modified_pattern = $pattern_str;

  if ($pattern_str =~ /^"([^"]*)"$/) {
    my $pat = $1;
    # Look for [!chars] or [^chars] - negated character class
    if ($pat =~ /\[([!\^])([^\]]+)\]/) {
      $needs_filter = 1;
      my $neg_marker = $1;
      $negated_chars = $2;
      # Replace negated class with ? wildcard for the glob
      my $simple_pat = $pat;
      $simple_pat =~ s/\[[!\^][^\]]+\]/?/g;
      $modified_pattern = qq{"$simple_pat"};
      $call = "(p-glob $modified_pattern)";
    }
  }

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  my $is_list_ctx = ($ctx == 1);  # LIST_CTX = 1

  if ($needs_filter) {
    # Generate: (remove-if (lambda (f) (find (char basename 0) "negated")) (p-glob pattern))
    # More Perl-like: filter files where the matched char is NOT in the negated set
    # Extract just the filename part for matching
    my $filter = qq{(remove-if (lambda (--f--) }
               . qq{(let ((--name-- (file-namestring (pathname --f--)))) }
               . qq{(and (> (length --name--) 0) }
               . qq{(find (char --name-- 0) "$negated_chars")))) }
               . qq{$call)};
    if ($is_list_ctx) {
      return "(let ((*wantarray* t)) $filter)";
    }
    return $filter;
  }

  if ($is_list_ctx) {
    return "(let ((*wantarray* t)) $call)";
  }
  return $call;
}


# Generate backtick command execution `command`
sub gen_backtick {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Backtick has a string child containing the command
  my $cmd = $self->gen_node($kids->[0]);
  return "(p-backtick $cmd)";
}


# Generate anonymous sub (for grep/map blocks)
# Output: (lambda () body)
sub gen_anon_sub {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Generate body from children
  my @body_parts;
  for my $kid_id (@$kids) {
    push @body_parts, $self->gen_node($kid_id);
  }
  my $body = join(' ', @body_parts);

  return "(lambda () $body)";
}


# Generate function reference for block callbacks
# Output: #'func-name
sub gen_func_ref {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  return $node->{raw_lambda} if $node->{raw_lambda};
  my $func_name = $node->{func_name};
  return "#'$func_name";
}

# Generate inline lambda for grep/map/sort blocks
# Output: (lambda (params) body)
sub gen_inline_lambda {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $params   = join(' ', @{$node->{params} // []});
  my $body     = $node->{body_cl} // 'nil';
  my $for_func = $node->{for_func} // '';

  # Named sort comparator (sort NAME LIST).
  # Perl sets $a/$b as package globals; the lambda params ($a $b) create dynamic
  # bindings so subs reading $a/$b globals still work.
  # For ($$) prototype subs (my($a,$b)=@_), pass $a $b as explicit args too.
  # For all other subs, pass no args (Perl's normal sort behaviour: @_ is empty).
  # If the function is undefined, dispatch to AUTOLOAD (Perl #30661).
  if ($for_func eq 'sort' && $node->{comparator_name}) {
    my $cl_func = $self->cl_name($node->{comparator_name});
    my $proto;
    if ($self->environment) {
      my $cname = $node->{comparator_name};
      $proto = $self->environment->get_prototype($cname)
            // $self->environment->get_prototype($cname =~ s/^:://r)
            // $self->environment->get_prototype($cname =~ s/.*:://r);
    }
    my $has_dollar_dollar = $proto && $proto->{is_proto}
                          && ($proto->{proto_string} // '') eq '$$';
    my $call_args = $has_dollar_dollar ? ' $a $b' : '';
    my $lambda_body = "(let ((*wantarray* nil))\n"
                    . "  (handler-case ($cl_func$call_args)\n"
                    . "    (undefined-function ()\n"
                    . "      (let ((al (intern \"PL-AUTOLOAD\" |sort--pkg|)))\n"
                    . "        (when (fboundp al) (funcall (symbol-function al)))))))";
    $kids = [];
    return "(let ((|sort--pkg| *package*))\n  (lambda ($params)\n    (catch :p-return\n      (block nil\n$lambda_body))))";
  }

  # Scalar comparator (sort $var LIST): call via p-sort-get-fn at runtime.
  # The lambda params $a/$b create dynamic bindings; p-sort-get-fn resolves
  # the scalar (coderef, string name, glob, or glob ref) to a CL function.
  if ($for_func eq 'sort' && $node->{scalar_cmp}) {
    my $scalar_cl = $kids && @$kids ? $self->gen_node($kids->[0]) : 'nil';
    # Capture *package* at lambda creation so p-sort-get-fn can look up
    # string sub names in the correct (user) package even when called from
    # inside p-sort (which runs in the :pcl package).
    my $lambda_body = "(let ((*wantarray* nil) (*package* |sort--pkg|))\n  (funcall (p-sort-get-fn $scalar_cl) \$a \$b))";
    $kids = [];
    return "(let ((|sort--pkg| *package*))\n  (lambda ($params)\n    (catch :p-return\n      (block nil\n$lambda_body))))";
  }

  # Sort comparator blocks may contain explicit `return` — wrap with catch.
  # Bind *wantarray* = nil (scalar): the comparator must return a scalar, and
  # Perl's wantarray() inside a comparator returns false (scalar context).
  # grep/map blocks do NOT get the catch: `return` inside them should
  # propagate to the enclosing sub's (catch :p-return ...).
  if ($for_func eq 'sort') {
    return "(lambda ($params)\n  (catch :p-return\n    (block nil\n(let ((*wantarray* nil))\n$body))))";
  }
  return "(lambda ($params)\n$body)";
}


# Generate substitution s///
# Output: (p-subst "pattern" "replacement" :g :i ...)
#         (p-subst pat-expr "replacement" :g :i ...)  when pattern has $var
#         (p-subst "pattern" (lambda () <cl-expr>) :g :i ...)  when replacement has $var
#         (p-subst "pattern" (lambda () <cl-expr>) :g :e ...)  when /e
sub gen_substitution {
  my $self = shift;
  my $node = shift;

  my $match = $node->get_match_string;
  my $subst = $node->get_substitute_string;
  my $mods  = $node->get_modifiers;

  my @mod_strs;
  for my $mod (sort keys %$mods) {
    push @mod_strs, ":$mod";
  }
  my $mods_str = @mod_strs ? ' ' . join(' ', @mod_strs) : '';

  # Build pattern CL expression (may be a string literal or a runtime string expression)
  my $match_cl;
  if (_has_regex_interpolation($match)) {
    $match_cl = _gen_interp_regex_pattern($match);  # CL expr evaluating to pattern string
  } else {
    $match =~ s/\\/\\\\/g;
    $match =~ s/"/\\"/g;
    $match_cl = qq{"$match"};
  }

  # s///e: replacement is Perl code — parse it and wrap in a lambda
  if ($mods->{e}) {
    my $cl_expr = $self->_compile_subst_e_expr($subst);
    return qq{(p-subst $match_cl (lambda () $cl_expr)$mods_str)};
  }

  # Replacement with variable interpolation: wrap in a lambda so $varname/$1..$9 evaluate at match time
  if (_has_regex_interpolation($subst)) {
    my $interp_expr = _gen_interp_replacement($subst);
    return qq{(p-subst $match_cl (lambda () $interp_expr)$mods_str)};
  }

  # Normal case: escape replacement for CL string literal (perl-to-ppcre-replacement handles $1..$9)
  $subst =~ s/\\/\\\\/g;
  $subst =~ s/"/\\"/g;
  return qq{(p-subst $match_cl "$subst"$mods_str)};
}

# Build a CL expression that evaluates to the interpolated replacement string.
# Handles $varname, $1-$9 (Perl backreferences, available as CL dynamic vars in lambda context).
sub _gen_interp_replacement {
  my ($str) = @_;
  my @parts;
  my $literal = '';
  my $i = 0;
  while ($i < length($str)) {
    my $c = substr($str, $i, 1);
    if ($c eq '\\') {
      my $next = $i+1 < length($str) ? substr($str, $i+1, 1) : '';
      if ($next =~ /[1-9]/) {
        # \1..\9 in replacement = backref, map to CL var $1..$9
        if (length($literal)) {
          (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
          push @parts, qq{"$esc"};
          $literal = '';
        }
        push @parts, "\$$next";
        $i += 2;
      } else {
        $literal .= substr($str, $i, 2);
        $i += 2;
      }
    } elsif ($c eq '$' && $i+1 < length($str) && substr($str, $i+1, 1) =~ /[1-9]/) {
      # $1..$9 backreference — available as CL dynamic variable inside the lambda
      my $n = substr($str, $i+1, 1);
      if (length($literal)) {
        (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
        push @parts, qq{"$esc"};
        $literal = '';
      }
      push @parts, "\$$n";
      $i += 2;
    } elsif ($c eq '$' && substr($str, $i) =~ /^\$\{([a-zA-Z_][a-zA-Z0-9_]*(?:::[a-zA-Z_][a-zA-Z0-9_]*)*)\}/) {
      my $varname = $1;
      if (length($literal)) {
        (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
        push @parts, qq{"$esc"};
        $literal = '';
      }
      push @parts, "\$$varname";
      $i += 3 + length($varname);
    } elsif ($c eq '$' && substr($str, $i) =~ /^\$([a-zA-Z_][a-zA-Z0-9_]*(?:::[a-zA-Z_][a-zA-Z0-9_]*)*)/) {
      my $varname = $1;
      if (length($literal)) {
        (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
        push @parts, qq{"$esc"};
        $literal = '';
      }
      push @parts, "\$$varname";
      $i += 1 + length($varname);
    } else {
      $literal .= $c;
      $i++;
    }
  }
  if (length($literal)) {
    (my $esc = $literal) =~ s/\\/\\\\/g; $esc =~ s/"/\\"/g;
    push @parts, qq{"$esc"};
  }
  return @parts == 0 ? '""'
       : @parts == 1 ? $parts[0]
       : "(p-string-concat " . join(" ", @parts) . ")";
}

# Parse a s///e replacement string as Perl and return CL code
sub _compile_subst_e_expr {
  my $self = shift;
  my $expr = shift;

  my $result = 'nil';
  eval {
    require PPI::Document;
    require Pl::PExpr;
    my $doc = PPI::Document->new(\$expr);

    # Significant (non-whitespace) top-level statements
    my @stmts = grep { !$_->isa('PPI::Token::Whitespace') } $doc->children;
    return unless @stmts;

    my @cl_parts;
    my @let_vars;  # variables declared with 'my' in the replacement

    for my $stmt (@stmts) {
      # Tokens (e.g. PPI::Token::Whitespace) have no children — skip
      next unless $stmt->can('children');
      my @parts = grep {
        ref($_) ne 'PPI::Token::Whitespace' && ref($_) ne 'PPI::Token::Structure'
      } $stmt->children;
      next unless @parts;

      # Detect 'my $var' declarations and collect the variable for a let wrapper
      if (ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content eq 'my'
          && @parts > 1 && ref($parts[1]) eq 'PPI::Token::Symbol') {
        push @let_vars, $parts[1]->content;
        # Drop the 'my' keyword — compile the rest as an assignment expression
        shift @parts;
      }

      my $expr_o = Pl::PExpr->new(
        e        => \@parts,
        full_PPI => $doc,
        ($self->environment ? (environment => $self->environment) : ()),
      );
      my $node_id = $expr_o->parse_expr_to_tree(\@parts);
      my $gen = Pl::ExprToCL->new(
        expr_o       => $expr_o,
        environment  => $self->environment,
        indent_level => $self->indent_level,
      );
      my $cl = $gen->generate($node_id);
      push @cl_parts, $cl if defined $cl && $cl ne '';
    }

    return unless @cl_parts;

    my $body = @cl_parts == 1 ? $cl_parts[0] : '(progn ' . join(' ', @cl_parts) . ')';
    if (@let_vars) {
      my $bindings = join(' ', map { "($_ (make-p-box nil))" } @let_vars);
      $result = "(let ($bindings) $body)";
    } else {
      $result = $body;
    }
  };
  if ($@) {
    warn "Failed to compile s///e expression '$expr': $@";
  }
  return $result;
}


# Generate transliteration tr/// or y///
# Output: (p-tr "from" "to" :c :d :s ...)
sub gen_transliteration {
  my $self = shift;
  my $node = shift;

  my $from = $node->get_match_string;
  my $to   = $node->get_substitute_string;
  my $mods = $node->get_modifiers;

  # Process tr escape sequences to actual characters, then build safe CL literals
  my $from_cl = _cl_string_literal(_expand_tr_escapes($from));
  my $to_cl   = _cl_string_literal(_expand_tr_escapes($to));

  my @mod_strs;
  for my $mod (sort keys %$mods) {
    push @mod_strs, ":$mod";
  }

  my $mods_str = @mod_strs ? ' ' . join(' ', @mod_strs) : '';
  return "(p-tr $from_cl $to_cl$mods_str)";
}

# Process tr/// string escape sequences (no interpolation, but \xHH etc. apply)
sub _expand_tr_escapes {
  my $str = shift;
  $str =~ s!\\(x\{[^}]*\}|x[0-9A-Fa-f]{1,2}|x|o\{[^}]*\}|N\{U\+[0-9A-Fa-f]+\}|[0-7]{1,3}|c.|[ntraefbd"\\/])!
    _process_tr_escape($1)
  !ge;
  return $str;
}

sub _process_tr_escape {
  my $esc = shift;
  return "\n" if $esc eq 'n';
  return "\t" if $esc eq 't';
  return "\r" if $esc eq 'r';
  return "\a" if $esc eq 'a';
  return "\e" if $esc eq 'e';
  return "\f" if $esc eq 'f';
  return "\b" if $esc eq 'b';
  return "\\" if $esc eq '\\';
  return '"'  if $esc eq '"';
  return '/'  if $esc eq '/';
  return "\x00" if $esc eq '0';
  if ($esc =~ /^c(.)$/) {
    return chr(ord(uc($1)) ^ 64);
  }
  if ($esc =~ /^x\{([^}]*)\}$/) {
    return _hex_brace_escape($1);
  }
  if ($esc =~ /^x([0-9A-Fa-f]{1,2})$/) {
    return chr(hex($1));
  }
  return chr(0) if $esc eq 'x';
  if ($esc =~ /^o\{([^}]*)\}$/) {
    return _octal_brace_escape($1);
  }
  if ($esc =~ /^N\{U\+([0-9A-Fa-f]+)\}$/) {
    return chr(hex($1));
  }
  if ($esc =~ /^([0-7]{1,3})$/) {
    return chr(oct($1));
  }
  return $esc;  # unknown \X -> X
}


# Helper: \x{HHHH} - parse hex with spaces/underscores, convert to chr
# Rules: spaces stripped, leading underscores stripped,
# single underscores between digits ok, double underscores stop parsing
sub _hex_brace_escape {
  my $inner = shift;
  $inner =~ s/\s//g;   # strip spaces
  $inner =~ s/^_+//;   # strip leading underscores
  return chr(0) if $inner eq '';  # \x{} = chr(0)
  # Parse: hex digits with optional single underscores between them
  my $hex = '';
  while ($inner =~ /\G([0-9A-Fa-f]+)(_(?=[0-9A-Fa-f]))?/gc) {
    $hex .= $1;
  }
  return chr(0) if $hex eq '';
  return chr(hex($hex));
}

# Helper: \o{OOO} - parse octal with spaces/underscores, convert to chr
sub _octal_brace_escape {
  my $inner = shift;
  $inner =~ s/\s//g;  # strip spaces
  return chr(0) if $inner eq '';
  # Parse: octal digits with optional single underscores between them
  my $oct = '';
  while ($inner =~ /\G([0-7]+)(_(?=[0-7]))?/gc) {
    $oct .= $1;
  }
  return chr(0) if $oct eq '';
  return chr(oct($oct));
}

# Single-pass escape sequence processor for double-quoted strings
# Handles all \X sequences including unknown ones (\. → .)
sub _process_dq_escape {
  my $esc = shift;
  return "\n" if $esc eq 'n';
  return "\t" if $esc eq 't';
  return "\r" if $esc eq 'r';
  return "\a" if $esc eq 'a';
  return "\e" if $esc eq 'e';
  return "\f" if $esc eq 'f';
  return "\$" if $esc eq '$';
  return '@'  if $esc eq '@';
  return '"'  if $esc eq '"';
  return "\\" if $esc eq '\\';
  # \cX - control character
  if ($esc =~ /^c(.)$/) {
    return chr(ord(uc($1)) ^ 64);
  }
  # \x{HHHH} - hex with braces
  if ($esc =~ /^x\{([^}]*)\}$/) {
    return _hex_brace_escape($1);
  }
  # \xHH - hex 1-2 digits
  if ($esc =~ /^x([0-9A-Fa-f]{1,2})$/) {
    return chr(hex($1));
  }
  # \x alone - chr(0)
  return chr(0) if $esc eq 'x';
  # \o{OOO} - octal with braces
  if ($esc =~ /^o\{([^}]*)\}$/) {
    return _octal_brace_escape($1);
  }
  # \N{U+HHHH} - named Unicode character by code point
  if ($esc =~ /^N\{U\+([0-9A-Fa-f]+)\}$/) {
    return chr(hex($1));
  }
  # \NNN - octal digits
  if ($esc =~ /^([0-7]{1,3})$/) {
    return chr(oct($1));
  }
  # Case-changing escapes: preserve as markers for _apply_case_escapes
  if ($esc =~ /^[ULulQFE]$/) {
    return "\\$esc";  # keep as \U, \L, etc. for later processing
  }
  # Unknown escape: \X → X (Perl drops the backslash)
  return $esc;
}

# Apply \U, \L, \u, \l, \Q, \F ... \E case transformations to a string
# These are processed after escape sequences, on the final text
sub _apply_case_escapes {
  my $str = shift;
  # Quick check: if no case escapes, return unchanged
  return $str unless $str =~ /\\/;

  my $result  = '';
  my @modes   = ();  # stack: 'U', 'L', 'Q', 'F'
  my $pending = undef;  # 'u' or 'l' — single-char transform for next char

  while (length $str) {
    if ($str =~ s/^\\([ULulQFE])//) {
      my $cmd = $1;
      if ($cmd eq 'E') {
        # \E cancels any pending single-char transform
        $pending = undef;
        # Pop the innermost mode.  If it was U or L, also remove all other
        # U/L modes (they're mutually exclusive case transforms).
        if (@modes) {
          my $popped = pop @modes;
          if ($popped eq 'U' || $popped eq 'L') {
            @modes = grep { $_ ne 'U' && $_ ne 'L' } @modes;
          }
        }
      } elsif ($cmd eq 'u' || $cmd eq 'l') {
        $pending = $cmd;
      } else {
        # \U, \L, \Q, \F — push onto mode stack
        push @modes, $cmd;
      }
    } elsif ($str =~ s/^((?:[^\\]|\\(?![ULulQFE]))+)//) {
      # Consume a run of literal text (non-case-escape content).
      # \\(?![ULulQFE]) matches \ not followed by a case-escape char,
      # including a lone trailing backslash at end-of-string.
      my $text = $1;
      if ($pending && length $text) {
        # The pending \u/\l applies to the FIRST character only,
        # overriding the current mode stack for that one char.
        my $first = $pending eq 'u' ? uc(substr($text, 0, 1))
                                    : lc(substr($text, 0, 1));
        $pending = undef;
        $result .= $first;
        $result .= _apply_mode(\@modes, substr($text, 1)) if length($text) > 1;
      } else {
        $result .= _apply_mode(\@modes, $text);
      }
    } else {
      last;  # shouldn't happen
    }
  }

  return $result;
}

# Apply the current mode stack to a piece of text
sub _apply_mode {
  my ($modes, $text) = @_;
  return $text unless @$modes && length($text);
  for my $mode (@$modes) {
    if ($mode eq 'U') { $text = uc($text); }
    elsif ($mode eq 'L') { $text = lc($text); }
    elsif ($mode eq 'F') { $text = lc($text); }  # fc ≈ lc for ASCII
    elsif ($mode eq 'Q') { $text = quotemeta($text); }
  }
  return $text;
}

# Convert Perl string with escapes to CL string
# Perl "\n" -> actual newline in CL string
sub convert_perl_string {
  my $self = shift;
  my $str = shift;

  # Determine quote type and extract content
  my $quote_char;
  my $content;

  if ($str =~ /^'(.*)'$/s) {
    # Single-quoted: no escape processing except \\ and \'
    $content = $1;
    $content =~ s/\\'/'/g;
    $content =~ s/\\\\/\\/g;
    # For CL, escape backslashes and quotes
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  elsif ($str =~ /^"(.*)"$/s) {
    # Double-quoted: process Perl escapes
    $content = $1;
  }
  elsif ($str =~ /^qq\s*\{(.*)\}$/s || $str =~ /^qq\s*\((.*)\)$/s ||
         $str =~ /^qq\s*\[(.*)\]$/s || $str =~ /^qq\s*<(.*)>$/s) {
    # qq{}, qq(), qq[], qq<> style (optional whitespace between qq and delimiter)
    $content = $1;
  }
  elsif ($str =~ /^qq\s*(.)(.*)(\1)$/s) {
    # qq/.../ or qq '...' style (optional whitespace between qq and delimiter)
    $content = $2;
  }
  elsif ($str =~ /^q\s*\{(.*)\}$/s || $str =~ /^q\s*\((.*)\)$/s ||
         $str =~ /^q\s*\[(.*)\]$/s || $str =~ /^q\s*<(.*)>$/s) {
    # q{}, q(), q[], q<> style - like single-quoted, no interpolation (optional whitespace)
    $content = $1;
    $content =~ s/\\\\/\\/g;    # only \\ is special in q{}
    # For CL, escape backslashes and quotes
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  elsif ($str =~ /^q\s*(.)(.*)(\1)$/s) {
    # q/.../ or q '...' style (optional whitespace between q and delimiter)
    $content = $2;
    $content =~ s/\\\\/\\/g;
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  else {
    # Unknown format, return as-is
    return $str;
  }

  # Process Perl escape sequences in single pass to handle \\ correctly
  $content =~ s!\\(x\{[^}]*\}|x[0-9A-Fa-f]{1,2}|x|o\{[^}]*\}|N\{[^}]*\}|[0-7]{1,3}|c.|[ntreafd"\\\$\@]|.)!
    _process_dq_escape($1)
  !ge;

  # Apply \U, \L, \u, \l, \Q, \F ... \E transformations (non-interpolated strings)
  $content = _apply_case_escapes($content);

  return _cl_string_literal($content);
}

# Build a CL string literal, escaping surrogate and non-character codepoints
# that can't be embedded in a UTF-8 source file.
sub _cl_string_literal {
  my $content = shift;
  # Characters invalid in UTF-8: surrogates U+D800-U+DFFF, and non-chars U+FFFE/U+FFFF
  # (and the pattern repeats at every 0x10000 boundary: U+1FFFE, U+1FFFF, etc.)
  my $bad_char_re = qr/[\x{D800}-\x{DFFF}]|[\x{FFFE}\x{FFFF}]/;
  if ($content !~ $bad_char_re) {
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  # Build a (concatenate 'string ...) form with code-char for bad chars
  my @parts;
  while (length $content) {
    if ($content =~ /\A((?:[^\x{D800}-\x{DFFF}\x{FFFE}\x{FFFF}])+)/s) {
      my $safe = $1;
      $safe =~ s/\\/\\\\/g;
      $safe =~ s/"/\\"/g;
      push @parts, qq{"$safe"};
      $content = substr($content, length($1));
    } else {
      my $ch = substr($content, 0, 1);
      push @parts, "(string (code-char " . ord($ch) . "))";
      $content = substr($content, 1);
    }
  }
  return @parts == 1 ? $parts[0] : "(concatenate 'string " . join(' ', @parts) . ")";
}


1;
