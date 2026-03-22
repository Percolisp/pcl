package Pl::ExprToCL;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.30;
use strict;
use warnings;

use Moo;

use Scalar::Util qw/looks_like_number/;
use Pl::PExpr qw(SCALAR_CTX LIST_CTX);

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
    'string_concat' => \&gen_string_concat,
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
  continue cos cwd decf declare-sub defined defpackage delete delete-array delete-array-slice
  delete-hash-slice delete-kv-hash-slice die do each ensure-arrayref ensure-hashref env-get
  env-set eof eval eval-block eval-direct exception exception-object exists exists-array exit
  exp fc fileno flatten flatten-args for foreach funcall-ref get-class get-coderef getc getcwd
  getgrent getgrgid getgrnam endgrent setgrent
  gethash gethash-box gethash-deref glob glob-assign glob-copy glob-slot glob-undef-name gmtime
  grep hash hash-= hex hslice if incf index int isa join keys kv-aslice kv-hslice last last-dynamic lc
  lcfirst length let list-= list-x local-glob localtime log lstat make-typeglob map method-call
  mkdir my my-= next not oct open opendir or or-assign ord our pack pipe pop pos post++ post-- pre++
  pre-- print printf prototype push quotemeta rand read readdir readline redo ref reftype regex
  rename require require-file reset resolve-invocant return reverse rewinddir rindex rmdir say
  scalar scalar-= seek select set-array-length set_up_inc setf shift sin sleep sort splice split
  sprintf sqrt srand stat str-cmp str-eq str-ge str-gt str-le str-lt str-ne str-x str-x=
  string-concat study sub sub-defined sub-exists subst substr super-call sysread system syswrite
  tell tie tie-proxy tie-proxy-p tie-proxy-saved-value tie-proxy-tie-obj tied time times tr
  truncate typeglob typeglob-name typeglob-p typeglob-package uc ucfirst undef undef-sub unless
  unlink unpack unshift untie until use values vec version-string wantarray warn while xor ||
);

# Only exceptions that need different CL names than p-<perl-op>
# If not listed here, the CL name is p-<perl-name> (runtime) or p-<perl-name> (user)
my %OP_EXCEPTIONS = (
  # Bitwise operators - avoid confusion with CL's & and |
  '&'   => 'p-bit-and',
  '|'   => 'p-bit-or',
  '^'   => 'p-bit-xor',
  '~'   => 'p-bit-not',

  # Assignment variants with clearer names
  '='   => 'p-setf',
  '+='  => 'p-incf',
  '-='  => 'p-decf',

  # Compound assignment - bitwise
  '&='  => 'p-bit-and=',
  '|='  => 'p-bit-or=',
  '^='  => 'p-bit-xor=',

  # Compound assignment - logical
  '&&=' => 'p-and-assign',
  '||=' => 'p-or-assign',

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
  '$\\' => '|$\\|',
  '$"'  => '|$"|',
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
  my $self      = shift;
  my $perl_name = shift;

  # Guard against undefined input
  return 'p-UNDEFINED' unless defined $perl_name && length($perl_name);

  # Check for operator exceptions first
  return $OP_EXCEPTIONS{$perl_name} if exists $OP_EXCEPTIONS{$perl_name};

  # Leading :: means main:: (e.g. ::is → main::is)
  $perl_name =~ s/^::/main::/;

  # Check for package-qualified name (Foo::bar or Foo::Bar::baz)
  if ($perl_name =~ /^(.+)::(.+)$/) {
    my ($pkg, $func) = ($1, $2);
    # CORE:: is Perl's built-in namespace — strip it and use the PCL built-in
    if ($pkg eq 'CORE') {
      return exists $RUNTIME_NAMES{$func} ? "p-$func" : "pl-$func";
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

sub gen_leaf {
  my $self = shift;
  my $node = shift;

  my $ref  = ref($node);

  # Variable (like $x, @arr, %hash)
  if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
    my $content = $node->content() // '';
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
    # Handle &subname - call subroutine
    # &foo -> (p-foo) - calls without passing @_ through
    # Note: &foo(@args) would be handled as funcall, not here
    if ($content =~ /^&(.+)$/) {
      my $func_name = $1;
      my $cl_func = $self->cl_name($func_name);
      return "($cl_func)";
    }
    # Check if this var is a state variable that was renamed
    if ($self->environment) {
      my $renames = $self->environment->state_var_renames;
      return $renames->{$content} if $renames && exists $renames->{$content};
    }
    # Unknown ${^...} caret variables — die so missing cases surface clearly.
    if ($content =~ /^\$\{\^/) {
      my $line = eval { $node->line_number } // '?';
      die "PCL: unsupported special variable '$content' at line $line\n";
    }
    return $content;
  }

  # Array last index ($#arr)
  if ($ref eq 'PPI::Token::ArrayIndex') {
    my $content = $node->content();
    # $#arr -> (p-array-last-index @arr)
    $content =~ s/^\$#/@/;
    return "(p-array-last-index $content)";
  }

  # Number literal - convert Perl format to CL format
  # (includes subclasses: ::Hex, ::Binary, ::Octal, ::Float, ::Exp, ::Version)
  if ($ref =~ /^PPI::Token::Number/) {
    my $num = $node->content();

    # Version strings: v1.20.300 -> string of chr values
    if ($num =~ /^v(\d[\d.]*)$/) {
      my @parts = split /\./, $1;
      my $str = join('', map { chr($_) } @parts);
      # Escape for CL string literal
      $str =~ s/\\/\\\\/g;
      $str =~ s/"/\\"/g;
      # For non-printable chars, use CL character escapes or just embed
      # Convert to a runtime call for safety with high codepoints
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
    # Remove trailing newline if present (heredocs include it)
    chomp $content;
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


# Binary operator: (pcl:p-OP left right)
# Operators always use pcl: prefix to avoid conflicts with user-defined subs
sub gen_binary_op {
  my $self    = shift;
  my $op      = shift;
  my $kids    = shift;
  my $node_id = shift;  # Optional: for context-dependent operators like 'x'

  my $cl_op = $self->cl_op_name($op);

  # Special case: 'x' operator - use list repeat when LHS is parenthesized and in list context
  if ($op eq 'x' && defined $node_id) {
    my $lhs_node = $self->expr_o->get_a_node($kids->[0]);
    my $lhs_is_paren = $self->expr_o->is_internal_node_type($lhs_node) &&
                       ($lhs_node->{type} eq 'tree_val' || $lhs_node->{type} eq 'progn');
    my $ctx = $self->expr_o->get_node_context($node_id);
    if ($lhs_is_paren && $ctx == 1) {  # LIST_CTX = 1
      # List repeat: (@x,1) x 4 — force LHS to list context so
      # gen_progn returns (vector ...) not (progn ...) / scalar last-val
      $self->expr_o->set_node_context($kids->[0], 1);  # LIST_CTX = 1
      my $left  = $self->gen_node($kids->[0]);
      my $right = $self->gen_node($kids->[1]);
      return "(p-list-x $left $right)";
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
  # %h = () or %h = (a => 1) should use (p-hash ...), not (vector ...)
  if ($op eq '=' && $left =~ /^%/) {
    my $rhs_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($rhs_node)) {
      my $rhs_type = $rhs_node->{type};
      if ($rhs_type eq 'tree_val' || $rhs_type eq 'progn') {
        my $rhs_kids = $self->expr_o->get_node_children($kids->[1]);
        if (@$rhs_kids == 0) {
          # Empty hash
          return "(p-hash-= $left (p-hash))";
        } else {
          # Hash with initial values - generate (p-hash k1 v1 k2 v2 ...)
          my @parts = map { $self->gen_node($_) } @$rhs_kids;
          my $hash_init = "(p-hash " . join(" ", @parts) . ")";
          return "(p-hash-= $left $hash_init)";
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

  # For assignment, dispatch to type-specific forms based on LHS sigil
  if ($op eq '=') {
    if ($left =~ /^\(vector /) {
      return "(p-list-= $left $right)";
    } elsif ($left =~ /^[\@]/) {
      return "(p-array-= $left $right)";
    } elsif ($left =~ /^%/) {
      return "(p-hash-= $left $right)";
    } elsif ($left =~ /^\$/) {
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
    }
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
    my $generated = $self->gen_node($kid_id) // '';

    # Check if this is an array variable (@arr) - needs to be joined
    my $kid_content = (ref($kid_node) eq 'PPI::Token::Symbol' && $kid_node->can('content'))
                      ? ($kid_node->content() // '') : '';
    if ($kid_content =~ /^@/) {
      # In Perl, "@arr" in string interpolation joins with $" (default space)
      # Use |$"| which is the CL variable for Perl's $" list separator
      push @parts, '(p-join |$"| ' . $generated . ')';
    } else {
      push @parts, $generated;
    }
  }
  return "(p-string-concat " . join(" ", @parts) . ")";
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
  my $cl_func   = $self->cl_name($func_name);

  # Special handling for next/last/redo with label argument
  if (($func_name eq 'next' || $func_name eq 'last' || $func_name eq 'redo') && @$kids == 2) {
    # Check if the argument is a bareword label (funcall with single word child)
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids == 1) {
        my $label_node = $self->expr_o->get_a_node($arg_kids->[0]);
        if (ref($label_node) eq 'PPI::Token::Word') {
          my $label = $label_node->content();
          return "($cl_func $label)";
        }
      }
    }
  }

  # Special handling for do { } blocks - evaluates block inline, returns last value
  if ($func_name eq 'do' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      if ($arg_node->{type} eq 'func_ref') {
        my $func_ref = $self->gen_node($kids->[1]);
        return "(funcall $func_ref)";
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
        # do { BLOCK } parsed as inline_lambda - just call it
        my $body = $arg_node->{body_cl} // 'nil';
        return "(progn $body)";
      }
    }
  }

  # Special handling for eval { } blocks
  # eval { block } catches exceptions and sets $@
  if ($func_name eq 'eval' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      if ($arg_node->{type} eq 'anon_sub') {
        # eval { block } with inline anon_sub - generate p-eval-block with body
        my $block_kids = $self->expr_o->get_node_children($kids->[1]);
        my @body_parts;
        for my $kid_id (@$block_kids) {
          push @body_parts, $self->gen_node($kid_id);
        }
        my $body = join(' ', @body_parts);
        return "(p-eval-block $body)";
      }
      elsif ($arg_node->{type} eq 'inline_lambda') {
        # eval { block } parsed as inline_lambda (avoids defun side-effect)
        my $body = $arg_node->{body_cl} // 'nil';
        return "(p-eval-block $body)";
      }
      elsif ($arg_node->{type} eq 'func_ref') {
        # eval { block } with named function (from Parser callback)
        # Generate p-eval-block that calls the function
        my $func_ref = $self->gen_node($kids->[1]);
        return "(p-eval-block (funcall $func_ref))";
      }
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
            # Handle special tokens: __PACKAGE__, __FILE__, __LINE__
            if ($classname eq '__PACKAGE__') {
              my $pkg = $self->environment
                  ? $self->environment->current_package : 'main';
              $pkg //= 'main';
              $class_arg = qq{"$pkg"};
              $is_bareword = 1;
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
        # Convert $a to @a for array
        if (ref($arr_node) eq 'PPI::Token::Symbol' && $arr =~ /^\$/) {
          $arr =~ s/^\$/\@/;
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
        # Convert $h to %h for hash
        if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\$/) {
          $hash =~ s/^\$/\%/;
        }
        my $key = $self->gen_node($arg_kids->[1]);
        return "(p-delete $hash $key)";
      }
    }
    # Hash slice: delete @foo{4,5} -> (p-delete-hash-slice %hash key1 key2 ...)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'slice_h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $hash = $self->gen_node($arg_kids->[0]);
        # Convert @ to % for hash access (@ is context sigil, % is container sigil)
        if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\@/) {
          $hash =~ s/^\@/\%/;
        }
        my @keys;
        for my $i (1 .. $#$arg_kids) {
          push @keys, $self->gen_node($arg_kids->[$i]);
        }
        my $keys_str = join(' ', @keys);
        return "(p-delete-hash-slice $hash $keys_str)";
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
          if (ref($arr_node) eq 'PPI::Token::Symbol' && $arr =~ /^\$/) {
            $arr =~ s/^\$/\@/;
          }
          my $idx = $self->gen_node($arg_kids->[1]);
          return "(p-exists-array $arr $idx)";
        }
        elsif ($arg_node->{type} eq 'h_acc') {
          # Hash access: exists $h{key} -> (p-exists %h key)
          my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
          my $hash = $self->gen_node($arg_kids->[0]);
          if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\$/) {
            $hash =~ s/^\$/\%/;
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

  my $args_str = @args ? ' ' . join(' ', @args) : '';
  my $call = "($cl_func$args_str)";

  my $ctx = $self->expr_o->get_node_context($node_id);

  # split: p-split always returns a vector; no *wantarray* wrapper needed.
  # Arguments must NOT be evaluated in list context (e.g. =~ as pattern arg
  # would return captures vector instead of 1/0 if *wantarray* is t).
  if ($func_name eq 'split') {
    return $ctx == 0 ? "(length $call)" : $call;
  }

  # Wrap in dynamic wantarray binding for list context
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
  }

  return $call;
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
  }

  # Rest are arguments
  my @args;
  for my $i (2 .. $#$kids) {
    push @args, $self->gen_node($kids->[$i]);
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';

  # Check for SUPER:: method call
  my $call;
  if ($method =~ /^SUPER::(.+)$/) {
    my $real_method = $1;
    # Need current package for SUPER:: lookup
    my $current_pkg = $self->environment ? $self->environment->current_package : 'main';
    $call = "(p-super-call $obj '$real_method \"$current_pkg\"$args_str)";
  } elsif ($is_dynamic_method) {
    # Dynamic method call: $obj->$method_var
    # Method name is in a variable, pass the variable value
    $call = "(p-method-call $obj $method$args_str)";
  } else {
    $call = "(p-method-call $obj '$method$args_str)";
  }

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
  }
  return $call;
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

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
  }
  return $call;
}


# Ternary: (p-if cond then else)
sub gen_ternary {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Check if condition is wantarray - if so, the 'then' branch should be in list context
  my $cond_node = $self->expr_o->get_a_node($kids->[0]);
  my $is_wantarray_cond = 0;

  if ($self->expr_o->is_internal_node_type($cond_node) &&
      $cond_node->{type} eq 'funcall') {
    my $cond_kids = $self->expr_o->get_node_children($kids->[0]);
    if (@$cond_kids) {
      my $func_node = $self->expr_o->get_a_node($cond_kids->[0]);
      if (!$self->expr_o->is_internal_node_type($func_node) &&
          $func_node->can('content') &&
          $func_node->content eq 'wantarray') {
        $is_wantarray_cond = 1;
      }
    }
  }

  my $cond = $self->gen_node($kids->[0]);

  # If condition is wantarray, set list context on 'then' branch
  if ($is_wantarray_cond) {
    $self->expr_o->set_node_context($kids->[1], 1);  # LIST_CTX = 1
  }

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
  if ($op eq '\\') {
    my $operand_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($operand_node) eq 'PPI::Token::Symbol' &&
        $operand_node->content() =~ /^&(.+)$/) {
      my $func_name = $1;
      my $cl_func = $self->cl_name($func_name);
      return "#'$cl_func";
    }
  }

  # $#{ array } — last index of array (braced form of $#array)
  # PPI tokenises $#{@a} as Cast[$#] + Block{@a}; handle before cl_name()
  if ($op eq '$#') {
    my $arr_expr = $self->gen_node($kids->[1]);
    return "(p-array-last-index $arr_expr)";
  }

  # ++, --, and \ need l-value context for array/hash elements
  # \ needs l-value to get a reference to the box, not a copy of the value
  my $needs_lvalue = ($op eq '++' || $op eq '--' || $op eq '\\');
  my $saved_lvalue = $self->lvalue_context;
  $self->lvalue_context(1) if $needs_lvalue;
  my $operand = $self->gen_node($kids->[1]);
  $self->lvalue_context($saved_lvalue);

  # Get CL name for the operator
  my $cl_op = $self->cl_name($op);

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
  elsif ($op eq '@' || $op eq '%' || $op eq '$') {
    $cl_op = "p-cast-$op";
  }
  # & Cast: &{expr} or &$var - dynamic coderef by name
  # \&{expr} becomes (p-backslash (p-get-coderef expr)), which returns the
  # function directly (p-backslash passes through non-box values).
  elsif ($op eq '&') {
    return "(p-get-coderef $operand)";
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

  my $arr = $self->gen_node($kids->[0]);
  my $idx = $self->gen_node($kids->[1]);

  # Convert $varname to @varname (Perl $arr[i] accesses @arr)
  # Handle both plain $arr and package-qualified Pkg::$arr
  $arr =~ s/(^|::)\$/$1@/;

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
  my $key  = $self->gen_node($kids->[1]);

  # Convert $varname to %varname (Perl $hash{k} accesses %hash)
  # Handle both plain $hash and package-qualified Pkg::$hash
  $hash =~ s/(^|::)\$/$1%/;

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

  # qw[...][idx] or (LIST)[idx]: if LHS is a progn (list literal), force LIST_CTX
  # so gen_progn produces (vector ...) that p-aref-deref can index into.
  my $child0_node = $self->expr_o->get_a_node($kids->[0]);
  if ($self->expr_o->is_internal_node_type($child0_node)
      && $child0_node->{type} eq 'progn') {
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
  my $key = $self->gen_node($kids->[1]);

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
    push @indices, $self->gen_node($kids->[$i]);
  }

  my $idx_str = join(' ', @indices);
  return "(p-aslice $arr $idx_str)";
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
  # Convert @ to % for hash access (@ is context sigil, % is container sigil)
  if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\@/) {
    $hash =~ s/^\@/\%/;
  }
  my @keys;
  for my $i (1 .. $#$kids) {
    push @keys, $self->gen_node($kids->[$i]);
  }

  my $key_str = join(' ', @keys);
  return "(p-hslice $hash $key_str)";
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
    push @keys, $self->gen_node($kids->[$i]);
  }

  my $key_str = join(' ', @keys);
  return "(p-kv-hslice $hash $key_str)";
}


# KV array slice: %arr[indices] - returns key-value pairs
sub gen_kv_array_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $arr = $self->gen_node($kids->[0]);
  # %arr[...] uses @ sigil for the array variable in CL
  $arr =~ s/^\%/\@/;
  # %$ref[...] — $ref is a scalar holding an array ref; unbox to get the vector
  $arr = "(unbox $arr)" if $arr =~ /^\$/;
  my @indices;
  for my $i (1 .. $#$kids) {
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

  my @elements;
  for my $kid_id (@$kids) {
    push @elements, $self->gen_node($kid_id);
  }

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

  # In unknown context with multiple forms, check wantarray at runtime.
  # Wrap @array items with (p-flatten ...) so %p-collect-list in
  # %p-flatten-for-list can spread @arrays while keeping arrayrefs as scalars.
  if (@forms > 1 && $ctx == SCALAR_CTX) {
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
    my $child = $self->gen_node($kids->[0]);
    if ($ctx == 1) {  # LIST_CTX = 1
      # Special case: regex match already returns captures in list context
      # Don't wrap in vector, just ensure *wantarray* is set
      if ($child =~ /\(p-=~\s/) {
        return "(let ((*wantarray* t)) $child)";
      }
      return "(vector $child)";
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
    # Quote bareword filehandles
    if ($fh =~ /^[A-Z][A-Z0-9_]*$/) {
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

  # Named sort comparator (sort NAME LIST): generate body as a 0-arg call to the sub.
  # The lambda params $a/$b are CL special vars (defvar'd), so they create dynamic
  # bindings that the named sub reads when it accesses $a/$b as globals.
  if ($for_func eq 'sort' && $node->{comparator_name}) {
    my $cl_func = $self->cl_name($node->{comparator_name});
    $body = "($cl_func)";
  }

  # Sort comparator blocks may contain explicit `return` — wrap with catch.
  # grep/map blocks do NOT get the catch: `return` inside them should
  # propagate to the enclosing sub's (catch :p-return ...).
  if ($for_func eq 'sort') {
    return "(lambda ($params)\n  (catch :p-return\n    (block nil\n$body)))";
  }
  return "(lambda ($params)\n$body)";
}


# Generate substitution s///
# Output: (p-subst "pattern" "replacement" :g :i ...)
#         (p-subst "pattern" (lambda () <cl-expr>) :g :e ...)  when /e
sub gen_substitution {
  my $self = shift;
  my $node = shift;

  my $match = $node->get_match_string;
  my $subst = $node->get_substitute_string;
  my $mods  = $node->get_modifiers;

  # Escape backslashes and quotes in pattern for CL string literal
  $match =~ s/\\/\\\\/g;
  $match =~ s/"/\\"/g;

  my @mod_strs;
  for my $mod (sort keys %$mods) {
    push @mod_strs, ":$mod";
  }
  my $mods_str = @mod_strs ? ' ' . join(' ', @mod_strs) : '';

  # s///e: replacement is Perl code — parse it and wrap in a lambda
  if ($mods->{e}) {
    my $cl_expr = $self->_compile_subst_e_expr($subst);
    return qq{(p-subst "$match" (lambda () $cl_expr)$mods_str)};
  }

  # Normal case: escape replacement for CL string literal
  $subst =~ s/\\/\\\\/g;
  $subst =~ s/"/\\"/g;
  return qq{(p-subst "$match" "$subst"$mods_str)};
}

# Parse a s///e replacement string as Perl and return CL code
sub _compile_subst_e_expr {
  my $self = shift;
  my $expr = shift;

  my $result = 'nil';
  eval {
    require PPI::Document;
    require Pl::PExpr;
    my $doc   = PPI::Document->new(\$expr);
    my @stmts = $doc->children;
    return unless @stmts;
    my @parts = grep { ref($_) ne 'PPI::Token::Whitespace' } $stmts[0]->children;
    return unless @parts;
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
    $result = $gen->generate($node_id);
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

  # Escape quotes
  $from =~ s/"/\\"/g;
  $to   =~ s/"/\\"/g;

  my @mod_strs;
  for my $mod (sort keys %$mods) {
    push @mod_strs, ":$mod";
  }

  my $mods_str = @mod_strs ? ' ' . join(' ', @mod_strs) : '';
  return qq{(p-tr "$from" "$to"$mods_str)};
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
  elsif ($str =~ /^qq(.)(.*)\1$/s) {
    # qq/.../  style - content is in $2
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
  elsif ($str =~ /^q(.)(.*)\1$/s) {
    # q/.../ style - content is in $2
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

  # Now escape for CL output: backslashes and quotes
  $content =~ s/\\/\\\\/g;
  $content =~ s/"/\\"/g;

  return qq{"$content"};
}


1;
