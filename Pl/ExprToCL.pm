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
use Pl::CLForm ();
use Pl::InterpScan ();

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

# E2 seam re-housing (docs/v2-endgame-plan.md E2): emitters converted from
# text-producing to CLForm-producing register here, keyed by the same node
# type as `handlers`.  A form handler WINS over the text handler for its
# type, but may DECLINE a shape it does not cover yet by returning undef —
# the text emitter then runs exactly as before.  Convention: a form handler
# must decline BEFORE causing any side effect (gensym counters, _emit,
# environment mutation), because the text path re-runs the node from
# scratch.  A type whose text handler has been deleted must never decline.
has form_handlers => (
  is      => 'ro',
  lazy    => 1,
  builder => '_build_form_handlers',
);

sub _build_handlers {
  my $self = shift;
  return {
    'funcall'       => \&gen_funcall,
    'methodcall'    => \&gen_methodcall,
    'ref_funcall'   => \&gen_ref_funcall,
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
    'glob_slot'        => \&gen_glob_slot,
  };
}

sub _build_form_handlers {
  my $self = shift;
  return {
    'funcall'          => \&gen_funcall_form,
    'methodcall'       => \&gen_methodcall_form,
    'ref_funcall'      => \&gen_ref_funcall_form,
    'anon_sub'         => \&gen_anon_sub_form,
    'prefix_op'        => \&gen_prefix_op_form,
    'postfix_op'       => \&gen_postfix_op_form,
    'tree_val'         => \&gen_tree_val_form,
    'ternary'          => \&gen_ternary,
    'string_concat'    => \&gen_string_concat,
    'array_str_interp' => \&gen_array_str_interp,
    'func_ref'         => \&gen_func_ref_form,
    'arr_init'         => \&gen_array_init_form,
    'hash_init'        => \&gen_hash_init_form,
    'a_acc'            => \&gen_array_access_form,
    'h_acc'            => \&gen_hash_access_form,
    'a_ref_acc'        => \&gen_array_ref_access_form,
    'h_ref_acc'        => \&gen_hash_ref_access_form,
    'slice_a_acc'      => \&gen_array_slice_form,
    'slice_h_acc'      => \&gen_hash_slice_form,
    'kv_slice_h_acc'   => \&gen_kv_hash_slice_form,
    'kv_slice_a_acc'   => \&gen_kv_array_slice_form,
    'progn'            => \&gen_progn_form,
    'inline_lambda'    => \&gen_inline_lambda_form,
    'glob'             => \&gen_glob_form,
    'backtick'         => \&gen_backtick_form,
    'readline'         => \&gen_readline_form,
    'filehandle'       => \&gen_filehandle_form,
    'glob_slot'        => \&gen_glob_slot_form,
  };
}

# Runtime built-in names (from pcl-runtime.lisp export list, p- prefix stripped).
# These get the p- prefix in generated code; user-defined subs get p-.
my %RUNTIME_NAMES = map { $_ => 1 } qw(
  ! != !~ $ % %= && * ** **= *= + - -d -e -f -r -s -w -x -z . .. ... .= / // //= /= < << <<=
  -l -p -S -b -c -t -u -g -k -o -O -R -W -X -T -B -M -A -C
  <= <=> == =~ > >= >> >>= abs and and-assign aref aref-box aref-deref array-= array-init
  array-last-index aslice atan2 backslash backtick binmode bit-and bit-and= bit-not bit-or
  bit-or= bit-xor bit-xor= bless box box-p box-value break caller can cast-$ cast-% cast-@
  chain-cmp chdir chmod chomp chop chr close closedir coderef-defined-p coderef-exists-p
  continue cos crypt cwd decf declare-sub defined defpackage delete delete-array delete-array-slice
  delete-hash-slice delete-kv-hash-slice die do each ensure-arrayref ensure-hashref env-get
  env-set eof eval eval-block eval-direct exception exception-object exists exists-array exit
  exp fc fileno flatten flatten-args for foreach fork wait waitpid getppid kill exec
  getpgrp setpgrp getpriority
  funcall-ref get-class get-coderef getc getcwd
  getgrent getgrgid getgrnam endgrent setgrent
  getpwent getpwuid getpwnam endpwent setpwent getlogin
  gethash gethash-box gethash-deref glob glob-assign glob-copy glob-slot glob-undef-name gmtime
  grep hash hash-= hex hslice if incf index int isa join keys kv-aslice kv-hslice last last-dynamic lc
  link symlink readlink chown utime lock
  lcfirst length let list-= list-x local-glob localtime log lstat make-typeglob map method-call
  alarm mkdir my my-= next not oct open opendir or or-assign ord our pack pipe pop pos post++ post-- pre++
  pre-- print printf prototype push quotemeta rand read readdir readline redo ref reftype regex
  rename require require-file reset resolve-invocant return reverse rewinddir rindex rmdir say
  evalbytes scalar scalar-= seek select set-array-length set_up_inc setf shift sin sleep sort splice split
  sprintf sqrt srand stat str-cmp str-eq str-ge str-gt str-le str-lt str-ne str-x str-x=
  string-concat study sub sub-defined sub-exists subst substr super-call sysread sysseek system syswrite
  socket socketpair bind connect listen accept send recv shutdown getsockname getpeername
  getprotobyname getprotobynumber setsockopt getsockopt
  tell tie tie-proxy tie-proxy-p tie-proxy-saved-value tie-proxy-tie-obj tied time times tr
  truncate typeglob typeglob-name typeglob-p typeglob-package uc ucfirst umask undef undef-sub unless
  unlink unpack unshift untie until use values vec version-string wantarray warn weaken isweak
  while write xor ||
  overloaded overload-strval
  __pcl_set_prototype
);

# Wantarray-sensitive built-ins: their RETURN VALUE depends on the caller's
# list-vs-scalar context, which they read at runtime from the *wantarray*
# dynamic var (e.g. `(if (eq *wantarray* t) <list> <scalar>)` in pcl-runtime).
# Because *wantarray* is the ENCLOSING SUB's context, a call to one of these in
# a statically-known context (the RHS of `my $x = …`, a boolean position, …)
# must bind *wantarray* to that static context or the sub's context leaks in
# (the recurring "wantarray leak" bug: `my $e = each %h` inside a list-context
# sub wrongly returns the (k,v) pair).  gen_funcall wraps every call to one of
# these in `(let ((*wantarray* t/nil)) …)` per the node's annotated context.
#
# INVARIANT: this set must list EVERY runtime builtin that branches on
# *wantarray*.  When you add such a builtin to pcl-runtime.lisp, add it here.
# (readline `<FH>` and the file-glob `<pat>` are separate PPI node types, not
# funcalls — they apply the same wrapper in gen_readline / gen_glob.)
my %WANTARRAY_SENSITIVE = map { $_ => 1 } qw(
  reverse localtime gmtime caller unpack each splice readdir
  getprotobyname getprotobynumber
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
# Values are CLForms: atoms (plain strings) or single-level array forms for
# the compound entries — both leaf paths render via gen_symbol_form.
my %SPECIAL_VARS = (
  '$!'  => ['p-errno-string'],
  '$?'  => '$?',
  '$.'  => '|$.|',
  '$0'  => '$0',
  '$@'  => '$@',
  '$^O' => '|$^O|',
  '$^V' => '|$^V|',
  '$^X' => '|$^X|',
  '$^T' => '|$^T|',   # BASETIME - program start time (Unix seconds; used by -M/-A/-C)
  '$^S' => '|$^S|',   # Eval state: 0 runtime, 1 inside eval (bound by p-eval/-block)
  '$^I' => '|$^I|',   # INPLACE_EDIT - in-place edit extension (<> / perl -i)
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
  # Process credentials — MUST be pipe-quoted: a bare $( / $) unbalances
  # the emitted form (op/groups.t read-error family, s310)
  '$<'  => '|$<|',
  '$>'  => '|$>|',
  '$('  => '|$(|',
  '$)'  => '|$)|',
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
  '${^WARNING_BITS}' => ['p-undef'],   # warning bits bitmask (Perl internal)
  '${^LAST_FH}'      => ['p-undef'],   # last filehandle used (Perl internal)
  # Lexical hints: $^H (hint bits) and %^H (hints hash). PCL does not model
  # compile-time hints, so these are inert always-bound empties — 0 and an empty
  # hash — so `$^H & MASK`, `\%^H` and `keys %^H` never crash with an unbound var.
  '$^H' => '|$^H|',
  '%^H' => '|%^H|',
  # Remaining caret vars with runtime defvars.  These MUST be pipe-quoted:
  # generated code loads under :invert, so a bare `$^P` token (all-upper) reads
  # as the symbol `$^p` — never the runtime's |$^P| — and aborts unbound.
  '$^P' => '|$^P|',   # PERLDB - debugger flag
  '$^D' => '|$^D|',   # DEBUGGING flags
  '$^F' => '|$^F|',   # SYSTEM_FD_MAX
  '$^M' => '|$^M|',   # emergency memory pool
  '$^R' => '|$^R|',   # last (?{...}) result
  '$^N' => '|$^N|',   # most-recently-closed participating capture group
  '$^W' => '|$^W|',   # global warnings flag (inert 0)
  '$['  => '|$[|',    # array base (always 0 since 5.30; inert)
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
  my $force_user  = shift // 0;  # 1 = &NAME(...) form: call the user sub, never a builtin

  # Guard against undefined input
  return 'p-UNDEFINED' unless defined $perl_name && length($perl_name);

  # Check for operator exceptions — but NOT when generating a function call name.
  # e.g. `x()` calls user sub x, not the string-repetition operator p-str-x.
  return $OP_EXCEPTIONS{$perl_name} if !$for_funcall && !$force_user && exists $OP_EXCEPTIONS{$perl_name};

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
    # Pipe-quote a package with ANY colon (Foo::Bar, but also the trailing-:
    # residue of odd names like a:::b) — a bare colon misreads as a package
    # marker and kills the whole file at load.
    my $cl_pkg = $pkg =~ /:/ ? "|$pkg|" : $pkg;
    return "${cl_pkg}::pl-${func}";
  }

  # Runtime built-in → p-prefix; user-defined sub → pl-prefix.
  # The `&NAME(...)` call form forces the user sub even when NAME is a builtin
  # (Perl's `&` sigil bypasses builtins/prototypes — e.g. a user `sub connect`
  # imported into main:: called as `&connect()`).
  if (!$force_user && exists $RUNTIME_NAMES{$perl_name}) {
    return "p-$perl_name";
  }
  # Inside a non-main package, qualify user-defined sub calls so SBCL's reader
  # resolves them in the right package (not MAIN, which is the load-time package).
  my $cur_pkg = ($self->environment && $self->environment->can('current_package'))
                  ? $self->environment->current_package()
                  : 'main';
  # Reader-safety net: a residual colon in the NAME position (e.g. the Word
  # `온ꪵ::` from a unicode stash access PPI can't tokenize as one Symbol)
  # would misread as a package marker — pipe-quote the whole symbol so a bad
  # shape fails as ONE undefined-function, never a whole-file read error.
  my $fn = $perl_name =~ /:/ ? "|pl-$perl_name|" : "pl-$perl_name";
  if ($cur_pkg && $cur_pkg ne 'main') {
    my $cl_pkg = $cur_pkg =~ /:/ ? "|$cur_pkg|" : $cur_pkg;
    return "${cl_pkg}::${fn}";
  }
  return $fn;
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


# Helper: if $arg_id is a prefix_op node whose operator is the `&` Cast
# (`&$var` / `&{expr}`), return the node-id of the coderef expression inside;
# undef otherwise.  Perl calls `&$f` (no parens) with the current @_, so the
# `&` prefix lowers to a CALL by default; the closed set of parents that
# instead want the coderef itself — \, defined, exists, undef, goto — use
# this to reach past the call and lower the mention themselves.
sub _amp_cast_operand_id {
  my ($self, $arg_id) = @_;
  my $arg_node = $self->expr_o->get_a_node($arg_id);
  return undef unless $self->expr_o->is_internal_node_type($arg_node)
    && ($arg_node->{type} // '') eq 'prefix_op';
  my $k = $self->expr_o->get_node_children($arg_id);
  return undef unless @$k >= 2;
  my $op_node = $self->expr_o->get_a_node($k->[0]);
  return undef unless ref($op_node) eq 'PPI::Token::Cast'
    && $op_node->content() eq '&';
  return $k->[1];
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

  # E2: a converted (form-producing) emitter wins when one exists for the
  # type.  Flat-printing the form here keeps every unconverted text caller
  # byte-identical to the pre-conversion output (to_flat embeds raw child
  # text verbatim), so byte-parity per conversion step is checkable with
  # tools/corpus-diff.pl.  undef = the handler declined this shape.
  if (my $fh = $self->form_handlers->{$type}) {
    my $form = $fh->($self, $node, $node_id, $kids);
    return Pl::CLForm::to_flat($form) if defined $form;
  }

  # E2: a type with no named handler is a binary operator (gen_binary_op) —
  # try the form emitter (declines =/=~/!~, converts the rest).
  if (!exists $self->handlers->{$type}) {
    my $form = $self->gen_binary_op_form($type, $kids, $node_id);
    return Pl::CLForm::to_flat($form) if defined $form;
  }

  return $self->gen_internal_node_text($node, $node_id, $kids);
}

# The text-emitter dispatch (pre-E2 shape).  Called directly by
# gen_node_form when a form handler declines, so a declining handler is
# never consulted twice for the same node.
sub gen_internal_node_text {
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

# E2: form-of-node, for CONVERTED emitters generating their children.
# Returns the child's CLForm when its emitter is converted (and does not
# decline), otherwise the child's v1 text embedded as an opaque raw atom —
# so a converted parent over unconverted children still reproduces v1's
# bytes exactly under to_flat, and under the real printer once the seam
# root goes structural (E2.final).
sub gen_node_form {
  my $self    = shift;
  my $node_id = shift;

  my $node = $self->expr_o->get_a_node($node_id);
  if ($self->expr_o->is_internal_node_type($node)) {
    my $type = $node->{type};
    my $kids = $self->expr_o->get_node_children($node_id);
    if (my $fh = $self->form_handlers->{$type}) {
      my $form = $fh->($self, $node, $node_id, $kids);
      return $form if defined $form;
    }
    # Internal-node-typed binary op (type not a named handler) → form emitter.
    if (!exists $self->handlers->{$type}) {
      my $form = $self->gen_binary_op_form($type, $kids, $node_id);
      return $form if defined $form;
    }
    warn "pcl-raw\tnode:$type\n" if $ENV{PCL_E2_RAW_CENSUS};
    return Pl::CLForm::raw($self->gen_internal_node_text($node, $node_id, $kids));
  }
  # A PPI::Token::Operator/Word WITH children is a binary op (gen_node's
  # gen_binary_op case), NOT a leaf → the binary-op form emitter (declines
  # =/=~/!~ before any side effect, so the raw(gen_node) fallback re-runs
  # gen_binary_op cleanly).  A genuine leaf (no children) goes to
  # gen_leaf_form: a converted leaf returns a CLForm; anything not yet
  # converted declines (undef) and the leaf's v1 text is embedded as a raw
  # atom.  gen_leaf's leaf side effects (idempotent package/caret set-adds,
  # read-only rename lookups) make the decline→re-run through gen_node safe.
  my $kids = $self->expr_o->get_node_children($node_id) || [];
  if (@$kids
      && (ref($node) eq 'PPI::Token::Operator' || ref($node) eq 'PPI::Token::Word')) {
    my $form = $self->gen_binary_op_form($node->content(), $kids, $node_id);
    return $form if defined $form;
  }
  if (!@$kids && defined(my $lf = $self->gen_leaf_form($node))) {
    return $lf;
  }
  if ($ENV{PCL_E2_RAW_CENSUS}) {
    my $desc = ref($node) ? ref($node) . ':' . (eval { $node->content } // '?')
                          : "scratch:$node";
    $desc = substr($desc, 0, 60);
    warn "pcl-raw\t$desc\n";
  }
  return Pl::CLForm::raw($self->gen_node($node_id));
}

# E2 leaf conversion (docs/v2-endgame-plan.md E2.1, literal/sym frontier):
# CLForm for a converted leaf token, or undef to decline (→ raw v1 text).
# Converted: Symbol/Magic genuine atoms, string/heredoc/word/operator/cast
# atoms, the Number family, $#arr (ArrayIndex), and m// / qr// regex leaves
# (both non-interpolated → (p-regex …)/(pcl::p-qr …) and interpolated →
# (pcl::p-regex-from-parts …)), and s/// / tr/// (gen_substitution_form /
# gen_transliteration_form; the /e-lambda body stays a raw atom until the
# inline_lambda step).  Declines (→ text path): Symbol/Magic compounds
# (stash/typeglob/&sub/errno).  Byte-for-byte gen_leaf's shapes.
sub gen_leaf_form {
  my ($self, $node) = @_;
  my $ref = ref($node);

  # Symbol / Magic — the frontier's heaviest leaves ($x/@a/%h, magic vars,
  # package-qualified, renamed).  gen_symbol_form returns atoms for genuine
  # variables and single-level array forms for the compound cases (stash /
  # typeglob / &sub-with-callers-args / errno).  Never declines.
  if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
    return $self->gen_symbol_form($node);
  }

  # String literals (Quote::Single/Double/Literal/Interpolate — NOT
  # QuoteLike:: qr//, whose gen_leaf has non-idempotent regex side effects):
  # the form entry, so a literal with surrogate/non-char codepoints comes
  # back as the structural (concatenate 'string … (string (code-char N)) …)
  # instead of raw text (task #78 residue).  Mirrors gen_leaf's Quote branch
  # exactly (content → convert_perl_string).  An interpolated string is a
  # string_concat NODE (already converted), never one of these leaves.
  if ($ref =~ /^PPI::Token::Quote::/) {
    my $f = $self->convert_perl_string_form($node->content());
    # Unknown-format fallthrough returns the token text as-is; a paren-shaped
    # ATOM there is not a form — decline to the text path (belt-and-braces).
    return undef if !ref $f && $f =~ /^\(/;
    return $f;
  }
  # Pure atom leaves: heredocs, barewords, and operator tokens.  gen_leaf for
  # these is pure and its output is always an atom (a "…" literal, a
  # bareword, a number, an operator string), so it never starts with "(" —
  # the guard below is belt-and-braces only, and there is no double-run risk
  # because these never decline.
  if ($ref eq 'PPI::Token::HereDoc'
      || $ref eq 'PPI::Token::Word'
      || $ref eq 'PPI::Token::Operator'
      || $ref eq 'PPI::Token::Cast') {   # deref sigil (@/%/$/\/&/*): bare content atom
    my $s = $self->gen_leaf($node);
    return undef if $s =~ /^\(/;
    return $s;
  }

  # $#arr / $#Pkg::arr → (p-array-last-index @arr).  Single-level form; the
  # only gen_leaf side effect is an idempotent state-var-rename lookup, so
  # re-deriving the transformed container atom here is safe.
  if ($ref eq 'PPI::Token::ArrayIndex') {
    my $content = $node->content();
    $content =~ s/^\$#(.*)::(.+)$/$1\::\@$2/  # $#A::ISA → A::@ISA
        || $content =~ s/^\$#/\@/;            # $#arr    → @arr
    if ($self->environment) {
      my $renames = $self->environment->state_var_renames;
      $content = $renames->{$content} if $renames && exists $renames->{$content};
    }
    return ['p-array-last-index', $content];
  }

  if ($ref =~ /^PPI::Token::Number/) {
    my $num = $node->content();

    # Version strings: v1.20.300 / 256.65.258 → (p-version-string N N N).
    if ($ref =~ /::Version$/ || $num =~ /^v(\d[\d._]*)$/) {
      my $vpart = $num;
      $vpart =~ s/^v//;
      $vpart =~ s/_//g;   # v1.2_3 == v1.23: underscores are digit separators
      return ['p-version-string', split /\./, $vpart];
    }
    # Radix literals → CL #x / #b / #o (optional leading - → (- …)).
    if ($num =~ /^(-?)0[xX]([0-9a-fA-F_]+)$/) {
      my ($sign, $d) = ($1, $2); $d =~ s/_//g;
      return $sign ? ['-', "#x$d"] : "#x$d";
    }
    if ($num =~ /^(-?)0[bB]([01_]+)$/) {
      my ($sign, $d) = ($1, $2); $d =~ s/_//g;
      return $sign ? ['-', "#b$d"] : "#b$d";
    }
    if ($num =~ /^(-?)0[oO]([0-7_]+)$/) {
      my ($sign, $d) = ($1, $2); $d =~ s/_//g;
      return $sign ? ['-', "#o$d"] : "#o$d";
    }
    if ($num =~ /^(-?)0([0-7_]+)$/ && $num ne '0') {
      my ($sign, $d) = ($1, $2); $d =~ s/_//g;
      return $sign ? ['-', "#o$d"] : "#o$d";
    }
    $num =~ s/_//g;
    if ($num =~ /[eE.]/) {
      my $val = eval($num);
      if (defined $val) {
        return ['p-double-inf']      if $val == 9**9**9;
        return ['p-double-inf', 't'] if $val == -(9**9**9);
      }
    }
    return $num;
  }

  # Match m// and qr// regex leaves convert, both non-interpolated and
  # interpolated.  Non-interp → a single-level (p-regex "…") / (pcl::p-qr "…")
  # (content re-escaped).  Interp → (pcl::p-regex-from-parts PAT "flags") where
  # PAT is _gen_interp_regex_pattern's CLForm ("…"/$var/(p-aref …)/(p-gethash …)
  # / (p-string-concat …)).  _parse_regex_content and _has_regex_interpolation
  # are pure; _gen_interp_regex_pattern lowers each reference through a
  # sub-compile whose only side effects are the environment's idempotent
  # set-adds (referenced packages, caret globals) — and this branch never
  # declines, so it runs exactly once per node either way.  s/// and tr/// via
  # gen_substitution_form / gen_transliteration_form (never decline, so their
  # side effects — the /e sub-compile — run exactly once).
  if ($ref eq 'PPI::Token::QuoteLike::Regexp') {
    my $content = $node->content();
    my ($pattern, $flags) = _parse_regex_content($content, 1);
    if (_has_regex_interpolation($pattern)) {
      my $pat_form = $self->_gen_interp_regex_pattern($pattern);
      (my $esc_flags = $flags) =~ s/"/\\"/g;
      return ['pcl::p-regex-from-parts', $pat_form, qq{"$esc_flags"}];
    }
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return ['pcl::p-qr', qq{"$content"}];
  }
  return $self->gen_substitution_form($node)
    if $ref eq 'PPI::Token::Regexp::Substitute';
  return $self->gen_transliteration_form($node)
    if $ref eq 'PPI::Token::Regexp::Transliterate';
  if ($ref =~ /^PPI::Token::Regexp/) {
    my $content = $node->content();
    my ($pattern, $flags) = _parse_regex_content($content, 0);
    if (_has_regex_interpolation($pattern)) {
      my $pat_form = $self->_gen_interp_regex_pattern($pattern);
      (my $esc_flags = $flags) =~ s/"/\\"/g;
      return ['pcl::p-regex-from-parts', $pat_form, qq{"$esc_flags"}];
    }
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return ['p-regex', qq{"$content"}];
  }

  return undef;   # decline: not a converted leaf type (never fires on the
                  # corpus — safety net, verified s304 census)
}


# Generate code for leaf nodes (literals, variables)
# Parse a regex token content into (pattern_content, flags).
# Works for /pat/flags, m/pat/flags, m{pat}flags, qr/pat/flags, qr{pat}flags, etc.
# $is_qr: 1 if token starts with 'qr', 0 otherwise.
sub _parse_regex_content {
  my ($content, $is_qr) = @_;
  my $prefix_len = $is_qr ? 2 : 0;
  $prefix_len++ if !$is_qr && $content =~ /^m/;
  # Perl allows whitespace between the operator and its delimiter: `qr //`
  $prefix_len++ while $prefix_len < length($content)
    && substr($content, $prefix_len, 1) =~ /\s/;
  my $open_ch = substr($content, $prefix_len, 1);
  my %pairs = ('{' => '}', '(' => ')', '[' => ']', '<' => '>');
  my $close_ch = $pairs{$open_ch} // $open_ch;
  my $end_pos = rindex($content, $close_ch);
  my $pattern = substr($content, $prefix_len + 1, $end_pos - $prefix_len - 1);
  my $flags = substr($content, $end_pos + 1);
  return ($pattern, $flags);
}

# Does a raw regex pattern interpolate?  The scanner answers — the same one
# that then places the references (task #237), so the gate and the consumer
# cannot disagree.  The private predicate this replaced looked only for
# `$name`/`@name`/`${`, so `/$1/`, `/$#a/`, `/$$/` and the punctuation magics
# stayed LITERAL text where perl interpolates them (probed s382f: `"b" =~
# /^$1$/` matches in perl, missed here).  The `[\$\@]` pre-filter keeps the
# no-sigil majority (most patterns) off the scanner entirely.
sub _has_regex_interpolation {
  my ($pattern) = @_;
  return 0 if $pattern !~ /[\$\@]/;
  return scalar @{ Pl::InterpScan::scan($pattern, in_regex => 1) };
}

# Does an s/// REPLACEMENT need the interpolating (lambda) path?  This is the
# pre-scanner predicate, kept deliberately: see gen_substitution_form.
sub _replacement_interpolates {
  my ($subst) = @_;
  return $subst =~ /(?<!\\)\$[a-zA-Z_{]|(?<!\\)\@[a-zA-Z_{]/;
}

# Build a CLForm that evaluates to the interpolated pattern string: a "…"
# literal, one reference part, or (p-string-concat …) over a mix.  Text callers
# wrap the result in Pl::CLForm::to_flat; the E2 leaf form path (gen_leaf_form)
# embeds it structurally.
#
# A pattern interpolates like a double-quoted string, but with perl's PATTERN
# start rules ($ before ")|" or whitespace is the tail anchor; @+/@- never
# interpolate) and with S_intuit_more deciding whether a `[…]`/`{…}` group
# after a variable is a SUBSCRIPT or regex syntax (a charclass / a {n,m}
# quantifier).  Both live in Pl::InterpScan — the one scanner every
# interpolation consumer shares (task #237; standing rule
# docs/var-handling-review-s379.md §8: interpolation scanning happens there,
# never in a private walk).  This consumer only places the events: the text
# BETWEEN them is copied verbatim, because regex escapes (\d, \b, \Q…) must
# reach cl-ppcre unprocessed — the one way pattern text differs from dq text.
sub _gen_interp_regex_pattern {
  my ($self, $pattern) = @_;
  my $events = Pl::InterpScan::scan($pattern, in_regex => 1);
  my @parts;
  my $literal = '';
  my $flush = sub {
    return if !length $literal;
    (my $esc = $literal) =~ s/\\/\\\\/g;
    $esc =~ s/"/\\"/g;
    push @parts, qq{"$esc"};
    $literal = '';
  };
  my $pos = 0;
  for my $ev (@$events) {
    my ($s, $e) = @{ $ev->{span} };
    $literal .= substr($pattern, $pos, $s - $pos);
    $pos = $e;
    $flush->();
    push @parts, $self->_interp_ref_form(substr($pattern, $s, $e - $s), $ev);
  }
  $literal .= substr($pattern, $pos);
  $flush->();
  return @parts == 0 ? '""'
       : @parts == 1 ? $parts[0]
       : ['p-string-concat', @parts];
}

# One scanner event → the CLForm producing its interpolated text.  A plain
# unqualified scalar is its own atom — the overwhelmingly common case, and
# byte-identical to what the private walk emitted.  Everything else (direct
# subscripts $a[i]/$h{k}, chains, package-qualified names, $#a, $$r, arrays,
# slices, ${EXPR}) is lowered by compiling the reference's SOURCE TEXT as
# ordinary code, so the one expression pipeline answers what each shape means
# instead of a second lowering table drifting beside it (rule 11).  An
# '@'-sigil reference yields a list and joins with $", exactly as
# gen_string_concat does for the same reference in dq text.
sub _interp_ref_form {
  my ($self, $src, $ev) = @_;
  my $list = $ev->{sigil} eq '@';
  if ($ev->{sigil} eq '$' && !@{ $ev->{chain} } && !$ev->{postderef}
      && ($ev->{form} eq 'plain' || $ev->{form} eq 'braced')
      && defined $ev->{name} && $ev->{name} =~ /^[A-Za-z_]\w*\z/
      && !exists $SPECIAL_VARS{ '$' . $ev->{name} }) {
    return "\$$ev->{name}";
  }
  my $form = $self->_compile_ref_text_form($src, $list);
  # rule 12: a reference this consumer cannot lower would silently land in
  # the pattern as literal text — a wrong VALUE the match then consumes.
  die "PCL: cannot compile interpolated regex reference '$src'\n"
    if !defined $form;
  return $list ? ['p-join', '|$"|', $form] : $form;
}

# Compile one fragment of Perl source (an interpolated reference) to a CLForm
# through the ordinary expression pipeline — the same move
# _gen_interp_replacement and StringInterpolation::_parse_postfix_deref make
# when a reference is easier to re-parse than to re-implement.  $list forces
# LIST context first (a slice in scalar context would reduce to its last
# element instead of joining — gen_string_concat's rule).  Returns undef when
# PPI/PExpr cannot read the fragment.
sub _compile_ref_text_form {
  my ($self, $src, $list) = @_;
  my $form = eval {
    require PPI::Document;
    require Pl::PExpr;
    my $doc = PPI::Document->new(\$src);
    return undef if !$doc;
    my @stmts = grep { !$_->isa('PPI::Token::Whitespace') } $doc->children;
    return undef if !@stmts || !$stmts[0]->can('children');
    my @parts = map { $_->clone() }
                grep { ref($_) ne 'PPI::Token::Whitespace' } $stmts[0]->children;
    return undef if !@parts;
    my $expr_o = Pl::PExpr->new(
      e        => \@parts,
      full_PPI => $doc,
      ($self->environment ? (environment => $self->environment) : ()),
    );
    my $id = $expr_o->parse_expr_to_tree(\@parts);
    return undef if !defined $id;
    $expr_o->set_node_context($id, LIST_CTX) if $list;
    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => $self->indent_level,
    );
    $gen->gen_node_form($id);
  };
  return undef if $@ || !defined $form || (!ref($form) && $form eq '');
  return $form;
}

# Symbol / Magic leaf → CLForm (E2-converted; shared by gen_leaf and
# gen_leaf_form).  Genuine variables are atoms; the compound cases — stash,
# typeglob, &foo-with-callers-args, and the compound %SPECIAL_VARS entries —
# are single-level array forms.  Side effects (referenced-package /
# caret-global set-adds, rename lookups) are idempotent.  Never declines.
sub gen_symbol_form {
  my $self = shift;
  my $node = shift;

  my $content = $node->content() // '';
  # Normalize Perl 4 package separator: $pkg'var -> $pkg::var
  $content =~ s/^([\$\@\%\*&])([a-zA-Z_]\w*)'/$1$2::/;
  # Handle magic/special variables via dispatch table
  return $SPECIAL_VARS{$content} if exists $SPECIAL_VARS{$content};
  # Handle package-qualified variables: $Pkg::var -> Pkg::$var
  # Perl: $Config::debug  ->  CL: Config::$debug
  # Also: $::foo means $main::foo (empty package = main)
  if ($content =~ /^[\$\@\%].*::[^:]+$/) {
    return qualified_var_to_cl($content, $self->environment);
  }
  # Handle package stash typeglob: *Pkg:: (no variable name) -> (p-stash "Pkg")
  # Perl: undef *Food:: or *Mover:: = *Mover2::
  # PCL: stash ops not fully supported but must be syntactically valid CL
  if ($content =~ /^\*(.*)::$/) {
    my $pkg = $1;
    $pkg = 'main' if $pkg eq '';
    $self->environment->add_referenced_package($pkg) if $self->environment;
    return ['p-stash', "\"$pkg\""];
  }
  # Handle package-qualified typeglobs: *Pkg::foo -> (p-make-typeglob "Pkg" "foo")
  # Also: *::foo means *main::foo (empty package = main)
  if ($content =~ /^\*(.*)::([^:]+)$/) {
    my ($pkg, $name) = ($1, $2);
    $pkg = 'main' if $pkg eq '';
    $self->environment->add_referenced_package($pkg) if $self->environment;
    return ['p-make-typeglob', "\"$pkg\"", "\"$name\""];
  }
  # Handle simple typeglob: *foo -> (p-make-typeglob "current-pkg" "foo")
  if ($content =~ /^\*(\w+)$/) {
    my $name = $1;
    my $pkg  = $self->environment ? $self->environment->current_package : 'main';
    $pkg //= 'main';
    return ['p-make-typeglob', "\"$pkg\"", "\"$name\""];
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
    return ['p-stash', "\"$pkg\""];
  }
  # &foo (no parens) re-uses the CALLER'S @_ — unlike &foo() which passes an
  # empty list, or foo() which is a normal call.  At file top level @_ is the
  # global empty vector, so emitting @_ is always safe.
  # Note: &foo(@args) is handled as a funcall, not here; \&foo is a refgen.
  if ($content =~ /^&(.+)$/) {
    my $func_name = $1;
    # &NAME (no parens) calls the user sub even when NAME is a builtin.
    my $cl_func = $self->cl_name($func_name, 1, 1);
    return [$cl_func, '@_'];
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

sub gen_leaf {
  my $self = shift;
  my $node = shift;

  my $ref  = ref($node);

  # Variable (like $x, @arr, %hash) — shared form logic, flattened for text
  if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
    return Pl::CLForm::to_flat($self->gen_symbol_form($node));
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
      my $pat_expr = Pl::CLForm::to_flat($self->_gen_interp_regex_pattern($pattern));
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
      my $pat_expr = Pl::CLForm::to_flat($self->_gen_interp_regex_pattern($pattern));
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
    # Assigning to a subroutine/code-ref CALL (`&sub = x`, `foo() = x`,
    # `$cref->() = x`) is only valid for an lvalue sub — which PCL does not
    # support — so it is a compile error, exactly as in Perl.  Raising it here
    # (a transpile-time die) is what makes feature-detection probes that rely on
    # the compile error work: Class::Method::Modifiers' _sub_attrs does
    # `eval 'return 1; &_sub = 1'` and treats failure as "not an lvalue sub".
    # In eval-string mode the die surfaces via the pl2cl server's error status,
    # so the eval returns undef and the probe correctly reports ''.  The
    # built-in magic lvalues substr/pos/vec ARE supported, so allow those.
    if (defined $node_id) {
      my $lnode = $self->expr_o->get_a_node($kids->[0]);
      my $bad_lvalue = 0;
      if ($self->expr_o->is_internal_node_type($lnode)
          && ($lnode->{type} eq 'funcall' || $lnode->{type} eq 'ref_funcall')) {
        # foo() = x  /  $cref->() = x
        $bad_lvalue = 1;
        if ($lnode->{type} eq 'funcall') {
          my $fkids = $self->expr_o->get_node_children($kids->[0]);
          if ($fkids && @$fkids) {
            my $fn = $self->expr_o->get_a_node($fkids->[0]);
            my $nm = (ref($fn) && $fn->can('content')) ? $fn->content : '';
            $bad_lvalue = 0 if $nm =~ /^(?:CORE::)?(?:substr|pos|vec)$/;
          }
        }
      } elsif (ref($lnode) && $lnode->can('content')
               && $lnode->content =~ /^&/) {
        # &sub = x  (ampersand call as an lvalue; a leaf Symbol token, not a
        # funcall node — see _emit_token's &NAME branch).
        $bad_lvalue = 1;
      }
      # PCL: prefix makes the parser propagate this as a hard error (Parser.pm
      # ~6722) instead of swallowing it into a ;; PARSE ERROR comment — so in
      # eval-string mode the transpile FAILS and the eval returns undef, exactly
      # like Perl's compile error for assignment to a non-lvalue sub.
      die "PCL: Can't modify non-lvalue subroutine call in assignment\n"
        if $bad_lvalue;
    }
    # Perl: `($c ? $a : $b) = V` is a SCALAR assignment to the chosen side
    # (perlop: the ternary is an lvalue), NOT a one-element list assignment —
    # V gets SCALAR context and the expression's value is the assigned value,
    # so a while-loop's implicit defined() applies to it.  The vector branch
    # below would evaluate V in LIST context (defins.t t10: draining readdir
    # on the first iteration) and return a COUNT (1 even for an undef RHS,
    # so the loop would never terminate).
    if (defined $node_id) {
      my $tern_id = $self->_sole_ternary_lvalue_id($kids->[0]);
      if (defined $tern_id) {
        $self->expr_o->set_node_context($kids->[1], SCALAR_CTX);
        my $tern = $self->gen_node($tern_id);
        my $rhs  = $self->gen_node($kids->[1]);
        return "(box-set $tern $rhs)";
      }
    }
    if ($left =~ /^\(vector[ )]/) {
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
    } elsif ($left =~ /^\(p-(?:gethash|aref|aslice|hslice) /) {
      # Single-element store: $h{k} = ... / $a[i] = ...  (via p-setf).  This
      # MUST precede the sigil regexes below: a package-qualified element form
      # like (p-gethash |Foo::Bar|::%H "x") contains "::%" (or "::@" for arrays)
      # which would otherwise be mis-detected as a whole %hash/@array LHS and
      # routed to p-hash-= / p-array-= (which expect a bare symbol place and
      # crash on the gethash/aref form).  Same trap for slices with a
      # package-qualified head: (p-aslice tmp::@a ...) = ... (array.t #8910
      # block after the our-alias requalify pre-pass).
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

# E2 form variant of gen_binary_op.  Converts every binary op structurally,
# mirroring gen_binary_op branch-for-branch (same generation ORDER, so the
# shared $g_flipflop_count and any gensyms match): arithmetic / comparison /
# logical / string / `.` / `x` / `..` flip-flop+range / `isa` / use-integer,
# `=~`/`!~` (whose s///-vs-match wantarray wrap is decided AST-level — RHS is a
# Regexp::Substitute/Transliterate node — instead of grepping the generated
# $right), and `=` assignment.  The `=` LHS-sigil / magic-lvalue / typeglob
# dispatch inspects the FLAT text of the (already-generated) left form (== v1's
# $left bytes by the to_flat contract), so the dispatch decisions match
# gen_binary_op exactly while the OUTPUT is a form — no raw child re-run, no
# double generation.
sub gen_binary_op_form {
  my ($self, $op, $kids, $node_id) = @_;

  my $cl_op = $self->cl_op_name($op);

  # '..'/'...' — range in list context, flip-flop in scalar context.
  if (($op eq '..' || $op eq '...') && defined $node_id) {
    my $ctx = $self->expr_o->get_node_context($node_id);
    my $left_node  = $self->expr_o->get_a_node($kids->[0]);
    my $right_node = $self->expr_o->get_a_node($kids->[1]);
    my $both_int  = _is_integer_literal_node($left_node) && _is_integer_literal_node($right_node);
    my $both_literal = ($both_int
                        || (_is_string_literal_node($left_node) && _is_string_literal_node($right_node)));
    my $effective_ctx = $ctx;
    $effective_ctx = SCALAR_CTX if $ctx == INHERIT_CTX && !$both_literal;
    if ($effective_ctx != LIST_CTX && $effective_ctx != INHERIT_CTX) {
      my $ff_id = $g_flipflop_count++;
      my $left  = $self->gen_node_form($kids->[0]);
      my $right = $self->gen_node_form($kids->[1]);
      my $macro = $both_int
                ? (($op eq '...') ? 'p-flipflop-num-3' : 'p-flipflop-num')
                : $both_literal
                ? (($op eq '...') ? 'p-flipflop-dyn-3' : 'p-flipflop-dyn')
                : (($op eq '...') ? 'p-flipflop-3' : 'p-flipflop');
      return [$macro, $ff_id, $left, $right];
    }
    if ($effective_ctx == INHERIT_CTX) {
      my $ff_id = $g_flipflop_count++;
      my $left  = $self->gen_node_form($kids->[0]);
      my $right = $self->gen_node_form($kids->[1]);
      my $ff_macro = $both_int
                   ? (($op eq '...') ? 'p-flipflop-num-3' : 'p-flipflop-num')
                   : (($op eq '...') ? 'p-flipflop-dyn-3' : 'p-flipflop-dyn');
      my $range_fn = ($op eq '...') ? 'p-...' : 'p-..';
      return ['if', ['eq', '*wantarray*', 't'],
                    [$range_fn, $left, $right],
                    [$ff_macro, $ff_id, $left, $right]];
    }
    # List context: range endpoints are always scalars, not lists.  Fall through.
    $self->expr_o->set_node_context($kids->[0], SCALAR_CTX);
    $self->expr_o->set_node_context($kids->[1], SCALAR_CTX);
  }

  # 'x' — list repeat when LHS is parenthesized and in list context.
  if ($op eq 'x' && defined $node_id) {
    my $lhs_node = $self->expr_o->get_a_node($kids->[0]);
    my $lhs_is_paren = $self->expr_o->is_internal_node_type($lhs_node) &&
                       ($lhs_node->{type} eq 'tree_val' || $lhs_node->{type} eq 'progn');
    my $ctx = $self->expr_o->get_node_context($node_id);
    if ($lhs_is_paren && $ctx == LIST_CTX) {
      $self->expr_o->set_node_context($kids->[0], LIST_CTX);
      my $left  = $self->gen_node_form($kids->[0]);
      my $right = $self->gen_node_form($kids->[1]);
      return ['p-list-x', $left, $right];
    }
    if ($lhs_is_paren && $ctx == INHERIT_CTX) {
      $self->expr_o->set_node_context($kids->[0], LIST_CTX);
      my $left_list   = $self->gen_node_form($kids->[0]);
      $self->expr_o->set_node_context($kids->[0], SCALAR_CTX);
      my $left_scalar = $self->gen_node_form($kids->[0]);
      my $right = $self->gen_node_form($kids->[1]);
      return ['if', ['eq', '*wantarray*', 't'],
                    ['p-list-x', $left_list, $right],
                    ['p-str-x', $left_scalar, $right]];
    }
  }

  # `$a[0] =~ s///` and `$h{k} =~ tr///` WRITE their target, so the target has
  # to be the element's BOX — the same lvalue_context the mutating builtins
  # already use, gated the same way (`_is_elem_arg`: only when the LHS itself
  # IS an element access, so an index subexpression is untouched).  Without it
  # the substitution ran against a COPY and did NOTHING: silent for an array
  # element, a "Cannot modify non-boxed value" warning for a hash element.
  # Found while building #189 — it is also what kept lib/File/Basename.pm on
  # a shim, since core's _strip_trailing_sep is `$_[0] =~ s{…}{}`.
  my $left = do {
    my $saved_lv = $self->lvalue_context;
    $self->lvalue_context(1)
      if ($op eq '=~' || $op eq '!~')
      && $self->_is_elem_arg($kids->[0])
      && _rhs_writes_match_target($self, $kids->[1]);
    my $l = $self->gen_node_form($kids->[0]);
    $self->lvalue_context($saved_lv);
    $l;
  };
  # Flat text of the left form == v1's $left bytes (to_flat contract); used
  # only to make the `=` dispatch decisions below, mirroring gen_binary_op.
  my $left_flat = ($op eq '=') ? Pl::CLForm::to_flat($left) : undef;

  # '=' hash assignment with list — %h = () / %h = (k=>v,…): pass a flat vector
  # so p-hash-= can count input elements for its scalar-context return.  Runs
  # BEFORE right-gen (like gen_binary_op), so the RHS kids are generated here
  # and $right is never produced for this shape (counter order preserved).
  if ($op eq '=' && $left_flat =~ /^%/) {
    my $rhs_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($rhs_node)
        && ($rhs_node->{type} eq 'tree_val' || $rhs_node->{type} eq 'progn')) {
      my $rhs_kids = $self->expr_o->get_node_children($kids->[1]);
      my $ctx = defined $node_id ? $self->expr_o->get_node_context($node_id) : 0;
      my $result = (@$rhs_kids == 0)
        ? ['p-hash-=', $left,
           ['make-array', '0', ':adjustable', 't', ':fill-pointer', '0']]
        : ['p-hash-=', $left,
           ['vector', map { $self->gen_node_form($_) } @$rhs_kids]];
      return $ctx == LIST_CTX
               ? ['let', ['list', ['list', '*wantarray*', 't']],   $result]
           : $ctx == SCALAR_CTX
               ? ['let', ['list', ['list', '*wantarray*', 'nil']], $result]
           : $result;
    }
  }

  # 'isa' — RHS bareword class name must be a string literal.
  if ($op eq 'isa') {
    my $rhs_node = $self->expr_o->get_a_node($kids->[1]);
    my $right;
    if (ref($rhs_node) eq 'PPI::Token::Word' && !$self->expr_o->get_node_children($kids->[1])) {
      my $class_name = $rhs_node->content();
      $right = qq{"$class_name"};
    } else {
      $right = $self->gen_node_form($kids->[1]);
    }
    return ['p-isa', $left, $right];
  }

  my $right = $self->gen_node_form($kids->[1]);

  # '=' assignment: dispatch on LHS shape (mirrors gen_binary_op, same order).
  if ($op eq '=') {
    # keys(%h) = N — hash pre-sizing, a no-op in CL: just the RHS value.
    return $right if $left_flat =~ /^\(p-keys /;
    # $#arr = N  →  (p-set-array-length @arr N).  The container comes out of
    # the LHS FORM when there is one (task #78: no raw wrap around an atom);
    # the text capture stays only for a raw LHS (declined subtree).
    if (ref $left eq 'ARRAY' && @$left == 2 && !ref $left->[0]
        && $left->[0] eq 'p-array-last-index') {
      return ['p-set-array-length', $left->[1], $right];
    }
    if ($left_flat =~ /^\(p-array-last-index (.+)\)$/) {
      return ['p-set-array-length', Pl::CLForm::raw($1), $right];
    }
    # *foo = RHS  →  (p-glob-assign "pkg" "name" rhs)
    if ($left_flat =~ /^\(p-make-typeglob "([^"]+)" "([^"]+)"\)$/) {
      return ['p-glob-assign', qq{"$1"}, qq{"$2"}, $right];
    }
    # *$var = RHS  →  (p-glob-assign-dynamic name-expr rhs); same form-first
    # rule as $#arr above.
    if (ref $left eq 'ARRAY' && @$left == 2 && !ref $left->[0]
        && $left->[0] eq 'p-dynamic-typeglob') {
      return ['p-glob-assign-dynamic', $left->[1], $right];
    }
    if ($left_flat =~ /^\(p-dynamic-typeglob (.+)\)$/) {
      return ['p-glob-assign-dynamic', Pl::CLForm::raw($1), $right];
    }
    # Assigning to a sub/code-ref CALL is a compile error (no lvalue subs); the
    # built-in magic lvalues substr/pos/vec ARE allowed.  AST-level, verbatim
    # from gen_binary_op so eval-string probes see the same failure.
    if (defined $node_id) {
      my $lnode = $self->expr_o->get_a_node($kids->[0]);
      my $bad_lvalue = 0;
      if ($self->expr_o->is_internal_node_type($lnode)
          && ($lnode->{type} eq 'funcall' || $lnode->{type} eq 'ref_funcall')) {
        # foo() = x  /  $cref->() = x
        $bad_lvalue = 1;
        if ($lnode->{type} eq 'funcall') {
          my $fkids = $self->expr_o->get_node_children($kids->[0]);
          if ($fkids && @$fkids) {
            my $fn = $self->expr_o->get_a_node($fkids->[0]);
            my $nm = (ref($fn) && $fn->can('content')) ? $fn->content : '';
            $bad_lvalue = 0 if $nm =~ /^(?:CORE::)?(?:substr|pos|vec)$/;
          }
        }
      } elsif (ref($lnode) && $lnode->can('content')
               && $lnode->content =~ /^&/) {
        # &sub = x  (ampersand call as an lvalue; a leaf Symbol token).
        $bad_lvalue = 1;
      }
      die "PCL: Can't modify non-lvalue subroutine call in assignment\n"
        if $bad_lvalue;
    }
    # Sole-ternary parenthesized lvalue = SCALAR assignment (see the string
    # emitter's twin branch above for the full rationale — defins.t t10).
    if (defined $node_id) {
      my $tern_id = $self->_sole_ternary_lvalue_id($kids->[0]);
      if (defined $tern_id) {
        $self->expr_o->set_node_context($kids->[1], SCALAR_CTX);
        my $tern = $self->gen_node_form($tern_id);
        my $rhs  = $self->gen_node_form($kids->[1]);
        return ['box-set', $tern, $rhs];
      }
    }
    if ($left_flat =~ /^\(vector[ )]/) {
      my $ctx = defined $node_id ? $self->expr_o->get_node_context($node_id) : 0;
      my $result = ['p-list-=', $left, $right];
      return $ctx == LIST_CTX
               ? ['let', ['list', ['list', '*wantarray*', 't']],   $result]
           : $ctx == SCALAR_CTX
               ? ['let', ['list', ['list', '*wantarray*', 'nil']], $result]
           : $result;
    } elsif ($left_flat =~ /^\(p-cast-% /) {
      # %$ref = (list): assign to a dereferenced hash
      return ['p-hash-deref-=', $left, $right];
    } elsif ($left_flat =~ /^\(p-cast-@ /) {
      # @$ref = (list): assign to a dereferenced array
      return ['p-array-deref-=', $left, $right];
    } elsif ($left_flat =~ /^\(p-(?:gethash|aref|aslice|hslice) /) {
      # Single-element / slice store: $h{k}=… / $a[i]=… (MUST precede the sigil
      # regexes: a package-qualified element form contains "::%"/"::@").
      return ['p-setf', $left, $right];
    } elsif ($left_flat =~ /(?:^|::)@/) {
      return ['p-array-=', $left, $right];
    } elsif ($left_flat =~ /(?:^|::)%/) {
      return ['p-hash-=', $left, $right];
    } elsif ($left_flat =~ /(?:^|::)\$/) {
      return ['p-scalar-=', $left, $right];
    }
    # else: fall through to (p-setf $left $right) at the generic tail.
  }

  # 'use integer' pragma: truncate operands first, then operate.
  if ($self->environment && $self->environment->has_pragma('use_integer')) {
    return ['truncate', ['p-int', $left], ['p-int', $right]]  if $op eq '/';
    return ['rem',      ['p-int', $left], ['p-int', $right]]  if $op eq '%';
    return ['+',        ['p-int', $left], ['p-int', $right]]  if $op eq '+';
    return ['-',        ['p-int', $left], ['p-int', $right]]  if $op eq '-';
    return ['*',        ['p-int', $left], ['p-int', $right]]  if $op eq '*';
    return ['p-to-s64', ['logand', ['pcl::%pcl-to-integer', ['to-number', $left]],
                                   ['pcl::%pcl-to-integer', ['to-number', $right]]]] if $op eq '&';
    return ['p-to-s64', ['logior', ['pcl::%pcl-to-integer', ['to-number', $left]],
                                   ['pcl::%pcl-to-integer', ['to-number', $right]]]] if $op eq '|';
    return ['p-to-s64', ['logxor', ['pcl::%pcl-to-integer', ['to-number', $left]],
                                   ['pcl::%pcl-to-integer', ['to-number', $right]]]] if $op eq '^';
    return ['p-<<-int', $left, $right] if $op eq '<<';
    return ['p->>-int', $left, $right] if $op eq '>>';
  }

  # Match operators read *wantarray* at runtime to choose boolean (scalar) vs
  # capture list (list); pin it to the node's static context so an enclosing
  # list construct's *wantarray* can't leak in.  A subst/tr RHS returns a scalar
  # count, so skip the wrapper for those — detected AST-level (the RHS is a
  # Regexp::Substitute / Regexp::Transliterate node) rather than by grepping the
  # generated $right for /^\(p-(subst|tr|translate)/.
  if ($op eq '=~' || $op eq '!~') {
    my $rhs_node = $self->expr_o->get_a_node($kids->[1]);
    my $rhs_is_subst_tr = ref($rhs_node) eq 'PPI::Token::Regexp::Substitute'
                       || ref($rhs_node) eq 'PPI::Token::Regexp::Transliterate';
    if (!$rhs_is_subst_tr) {
      my $ctx = defined $node_id ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;
      return ['let', ['list', ['list', '*wantarray*', 'nil']], [$cl_op, $left, $right]]
        if $ctx == SCALAR_CTX;
      return ['let', ['list', ['list', '*wantarray*', 't']], [$cl_op, $left, $right]]
        if $ctx == LIST_CTX;
    }
  }

  return [$cl_op, $left, $right];
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
# Form-producing (E2-converted).  Never declines.
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
    my $generated = $self->gen_node_form($kid_id);

    # Check if this is an array variable (@arr) - needs to be joined
    my $kid_content = (ref($kid_node) eq 'PPI::Token::Symbol' && $kid_node->can('content'))
                      ? ($kid_node->content() // '') : '';
    if ($kid_content =~ /^@/ || $is_slice) {
      # In Perl, "@arr" in string interpolation joins with $" (default space)
      # Use |$"| which is the CL variable for Perl's $" list separator
      push @parts, ['p-join', '|$"|', $generated];
    } else {
      push @parts, $generated;
    }
  }
  return ['p-string-concat', @parts];
}


# Array interpolation in string: "@{[expr]}" or "@{$ref}" → (p-join |$"| (p-cast-@ EXPR))
# Form-producing (E2-converted).  Never declines.
sub gen_array_str_interp {
  my ($self, $node, $node_id, $kids) = @_;
  return '""' unless @$kids;
  return ['p-join', '|$"|', ['p-cast-@', $self->gen_node_form($kids->[0])]];
}


# Function call: (p-FUNC args...)
# A hash/array ELEMENT passed to a USER sub must reach @_ as an alias to
# the element (perl's defelem magic; task #131).  lvalue_context carries a
# third value, 'argbox', set around user-sub argument generation: the
# named-container element sites pick the -argbox accessor — the live slot
# box when the element exists, a lazy defelem cell when it does not, so a
# read-only callee never vivifies.  Reuses lvalue_context's existing
# set/clear discipline instead of a new flag (the deref element sites
# treat 'argbox' as plain lvalue: aliasing with eager vivify).
# True when the RHS of a `=~` / `!~` MODIFIES the bound target.  Node-type
# test, not a grep of the generated text (the same rule the *wantarray*
# wrapper below already uses).  perl's own boundary, probed:
#
#   $a[2] =~ s/x/y/     modifies (and so VIVIFIES a missing element: @a == 3)
#   $a[2] =~ tr/x/y/    modifies, vivifies
#   $a[2] =~ tr/N/N/    COUNTS — identical lists, no d/s/c: @a stays empty
#   $a[2] =~ s/x/y/r    builds a NEW string, target untouched
#   $a[2] =~ /x/        a read
#
# The count-only case matters beyond tidiness: taking the lvalue there creates
# the element, which perl-tests/tr.t checks by name ("doesn't extend the
# array").  An empty tr replacement list replicates the search list, so it is
# a count too — unless /d, where empty means DELETE.
sub _rhs_writes_match_target {
  my ($self, $rhs_id) = @_;
  my $n = $self->expr_o->get_a_node($rhs_id);
  my $r = ref $n;
  return 0 unless $r eq 'PPI::Token::Regexp::Substitute'
               || $r eq 'PPI::Token::Regexp::Transliterate';
  my $mods = eval { $n->get_modifiers } || {};
  return 0 if $mods->{r};
  if ($r eq 'PPI::Token::Regexp::Transliterate') {
    my $from = eval { $n->get_match_string };
    my $to   = eval { $n->get_substitute_string };
    return 1 unless defined $from && defined $to;
    $to = $from if $to eq '' && !$mods->{d};
    return 0 if $to eq $from && !$mods->{d} && !$mods->{s} && !$mods->{c};
  }
  return 1;
}

sub _elem_accessor {
  my ($self, $base) = @_;
  my $lv = $self->lvalue_context;
  return $base unless $lv;
  return "$base-argbox" if $lv eq 'argbox';
  return "$base-box";
}

# True when the arg node itself IS a named-container element access — the
# only shape 'argbox' applies to.  Gating per-arg keeps the context from
# leaking into arbitrary subtrees (a `~$_` method arg once flipped from
# string- to numeric-complement under a blanket 'argbox').
sub _is_elem_arg {
  my ($self, $kid_id) = @_;
  my $an = $self->expr_o->get_a_node($kid_id);
  return $self->expr_o->is_internal_node_type($an)
      && ($an->{type} eq 'a_acc' || $an->{type} eq 'h_acc');
}

# A CLASS-NAME argument position (bless's 2nd arg, tie's 2nd arg): a bareword
# there is a STRING, never a call — perl's own rule for those slots.  This is
# a per-builtin ARGUMENT-POSITION rule on purpose: unlike a global
# bareword-is-a-string rule it cannot reach `Foo::init` in expression position
# or a method invocant (task #142 records three failed global attempts).
# Returns the CL string literal, or undef when the argument is not a bareword —
# then the caller generates it normally (quoted string, `shift`, expression…).
# The LHS of `(EXPR) = …` when EXPR is a sole TERNARY: `($c ? $a : $b) = V`
# is a SCALAR assignment in perl (the ternary is an lvalue, perlop), not a
# one-element list assignment.  Returns the ternary node's id, or undef when
# the LHS is anything else.  Shared by the string and CLForm '=' handlers.
sub _sole_ternary_lvalue_id {
  my ($self, $lhs_id) = @_;
  my $lnode = $self->expr_o->get_a_node($lhs_id);
  return undef unless $self->expr_o->is_internal_node_type($lnode)
      && ($lnode->{type} eq 'tree_val' || $lnode->{type} eq 'progn');
  my $ch = $self->expr_o->get_node_children($lhs_id) || [];
  return undef unless @$ch == 1;
  my $c = $self->expr_o->get_a_node($ch->[0]);
  return undef unless $self->expr_o->is_internal_node_type($c)
      && $c->{type} eq 'ternary';
  return $ch->[0];
}

sub _class_name_bareword {
  my ($self, $kid_id) = @_;
  my $class_node = $self->expr_o->get_a_node($kid_id);
  return undef unless $self->expr_o->is_internal_node_type($class_node)
                   && $class_node->{type} eq 'funcall';
  # Bareword funcalls have exactly 1 child (the word itself, no arguments).
  my $class_kids = $self->expr_o->get_node_children($kid_id);
  return undef unless @$class_kids == 1;
  my $word_node = $self->expr_o->get_a_node($class_kids->[0]);
  return undef unless ref($word_node) eq 'PPI::Token::Word';
  my $classname = $word_node->content();
  if ($classname eq '__PACKAGE__') {
    my $pkg = $self->environment ? $self->environment->current_package : 'main';
    $pkg //= 'main';
    return qq{"$pkg"};
  }
  # undef keyword: not a bareword class name — the caller's normal generation
  # gives (p-undef), and the runtime handles an undef class.
  return undef if $classname eq 'undef';
  $classname =~ s/::$//;   # o:: -> o
  return qq{"$classname"};
}

# ---- E2.1: form-producing funcall (the generic call path) ------------------
# Form-producing (E2-converted).  Covers the GENERIC call path — user subs
# (word:is/ok/… = the seam frontier head) and non-special builtins —
# including the prototype machinery ('$'-slot scalar imposition, \@/\%/\$
# auto-boxing), the print-family $_ default, die/warn :loc, my/our
# identity, the split/join wraps, eval (block + string, with the
# lexical-capture alist), and the *wantarray* context wraps.
# Byte-for-byte the text emitter's shapes.  The only remaining decline is
# a non-Word call head (never fires on the corpus — s304 census); the
# decline decision precedes any side effect.
sub gen_funcall_form {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Zero-arg special words (Parser wraps them in funcall when followed by
  # operators) and -bareword strings: pure atoms, same bytes as the text
  # emitter's early branch.
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
        return $func_node->line_number // 0;
      }
      if ($content eq '__PACKAGE__') {
        my $pkg = $self->environment
            ? $self->environment->current_package : 'main';
        $pkg //= 'main';
        return qq{"$pkg"};
      }
      # Perl: -bareword produces string "-bareword"
      if ($content =~ /^-[A-Za-z_]\w*$/) {
        return qq{"$content"};
      }
    }
  }

  # Only plain Word heads: gen_node on a Word is pure (gen_leaf), so the
  # decline→re-run of the text path repeats no side effect.  Non-Word
  # heads (Symbol '&foo' etc.) stay on the text path entirely.
  if (ref($self->expr_o->get_a_node($kids->[0])) ne 'PPI::Token::Word') {
    return undef;
  }

  my $func_name = $self->gen_node($kids->[0]);

  # -funcname with arguments = unary negation of the call (PPI tokenizes
  # "-splice @a" as one Word).  Known built-ins convert; an unknown -name
  # falls through to the generic tail exactly like the text emitter.
  if ($func_name =~ /^-([A-Za-z_]\w*)$/) {
    my $real_func = $1;
    if (exists $RUNTIME_NAMES{$real_func}) {
      my $inner_cl = $self->cl_name($real_func, 1);
      my @arg_forms = map { $self->gen_node_form($_) } @{$kids}[1 .. $#$kids];
      return ['p--', [$inner_cl, @arg_forms]];
    }
  }

  # SUPER::method(args) — indirect-object super call: all args flatten at
  # runtime; the first element of the combined list is the invocant.
  if ($func_name =~ /^SUPER::(.+)$/) {
    my $method = $1;
    my $cur_pkg = ($self->environment && $self->environment->can('current_package'))
                    ? ($self->environment->current_package // 'main')
                    : 'main';
    if (@$kids >= 2) {
      my @arg_forms = map { $self->gen_node_form($_) } @{$kids}[1 .. $#$kids];
      return ['pcl::%pcl-super-indirect', "\"$method\"", "\"$cur_pkg\"", @arg_forms];
    }
    return ['pcl::%pcl-super-indirect', "\"$method\"", "\"$cur_pkg\"", 'nil'];
  }

  my $cl_func = $self->cl_name($func_name, 1, $node->{force_user_sub} ? 1 : 0);

  # ---- converted special branches (same order as the text emitter; a
  # ---- non-matching shape FALLS THROUGH to the generic tail, exactly
  # ---- like the text branches do) ----

  # require BAREWORD in expression context → (p-require "Module")
  if ($func_name eq 'require' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    my $mod;
    if (ref($arg_node) eq 'PPI::Token::Word') {
      $mod = $arg_node->content;
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node)
           && $arg_node->{type} eq 'funcall') {
      my $ak = $self->expr_o->get_node_children($kids->[1]);
      if (@$ak == 1) {
        my $w = $self->expr_o->get_a_node($ak->[0]);
        $mod = $w->content if ref($w) eq 'PPI::Token::Word';
      }
    }
    if (defined $mod && $mod =~ /^\w+(?:::\w+)*$/) {
      return ['p-require', "\"$mod\""];
    }
    # Non-bareword `require EXPR` (a variable, e.g. the inserted bare-require
    # $_): perl's EXPR form has FILENAME semantics — and p-require-file also
    # dispatches a numeric value to the version check.
    return ['p-require-file', $self->gen_node_form($kids->[1])];
  }

  # eval BLOCK / eval STRING — mirrors the text emitter branch for branch.
  # Block forms → (p-eval-block …) [inline_lambda bodies stay a raw atom
  # until that conversion]; string/computed forms → (p-eval …) carrying the
  # lexical-capture alist (docs/eval-lexical-capture.md).
  if ($func_name eq 'eval' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      my $ctx = $self->expr_o->get_node_context($node_id);
      my $wrap = sub {
        my ($inner) = @_;
        return $inner if $ctx == INHERIT_CTX;
        return $self->_ctx_wrap_form($inner, $ctx);
      };
      if ($arg_node->{type} eq 'anon_sub') {
        my $block_kids = $self->expr_o->get_node_children($kids->[1]);
        # empty eval {}: text emits "(p-eval-block )" (trailing space) —
        # normalize at E2.final with the other empty-shape quirks
        return undef if !@$block_kids;
        my @body_forms = map { $self->gen_node_form($_) } @$block_kids;
        return $wrap->(['p-eval-block', @body_forms]);
      }
      elsif ($arg_node->{type} eq 'inline_lambda') {
        # task #78: a Parser2-lowered body arrives structured (body_form).
        if (my $bf = $arg_node->{body_form}) {
          return $wrap->(['p-eval-block', @$bf]);
        }
        my $body = $arg_node->{body_cl} // 'nil';
        return $wrap->(['p-eval-block', Pl::CLForm::raw($body)]);
      }
      elsif ($arg_node->{type} eq 'func_ref') {
        my $func_ref = $self->gen_node_form($kids->[1]);
        return $wrap->(['p-eval-block', ['funcall', $func_ref]]);
      }
      else {
        # Internal node that is NOT a block form = interpolated/computed
        # STRING — still eval STRING, must carry the capture alist.
        my $arg_form = $self->gen_node_form($kids->[1]);
        my $alist    = $self->_eval_lexical_alist;
        return $alist ? ['p-eval', $arg_form, $alist]
                      : ['p-eval', $arg_form];
      }
    }
    else {
      # eval STRING (plain string literal) with the caller's in-scope
      # lexicals as an alist (docs/eval-lexical-capture.md).
      my $arg_form = $self->gen_node_form($kids->[1]);
      my $alist    = $self->_eval_lexical_alist;
      return $alist ? ['p-eval', $arg_form, $alist]
                    : ['p-eval', $arg_form];
    }
  }

  # next/last/redo LABEL → (p-next LABEL) etc.
  if (($func_name eq 'next' || $func_name eq 'last' || $func_name eq 'redo')
      && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids == 1) {
        my $label_node = $self->expr_o->get_a_node($arg_kids->[0]);
        if (ref($label_node) eq 'PPI::Token::Word') {
          return [$cl_func, $label_node->content()];
        }
      }
    }
  }

  # goto — tail-call (&sub / &$cref), goto LABEL (throw-wrap or lexical go),
  # and computed goto EXPR.  Falls through to the generic tail only for a
  # shape none of these match (which the text emitter never produced).
  if ($func_name eq 'goto' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);

    # goto &funcname — tail-call to a named sub.
    if (ref($arg_node) eq 'PPI::Token::Symbol' &&
        $arg_node->content() =~ /^&(.+)$/) {
      my $target = $self->cl_name($1, 1);
      return ['p-goto-sub', "#'$target"];
    }

    # goto &$scalar — tail-call via dynamic coderef (Cast '&' prefix_op).
    # goto wants the coderef MENTION, not the call the `&` prefix lowers to.
    if (defined(my $amp_id = $self->_amp_cast_operand_id($kids->[1]))) {
      return ['p-goto-sub', ['p-get-coderef', $self->gen_node_form($amp_id)]];
    }

    # goto LABEL — forward (catch-wrapped) throw, else lexical (go).
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids == 1) {
        my $label_node = $self->expr_o->get_a_node($arg_kids->[0]);
        if (ref($label_node) eq 'PPI::Token::Word') {
          my $label = $label_node->content();
          my $parser = ($self->expr_o->can('has_parser')
                        && $self->expr_o->has_parser) ? $self->expr_o->parser : undef;
          if ($parser && $parser->{_catch_labels}
              && $parser->{_catch_labels}{$label}) {
            return ['throw', $parser->{_catch_labels}{$label}, 'nil'];
          }
          return ['go', ":$label"];
        }
      }
    }

    # goto EXPR (computed) — no-op stub (CL tags are not first-class).
    return ['p-goto-computed', $self->gen_node_form($kids->[1])];
  }

  # do { BLOCK } / do &CODE — inline evaluation, returns last value.  (do FILE
  # has a non-internal-node arg and falls through to the generic tail, which
  # carries the do ctx-wrap.)
  if ($func_name eq 'do' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      if ($arg_node->{type} eq 'func_ref') {
        my $func_ref = $self->gen_node_form($kids->[1]);
        my $ctx = $self->expr_o->get_node_context($node_id);
        return ['funcall', $func_ref] if $ctx == INHERIT_CTX;
        return $self->_ctx_wrap_form(['funcall', $func_ref], $ctx);
      }
      elsif ($arg_node->{type} eq 'anon_sub') {
        my $block_kids = $self->expr_o->get_node_children($kids->[1]);
        my @body_parts = map { $self->gen_node_form($_) } @$block_kids;
        return ['progn', @body_parts];
      }
      elsif ($arg_node->{type} eq 'inline_lambda') {
        # do { BLOCK } parsed as inline_lambda.  A Parser2-lowered body
        # arrives structured (body_form, task #78); a declined one keeps
        # v1's body_cl text as a raw atom.  The funcall node itself is
        # form-producing either way.
        my $ctx  = $self->expr_o->get_node_context($node_id);
        my @body = ($arg_node->{body_form})
                 ? @{ $arg_node->{body_form} }
                 : (Pl::CLForm::raw($arg_node->{body_cl} // 'nil'));
        return ['progn', @body] if $ctx == INHERIT_CTX;
        return $self->_ctx_wrap_form(['progn', @body], $ctx);
      }
    }
  }

  # grep/map EXPRESSION form: grep EXPR, LIST → (p-grep (lambda ($_) EXPR) LIST).
  # The BLOCK/lambda form (first arg is inline_lambda/func_ref/anon_sub) is NOT
  # handled here — it is a plain funcall that rides the generic tail, which
  # gen_node_form's the lambda child (the inline_lambda emitter's output,
  # embedded as a raw atom until THAT emitter is converted — E2's final step —
  # at which point grep/map go structural with no change here).  Exactly the
  # text emitter's split: only the non-lambda first arg gets the lambda wrap.
  if (($func_name eq 'grep' || $func_name eq 'map') && @$kids >= 2) {
    my $first_arg_node = $self->expr_o->get_a_node($kids->[1]);
    my $is_lambda_form = $self->expr_o->is_internal_node_type($first_arg_node) &&
                         ($first_arg_node->{type} eq 'inline_lambda' ||
                          $first_arg_node->{type} eq 'func_ref' ||
                          $first_arg_node->{type} eq 'anon_sub');
    if (!$is_lambda_form) {
      my $expr_form = $self->gen_node_form($kids->[1]);
      my @rest = map { $self->gen_node_form($kids->[$_]) } 2 .. $#$kids;
      return [$cl_func, ['lambda', ['list', '$_'], $expr_form], @rest];
    }
  }

  # bless(REF, CLASSNAME): bareword class → string; default = current pkg.
  if ($func_name eq 'bless' && @$kids >= 2) {
    my $ref_arg = $self->gen_node_form($kids->[1]);
    my $cur_pkg = $self->environment ? $self->environment->current_package : 'main';
    my $class_arg = "\"$cur_pkg\"";
    if (@$kids >= 3) {
      $class_arg = $self->_class_name_bareword($kids->[2])
                // $self->gen_node_form($kids->[2]);
    }
    return ['p-bless', $ref_arg, $class_arg];
  }

  # tie VARIABLE, CLASSNAME, LIST: same class-name argument position as bless.
  # `tie %h, Tie::StdHash;` (trailing bareword, no LIST) parses as a funcall —
  # every other shape already reaches here as a string (task #142).
  if ($func_name eq 'tie' && @$kids >= 3) {
    my $class_arg = $self->_class_name_bareword($kids->[2]);
    if (defined $class_arg) {
      my @rest = map { $self->gen_node_form($kids->[$_]) } 3 .. $#$kids;
      return ['p-tie', $self->gen_node_form($kids->[1]), $class_arg, @rest];
    }
  }

  # push/unshift: flatten @-sigiled / @-deref arguments.
  if (($func_name eq 'push' || $func_name eq 'unshift') && @$kids >= 2) {
    my $target = $self->gen_node_form($kids->[1]);
    my @items;
    for my $i (2 .. $#$kids) {
      my $arg_node = $self->expr_o->get_a_node($kids->[$i]);
      my $arg = $self->gen_node_form($kids->[$i]);
      my $should_flatten = 0;
      if (ref($arg_node) eq 'PPI::Token::Symbol') {
        my $sigil = substr($arg_node->content(), 0, 1);
        $should_flatten = 1 if $sigil eq '@';
      }
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
      $arg = ['p-flatten', $arg] if $should_flatten;
      push @items, $arg;
    }
    return [$cl_func, $target, @items];
  }

  # readline(BAREWORD) / select(BAREWORD): the arg is a filehandle name.
  if (($func_name eq 'readline' || $func_name eq 'select') && @$kids == 2) {
    my $head = $func_name eq 'readline' ? 'p-readline' : 'p-select';
    my $fh_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($fh_node) eq 'PPI::Token::Word' && $fh_node->can('content')) {
      return [$head, "'" . ($fh_node->content() // '')];
    }
    if ($self->expr_o->is_internal_node_type($fh_node) && $fh_node->{type} eq 'funcall') {
      my $fh_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$fh_kids == 1) {
        my $word_node = $self->expr_o->get_a_node($fh_kids->[0]);
        if (ref($word_node) eq 'PPI::Token::Word' && $word_node->can('content')) {
          return [$head, "'" . ($word_node->content() // '')];
        }
      }
    }
  }

  # tied($a[i]) / tied($h{k}): needs the box for identity tracking.
  if ($func_name eq 'tied' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr = $self->gen_node($arg_kids->[0]);
        my $idx = $self->gen_node_form($arg_kids->[1]);
        $arr = _swap_elem_sigil($arr, q(@));
        return ['p-tied', ['p-aref-box', $arr, $idx]];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my $key  = $self->gen_node_form($arg_kids->[1]);
        $hash = _swap_elem_sigil($hash, q(%));
        return ['p-tied', ['p-gethash-box', $hash, $key]];
      }
    }
  }

  # pos($a[i]) / pos($h{k}): needs the box for *p-match-pos* tracking.
  if ($func_name eq 'pos' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr = $self->gen_node($arg_kids->[0]);
        my $idx = $self->gen_node_form($arg_kids->[1]);
        $arr = _swap_elem_sigil($arr, q(@));
        return ['p-pos', ['p-aref-box', $arr, $idx]];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my $key  = $self->gen_node_form($arg_kids->[1]);
        $hash = _swap_elem_sigil($hash, q(%));
        return ['p-pos', ['p-gethash-box', $hash, $key]];
      }
    }
  }

  # delete on array/hash elements and slices: pass container + key/index.
  if ($func_name eq 'delete' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $arr = $self->gen_node($arg_kids->[0]);
        if ((ref($arr_node) eq 'PPI::Token::Symbol'
             || ref($arr_node) eq 'PPI::Token::Magic')) {
          $arr = _swap_elem_sigil($arr, q(@));
        }
        my $idx = $self->gen_node_form($arg_kids->[1]);
        return ['p-delete-array', $arr, $idx];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $hash = $self->gen_node($arg_kids->[0]);
        if ((ref($hash_node) eq 'PPI::Token::Symbol'
             || ref($hash_node) eq 'PPI::Token::Magic')) {
          $hash = _swap_elem_sigil($hash, q(%));
        }
        my $key = $self->gen_node_form($arg_kids->[1]);
        return ['p-delete', $hash, $key];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'slice_h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 1) {
        my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $hash = $self->gen_node($arg_kids->[0]);
        if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /(?:^|::)\@/) {
          $hash =~ s/(^|::)\@/${1}%/;
        }
        my @keys = map { $self->gen_node_form($arg_kids->[$_]) } 1 .. $#$arg_kids;
        return ['p-delete-hash-slice', $hash, @keys];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'slice_a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr = $self->gen_node($arg_kids->[0]);
        my @indices = map { $self->gen_node_form($arg_kids->[$_]) } 1 .. $#$arg_kids;
        return ['p-delete-array-slice', $arr, @indices];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'kv_slice_h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my @keys = map { $self->gen_node_form($arg_kids->[$_]) } 1 .. $#$arg_kids;
        return ['p-delete-kv-hash-slice', $hash, @keys];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'kv_slice_a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 1) {
        my $arr = $self->gen_node($arg_kids->[0]);
        $arr =~ s/(^|::)\%/${1}\@/;
        my @indices = map { $self->gen_node_form($arg_kids->[$_]) } 1 .. $#$arg_kids;
        return ['p-delete-kv-array-slice', $arr, @indices];
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_ref_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $ref = $self->gen_node_form($arg_kids->[0]);
        my $key = $self->gen_node_form($arg_kids->[1]);
        return ['p-delete', ['unbox', $ref], $key];
      }
    }
    # Array-ref access: delete $ref->[i] / delete $$ref[i].  Without this the
    # element lowered as a VALUE and delete got one argument — an arity crash,
    # not a wrong answer.  exists has had both ref arms all along; delete had
    # only the hash one.
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'a_ref_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $ref = $self->gen_node_form($arg_kids->[0]);
        my $idx = $self->gen_node_form($arg_kids->[1]);
        return ['p-delete-array', ['unbox', $ref], $idx];
      }
    }
  }

  # exists on array/hash elements, refs, and sub/coderef existence.
  if ($func_name eq 'exists' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($arg_node) eq 'PPI::Token::Symbol') {
      my $sym = $arg_node->content();
      if ($sym =~ /^&(.+)$/) {
        my ($pkg, $name) = $self->_split_func_sym($1);
        return ['p-sub-exists', "\"$pkg\"", "\"$name\""];
      }
    }
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'prefix_op') {
      my $arg_kids2 = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids2 >= 2) {
        my $cast_node = $self->expr_o->get_a_node($arg_kids2->[0]);
        if (ref($cast_node) eq 'PPI::Token::Cast' && $cast_node->content() eq '&') {
          my $inner = $self->gen_node_form($arg_kids2->[1]);
          return ['p-coderef-exists-p', $inner];
        }
      }
    }
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        if ($arg_node->{type} eq 'a_acc') {
          my $arr_node = $self->expr_o->get_a_node($arg_kids->[0]);
          my $arr = $self->gen_node($arg_kids->[0]);
          if ((ref($arr_node) eq 'PPI::Token::Symbol'
               || ref($arr_node) eq 'PPI::Token::Magic')) {
            $arr = _swap_elem_sigil($arr, q(@));
          }
          my $idx = $self->gen_node_form($arg_kids->[1]);
          return ['p-exists-array', $arr, $idx];
        }
        elsif ($arg_node->{type} eq 'h_acc') {
          my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
          my $hash = $self->gen_node($arg_kids->[0]);
          if ((ref($hash_node) eq 'PPI::Token::Symbol'
               || ref($hash_node) eq 'PPI::Token::Magic')) {
            $hash = _swap_elem_sigil($hash, q(%));
          }
          my $key = $self->gen_node_form($arg_kids->[1]);
          return ['p-exists', $hash, $key];
        }
        elsif ($arg_node->{type} eq 'h_ref_acc') {
          my $ref = $self->gen_node_form($arg_kids->[0]);
          my $key = $self->gen_node_form($arg_kids->[1]);
          return ['p-exists', ['unbox', $ref], $key];
        }
        elsif ($arg_node->{type} eq 'a_ref_acc') {
          my $ref = $self->gen_node_form($arg_kids->[0]);
          my $idx = $self->gen_node_form($arg_kids->[1]);
          return ['p-exists-array', ['unbox', $ref], $idx];
        }
      }
    }
  }

  # defined: sub/coderef defined check, and bareword-filehandle check.
  if ($func_name eq 'defined' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($arg_node) eq 'PPI::Token::Symbol') {
      my $sym = $arg_node->content();
      if ($sym =~ /^&(.+)$/) {
        my ($pkg, $name) = $self->_split_func_sym($1);
        return ['p-sub-defined', "\"$pkg\"", "\"$name\""];
      }
    }
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'prefix_op') {
      my $arg_kids2 = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids2 >= 2) {
        my $cast_node = $self->expr_o->get_a_node($arg_kids2->[0]);
        if (ref($cast_node) eq 'PPI::Token::Cast' && $cast_node->content() eq '&') {
          my $inner = $self->gen_node_form($arg_kids2->[1]);
          return ['p-coderef-defined-p', $inner];
        }
      }
    }
    if (ref($arg_node) eq 'PPI::Token::Word') {
      my $name = $arg_node->content();
      if ($name =~ /^[A-Z][A-Z0-9_]*$/) {
        return ['p-defined-fh', "'$name"];
      }
    }
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids2 = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids2 == 1) {
        my $fn_node = $self->expr_o->get_a_node($arg_kids2->[0]);
        if (ref($fn_node) eq 'PPI::Token::Word') {
          my $name = $fn_node->content();
          if ($name =~ /^[A-Z][A-Z0-9_]*$/) {
            return ['p-defined-fh', "'$name"];
          }
        }
      }
    }
  }

  # undef &funcname — undefine a sub (keeps it in the exists table).
  if ($func_name eq 'undef' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($arg_node) eq 'PPI::Token::Symbol') {
      my $sym = $arg_node->content();
      if ($sym =~ /^&(.+)$/) {
        my ($pkg, $name) = $self->_split_func_sym($1);
        return ['p-undef-sub', "\"$pkg\"", "\"$name\""];
      }
    }
    # undef &{expr} / undef &$cref — the coderef mention, not a call.
    if (defined(my $amp_id = $self->_amp_cast_operand_id($kids->[1]))) {
      return ['p-undef', ['p-get-coderef', $self->gen_node_form($amp_id)]];
    }
  }

  # ---- from here on: the text emitter's generic tail, form-shaped ----

  my $proto = $self->environment ? $self->environment->get_prototype($func_name) : undef;
  my @ref_params;
  if ($proto && $proto->{is_proto} && $proto->{params}) {
    @ref_params = map { $_->{proto_type} // $_->{name} } @{$proto->{params}};
  }

  # '$'-slot scalar imposition only when args >= mandatory-param count
  # (fewer args = an array is flattening across the slots; see the text
  # emitter's comment for the full rule).
  my $n_call_args = $#$kids;
  my $may_impose_scalar =
       @ref_params
    && defined $proto->{min_params}
    && $proto->{min_params} >= 0
    && $n_call_args >= $proto->{min_params};

  # Functions that modify their arguments (chop/chomp/undef) need l-value
  # access: undef $hash{k} / undef $arr[i] must receive the box, not the
  # unboxed value.  Same rule as the text emitter's %lvalue_funcs.
  my %lvalue_funcs = map { $_ => 1 } qw(chop chomp undef);
  my $needs_lvalue = $lvalue_funcs{$func_name} // 0;

  # Arguments are NOT the tail call: clear tail_position around their
  # generation so they get their own annotated context.
  my $saved_tail = $self->environment ? $self->environment->tail_position : 0;
  $self->environment->tail_position(0) if $self->environment && $saved_tail;

  my @args;
  for my $i (1 .. $#$kids) {
    my $param_idx = $i - 1;
    my $impose_scalar = ($may_impose_scalar
                         && $param_idx < @ref_params
                         && defined $ref_params[$param_idx]
                         && $ref_params[$param_idx] eq '$');
    $self->expr_o->set_node_context($kids->[$i], SCALAR_CTX) if $impose_scalar;

    my $saved_lvalue = $self->lvalue_context;
    if    ($needs_lvalue)               { $self->lvalue_context(1) }
    elsif (index($cl_func, 'pl-') >= 0
           && $self->_is_elem_arg($kids->[$i])) { $self->lvalue_context('argbox') }
    my $arg = $self->gen_node_form($kids->[$i]);
    $self->lvalue_context($saved_lvalue);

    if ($impose_scalar) {
      my $an = $self->expr_o->get_a_node($kids->[$i]);
      my $r = ref($an);
      my $already_scalar =
           $r eq 'PPI::Token::Number'
        || $r =~ /^PPI::Token::Quote\b/
        || ($r eq 'PPI::Token::Symbol' && $an->content() =~ /^\$/)
        || ($r eq 'PPI::Token::Magic'  && $an->content() =~ /^\$/);
      $arg = ['p-scalar', $arg] unless $already_scalar;
    }

    # Reference prototype slot (\@, \%, \$): auto-box a matching bare var.
    if ($param_idx < @ref_params) {
      my $param_type = $ref_params[$param_idx];
      if ($param_type =~ /^\\([@%\$])$/) {
        my $expected_sigil = $1;
        my $arg_node = $self->expr_o->get_a_node($kids->[$i]);
        if (ref($arg_node) eq 'PPI::Token::Symbol') {
          my $arg_sigil = substr($arg_node->content(), 0, 1);
          if ($arg_sigil eq $expected_sigil) {
            $arg = ['p-backslash', $arg];
          }
        }
      }
    }
    push @args, $arg;
  }

  # Bare print/say/printf defaults to $_ (a `:fh …` marker is not a list
  # arg).  The text emitter's regex runs on arg text; to_flat gives the
  # same text for form args (a form never starts with ':fh').
  if ($func_name eq 'print' || $func_name eq 'say' || $func_name eq 'printf') {
    push @args, '$_'
      unless grep { Pl::CLForm::to_flat($_) !~ /^:fh\b/ } @args;
  }

  $self->environment->tail_position($saved_tail) if $self->environment && $saved_tail;

  my $call;
  if ($cl_func eq 'p-die' || $cl_func eq 'p-warn') {
    my $word = $self->expr_o->get_a_node($kids->[0]);
    my $line = (ref($word) && $word->can('line_number')) ? ($word->line_number // 0) : 0;
    my $file = ($self->environment && $self->environment->source_file) || '-';
    $file =~ s/(["\\])/\\$1/g;
    $call = [$cl_func, ':loc', "\"$file line $line\"", @args];
  } else {
    $call = [$cl_func, @args];
  }

  # 'my'/'our' in expression context is an identity.
  if (($func_name eq 'my' || $func_name eq 'our') && @args == 1) {
    return $args[0];
  }

  my $ctx = $self->expr_o->get_node_context($node_id);

  # split: p-split always returns a vector; scalar context takes its length.
  if ($func_name eq 'split') {
    return $ctx == 0 ? ['length', $call] : $call;
  }

  # INHERIT_CTX or tail position: let the caller's *wantarray* flow through.
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;

  if ($WANTARRAY_SENSITIVE{$func_name}) {
    return $self->_wrap_wantarray_ctx_form($call, $ctx);
  }

  # join always evaluates its list arguments in list context.
  if ($func_name eq 'join') {
    return ['let', ['list', ['list', '*wantarray*', 't']], $call];
  }

  # do FILE: same ctx-wrap as a user sub (do is a built-in, so it needs an
  # explicit case ahead of the built-in list-only default below).
  if ($func_name eq 'do') {
    return $self->_ctx_wrap_form($call, $ctx);
  }

  # User sub calls: always bind *wantarray*; built-ins only in list context.
  if (!exists $RUNTIME_NAMES{$func_name}) {
    return $self->_ctx_wrap_form($call, $ctx);
  }
  return $ctx == LIST_CTX
      ? ['let', ['list', ['list', '*wantarray*', 't']], $call]
      : $call;
}

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

  my $cl_func   = $self->cl_name($func_name, 1, $node->{force_user_sub} ? 1 : 0);

  # Special handling: SUPER::method(args) as indirect-object call
  # SUPER::m{@a} is indirect-object syntax: first arg is the invocant (from block)
  # Generate: (pcl::%pcl-super-indirect "m" "pkg" ARGS) where first arg is invocant
  if ($func_name =~ /^SUPER::(.+)$/) {
    my $method = $1;
    my $cur_pkg = ($self->environment && $self->environment->can('current_package'))
                    ? ($self->environment->current_package // 'main')
                    : 'main';
    if (@$kids >= 2) {
      # ALL args: the trailing-LIST shapes (SUPER::m{}@a, SUPER::m{@a}"b")
      # parse to multiple kids; the runtime flattens and takes the first
      # element of the combined list as the invocant.
      my $arg_str = join ' ', map { $self->gen_node($_) } @{$kids}[1 .. $#$kids];
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
    # Non-bareword `require EXPR`: perl's EXPR form has FILENAME semantics
    # (numeric values dispatch to the version check in p-require-file).
    return "(p-require-file " . $self->gen_node($kids->[1]) . ")";
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
    # goto wants the coderef MENTION (the `&` prefix alone lowers to a call
    # with @_), so reach past it and wrap the inner expression ourselves.
    if ($func_name eq 'goto' &&
        defined(my $amp_id = $self->_amp_cast_operand_id($kids->[1]))) {
      my $fn_expr = $self->gen_node($amp_id);
      return "(p-goto-sub (p-get-coderef $fn_expr))";
    }

    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids == 1) {
        my $label_node = $self->expr_o->get_a_node($arg_kids->[0]);
        if (ref($label_node) eq 'PPI::Token::Word') {
          my $label = $label_node->content();
          if ($func_name eq 'goto') {
            # goto LABEL: a FORWARD goto being catch-wrapped by Parser2's
            # #63 lowering (flag localized around the prefix lowering, so
            # it is set exactly while this goto's region is lowered) must
            # be a dynamic transfer — the tag's tagbody opens later, and
            # the goto may sit inside a map/grep lambda, both unreachable
            # by a lexical (go).  Backward gotos keep the (go): the
            # enclosing (tagbody :label …) re-runs the jumped-to region.
            my $parser = ($self->expr_o->can('has_parser')
                          && $self->expr_o->has_parser) ? $self->expr_o->parser : undef;
            if ($parser && $parser->{_catch_labels}
                && $parser->{_catch_labels}{$label}) {
              return "(throw $parser->{_catch_labels}{$label} nil)";
            }
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
        return $self->_ctx_wrap("(funcall $func_ref)", $ctx);
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
        return $self->_ctx_wrap("(progn $body)", $ctx);
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
        return $self->_ctx_wrap($inner, $ctx);
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
        # eval { block } parsed as inline_lambda (avoids defun side-effect).
        # task #78 safety net: a Parser2-lowered body has body_form only —
        # flat-print it (the form branch normally intercepts this shape).
        my $body = defined $arg_node->{body_cl} ? $arg_node->{body_cl}
                 : $arg_node->{body_form}
                 ? join(' ', map { Pl::CLForm::to_flat($_) } @{$arg_node->{body_form}})
                 : 'nil';
        return $wrap->("(p-eval-block $body)");
      }
      elsif ($arg_node->{type} eq 'func_ref') {
        # eval { block } with named function (from Parser callback)
        my $func_ref = $self->gen_node($kids->[1]);
        return $wrap->("(p-eval-block (funcall $func_ref))");
      }
      else {
        # An internal node that is NOT a block form = an interpolated/computed
        # STRING, e.g. eval "package $into; sub greet { ... }" (a concatenation).
        # This is still eval STRING and must carry the lexical-capture alist —
        # without this it fell through to the generic funcall path with no alist,
        # so the modifier idiom never captured its lexicals.  See
        # docs/eval-free-vars-plan.md / docs/eval-lexical-capture.md.
        my $arg_cl = $self->gen_node($kids->[1]);
        my $alist  = $self->_eval_lexical_alist;
        return $alist
          ? "(p-eval $arg_cl " . Pl::CLForm::to_flat($alist) . ")"
          : "(p-eval $arg_cl)";
      }
    }
    else {
      # eval STRING (plain string literal): pass the caller's in-scope lexicals as
      # an alist so the eval body can capture them (the transpiler wraps the body
      # in a lambda whose params are those vars).  See docs/eval-lexical-capture.md.
      my $arg_cl = $self->gen_node($kids->[1]);
      my $alist  = $self->_eval_lexical_alist;
      return $alist
        ? "(p-eval $arg_cl " . Pl::CLForm::to_flat($alist) . ")"
        : "(p-eval $arg_cl)";
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
      # Bareword class → string (_class_name_bareword); anything else generates
      # normally (quoted string, shift, or other expression).
      $class_arg = $self->_class_name_bareword($kids->[2])
                // $self->gen_node($kids->[2]);
    }
    return "(p-bless $ref_arg $class_arg)";
  }

  # tie VARIABLE, CLASSNAME, LIST — the same class-name argument position.
  if ($func_name eq 'tie' && @$kids >= 3) {
    my $class_arg = $self->_class_name_bareword($kids->[2]);
    if (defined $class_arg) {
      my @args = ($self->gen_node($kids->[1]), $class_arg,
                  map { $self->gen_node($kids->[$_]) } 3 .. $#$kids);
      return "(p-tie " . join(' ', @args) . ")";
    }
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

  # Special handling for select(BAREWORD): the single argument is a filehandle,
  # not a value — quote it so CL does not evaluate an unbound bareword symbol
  # (e.g. `select STDERR`). p-select is a no-op stub, so the quoted name is
  # harmless; select($fh) and 4-arg select(...) are unaffected (@$kids != 2 or
  # the arg is not a bareword).
  if ($func_name eq 'select' && @$kids == 2) {
    my $fh_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($fh_node) eq 'PPI::Token::Word' && $fh_node->can('content')) {
      return "(p-select '" . ($fh_node->content() // '') . ")";
    }
    if ($self->expr_o->is_internal_node_type($fh_node) && $fh_node->{type} eq 'funcall') {
      my $fh_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$fh_kids == 1) {
        my $word_node = $self->expr_o->get_a_node($fh_kids->[0]);
        if (ref($word_node) eq 'PPI::Token::Word' && $word_node->can('content')) {
          return "(p-select '" . ($word_node->content() // '') . ")";
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
        $arr = _swap_elem_sigil($arr, q(@));
        return "(p-tied (p-aref-box $arr $idx))";
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my $key  = $self->gen_node($arg_kids->[1]);
        $hash = _swap_elem_sigil($hash, q(%));
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
        $arr = _swap_elem_sigil($arr, q(@));
        return "(p-pos (p-aref-box $arr $idx))";
      }
    }
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my $key  = $self->gen_node($arg_kids->[1]);
        $hash = _swap_elem_sigil($hash, q(%));
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
             || ref($arr_node) eq 'PPI::Token::Magic')) {
          $arr = _swap_elem_sigil($arr, q(@));
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
             || ref($hash_node) eq 'PPI::Token::Magic')) {
          $hash = _swap_elem_sigil($hash, q(%));
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
    # Array ref access: delete $ref->[i] -> (p-delete-array (unbox ref) i).
    # Mirrors the exists a_ref_acc arm; its absence made delete arity-crash.
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'a_ref_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $ref = $self->gen_node($arg_kids->[0]);
        my $idx = $self->gen_node($arg_kids->[1]);
        return "(p-delete-array (unbox $ref) $idx)";
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
               || ref($arr_node) eq 'PPI::Token::Magic')) {
            $arr = _swap_elem_sigil($arr, q(@));
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
               || ref($hash_node) eq 'PPI::Token::Magic')) {
            $hash = _swap_elem_sigil($hash, q(%));
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
    # undef &{expr} / undef &$cref — the coderef mention, not a call.
    if (defined(my $amp_id = $self->_amp_cast_operand_id($kids->[1]))) {
      my $inner = $self->gen_node($amp_id);
      return "(p-undef (p-get-coderef $inner))";
    }
  }

  # Check for reference prototype that requires auto-boxing
  my $proto = $self->environment ? $self->environment->get_prototype($func_name) : undef;
  my @ref_params;
  if ($proto && $proto->{is_proto} && $proto->{params}) {
    # Use proto_type for auto-boxing checks (contains original like \@, \%, $, etc.)
    @ref_params = map { $_->{proto_type} // $_->{name} } @{$proto->{params}};
  }

  # Whether the '$'-slot SCALAR-context imposition (below) may fire for this
  # call.  When the call supplies FEWER syntactic arguments than the prototype's
  # mandatory-param count, an array argument is flattening to fill the remaining
  # mandatory slots (Perl does NOT scalarize it) — e.g. `sub like_yn ($$$@)`
  # called as `like_yn(0, @_)` where @_ spreads across the trailing $$@.  Only
  # impose scalar context when args >= min_params.  (This also matches Perl's
  # rule that a prototype defined LATER than the call site isn't enforced: in
  # those flatten-into-@_ helpers the count is always short.)
  my $n_call_args = $#$kids;  # kids[0] is the function word
  my $may_impose_scalar =
       @ref_params
    && defined $proto->{min_params}
    && $proto->{min_params} >= 0
    && $n_call_args >= $proto->{min_params};

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
    # A plain '$' prototype slot imposes SCALAR context on its argument
    # (e.g. perl's test.pl `sub is ($$@)` → `is(@a, 3)` counts @a; `is(each
    # @h, 0)` evaluates each in scalar context).  The '\@'/'\%'/'\$' ref slots
    # are handled separately below (auto-box, not scalar-context).  Set the
    # node's context BEFORE gen_node so wantarray-sensitive builtins (each,
    # keys, ...) are generated in scalar context, not merely wrapped after.
    my $param_idx = $i - 1;
    my $impose_scalar = ($may_impose_scalar
                         && $param_idx < @ref_params
                         && defined $ref_params[$param_idx]
                         && $ref_params[$param_idx] eq '$');
    $self->expr_o->set_node_context($kids->[$i], SCALAR_CTX) if $impose_scalar;

    # Set l-value context for functions that modify their arguments; user
    # subs get 'argbox' — their element args alias through @_ (#131).
    my $saved_lvalue = $self->lvalue_context;
    if    ($needs_lvalue)               { $self->lvalue_context(1) }
    elsif (index($cl_func, 'pl-') >= 0
           && $self->_is_elem_arg($kids->[$i])) { $self->lvalue_context('argbox') }
    my $arg = $self->gen_node($kids->[$i]);
    $self->lvalue_context($saved_lvalue);

    # An aggregate (@a/%h) or list-returning builtin (keys/values) passed to a
    # '$' slot must yield its scalar value (count), not flatten into the arg
    # list — wrap it like the `scalar EXPR` keyword does.  Args that are
    # already obviously scalar (number/string literals, $-sigil symbols) need
    # no coercion, and wantarray-sensitive builtins (each/sort/...) are already
    # handled by the SCALAR_CTX annotation above — so skip the wrap for those
    # to keep generated code clean.
    if ($impose_scalar) {
      my $an = $self->expr_o->get_a_node($kids->[$i]);
      my $r = ref($an);
      my $already_scalar =
           $r eq 'PPI::Token::Number'
        || $r =~ /^PPI::Token::Quote\b/
        || ($r eq 'PPI::Token::Symbol' && $an->content() =~ /^\$/)
        || ($r eq 'PPI::Token::Magic'  && $an->content() =~ /^\$/);
      $arg = "(p-scalar $arg)" unless $already_scalar;
    }

    # Check if this position has a reference prototype (\@, \%, \$)
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

  # Bare print/say/printf (no LIST after the optional filehandle) defaults to
  # $_ — emitted EXPLICITLY so the generated CL is self-describing (the runtime
  # no longer supplies a hidden $_ default; see docs/generated-cl-ir-review.md).
  # A `:fh …` marker is not a list arg, so `print STDERR;` / `printf STDERR;`
  # also get the default.  This is what fixes bare `printf;` (which had no
  # runtime default and printed nothing), making the family consistent.
  if ($func_name eq 'print' || $func_name eq 'say' || $func_name eq 'printf') {
    push @args, '$_' unless grep { $_ !~ /^:fh\b/ } @args;
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

  # Wantarray-sensitive built-ins (reverse/localtime/gmtime/caller/unpack/each/
  # splice): their result depends on *wantarray*, which they read at runtime.
  # Bind it explicitly to the node's static context so the enclosing sub's
  # *wantarray* can't leak in (see %WANTARRAY_SENSITIVE).  unpack: scalar
  # unpack() in a list-context assignment (@a = scalar unpack()) must force
  # scalar so p-unpack returns $result[0] not @result.
  if ($WANTARRAY_SENSITIVE{$func_name}) {
    return $self->_wrap_wantarray_ctx($call, $ctx);
  }

  # join always evaluates its list arguments in list context (args after sep),
  # regardless of the context in which join() itself is called.
  if ($func_name eq 'join') {
    return "(let ((*wantarray* t)) $call)";
  }
  if ($func_name eq 'do') {
    return $self->_ctx_wrap($call, $ctx);
  }

  # User sub calls: always bind *wantarray* so the callee sees the correct
  # context regardless of what the surrounding scope has set.
  # Built-ins (in %RUNTIME_NAMES) don't call p-wantarray; only wrap them for
  # list context (to avoid disturbing wantarray-sensitive built-ins called
  # inside a scalar-context scope).
  if (!exists $RUNTIME_NAMES{$func_name}) {
    return $self->_ctx_wrap($call, $ctx);
  }

  # Built-in in list context: still wrap so it gets list-context signal
  # (e.g. a wantarray-sensitive built-in called as the RHS of @arr = builtin())
  return $ctx == LIST_CTX ? "(let ((*wantarray* t)) $call)" : $call;
}


# Wrap a wantarray-sensitive CALL in `(let ((*wantarray* t/nil)) …)` for the
# given static context CTX, so the enclosing sub's *wantarray* cannot leak into
# it.  Shared by gen_funcall (built-ins in %WANTARRAY_SENSITIVE), gen_readline
# (<FH>) and gen_glob (<pat>) — the three syntactic forms whose result depends
# on *wantarray*.  INHERIT_CTX or a tail-call position is left UNWRAPPED so the
# real caller's dynamic binding flows through (these built-ins treat :void like
# scalar, so a 2-way t/nil mapping is sufficient).
sub _wrap_wantarray_ctx {
  my ($self, $call, $ctx) = @_;
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  return $ctx == LIST_CTX
      ? "(let ((*wantarray* t)) $call)"
      : "(let ((*wantarray* nil)) $call)";
}

# Wrap CALL in (let ((*wantarray* WA)) ...) for the node's static context CTX,
# where WA is t (list) / nil (scalar) / :void (void).  Callers handle INHERIT_CTX
# themselves (it must NOT reach here).  When CTX is VOID and a sub-body :void
# regime is active (environment->wa_void_active), the ambient *wantarray* is
# already :void, so the binding is a no-op and is skipped — this is what keeps
# nested void-context calls from re-asserting :void on every statement.
sub _ctx_wrap {
  my ($self, $call, $ctx) = @_;
  return $call if $ctx == VOID_CTX
               && $self->environment && $self->environment->wa_void_active;
  my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
  return "(let ((*wantarray* $wa)) $call)";
}

# Form variants of _wrap_wantarray_ctx / _ctx_wrap for E2-converted
# emitters: same logic, CLForm output (flat-prints to the same bytes).
sub _wrap_wantarray_ctx_form {
  my ($self, $call, $ctx) = @_;
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  return ['let',
          ['list', ['list', '*wantarray*', $ctx == LIST_CTX ? 't' : 'nil']],
          $call];
}

sub _ctx_wrap_form {
  my ($self, $call, $ctx) = @_;
  return $call if $ctx == VOID_CTX
               && $self->environment && $self->environment->wa_void_active;
  my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
  return ['let', ['list', ['list', '*wantarray*', $wa]], $call];
}

# Build the lexical-capture alist passed as the 2nd arg to (p-eval STRING ...).
# Each in-scope lexical becomes (cons "$name" $name), mapping its Perl name to
# its live CL container (box/array/hash).  The in-scope lexicals are the
# parser's _let_bound_vars (the rolling set of `my`/let-bound names, saved and
# restored around every closure).  Returns a CLForm (E2-converted; text
# callers flatten it), or '' when there are none (top-level eval), so codegen
# emits a plain (p-eval STRING).
sub _eval_lexical_alist {
  my $self = shift;
  my $parser = ($self->expr_o && $self->expr_o->can('has_parser')
                && $self->expr_o->has_parser) ? $self->expr_o->parser : undef;
  return '' unless $parser;
  my $lb = $parser->{_let_bound_vars} // {};
  # The alist KEY is the original Perl name; the VALUE is the live CL symbol.
  # Closure-captured lexicals are renamed to $name__lex__N (so per-call let
  # bindings stay lexical); strip that suffix so the key matches the bare name
  # the eval body uses (the eval string never sees the rename).  Same for the
  # v2 seam my-shadow renames $name__shadow__N — and within one stripped key
  # the DEEPEST shadow must come FIRST (p-eval-lex-lookup assoc takes the
  # first match = the innermost live binding).  Shadow counters increase with
  # nesting depth (roots rename outer-first), so descending N is innermost-
  # first; the plain name (depth -1) comes last.  __file__N strips the same
  # way (E4.1 M5, s353): the only __file__ cells that ever enter
  # _let_bound_vars are the enclosing-outer block promotions, whose cell
  # must beat the outer plain binding at sites inside the block; their
  # counters mint in source order, so descending N is innermost-first too.
  # Keys v1 can mint (__lex__, plain) keep the plain sort order — v1
  # emissions stay byte-identical.
  # __cond__N strips the same way (#254 B-i, s363): the poisoned-condition-my
  # rename (`if (my $x = …)` where a package global $x is also live) mints a
  # LET-BOUND `$x__cond__N`, exactly like the shadow rename — so the eval body
  # naming `$x` finds it here, and only while it is in scope.  Its counters
  # mint in source order, so a cond-my nested inside another gets the higher N
  # and descending N is innermost-first, as for __file__.  __emb__N (#265) is
  # the same story one scope in: an expression-embedded `my` inside a named
  # sub, renamed so the let-hoist's scope-blind veto stops refusing it.
  # `state` renames
  # (`__state__`) are deliberately NOT stripped: those are defvar'd cells, not
  # let bindings, and never enter _let_bound_vars.
  my $skey = sub {
    my ($v) = @_;
    $v =~ s/__lex__\d+$//;
    my $d = $v =~ s/__shadow__(\d+)$// ? $1
          : $v =~ s/__file__(\d+)$//   ? $1
          : $v =~ s/__emb__(\d+)$//    ? $1
          : $v =~ s/__cond__(\d+)$//   ? $1 : -1;
    return ($v, $d);
  };
  my @vars = sort {
    my ($ka, $da) = $skey->($a);
    my ($kb, $db) = $skey->($b);
    $ka cmp $kb || $db <=> $da || $a cmp $b
  } keys %$lb;
  my (@pairs, %seen);
  for my $v (@vars) {
    my ($key) = $skey->($v);
    push @pairs, ['cons', "\"$key\"", $v];
    $seen{$key} = 1;
  }
  # Span-mangled file cells (v2's _rename_spanning_lexicals): the eval body
  # names the ORIGINAL `$x`, but the file lexical was renamed to a package
  # cell `Pkg::$x__file__N`.  The rename pass records original→cell per
  # segment; the section driver publishes the current segment's map here.
  # APPENDED after the let-bound pairs so a live lexical shadow of the same
  # name wins (p-eval-lex-lookup assoc takes the first match); a key already
  # let-bound is skipped outright.  The cell is a defvar'd box, so eval
  # writes propagate back exactly like let-bound captures.
  # (Promoted — captured-lexical — cells carry no per-site pairs: they reach
  # evals through the alias rule (p-alias-eval-cell writes the cell into the
  # original-name global, found by p-eval-lex-lookup's global fall-through;
  # ir-spec §9.1).  Only SPAN cells keep emitted pairs, package-qualified
  # for the cross-segment case.)
  my $span = $parser->{_eval_span_captures} // {};
  for my $key (sort keys %$span) {
    next if $seen{$key};
    push @pairs, ['cons', "\"$key\"", $span->{$key}];
  }
  # EVAL MODE (#295, ir-spec §9.1): an eval site inside a compiled eval body
  # appends %p-eval-env% — the lexical holding the ENCLOSING eval's alist,
  # bound once at body entry by _assemble_eval_mode.  Perl's rule is that an
  # eval site sees its whole pad chain, which for eval'd text continues past
  # the text into the scope of the outer eval site; the site alist is that
  # chain's reification, so the outer link must ride the ALIST (lexical, so a
  # named sub the eval defines closes over it and keeps it after the eval
  # returns) — never the dynamic *p-eval-lex-alist* (extent dies with the
  # eval, and an ambient rebind would leak the caller's scope into subs it
  # merely CALLS).  Appended LAST: own let-bound pairs are inner scope.
  if ($parser->can('eval_mode') && $parser->eval_mode) {
    $parser->{_eval_env_used} = 1;
    return '%p-eval-env%' if !@pairs;
    return ['list', 'append', ['list', 'list', @pairs], '%p-eval-env%'];
  }
  return '' if !@pairs;
  # NB: a 'list' HEAD is CLForm's bare-parens marker, so the literal CL
  # (list …) call is spelled with 'list' as the first ELEMENT instead.
  return ['list', 'list', @pairs];
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
        if ($name eq '__PACKAGE__') {
          # Compile-time token: __PACKAGE__->method resolves the invocant to the
          # current package name (same as bareword __PACKAGE__ elsewhere), NOT a
          # class literally named "__PACKAGE__".  Common idiom in module BEGIN
          # blocks, e.g. Math::BigInt::Calc's __PACKAGE__->_base_len(...).
          my $pkg = ($self->environment && $self->environment->current_package)
                      ? $self->environment->current_package : 'main';
          $obj = '"' . $pkg . '"';
        } elsif ($self->environment && $self->environment->is_package($name)) {
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
  } elsif ($self->_is_paren_scalar_base($kids->[0])) {
    # A parenthesised single-value invocant — (EXPR)->method — is a scalar, so
    # generate it in SCALAR context.  Under LIST_CTX (e.g. the call is an
    # argument to another sub) a paren node otherwise renders as (vector ...),
    # making the invocant an array ref → "Can't call method on unblessed
    # reference".  Same fix as the array/hash arrow-deref bases above.
    $obj = $self->_gen_scalar_deref_base($kids->[0]);
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
  # Method args alias through @_ like any user-sub call (defelem, #131);
  # 'argbox' only when the arg itself is an element access.
  my $saved_lvalue_mc = $self->lvalue_context;
  my @args;
  for my $i (2 .. $#$kids) {
    $self->lvalue_context(
      $self->_is_elem_arg($kids->[$i]) ? 'argbox' : $saved_lvalue_mc);
    push @args, $self->gen_node($kids->[$i]);
  }
  $self->lvalue_context($saved_lvalue_mc);

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
  return $self->_ctx_wrap($call, $ctx);
}

# E2 form variant of gen_methodcall.  Same invocant disambiguation, dynamic /
# SUPER:: detection (all AST-level — is_package/has_prototype lookups and node
# ref-type checks, no generated-text inspection except the SUPER:: prefix on a
# STATIC Word method name, which is a bareword and never a converted form here).
# The method child is generated exactly ONCE, in the same position (after the
# invocant, before the args), so gensym/side-effect ordering matches the text
# emitter and byte-parity holds on both pipelines.
sub gen_methodcall_form {
  my ($self, $node, $node_id, $kids) = @_;

  # --- invocant / object ---
  my $obj_node = $self->expr_o->get_a_node($kids->[0]);
  my $obj;
  if ($self->expr_o->is_internal_node_type($obj_node) &&
      $obj_node->{type} eq 'funcall') {
    my $obj_kids = $self->expr_o->get_node_children($kids->[0]);
    if (@$obj_kids == 1) {
      my $class_node = $self->expr_o->get_a_node($obj_kids->[0]);
      if (ref($class_node) eq 'PPI::Token::Word') {
        my $name = $class_node->content();
        if ($name eq '__PACKAGE__') {
          my $pkg = ($self->environment && $self->environment->current_package)
                      ? $self->environment->current_package : 'main';
          $obj = '"' . $pkg . '"';
        } elsif ($self->environment && $self->environment->is_package($name)) {
          $obj = '"' . $name . '"';
        } elsif ($self->environment && $self->environment->has_prototype($name)) {
          $obj = $self->gen_node_form($kids->[0]);
        } else {
          $obj = ['p-resolve-invocant', '"' . $name . '"'];
        }
      } else {
        $obj = $self->gen_node_form($kids->[0]);
      }
    } else {
      $obj = $self->gen_node_form($kids->[0]);
    }
  } elsif ($self->_is_paren_scalar_base($kids->[0])) {
    $obj = $self->_gen_scalar_deref_base_form($kids->[0]);
  } else {
    $obj = $self->gen_node_form($kids->[0]);
  }

  # --- method name (generated once, per dynamic/static branch) ---
  my $method_node = $self->expr_o->get_a_node($kids->[1]);
  my $is_dynamic_method = 0;
  if (ref($method_node) eq 'PPI::Token::Symbol' && $method_node->content() =~ /^\$/) {
    $is_dynamic_method = 1;
  } elsif ($self->expr_o->is_internal_node_type($method_node)) {
    $is_dynamic_method = 1;
  }
  my ($method_form, $method_text);
  if ($is_dynamic_method) {
    $method_form = $self->gen_node_form($kids->[1]);
  } else {
    $method_text = $self->gen_node($kids->[1]);
  }

  # --- arguments ---
  # Method args alias through @_ like any user-sub call (defelem, #131);
  # 'argbox' only when the arg itself is an element access.
  my $saved_lvalue_mc = $self->lvalue_context;
  my @args = map {
    $self->lvalue_context(
      $self->_is_elem_arg($kids->[$_]) ? 'argbox' : $saved_lvalue_mc);
    $self->gen_node_form($kids->[$_]);
  } 2 .. $#$kids;
  $self->lvalue_context($saved_lvalue_mc);

  # --- assemble the call form ---
  my $call;
  if (!$is_dynamic_method && $method_text =~ /^SUPER(?:::|')(.+)$/) {
    my $real_method = $1;
    my $current_pkg = $self->environment ? $self->environment->current_package : 'main';
    (my $rm_str = $real_method) =~ s/"/\\"/g;
    $call = ['p-super-call', $obj, "\"$rm_str\"", "\"$current_pkg\"", @args];
  } elsif ($is_dynamic_method) {
    $call = ['p-method-call', $obj, $method_form, @args];
  } else {
    (my $method_str = $method_text) =~ s/"/\\"/g;
    $call = ['p-method-call', $obj, "\"$method_str\"", @args];
  }

  my $ctx = $self->expr_o->get_node_context($node_id);
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  return $self->_ctx_wrap_form($call, $ctx);
}


# Code ref call: (p-funcall-ref ref args...)
sub gen_ref_funcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is the code reference
  my $ref = $self->gen_node($kids->[0]);

  # Rest are arguments.  A coderef call is always user code — element args
  # alias through @_ (defelem, #131); 'argbox' only when the arg itself is
  # an element access.
  my @args;
  my $saved_lvalue = $self->lvalue_context;
  for my $i (1 .. $#$kids) {
    $self->lvalue_context(
      $self->_is_elem_arg($kids->[$i]) ? 'argbox' : $saved_lvalue);
    push @args, $self->gen_node($kids->[$i]);
  }
  $self->lvalue_context($saved_lvalue);

  my $args_str = @args ? ' ' . join(' ', @args) : '';
  my $call = "(p-funcall-ref $ref$args_str)";

  # Bind *wantarray* so the code-ref body sees the correct call context.
  my $ctx = $self->expr_o->get_node_context($node_id);
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  return $self->_ctx_wrap($call, $ctx);
}

# E2 form variant of gen_ref_funcall.  No operand-text inspection; converts
# fully.  Same ctx-wrap discipline as gen_methodcall_form / gen_funcall_form.
sub gen_ref_funcall_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $ref  = $self->gen_node_form($kids->[0]);
  # A coderef call is always user code — element args alias through @_
  # (defelem, #131), same as the named-user-sub tail in gen_funcall_form;
  # 'argbox' only when the arg itself is an element access.
  my $saved_lvalue = $self->lvalue_context;
  my @args = map {
    $self->lvalue_context(
      $self->_is_elem_arg($kids->[$_]) ? 'argbox' : $saved_lvalue);
    $self->gen_node_form($kids->[$_]);
  } 1 .. $#$kids;
  $self->lvalue_context($saved_lvalue);
  my $call = ['p-funcall-ref', $ref, @args];

  my $ctx = $self->expr_o->get_node_context($node_id);
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  return $self->_ctx_wrap_form($call, $ctx);
}


# Ternary: (p-if cond then else)
# Form-producing (E2-converted; the E2.0 pilot).  Never declines.
sub gen_ternary {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # The ternary's value is its chosen branch, so the branches inherit the
  # ternary's own context (the condition stays boolean/scalar).  Without this a
  # range in a branch — `wantarray ? (1..3) : "x"` in list context — sees a
  # non-list context and is mis-emitted as a flip-flop.
  my $ctx = $self->expr_o->get_node_context($node_id);
  if (defined $ctx) {
    $self->expr_o->set_node_context($kids->[1], $ctx);
    $self->expr_o->set_node_context($kids->[2], $ctx);
  }
  my $cond = $self->gen_node_form($kids->[0]);
  my $then  = $self->gen_node_form($kids->[1]);
  my $else  = $self->gen_node_form($kids->[2]);

  return ['p-if', $cond, $then, $else];
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
      # \&NAME references the sub slot (the user sub), never a builtin — even
      # when NAME happens to be a builtin name (Perl: `\&length` refers to
      # `&main::length`, not the `length` operator). Force the user sub.
      my $cl_func = $self->cl_name($func_name, 1, 1);
      return "(p-backslash-sub '$cl_func)";
    }
    # \&{expr} / \&$var — the coderef itself, not a call: reach past the
    # `&` prefix (which alone would lower to a call with @_) and take the
    # coderef mention.  p-backslash passes functions through unchanged.
    if (defined(my $amp_id = $self->_amp_cast_operand_id($operand_id))) {
      my $saved = $self->lvalue_context;
      $self->lvalue_context(1);
      my $inner = $self->gen_node($amp_id);
      $self->lvalue_context($saved);
      return "(p-backslash (p-get-coderef $inner))";
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
  # /s: a complex first argument (e.g. a do-block lowered to a multiline
  # funcall-lambda) puts newlines in the operand text — without /s the match
  # failed and the \substr silently degraded to a copy (no write-through).
  if ($op eq '\\') {
    if ($operand =~ /^\(p-substr (.+)\)$/s) { return "(p-substr-ref $1)"; }
    if ($operand =~ /^\(p-pos (.+)\)$/s)    { return "(p-pos-ref $1)"; }
    if ($operand =~ /^\(p-vec (.+)\)$/s)    { return "(p-vec-ref $1)"; }
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
  # & Cast: &{expr} / &$var with no argument list is a CALL passing the
  # current @_ (same rule as the leaf-Symbol `&foo;` form).  The parents
  # that want the coderef itself — \, defined, exists, undef, goto — never
  # reach this branch: they intercept via _amp_cast_operand_id and lower
  # the mention themselves ((p-get-coderef expr) and friends).
  elsif ($op eq '&') {
    return "(p-funcall-ref $operand @_)";
  }
  # * Cast: *$var (typeglob ref) — use distinct marker so assignment can detect it.
  # When on LHS of =, becomes (p-glob-assign-dynamic ...).
  # As rvalue, returns the typeglob object.
  elsif ($op eq '*') {
    return "(p-dynamic-typeglob $operand)";
  }

  return "($cl_op $operand)";
}

# E2 form variant of gen_prefix_op.  The `\` backslash family and `++`/`--`
# — whose TEXT emitter regexes the generated operand text to detect magic
# lvalues (\$#array→p-arylen-ref, \substr/\pos/\vec→*-ref twins,
# $#array++→p-set-array-length) — convert by destructuring the operand
# FORM's head instead: same generated output inspected, structurally.  A
# producer that emits a different head (e.g. a user-defined `sub substr` →
# pl-substr) falls to the generic wrap on both paths, so parity holds by
# construction.  Never declines (the last decline — the \(RANGE, …)
# range-mix multi-term — converted via _gen_backslash_multi_term_form).
sub gen_prefix_op_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $op_node = $self->expr_o->get_a_node($kids->[0]);
  my $op      = $op_node->content();

  if ($op eq '\\') {
    my $operand_id   = $kids->[1];
    my $operand_node = $self->expr_o->get_a_node($operand_id);
    # \&NAME — the sub slot, never a builtin; force the user sub.
    if (ref($operand_node) eq 'PPI::Token::Symbol'
        && $operand_node->content() =~ /^&(.+)$/) {
      my $cl_func = $self->cl_name($1, 1, 1);
      return ['p-backslash-sub', "'$cl_func"];
    }
    # \&{expr} / \&$var — the coderef itself, not a call (mirrors the text
    # emitter's intercept above the `&`-prefix call lowering).
    if (defined(my $amp_id = $self->_amp_cast_operand_id($operand_id))) {
      my $saved = $self->lvalue_context;
      $self->lvalue_context(1);
      my $inner = $self->gen_node_form($amp_id);
      $self->lvalue_context($saved);
      return ['p-backslash', ['p-get-coderef', $inner]];
    }
    # \(LIST) — the distribute-over-elements family.  Mirrors the text
    # emitter branch for branch: single-scalar tree_val → (p-backslash …),
    # multi-term comma list (with or without ranges) →
    # _gen_backslash_multi_term_form, general list → (p-refgen-list …)
    # [+ (p-list-scalar …) in scalar/void ctx].
    if ($self->expr_o->node_tree->get_metadata($operand_id, 'backslash_paren_list')) {
      my $inner_node = $self->expr_o->get_a_node($operand_id);
      if ($self->expr_o->is_internal_node_type($inner_node)
          && ($inner_node->{type} // '') eq 'tree_val') {
        my $tv_kids = $self->expr_o->get_node_children($operand_id);
        if (@$tv_kids == 1 && !$self->_is_list_node_for_refgen($tv_kids->[0])) {
          # Single scalar child: \(scalar_expr) == \scalar_expr
          my $saved_ctx = $self->expr_o->get_node_context($operand_id);
          $self->expr_o->set_node_context($operand_id, 0);
          my $scalar_form = $self->gen_node_form($operand_id);
          $self->expr_o->set_node_context($operand_id, $saved_ctx);
          return ['p-backslash', $scalar_form];
        }
        if (@$tv_kids > 1) {
          return $self->_gen_backslash_multi_term_form($tv_kids);
        }
      }
      my $raw_ctx   = $self->expr_o->get_node_context_raw($node_id);
      my $saved_ctx = $self->expr_o->get_node_context($node_id);
      $self->expr_o->set_node_context($operand_id, LIST_CTX);
      my $list_form = $self->gen_node_form($operand_id);
      $self->expr_o->set_node_context($operand_id, $saved_ctx);
      # \(LIST) is a list operator: in explicit scalar/void context it yields
      # a ref to the LAST element (comma-operator semantics); otherwise the
      # full vector.
      if (defined $raw_ctx && ($raw_ctx == SCALAR_CTX || $raw_ctx == VOID_CTX)) {
        return ['p-list-scalar', ['p-refgen-list', $list_form]];
      }
      return ['p-refgen-list', $list_form];
    }
  }

  if ($op eq '\\' || $op eq '++' || $op eq '--') {
    # l-value context: \ needs the box (a ref to it, not a copy), ++/--
    # need writable element boxes — mirrors the text emitter exactly.
    my $saved_lvalue = $self->lvalue_context;
    $self->lvalue_context(1);
    my $operand = $self->gen_node_form($kids->[1]);
    $self->lvalue_context($saved_lvalue);
    my $head = (ref($operand) eq 'ARRAY' && !ref($operand->[0]))
             ? $operand->[0] : '';
    my @rest = ref($operand) eq 'ARRAY' ? @$operand[1 .. $#$operand] : ();
    if ($op eq '\\') {
      # magic-lvalue refs: live cells with write-through setters
      return ['p-arylen-ref', @rest] if $head eq 'p-array-last-index';
      return [$head . '-ref', @rest]
        if $head eq 'p-substr' || $head eq 'p-pos' || $head eq 'p-vec';
      return [$self->cl_name($op), $operand];
    }
    # prefix ++/--: arylen target gets the setter shape (value = new length)
    if ($head eq 'p-array-last-index') {
      my $delta_op = ($op eq '++') ? '1+' : '1-';
      return ['p-set-array-length', @rest,
              [$delta_op, ['p-array-last-index', @rest]]];
    }
    return ["p-pre$op", $operand];
  }

  # $#{ array } — last index of array (braced form of $#array).
  if ($op eq '$#') {
    return ['p-array-last-index', $self->gen_node_form($kids->[1])];
  }

  # ${expr}++ / @{expr}-- shunting-yard fixup: prefix_op($, postfix_op(X, ++))
  # → (p-post++ (p-cast-$ X)).  Operand read in rvalue context (the ref value,
  # not the box).
  if ($op eq '$' || $op eq '@' || $op eq '%') {
    my $inner_id   = $kids->[1];
    my $inner_node = $self->expr_o->get_a_node($inner_id);
    if ($self->expr_o->is_internal_node_type($inner_node)
        && $inner_node->{type} eq 'postfix_op') {
      my $po_kids    = $self->expr_o->get_node_children($inner_id);
      my $po_op_node = $self->expr_o->get_a_node($po_kids->[1]);
      my $po_op      = $po_op_node->content();
      if ($po_op eq '++' || $po_op eq '--') {
        my $saved = $self->lvalue_context;
        $self->lvalue_context(0);
        my $inner_expr = $self->gen_node_form($po_kids->[0]);
        $self->lvalue_context($saved);
        return ["p-post$po_op", ["p-cast-$op", $inner_expr]];
      }
    }
    # else: fall through to the general sigil-cast handling below.
  }

  # Unary + — a pure no-op disambiguator: pass the operand through unchanged,
  # inheriting our context.  `+(EXPR)` unwraps a single-child tree_val.
  if ($op eq '+') {
    my $operand_id = $kids->[1];
    my $my_ctx     = defined $node_id
                     ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;
    my $on = $self->expr_o->get_a_node($operand_id);
    if ($self->expr_o->is_internal_node_type($on)
        && ($on->{type} // '') eq 'tree_val') {
      my $tv_kids = $self->expr_o->get_node_children($operand_id);
      $operand_id = $tv_kids->[0] if @$tv_kids == 1;
    }
    my $saved = $self->expr_o->get_node_context($operand_id);
    $self->expr_o->set_node_context($operand_id, $my_ctx);
    my $inner = $self->gen_node_form($operand_id);
    $self->expr_o->set_node_context($operand_id, $saved);
    return $inner;
  }

  # @ needs lvalue context so subscripts return boxes → p-cast-@ can autoviv.
  # (++/--/\ already declined above.)
  my $needs_lvalue = ($op eq '@');
  my $saved_lvalue = $self->lvalue_context;
  $self->lvalue_context(1) if $needs_lvalue;
  my $operand = $self->gen_node_form($kids->[1]);
  $self->lvalue_context($saved_lvalue);

  my $cl_op = $self->cl_name($op);

  # Under 'use integer', ~ returns signed 64-bit complement.
  if ($op eq '~' && $self->environment && $self->environment->has_pragma('use_integer')) {
    return ['p-to-s64', ['lognot', ['pcl::%pcl-to-integer', ['to-number', $operand]]]];
  }
  # Sigil cast operators (dereference).
  if ($op eq '@' || $op eq '%' || $op eq '$') {
    return ["p-cast-$op", $operand];
  }
  # & Cast: &{expr} / &$var with no argument list — a CALL with the current
  # @_ (the coderef-mention parents intercept before this, as in the text
  # emitter).
  if ($op eq '&') {
    return ['p-funcall-ref', $operand, '@_'];
  }
  # * Cast: *$var — typeglob ref (distinct marker for lvalue detection).
  if ($op eq '*') {
    return ['p-dynamic-typeglob', $operand];
  }

  return [$cl_op, $operand];
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

# E2 form variant of gen_postfix_op — FULL coverage: the chained-comparison
# container, plain ++/--/other postfix ops, and the `$#array++` arylen
# setter (keyed on the generated operand form's head, like
# gen_prefix_op_form's magic-lvalue routing).
sub gen_postfix_op_form {
  my ($self, $node, $node_id, $kids) = @_;

  # Chained comparison ($x < $y < $z): odd kids >= 5, term/op/term/…/term.
  if (scalar(@$kids) >= 5 && scalar(@$kids) % 2 == 1) {
    my @parts;
    for my $i (0 .. $#$kids) {
      if ($i % 2 == 0) {
        push @parts, $self->gen_node_form($kids->[$i]);                   # term
      } else {
        push @parts, "'" . $self->expr_o->get_a_node($kids->[$i])->content();  # 'op
      }
    }
    return ['p-chain-cmp', @parts];
  }

  my $op_node = $self->expr_o->get_a_node($kids->[1]);
  my $op      = $op_node->content();

  my $needs_lvalue = ($op eq '++' || $op eq '--');
  my $saved_lvalue = $self->lvalue_context;
  $self->lvalue_context(1) if $needs_lvalue;
  my $operand = $self->gen_node_form($kids->[0]);
  $self->lvalue_context($saved_lvalue);

  # $#array++ / $#array-- : setter shape returning the OLD length — the
  # form twin of the text emitter's operand-text arylen check, keyed on
  # the generated form's head (see gen_prefix_op_form).
  if (($op eq '++' || $op eq '--')
      && ref($operand) eq 'ARRAY' && !ref($operand->[0])
      && $operand->[0] eq 'p-array-last-index') {
    my @arr = @$operand[1 .. $#$operand];
    my $delta_op = ($op eq '++') ? '1+' : '1-';
    return ['let', ['list', ['list', '_prev', ['p-array-last-index', @arr]]],
            ['p-set-array-length', @arr, [$delta_op, '_prev']],
            '_prev'];
  }

  my $cl_op = ($op eq '++' || $op eq '--')
            ? "p-post$op"
            : $self->cl_name($op) . '-post';
  return [$cl_op, $operand];
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
    $arr = _swap_elem_sigil($arr, q(@));
  }

  # Numeric-named arrays like @0, @1 are not valid Perl identifiers.
  # $0[n] parses as @0[n] but @0 is never a real variable; return undef.
  return '(p-undef)' if $arr =~ /^@\d+$/;

  # Punctuation-named `#` array (@#) comes from `$#[idx]` — the removed `$#`
  # magic taking a subscript, which Perl reads as element idx of @#.  Its name
  # is not a word char, so the forward-declaration scan misses it; register it
  # so a file-level defvar is emitted (undef/empty, not an unbound crash).
  $self->environment->register_punct_global($arr)
    if $self->environment && $arr eq '@#';

  # Apply rename map for @varname (closure/state variable captures)
  if ($self->environment) {
    my $renames = $self->environment->state_var_renames;
    $arr = $renames->{$arr} if $renames && exists $renames->{$arr};
  }

  # Use p-aref-box in l-value context for modifying operations
  my $func = $self->_elem_accessor('p-aref');
  return "($func $arr $idx)";
}

# E2 form variant of gen_array_access.  Same shape/side effects as the text
# emitter; generation ORDER preserved (container then index).  The container
# is a text-atom string only for a bare variable (so the sigil rewrite and the
# @N / @# / rename string ops apply); a nested container is a structural form.
sub gen_array_access_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $arr_node = $self->expr_o->get_a_node($kids->[0]);
  my $is_bare = ref($arr_node) eq 'PPI::Token::Symbol'
             || ref($arr_node) eq 'PPI::Token::Magic';
  my $arr = $is_bare ? $self->gen_node($kids->[0]) : $self->gen_node_form($kids->[0]);
  my $idx = $self->gen_node_form($kids->[1]);
  if ($is_bare) {
    $arr = _swap_elem_sigil($arr, q(@));
    return ['p-undef'] if $arr =~ /^@\d+$/;
    $self->environment->register_punct_global($arr)
      if $self->environment && $arr eq '@#';
    if ($self->environment) {
      my $renames = $self->environment->state_var_renames;
      $arr = $renames->{$arr} if $renames && exists $renames->{$arr};
    }
  }
  my $func = $self->_elem_accessor('p-aref');
  return [$func, $arr, $idx];
}


# Hash access: (p-gethash hash key) or (p-gethash-box hash key) in l-value context
# In Perl, $hash{key} accesses %hash, so we convert $sigil to %sigil
# Perl's element-access sigil swap: $h{k} reads %h, $a[i] reads @a.  The
# rendered symbol may be pipe-quoted (|$-|, |$^H|) when the name needs CL
# escaping — swap inside the pipes, keeping them (harmless when unneeded).
# Package-qualified renders (|Pkg|::$h) hit the first branch via `::$`.
# Package function (not a method): render a package-qualified Perl variable
# name in CL order — `$Pkg::var` -> `Pkg::$var`, `$::foo` -> `main::$foo`,
# nested `$A::B::x` -> `|A::B|::$x`.  Unqualified names (and stash refs like
# `$Pkg::Sub::`, excluded by the [^:]+ tail) return unchanged.  Registers the
# package with ENV (optional) so the preamble pre-declares it.  Used by
# gen_symbol_form and by both pipelines' loop-variable bindings, which take
# the name from raw token content (a raw `$main::x` binding is UNREADABLE —
# the CL reader parses `$MAIN` as a package; comp/require.t s309).
sub qualified_var_to_cl {
  my ($name_in, $env) = @_;
  if ($name_in =~ /^([\$\@\%])(.*)::([^:]+)$/) {
    my ($sigil, $pkg, $name) = ($1, $2, $3);
    $pkg = 'main' if $pkg eq '';
    $env->add_referenced_package($pkg) if $env;
    my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
    return "${cl_pkg}::${sigil}${name}";
  }
  return $name_in;
}

sub _swap_elem_sigil {
  my ($sym, $sigil) = @_;
  $sym =~ s/(^|::)\$/$1$sigil/
    or $sym =~ s/^\|\$(.*)\|$/|$sigil$1|/;
  return $sym;
}

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
    $hash = _swap_elem_sigil($hash, q(%));
  }

  # Punctuation-named `#` hash (%#) from `$#{key}` — see gen_array_access.
  $self->environment->register_punct_global($hash)
    if $self->environment && $hash eq '%#';

  # Apply rename map for %varname (closure/state variable captures)
  if ($self->environment) {
    my $renames = $self->environment->state_var_renames;
    $hash = $renames->{$hash} if $renames && exists $renames->{$hash};
  }

  # Use p-gethash-box in l-value context for modifying operations
  my $func = $self->_elem_accessor('p-gethash');
  return "($func $hash $key)";
}


# True when NODE is a parenthesised single-value base — a tree_val or progn with
# exactly one child, e.g. the `($r//0)` in `($r//0)->[i]`. Such a base is a
# scalar REF being dereferenced, not a list.
sub _is_paren_scalar_base {
  my ($self, $node_id) = @_;
  my $node = $self->expr_o->get_a_node($node_id);
  return 0 unless $self->expr_o->is_internal_node_type($node);
  my $type = $node->{type} // '';
  return 0 unless $type eq 'tree_val' || $type eq 'progn';
  my $kids = $self->expr_o->get_node_children($node_id);
  return $kids && @$kids == 1;
}

# Generate a parenthesised arrow-deref base in SCALAR context, so a single-child
# tree_val/progn renders as the bare scalar ref rather than (vector ...) (which
# happens under LIST_CTX — gen_tree_val — or lvalue — gen_progn). The base is a
# scalar REF that is READ to obtain the pointer; autoviv of the indexed slot is
# still performed by the -box runtime fn at the call site.
sub _gen_scalar_deref_base {
  my ($self, $base_id) = @_;
  my $saved_ctx = $self->expr_o->get_node_context($base_id);
  $self->expr_o->set_node_context($base_id, 0);   # SCALAR_CTX
  my $saved_lv = $self->lvalue_context;
  $self->lvalue_context(0);
  my $cl = $self->gen_node($base_id);
  $self->lvalue_context($saved_lv);
  $self->expr_o->set_node_context($base_id, $saved_ctx);
  return $cl;
}

# E2 form twin: same scalar-context/lvalue dance, structural child.
sub _gen_scalar_deref_base_form {
  my ($self, $base_id) = @_;
  my $saved_ctx = $self->expr_o->get_node_context($base_id);
  $self->expr_o->set_node_context($base_id, 0);   # SCALAR_CTX
  my $saved_lv = $self->lvalue_context;
  $self->lvalue_context(0);
  my $f = $self->gen_node_form($base_id);
  $self->lvalue_context($saved_lv);
  $self->expr_o->set_node_context($base_id, $saved_ctx);
  return $f;
}

# E2 form variant of gen_hash_access.  Container then key (order preserved);
# multi-key $h{a,b,c} → (p-join |$;| (vector …)); bare-var sigil rewrite /
# %# register / rename as strings, nested container structural.
sub gen_hash_access_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $hash_node = $self->expr_o->get_a_node($kids->[0]);
  my $is_bare = ref($hash_node) eq 'PPI::Token::Symbol'
             || ref($hash_node) eq 'PPI::Token::Magic';
  my $hash = $is_bare ? $self->gen_node($kids->[0]) : $self->gen_node_form($kids->[0]);

  my $key_node = $self->expr_o->get_a_node($kids->[1]);
  my $key;
  if ($self->expr_o->is_internal_node_type($key_node)
      && $key_node->{type} eq 'progn') {
    my $key_kids = $self->expr_o->get_node_children($kids->[1]);
    if (@$key_kids > 1) {
      my @parts = map { $self->gen_node_form($_) } @$key_kids;
      $key = ['p-join', '|$;|', ['vector', @parts]];
    } else {
      $key = $self->gen_node_form($kids->[1]);
    }
  } else {
    $key = $self->gen_node_form($kids->[1]);
  }

  if ($is_bare) {
    $hash = _swap_elem_sigil($hash, q(%));
    $self->environment->register_punct_global($hash)
      if $self->environment && $hash eq '%#';
    if ($self->environment) {
      my $renames = $self->environment->state_var_renames;
      $hash = $renames->{$hash} if $renames && exists $renames->{$hash};
    }
  }
  my $func = $self->_elem_accessor('p-gethash');
  return [$func, $hash, $key];
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
  # A parenthesised single-value base — ($r//0)->[N] — is a scalar ref, NOT a
  # list subscript. Without this, the base is forced to LIST_CTX/lvalue and
  # renders as (vector ...), so `($r//0)->[i]{k}...=v` autovivified into a bogus
  # 1-element vector (p-autoviv-aref-for-hash TYPE-ERROR, multideref.t).
  my $paren_scalar_base = !$is_list_subscript && $self->_is_paren_scalar_base($kids->[0]);
  if (!$paren_scalar_base
      && ($is_list_subscript
          || ($self->expr_o->is_internal_node_type($child0_node)
              && $child0_node->{type} eq 'progn'))) {
    $self->expr_o->set_node_context($kids->[0], 1);  # LIST_CTX = 1
  }

  my $ref = $paren_scalar_base
            ? $self->_gen_scalar_deref_base($kids->[0])
            : $self->gen_node($kids->[0]);
  my $idx = $self->gen_node($kids->[1]);

  # In l-value context (e.g. \$ref->[i], or a modifying op) return the LIVE box
  # at the slot so a reference tracks later writes (stacked Moo `around` relies on
  # \$cache->{wrapped} seeing reassignment).  Plain reads stay snapshot-valued.
  my $func = $self->lvalue_context ? 'p-aref-deref-box' : 'p-aref-deref';
  return "($func $ref $idx)";
}


# Hash ref access: (p-gethash-deref ref key)
sub gen_hash_ref_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # A parenthesised scalar base — ($r//0)->{k} — is a scalar ref, so generate it
  # in scalar context rather than letting it render as (vector ...). (Hash-ref
  # access has no list-subscript form, so a single-value paren base here is always
  # a scalar arrow-deref base.)
  my $ref = $self->_is_paren_scalar_base($kids->[0])
            ? $self->_gen_scalar_deref_base($kids->[0])
            : $self->gen_node($kids->[0]);

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

  # L-value context (\$ref->{k}, modifying ops): return the LIVE box at the slot
  # so a reference tracks later writes (see gen_array_ref_access).
  my $func = $self->lvalue_context ? 'p-gethash-deref-box' : 'p-gethash-deref';
  return "($func $ref $key)";
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

# E2 form variant of _slice_in_context (same context rule, CLForm in/out).
sub _slice_in_context_form {
  my ($self, $slice_form, $node_id) = @_;
  my $ctx = defined $node_id ? $self->expr_o->get_node_context_raw($node_id) : undef;
  return $slice_form unless defined $ctx;
  return ['p-list-scalar',  $slice_form] if $ctx == SCALAR_CTX;
  return ['p-slice-result', $slice_form] if $ctx == INHERIT_CTX;
  return $slice_form;
}

# --- E2 form variants of the access / slice family --------------------------

# $ref->[i] → (p-aref-deref ref i) / (p-aref-deref-box …) in lvalue context.
sub gen_array_ref_access_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $is_list_subscript =
      $self->expr_o->node_tree->get_metadata($node_id, 'list_ctx_subscript');
  my $child0_node = $self->expr_o->get_a_node($kids->[0]);
  my $paren_scalar_base =
      !$is_list_subscript && $self->_is_paren_scalar_base($kids->[0]);
  if (!$paren_scalar_base
      && ($is_list_subscript
          || ($self->expr_o->is_internal_node_type($child0_node)
              && $child0_node->{type} eq 'progn'))) {
    $self->expr_o->set_node_context($kids->[0], 1);
  }
  my $ref = $paren_scalar_base
            ? $self->_gen_scalar_deref_base_form($kids->[0])
            : $self->gen_node_form($kids->[0]);
  my $idx = $self->gen_node_form($kids->[1]);
  my $func = $self->lvalue_context ? 'p-aref-deref-box' : 'p-aref-deref';
  return [$func, $ref, $idx];
}

# $ref->{k} → (p-gethash-deref ref k) / (p-gethash-deref-box …); multi-key
# $ref->{a,b} → (p-join |$;| (vector …)).
sub gen_hash_ref_access_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $ref = $self->_is_paren_scalar_base($kids->[0])
            ? $self->_gen_scalar_deref_base_form($kids->[0])
            : $self->gen_node_form($kids->[0]);
  my $key_node = $self->expr_o->get_a_node($kids->[1]);
  my $key;
  if ($self->expr_o->is_internal_node_type($key_node)
      && $key_node->{type} eq 'progn') {
    my $key_kids = $self->expr_o->get_node_children($kids->[1]);
    if (@$key_kids > 1) {
      my @parts = map { $self->gen_node_form($_) } @$key_kids;
      $key = ['p-join', '|$;|', ['vector', @parts]];
    } else {
      $key = $self->gen_node_form($kids->[1]);
    }
  } else {
    $key = $self->gen_node_form($kids->[1]);
  }
  my $func = $self->lvalue_context ? 'p-gethash-deref-box' : 'p-gethash-deref';
  return [$func, $ref, $key];
}

# @a[i,j] → (p-aslice @a i j), context-wrapped.
sub gen_array_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  my $arr = $self->gen_node_form($kids->[0]);
  my @indices;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @indices, $self->gen_node_form($kids->[$i]);
  }
  return $self->_slice_in_context_form(['p-aslice', $arr, @indices], $node_id);
}

# @h{a,b} → (p-hslice %h a b), context-wrapped (bare Symbol @→% sigil rewrite).
sub gen_hash_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  my $hash_node = $self->expr_o->get_a_node($kids->[0]);
  my $is_bare = ref($hash_node) eq 'PPI::Token::Symbol';
  my $hash = $is_bare ? $self->gen_node($kids->[0]) : $self->gen_node_form($kids->[0]);
  if ($is_bare && $hash =~ /(?:^|::)\@/) {
    $hash =~ s/(^|::)\@/${1}%/;
  }
  my @keys;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @keys, $self->gen_node_form($kids->[$i]);
  }
  return $self->_slice_in_context_form(['p-hslice', $hash, @keys], $node_id);
}

# %h{a,b} → (p-kv-hslice %h a b)  (no context wrap).
sub gen_kv_hash_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  my $hash = $self->gen_node_form($kids->[0]);
  my @keys;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @keys, $self->gen_node_form($kids->[$i]);
  }
  return ['p-kv-hslice', $hash, @keys];
}

# %a[i,j] → (p-kv-aslice @a i j)  (%→@ sigil rewrite; $ref base → (unbox …)).
sub gen_kv_array_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  my $arr = $self->gen_node($kids->[0]);
  $arr =~ s/(^|::)\%/${1}\@/;
  my $arr_form = $arr =~ /^\$/ ? ['unbox', $arr] : $arr;
  my @indices;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @indices, $self->gen_node_form($kids->[$i]);
  }
  return ['p-kv-aslice', $arr_form, @indices];
}

# --- E2 form variants: progn + the small I/O nodes --------------------------

# Comma/list expression.  AST-level classification (_node_is_definitely_scalar
# / _is_array_expr_node), no generated-text inspection.  EMPTY () declines: the
# text emitter's "(vector )" / "(progn )" carry a trailing space a form cannot
# reproduce.
sub gen_progn_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $ctx = $self->expr_o->get_node_context($node_id);
  # Empty comma-list: same shapes as gen_tree_val_form's empty () — text
  # twin's trailing space normalized away (task #78 E2.final).
  return $ctx == 1 ? ['vector'] : ['progn'] unless @$kids;
  if ($ctx == 1) {  # LIST_CTX
    $self->expr_o->set_node_context($_, 1) for @$kids;
  }
  my @forms = map { $self->gen_node_form($_) } @$kids;
  if ($ctx == 1) {
    my $all_scalar = 1;
    for my $kid_id (@$kids) {
      unless ($self->_node_is_definitely_scalar($kid_id)) { $all_scalar = 0; last }
    }
    # NB: a real CL (list …) call — 'list' is CLForm's RESERVED headless-list
    # head, so emit it as a headless list whose first atom is the symbol `list`.
    return $all_scalar ? ['vector', @forms]
                       : ['p-flatten-args', ['list', 'list', @forms]];
  }
  if (@forms > 1 && ($ctx == VOID_CTX || $ctx == INHERIT_CTX)) {
    my @flat;
    for my $i (0 .. $#$kids) {
      my $kid_node = $self->expr_o->get_a_node($kids->[$i]);
      push @flat, $self->_is_array_expr_node($kid_node, $kids->[$i])
                  ? ['p-flatten', $forms[$i]] : $forms[$i];
    }
    return ['if', '*wantarray*', ['vector', @flat], ['progn', @forms]];
  }
  return ['progn', @forms];
}

sub gen_backtick_form {
  my ($self, $node, $node_id, $kids) = @_;
  return ['p-backtick', $self->gen_node_form($kids->[0])];
}

# <FH> / <$fh> / <> — wantarray-sensitive, context-bound like the text emitter.
sub gen_readline_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $call;
  if (@$kids) {
    my $fh = $self->gen_node($kids->[0]);
    $call = ['p-readline', ($fh =~ /^[A-Za-z_][A-Za-z0-9_]*$/) ? "'$fh" : $fh];
  } else {
    $call = ['p-readline'];
  }
  my $ctx = defined $node_id
            ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;
  return $self->_wrap_wantarray_ctx_form($call, $ctx);
}

# Filehandle marker for the print/say/printf family: :fh 'NAME / :fh $fh / :fh
# nil (a bare marker string, rendered verbatim inside the parent call form).
sub gen_filehandle_form {
  my ($self, $node, $node_id, $kids) = @_;
  if (@$kids) {
    my $fh = $self->gen_node($kids->[0]);
    return ($fh =~ /^[A-Za-z_][A-Za-z0-9_]*$/) ? ":fh '$fh" : ":fh $fh";
  }
  return ':fh nil';
}

# *glob{SLOT} → (p-glob-slot glob "SLOT") or computed (p-glob-slot glob EXPR).
sub gen_glob_slot_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $glob = $self->gen_node_form($kids->[0]);
  if ($node->{slot_is_expr}) {
    return ['p-glob-slot', $glob, $self->gen_node_form($kids->[1])];
  }
  my $slot_name = uc($node->{slot_name} // 'SCALAR');
  return ['p-glob-slot', $glob, "\"$slot_name\""];
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
    # Perl flattens a parenthesised list: any element that yields a list (an
    # @array, a range 1..3, a list-returning call like reverse/map/sort, a
    # nested (...), ...) spreads into the outer list.  A plain (vector ...) does
    # NOT flatten, so e.g. (1..3, 9) wrongly nests the range as one element.
    # Flatten at RUNTIME via p-flatten-args, which spreads any raw vector and
    # keeps scalars/refs/blessed as-is — so it is correct for EVERY list-valued
    # element without enumerating node types.  Keep the fast plain-vector path
    # only when every child is a provably-scalar leaf (number/string/$scalar),
    # which covers the common (1,2,3)/($x,$y) literal-list case.
    my $all_scalar = 1;
    for my $kid_id (@$kids) {
      unless ($self->_node_is_definitely_scalar($kid_id)) {
        $all_scalar = 0;
        last;
      }
    }
    return $all_scalar
        ? "(vector $forms_str)"
        : "(p-flatten-args (list $forms_str))";
  }

  # In VOID/INHERIT context with multiple forms, check wantarray at runtime.
  # The runtime check handles map blocks (whose body is compiled in VOID_CTX
  # but whose lambda runs with *wantarray* t) and caller-dependent positions.
  # Statically-annotated SCALAR_CTX must NOT defer to the dynamic *wantarray*:
  # a proven-scalar operand position (the LHS of and/or, an if condition, a
  # cmpchain comparison operand) stays the comma operator (progn) even when
  # the dynamic *wantarray* happens to be t — same contract as gen_tree_val.
  # Wrap @array items with (p-flatten ...) so %p-collect-list in
  # %p-flatten-for-list can spread @arrays while keeping arrayrefs as scalars.
  if (@forms > 1 && ($ctx == VOID_CTX || $ctx == INHERIT_CTX)) {
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

# Returns true only when a node is PROVABLY a single scalar (a number/string
# literal or a $-sigiled scalar/element).  Used to decide whether a list literal
# needs runtime flattening: a false answer just means "flatten to be safe"
# (always correct, slightly slower), so this must NEVER return true for anything
# that could yield a list at runtime.
sub _node_is_definitely_scalar {
  my ($self, $node_id) = @_;
  my $node = $self->expr_o->get_a_node($node_id);
  # Internal (non-leaf) nodes can produce lists (ranges, calls, nested (...),
  # ternaries, …) — treat them all as "not provably scalar".
  return 0 if $self->expr_o->is_internal_node_type($node);
  my $ref = ref($node);
  return 1 if $ref eq 'PPI::Token::Number'
           || $ref =~ /^PPI::Token::Quote\b/;       # 'str', "str"
  if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
    return substr($node->content() // '', 0, 1) eq '$';  # $x, $a[0], $h{k}, $_
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

# Form twin of _gen_backslash_multi_term (E2): same parts walk, same counter
# id, same token stream — but a CLForm, so the range-mix multi-term no longer
# declines to the text path.  Layout is the printer's (flat under to_flat,
# depth-indented under to_string), not the text twin's hand-rolled multiline
# let — the one deliberate byte change of the conversion.
sub _gen_backslash_multi_term_form {
  my ($self, $tv_kids) = @_;
  my $id = $g_refgen_count++;

  my @parts;  # each is ['single', FORM] or ['range', FORM]
  for my $kid_id (@$tv_kids) {
    my $kid_node = $self->expr_o->get_a_node($kid_id);
    my $is_range = ref($kid_node) eq 'PPI::Token::Operator'
                && ($kid_node->content() // '') eq '..';
    if ($is_range) {
      my $saved = $self->expr_o->get_node_context($kid_id);
      $self->expr_o->set_node_context($kid_id, LIST_CTX);
      my $kid_form = $self->gen_node_form($kid_id);
      $self->expr_o->set_node_context($kid_id, $saved);
      push @parts, ['range', ['p-refgen-list', $kid_form]];
    } else {
      push @parts, ['single', ['p-backslash', $self->gen_node_form($kid_id)]];
    }
  }

  my $has_range = grep { $_->[0] eq 'range' } @parts;
  if (!$has_range) {
    return ['vector', map { $_->[1] } @parts];
  }

  my $result_var = "|--pcl-bsl-r$id--|";
  my $iter_var   = "|--pcl-bsl-x$id--|";
  my @stmts;
  for my $part (@parts) {
    if ($part->[0] eq 'range') {
      push @stmts, ['loop', 'for', $iter_var, 'across', $part->[1],
                    'do', ['vector-push-extend', $iter_var, $result_var]];
    } else {
      push @stmts, ['vector-push-extend', $part->[1], $result_var];
    }
  }
  return ['let',
          ['list', ['list', $result_var,
                    ['make-array', '4', ':adjustable', 't', ':fill-pointer', '0']]],
          @stmts,
          $result_var];
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
    # Propagate context to the child so split/funcs return lists not scalars,
    # and a range (..) emits its runtime wantarray check rather than defaulting
    # to flip-flop.  A single-child (EXPR) is transparent: the child shares the
    # paren's context (LIST, or INHERIT for a ternary branch in a sub tail).
    if ($ctx == LIST_CTX || $ctx == INHERIT_CTX) {
      $self->expr_o->set_node_context($kids->[0], $ctx);
    }
    # Check at AST level (before codegen) whether child is a list-returning expr.
    # If so, we must NOT wrap it in (vector ...) — the child already returns a
    # vector.  Use the range-aware predicate so a single paren'd range like
    # (10..12) (e.g. the RHS of @a[..] = (10..12)) is not nested as one element.
    my $child_is_list = ($ctx == 1) && $self->_is_list_node_for_refgen($kids->[0]);
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

# E2 form variant of gen_tree_val.  Mirrors the text emitter branch-for-branch.
# The one text inspection — `$child =~ /\(p-=~\s/` in the single-child
# list-context branch (a regex match already returns captures in list context,
# so it is NOT re-wrapped in (vector …)) — is reproduced BYTE-EXACTLY via
# to_flat($child): the E2 invariant guarantees to_flat(gen_node_form(x)) equals
# gen_node(x) (corpus-verified every step), so grepping the flat rendering is
# identical to the text emitter's grep of $child.  (A pure AST walk cannot be
# sound here: a nested inline_lambda embeds a pre-generated `body_cl` string
# that may itself contain (p-=~; to_flat sees it, an AST walk would not.
# Structuring regex emission is a separate roadmap item — until then this
# single grep is the faithful bridge.)  Empty () now emits (vector)/(progn)
# — the text twin's trailing space, normalized away (task #78 E2.final).
sub gen_tree_val_form {
  my ($self, $node, $node_id, $kids) = @_;

  my $ctx = $self->expr_o->get_node_context($node_id);

  # Empty (): (vector) in list context, (progn) otherwise — the text twin's
  # "(vector )"/"(progn )" trailing space normalized away (task #78 E2.final).
  return $ctx == LIST_CTX ? ['vector'] : ['progn'] unless @$kids;

  if (scalar(@$kids) == 1) {
    if ($ctx == LIST_CTX || $ctx == INHERIT_CTX) {
      $self->expr_o->set_node_context($kids->[0], $ctx);
    }
    my $child_is_list = ($ctx == LIST_CTX) && $self->_is_list_node_for_refgen($kids->[0]);
    my $child = $self->gen_node_form($kids->[0]);
    if ($ctx == LIST_CTX) {
      if (Pl::CLForm::to_flat($child) =~ /\(p-=~\s/) {
        return ['let', ['list', ['list', '*wantarray*', 't']], $child];
      }
      return $child_is_list ? $child : ['vector', $child];
    }
    return $child;
  }

  my @forms = map { $self->gen_node_form($_) } @$kids;
  if ($ctx == LIST_CTX) {
    return ['vector', @forms];
  }
  if (@forms > 1 && $ctx == INHERIT_CTX) {
    # headless-list idiom ['list','list',…] → (list …); progn head is literal.
    return ['if', ['eq', '*wantarray*', 't'],
                  ['p-flatten-args', ['list', 'list', @forms]],
                  ['progn', @forms]];
  }
  return ['progn', @forms];
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
    # Quote bareword filehandles (FH, STDOUT, foo, etc.) so CL doesn't try to
    # evaluate them.  Barewords are identifiers without sigils or parens —
    # Perl allows any-case handles (`open(foo,...); print foo LIST`), so this
    # covers both all-caps and lower/mixed-case registered handles.  A `$fh`
    # variable or a parenthesised expression is left to evaluate as-is.
    if ($fh =~ /^[A-Za-z_][A-Za-z0-9_]*$/) {
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
  my $call;
  if (@$kids) {
    my $fh = $self->gen_node($kids->[0]);
    # Quote bareword filehandles (any word not starting with $ or ( )
    $call = ($fh =~ /^[A-Za-z_][A-Za-z0-9_]*$/)
            ? "(p-readline '$fh)"
            : "(p-readline $fh)";
  } else {
    # Empty <> reads from ARGV or STDIN
    $call = "(p-readline)";
  }

  # <FH> is wantarray-sensitive (list context reads all records, scalar reads
  # one), exactly like reverse/unpack in gen_funcall.  Bind *wantarray* for the
  # node's annotated context so the enclosing sub's context can't leak in — e.g.
  # `my $a = <FH>` inside a sub called in list context must still read ONE line.
  # (The p-readline macro's *p-in-list-assign-rhs* guard still wins for
  # `while (($x) = <FH>)`.)
  my $ctx = defined $node_id
            ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;
  return $self->_wrap_wantarray_ctx($call, $ctx);
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

  # The file glob <pat> is wantarray-sensitive (list returns all matches, scalar
  # iterates one per call), so bind *wantarray* to the node's static context the
  # same way gen_readline / gen_funcall do — otherwise the enclosing sub's
  # context leaks in (`my $first = <*.c>` inside a list-context sub).
  my $ctx = defined $node_id
            ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;

  if ($needs_filter) {
    # Generate: (remove-if (lambda (f) (find (char basename 0) "negated")) (p-glob pattern))
    # More Perl-like: filter files where the matched char is NOT in the negated set
    # Extract just the filename part for matching
    my $filter = qq{(remove-if (lambda (--f--) }
               . qq{(let ((--name-- (file-namestring (pathname --f--)))) }
               . qq{(and (> (length --name--) 0) }
               . qq{(find (char --name-- 0) "$negated_chars")))) }
               . qq{$call)};
    return $self->_wrap_wantarray_ctx($filter, $ctx);
  }

  return $self->_wrap_wantarray_ctx($call, $ctx);
}

# E2 form variant of gen_glob.  No operand-text dispatch: the pattern is
# generated as a form and the negated-char-class detection runs on its FLAT
# text (== v1's $pattern_str bytes) exactly like gen_glob, so the same globs
# get the same filter.  The remove-if filter is built structurally (its
# to_flat is byte-identical to the text template).  wantarray wrap via the
# form variant.
sub gen_glob_form {
  my ($self, $node, $node_id, $kids) = @_;

  my ($pattern_flat, $call);
  if (@$kids == 1) {
    my $pf = $self->gen_node_form($kids->[0]);
    $pattern_flat = Pl::CLForm::to_flat($pf);
    $call = ['p-glob', $pf];
  } elsif (@$kids > 1) {
    my @parts  = map { $self->gen_node_form($_) } @$kids;
    my $concat = ['p-.', @parts];
    $pattern_flat = Pl::CLForm::to_flat($concat);
    $call = ['p-glob', $concat];
  } else {
    $pattern_flat = '"*"';
    $call = ['p-glob'];
  }

  # Negated character class [!chars]/[^chars] in a LITERAL pattern: SBCL's
  # pathname wildcards can't negate, so glob a ?-simplified pattern and filter.
  my $negated_chars;
  if ($pattern_flat =~ /^"([^"]*)"$/) {
    my $pat = $1;
    if ($pat =~ /\[([!\^])([^\]]+)\]/) {
      $negated_chars = $2;
      (my $simple_pat = $pat) =~ s/\[[!\^][^\]]+\]/?/g;
      $call = ['p-glob', qq{"$simple_pat"}];
    }
  }

  my $ctx = defined $node_id
            ? $self->expr_o->get_node_context($node_id) : INHERIT_CTX;

  if (defined $negated_chars) {
    my $filter = ['remove-if',
                  ['lambda', ['list', '--f--'],
                   ['let', ['list', ['list', '--name--',
                                     ['file-namestring', ['pathname', '--f--']]]],
                    ['and', ['>', ['length', '--name--'], '0'],
                            ['find', ['char', '--name--', '0'],
                                     qq{"$negated_chars"}]]]],
                  $call];
    return $self->_wrap_wantarray_ctx_form($filter, $ctx);
  }

  return $self->_wrap_wantarray_ctx_form($call, $ctx);
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

# E2 form variant of gen_anon_sub: sub { … } → (lambda () body…).  Clean, no
# text inspection.  Empty body declines (the text emitter emits "(lambda () )"
# with a trailing space a form cannot reproduce).
sub gen_anon_sub_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;
  my @body = map { $self->gen_node_form($_) } @$kids;
  return ['lambda', ['list'], @body];
}


# Generate function reference for block callbacks
# Output: #'func-name
sub gen_func_ref {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  return Pl::CLForm::to_flat($node->{lambda_form}) if $node->{lambda_form};
  return $node->{raw_lambda} if $node->{raw_lambda};
  my $func_name = $node->{func_name};
  return "#'$func_name";
}

# E2 form variant: \&foo → #'name atom.  A lambda_form is a Parser2-lowered
# do{}/anon-sub lambda (task #78); a raw_lambda is v1's pre-generated CL
# string, embedded as a raw atom.
sub gen_func_ref_form {
  my ($self, $node, $node_id, $kids) = @_;
  return $node->{lambda_form} if $node->{lambda_form};
  return Pl::CLForm::raw($node->{raw_lambda}) if $node->{raw_lambda};
  return "#'" . $node->{func_name};
}

# E2 form variant of gen_array_init: [ … ] → (make-p-box (p-array-init …)).
# Same list-context + tail_position handling as the text emitter (run once).
sub gen_array_init_form {
  my ($self, $node, $node_id, $kids) = @_;
  my $saved_tail = $self->environment ? $self->environment->tail_position : 0;
  $self->environment->tail_position(0) if $self->environment && $saved_tail;
  my @elements;
  for my $kid_id (@$kids) {
    my $saved_ctx = $self->expr_o->get_node_context($kid_id);
    $self->expr_o->set_node_context($kid_id, LIST_CTX);
    push @elements, $self->gen_node_form($kid_id);
    $self->expr_o->set_node_context($kid_id, $saved_ctx);
  }
  $self->environment->tail_position($saved_tail) if $self->environment && $saved_tail;
  return @elements
      ? ['make-p-box', ['p-array-init', @elements]]
      : ['make-p-box', ['make-array', '0', ':adjustable', 't', ':fill-pointer', '0']];
}

# E2 form variant of gen_hash_init: { … } → (make-p-box (p-hash …)).  The
# EMPTY case declines (before any child generation): the text emitter emits
# "(p-hash )" with a trailing space that a form cannot reproduce; keeping it on
# the text path preserves v1's exact bytes for the rare empty-hash constructor.
sub gen_hash_init_form {
  my ($self, $node, $node_id, $kids) = @_;
  # Empty {} emits (p-hash) — the text twin printed "(p-hash )" (trailing
  # space from join-interpolation); normalized here per task #78 E2.final.
  my @pairs = map { $self->gen_node_form($_) } @$kids;
  return ['make-p-box', ['p-hash', @pairs]];
}

# Generate inline lambda for grep/map/sort blocks
# Output: (lambda (params) body)
sub gen_inline_lambda {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @pair     = @{$node->{params} // []};
  my $params   = join(' ', @pair);
  my $spec     = _sort_pair_special_decl($node->{params});
  # task #78 safety net: a Parser2-lowered body carries body_form only; the
  # form handler normally intercepts it, but flat-print here too so no text
  # caller can ever see a bodyless lambda.
  my $body     = defined $node->{body_cl} ? $node->{body_cl}
               : $node->{body_form}
               ? join(' ', map { Pl::CLForm::to_flat($_) } @{$node->{body_form}})
               : 'nil';
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
    my $call_args = $has_dollar_dollar ? " $params" : '';
    my $lambda_body = "(let ((*wantarray* nil))\n"
                    . "  (handler-case ($cl_func$call_args)\n"
                    . "    (undefined-function ()\n"
                    . "      (let ((al (intern \"PL-AUTOLOAD\" |sort--pkg|)))\n"
                    . "        (when (fboundp al) (funcall (symbol-function al)))))))";
    $kids = [];
    return "(let ((|sort--pkg| *package*))\n  (lambda ($params)\n    $spec(catch :p-return\n      (block nil\n$lambda_body))))";
  }

  # Scalar comparator (sort $var LIST): call via p-sort-get-fn at runtime.
  # The lambda params $a/$b create dynamic bindings; p-sort-get-fn resolves
  # the scalar (coderef, string name, glob, or glob ref) to a CL function.
  if ($for_func eq 'sort' && $node->{scalar_cmp}) {
    my $scalar_cl = $kids && @$kids ? $self->gen_node($kids->[0]) : 'nil';
    # Capture *package* at lambda creation so p-sort-get-fn can look up
    # string sub names in the correct (user) package even when called from
    # inside p-sort (which runs in the :pcl package).
    my $lambda_body = "(let ((*wantarray* nil) (*package* |sort--pkg|))\n  (funcall (p-sort-get-fn $scalar_cl) $params))";
    $kids = [];
    return "(let ((|sort--pkg| *package*))\n  (lambda ($params)\n    $spec(catch :p-return\n      (block nil\n$lambda_body))))";
  }

  # Sort comparator blocks may contain explicit `return` — wrap with catch.
  # Bind *wantarray* = nil (scalar): the comparator must return a scalar, and
  # Perl's wantarray() inside a comparator returns false (scalar context).
  # grep/map blocks do NOT get the catch: `return` inside them should
  # propagate to the enclosing sub's (catch :p-return ...).
  if ($for_func eq 'sort') {
    return "(lambda ($params)\n  $spec(catch :p-return\n    (block nil\n(let ((*wantarray* nil))\n$body))))";
  }
  return "(lambda ($params)\n$body)";
}

# A sort comparator lambda inside a block-level `package X;` region binds the
# REGION's pair — X::$a / X::$b — because that is what the requalified body
# reads and what perl sets (Pl::PExpr::_sort_pair has the full reasoning).
# Those two symbols are defvar'd by the region's own entry forms, but those
# run INSIDE the enclosing top-level form, so they have NOT proclaimed the
# symbols special by the time this lambda is compiled (probed s380: a nested
# defvar leaves the parameter a plain LEXICAL).  This declaration is what
# makes the binding dynamic, so a comparator that reads the GLOBAL — perl's
# `sort NAME LIST` shape, and the `${(caller)[0]."::a"}` symbolic shape
# Sort::Versions uses — sees the values.  The section's bare $a/$b need
# nothing: their defvars are top level, hence no declaration and byte-for-byte
# unchanged emission for every unswitched sort.
sub _sort_pair_special_decl {
  my ($params) = @_;
  my @qualified = grep { /::/ } @{ $params // [] };
  return '' unless @qualified;
  return '(declare (special ' . join(' ', @qualified) . ")) ";
}

# E2 form variant (task #78): only for a Parser2-lowered body (`body_form`,
# an arrayref of CLForms — see Pl::Parser2::lower_embedded_block); every
# other shape (v1 pipeline, named/scalar sort comparators, declined blocks)
# keeps the text emitter's fixed multiline layouts via body_cl.  Same
# wrappers as the text emitter: sort comparators get the :p-return catch and
# the scalar-context *wantarray* bind; grep/map/eval bodies get neither
# (`return` inside them must propagate to the enclosing sub's catch).
sub gen_inline_lambda_form {
  my ($self, $node, $node_id, $kids) = @_;
  my @pair     = @{$node->{params} // []};
  my $params   = ['list', @pair];
  my $for_func = $node->{for_func} // '';
  # Form twin of _sort_pair_special_decl (see its comment): a region's
  # qualified pair must be declared special here or the parameter binding is
  # lexical and a comparator reading the global sees nothing.  Empty — hence
  # emission byte-identical — for the bare pair.
  my @spec     = grep { /::/ } @pair;
  my @decl     = @spec ? (['declare', ['special', @spec]]) : ();

  # Named comparator (sort NAME LIST) — form twin of the text branch:
  # $a/$b dynamic bindings, ($$)-prototype subs get them as args too, and
  # an undefined comparator dispatches to AUTOLOAD (Perl #30661) in the
  # package captured at lambda creation.
  if ($for_func eq 'sort' && $node->{comparator_name}) {
    my $cl_func = $self->cl_name($node->{comparator_name});
    my $proto;
    if ($self->environment) {
      my $cname = $node->{comparator_name};
      $proto = $self->environment->get_prototype($cname)
            // $self->environment->get_prototype($cname =~ s/^:://r)
            // $self->environment->get_prototype($cname =~ s/.*:://r);
    }
    my $call = ($proto && $proto->{is_proto}
                && ($proto->{proto_string} // '') eq '$$')
             ? [$cl_func, @pair] : [$cl_func];
    return
      ['let', ['list', ['list', '|sort--pkg|', '*package*']],
        ['lambda', $params, @decl,
          ['catch', ':p-return',
            ['block', 'nil',
              ['let', ['list', ['list', '*wantarray*', 'nil']],
                ['handler-case', $call,
                  ['undefined-function', ['list'],
                    ['let', ['list',
                             ['list', 'al',
                              ['intern', '"PL-AUTOLOAD"', '|sort--pkg|']]],
                      ['when', ['fboundp', 'al'],
                        ['funcall', ['symbol-function', 'al']]]]]]]]]]];
  }

  # Scalar comparator (sort $var LIST) — resolved at runtime by
  # p-sort-get-fn; *package* rebound to the creation-time package so string
  # sub names resolve in USER code, not :pcl (p-sort's own package).
  if ($for_func eq 'sort' && $node->{scalar_cmp}) {
    my $scalar = ($kids && @$kids) ? $self->gen_node_form($kids->[0]) : 'nil';
    return
      ['let', ['list', ['list', '|sort--pkg|', '*package*']],
        ['lambda', $params, @decl,
          ['catch', ':p-return',
            ['block', 'nil',
              ['let', ['list', ['list', '*wantarray*', 'nil'],
                               ['list', '*package*', '|sort--pkg|']],
                ['funcall', ['p-sort-get-fn', $scalar], @pair]]]]]];
  }

  my $bf = $node->{body_form};
  if ($ENV{PCL_E2_RAW_CENSUS} && !$bf) {
    (my $snip = $node->{body_cl} // '') =~ s/\s+/ /g;
    warn "pcl-raw\tlambda:$for_func:body_cl\t" . substr($snip, 0, 70) . "\n";
  }
  return undef unless $bf;
  if ($for_func eq 'sort') {
    return ['lambda', $params, @decl,
            ['catch', ':p-return',
             ['block', 'nil',
              ['let', ['list', ['list', '*wantarray*', 'nil']], @$bf]]]];
  }
  return ['lambda', $params, @$bf];
}


# Generate substitution s///
# Output: (p-subst "pattern" "replacement" :g :i ...)
#         (p-subst pat-expr "replacement" :g :i ...)  when pattern has $var
#         (p-subst "pattern" (lambda () <cl-expr>) :g :i ...)  when replacement has $var
#         (p-subst "pattern" (lambda () <cl-expr>) :g :e ...)  when /e
sub gen_substitution {
  my $self = shift;
  my $node = shift;
  return Pl::CLForm::to_flat($self->gen_substitution_form($node));
}

# Form-producing (E2-converted).  Never declines.  The /e (and interpolated)
# replacement body is compiled by _compile_subst_e_expr / _gen_interp_replacement
# — both still produce text, embedded as a raw atom inside the (lambda () …)
# form; structuring those bodies is the inline_lambda step (E2 last item).
sub gen_substitution_form {
  my $self = shift;
  my $node = shift;

  my $match = $node->get_match_string;
  my $subst = $node->get_substitute_string;
  my $mods  = $node->get_modifiers;

  my @mod_strs = map { ":$_" } sort keys %$mods;

  # Pattern: a string literal atom, or the interpolation form ("…"/$var/
  # (p-string-concat …)) evaluated to the pattern string at runtime.
  my $match_form;
  if (_has_regex_interpolation($match)) {
    $match_form = $self->_gen_interp_regex_pattern($match);
  } else {
    (my $m = $match) =~ s/\\/\\\\/g;
    $m =~ s/"/\\"/g;
    $match_form = qq{"$m"};
  }

  # s///e: replacement is Perl code — parse it and wrap in a lambda.
  # The body arrives as a CLForm (task #78); raw only inside declined subtrees.
  if ($mods->{e}) {
    return ['p-subst', $match_form,
            ['lambda', ['list'], $self->_compile_subst_e_expr($subst)],
            @mod_strs];
  }

  # Replacement with variable interpolation: wrap in a lambda so $varname/$1..$9
  # evaluate at match time.  Deliberately NOT the scanner gate: a replacement
  # is dq text, not a pattern, and a bare `$1`/`$2` replacement is served
  # better by the runtime's native backref substitution (no lambda call per
  # match) than by the interpolation path.  Widening this gate is its own
  # measured change (docs/interp-scan.md §wiring).
  if (_replacement_interpolates($subst)) {
    return ['p-subst', $match_form,
            ['lambda', ['list'], $self->_gen_interp_replacement($subst)],
            @mod_strs];
  }

  # Normal case: the replacement is double-quoted context, so its escapes
  # (\n, \t, \x41, \x{263a}, …) are processed HERE, at transpile time — the
  # runtime only rewrites $N backrefs for cl-ppcre.  \1-\9 are kept as $1-$9
  # (perl reads both as backrefs in a replacement), and literal backslashes
  # are doubled because cl-ppcre's replacement parser treats \ specially.
  my $s = _unescape_subst_replacement($subst);
  $s =~ s/\\/\\\\/g;    # CL string literal escaping
  $s =~ s/"/\\"/g;
  return ['p-subst', $match_form, qq{"$s"}, @mod_strs];
}

# One dq-escape token starting at the backslash at position I of STR — the
# same alternation StringInterpolation::unescape_string feeds to
# _process_dq_escape.  Returns (processed-characters, source-length).
sub _take_dq_escape {
  my ($str, $i) = @_;
  my $rest = substr($str, $i + 1);
  if ($rest =~ /^(x\{[^}]*\}|x[0-9A-Fa-f]{1,2}|x|o\{[^}]*\}|N\{[^}]*\}|[0-7]{1,3}|c.|[ntreafd"\\\$\@]|.)/s) {
    my $tok = $1;
    return (_process_dq_escape($tok), 1 + length($tok));
  }
  return ('\\', 1);   # lone trailing backslash
}

# Escape-process a NON-interpolated s/// replacement (dq semantics).  \1-\9
# become $1-$9 so the runtime's $N → ppcre-\N rewrite picks them up; every
# literal backslash the escapes produce is doubled so cl-ppcre's replacement
# parser (where \ introduces a register) reads it as a literal.
sub _unescape_subst_replacement {
  my ($str) = @_;
  my $out = '';
  my $i = 0;
  while ($i < length($str)) {
    my $c = substr($str, $i, 1);
    if ($c eq '\\') {
      my $next = $i + 1 < length($str) ? substr($str, $i + 1, 1) : '';
      if ($next =~ /[1-9]/) {
        $out .= "\$$next";
        $i += 2;
      } else {
        my ($chars, $len) = _take_dq_escape($str, $i);
        $chars =~ s/\\/\\\\/g;
        $out .= $chars;
        $i += $len;
      }
    } else {
      $out .= $c;
      $i++;
    }
  }
  return $out;
}

# Build a CL expression that evaluates to the interpolated replacement string.
# Handles $varname, $1-$9 (Perl backreferences, available as CL dynamic vars in lambda context).
# s/// replacement (non-/e) -> CL, via the REAL double-quoted-string
# interpolator.
#
# It used to be a hand-rolled mini-interpolator (kept below as
# _gen_interp_replacement_simple) whose loop understood exactly \1..\9,
# $1..$9, ${name} and $name.  A SUBSCRIPT fell through as literal text, so
# `s/(a)/$h{$1}/g` emitted (p-string-concat $h "{" $1 "}") and produced the
# string "{a}" — plausible garbage, silently, for one of the most common
# idioms in Perl (`s/(\w+)/$map{$1}/g`).  Task #182.
#
# The fix is CLAUDE.md 11: `"$h{$1}"` in an ordinary string was already
# correct, so normalise into that path instead of teaching the copy new
# tricks.  The ONE thing a replacement has that a dq string does not is
# \1..\9 as backrefs, so those are rewritten to $1..$9 first.
sub _gen_interp_replacement {
  my ($self, $str) = @_;

  my $norm = _subst_backrefs_to_dollars($str);
  # PPI needs a well-formed "..." token; a trailing odd backslash would eat
  # the closing quote.  That shape cannot be interpolated meaningfully anyway,
  # so leave it to the simple path rather than hand PPI something broken.
  my $trailing = ($norm =~ /(\\+)\z/) ? length($1) : 0;
  unless ($trailing % 2) {
    my $q = $norm;
    $q =~ s/"/\\"/g;
    my $form = eval {
      require PPI::Token::Quote::Double;
      require Pl::PExpr;
      my $fake = PPI::Token::Quote::Double->new(qq{"$q"});
      my $expr_o = Pl::PExpr->new(
        e => [$fake],
        ($self->environment ? (environment => $self->environment) : ()),
      );
      my $id = $expr_o->str_interpol->parse_interpolated_string($expr_o, $fake);
      my $gen = Pl::ExprToCL->new(
        expr_o       => $expr_o,
        environment  => $self->environment,
        indent_level => $self->indent_level,
      );
      $gen->gen_node_form($id);
    };
    return $form if defined $form && (ref $form || $form ne '');
  }
  return _gen_interp_replacement_simple($str);
}

# \1..\9 mean the same as $1..$9 in a replacement; the dq-string parser has no
# such rule.  An escaped backslash (\\) is consumed as a pair so that \\1 keeps
# its literal 1.
sub _subst_backrefs_to_dollars {
  my ($str) = @_;
  my $out = '';
  my $i = 0;
  while ($i < length($str)) {
    my $c = substr($str, $i, 1);
    if ($c eq '\\' && $i + 1 < length($str)) {
      my $n = substr($str, $i + 1, 1);
      $out .= ($n =~ /[1-9]/) ? "\$$n" : "$c$n";
      $i += 2;
    } else {
      $out .= $c;
      $i++;
    }
  }
  return $out;
}

sub _gen_interp_replacement_simple {
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
        # dq escape (\n, \x41, \x{263a}, …) — process to its characters; the
        # lambda's result is spliced in verbatim, so no cl-ppcre doubling here.
        my ($chars, $len) = _take_dq_escape($str, $i);
        $literal .= $chars;
        $i += $len;
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
  # Every part is an atom ("literal", $1..$9, $varname) — the multi-part
  # case is a structural concat form (task #78: no raw text out of here).
  return @parts == 0 ? '""'
       : @parts == 1 ? $parts[0]
       : ['p-string-concat', @parts];
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
      # task #78: statements come back as CLForms (raw only for genuinely-
      # declining subtrees), assembled structurally below.
      my $f = $gen->gen_node_form($node_id);
      push @cl_parts, $f if defined $f && (ref $f || $f ne '');
    }

    return unless @cl_parts;

    my $body = @cl_parts == 1 ? $cl_parts[0] : ['progn', @cl_parts];
    if (@let_vars) {
      $result = ['let',
                 ['list', map { ['list', $_, ['make-p-box', 'nil']] } @let_vars],
                 $body];
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
  return Pl::CLForm::to_flat($self->gen_transliteration_form($node));
}

# Form-producing (E2-converted).  Never declines; pure (escape expansion only).
sub gen_transliteration_form {
  my $self = shift;
  my $node = shift;

  my $from = $node->get_match_string;
  my $to   = $node->get_substitute_string;
  my $mods = $node->get_modifiers;

  # Process tr escape sequences to actual characters, then build safe CL literals
  my $from_cl = _cl_string_literal(_expand_tr_escapes($from));
  my $to_cl   = _cl_string_literal(_expand_tr_escapes($to));

  my @mod_strs = map { ":$_" } sort keys %$mods;
  return ['p-tr', $from_cl, $to_cl, @mod_strs];
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
# E2: form twin — identical body, the compound case returns the CLForm.
# The text entry is its exact flat print (all early cases are "…" atoms).
sub convert_perl_string {
  my $self = shift;
  return Pl::CLForm::to_flat($self->convert_perl_string_form(@_));
}
sub convert_perl_string_form {
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

  return _cl_string_literal_form($content);
}

# Build a CL string literal, escaping surrogate and non-character codepoints
# that can't be embedded in a UTF-8 source file.  The form twin returns a
# plain "…" atom, or ['concatenate', "'string", …] with ['string',
# ['code-char', N]] parts for the bad codepoints; the text entry is its
# exact flat print (E2: gen_leaf_form embeds the form, text callers keep
# their bytes).
sub _cl_string_literal { Pl::CLForm::to_flat(_cl_string_literal_form(shift)) }
sub _cl_string_literal_form {
  my $content = shift;
  # Characters invalid in UTF-8: surrogates U+D800-U+DFFF, and non-chars U+FFFE/U+FFFF
  # (and the pattern repeats at every 0x10000 boundary: U+1FFFE, U+1FFFF, etc.)
  my $bad_char_re = qr/[\x{D800}-\x{DFFF}]|[\x{FFFE}\x{FFFF}]/;
  if ($content !~ $bad_char_re) {
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
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
      push @parts, ['string', ['code-char', ord($ch)]];
      $content = substr($content, 1);
    }
  }
  return @parts == 1 ? $parts[0] : ['concatenate', "'string", @parts];
}


1;
