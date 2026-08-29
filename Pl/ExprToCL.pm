# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::ExprToCL;

use v5.20;
use strict;
use warnings;

use Moo;

use Scalar::Util qw/looks_like_number/;
use Pl::PExpr qw(SCALAR_CTX LIST_CTX VOID_CTX INHERIT_CTX);
use Pl::CLForm qw(cl_sym cl_pkg);
use Pl::InterpScan ();
use Pl::Passes ();
use Pl::Environment ();   # fh_bareword_shape — see Pl/PExpr.pm's note

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

# The two per-sub / per-scope FACTS the Kind-A rules below consume (Phase A of
# docs/plan-one-compiler-s411.md — folded here from ExprToCL2, the deleted
# second generator).  Both default EMPTY: a caller that has no facts (v1's
# statement layer, the re-entrant string/regex compiles) simply never
# licenses the fast shapes.  Parser2::_lower_expr passes its _cur_sub_info
# and _let_bound_vars.
has sub_info => (          # name → { cl_name, insensitive, … } (Parser2 sub facts)
  is      => 'ro',
  default => sub { {} },
);
has lexicals => (          # sigil-name → 1 for every let-bound lexical in scope
  is      => 'ro',
  default => sub { {} },
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

# The node types that have a NAMED emitter — as opposed to a binary
# operator, which is dispatched by its operator text (gen_binary_op_form).
# This is the key set of the pre-E2 text `handlers` table; the table and the
# last text emitter it named are gone (#303 s390, Phase A4 s411).  The SET
# still has to exist: `!$NAMED_TYPE{...}` is the "this type is a binary
# operator" test at the dispatch site, and a named type that fell out of it
# would be handed to gen_binary_op_form under its own name.
my %NAMED_TYPE = map { $_ => 1 } qw(
  funcall methodcall ref_funcall prefix_op postfix_op
  a_acc h_acc a_ref_acc h_ref_acc
  slice_a_acc slice_h_acc kv_slice_h_acc kv_slice_a_acc
  arr_init hash_init progn tree_val filehandle readline
  glob backtick anon_sub func_ref inline_lambda glob_slot
);

sub _build_form_handlers {
  my $self = shift;
  return {
    'funcall'          => \&gen_funcall_form,
    'methodcall'       => \&gen_methodcall_form,
    'ref_funcall'      => \&gen_ref_funcall_form,
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
  exp fc fcntl fileno flatten flatten-args for foreach fork wait waitpid getppid kill exec
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
# sub wrongly returns the (k,v) pair).  gen_funcall_form wraps every call to one of
# these in `(let ((*wantarray* t/nil)) …)` per the node's annotated context.
#
# INVARIANT: this set must list EVERY runtime builtin that branches on
# *wantarray*.  When you add such a builtin to pcl-runtime.lisp, add it here.
# (readline `<FH>` and the file-glob `<pat>` are separate PPI node types, not
# funcalls — they apply the same wrapper in gen_readline_form / gen_glob_form.)
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

  # Note: Sigil cast operators (@, %, $) are handled in gen_prefix_op_form
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
  # @{^CAPTURE} (5.26+) is the last match's capture VALUES; the runtime keeps
  # it beside @-/@+ (set-capture-groups).  %{^CAPTURE} and %{^CAPTURE_ALL} are
  # perl SYNONYMS for %+ and %- — the same variable, so they map onto those two
  # rather than duplicating the state.  MUST be pipe-quoted: generated code
  # loads under :invert, where a bare all-caps token reads DOWN-cased and the
  # runtime's symbol is never found (task #412).
  '@{^CAPTURE}'      => '|@{^CAPTURE}|',
  '%{^CAPTURE}'      => '%+',
  '%{^CAPTURE_ALL}'  => '%-',
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
  # $^E is $! on POSIX — the SAME variable, probed in both directions (set $!,
  # read $^E; set $^E, read $!), so it maps onto the errno accessor rather than
  # onto a cell of its own.  An inert cell would read "" after a failed syscall
  # where perl reads the strerror text: silent wrong, not a stub (task #571).
  '$^E' => ['p-errno-string'],
  '$^C' => '|$^C|',   # COMPILING — 0 at run time (PCL has no -c)
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

  # perl's PERL-4 package separator: `A'B` IS `A::B` (still valid in 5.40, the
  # oracle; deprecated in 5.38).  The runtime already reads a SYMBOLIC name
  # that way (%p-tick-package-seps) and the compiler has to agree, or the tick
  # ends up INSIDE a CL symbol — where `'` is the reader's quote character and
  # terminates the token: `pl-main'Backwards` reads as `(pl-main 'Backwards)`,
  # a call of an undefined sub, and in a quoted position it silently becomes
  # two arguments (task #550, found by #514's guard on perl-tests/sort.t:240).
  # An apostrophe in a perl identifier can only ever BE the separator, so the
  # rewrite is the whole rule; the lookahead is perl's own toker test.
  $perl_name =~ s/'(?=\w)/::/g;

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
    my $cl_pkg = cl_pkg($pkg);
    return "${cl_pkg}::" . cl_sym("pl-${func}");
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
  my $fn = $perl_name =~ /:/ ? "|pl-$perl_name|" : cl_sym("pl-$perl_name");
  if ($cur_pkg && $cur_pkg ne 'main') {
    my $cl_pkg = cl_pkg($cur_pkg);
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


# The SUB a `&`-MENTION names — `&NAME` / `&Pkg::NAME` as one Symbol token
# (#387, s414: `exists`, `defined` and `undef` each spelled this).  Returns
# the split package and name, or () when ARG is not that shape.  Its sibling
# spelling, `&{EXPR}` / `&$cref`, is _amp_cast_operand_id above — which the
# `undef` arm already used while `exists`/`defined` open-coded the same walk.
sub _amp_sub_name {
  my ($self, $arg_id) = @_;
  my $node = $self->expr_o->get_a_node($arg_id);
  return () unless ref($node) eq 'PPI::Token::Symbol';
  my $sym = $node->content();
  return () unless $sym =~ /^&(.+)$/;
  return $self->_split_func_sym($1);
}


# Main entry point: generate CL code from AST
sub generate {
  my $self    = shift;
  my $node_id = shift // $self->expr_o->root;

  # ONE dialect (Phase A4): the text entry is the FORM entry, printed flat —
  # E2 made the two byte-identical, so every text caller (v1's statement
  # layer, the constant/default compiles, the re-entrant string compiles)
  # now runs the same generator as Parser2.
  return ($self->indent_str x $self->indent_level)
       . Pl::CLForm::to_flat($self->gen_node_form($node_id));
}


# `$#arr` → the CONTAINER name to take the last index of.  Both leaf emitters
# (form and text) carried their own copy of this; the copies also both missed
# %SPECIAL_VARS, so a MAGIC container came out bare — and a bare all-caps token
# reads DOWN-cased under :invert, so `$#{^CAPTURE}` aborted the load unbound
# instead of answering -1 (task #412).
#   $#arr      → @arr
#   $#A::ISA   → A::@ISA        (the @ goes AFTER the package prefix)
#   $#{^CAPTURE} → |@{^CAPTURE}|
# state-renamed arrays follow their rename, as before.
sub _array_index_container {
  my ($self, $content) = @_;
  $content =~ s/^\$#(.*)::(.+)$/$1\::\@$2/
      || $content =~ s/^\$#/\@/;
  if ($self->environment) {
    my $renames = $self->environment->state_var_renames;
    $content = $renames->{$content} if $renames && exists $renames->{$content};
  }
  my $special = $SPECIAL_VARS{$content};
  return $special if defined $special && !ref $special;
  # THE #418 SPELLING (s425 review fix): this site built the array token BARE —
  # `@Ｘ` read back NFKC-folded as `@X` ("The variable @X is unbound", code
  # and strings alike), and a MULTI-segment package, `$#Foo::Bar::x`, emitted
  # `Foo::Bar::@x`, which the reader cannot even READ (the file died at load;
  # ASCII, pre-existing).  cl_pkg/cl_sym spell both halves the way every other
  # emitter does: `|Foo::Bar|::@x`, `|@Ｘ|`; identity on single-segment ASCII.
  return cl_pkg($1) . '::' . cl_sym($2) if $content =~ /^(.*)::(\@.+)$/;
  return cl_sym($content);
}

# Generate code for a single node
sub gen_node {
  my ($self, $node_id) = @_;
  # ONE dialect (Phase A4): the flat print of the FORM emitter.  Every
  # "I want this node as text" site (a container symbol, a filehandle word,
  # a method name, the v1 statement layer through generate()) reads the same
  # generator Parser2 does; the text emitters that used to live behind this
  # entry are gone.
  return Pl::CLForm::to_flat($self->gen_node_form($node_id));
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
    if (!$NAMED_TYPE{$type}) {
      my $form = $self->gen_binary_op_form($type, $kids, $node_id);
      return $form if defined $form;
    }
    # No form emitter took this node.  Rule 12: a missing case DIES — there is
    # no text emitter to fall back to any more (Phase A4), and a decline here
    # was always a compiler gap; _parse_expression_form turns the die into the
    # announced PARSE-ERROR drop.
    die "PCL internal: no form emitter for expression node type '$type'\n";
  }
  # A PPI::Token::Operator/Word WITH children is a binary op, NOT a leaf →
  # the binary-op form emitter.  A genuine leaf (no children) goes to
  # gen_leaf_form; a leaf it does not cover DIES below (rule 12 — since
  # Phase A4 there is no text emitter to fall back to).
  my $kids = $self->expr_o->get_node_children($node_id) || [];
  if (@$kids
      && (ref($node) eq 'PPI::Token::Operator' || ref($node) eq 'PPI::Token::Word')) {
    my $form = $self->gen_binary_op_form($node->content(), $kids, $node_id);
    return $form if defined $form;
  }
  if (!@$kids && defined(my $lf = $self->gen_leaf_form($node))) {
    return $lf;
  }
  my $desc = ref($node) ? ref($node) . ':' . (eval { $node->content } // '?')
                        : "scratch:$node";
  die "PCL internal: no form emitter for expression leaf " . substr($desc, 0, 60) . "\n";
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
    return ['p-array-last-index', $self->_array_index_container($node->content())];
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
    my ($pattern, $flags, $delim) = _parse_regex_content($content, 1);
    if (_delim_interpolates($delim) && _has_regex_interpolation($pattern)) {
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
    my ($pattern, $flags, $delim) = _parse_regex_content($content, 0);
    if (_delim_interpolates($delim) && _has_regex_interpolation($pattern)) {
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
  return ($pattern, $flags, $open_ch);
}

# Does a quote-like construct with this OPENING DELIMITER interpolate?
# A SINGLE-QUOTED one does not, and that is a delimiter fact, not a content
# fact: `m'$x'`, `qr'$x'` and `s'$x'$y'` are literal in perl exactly as '…' is
# (probed 5.40.3 — `'m$xn' =~ s'$x'Q'` leaves the string alone, and `s'(A)'$1'`
# writes the two characters `$1`).  One predicate for every gate, so the three
# regex sites and the s/// replacement cannot disagree about it.
sub _delim_interpolates {
  my ($open_ch) = @_;
  return (defined $open_ch && $open_ch eq "'") ? 0 : 1;
}

# The two delimiters of an s/// token, as the interpolation question: perl
# takes them separately, so `s{A}'[$x]'` has a dq-like PATTERN and a literal
# REPLACEMENT (probed).  PPI's get_delimiters returns the pair as two 2-char
# strings ("//" , "''"); a bracketing form gives "{}".
sub _subst_delims {
  my ($node) = @_;
  my @d = eval { $node->get_delimiters };
  return (undef, undef) if $@ || @d < 1;
  my $m = defined $d[0] ? substr($d[0], 0, 1) : undef;
  # A bracketing match delimiter is followed by its own opening delimiter for
  # the replacement; the one-delimiter forms reuse the match's.
  my $r = @d > 1 && defined $d[1] ? substr($d[1], 0, 1) : $m;
  return ($m, $r);
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

# Does an s/// REPLACEMENT need the interpolating (lambda) path?
#
# WHERE THE TEXT MENTIONS A VARIABLE IS THE SCANNER'S ANSWER — Pl::InterpScan,
# the one every interpolation consumer shares (task #237; standing rule
# docs/var-handling-review-s379.md §8).  This site used to ask a private
# `(?<!\\)[\$\@][a-zA-Z_{]` of its own, and that class is ASCII where the rest
# of the pipeline is Unicode-aware, so with `use utf8` in force `s/Ｘ/$ｉ/`
# answered NO and emitted the LITERAL text `$ｉ` (#492) — silent wrong, while
# the braced `${ｉ}` and the identical dq string `"$ｉ"` were both right.  The
# replacement text reaches here DECODED (measured: utf8 flag on, ord 65353), so
# the class was the whole bug.  The same one scanner also answers for `$::qq`,
# `$#arr` and a `\\`-escaped sigil, which the private class read as literal
# text too (all three probed against perl 5.40.3).
#
# THE ONE NAME THIS GATE STILL SKIPS IS A NUMBERED BACKREF.  `$1`..`$N` are
# served by the runtime's native $N rewrite (no lambda call per match), which is
# the gate's deliberate narrowness.  Every OTHER magic goes down the lambda
# path since task #520: `$&`, `` $` ``, `$'`, `$+`, `$^N`, `$!`, `$$`, `$0`,
# `$,`, `@-`, `@+` … all used to emit as LITERAL TEXT (`s/A/[$&]/` on "xAy"
# gave `x[$&]y`).  An earlier attempt widened only this gate and found `` $` ``
# and `$'` coming back EMPTY on the lambda path; that was the OTHER half of the
# bug and is fixed in p-subst — the replacement lambda now gets its match state
# from the same `set-capture-groups` + `set-match-vars` pair the m// path uses,
# so all six magics are live inside it.  `$0` is NOT a backref (it is the
# program name, probed), so only `[1-9][0-9]*` is skipped.  A BRACED spelling
# always takes the lambda path, which is what the `{` in the old class meant and
# is what keeps `${^NAME}` working.
#
# CASE-SHIFT ESCAPES are the second reason to take this path (task #522).  `\U`
# `\L` `\u` `\l` `\Q` `\E` are dq-string operators, not interpolation, so no
# scanner event names them and `s/(A)/[\U$1\E]/` used to emit the escapes
# LITERALLY.  The dq-string compiler already implements them (probed), so the
# fix is to route the replacement there — never a second copy (rule 11).  This
# reason can fire with no sigil in the text at all (`s/A/[\Uab\E]/`), which is
# why it is tested BEFORE the `[\$\@]` pre-filter.
sub _replacement_interpolates {
  my ($subst) = @_;
  return 1 if _replacement_case_shifts($subst);
  return 0 if $subst !~ /[\$\@]/;
  for my $ev (@{ Pl::InterpScan::scan($subst) }) {
    return 1 if substr($subst, $ev->{span}[0] + length($ev->{sigil}), 1) eq '{';
    next if $ev->{form} eq 'magic'
         && defined $ev->{name} && $ev->{name} =~ /^[1-9][0-9]*\z/;
    return 1;
  }
  return 0;
}

# Does the replacement carry a dq case-shift / quotemeta escape?  Walks the
# escapes rather than pattern-matching, so `\\U` (an escaped backslash followed
# by a plain U) is not one — the same pair-skipping rule
# _unescape_subst_replacement and _subst_backrefs_to_dollars use.
sub _replacement_case_shifts {
  my ($subst) = @_;
  my $n = length $subst;
  my $i = 0;
  while ($i < $n) {
    if (substr($subst, $i, 1) eq '\\') {
      return 1 if substr($subst, $i + 1, 1) =~ /^[ULQEul]\z/;
      $i += 2;
      next;
    }
    $i++;
  }
  return 0;
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
    # Lazy: this file is loaded FROM Pl::Parser, so a compile-time
    # `use` would be circular; a runtime require is a %INC lookup once loaded.
    require Pl::Parser;
    my $doc = Pl::Parser::fragment_doc($src);
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
  # NAME OVERRIDE (task #561): the element-access container asks this renderer
  # for the AGGREGATE spelling of the node's name — see _bare_container_sym.
  my $override = shift;

  my $content = $override // $node->content() // '';
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
    return cl_sym($renames->{$content}) if $renames && exists $renames->{$content};
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
      return cl_pkg($pkg) . '::' . cl_sym("${sigil}${name}");
    }
  }
  # Unknown ${^...} caret variables. Perl (perlvar: "alphanumeric strings
  # preceded by a caret") treats any ${^NAME} without assigned special meaning
  # as an ordinary, main-forced global scalar: undef until set, autovivifying
  # (e.g. `is ${^MPE}, undef` then `++${^MPE}` is 1). The reserved names we DO
  # model live in %SPECIAL_VARS above; everything else degrades to a normal
  # global here rather than aborting the whole transpile. We register the
  # symbol so _insert_variable_forward_declarations emits a file-level defvar.
  # Any sigil: `@{^FOO}` and `%{^FOO}` are ordinary globals too, and emitting
  # one BARE produced an unbound symbol under :invert — a load-time abort of
  # the whole file rather than an undef read (found on @{^CAPTURE}, task #412).
  if ($content =~ /^[\$\@\%]\{\^/) {
    my $sym = "|$content|";
    $self->environment->add_caret_global($sym) if $self->environment;
    return $sym;
  }
  # THE plain variable token.  cl_sym is the identity on an ASCII name (which
  # is what keeps every existing emission byte-identical) and pipe-quotes a
  # name carrying a non-ASCII character (#418) — `%Ｘ` and `%X` are otherwise
  # ONE symbol, because the reader NFKC-folds the fullwidth Ｘ to X.
  return cl_sym($content);
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
    return "(p-array-last-index " . $self->_array_index_container($node->content()) . ")";
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
    my ($pattern, $flags, $delim) = _parse_regex_content($content, 1);
    if (_delim_interpolates($delim) && _has_regex_interpolation($pattern)) {
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
    # A bareword the environment KNOWS to be a filehandle becomes a CL SYMBOL
    # in the emitted call (`(p-open ＦＨ …)`, `(p-close ＦＨ)`, `p-binmode`,
    # `p-eof`, …), so it takes the #418 spelling.  The `is_filehandle`
    # registry is the mechanism that already tells the parser which barewords
    # are handles (PExpr registers them at `open`), so keying on it covers
    # every builtin that takes one, at once — rather than a per-builtin branch
    # (rule 11).  Its SIBLINGS are gen_filehandle_form (`print ＦＨ …`) and
    # gen_readline_form (`<ＦＨ>`), which quote the same name; before this,
    # `open ＦＨ` registered the reader's NFKC-folded `FH` while those two used
    # the pipe-quoted symbol, so the program wrote to one handle, read from
    # another, and printed NOTHING (measured s423).
    # NOT a blanket rule for every Word: this leaf also carries names that are
    # fed BACK to cl_name as perl names (a qualified call's Word), and a
    # `|ＦＯＯ::f|` there splits on `::` into the package `|ＦＯＯ` — an
    # unbalanced token that kills the file at READ (measured s423).
    # A package-QUALIFIED handle name goes out QUOTED and registers its
    # package — see _fh_designator (#491).
    return $self->_fh_designator($content)
      if $self->environment && $self->environment->is_filehandle($content);
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
    my ($pattern, $flags, $delim) = _parse_regex_content($content, 0);
    if (_delim_interpolates($delim) && _has_regex_interpolation($pattern)) {
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

sub _is_string_literal_node {
  # Returns true for PPI string literal nodes (single/double/heredoc quotes etc.)
  # String literals in flip-flops use $.  comparison (may warn "isn't numeric").
  my ($node) = @_;
  return 0 unless defined $node;
  return ref($node) =~ /^PPI::Token::Quote/;
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
               ? Pl::CLForm::ctx_bind('t', $result)
           : $ctx == SCALAR_CTX
               ? Pl::CLForm::ctx_bind('nil', $result)
           : $result;
    }
  }

  # `\(@a) = LIST` — refaliasing an array's ELEMENT SLOTS (perlref, "Assigning
  # to References": `\(@x) = \(@y)` makes @x's elements the same scalars as
  # @y's, and the array is resized to the right-hand length).  All three
  # spellings — `\(@a)`, `\my(@x)`, `\(my @x)` — reach here as the RVALUE form
  # `(p-list-scalar (p-refgen-list @a))`, a list of FRESH refs, which is not a
  # \-cast place: p-setf's alias arm never saw it and the write landed in a
  # throwaway box, SILENT WRONG (task #332).  Recognised on the FORM, which is
  # the one shape the three spellings share, and lowered to the \-cast place
  # `(p-backslash-list @a)` so it goes through the SAME alias dispatch as
  # `\$x = REF`.  Runs BEFORE right-gen: the right-hand side is a LIST here
  # (`\(@a) = (\$x,\$y)` had been emitting the comma-operator scalar form).
  if ($op eq '=' && defined(my $tgt = _refgen_list_place_target($left))) {
    # A whole ARRAY is the only lvalue perl allows in this position: `\(%h) =`
    # and `\(@$ref) =` are compile errors there ("Can't modify reference to
    # parenthesized hash in list assignment"), and rule 12 says the compiler
    # says so rather than writing into a temporary.
    die "PCL: refaliasing target not supported: " . Pl::CLForm::to_flat($left) . "\n"
      unless !ref($tgt) && $tgt =~ /^(?:[\w:]+::)?\@\w+$/;
    $self->expr_o->set_node_context($kids->[1], LIST_CTX);
    my $rhs = $self->gen_node_form($kids->[1]);
    return ['p-setf', ['p-backslash-list', $tgt], $rhs];
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
    # \($x) = LIST — a one-element \(…) lvalue is still a LIST assignment
    # (see _is_backslash_paren_lvalue); re-wrap it into the (vector …) shape
    # the branch below already handles, rather than adding a second list path.
    if ($left_flat =~ /^\(p-backslash / && $self->_is_backslash_paren_lvalue($kids->[0])) {
      $left      = ['vector', $left];
      $left_flat = Pl::CLForm::to_flat($left);
    }
    # A \-cast LVALUE is refaliasing, and p-setf's place dispatch is what knows
    # it (task #325).  This must precede the sigil tests below: a PACKAGE
    # target emits `(p-backslash main::%a)`, whose text matches the `::%`
    # hash-assignment regex and would lower as an ordinary hash assignment.
    if ($left_flat =~ /^\(p-backslash(?:-sub)? /) {
      return ['p-setf', $left, $right];
    }
    if ($left_flat =~ /^\(vector[ )]/) {
      my $ctx = defined $node_id ? $self->expr_o->get_node_context($node_id) : 0;
      my $result = ['p-list-=', $left, $right];
      return $ctx == LIST_CTX
               ? Pl::CLForm::ctx_bind('t', $result)
           : $ctx == SCALAR_CTX
               ? Pl::CLForm::ctx_bind('nil', $result)
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
      # Kind-A `elem-setf` (Pl::Passes; ExprToCL2's W11 element-write rule,
      # folded here in Phase A): a plain element of a LET-BOUND lexical
      # container writes through CL's setf directly — p-setf's arm adds only
      # a `boundp` auto-declare, which a lexical never needs and which
      # manufactures a phantom GLOBAL of the same name.  Conditions: an
      # unqualified `%name`/`@name` in `lexicals`, not a state-renamed
      # container, and a key/index form free of side effects (setf
      # evaluates the key BEFORE the value; perl and p-setf evaluate the
      # value first — with a pure key the order cannot be observed).
      return ['setf', $left, $right] if $self->_elem_setf_ok($left);
      return ['p-setf', $left, $right];
    # The sigil tests read the EMITTED text, so they must see through the
    # pipe-quoting a non-ASCII name carries (#418): `|@Ｘ|` and
    # `|ＦＯＯ|::|@Ｘ|` are array targets exactly as `@x` and `Foo::@x` are.
    # Without it the quoted name matched none of the three and fell to the
    # generic p-setf tail — a DIFFERENT assignment operator (measured).
    # The quoted alternative requires a WORD-SHAPED name after the sigil:
    # `|$\||`, `|@#|`, `|${^WARNING_BITS}|` are the PUNCTUATION/caret magic
    # globals, which have always taken the p-setf tail (their places have
    # bespoke setf arms) and must keep taking it.
    } elsif ($left_flat =~ /(?:^|::)(?:@|\|@(?=[^\W\d]))/) {
      return ['p-array-=', $left, $right];
    } elsif ($left_flat =~ /(?:^|::)(?:%|\|%(?=[^\W\d]))/) {
      return ['p-hash-=', $left, $right];
    } elsif ($left_flat =~ /(?:^|::)(?:\$|\|\$(?=[^\W\d]))/) {
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
      return Pl::CLForm::ctx_bind('nil', [$cl_op, $left, $right])
        if $ctx == SCALAR_CTX;
      return Pl::CLForm::ctx_bind('t', [$cl_op, $left, $right])
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
# A statement keyword is never a FUNCTION, and `if` is in %RUNTIME_NAMES, so a
# bareword one lowered to the p-if MACRO with whatever arity it happened to
# have: `my $x = if if if` (legal perl when a lexical sub is named `if` —
# t/op/lexsub.t asserts it) emitted `(p-if (p-if) (p-my-= $x (p-if)))`, a
# zero-argument p-if whose MACROEXPANSION error killed the whole file at load.
# A statement the compiler cannot lower is a DROP — announced, one statement
# lost, the rest of the program runs (#138 family, task #374).
#
# The word list is Pl::PExpr::Config's `statement_keywords`, not a local copy:
# the same six words are already inlined as a regex in several parser sites and
# an seventh copy here would be the drift rule 11 exists to stop.
sub gen_funcall_form {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  if (@$kids >= 1) {
    my $fn = $self->expr_o->get_a_node($kids->[0]);
    if (ref($fn) eq 'PPI::Token::Word' && $fn->can('content')
        && $self->expr_o->statement_keywords->{ $fn->content // '' }) {
      # NOT a `PCL:`-prefixed message: that prefix is the ruled-refusal channel
      # (_shape_expr_error re-raises it, killing the file).  This is a DROP —
      # one statement replaced by nil, announced, the program runs on.
      die "statement keyword `" . $fn->content . "` used as a function\n";
    }
  }

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
        # The body is always forms (structural, or v1's text as one raw form
        # — Phase B3); body-less = an analysis-only tree, never emitted.
        my $bf = $arg_node->{body_form}
          or die "PCL internal: analysis-only inline_lambda (eval) reached emission\n";
        return $wrap->(['p-eval-block', @$bf]);
      }
      elsif ($arg_node->{type} eq 'func_ref') {
        my $func_ref = $self->gen_node_form($kids->[1]);
        return $wrap->(['p-eval-block', ['funcall', $func_ref]]);
      }
      else {
        # Internal node that is NOT a block form = interpolated/computed
        # STRING — still eval STRING, must carry the capture alist.
        return $self->_gen_eval_string_form($self->gen_node_form($kids->[1]));
      }
    }
    else {
      # eval STRING (plain string literal) with the caller's in-scope
      # lexicals as an alist (docs/eval-lexical-capture.md).
      return $self->_gen_eval_string_form($self->gen_node_form($kids->[1]));
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
          return [$cl_func, cl_sym($label_node->content())];
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
          my $catch = $parser && $parser->lex_home->{_catch_labels};
          if ($catch && $catch->{$label}) {
            return ['throw', $catch->{$label}, 'nil'];
          }
          return ['go', ':' . cl_sym($label)];
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
        # do { BLOCK } parsed as inline_lambda: the body is always forms
        # (structural, or v1's text as one raw form — Phase B3).
        my $ctx  = $self->expr_o->get_node_context($node_id);
        my $bf = $arg_node->{body_form}
          or die "PCL internal: analysis-only inline_lambda (do) reached emission\n";
        return ['progn', @$bf] if $ctx == INHERIT_CTX;
        return $self->_ctx_wrap_form(['progn', @$bf], $ctx);
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

  # readline(BAREWORD) / select(BAREWORD): the arg is a filehandle name — and
  # it goes out through `_fh_sym`, the ONE handle-name emitter, or this site
  # spells a name the `open` beside it does not: `readline(main::FH2)` used to
  # keep its qualifier while `open(main::FH2, …)` canonicalised to `FH2`, which
  # is two symbols for one handle and a read that finds nothing (#491).
  if (($func_name eq 'readline' || $func_name eq 'select') && @$kids == 2) {
    my $head = $func_name eq 'readline' ? 'p-readline' : 'p-select';
    my $fh_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($fh_node) eq 'PPI::Token::Word' && $fh_node->can('content')) {
      return [$head, "'" . $self->_fh_sym($fh_node->content() // '')];
    }
    if ($self->expr_o->is_internal_node_type($fh_node) && $fh_node->{type} eq 'funcall') {
      my $fh_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$fh_kids == 1) {
        my $word_node = $self->expr_o->get_a_node($fh_kids->[0]);
        if (ref($word_node) eq 'PPI::Token::Word' && $word_node->can('content')) {
          return [$head, "'" . $self->_fh_sym($word_node->content() // '')];
        }
      }
    }
  }

  # tied($a[i]) / tied($h{k}): needs the box for identity tracking.
  if ($func_name eq 'tied' && @$kids == 2) {
    my ($kind, $container, @keys) = $self->_elem_container_key($kids->[1]);
    $kind //= '';
    return ['p-tied', ['p-aref-box',    $container, $keys[0]]] if $kind eq 'a_acc';
    return ['p-tied', ['p-gethash-box', $container, $keys[0]]] if $kind eq 'h_acc';
  }

  # pos($a[i]) / pos($h{k}): needs the box for *p-match-pos* tracking.
  if ($func_name eq 'pos' && @$kids == 2) {
    my ($kind, $container, @keys) = $self->_elem_container_key($kids->[1]);
    $kind //= '';
    return ['p-pos', ['p-aref-box',    $container, $keys[0]]] if $kind eq 'a_acc';
    return ['p-pos', ['p-gethash-box', $container, $keys[0]]] if $kind eq 'h_acc';
  }

  # delete on array/hash elements and slices: pass container + key/index.
  if ($func_name eq 'delete' && @$kids == 2) {
    my ($kind, $container, @keys) = $self->_elem_container_key($kids->[1]);
    my %head = (
      'a_acc'          => 'p-delete-array',
      'h_acc'          => 'p-delete',
      'a_ref_acc'      => 'p-delete-array',
      'h_ref_acc'      => 'p-delete',
      'slice_h_acc'    => 'p-delete-hash-slice',
      'slice_a_acc'    => 'p-delete-array-slice',
      'kv_slice_h_acc' => 'p-delete-kv-hash-slice',
      'kv_slice_a_acc' => 'p-delete-kv-array-slice',
    );
    # Without the two ref arms the element lowered as a VALUE and delete got
    # one argument — an arity crash, not a wrong answer.
    return [$head{$kind}, $container, @keys] if $kind && $head{$kind};
  }

  # exists on array/hash elements, refs, and sub/coderef existence.
  if ($func_name eq 'exists' && @$kids == 2) {
    if (my ($pkg, $name) = $self->_amp_sub_name($kids->[1])) {
      return ['p-sub-exists', "\"$pkg\"", "\"$name\""];
    }
    if (defined(my $amp_id = $self->_amp_cast_operand_id($kids->[1]))) {
      return ['p-coderef-exists-p', $self->gen_node_form($amp_id)];
    }
    my ($kind, $container, @keys) = $self->_elem_container_key($kids->[1]);
    my %head = (
      'a_acc'     => 'p-exists-array',
      'h_acc'     => 'p-exists',
      'a_ref_acc' => 'p-exists-array',
      'h_ref_acc' => 'p-exists',
    );
    return [$head{$kind}, $container, $keys[0]] if $kind && $head{$kind};
  }

  # defined: sub/coderef defined check, and bareword-filehandle check.
  if ($func_name eq 'defined' && @$kids == 2) {
    if (my ($pkg, $name) = $self->_amp_sub_name($kids->[1])) {
      return ['p-sub-defined', "\"$pkg\"", "\"$name\""];
    }
    if (defined(my $amp_id = $self->_amp_cast_operand_id($kids->[1]))) {
      return ['p-coderef-defined-p', $self->gen_node_form($amp_id)];
    }
    # …and the bareword-filehandle forms, which read the node itself.  The
    # shape test is the one every other handle site asks (#491), so
    # `defined main::FH` is the handle `FH` here as it is at open/print.
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($arg_node) eq 'PPI::Token::Word') {
      my $name = $arg_node->content();
      if (Pl::Environment::fh_bareword_shape($name)) {
        return ['p-defined-fh', "'" . $self->_fh_sym($name)];
      }
    }
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids2 = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids2 == 1) {
        my $fn_node = $self->expr_o->get_a_node($arg_kids2->[0]);
        if (ref($fn_node) eq 'PPI::Token::Word') {
          my $name = $fn_node->content();
          if (Pl::Environment::fh_bareword_shape($name)) {
            return ['p-defined-fh', "'" . $self->_fh_sym($name)];
          }
        }
      }
    }
  }

  # undef &funcname — undefine a sub (keeps it in the exists table).
  if ($func_name eq 'undef' && @$kids == 2) {
    if (my ($pkg, $name) = $self->_amp_sub_name($kids->[1])) {
      return ['p-undef-sub', "\"$pkg\"", "\"$name\""];
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

    # open(FH, DUP-MODE, BAREWORD): the THIRD argument of a DUP open is a
    # filehandle DESIGNATOR, not a filename, so a bareword there is a handle
    # NAME and goes out through `_fh_sym` like readline's above (task #594).
    # `open(my $d, ">&", STDOUT)` used to emit the BARE CL symbol and die
    # "The variable STDOUT is unbound", taking the whole top-level form with
    # it — the third site of one family: #452 fixed `<FH>` and print's `:fh`
    # slot, #491 the qualified spellings, and neither reached here.
    #
    # An ARGUMENT override rather than an arm of its own above, so the
    # generic tail's CONTEXT WRAP still applies: an early return dropped the
    # `(p-list-ctx …)` around `ok(open(F, ">&", STDOUT))` — measured on
    # t/io/dup.t, and the kind of shape change an emission A/B is for.
    my $dup_src = ($func_name eq 'open' && @$kids == 4 && $i == 3)
                ? $self->_dup_source_handle($kids->[2], $kids->[3]) : undef;

    my $saved_lvalue = $self->lvalue_context;
    if    ($needs_lvalue)               { $self->lvalue_context(1) }
    elsif (index($cl_func, 'pl-') >= 0
           && $self->_is_elem_arg($kids->[$i])) { $self->lvalue_context('argbox') }
    my $arg = defined $dup_src ? $dup_src : $self->gen_node_form($kids->[$i]);
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
    return Pl::CLForm::ctx_bind('t', $call);
  }

  # do FILE: same ctx-wrap as a user sub (do is a built-in, so it needs an
  # explicit case ahead of the built-in list-only default below).
  if ($func_name eq 'do') {
    return $self->_ctx_wrap_form($call, $ctx);
  }

  # User sub calls: always bind *wantarray*; built-ins only in list context.
  if (!exists $RUNTIME_NAMES{$func_name}) {
    # Kind-A `insensitive-call` (Pl::Passes): a KNOWN user sub whose body
    # never observes its context — no `wantarray`, every `return` scalar-
    # rooted (Parser2::_sub_ctx_insensitive, the fact rides sub_info) — is
    # called WITHOUT the *wantarray* bind: the callee cannot tell, and the
    # bind is a special-variable rebinding per call.  ExprToCL2's native
    # funcall rule, folded here (Phase A); the general form is the bind.
    my $info = $self->sub_info->{$func_name};
    return $call
      if $info && $info->{insensitive} && Pl::Passes::enabled('insensitive-call');
    return $self->_ctx_wrap_form($call, $ctx);
  }
  return $ctx == LIST_CTX
      ? Pl::CLForm::ctx_bind('t', $call)
      : $call;
}


# Form variants of _wrap_wantarray_ctx / _ctx_wrap for E2-converted
# emitters: same logic, CLForm output (flat-prints to the same bytes).
sub _wrap_wantarray_ctx_form {
  my ($self, $call, $ctx) = @_;
  return $call if $ctx == INHERIT_CTX;
  return $call if $self->environment && $self->environment->tail_position;
  return Pl::CLForm::ctx_bind($ctx == LIST_CTX ? 't' : 'nil', $call);
}

# elem-setf licence (see the `=` dispatch): LEFT is a lowered element place
# whose container is a let-bound lexical and whose key/index is PURE — an
# atom (literal, symbol) or a tree of the arithmetic/string/comparison ops
# over atoms.  Anything with a call, a deref or an assignment in it is not.
my %PURE_HEAD = map { $_ => 1 }
  qw(p-+ p-- p-* p-/ p-% p-** p-. p-x p-neg p-! p-not
     p-== p-!= p-< p-<= p-> p->= p-<=>
     p-str-eq p-str-ne p-str-lt p-str-gt p-str-le p-str-ge p-str-cmp
     p-and p-or p-&& p-|| p-// p-scalar);
sub _pure_form {
  my ($f) = @_;
  return 1 unless ref $f;                                   # atom
  return 0 unless ref $f eq 'ARRAY' && @$f && !ref $f->[0];
  return 0 unless $PURE_HEAD{ $f->[0] };
  _pure_form($_) or return 0 for @{$f}[1 .. $#$f];
  return 1;
}
sub _elem_setf_ok {
  my ($self, $left) = @_;
  return 0 unless Pl::Passes::enabled('elem-setf');
  return 0 unless ref $left eq 'ARRAY' && @$left == 3 && !ref $left->[0]
    && ($left->[0] eq 'p-gethash' || $left->[0] eq 'p-aref');
  my $container = $left->[1];
  return 0 if ref $container || $container !~ /^[%@]\w+$/;
  return 0 unless $self->lexicals->{$container};
  return 0 if $self->environment
    && exists +($self->environment->state_var_renames // {})->{$container};
  return _pure_form($left->[2]);
}

sub _ctx_wrap_form {
  my ($self, $call, $ctx) = @_;
  return $call if $ctx == VOID_CTX
               && $self->environment && $self->environment->wa_void_active;
  my $wa = $ctx == LIST_CTX ? 't' : $ctx == VOID_CTX ? ':void' : 'nil';
  return Pl::CLForm::ctx_bind($wa, $call);
}

# `eval STRING` — the call, with everything the eval SITE knows that the eval
# TEXT cannot: the caller's in-scope lexicals (arg 2) and the perl features in
# effect here (arg 3, #364).  Both are compiler INPUT for the string's own
# transpile, and both are therefore part of the runtime's eval cache key.
# Shorter forms are kept EXACTLY as they were — no features and no lexicals
# still emits a plain (p-eval STRING) — so nothing that has neither moves.
sub _gen_eval_string_form {
  my ($self, $arg_form) = @_;
  my $alist = $self->_eval_lexical_alist;
  my $feats = $self->_eval_site_features;
  return ['p-eval', $arg_form, ($alist || 'nil'), $feats] if $feats;
  return $alist ? ['p-eval', $arg_form, $alist] : ['p-eval', $arg_form];
}

# The features in effect at this eval site, as a quoted CL list of names, or
# '' when there are none.  Published per statement by Parser2 (#364).
sub _eval_site_features {
  my ($self) = @_;
  my $parser = ($self->expr_o && $self->expr_o->can('has_parser')
                && $self->expr_o->has_parser) ? $self->expr_o->parser : undef;
  return '' if !$parser;
  my $f = $parser->lex_home->{_eval_site_features};
  return '' if !$f || !@$f;
  return Pl::CLForm::raw("'(" . join(' ', map { "\"$_\"" } @$f) . ")");
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
  my $lb = $parser->lex_home->{_let_bound_vars} // {};
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
  # (The two poisoned-`my` renames `__cond__N` and `__emb__N` are GONE with
  # #291 — a lexical and a package global of the same name coexist now, so
  # nothing mints those suffixes and they have no entries below.  `__shadow__`
  # survives as the v1-SEAM rename only.)
  # `state` renames
  # (`__state__`) are deliberately NOT stripped: those are defvar'd cells, not
  # let bindings, and never enter _let_bound_vars — their pairs arrive via
  # _eval_state_captures below (s418, #401).
  my $skey = sub {
    my ($v) = @_;
    $v =~ s/__lex__\d+$//;
    my $d = $v =~ s/__shadow__(\d+)$// ? $1
          : $v =~ s/__file__(\d+)$//   ? $1
          : $v =~ s/__excl__(\d+)$//   ? $1 : -1;
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
    # KEY is the perl name (a STRING the eval body is matched against); the
    # VALUE is the CL symbol, so it takes the #418 spelling.
    push @pairs, ['cons', "\"$key\"", cl_sym($v)];
    $seen{$key} = 1;
  }
  # State cells (s418, #401): a `state $x` in a named sub is a defvar'd cell
  # `$x__state__N`; the eval body names the ORIGINAL `$x`.  Parser2 registers
  # original→cell when the decl statement lowers (_eval_state_captures,
  # scoped by _lower_sub's save/restore).  APPENDED after the let-bound pairs
  # so a live `my $x` shadow inside the sub wins, and BEFORE the span pairs —
  # the state decl is sub-level, inner to any file-span cell of the name.
  # The cell is a defvar'd box, so eval writes propagate like the span cells.
  my $stcap = $parser->lex_home->{_eval_state_captures} // {};
  for my $key (sort keys %$stcap) {
    next if $seen{$key};
    push @pairs, ['cons', "\"$key\"", cl_sym($stcap->{$key})];
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
  my $span = $parser->lex_home->{_eval_span_captures} // {};
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
  } elsif (ref($obj_node) eq 'PPI::Token::Word'
           && $self->environment
           && $self->environment->is_filehandle($obj_node->content())) {
    # A registered bareword HANDLE in invocant position is a NAME, exactly as
    # in the funcall branch above (#491).  PExpr leaves a REGISTERED handle as
    # a plain Word instead of a funcall, so this slot would otherwise take the
    # leaf emitter's handle DESIGNATOR — a CL symbol — and p-method-call would
    # resolve it against the wrong package (`Colour::H1->getline` reported
    # "via package main").  Same form, same string, either way the word came.
    $obj = ['p-resolve-invocant', '"' . $obj_node->content() . '"'];
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


# E2 form variant of gen_ref_funcall.  No operand-text inspection; converts
# fully.  Same ctx-wrap discipline as gen_methodcall_form / gen_funcall_form.
sub gen_ref_funcall_form {
  my ($self, $node, $node_id, $kids) = @_;
  # `->(` is the FOURTH member of the paren-scalar-base family — the invocant
  # of a postfix arrow is a single scalar value, whatever the surrounding
  # context (perl evaluates it in scalar context; `(sub{…})->()` is a call, not
  # a one-element list).  The other three members (`->method`, `->[i]`, `->{k}`)
  # already ask _is_paren_scalar_base; this one did not, so in LIST context the
  # paren group lowered to `(vector (lambda …))` and p-funcall-ref rejected the
  # vector — "Not a CODE reference", fatal, whole file (task #516).  In SCALAR
  # context the progn collapsed on its own, which is why `my $s = (sub{…})->()`
  # always worked and the same expression inside a `print` list did not.
  my $ref = $self->_is_paren_scalar_base($kids->[0])
            ? $self->_gen_scalar_deref_base_form($kids->[0])
            : $self->gen_node_form($kids->[0]);
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
      # p-backslash-sub-ref, not (p-backslash (p-get-coderef …)): a NAME with
      # no body is late-bound exactly as `\&NAME` is (task #517).  p-get-coderef
      # answers NIL there, and the \-wrap made that a SCALAR ref whose call
      # reached AUTOLOAD with an empty name.
      return ['p-backslash-sub-ref', $inner];
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
        if (@$tv_kids == 1
            && !$self->_is_list_node_for_refgen($tv_kids->[0], 'spread')) {
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
    $arr = $self->_bare_container_sym($arr_node, $arr, q(@));
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
    return cl_pkg($pkg) . '::' . cl_sym("${sigil}${name}");
  }
  # An UNQUALIFIED name still needs the #418 spelling: this is the entry point
  # the foreach loop variable and the try/catch variable use for their BINDING
  # token, and the body's uses come through gen_symbol_form, which quotes.
  return cl_sym($name_in);
}

# The container and subscript(s) of an ELEMENT or SLICE access, decomposed
# once for every builtin arm that needs them (#387 families 2 + 10).  Given
# the node id of a builtin's single argument, answers
#     ($kind, $container, @keys)
# where $kind is the access node's type and $container is the form to hand
# the runtime for the aggregate; () when ARG is not such an access, so the
# caller falls through to the generic funcall path exactly as before.
#
# The container is built by the rule its kind needs:
#   a_acc / h_acc        the container's TEXT with its scalar sigil swapped
#                        for the aggregate one;
#   a_ref_acc/h_ref_acc  (unbox FORM) — the container is a reference;
#   slice_h_acc          @h{…} names the hash, so @ → %;
#   kv_slice_a_acc       %a[…] names the array, so % → @;
#   slice_a_acc / kv_slice_h_acc   the slice token already names the right
#                        aggregate — no swap.
#
# A SIGIL SWAP HAPPENS ONLY FOR A BARE Symbol/Magic CONTAINER.  That guard is
# load-bearing, not tidiness: _swap_elem_sigil's regex is unanchored, so on a
# nested access's already-generated text it also rewrites a package-qualified
# INDEX — `$a[$i]{$k}` with `our $i` emitted (p-aref @a Pkg::%i …), a
# reference to a different variable (ac6fdc1).  exists/delete were guarded
# then; tied/pos and the kv_slice_a_acc swap were not, and this unification
# is what fixes them (task #397).
sub _elem_container_key {
  my ($self, $arg_id) = @_;
  my $node = $self->expr_o->get_a_node($arg_id);
  return () unless $self->expr_o->is_internal_node_type($node);
  my $kind = $node->{type} // '';
  my %swap = ('a_acc'          => [q($), q(@)],
              'h_acc'          => [q($), q(%)],
              'slice_h_acc'    => [q(@), q(%)],
              'kv_slice_a_acc' => [q(%), q(@)]);
  my $is_slice = $kind =~ /^(?:kv_)?slice_[ah]_acc$/;
  return () unless $is_slice || $kind =~ /^[ah]_(?:ref_)?acc$/;

  # A SLICE needs only its container: `scalar delete @h{()}` ([perl #29127])
  # is a legal one-child node and must reach the slice runtime — the two
  # siblings that demanded a subscript (slice_a_acc, kv_slice_h_acc) fell
  # through to the scalar `delete`, which then CRASHED on arity: probed,
  # `delete @a[()]` = "invalid number of arguments: 1" where perl gives undef.
  my $kids = $self->expr_o->get_node_children($arg_id);
  return () unless $kids && @$kids >= ($is_slice ? 1 : 2);

  my $container;
  if ($kind =~ /_ref_acc$/) {
    $container = ['unbox', $self->gen_node_form($kids->[0])];
  }
  else {
    my $c_node = $self->expr_o->get_a_node($kids->[0]);
    $container = $self->gen_node($kids->[0]);
    if ($swap{$kind}
        && (ref($c_node) eq 'PPI::Token::Symbol'
            || ref($c_node) eq 'PPI::Token::Magic')) {
      $container = $self->_bare_container_sym($c_node, $container,
                                              $swap{$kind}[1], $swap{$kind}[0]);
    }
    # A SLICE through a reference (`delete @$h{a}`, `exists @$r[0]`) has a
    # SCALAR container the swap cannot touch — the same hole the four slice
    # emitters carried, and the same one-line answer (s426, task #420).
    $container = $self->_slice_container_form($kids->[0], $container,
                   $kind =~ /_a_acc$/ ? 'p-cast-@' : 'p-cast-%')
      if $is_slice;
  }
  my @keys = map { $self->gen_node_form($kids->[$_]) }
             ($is_slice ? (1 .. $#$kids) : (1));
  return ($kind, $container, @keys);
}

# The BARE CONTAINER of an element access, as symbol TEXT.  Perl swaps the
# sigil on the NAME — `$h{k}` reads %h — and PCL swaps it on the RENDERED
# scalar, which is exactly right for every name whose render IS a symbol:
# `$h` → `%h`, `|$-|` → `|@-|`, `|Foo|::$h` → `|Foo|::%h`.
#
# It is not right for a %SPECIAL_VARS name that renders as a compound FORM.
# `$!` is `(p-errno-string)`, and no sigil swap reaches inside a call, so
# `$!{ENOENT}` emitted (p-gethash (p-errno-string) "ENOENT") — a symbolic hash
# whose NAME was the strerror text — and answered undef, silently, instead of
# reading %! (task #561).  The four such names today are `$!`, `$^E` and the
# two `['p-undef']` caret stubs; for those, and only those, the container is
# re-rendered from the AGGREGATE spelling through this same one renderer, so
# `%!`/`@!` and `%{^LAST_FH}` come out the way every other container does.
# Every other name keeps the text swap and its bytes.
sub _bare_container_sym {
  my ($self, $node, $rendered, $to, $from) = @_;
  $from = q($) unless defined $from;
  my $content = $node->content() // '';
  if (ref $SPECIAL_VARS{$content}) {
    (my $agg = $content) =~ s/^\Q$from\E/$to/;
    return Pl::CLForm::to_flat($self->gen_symbol_form($node, $agg))
      if $agg ne $content;
  }
  return _swap_elem_sigil($rendered, $to, $from);
}

sub _swap_elem_sigil {
  my ($sym, $sigil, $from) = @_;
  my $f = quotemeta(defined $from ? $from : q($));   # $ unless told otherwise
  # The sigil always opens the NAME, and the name has exactly two spellings:
  # BARE (`$x`, `Foo::$x`) or #418 pipe-quoted (`|$Ｘ|`, `|ＦＯＯ|::|$Ｘ|`),
  # which differ only by the `|` in front of the sigil.  ONE pattern with an
  # optional `|` covers both — the older two-alternative form matched a whole
  # `|$x|` token but not the QUALIFIED `Pkg::|$x|`, so `"$main::Ｌ[0]"` asked
  # p-aref for the SCALAR `$Ｌ` and died in gethash (measured s423).
  $sym =~ s/(^|::)(\|?)$f/$1$2$sigil/;
  return $sym;
}


# True when NODE is a PARENTHESISED base of a postfix `->` — a tree_val or
# progn, e.g. the `($r//0)` in `($r//0)->[i]`.  The invocant of an arrow is ONE
# scalar value in every context, so such a base is a scalar REF being
# dereferenced, not a list.
#
# A MULTI-element group is the same case (#527): perl's rule is the comma
# operator's LAST element in scalar context, so `(1,2,$r)->[1]` is 20 — every
# element is still evaluated for its side effects, which is exactly what the
# scalar-context progn this licenses already does.  Requiring exactly one child
# made `(1,2,$r)->[1]` answer 2 (silent wrong — the group lowered as a list and
# the subscript indexed IT), and made `->{k}` / `->(…)` / `->method` die "Not a
# HASH reference" / "Not a CODE reference" / "on unblessed reference".
# The LIST-SLICE spelling `(1,2,$r)[1]` is a different operator and never
# arrives here — PExpr marks it `list_ctx_subscript` and gen_array_ref_access_form
# refuses the scalar base for it.
sub _is_paren_scalar_base {
  my ($self, $node_id) = @_;
  my $node = $self->expr_o->get_a_node($node_id);
  return 0 unless $self->expr_o->is_internal_node_type($node);
  my $type = $node->{type} // '';
  return 0 unless $type eq 'tree_val' || $type eq 'progn';
  my $kids = $self->expr_o->get_node_children($node_id);
  return $kids && @$kids >= 1;
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

# The KEY of a hash element (#387 family 20, s413 — the half that was
# identical between the bare `$h{…}` and the `->{…}` access): a multi-key
# `$h{a,b,c}` (a progn of 2+ parts) joins with $; — (p-join |$;| (vector …));
# anything else is the key expression itself.
sub _hash_key_form {
  my ($self, $key_id) = @_;
  my $key_node = $self->expr_o->get_a_node($key_id);
  if ($self->expr_o->is_internal_node_type($key_node)
      && $key_node->{type} eq 'progn') {
    my $key_kids = $self->expr_o->get_node_children($key_id);
    return ['p-join', '|$;|', ['vector', map { $self->gen_node_form($_) } @$key_kids]]
      if @$key_kids > 1;
  }
  return $self->gen_node_form($key_id);
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

  my $key = $self->_hash_key_form($kids->[1]);

  if ($is_bare) {
    $hash = $self->_bare_container_sym($hash_node, $hash, q(%));
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
  my $key = $self->_hash_key_form($kids->[1]);
  my $func = $self->lvalue_context ? 'p-gethash-deref-box' : 'p-gethash-deref';
  return [$func, $ref, $key];
}

# The index/key operands of a slice node — children 1.. of $kids, each put in
# LIST context and lowered, in order (#387 family 17, s413: the loop the four
# slice emitters below each spelled).  The container (child 0) is lowered
# by the caller FIRST, as before.
sub _slice_index_forms {
  my ($self, $kids) = @_;
  my @forms;
  for my $i (1 .. $#$kids) {
    $self->expr_o->set_node_context($kids->[$i], LIST_CTX);
    push @forms, $self->gen_node_form($kids->[$i]);
  }
  return @forms;
}

# A slice whose CONTAINER is a scalar is a slice THROUGH A REFERENCE —
# `@$r[0,1]`, `@{$r}[1]`, `@$h{a,b}`, `@{$h}{a,b}` — and the reference has
# to be dereferenced before p-aslice/p-hslice index it.  The ELEMENT sibling
# has always done this (`$$r[1]` lowers to p-aref-deref, `$$h{k}` to
# p-gethash-deref); the slice emitters handed the raw box straight to the
# slice runtime, which then indexed the REFERENCE.  What that cost depends
# on how many box layers the ref carries, which is why it looked like it
# worked: `my $ao=[7,8,9]; @$ao[1]` is right, `my $r=\@named; @$r[0,1]` came
# back all-undef (SILENT WRONG) and every hash spelling DIED "Not a HASH
# reference" (probed vs perl 5.40.3, s426 — task #420's `"@$r[0,1]"` row).
# p-cast-@ / p-cast-% peel whichever layering is there and are the identity
# on a container that is already the aggregate.
#
# A bare Symbol/Magic naming the aggregate (`@a`, `%h`, `@-`, `@{name}`) IS
# the container; anything else in container position is a scalar value, and
# a scalar there can only be a reference.
sub _slice_container_form {
  my ($self, $kid_id, $form, $cast) = @_;
  my $node = $self->expr_o->get_a_node($kid_id);
  my $r = ref $node;
  if ($r eq 'PPI::Token::Symbol' || $r eq 'PPI::Token::Magic') {
    return $form if ($node->content // '') =~ /^[\@\%]/;
  }
  return [$cast, $form];
}

# @a[i,j] → (p-aslice @a i j), context-wrapped.
sub gen_array_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  my $arr = $self->_slice_container_form($kids->[0],
              $self->gen_node_form($kids->[0]), 'p-cast-@');
  return $self->_slice_in_context_form(['p-aslice', $arr, $self->_slice_index_forms($kids)], $node_id);
}

# @h{a,b} → (p-hslice %h a b), context-wrapped (bare Symbol @→% sigil rewrite).
sub gen_hash_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  my $hash_node = $self->expr_o->get_a_node($kids->[0]);
  my $is_bare = ref($hash_node) eq 'PPI::Token::Symbol';
  my $hash = $is_bare ? $self->gen_node($kids->[0]) : $self->gen_node_form($kids->[0]);
  # ONE swap helper (rule 11): _swap_elem_sigil also knows the PIPE-QUOTED
  # spelling (|@Ｘ| → |%Ｘ|), which this local copy did not — a non-ASCII hash
  # slice handed p-hslice the ARRAY of the same name and died "Not a HASH
  # reference" (#418).  The `$is_bare` guard stays: on a nested access's
  # already-generated text the unanchored swap would rewrite an index.
  $hash = _swap_elem_sigil($hash, q(%), q(@)) if $is_bare;
  $hash = $self->_slice_container_form($kids->[0], $hash, 'p-cast-%');
  return $self->_slice_in_context_form(['p-hslice', $hash, $self->_slice_index_forms($kids)], $node_id);
}

# %h{a,b} → (p-kv-hslice %h a b)  (no context wrap).
sub gen_kv_hash_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  my $hash = $self->_slice_container_form($kids->[0],
               $self->gen_node_form($kids->[0]), 'p-cast-%');
  return ['p-kv-hslice', $hash, $self->_slice_index_forms($kids)];
}

# %a[i,j] → (p-kv-aslice @a i j)  (%→@ sigil rewrite; $ref base derefed).
sub gen_kv_array_slice_form {
  my ($self, $node, $node_id, $kids) = @_;
  return undef unless @$kids;  # empty SLICE normalized: text twin printed a trailing space (task #78)
  # The sigil swap is _swap_elem_sigil's (rule 11, #418): it also knows the
  # pipe-quoted spelling a non-ASCII name carries.
  my $arr = _swap_elem_sigil($self->gen_node($kids->[0]), q(@), q(%));
  # `(unbox $r)` stood here — the shape-blind half of _slice_container_form's
  # rule, right for a single-boxed anon ref and one layer short of \@named.
  my $arr_form = $self->_slice_container_form($kids->[0], $arr, 'p-cast-@');
  return ['p-kv-aslice', $arr_form, $self->_slice_index_forms($kids)];
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
    # The bareword test is UNICODE word-shape, not ASCII: `use utf8` source has
    # real bareword filehandles like `<Fʜ>` (perl's own t/uni/readline.t).  It
    # asks about the perl NAME, so it looks through the #418 pipe-quoting the
    # leaf emitter already applied — the quoted token IS the spelling the
    # runtime agrees on, so it is passed through unchanged.
    $call = ['p-readline', _bareword_fh_p($fh) ? "'" . $self->_fh_sym($fh) : $fh];
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
    # Unicode word-shape, quoted spelling included — see gen_readline_form.
    return _bareword_fh_p($fh) ? ":fh '" . $self->_fh_sym($fh) : ":fh $fh";
  }
  return ':fh nil';
}

# Is TOKEN a bareword filehandle NAME, as opposed to `$fh` / an expression?
# TOKEN is already-emitted text, so the question is asked of the perl name
# underneath the #418 pipe-quoting a non-ASCII name carries (Pl::CLForm
# spells it; this only reads it back).  The shape is perl's own \w+ under
# `use utf8` — `<Fʜ>` in perl's t/uni/readline.t is a real bareword handle.
# A BAREWORD FILEHANDLE NAME, qualified or not (task #452).  The name is
# QUOTED into the call — `(p-readline (quote FH))`, `:fh (quote FH)` — because
# a handle is a name the runtime looks up, not a variable to read; anything
# else (a lexical `$fh`, an expression) is passed through as the form it is.
#
# THE `::` HALF WAS MISSING, and it cost a whole file each time: `main::FH2`
# failed this test, so `<main::FH2>` emitted `(p-readline main::FH2)` — a BARE
# CL symbol — and died at LOAD with "The variable FH2 is unbound".  The same
# predicate serves the print/say/printf `:fh` marker, so `print main::FH5 "x"`
# died the same way.  `readline(main::FH3)` was always right, because the
# BUILTIN path quotes the name itself: one spelling of one thing, answered
# three different ways by two predicates and a builtin.
#
# UNICODE word-shape, not ASCII: `use utf8` source has real bareword handles
# (perl's own t/uni/readline.t).  It asks about the perl NAME, so it looks
# through the #418 pipe-quoting the leaf emitter already applied.
sub _bareword_fh_p {
  my ($tok) = @_;
  return Pl::CLForm::cl_unquote($tok) =~ /^[^\W\d]\w*(?:::[^\W\d]\w*)*$/ ? 1 : 0;
}

# The handle NAME a DUP open's SOURCE slot spells — as the quoted CL symbol
# every other bareword-handle site emits — or undef when this open's third
# argument is not one (task #594; see the call site for the mode gate).
#
# A DECLARED SUB IN THAT SLOT IS CALLED, probed 5.40.3: `sub SRC {"STDERR"}
# open(my $d, ">&", SRC)` dups STDERR, and it wins even when a handle of that
# name is open.  That is the same three-valued question
# `PExpr::_read_star_slot_bareword` asks for a user `(*)` slot, so it is asked
# through the same classifier rather than a fresh name test (rule 11).
# ALL-CAPS is NOT required here: `open(lch,…); open(my $d,">&",lch)` dups in
# perl, and the slot admits no other reading of a non-callable bareword.
sub _dup_source_handle {
  my ($self, $mode_id, $src_id) = @_;
  my $mode = $self->expr_o->get_a_node($mode_id);
  return undef unless ref($mode) && $mode->isa('PPI::Token::Quote')
                   && $mode->can('string');
  my $text = eval { $mode->string };
  return undef unless defined $text && index($text, '&') >= 0;
  my $word = $self->_bareword_arg_word($src_id);
  return undef unless $word;
  my $name = $word->content // '';
  return undef if $name eq '';
  return undef if $self->expr_o->_bareword_callable_here($name, $word) eq 'yes';
  return "'" . $self->_fh_sym($name);
}

# A bareword ARGUMENT node: a plain Word, or the one-child `funcall` wrapper
# the bareword classifier builds for a name it cannot place — the same two
# shapes the readline/select arm accepts, and for the same reason (whether a
# bareword arrives wrapped depends on what else the file declares).
sub _bareword_arg_word {
  my ($self, $id) = @_;
  my $n = $self->expr_o->get_a_node($id);
  return $n if ref($n) eq 'PPI::Token::Word';
  return undef unless $self->expr_o->is_internal_node_type($n)
                   && ($n->{type} // '') eq 'funcall';
  my $kids = $self->expr_o->get_node_children($id);
  return undef if @$kids != 1;
  my $w = $self->expr_o->get_a_node($kids->[0]);
  return ref($w) eq 'PPI::Token::Word' ? $w : undef;
}

# THE handle-NAME emitter (task #491).  Every site that turns a bareword
# filehandle NAME into a CL symbol goes through here, because a package-
# QUALIFIED name has two requirements an unqualified one does not:
#
#   (1) it must be READABLE — `(p-open Foo::H1 …)` is a read error, and the
#       error kills the WHOLE file, when no `package Foo` in the source made
#       the CL package exist.  A qualified CALL already registers its package
#       so the pre-pass emits `(pcl:p-defpackage :Foo)` at the top; a handle
#       name is the same fact about the same document, so it registers the
#       same way rather than growing a second mechanism (rule 11).
#   (2) it must KEEP its package.  `%p-fh-arg` re-interns an UNQUOTED bareword
#       symbol's name in the current package (that is how `open FH` and
#       `print FH` meet), which silently re-homes `P::FH2` opened from main
#       into main — so the `<P::FH2>` read, which quotes and keeps package P,
#       found nothing and returned empty.  A QUOTED designator falls through
#       `%p-fh-arg` untouched, so the reader's package choice survives.
#
# The name is CANONICALISED first (Pl::Environment::canon_filehandle_name), so
# the two spellings of one handle emit ONE symbol instead of two that have to
# find each other through %p-resolve-fh's by-name fallback: `open(main::FH,…)`
# and `print FH "x"` both emit `FH`.
#
# Unqualified names keep their unquoted spelling: byte-identical emission for
# every handle in every corpus, and `%p-fh-arg`'s re-intern is the identity
# there (the reader interned the same symbol in the same package).
sub _fh_sym {
  my ($self, $name) = @_;
  my $perl = Pl::CLForm::cl_unquote($name);
  # A PARTLY-quoted token (`|ＦＯＯ|::x`) is a CL SPELLING the caller already
  # built; naming it again would name a different symbol (CLForm's _already_cl
  # note).  cl_unquote only unwraps a WHOLE `|…|`, so "a pipe survived it" is
  # exactly that case.
  return $name if $perl eq $name && index($name, '|') >= 0;
  $perl = $self->environment->canon_filehandle_name($perl) if $self->environment;
  if ($perl =~ /\A(.+)::([^:]+)\z/ && $1 ne 'main') {
    # `Foo::STDOUT` is NOT `STDOUT` (perl: it writes nothing and returns undef,
    # probed).  A CL symbol `|Foo|::|stdout|` would be, because %p-resolve-fh
    # falls back to find-symbol on the SHORT name in :pcl — which is where
    # exactly these eight handles live.  Spelling the whole perl name as ONE
    # pipe-quoted symbol keeps it out of that fallback and needs no package.
    return Pl::CLForm::cl_whole_sym($perl)
      if Pl::Environment::fh_forced_main_name($2);
    $self->environment->add_referenced_package($1) if $self->environment;
  }
  return cl_sym($perl);
}

# The designator for a slot the RUNTIME quotes itself (`%p-fh-arg`: open,
# close, eof, binmode, fileno, seek, tell …).  Sites that quote the name
# themselves (`<FH>`, the print `:fh` marker) call _fh_sym directly.
sub _fh_designator {
  my ($self, $name) = @_;
  my $sym = $self->_fh_sym($name);
  # The CANONICAL spelling decides, not the one written: `main::FH` canonicalises
  # to `FH` and must then emit exactly what a written `FH` emits.
  return Pl::CLForm::cl_unquote($sym) =~ /::/ ? "'$sym" : $sym;
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
# Used by gen_tree_val_form to decide whether to wrap in (vector ...) or not.
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
#
# MODE picks which of two nearly-identical questions is being asked, and they
# differ on exactly ONE shape — a hash:
#   default    "does this already EVALUATE to a vector, so a paren around it
#              needs no (vector …) wrapper?"  A hash evaluates to a hash-table,
#              which is NOT a vector: `(%h)` must keep its wrapper, and `(%h) =
#              LIST` must stay a LIST assignment (op/inc.t's chained
#              `my (%orig) = my (%inc) = …` breaks otherwise).
#   'spread'   "does `\(X)` distribute over more than one element?"  A hash
#              spreads into its 2N key/value scalars, so `\(%h)` is 2N scalar
#              refs and not one `\%h` (perl, probed).
# One function so the shared part cannot drift; the trailing-character test is
# what separates the VARIABLE `%h` / `%$r` / `%{…}` from the MODULUS operator,
# whose token content is exactly "%".
sub _is_list_node_for_refgen {
  my ($self, $node_id, $mode) = @_;
  return 1 if $self->_child_is_list_expr($node_id);
  my $node = $self->expr_o->get_a_node($node_id);
  if (($mode // '') eq 'spread' && ref($node) && $node->can('content')) {
    return 1 if ($node->content() // '') =~ /^\%[\w\$\{:]/;
  }
  # Range operator .. — binary op stored as PPI::Token::Operator with children
  if (ref($node) eq 'PPI::Token::Operator') {
    return 1 if ($node->content() // '') eq '..';
  }
  return 0;
}

# True when NODE_ID is `\( … )` — a refgen whose operand is a PARENTHESIZED
# list.  Only the assignment lowering cares: as an rvalue `\($x)` really is
# `\$x` (one ref either way, which is why gen_prefix_op_form collapses it),
# but as an LVALUE the parens are what make it a LIST assignment — perl's
# `\($x) = @_` aliases $x to $_[0], while `\$x = @_` is scalar context and
# dies "not a SCALAR reference".  The multi-element spelling keeps its
# list-ness on its own (it emits a (vector …)); this is the one-element case.
sub _is_backslash_paren_lvalue {
  my ($self, $node_id) = @_;
  return 0 unless defined $node_id;
  my $kids = $self->expr_o->get_node_children($node_id);
  return 0 unless $kids && @$kids == 2;
  my $op_node = $self->expr_o->get_a_node($kids->[0]);
  return 0 unless ref($op_node) && $op_node->can('content')
                  && ($op_node->content() // '') eq '\\';
  return $self->expr_o->node_tree->get_metadata($kids->[1], 'backslash_paren_list')
           ? 1 : 0;
}

# The target form inside a `\(LIST)` lvalue, or undef when LEFT is not one.
# `\(…)` in a non-single-scalar, non-multi-term spelling emits
# `(p-refgen-list X)` — wrapped in `(p-list-scalar …)` in scalar/void context,
# which a statement-level assignment always is.  Sibling of
# _is_backslash_paren_lvalue: that one answers "is the one-element scalar
# spelling a list assignment", this one hands back X so the `=` lowering can
# decide whether X is an lvalue at all (task #332).
sub _refgen_list_place_target {
  my ($form) = @_;
  return undef unless ref $form eq 'ARRAY' && @$form == 2 && !ref $form->[0];
  $form = $form->[1] if $form->[0] eq 'p-list-scalar';
  return undef unless ref $form eq 'ARRAY' && @$form == 2 && !ref $form->[0]
                   && $form->[0] eq 'p-refgen-list';
  return $form->[1];
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
        return Pl::CLForm::ctx_bind('t', $child);
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



# (gen_anon_sub_form deleted #303/s392: the `anon_sub` node type reached this
# seam ZERO times over both populations — 111 corpus transpiles and the 138-file
# Pl/t gate.  PExpr still MAKES anon_sub nodes; Parser2 lowers them before the
# expression seam is asked.  `anon_sub` deliberately STAYS in %NAMED_TYPE so an
# arriving node hits gen_internal_node_text's rule-12 die instead of being
# lowered as a binary operator named "anon_sub" — ruled s391.)

# E2 form variant: \&foo → #'name atom.  A lambda_form is the do{}/anon-sub
# lambda the parser's embed_block answered with (structural, or v1's text as
# one raw form).  A node with neither was built by an analysis-only parse
# (PExpr `analysis_only`) — such a tree never reaches emission (rule 12).
sub gen_func_ref_form {
  my ($self, $node, $node_id, $kids) = @_;
  return $node->{lambda_form} if $node->{lambda_form};
  die "PCL internal: analysis-only func_ref reached emission\n"
    if !defined $node->{func_name};
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

# The inline lambda for grep/map/sort/eval/do bodies (task #78 form emitter;
# since Phase A4 the ONLY one).  A Parser2-lowered body arrives as body_form
# (an arrayref of CLForms); a body v1 compiled to text (a block
# lower_embedded_block declined, or an expression inside a v1-routed
# statement) arrives as body_cl and rides as one raw form.  Sort comparators
# get the :p-return catch and the scalar-context *wantarray* bind; grep/map/
# eval bodies get neither (`return` inside them must propagate to the
# enclosing sub's catch).
#
# A sort comparator lambda inside a block-level `package X;` region binds the
# REGION's pair — X::$a / X::$b — because that is what the requalified body
# reads and what perl sets (Pl::PExpr::_sort_pair has the full reasoning).
# Those two symbols are defvar'd by the region's own entry forms, but those
# run INSIDE the enclosing top-level form, so they have NOT proclaimed the
# symbols special by the time this lambda is compiled (probed s380: a nested
# defvar leaves the parameter a plain LEXICAL).  The (declare (special …))
# below is what makes the binding dynamic, so a comparator that reads the
# GLOBAL — perl's `sort NAME LIST` shape, and the `${(caller)[0]."::a"}`
# symbolic shape Sort::Versions uses — sees the values.  The section's bare
# $a/$b need nothing: their defvars are top level.
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
  # $a/$b dynamic bindings, and a ($$)-prototype sub gets the pair as
  # arguments as well.  It is a PLAIN CALL: "the comparator has no body" is
  # answered where perl answers it, at the call itself (task #468 —
  # %p-call-of-undefined-sub: the sub's OWN package's AUTOLOAD, with
  # $AUTOLOAD set, else perl's die).  This branch used to wrap the call in a
  # THIRD copy of that dispatch (`handler-case` + `intern "PL-AUTOLOAD"` in
  # the package captured at lambda creation, citing [perl #30661]) whose
  # no-AUTOLOAD arm returned nil — so `sort nonexistent LIST` compared
  # everything EQUAL where perl dies, and the arm could never fire anyway
  # ("PL-AUTOLOAD" is not the symbol %pcl-cl-sub-name "AUTOLOAD" produces).
  # Deleted in task #501; perl's AUTOLOAD-for-a-sort-name behaviour is
  # probed and preserved by the one mechanism (Pl/t/sort-01.t).
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
    # The NAME rides along (task #514): perl resolves a named comparator at
    # sort ENTRY and dies for a body-less one even when the list is empty or
    # one element long, so the check cannot live in the comparator body.  It
    # belongs to p-sort rather than to this form, because this form is an
    # ARGUMENT — evaluated BEFORE the list, whose own side effects perl runs
    # first (probed).
    return ['p-sort-named', "'$cl_func",
            ['p-sort-cmp', $params, @decl, Pl::CLForm::ctx_bind('nil', $call)]];
  }

  # Scalar comparator (sort $var LIST) — resolved at runtime by
  # p-sort-get-fn; *package* rebound to the value it has where the lambda is
  # built, so a symbolic READ inside the comparator body (${"name"} and its
  # siblings, which all consult *package*) sees the user's package rather than
  # p-sort's.  It is NOT what resolves a string COMPARATOR NAME any more: that
  # goes through %p-resolve-sub-symbol, which reads the Perl-level current
  # package (*pcl-current-package*, task #503) — this binding could not do that
  # job, because it captures *package* at CALL time, where the loader has long
  # since left the file's own (in-package …) and it reads MAIN.
  if ($for_func eq 'sort' && $node->{scalar_cmp}) {
    my $scalar = ($kids && @$kids) ? $self->gen_node_form($kids->[0]) : 'nil';
    return
      ['let', ['list', ['list', '|sort--pkg|', '*package*']],
        ['p-sort-cmp', $params, @decl,
          # TWO bindings, so this one stays a plain `let`: p-scalar-ctx names
          # the context bind alone and would drop the *package* rebind.
          ['let', ['list', ['list', '*wantarray*', 'nil'],
                           ['list', '*package*', '|sort--pkg|']],
            ['funcall', ['p-sort-get-fn', $scalar], @pair]]]];
  }

  # The body is ALWAYS forms since Phase B3 — the parser's embed_block answers
  # with structural forms or with v1's text as ONE raw form (E2's residue
  # rule); a node with no body was built by an analysis-only parse (PExpr
  # `analysis_only`), and such a tree never reaches emission (rule 12).
  my $bf = $node->{body_form}
    or die "PCL internal: analysis-only inline_lambda ($for_func) reached emission\n";
  if ($for_func eq 'sort') {
    return ['p-sort-cmp', $params, @decl,
            Pl::CLForm::ctx_bind('nil', @$bf)];
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

  # perl takes the two DELIMITERS separately, and a single-quoted one turns
  # interpolation off for its half alone: `s{A}'[$x]'` has a dq-like pattern and
  # a literal replacement (probed).
  my ($mdelim, $rdelim) = _subst_delims($node);

  # Pattern: a string literal atom, or the interpolation form ("…"/$var/
  # (p-string-concat …)) evaluated to the pattern string at runtime.
  my $match_form;
  if (_delim_interpolates($mdelim) && _has_regex_interpolation($match)) {
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

  # A SINGLE-QUOTED replacement is literal text — no interpolation, no case
  # shifts, and no backrefs either: `s'(A)'$1'` writes the two characters `$1`
  # and `s'(A)'\1'` the two characters `\1` (probed).  It is emitted as a
  # LAMBDA returning a constant rather than as the string p-subst hands to
  # cl-ppcre, because that path is exactly the one that would read `$1`/`\1` as
  # a register reference.  Only `\'` and `\\` are unescaped, as in '…'.
  if (!_delim_interpolates($rdelim)) {
    return ['p-subst', $match_form,
            ['lambda', ['list'], _cl_string_literal_form(_unescape_sq($subst))],
            @mod_strs];
  }

  # Replacement with variable interpolation (or a case-shift escape): wrap in a
  # lambda so $varname / the magics evaluate at match time and `\U…\E` reaches
  # the dq-string compiler.  A bare `$1`/`$2` replacement is deliberately NOT
  # routed here — it is served better by the runtime's native backref
  # substitution (no lambda call per match).  Widening this gate is its own
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
  # CL string literal escaping — through the one writer, so a replacement
  # holding a surrogate or an above-U+10FFFF code point is split out the same
  # way a dq literal's is (#419) instead of being written raw.
  return ['p-subst', $match_form, _cl_string_literal_form($s), @mod_strs];
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

# Escape-process a SINGLE-QUOTED s/// replacement: only `\'` and `\\` mean
# anything, everything else — `\t`, `\n`, `\1` — is the backslash and the
# character (probed 5.40.3).  Same rule '…' itself follows.
sub _unescape_sq {
  my ($str) = @_;
  my $out = '';
  my $i = 0;
  my $n = length $str;
  while ($i < $n) {
    my $c = substr($str, $i, 1);
    if ($c eq '\\' && $i + 1 < $n && substr($str, $i + 1, 1) =~ /^['\\]\z/) {
      $out .= substr($str, $i + 1, 1);
      $i += 2;
      next;
    }
    $out .= $c;
    $i++;
  }
  return $out;
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
# It used to be a hand-rolled mini-interpolator (_gen_interp_replacement_simple,
# deleted #303/s393 — it was still the fallback here, entered ZERO times over
# corpus, Pl/t gate and full sweep) whose loop understood exactly \1..\9,
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
  die "PCL internal: s/// replacement ends in an odd run of backslashes, "
    . "which cannot be quoted for the dq-string parser: '$str'\n"
    if $trailing % 2;
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
  # Rule 12: the value flows onward AS THE REPLACEMENT STRING, so a miss here
  # is silent-wrong text in the output, not a lost effect.  The hand-rolled
  # mini-interpolator this used to fall back to (_gen_interp_replacement_simple)
  # was deleted in #303/s393 after all three of its entry routes measured ZERO
  # over all three populations — corpus, Pl/t gate, full sweep.  An EMPTY
  # replacement never reaches this sub at all: gen_subst_form only calls it
  # when _replacement_interpolates says so, and `s/x//` does not (probed s393,
  # it emits (p-subst "x" "")), so an empty form here is a parse miss too.
  die "PCL internal: s/// replacement did not lower as a dq string: '$str'"
    . ($@ ? " ($@)" : " (parser returned an empty form)\n")
    if !defined $form || (!ref $form && $form eq '');
  return $form;
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


# Parse a s///e replacement string as Perl and return CL code
sub _compile_subst_e_expr {
  my $self = shift;
  my $expr = shift;

  my ($result, $err) = $self->_try_compile_subst_e($expr);
  if (!defined $result) {
    # RETRY with any interpolation-block heredoc spliced in (task #342 piece
    # 2).  Only on a replacement that already failed, so every shape that
    # compiles today keeps its emission byte-for-byte.
    my $flat = _splice_interp_heredocs($expr);
    ($result) = $self->_try_compile_subst_e($flat) if defined $flat;
  }
  # RULE 12 (task #342 piece 1): a replacement we could not compile must DIE,
  # not become nil.  `s/…/EXPR/e` REPLACES matched text with EXPR's value, so a
  # nil body is a value the program then consumes — it substituted the empty
  # string and carried on, rc 0, with only a warning on stderr nobody reads:
  #
  #     s|(?:)|"${\<<END}" … |e;      perl: the heredoc text
  #                                    PCL : "", exit 0   (before this)
  #
  # The same file could ALSO die from a different shape of the same construct
  # (base/lex.t did, rc 255), so one input path died and another silently
  # substituted nothing — that inconsistency was the bug.  Measured population
  # before the change: 7 sites in 2 files (t/op/taint.t 6, t/base/lex.t 1),
  # both of which already fail to transpile for other reasons, so no verdict
  # moved; what changed is that a future one cannot hide.
  if (!defined $result) {
    my $why = $err || "the replacement produced no form";
    $why =~ s/\s+\z//;
    die "PCL: cannot compile the s///e replacement '$expr': $why\n";
  }
  return $result;
}

# A heredoc OPENED INSIDE an interpolation block — `"${\<<END}"`, with the body
# on the lines after it (t/base/lex.t).  PPI lexes the whole `"${\<<END}"` as
# ONE Quote::Double token, so it never sees the opener, and the body and its
# terminator are left behind as loose code that the expression parser reads as
# `ok $test - heredoc END` and refuses.  perl gets this right because the
# interpolation block is compiled as code while the heredoc body follows the
# STATEMENT — and for an s///e replacement the statement IS the replacement
# text, so the body is already inside the string PCL was handed.
#
# The body is HOISTED into a `my` variable and the opener replaced by that
# variable, NOT spliced in as a literal where it stood: the opener sits inside
# the `"…"`, so a literal there would end the string at its own first quote
# (measured — `"${\"ok …"}"`, and the CL reader then choked).  A variable
# reference is quote-free, `${\ $v }` derefs it exactly as `${\ <<END}` did,
# and `_try_compile_subst_e` already turns a leading `my $v = …;` statement in
# a replacement into a `let` binding.  Returns the rewritten text, or undef
# when there was nothing to splice.
sub _splice_interp_heredocs {
  my ($text) = @_;
  my @lines = split /\n/, $text, -1;
  my ($changed, $guard) = (0, 0);
  my @hoist;
  LINE: for (my $i = 0; $i < @lines; $i++) {
    # <<IDENT / <<"IDENT" / <<'IDENT', each optionally ~-indented.
    next unless $lines[$i] =~ /<<(~?)(?:"([^"\n]*)"|'([^'\n]*)'|([A-Za-z_]\w*))/;
    my ($tilde, $dq, $sq, $bare) = ($1, $2, $3, $4);
    my ($from, $len) = ($-[0], $+[0] - $-[0]);
    my $term = defined $sq ? $sq : defined $dq ? $dq : $bare;
    my $end;
    for my $j ($i + 1 .. $#lines) {
      my $l = $lines[$j];
      $l =~ s/^\s+// if $tilde;
      next if $l ne $term;
      $end = $j;
      last;
    }
    next if !defined $end;
    my @body = @lines[$i + 1 .. $end - 1];
    if ($tilde) {                       # <<~END strips the common indentation
      my $ind;
      for my $b (@body, $lines[$end]) {
        next if $b !~ /\S/;
        my ($w) = $b =~ /^(\s*)/;
        $ind = $w if !defined $ind || length($w) < length($ind);
      }
      if (defined $ind && length $ind) { s/^\Q$ind\E// for @body }
    }
    my $lit = join '', map { "$_\n" } @body;
    if (defined $sq) { $lit =~ s/([\\'])/\\$1/g; $lit = "'$lit'" }
    else             { $lit =~ s/([\\"])/\\$1/g; $lit = "\"$lit\"" }
    my $var = '$__pcl_hd_' . scalar(@hoist);
    push @hoist, "my $var = $lit;";
    substr($lines[$i], $from, $len) = $var;
    splice @lines, $i + 1, $end - $i;
    $changed = 1;
    redo LINE if ++$guard < 20;         # a second opener on the same line
  }
  return undef if !$changed;
  return join("\n", @hoist, @lines);
}

# One compile attempt.  Returns (FORM, undef) or (undef, WHY).
#
# `nil` is the ANSWER, not a failure, when the replacement parses to no
# statements at all — `s/o//eg` reaches here with an empty replacement and its
# CL is `nil` (measured: initialising this to undef instead made closure.t
# fail to transpile).  The rule-12 die is for a replacement that THREW.
sub _try_compile_subst_e {
  my $self = shift;
  my $expr = shift;

  my $result = 'nil';
  eval {
    require PPI::Document;
    require Pl::PExpr;
    # Lazy: this file is loaded FROM Pl::Parser, so a compile-time
    # `use` would be circular; a runtime require is a %INC lookup once loaded.
    require Pl::Parser;
    my $doc = Pl::Parser::fragment_doc($expr);

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
  return (undef, $@) if $@;
  return (undef, undef) if !defined $result;
  return ($result, undef);
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
  my $from_cl = cl_string_literal(_expand_tr_escapes($from));
  my $to_cl   = cl_string_literal(_expand_tr_escapes($to));

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

# tr/// escapes are the dq table WITHOUT the case markers (`\U` in tr is a
# plain U — perl-probed): the shared decoder alone.  (#387 family 44, s413:
# the two decoders were 19 EXACT lines apart; their only real difference was
# the `\b` bug, #393.)
sub _process_tr_escape { return _decode_escape($_[0]) }

# THE escape decoder shared by double-quoted context and tr/// (#387 family
# 44): \n \t \r \a \e \f \b, `\\`, `\"`, \cX, \x{…} \xHH \x, \o{…},
# \N{U+HHHH}, \NNN octal — and an unknown \X is X (which is also how `\$`,
# `\@`, `\/`, `\0` come out right).  A dq context adds its case markers on
# top (_process_dq_escape).
sub _decode_escape {
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
  # \N{U+HHHH} - Unicode character by code point
  if ($esc =~ /^N\{U\+([0-9A-Fa-f]+)\}$/) {
    return chr(hex($1));
  }
  # \NNN - octal digits (\0 included)
  if ($esc =~ /^([0-7]{1,3})$/) {
    return chr(oct($1));
  }
  # Unknown escape: \X → X (Perl drops the backslash)
  return $esc;
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

# Single-pass escape sequence processor for double-quoted strings: the shared
# decoder, plus the case-changing escapes kept as markers (`\U` `\L` `\u` `\l`
# `\Q` `\F` `\E`) for _apply_case_escapes.
sub _process_dq_escape {
  my $esc = shift;
  return "\\$esc" if $esc =~ /^[ULulQFE]$/;
  return _decode_escape($esc);
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
    # One CL-literal writer for every quote form (#419): it does the same
    # backslash/quote escaping, and additionally splits out the code points
    # that cannot go into a UTF-8 source file or into a CL character at all.
    return _cl_string_literal_form($content);
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
    return _cl_string_literal_form($content);
  }
  elsif ($str =~ /^q\s*(.)(.*)(\1)$/s) {
    # q/.../ or q '...' style (optional whitespace between q and delimiter)
    $content = $2;
    $content =~ s/\\\\/\\/g;
    return _cl_string_literal_form($content);
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
#
# A code point ABOVE U+10FFFF is a third case and not an escaping problem at
# all: SBCL's char-code-limit is #x110000, so no CL character holds it and
# `(code-char N)` is NIL.  perl's own extended UTF-8 does hold it, and emitting
# it raw wrote the pre-2003 five/six-byte form — bytes SBCL's UTF-8 reader
# rejects, which killed the WHOLE FILE rather than the one expression (#419:
# t/re/pat.t's 1263 perl rows measured as 0).  Such a part becomes
# ['p-unrepresentable-char', N]: a form that READS, and dies where the value
# would have been used, naming the code point (docs/not-supported.md
# "Code points above U+10FFFF (perl's extended UTF-8)").
#
# The two classes are exact complements over the whole code-point range, so a
# character is either in a $SAFE_CHARS run or gets its own part:
#   $BAD_CHAR_RE  = surrogates | U+FFFE/U+FFFF | anything > U+10FFFF
#   $SAFE_CHARS   = everything else
our $BAD_CHAR_RE = qr/[\x{D800}-\x{DFFF}\x{FFFE}\x{FFFF}]|[^\x{0}-\x{10FFFF}]/;
our $SAFE_CHARS  = qr/[\x{0}-\x{D7FF}\x{E000}-\x{FFFD}\x{10000}-\x{10FFFF}]/;
use constant MAX_CL_CODEPOINT => 0x10FFFF;   # SBCL char-code-limit - 1

# PUBLIC (s435): the tree's ONE string-literal escaper, and the only code
# that renders the uni/ corpus's unrepresentable characters.  Named without
# the underscore because Pl/Parser.pm's drop form calls it across the module
# boundary -- it escapes the dropped statement's own source text.
sub cl_string_literal { Pl::CLForm::to_flat(_cl_string_literal_form(shift)) }
sub _cl_string_literal_form {
  my $content = shift;
  # Characters invalid in UTF-8: surrogates U+D800-U+DFFF, and non-chars U+FFFE/U+FFFF
  # (and the pattern repeats at every 0x10000 boundary: U+1FFFE, U+1FFFF, etc.)
  if ($content !~ $BAD_CHAR_RE) {
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  my @parts;
  while (length $content) {
    if ($content =~ /\A((?:$SAFE_CHARS)+)/s) {
      my $safe = $1;
      $safe =~ s/\\/\\\\/g;
      $safe =~ s/"/\\"/g;
      push @parts, qq{"$safe"};
      $content = substr($content, length($1));
    } else {
      my $cp = ord(substr($content, 0, 1));
      push @parts, $cp > MAX_CL_CODEPOINT
        ? ['p-unrepresentable-char', $cp]
        : ['string', ['code-char', $cp]];
      $content = substr($content, 1);
    }
  }
  return @parts == 1 ? $parts[0] : ['concatenate', "'string", @parts];
}


1;
