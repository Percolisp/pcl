# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Environment;

use v5.20;
use strict;
use warnings;

use Moo;

=head1 NAME

Pl::Environment - Environment information for Perl expression parser

=head1 SYNOPSIS

    use Pl::Environment;
    
    my $env = Pl::Environment->new(
        prototypes => { my_func => '$$' },
    );

    my $proto = $env->get_prototype('my_func');

=head1 DESCRIPTION

Pl::Environment provides information about the Perl environment that affects
expression parsing. This includes:

- Subroutine prototypes
- Bareword filehandles
- Lvalue subroutines

The environment is INPUT to the parser - it does not manage scope or track
declarations. That's the job of a statement parser (future work).

=cut

=head1 ATTRIBUTES

=head2 prototypes

Hash reference mapping subroutine names to their signature info.

    prototypes => {
        my_func => {
            params     => [ { name => '$x', default_cl => undef },
                            { name => '$y', default_cl => '10' } ],
            min_params => 1,
            is_proto   => 0,
        }
    }

Signature info includes:
- params: Array of parameter hashes with name and optional default_cl (compiled CL)
- min_params: Minimum number of required arguments
- is_proto: True if old-style prototype ($$), false if new-style signature ($x, $y)

=cut

has prototypes => (
    is => 'rw',
    default => sub { _builtin_prototypes() },
);

# The SAME entries, keyed by the DECLARING package: { bare => { pkg => info } }.
# `prototypes` above is one flat bare-name table, so two packages declaring the
# same sub name with different prototypes collide and the LAST registration
# wins for every call site (task #421: a silent wrong in one declaration order,
# a DROP in the other).  This table is consulted ONLY when a bare name has more
# than one declaring package — with 0 or 1 the flat table IS that entry, so
# every non-colliding program takes exactly the path it took before.
has pkg_prototypes => (
    is => 'rw',
    default => sub { {} },
);

# Names in the module's @EXPORT/@EXPORT_OK (set only on the throwaway env a
# _extract_module_prototypes parse fills in).  Sub EXISTENCE is parse data:
# a bareword before a comma is a call only for a KNOWN sub, so the caller's
# merge imports exported plain subs even when they carry no prototype.
has export_names => (
    is => 'rw',
    default => sub { {} },
);

# Built-in function prototypes for functions that take bareword filehandles.
# The '*' prototype means "accepts bareword as filehandle".
# NOTE: We don't set min_params here - that's handled by Config.pm's known_no_of_params.
# These prototypes only provide the '*' type info for post-processing bareword filehandles.
sub _builtin_prototypes {
    return {
        # File I/O - these take filehandle as first arg
        'open'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'close'     => { params => [{proto_type => '*'}], is_proto => 1 },
        'binmode'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'eof'       => { params => [{proto_type => '*'}], is_proto => 1 },
        'tell'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'seek'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'truncate'  => { params => [{proto_type => '*'}], is_proto => 1 },
        'flock'     => { params => [{proto_type => '*'}], is_proto => 1 },
        'read'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'sysread'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'syswrite'  => { params => [{proto_type => '*'}], is_proto => 1 },
        'sysseek'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'fileno'    => { params => [{proto_type => '*'}], is_proto => 1 },
        'getc'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'stat'      => { params => [{proto_type => '*'}], is_proto => 1 },
        'lstat'     => { params => [{proto_type => '*'}], is_proto => 1 },
        # Directory operations
        'opendir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'readdir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'closedir'  => { params => [{proto_type => '*'}], is_proto => 1 },
        'rewinddir' => { params => [{proto_type => '*'}], is_proto => 1 },
        'seekdir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        'telldir'   => { params => [{proto_type => '*'}], is_proto => 1 },
        # Note: print/say/printf are NOT included here - they have special handling
        # in PExpr.pm that deals with their complex filehandle detection logic.
    };
}

=head2 filehandles

Hash reference of bareword filehandles.

    filehandles => { STDIN => 1, STDOUT => 1, FH => 1 }

Used to distinguish barewords in print statements:

    print FH "data";    # FH is filehandle
    print foo "data";   # foo is function

=cut

has filehandles => (
    is => 'rw',
    coerce => sub {
        my $fh = shift // {};
        return { STDIN => 1, STDOUT => 1, STDERR => 1, DATA => 1, ARGV => 1, %$fh };
    },
    default => sub { { STDIN => 1, STDOUT => 1, STDERR => 1, DATA => 1, ARGV => 1 } },
);

=head2 filehandle_scope

Tracks scope level for each filehandle. Standard filehandles are at scope 0.
When pop_scope() is called, filehandles added at that scope level are removed.

=cut

has filehandle_scope => (
    is => 'rw',
    default => sub { { STDIN => 0, STDOUT => 0, STDERR => 0, DATA => 0, ARGV => 0 } },
);

=head2 scope_level

Current scope nesting level. Starts at 0.

=cut

has scope_level => (
    is => 'rw',
    default => 0,
);

=head2 scope_stack

Array of scope frames for tracking pragmas and variable declarations.
Each frame is { pragmas => {}, declared_vars => {} }.
Pragmas are inherited from parent on push_scope; declared_vars start fresh.

=cut

has scope_stack => (
    is => 'rw',
    default => sub { [{ pragmas => {}, declared_vars => {} }] },
);

=head2 undeclared_vars

Accumulates variables that need file-level defvar (referenced but not
declared in any enclosing scope). Built during code generation.

=cut

has undeclared_vars => (
    is => 'rw',
    default => sub { {} },
);

=head2 caret_globals

Accumulates unknown C<${^NAME}> caret variables encountered during code
generation. Perl treats any C<${^NAME}> not given special meaning as an
ordinary (main-forced) global scalar: undef until assigned, autovivifying.
Each one needs a file-level defvar so CL reads/increments it as a box rather
than crashing. Keyed on the pipe-quoted CL symbol (e.g. C<|${^MPE}|>).

=cut

has caret_globals => (
    is => 'rw',
    default => sub { {} },
);

=head2 expression_our_vars

Accumulates C<our $var> declarations encountered in EXPRESSION context (e.g.
C<\our $referent>, C<bless \our $x>, C<use constant K =E<gt> \our $v>) rather
than as a top-level statement. A statement-level C<our> emits its defvar via
C<_process_our_declaration>, but an embedded C<our> is otherwise reduced to a
bare variable reference, leaving the package global undeclared (it is referenced
inside a generated sub body, so the file-scope forward-declaration scan misses
it). Keyed on the CL symbol (e.g. C<$referent> or C<|Foo|::$x>) → sigil, so the
forward-declaration pass can emit one idempotent file-level defvar each.

=cut

has expression_our_vars => (
    is => 'rw',
    default => sub { {} },
);

=head2 punct_globals

Accumulates punctuation-named C<#> globals (C<$#>, C<@#>, C<%#>) emitted during
code generation.  These arise from the removed C<$#> magic taking a subscript:
Perl parses C<$#[0]> as element 0 of C<@#> and C<$#{k}> as a slice of C<@#>
(verified vs perl: C<@{"#"}=(10,20,30); $#[0]==10>).  Their names are not word
characters, so the file-scope forward-declaration scan misses them; codegen
registers each one here so the forward-declaration pass can emit one file-level
defvar (matching the sigil's container).  An undeclared Perl global reads as
empty/undef, so this avoids an unbound-symbol crash.  Keyed on the CL symbol
(e.g. C<@#>) so duplicates collapse.

=cut

has punct_globals => (
    is => 'rw',
    default => sub { {} },
);

=head2 in_subroutine

Counter tracking subroutine nesting depth. 0 = top level, >0 = inside sub.
Used to determine whether shift/pop should default to @_ or @ARGV.

=cut

has in_subroutine => (
    is => 'rw',
    default => 0,
);

=head2 wa_void_active

Set to 1 while emitting statements inside a sub-body void regime: the body is
wrapped in a single C<(let ((*wantarray* :void)) ...)>, so the ambient
C<*wantarray*> is already C<:void>.  Both the Parser's per-statement wrap site
and ExprToCL's per-call context wrap read this to SKIP a redundant C<:void>
binding (binding C<:void> to C<:void> is a no-op).  Cleared at do/eval/map/grep/
sort boundaries (whose macros rebind C<*wantarray*>, so the ambient is no longer
C<:void> there).

=cut

has wa_void_active => (
    is      => 'rw',
    default => 0,
);

=head2 tail_position

Set to 1 when the current expression is in tail position (last expression of a
sub body, or direct value of a return statement).  gen_funcall reads this to
suppress the *wantarray* binding, allowing context to propagate from the caller.

=cut

has tail_position => (
    is      => 'rw',
    default => 0,
);

=head2 tail_wants_list

Set to 1 while processing the tail (last) statement of a block whose value is
consumed in LIST context — currently a C<map { ... }> block, which Perl
evaluates in list context.  A bare expression statement at tail position
normally compiles in VOID_CTX; when this flag is set it compiles in LIST_CTX
instead, so e.g. C<map { 1..$_ }> treats C<..> as the range operator (not the
flip-flop, which is what scalar/boolean context would select).

=cut

has tail_wants_list => (
    is      => 'rw',
    default => 0,
);

=head2 lvalue_subs

Hash reference of subroutines declared with :lvalue attribute.

    lvalue_subs => { get_value => 1 }

Lvalue subs can appear on the left side of assignment:

    get_value() = 42;

=cut

has lvalue_subs => (
    is => 'rw',
    default => sub { {} },
);

=head2 package_stack

Array reference representing the current package scope stack.
The default is ['main']. When entering a package block, push the
new package name. When leaving, pop back to the previous package.

    package MyClass { ... }  # push 'MyClass', then pop after block

=cut

has package_stack => (
    is => 'rw',
    default => sub { ['main'] },
);

=head2 known_packages

Hash reference of all declared package/class names.

    known_packages => { Counter => 1, Point => 1 }

Used to distinguish class names from function calls in method calls:

    Counter->new();  # Counter is a known package, use as class name

=cut

has known_packages => (
    is => 'rw',
    default => sub { {} },
);

=head2 referenced_packages

Hash of package names referenced in code (e.g., from Foo::bar() calls).
Used to pre-declare packages that might not be defined until runtime.

=cut

has referenced_packages => (
    is => 'rw',
    default => sub { {} },
);

=head2 our_variables

Hash of package variables declared with 'our'.
Keys are "Package::$varname", values are 1.

    our_variables => { 'Counter::$count' => 1 }

=cut

has our_variables => (
    is => 'rw',
    default => sub { {} },
);

=head2 overridden_builtins

Core builtins a package has displaced with C<use subs>.
Keys are "Package::name", values are the C<[line, column]> of the earliest
declaration.

    overridden_builtins => { 'o::readpipe' => [12, 5] }

perl only lets a compile-time PREDECLARATION (C<use subs>, or an import) take
a builtin's name; a plain C<sub readpipe {...}> in the package does NOT
override (probed s451z, task #703).  The effect is PACKAGE-scoped, not
lexical: C<package Other;> in the same file stops seeing it and re-entering
the declaring package brings it back.

=cut

has overridden_builtins => (
    is => 'rw',
    default => sub { {} },
);

=head2 isa_declarations

Hash of @ISA declarations per package.
Keys are package names, values are arrayrefs of parent package names.

    isa_declarations => { 'Child' => ['Parent1', 'Parent2'] }

=cut

has isa_declarations => (
    is => 'rw',
    default => sub { {} },
);

=head2 declared_subs

Array of subs declared in this file, with their package names.
Each entry is { name => 'subname', package => 'PackageName' }.
Used to emit forward declarations so top-level code can call subs
defined later in the file.

=cut

has declared_subs => (
    is => 'rw',
    default => sub { [] },
);

=head2 source_file

The source filename being parsed. Used for __FILE__ token expansion.
Defaults to '-' (stdin) if not set.

=cut

has source_file => (
    is => 'rw',
    default => '-',
);

has state_var_renames => (
    is      => 'rw',
    default => sub { {} },
);

=head1 METHODS

=head2 get_prototype($name)

Returns the signature info hash for a subroutine, or undef if not found.

    my $sig_info = $env->get_prototype('my_func');
    # Returns: { params => [...], min_params => N, is_proto => 0/1 }

=cut

sub get_prototype {
    my $self = shift;
    my $name = shift;

    return $self->_proto_entry($name);
}

# THE one prototype lookup (task #421).  A qualified spelling names its own
# package; an unqualified one is resolved in the CURRENT package, which is
# what perl resolves it in.  The per-package table is consulted only when the
# bare name actually has competing declarations — otherwise the flat table is
# that same entry, and this stays a pure no-op.  When the current package has
# no declaration of its own the flat table answers, because that is how an
# IMPORTED sub's prototype reaches a call site (imports are recorded under the
# exporting module's name, or under no package at all).
sub _proto_entry {
    my ($self, $name) = @_;
    my $bare = _bare_sub_name($name);
    my $per  = $self->pkg_prototypes->{$bare};
    my $q    = (defined $name && $name =~ /\A(.+)::[^:]+\z/) ? $1 : undef;
    # A QUALIFIED spelling asks its own package FIRST: a module sub reached
    # only by qualified calls (`Test2::API::context_do { … }`) is in no
    # import list, so the flat table has never heard of it — its record
    # arrives through add_pkg_prototype and lives in the per-package table
    # alone.  (With a single add_prototype-written key the two tables hold
    # the same entry, so answering per-package here changes nothing else.)
    return $per->{$q} if defined $q && $per && exists $per->{$q};
    return $self->prototypes->{$bare} if !$per || keys(%$per) < 2;
    my $pkg = $q // ($self->current_package // 'main');
    return $per->{$pkg} if exists $per->{$pkg};
    return $self->prototypes->{$bare};
}

=head2 add_pkg_prototype($name, $sig_info, $package)

Register a prototype under its DECLARING package ONLY — the per-package
table, never the flat one.  This is the record a package-QUALIFIED call site
resolves; the flat table is what unqualified call sites read, and a fact
that arrived without an import must not change those.  Consumer:
C<_merge_module_prototypes>'s declared-subs pass (a module's subs are in its
stash whatever the import list says, so perl parses C<Module::name { … }>
with the declared prototype).  A local untagged declaration for the same
package is never overwritten — the same rule the merge applies to the flat
table.

=cut

sub add_pkg_prototype {
    my ($self, $name, $sig_info, $package) = @_;
    my $bare = _bare_sub_name($name);
    my $existing = $self->pkg_prototypes->{$bare}{$package};
    return if $existing && !$existing->{from_module};
    $self->pkg_prototypes->{$bare}{$package} = $sig_info;
}

=head2 has_prototype($name)

Returns true if the subroutine has a known prototype/signature.

    if ($env->has_prototype('my_func')) { ... }

=cut

sub has_prototype {
    my $self = shift;
    my $name = shift;

    return defined $self->_proto_entry($name);
}

=head2 get_min_params($name)

Returns the minimum number of required parameters for a subroutine,
or undef if not found.

    my $min = $env->get_min_params('my_func');

=cut

sub get_min_params {
    my $self = shift;
    my $name = shift;

    my $sig_info = $self->_proto_entry($name);
    return undef unless $sig_info;
    return $sig_info->{min_params};
}

=head2 proto_is_zero_arg($sig_info)

True when a prototype RECORD is one of perl's zero-argument TERMS — an empty
prototype (`sub pi () {…}`), a `use constant`, an all-defaulted signature.
The record shape is min_params 0 with no parameter slots; `is_proto` does NOT
distinguish it, because the `()` spelling arrives both ways (is_proto 1 with an
empty proto_string from a prototype, is_proto 0 from parse_prototype_or_
signature's empty case and from `use constant`'s registration).

    $env->proto_is_zero_arg($env->get_prototype('pi'))   # 1

Callable as a class method: it reads only the record.  It is THE one reading
of that shape — PExpr::_is_zero_arg_func (does this bareword parse as a term?)
and Parser::_merge_module_prototypes (must this prototype cross a `use`?) both
ask it, and they used to carry the test inline.

=cut

sub proto_is_zero_arg {
    my ($self, $sig_info) = @_;
    return 0 unless $sig_info && defined $sig_info->{min_params};
    return ($sig_info->{min_params} == 0
            && @{ $sig_info->{params} || [] } == 0) ? 1 : 0;
}

=head2 add_prototype($name, $sig_info)

Adds or updates a subroutine signature info.

    $env->add_prototype('my_func', {
        params     => [ { name => '$x' }, { name => '$y', default_cl => '10' } ],
        min_params => 1,
        is_proto   => 0,
    });

=cut

# The prototype table and declared_subs are keyed by the BARE sub name (the
# convention PExpr::_bareword_callable_here documents and relies on: it splits
# a qualified CALL into (package, basename) and matches the basename).  A
# package-QUALIFIED DECLARATION — `sub main::end(&)` written from inside
# another package (die_unwind.t), `sub Foo::g(&)` — broke that convention on
# the way IN: the entry went in under `main::end`, so the `end { … }` call
# site in main found no prototype and the trailing block was not parsed as a
# block-form argument (task #413, 6 census drops).  Normalize at the SEAM, so
# every producer (v1 _register_sub_prototype, v2 _scan_segment, module
# extraction) and every consumer agrees: strip the qualifier on the way in,
# and again on the way out for a qualified call site.  perl agrees with both
# halves — probed: `sub main::f(&)` from main, `sub Foo::g(&)` called as
# `Foo::g { … }`, and the die_unwind.t shape all take the block form.
sub _bare_sub_name {
    my $name = shift;
    return $name unless defined $name;
    $name =~ s/\A.*:://s;
    return $name;
}

# $package is the DECLARING package — the one perl installs the sub in.  A
# qualified NAME carries it and wins (the #413 rule); otherwise the caller
# supplies it (the sub pre-scan already computes it for add_declared_sub) and
# the current package is the fallback.  The entry goes in BOTH tables: the
# flat one keeps answering for imports and for every non-colliding name, the
# per-package one settles a collision (task #421).
sub add_prototype {
    my $self     = shift;
    my $name     = shift;
    my $sig_info = shift;
    my $package  = shift;

    my $bare  = _bare_sub_name($name);
    my $owner = (defined $name && $name =~ /\A(.+)::[^:]+\z/) ? $1
              : (defined $package ? $package : ($self->current_package // 'main'));

    $self->prototypes->{$bare} = $sig_info;
    $self->pkg_prototypes->{$bare}{$owner} = $sig_info;
}

=head2 fh_bareword_shape($name)

THE shape test for a bareword FILEHANDLE name — the ALL-CAPS convention every
parse site uses to guess "handle" for a word it has not seen opened (task
#491).  A plain sub, not a method: the parse sites that ask it do not all have
an Environment, and the answer does not depend on one.

It is asked of the NAME, never of the qualifier: C<main::STDOUT> and
C<Foo::H1> are handle spellings exactly as C<STDOUT> and C<H1> are, and perl
reads all four the same way.  Testing the whole spelling made every qualified
handle fail, and the four sites then disagreed with each other — the print
C<:fh> slot said "not a handle" (a CALL to an undefined sub) while
C<readline>, which quotes the name itself, said "handle".

=cut

sub fh_bareword_shape {
    my $name = shift;

    return 0 if !defined $name;
    $name =~ s/\A.*:://;
    return all_caps_shape($name);
}

=head2 all_caps_shape($name)

THE ALL-CAPS bareword convention, as a shape — the one test behind every site
that guesses "filehandle or constant" for a word it cannot place (task #820).

UNICODE, not ASCII.  Under C<use utf8> a source name is a decoded string and
C<Ạ> (U+1EA0) is an uppercase letter, so C<open Ạ, …> is exactly as much a
handle-shaped bareword as C<open BEE, …> — perl reads both the same way.  The
pattern is a strict superset of the old C<[A-Z][A-Z0-9_]*>: on a pure-ASCII
name C<\p{Lu}> IS C<[A-Z]> and C<\p{Nd}> IS C<[0-9]>, so every ASCII answer is
unchanged by construction.

Asked of a PLAIN name.  A caller that must look through a package qualifier
strips it first — that is what C<fh_bareword_shape> is (a handle NAME is the
same handle however it is qualified, #491), and the callers that must NOT look
through one (the indirect-object skip, the C<WORD /> term guess) ask this
directly.

=cut

sub all_caps_shape {
    my $name = shift;

    return 0 if !defined $name;
    return $name =~ /\A\p{Lu}[\p{Lu}\p{Nd}_]*\z/ ? 1 : 0;
}

=head2 all_caps_call_guess($name)

The SAME convention asked for the opposite purpose: "this bareword cannot be
placed — keep it a CALL rather than read it as a no-strict bareword string".
Three sites ask it (the two C<handle_subcalls> string fall-throughs and the
eval-mode array-subscript autoquote).

ASCII ONLY, and deliberately so — this is the one place where the two
questions must NOT share an answer, because their false positives point in
opposite directions.  A wrong "yes" in a HANDLE slot is harmless (the slot
admits no other reading), while a wrong "yes" HERE turns a value perl prints
into an undefined-subroutine death: perl reads C<no strict; print "x=", Ẹ;>
as the string C<Ẹ>, and PCL already gets the ASCII twin C<ABC> wrong for
exactly this reason (#266's accepted residue — an ALL-CAPS word is far more
often a constant than a string, so the guess earns its keep).  Widening THAT
compromise to a new character range was measured: it converts a correct
answer into a dying one and buys nothing (s457aj probe 08c), so the legacy
ASCII spelling stays until #266's classifier replaces the guess outright.

=cut

sub all_caps_call_guess {
    my $name = shift;

    return 0 if !defined $name;
    return $name =~ /\A[A-Z][A-Z0-9_]*\z/ ? 1 : 0;
}

# The handles perl resolves in main:: whatever package names them unqualified
# (gv.c's forced-main list, handle members).  A qualifier on one of these is
# therefore NOT the same glob as the bare name — see canon_filehandle_name.
my %FORCED_MAIN_HANDLE = map { $_ => 1 }
    qw(STDIN STDOUT STDERR ARGV ARGVOUT ENV INC _);

=head2 fh_forced_main_name($name)

True when C<$name> (unqualified) is one of the handles perl resolves in
C<main::> from every package.  The same fact C<canon_filehandle_name> uses,
asked separately by C<Pl::ExprToCL::_fh_sym>: these are the only handle names
the RUNTIME can find by their short name (they are the ones interned in the
C<:pcl> package, which is what C<%p-resolve-fh>'s by-name fallback searches),
so a qualified spelling of one must not be emitted as a symbol carrying that
short name — or C<print Foo::STDOUT "x"> silently reaches main's STDOUT where
perl writes nothing.

=cut

sub fh_forced_main_name {
    my $name = shift;
    return defined $name && $FORCED_MAIN_HANDLE{$name} ? 1 : 0;
}

=head2 canon_filehandle_name($name)

THE canonical registry key AND emitted spelling for a bareword filehandle NAME
(task #491).

Perl names a bareword handle by its GLOB, so one handle has more than one
spelling: C<main::FH> IS C<FH> in package main, and a qualifier equal to the
CURRENT package is the same glob as the unqualified spelling (C<package P;
open(P::FH,...)> and C<open(FH,...)> are one handle).  The registry used to be
keyed by the literal spelling, so C<open(main::FH, ...)> registered "main::FH"
while the later C<print FH "x"> asked about "FH" — the two never met, and the
print was emitted as a CALL to an undefined sub.

Both C<is_filehandle> and C<add_filehandle> ask this, so the registry cannot
disagree with itself, and C<Pl::ExprToCL::_fh_sym> asks it too, so the emitted
CL symbol for the two spellings is one symbol rather than two that have to
find each other at run time.

A qualifier naming some OTHER package (C<Foo::H1> from main) is a different
handle and keeps its qualifier — perl's stash autovivifies, so C<Foo::H1> is a
handle whether or not a C<package Foo> statement exists.

THE ASYMMETRY, probed: the standard handles are forced into C<main::> from
ANY package, so the UNQUALIFIED spelling is main's from everywhere — but an
explicit qualifier still names that package's own glob.  Inside
C<package Foo>, C<print STDOUT "x"> prints and C<print Foo::STDOUT "x">
prints NOTHING and returns undef.  So the C<$pkg eq $here> collapse, which is
right for a user handle (C<package Foo; open(H2,…)> IS C<Foo::H2>, probed),
would be wrong for those eight names.

=cut

sub canon_filehandle_name {
    my $self = shift;
    my $name = shift;

    return $name if !defined $name || index($name, '::') < 0;
    return $name if $name !~ /\A(.+)::([^:]+)\z/;
    my ($pkg, $short) = ($1, $2);
    return $pkg eq 'main' ? $short : $name if $FORCED_MAIN_HANDLE{$short};
    my $here = $self->current_package // 'main';
    return $short if $pkg eq 'main' || $pkg eq $here;
    return $name;
}

=head2 is_filehandle($name)

Returns true if $name is a known filehandle.

    if ($env->is_filehandle('FH')) { ... }

=cut

sub is_filehandle {
    my $self = shift;
    my $name = shift;

    return exists $self->filehandles->{ $self->canon_filehandle_name($name) };
}

=head2 add_filehandle($name)

Adds a bareword filehandle at the current scope level.

    $env->add_filehandle('FH');

=cut

sub add_filehandle {
    my $self = shift;
    my $name = shift;

    $name = $self->canon_filehandle_name($name);
    $self->filehandles->{$name} = 1;
    $self->filehandle_scope->{$name} = $self->scope_level;
}

=head2 push_scope()

Enters a new scope level. Called when entering a block.

    $env->push_scope();

=cut

sub push_scope {
    my $self = shift;
    $self->scope_level($self->scope_level + 1);

    # Push new scope frame: inherit pragmas from parent, fresh declared_vars
    my $parent = $self->scope_stack->[-1];
    push @{$self->scope_stack}, {
        pragmas      => { %{$parent->{pragmas}} },
        declared_vars => {},
    };
}

=head2 pop_scope()

Leaves the current scope level. Removes filehandles added at this level.
Called when leaving a block.

    $env->pop_scope();

=cut

sub pop_scope {
    my $self = shift;
    my $level = $self->scope_level;

    # Remove filehandles added at this scope level
    my $fh = $self->filehandles;
    my $fh_scope = $self->filehandle_scope;

    for my $name (keys %$fh_scope) {
        if ($fh_scope->{$name} == $level) {
            delete $fh->{$name};
            delete $fh_scope->{$name};
        }
    }

    # Pop scope frame (never pop below initial frame)
    pop @{$self->scope_stack} if @{$self->scope_stack} > 1;

    # Decrease scope level (but never below 0)
    $self->scope_level($level - 1) if $level > 0;
}

=head2 set_pragma($name, $val)

Sets a pragma on the current scope frame.

    $env->set_pragma('use_integer', 1);

=cut

sub set_pragma {
    my ($self, $name, $val) = @_;
    $self->scope_stack->[-1]{pragmas}{$name} = $val;
}

=head2 has_pragma($name)

Returns the value of a pragma in the current scope, or undef if not set.

    if ($env->has_pragma('use_integer')) { ... }

=cut

sub has_pragma {
    my ($self, $name) = @_;
    return $self->scope_stack->[-1]{pragmas}{$name};
}

=head2 add_caret_global($sym)

Records an unknown C<${^NAME}> caret variable (by its pipe-quoted CL symbol)
that needs a file-level defvar.

=cut

sub add_caret_global {
    my ($self, $sym) = @_;
    $self->caret_globals->{$sym} = 1;
}

=head2 register_punct_global($sym)

Record a punctuation-named C<#> global (C<$#>/C<@#>/C<%#>) seen during codegen so
the forward-declaration pass emits a defvar for it.  See L</punct_globals>.

=cut

sub register_punct_global {
    my ($self, $sym) = @_;
    $self->punct_globals->{$sym} = 1;
}

=head2 get_caret_globals()

Returns sorted list of caret-variable CL symbols needing a file-level defvar.

=cut

sub get_caret_globals {
    my $self = shift;
    return [sort keys %{$self->caret_globals}];
}

=head2 is_lvalue_sub($name)

Returns true if $name is a known lvalue subroutine.

    if ($env->is_lvalue_sub('get_value')) { ... }

=cut

sub is_lvalue_sub {
    my $self = shift;
    my $name = shift;
    
    return exists $self->lvalue_subs->{$name};
}

=head2 add_lvalue_sub($name)

Adds an lvalue subroutine.

    $env->add_lvalue_sub('get_value');

=cut

sub add_lvalue_sub {
    my $self = shift;
    my $name = shift;

    $self->lvalue_subs->{$name} = 1;
}

=head2 current_package()

Returns the current package name (top of stack).

    my $pkg = $env->current_package();  # 'main' by default

=cut

sub current_package {
    my $self = shift;
    return $self->package_stack->[-1];
}

=head2 push_package($name)

Pushes a new package onto the stack.

    $env->push_package('MyClass');

=cut

sub push_package {
    my $self = shift;
    my $name = shift;
    push @{$self->package_stack}, $name;
}

=head2 pop_package()

Pops the current package from the stack.
Never pops below 'main'.

    $env->pop_package();

=cut

sub pop_package {
    my $self  = shift;
    my $stack = $self->package_stack;
    pop @$stack if @$stack > 1;  # Never pop 'main'
}

=head2 add_package($name)

Adds a package to the known packages set.

    $env->add_package('Counter');

=cut

sub add_package {
    my $self = shift;
    my $name = shift;
    $self->known_packages->{$name} = 1;
}

=head2 is_package($name)

Returns true if $name is a known package/class.

    if ($env->is_package('Counter')) { ... }

=cut

sub is_package {
    my $self = shift;
    my $name = shift;
    return exists $self->known_packages->{$name};
}

=head2 add_referenced_package($name)

Records that a package is referenced in code (e.g., via Foo::bar() call).
Only records if the package is not already declared via known_packages.

=cut

sub add_referenced_package {
    my $self = shift;
    my $name = shift;
    return if exists $self->known_packages->{$name};
    $self->referenced_packages->{$name} = 1;
}

=head2 get_undeclared_packages()

Returns list of packages that are referenced but not declared.
Used to emit pre-declarations at the top of generated code.

=cut

sub get_undeclared_packages {
    my $self = shift;
    my @pkgs = grep { !exists $self->known_packages->{$_} }
               keys %{$self->referenced_packages};
    return [sort @pkgs];
}

=head2 add_our_variable($pkg, $var)

Records that a variable was declared with 'our' in the given package.

    $env->add_our_variable('Counter', '$count');

=cut

sub add_our_variable {
    my ($self, $pkg, $var) = @_;
    $self->our_variables->{"${pkg}::${var}"} = 1;
}

=head2 is_our_variable($pkg, $var)

Returns true if $var was declared with 'our' in $pkg.

    if ($env->is_our_variable('Counter', '$count')) { ... }

=cut

sub is_our_variable {
    my ($self, $pkg, $var) = @_;
    return exists $self->our_variables->{"${pkg}::${var}"};
}

=head2 add_builtin_override($pkg, $name, $line, $col) / builtin_is_overridden($pkg, $name, $line, $col)

Records / asks whether C<$pkg> displaced the core builtin C<$name> with a
C<use subs> predeclaration.  See the C<overridden_builtins> attribute.

perl decides this at each use site's PARSE, so a use site BEFORE the
C<use subs> still gets the builtin — hence the source position on both calls.
The earliest declaration for a name wins (a package may say it twice); a query
without a position asks only whether the package declared it at all.

    $env->add_builtin_override('o', 'readpipe', 12, 5);
    if ($env->builtin_is_overridden('o', 'readpipe', $line, $col)) { ... }

=cut

sub add_builtin_override {
    my ($self, $pkg, $name, $line, $col) = @_;
    my $key = "${pkg}::${name}";
    my $at  = [ $line // 0, $col // 0 ];
    my $old = $self->overridden_builtins->{$key};
    return if $old && ($old->[0] < $at->[0]
                       || ($old->[0] == $at->[0] && $old->[1] <= $at->[1]));
    $self->overridden_builtins->{$key} = $at;
}

sub builtin_is_overridden {
    my ($self, $pkg, $name, $line, $col) = @_;
    return 0 if !defined $pkg || !defined $name;
    my $at = $self->overridden_builtins->{"${pkg}::${name}"} or return 0;
    return 1 if !defined $line;
    return 0 if $line < $at->[0];
    return 0 if $line == $at->[0] && defined $col && $col < $at->[1];
    return 1;
}

=head2 builtin_is_overridable($name)

Whether perl lets a C<use subs>/import predeclaration displace the builtin
C<$name> at all (task #732).  perl's discriminator is the SIGN of the keyword
code: C<Perl_keyword()> returns a NEGATIVE code for a "weak" keyword, and only
then does the toker look for an imported sub.  C<prototype("CORE::NAME")> is
NOT the rule (C<system> has an undef prototype yet IS overridable — probed
5.40.3), so the set below is the weak ('-') half of perl's own
C<regen/keywords.pl> data section, extracted from the 5.40.3 source with

    perl -ne 'print "$1 " if /^-([A-Za-z_2]+)$/' regen/keywords.pl

A strong ('+') keyword — C<print>, C<sort>, C<grep>, C<defined>, … — keeps
the builtin meaning even when a C<use subs> row was recorded for it.

=cut

my %WEAK_KEYWORDS = map { $_ => 1 } qw(
    __FILE__ __LINE__ __PACKAGE__ __CLASS__ __SUB__ abs accept alarm and atan2
    bind binmode bless break caller chdir chmod chomp chop chown chr chroot
    class close closedir cmp connect continue cos crypt dbmclose dbmopen die
    dump each endgrent endhostent endnetent endprotoent endpwent endservent
    eof eq evalbytes exec exit exp fc fcntl field fileno flock fork formline
    ge getc getgrent getgrgid getgrnam gethostbyaddr gethostbyname gethostent
    getlogin getnetbyaddr getnetbyname getnetent getpeername getpgrp getppid
    getpriority getprotobyname getprotobynumber getprotoent getpwent getpwnam
    getpwuid getservbyname getservbyport getservent getsockname getsockopt
    gmtime gt hex index int ioctl isa join keys kill lc lcfirst le length link
    listen localtime lock log lstat lt method mkdir msgctl msgget msgrcv
    msgsnd ne not oct open opendir or ord pack pipe pop push quotemeta rand
    read readdir readline readlink readpipe recv ref rename reset reverse
    rewinddir rindex rmdir seek seekdir select semctl semget semop send
    setgrent sethostent setnetent setpgrp setpriority setprotoent setpwent
    setservent setsockopt shift shmctl shmget shmread shmwrite shutdown sin
    sleep socket socketpair splice sprintf sqrt srand stat substr symlink
    syscall sysopen sysread sysseek system syswrite tell telldir tie tied
    time times truncate uc ucfirst umask unlink unpack unshift untie utime
    values vec wait waitpid wantarray warn write x xor
);

sub builtin_is_overridable {
    my ($self, $name) = @_;
    return defined($name) && $WEAK_KEYWORDS{$name} ? 1 : 0;
}

=head2 set_isa($pkg, \@parents)

Records the @ISA declaration for a package.

    $env->set_isa('Child', ['Parent1', 'Parent2']);

=cut

sub set_isa {
    my ($self, $pkg, $parents) = @_;
    $self->isa_declarations->{$pkg} = $parents;
}

=head2 add_declared_sub($name, $package, $at)

Records that a sub was declared in the given package.
Used to emit forward declarations.

    $env->add_declared_sub('greet', 'main');

C<$at> is the optional declaration SITE — C<{ doc =E<gt> ..., pos =E<gt> [line, col] }>
as produced by L<Pl::PExpr::TokenUtils/decl_site>.  A bareword call site asks
whether the declaration is ABOVE it before reading the name as a call
(task #266); without a site the entry answers "position unknown", which every
caller must read as the old whole-file answer, never as "below".

=cut

sub add_declared_sub {
    my ($self, $name, $package, $at) = @_;
    # Same normalization as add_prototype, and for the same reason: a
    # qualified declaration `sub main::end(&)` declares `end` in package
    # `main`, not `main::end` in the enclosing package (task #413).  The
    # NAME carries the package when it is qualified, and it wins — that is
    # what perl installs.
    if (defined $name && $name =~ /\A(.+)::([^:]+)\z/) {
        ($package, $name) = ($1, $2);
    }
    push @{$self->declared_subs}, { name => $name, package => $package,
                                    ($at ? %$at : ()) };
}

=head2 get_declared_subs()

Returns arrayref of all declared subs with their packages.

    my $subs = $env->get_declared_subs();
    # [ { name => 'foo', package => 'main' }, { name => 'bar', package => 'MyClass' } ]

=cut

sub get_declared_subs {
    my $self = shift;
    return $self->declared_subs;
}

=head2 merge($other_env)

Merges another environment into this one.

    $env->merge($other_env);

All prototypes, filehandles, and lvalue subs from $other_env
are added to this environment. In case of conflicts, $other_env wins.

=cut

sub merge {
    my $self  = shift;
    my $other = shift;

    # Merge prototypes — both tables, or a merged-in name would be invisible
    # to the per-package lookup and a collision would resolve by the flat
    # table alone (task #421).
    for my $name (keys %{$other->prototypes}) {
        $self->prototypes->{$name} = $other->prototypes->{$name};
    }
    for my $name (keys %{$other->pkg_prototypes}) {
        my $per = $other->pkg_prototypes->{$name};
        $self->pkg_prototypes->{$name}{$_} = $per->{$_} for keys %$per;
    }


    # Merge filehandles
    for my $name (keys %{$other->filehandles}) {
        $self->filehandles->{$name} = 1;
    }
    
    # Merge lvalue subs
    for my $name (keys %{$other->lvalue_subs}) {
        $self->lvalue_subs->{$name} = 1;
    }

    # Merge known packages
    for my $name (keys %{$other->known_packages}) {
        $self->known_packages->{$name} = 1;
    }
}

=head2 clone()

Creates a shallow copy of this environment.

    my $env2 = $env->clone();

Useful for creating a modified environment without affecting the original.

=cut

sub clone {
    my $self = shift;

    return Pl::Environment->new(
        prototypes       => { %{$self->prototypes} },
        pkg_prototypes   => { map { ($_ => { %{ $self->pkg_prototypes->{$_} } }) }
                              keys %{$self->pkg_prototypes} },
        filehandles      => { %{$self->filehandles} },
        filehandle_scope => { %{$self->filehandle_scope} },
        scope_level      => $self->scope_level,
        lvalue_subs      => { %{$self->lvalue_subs} },
        package_stack    => [ @{$self->package_stack} ],
        known_packages   => { %{$self->known_packages} },
    );
}

=head1 SCOPE MANAGEMENT

Scope management for filehandles is handled via push_scope/pop_scope.

Scope management requires tracking:
- Block entry/exit
- my/our/local declarations
- Package changes

This is the job of a statement parser, not an expression parser.

For V1, the environment is INPUT to the parser - typically created by:
1. Scanning declarations in the file
2. Loading from a configuration
3. Building from known built-ins

=cut

=head1 USAGE WITH PARSER

    use Pl::PExpr;
    use Pl::Environment;

    # Create environment
    my $env = Pl::Environment->new(
        prototypes => { my_func => '$$' },
    );
    
    # Parse with environment
    my $parser = Pl::PExpr->new(
        e => $expr,
        environment => $env,
    );
    
    my $tree_id = $parser->parse_expr_to_tree($expr);

The parser can then use the environment to:
- Apply prototype-based parsing rules
- Handle bareword filehandles in print
- Validate lvalue contexts

=cut

1;

=head1 AUTHOR

Perl Expression Parser Project

=head1 SEE ALSO

L<Pl::PExpr>, L<Pl::PExpr::Config>

=cut
