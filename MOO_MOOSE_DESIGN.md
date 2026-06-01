# Moo/Moose Support for PCL

## Executive Summary

This document outlines how to support Moo and Moose OO frameworks in the PCL Perl-to-Common-Lisp transpiler. While Moo/Moose's metaprogramming *could* theoretically be transpiled (CL supports dynamic symbol manipulation), they rely heavily on `eval "string"` which requires a runtime transpiler (self-hosting). As a pragmatic workaround until self-hosting is available, we **recognize Moo/Moose patterns in user code and emit equivalent CLOS constructs** directly.

---

## The Challenge

### Why Not Just Transpile Moo/Moose Directly?

A reasonable question: if we can transpile Perl to CL, why not just transpile Moo.pm itself and let it do its metaprogramming in CL?

**Symbol table manipulation IS translatable.** CL is dynamic and can do runtime symbol manipulation:

```perl
# Perl:
*{"${pkg}::${name}"} = sub { ... };
```
```lisp
;; CL equivalent:
(setf (symbol-function (intern name pkg))
      (lambda () ...))
```

So basic metaprogramming isn't the blocker. The real issues are:

### The Real Blockers

**1. `eval "string"` — The Core Problem**

Moo/Moose generate Perl code as strings and eval them:
```perl
# Inside Moo's has() implementation:
my $code = "sub $attr_name { \$_[0]->{$attr_name} }";
eval $code;
```

This requires a **Perl parser at runtime**. To transpile this, we'd need the transpiler available at runtime (self-hosting), not just symbol manipulation.

**2. Dependency Chain**

Moo.pm depends on dozens of modules (Sub::Quote, Class::Method::Modifiers, Role::Tiny, etc.) that also use metaprogramming and eval. We'd need to transpile the entire ecosystem.

**3. Perl-specific Introspection**

Things like `caller()` stack inspection and `B::` bytecode introspection have no direct CL equivalents.

### Why Pattern-Matching Instead?

The pattern-matching approach is a **pragmatic workaround**, not a fundamental necessity:

| Approach | Pros | Cons |
|----------|------|------|
| **Pattern-matching** (this plan) | Works now, no runtime transpiler needed | Only handles known patterns, brittle |
| **Self-hosting** (transpile Moo.pm) | General solution, works for any OO framework | Requires `eval "string"` support (P5 roadmap) |

**With self-hosting (future):**
```
User code: "use Moo; has 'name'"
  → Transpile Moo.pm and dependencies
  → Run transpiled Moo code
  → Moo does eval "sub name {...}"
  → Runtime transpiler converts that to CL
  → Symbol installed via (setf (symbol-function ...))
```

**With pattern-matching (current plan):**
```
User code: "use Moo; has 'name'"
  → Recognize pattern directly
  → Emit CLOS equivalent
  → Skip Moo.pm entirely
```

### Performance Considerations

Would we gain speed with pattern-matching vs. self-hosting?

| Aspect | Pattern-matching | Self-hosting |
|--------|------------------|--------------|
| **Load time** | Faster (no runtime transpilation) | Slower (transpile on each `has`) |
| **Runtime** | CLOS slot access (native, optimized) | Likely hash-based access |
| **Memory** | CLOS instances (compact) | Hash-table per object |

The performance difference is **modest** for most applications—SBCL compiles fast and hash access isn't slow.

The bigger wins from pattern-matching are **ergonomic**:
- **Integration**: CLOS objects work with CL debuggers, inspectors, type declarations
- **Idiomacy**: Generated code is readable CL, not Perl-isms running on CL
- **Interop**: CLOS objects can inherit from native CL libraries

### Conclusion

Pattern-matching is chosen because:
1. It works **now** without waiting for self-hosting
2. It produces **better** CL code (native CLOS vs. emulated Perl OO)
3. Self-hosting would unlock this automatically, but is a larger project

Once `eval "string"` works (P5 in roadmap), directly transpiling Moo/Moose becomes viable and would handle edge cases pattern-matching can't.

---

### The Solution

**Recognize Moo/Moose usage patterns and emit CLOS equivalents:**

```
Perl with Moo/Moose    ->    PCL Parser recognizes    ->    CLOS + Runtime macros
                              'use Moo/Moose'
                              'has', 'extends', etc.
```

This approach leverages CLOS (Common Lisp Object System) which is actually **more powerful** than Perl's OO system, making this a feature rather than a limitation.

---

## Moo/Moose Features to Support

| Feature | Priority | CLOS Equivalent | Status |
|---------|----------|-----------------|--------|
| `has` (attributes) | Critical | CLOS slots + accessor methods | Planned |
| `extends` | Critical | CLOS superclasses | Works via @ISA |
| `with` (roles) | High | CLOS mixins | Planned |
| `before`/`after`/`around` | High | CLOS method combinations | Planned |
| `BUILD` | Medium | `:after` on `initialize-instance` | Planned |
| `BUILDARGS` | Medium | Custom constructor wrapper | Planned |
| `DEMOLISH` | Low | Finalizers | Planned |
| Type constraints | Medium | Runtime type checks | Planned |
| `required` | Medium | Constructor validation | Planned |
| `default`/`builder` | Critical | `:initform` / lazy slots | Planned |
| `lazy` | Medium | Delayed slot computation | Planned |
| `trigger` | Low | Setter side-effects | Planned |
| `clearer`/`predicate` | Low | Additional accessor methods | Planned |

### What Will NOT Be Supported

- **Full Moose MOP** (Meta-Object Protocol) - Too complex
- **Runtime class/attribute modification** - Requires Perl runtime
- **Complex type coercions** - Would need full type system
- **MooseX::* extensions** - Third-party, too varied
- **Attribute traits** - Advanced metaprogramming
- **Native delegation** (`handles => { ... }`) - Complex dispatch

---

## Phase 1: Core Attribute System (`has`)

### 1.1 Detect Moo/Moose Usage

**File: `Pl/Parser.pm`**

In `_process_include_statement()`, detect Moo/Moose:

```perl
if ($module eq 'Moo' || $module eq 'Moose') {
    $self->environment->set_oo_framework($module);
}
if ($module eq 'Moo::Role' || $module eq 'Moose::Role') {
    $self->environment->set_is_role(1);
    $self->environment->set_oo_framework($module =~ /Moo/ ? 'Moo' : 'Moose');
}
```

**File: `Pl/Environment.pm`**

Add tracking:
```perl
has oo_framework => (is => 'rw', default => '');  # 'Moo', 'Moose', or ''
has is_role => (is => 'rw', default => 0);
has moo_attributes => (is => 'rw', default => sub { {} });  # pkg -> [attrs]
```

### 1.2 Parse `has` Declarations

**Syntax to recognize:**
```perl
has 'name' => (is => 'ro');
has 'name' => (is => 'rw', default => 42);
has 'name' => (is => 'ro', required => 1);
has 'name' => (is => 'lazy', builder => '_build_name');
has 'name' => (is => 'ro', isa => 'Str');
has ['attr1', 'attr2'] => (is => 'rw');  # Multi-attribute
has name => (is => 'ro');  # Bareword
```

**File: `Pl/Parser.pm`**

Add `_process_has_statement()`:
```perl
sub _process_has_statement {
    my ($self, $stmt) = @_;

    # Extract attribute name(s) - string, bareword, or arrayref
    # Extract options hash: is, default, builder, required, isa, etc.
    # Store in environment for code generation

    my $attr = {
        name      => $name,
        is        => $options{is} // 'bare',
        default   => $options{default},
        builder   => $options{builder},
        required  => $options{required} // 0,
        lazy      => ($options{is} eq 'lazy' || $options{lazy}),
        isa       => $options{isa},
        trigger   => $options{trigger},
        clearer   => $options{clearer},
        predicate => $options{predicate},
    };

    $self->environment->add_moo_attribute($pkg, $attr);
}
```

### 1.3 Generate CLOS Slots and Accessors

**File: `Pl/Parser.pm`** (in package epilogue or at end of parsing)

For each `has` declaration, emit:

```lisp
;; has 'name' => (is => 'rw', default => 42);

;; 1. CLOS class with slot
(defclass person ()
  ((name :initarg :name :initform 42)))

;; 2. Accessor method (Perl-style: $obj->name or $obj->name($val))
(defun pl-name ($self &optional $value $value-supplied-p)
  (if $value-supplied-p
      (setf (slot-value $self 'name) $value)  ;; setter
      (slot-value $self 'name)))               ;; getter
```

For `is => 'ro'`:
```lisp
(defun pl-name ($self)
  (slot-value $self 'name))
```

For `is => 'lazy'`:
```lisp
(defun pl-name ($self)
  (unless (slot-boundp $self 'name)
    (setf (slot-value $self 'name) (pl-_build_name $self)))
  (slot-value $self 'name))
```

### 1.4 Generate Constructor

Moo/Moose auto-generate `new`:

**File: `cl/pcl-runtime.lisp`**

```lisp
(defun pl-moo-new (class &rest args)
  "Moo/Moose-style constructor"
  (let* ((clos-class (find-class (perl-pkg-to-clos-class class)))
         (obj (apply #'make-instance clos-class args)))
    ;; Validate required attributes
    ;; Call BUILD if defined
    obj))
```

### Phase 1 Tests

```perl
# Pl/t/moo-basic-01.t

package Person;
use Moo;

has 'name' => (is => 'ro', required => 1);
has 'age' => (is => 'rw', default => 0);

package main;
my $p = Person->new(name => 'Alice');
is($p->name, 'Alice', 'ro accessor works');
is($p->age, 0, 'default value works');
$p->age(25);
is($p->age, 25, 'rw setter works');
```

---

## Phase 2: Inheritance (`extends`)

### 2.1 Parse `extends` Statement

**Syntax:**
```perl
extends 'Parent';
extends 'Parent1', 'Parent2';
```

**File: `Pl/Parser.pm`**

Add `_process_extends_statement()`:
```perl
sub _process_extends_statement {
    my ($self, $stmt) = @_;
    # Extract parent class names
    # Equivalent to: our @ISA = qw(Parent1 Parent2);
    # Use existing _process_isa_declaration logic
}
```

This is essentially syntactic sugar for `@ISA` which already works.

### Phase 2 Tests

```perl
package Animal;
use Moo;
has 'name' => (is => 'ro');

package Dog;
use Moo;
extends 'Animal';
has 'breed' => (is => 'ro');

my $dog = Dog->new(name => 'Rex', breed => 'Lab');
is($dog->name, 'Rex', 'inherited attribute works');
```

---

## Phase 3: Roles (`with`)

### 3.1 Parse Role Definition

```perl
package Printable;
use Moo::Role;

requires 'as_string';

has 'print_prefix' => (is => 'ro', default => '');

sub print_me {
    my $self = shift;
    print $self->print_prefix . $self->as_string;
}
```

**File: `Pl/Parser.pm`**

- Detect `use Moo::Role` / `use Moose::Role`
- Parse `requires 'method_name'` statements
- Track role attributes and methods separately

### 3.2 Parse `with` Statement

```perl
package Document;
use Moo;
with 'Printable';
with 'Serializable', 'Comparable';
```

### 3.3 Generate CLOS Mixins

CLOS doesn't have roles, but mixins work similarly:

```lisp
;; Role becomes a mixin class
(defclass printable-mixin ()
  ((print-prefix :initarg :print-prefix :initform "")))

(defun printable-print-me ($self)
  (pl-print (pl-. (pl-print_prefix $self) (pl-as_string $self))))

;; Class composing role includes mixin in superclass list
(defclass document (printable-mixin) ())
```

**Runtime check for `requires`:**
```lisp
(defun check-role-requirements (class role-name required-methods)
  (dolist (method required-methods)
    (unless (method-exists-p class method)
      (error "Class ~A does not implement required method ~A from role ~A"
             class method role-name))))
```

### Phase 3 Tests

```perl
package Printable;
use Moo::Role;
requires 'as_string';
sub print_me { print shift->as_string }

package Doc;
use Moo;
with 'Printable';
sub as_string { "I am a doc" }

my $d = Doc->new;
is($d->as_string, "I am a doc");
# $d->print_me should work
```

---

## Phase 4: Method Modifiers

### 4.1 Parse Modifiers

```perl
before 'method_name' => sub { my $self = shift; ... };
after 'method_name' => sub { my $self = shift; ... };
around 'method_name' => sub {
    my ($orig, $self, @args) = @_;
    # ... call $self->$orig(@args) ...
};
```

### 4.2 Generate CLOS Method Combinations

CLOS has native support for method combinations:

```lisp
;; before 'save' => sub { validate($self) };
(defmethod pl-save :before ((self my-class))
  (pl-validate self))

;; after 'save' => sub { log($self) };
(defmethod pl-save :after ((self my-class))
  (pl-log self))

;; around 'save' => sub { my ($orig, $self) = @_; ... };
(defmethod pl-save :around ((self my-class))
  (let ((result (call-next-method)))
    ;; modify result
    result))
```

### Phase 4 Tests

```perl
package Audited;
use Moo;
has 'log' => (is => 'rw', default => sub { [] });

sub save { shift->{saved} = 1 }

before 'save' => sub {
    push @{shift->log}, 'before';
};

after 'save' => sub {
    push @{shift->log}, 'after';
};

my $a = Audited->new;
$a->save;
is_deeply($a->log, ['before', 'after']);
```

---

## Phase 5: Lifecycle Hooks

### BUILD

Called after construction:
```perl
sub BUILD {
    my ($self, $args) = @_;
    # Post-construction init
}
```

Generate:
```lisp
(defmethod initialize-instance :after ((self my-class) &rest args)
  (pl-BUILD self (apply #'make-hash-from-plist args)))
```

### BUILDARGS

Modify constructor args:
```perl
sub BUILDARGS {
    my ($class, @args) = @_;
    # Return hashref
}
```

Generate wrapper in `pl-moo-new`.

### DEMOLISH

Destructor:
```perl
sub DEMOLISH { ... }
```

Generate via SBCL finalizers:
```lisp
(sb-ext:finalize obj (lambda () (pl-DEMOLISH obj)))
```

---

## Phase 6: Type Constraints (Basic)

### Common Types

| Perl Type | CLOS Check |
|-----------|------------|
| `Str` | `(stringp val)` |
| `Int` | `(integerp val)` |
| `Num` | `(numberp val)` |
| `Bool` | `t` (always valid) |
| `ArrayRef` | `(vectorp val)` |
| `HashRef` | `(hash-table-p val)` |
| `CodeRef` | `(functionp val)` |
| `Maybe[X]` | `(or (null val) (X-check val))` |
| `Object` | `(pl-blessed-p val)` |

### Generate Type Validation

In setter:
```lisp
(defun pl-name ($self &optional $value $supplied-p)
  (when $supplied-p
    (unless (stringp (unbox $value))
      (error "Attribute 'name' must be Str, got ~A" (type-of $value)))
    (setf (slot-value $self 'name) $value))
  (slot-value $self 'name))
```

---

## Implementation Order

```
Phase 1: has (attributes)     [Critical - 3-4 days]
    |
    v
Phase 2: extends              [Critical - 0.5 days, mostly done via @ISA]
    |
    v
Phase 3: with (roles)         [High - 2-3 days]
    |
    v
Phase 4: method modifiers     [High - 1-2 days]
    |
    v
Phase 5: lifecycle hooks      [Medium - 1 day]
    |
    v
Phase 6: type constraints     [Medium - 2 days]
```

**Total: ~10-12 days**

---

## Files to Modify

### Pl/Parser.pm
- Detect `use Moo/Moose` in `_process_include_statement()`
- Add `_process_has_statement()` for attribute declarations
- Add `_process_extends_statement()` (wrapper around @ISA logic)
- Add `_process_with_statement()` for role composition
- Add `_process_requires_statement()` for role requirements
- Add `_process_method_modifier()` for before/after/around
- Emit CLOS class with slots at end of package

### Pl/Environment.pm
- Add `oo_framework` attribute ('Moo', 'Moose', or '')
- Add `is_role` flag
- Add `moo_attributes` hash: `{ pkg => [ {name, is, default, ...}, ... ] }`
- Add `moo_roles` hash for role tracking
- Add `role_requirements` for `requires` statements

### Pl/ExprToCL.pm
- Generate accessor methods for `has` declarations
- Handle method modifier code generation

### cl/pcl-runtime.lisp
- Add `pl-moo-new` constructor
- Add type checking utilities (`pl-check-type`)
- Add role composition helpers
- Add method modifier macros if needed

---

## Test Files to Create

- `Pl/t/moo-basic-01.t` - Basic has, ro/rw, defaults
- `Pl/t/moo-extends-01.t` - Inheritance with extends
- `Pl/t/moo-roles-01.t` - Role composition
- `Pl/t/moo-modifiers-01.t` - before/after/around
- `Pl/t/moo-lifecycle-01.t` - BUILD/BUILDARGS/DEMOLISH
- `Pl/t/moo-types-01.t` - Type constraints

---

## Verification

```bash
# Run Moo tests
prove -v Pl/t/moo-*.t

# Run full suite for regressions
prove Pl/t/
```

---

## Open Questions

1. **Moose vs Moo differences**: Moose has more features (full MOP, native types, more coercions). Support common subset first, which is essentially Moo's feature set.

2. **Role conflict resolution**: When two roles define the same method, Perl dies with "Due to a method name conflict...". We should detect this at transpile time.

3. **Lazy attribute storage**: Use CLOS `slot-boundp` to detect uninitialized lazy slots.

4. **Coercions**: Skip for now. Basic type constraints are sufficient for most use cases.

5. **Attribute inheritance**: `has '+attr' => (...)` extends parent attribute. Parse the `+` prefix and merge options.

---

## References

- [Moo Documentation](https://metacpan.org/pod/Moo)
- [Moose Documentation](https://metacpan.org/pod/Moose)
- [CLOS Specification](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node260.html)
- [SBCL MOP](http://www.sbcl.org/manual/#Metaobject-Protocol)

---

## Comparison: Perl Moo vs CLOS

| Concept | Perl Moo | CLOS |
|---------|----------|------|
| Class definition | `package Foo; use Moo;` | `(defclass foo () ...)` |
| Attributes | `has 'name' => (...)` | Slots in `defclass` |
| Inheritance | `extends 'Parent'` | Superclass list in `defclass` |
| Roles/Mixins | `with 'Role'` | Multiple inheritance |
| Method modifiers | `before/after/around` | `:before/:after/:around` methods |
| Constructor | Auto-generated `new` | `make-instance` |
| Type constraints | `isa => 'Type'` | Manual checks or CLOS types |
| Lazy attributes | `lazy => 1` | Custom accessor with `slot-boundp` |

CLOS is actually more powerful than Moo/Moose in several ways:
- True multiple inheritance with C3 MRO
- Method combinations are first-class
- Generic functions allow methods on multiple classes
- Full metaobject protocol for introspection

---

## Note: the `mro` module (deferred — revisit when integrating serious OO)

**Question raised (2026-06-01):** Does real Perl code actually use the `mro` module?
**Answer:** Yes, but almost entirely inside OO meta-frameworks and introspection
code — very rarely in plain application code. Split it into three distinct things,
because PCL would treat each differently:

### 1. The pragma `use mro 'c3';`
Switches a class from Perl's default DFS-flavored resolution to true C3. **Rarest in
the wild.** Most code that wants C3 does `use Class::C3` (the CPAN backport) or lets a
framework do it; direct `use mro 'c3'` appears mostly in framework base classes that
knowingly build diamond hierarchies.

PCL mapping is awkward: our runtime already resolves everything via CLOS's single fixed
C3 MRO, so a *per-class* DFS-vs-C3 choice at the Perl level doesn't map cleanly onto
"CLOS already picked C3 for everything." Low CPAN payoff. → `not-supported.md` territory.

### 2. `mro::get_linear_isa($class)` — **the high-value part**
Returns the linearized ancestor list. This is *everywhere* in the meta-object layer:
Class::MOP / Moose (`linearized_isa`), Moo / Role::Tiny, namespace::clean,
mro::compat / MRO::Compat, and assorted `*::Util` introspection helpers. Any
nontrivial Moose/Moo program calls it heavily under the hood.

**Cheap for us:** PCL already computes C3 linearization (`@ISA` → CLOS C3 MRO). This is
a *surfacing* problem, not a new algorithm — expose the existing linearization as a
callable function returning package names in MRO order. A thin shim over machinery we
already have.

### 3. `next::method` / `next::can` / `maybe::next::method`
The C3 "call the next method in the chain" dispatch (the MRO-aware cousin of `SUPER::`).
Travels together with `use mro 'c3'`; appears in code that opted into C3 (older
Catalyst guts, DBIx::Class, plugin systems). Depends on having the linearization, which
we have — maps reasonably onto `call-next-method` semantics.

The cache-management functions (`mro::invalidate_all_method_caches`,
`mro::method_changed_in`, `mro::set_mro`/`get_mro`) are purely framework-internal and
essentially never appear in user code — skip.

### Verdict for when we revisit serious OO
- **Pragma `use mro 'c3'`** → defer / not-supported (awkward mapping, low payoff).
- **`mro::get_linear_isa`** → implement; cheap given our existing C3 machinery, and it's
  the part real CPAN OO code actually exercises.
- **`next::method`** → implement alongside, maps onto `call-next-method`.

Decision deferred — note kept here so the analysis isn't re-derived. Pairs with the
Moo/Moose plan above (both are about running real CPAN OO frameworks).
