# Moo subclass empty-attrs bug — MGC investigation (session 250)

**Status:** root cause precisely localized, NOT fixed. This doc is the handoff
for turning the localization into a fix plan next session.

## Symptom

A Moo subclass builds an **empty** object:

```perl
package Animal; use Moo; has name => (is=>'ro'); has sound => (is=>'ro', default=>sub{'generic'});
package Dog;    use Moo; extends 'Animal'; has breed => (is=>'ro', default=>sub{'mutt'});
package main;
my $d = Dog->new(name=>'Rex', breed=>'lab');   # PCL: name/sound/breed all UNDEF
```

`Dog->new` returns a correctly-blessed `Dog` that `isa Animal`, but every
attribute is undef. Single-class Moo (`Animal->new`) works fully. The s249
infinite-loop fix holds — this is a *separate* downstream bug. The s250
string-eval lexical-capture work did **not** fix it.

Repro: `/tmp/moo_probe.pl` (recreate from the snippet above, with `print`s for
`name`/`breed`). Run `perl -I lib FILE` (oracle) vs `./runpcl FILE` (PCL).

## The failure chain (verified)

1. **`Dog->can('new')` returns the *inherited* `Animal::new`** — Dog's own
   constructor is never installed (`defined &Dog::new` = NO; `Dog->can('new')`
   has the same coderef id as `Animal->can('new')`).
2. So `Dog->new` runs `Animal::new`, hits its
   `if ($class ne "Animal") { if ($Moo::MAKERS{$class}{constructor}) { return $invoker->SUPER::new(@_) } }`
   branch (because `MAKERS{Dog}{constructor}` is truthy) →
   `$invoker->SUPER::new` → `Moo::Object::new` → bare `bless {}` → empty object.
   In real perl this SUPER branch is only reached *via* Dog's own constructor;
   in PCL it's the primary path because Dog::new doesn't exist as a distinct sub.
3. **Dog::new isn't installed** because `Method::Generate::Constructor::install_delayed`
   does `defer_sub "${package}::new"` with **`$self->{package}` = "" (empty)**.
   `defer_sub "::new"` installs nothing useful.
   (Instrumented MGC `install_delayed`: `pkg=[] self-keys=[]` for the Dog maker;
   perl shows `pkg=[Dog] self-keys=[accessor_generator construction_string package subconstructor_handler]`.)
4. **package="" because the maker object came back EMPTY** — `MGC->new(%construct_opts)`
   for Dog stored *none* of its args (`selfkeys={}`), so `{package}` is undef→"".

## What is RULED OUT (this is the advance over the pre-250 guesses)

- **Argument flattening / `{@_}` key-value pairing shift** (the prior main
  hypothesis): feeding the *real* `construction_string` value — and blessed,
  non-empty hashref values — through a plain `sub { my %a = @_ }` preserves every
  key in PCL. Not a flatten bug.
- **Re-entrancy** ("call 3 from inside the eval'd `Animal::new`"): the empty
  install fires at **setup time**, during Dog's `has breed` (which calls
  `_constructor_maker_for(Dog)`), *before* any `Dog->new`. Not re-entrant.
- **Wrong invoker / wrong opts**: instrumenting `Moo::_constructor_maker_for`
  right before `->new(%construct_opts)` shows, for the Dog call:
  `target=Dog`, `con=Method::Generate::Constructor`,
  **`invoker=Method::Generate::Constructor`**, `optpkg=Dog`,
  `optkeys=[accessor_generator construction_string package subconstructor_handler]`
  — all correct and **identical to real perl**.
- **`ref($con)` wrong**: `ref($Moo::MAKERS{Animal}{constructor})` correctly returns
  `Method::Generate::Constructor` in PCL.
- **A clean Moo class with the same four attributes** (`package`,
  `accessor_generator`, `subconstructor_handler`, `construction_string`) stores
  all of them correctly in PCL.

## NARROWED TO

`Method::Generate::Constructor->new(%opts)` is called with **perfect inputs**
(invoker = the MGC class, `package=Dog`, all keys present) yet returns an EMPTY
object **only for the call that carries `construction_string`** (the 3rd call;
the MGC-self and Animal calls — which have no `construction_string` — store
fine).

Since the invoker IS `"Method::Generate::Constructor"`, the bootstrap `new`'s
`if ($class ne "Method::Generate::Constructor") {...}` guard should be FALSE and
it should fall through to the store-chain. The fact that it returns empty means
either:
  (a) it took the `$class ne ...` subconstructor branch anyway (→ `SUPER::new`,
      empty) — i.e. `$class` was NOT `"Method::Generate::Constructor"` *inside*
      the body even though the invoker was; or
  (b) dispatch invoked a *different, empty* `MGC::new` body — the long-suspected
      **"2nd bootstrap-body function"** (memory: a distinct fn appears at MGC
      build that still runs the bootstrap body but is a different object than the
      load-time `\&new`).

MGC's bootstrap `new` body (from `SUB_QUOTE_DEBUG=1 ./runpcl` → `/tmp/sqdump.txt`,
lines ~9–52) is the chicken-and-egg constructor: it has the
`if ($class ne "Method::Generate::Constructor")` subconstructor branch and an
inline `my $args = ... {@_}` (no `BUILDARGS` hook), then the
`(exists $args->{K} and ($new->{K}=$args->{K})),` store-chain (which DOES include
a `construction_string` line), then `return $new`.

## Next probe (turn this into the plan)

Goal: observe what happens *inside* `MGC::new` for the Dog call — specifically
`$class` (the `ref($invoker)?...:$invoker` value) and which code path is taken.
`BUILDARGS` override won't help (the bootstrap `new` inlines `{@_}`). Options:

1. **Identify which coderef runs.** Dump `\&Method::Generate::Constructor::new`'s
   id at load time, then dump the id of the coderef that `ref($con)->new`
   actually dispatches to for the Dog call (e.g. via an instrumented wrapper or
   by comparing `Method::Generate::Constructor->can('new')` id across the 3
   calls). If the 3rd call hits a different id → confirms (b), the 2nd-body
   theory → look at PCL sub-storage / glob-CODE vs defun for MGC::new and why a
   2nd body exists / is dispatched.
2. **Force a single MGC::new body** (diagnostic): if there are two, make dispatch
   pick the load-time one and see if Dog then stores → confirms (b).
3. **Check `$class` inside the body** by temporarily replacing MGC's `new` (in the
   lib/ copy) with a hand-written equivalent that `warn`s `$class` and `keys %$args`,
   then runs the same store logic. If `$class ne "Method::Generate::Constructor"`
   there despite a correct invoker → a PCL `ref()`/`$invoker` issue specific to
   how this bootstrap sub is called (e.g. the coderef is invoked with a shifted
   `@_`, dropping the invoker).

Hypothesis to test first: **the 3rd `MGC::new` invocation receives `@_` shifted by
one** (invoker lost), so `$invoker`=`package`, `$class`=`"Dog"` → `ne "MGC"` TRUE
→ `MAKERS{Dog}{constructor}` truthy → `SUPER::new` → empty. That would also
explain why only the construction_string-bearing call differs IF that call goes
through a different (deferred/2nd-body) path with a different calling convention.

## Tooling / repro recipe

- **Instrument**: `cp $(perl -MMoo -e 'print $INC{"Moo.pm"}') lib/Moo.pm` and
  `cp $(perl -MMethod::Generate::Constructor -e 'print $INC{"Method/Generate/Constructor.pm"}') lib/Method/Generate/Constructor.pm`,
  `chmod u+w`, add `warn`s. **REMOVE the copies afterwards** — they shadow
  site_perl and will break everything (and the gate) if left.
- **Dump generated subs**: `SUB_QUOTE_DEBUG=1 ./runpcl /tmp/moo_probe.pl > /tmp/sqdump.txt 2>&1`.
  Host pl2cl's own Moo pollutes the dump — grep your package / the MGC `new`.
- Run perl with `-I lib` to make perl honor the instrumented copies as the oracle.

## Relevant source

- `Moo.pm` `_constructor_maker_for` (the `($con?ref($con):'MGC')->new(%construct_opts)->install_delayed->register_attribute_specs` chain; the `grep +(defined &{$_.'::new'}), @isa` parent-finder).
- `Moo.pm` `_set_superclasses` (extends): sets `@ISA`, conditionally
  `delete _getstash($target)->{new}` (s247 stash-delete → `fmakunbound`). For a
  fresh subclass this branch is NOT taken (no prior constructor), so it's not the
  trigger here.
- `Method/Generate/Constructor.pm` `install_delayed` (`defer_sub "${package}::new"`).
- PCL: `p-method-call` / `->can` resolution, glob CODE-slot side table vs defun,
  s247 stash-delete (`p-delete` + `fmakunbound`), s249 stable coderef identity
  (`object-address` weak id table).

See `memory/project_moo_progress.md` and `memory/project_coderef_identity_blocker.md`.
