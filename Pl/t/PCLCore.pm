package PCLCore;
# Optional saved-core acceleration for the test suite.
#
# `--load`ing the runtime SOURCE recompiles it (~1.2s) on EVERY sbcl spawn, and
# the big transpile-test files spawn sbcl once per test — so runtime compilation
# dominates the gate.  A saved core (an SBCL image with the runtime already
# compiled in) drops that to ~0.003s.
#
# This module is a pure CONSUMER: it never builds a core.  The core is built
# FRESH, once per run, by tools/prove-core, which points PCL_TEST_CORE at it.
# Building every run (rather than caching one across runs) is deliberate — a
# stale core would silently run tests against OLD runtime code.  As a second
# guard, sbcl_prefix refuses a core older than the runtime and falls back to
# source-load, so a hand-set PCL_TEST_CORE can never mask a runtime edit.
use strict;
use warnings;

# sbcl args that go BETWEEN `sbcl` and the caller's `--load <cl_file>`:
#   core mode:   --core <core> --control-stack-size 512 --noinform --non-interactive
#   source mode: --control-stack-size 512 --noinform --non-interactive --load <runtime>
# NB: --core is a RUNTIME option and must precede the toplevel options, or SBCL
# aborts with "C runtime option --core in the middle of Lisp options" (the same
# trap the pcl wrapper hit).  The caller appends `--load <cl_file>` after these.
sub sbcl_prefix {
    my ($runtime) = @_;
    my @base = ('--control-stack-size', '512', '--noinform', '--non-interactive');
    my $core = $ENV{PCL_TEST_CORE};
    if ($core && $core ne '1' && -f $core && _fresh($core, $runtime)) {
        return ('--core', $core, @base);
    }
    return (@base, '--load', $runtime);
}

# A core is usable only if it is at least as new as the runtime it must reflect.
sub _fresh {
    my ($core, $runtime) = @_;
    return 0 unless -f $core && -f $runtime;
    return (stat $core)[9] >= (stat $runtime)[9];
}

1;
