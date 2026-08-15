package PCLSbcl;
# The ONE place that builds the SBCL command line a PCL runner spawns.
#
# WHY THIS EXISTS (task #344, found by #324 in s399).  FOUR runners start SBCL
# to run transpiled code — the gate (Pl/t/PCLCore.pm), the sweep
# (sweep-perl-tests.pl), the companion suite (tools/run-perl-suite.pl) and
# ./runpcl — and each used to hand-write its own option string.  They must
# agree about everything that changes what PCL *is* while a test runs (stack
# size, which libraries are loaded, the cache setting), and they agreed only by
# hand.  When one drifts, the difference reads as a PCL bug in whichever
# measurement is the odd one out:
#
#   * s399/#324: run-perl-suite.pl had NO --control-stack-size, so the
#     companion suite ran PCL on SBCL's 2 MB default while every other
#     measurement used 512 MB.  Four files died `control-stack-exhausted`
#     THERE and nowhere else; one had its snapshot row blaming `(?{ CODE })`
#     and a task filed against the wrong cause.  The probes in that task did
#     not reproduce because they went through ./runpcl, which had the flag.
#   * earlier: the sweep's `--load` of cl/pcl-test.lisp was the same shape.
#
# So: the prefix — everything between `sbcl` and the caller's own --load/--eval
# arguments — is built here, once.  Callers still choose WHAT to load; they no
# longer choose the stack size, the banner flags, or the --core placement.
#
# NB: --core is a C RUNTIME option and must precede the toplevel options, or
# SBCL aborts with "C runtime option --core in the middle of Lisp options".
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(sbcl_prefix sbcl_prefix_str);

# Control stack, MB.  PCL recurses deeply in both the compiler and the runtime;
# SBCL's 2 MB default is not enough (#324).  Changing the value is a decision
# about all four runners at once, which is the point of it living here.
our $STACK_MB = 512;

# The arguments between `sbcl` and the caller's own --load/--eval, as a LIST.
#   core mode:    --core <core> --control-stack-size N --noinform --non-interactive
#   source mode:  --control-stack-size N --noinform --non-interactive --load <runtime>
#
# Options:
#   runtime  => path to cl/pcl-runtime.lisp (source mode; omit to load nothing)
#   core     => path to a saved core, or '' / undef for source mode
#   env_core => 1: take the core from $ENV{PCL_TEST_CORE} when it is fresher
#               than the runtime (the gate's contract — a hand-set stale core
#               can never mask a runtime edit).  An explicit `core` wins.
#   stack_mb => override $STACK_MB for this call
sub sbcl_prefix {
    my (%o) = @_;
    my @base = ('--control-stack-size', $o{stack_mb} // $STACK_MB,
                '--noinform', '--non-interactive');
    my $core = $o{core};
    if ($o{env_core} && !(defined $core && length $core)) {
        my $c = $ENV{PCL_TEST_CORE};
        $core = $c if $c && $c ne '1' && -f $c && _fresh($c, $o{runtime});
    }
    return ('--core', $core, @base) if defined $core && length $core;
    return (@base, defined $o{runtime} ? ('--load', $o{runtime}) : ());
}

# The same thing as a shell string, starting with `sbcl`.
#
# `quote`: whether PATH arguments are quotemeta'd (\Q..\E).  The three string
# callers disagreed about this before the move and the difference is preserved
# so that the #344 command lines are byte-identical to the ones they replace:
# run-perl-suite.pl quoted, the sweep and ./runpcl did not.  Nothing in the
# repo has a path with a shell metacharacter, so the unquoted callers work —
# but they would break on one, which is why quote => 1 is the default.
sub sbcl_prefix_str {
    my (%o) = @_;
    my $quote = exists $o{quote} ? delete $o{quote} : 1;
    my @args  = sbcl_prefix(%o);
    my %is_path;   # only the path arguments get quoted, never the flags
    for my $i (0 .. $#args - 1) {
        $is_path{$i + 1} = 1 if $args[$i] eq '--core' || $args[$i] eq '--load';
    }
    return join ' ', 'sbcl',
        map { $quote && $is_path{$_} ? quotemeta($args[$_]) : $args[$_] } 0 .. $#args;
}

# A core is usable only if it is at least as new as the runtime it must reflect.
sub _fresh {
    my ($core, $runtime) = @_;
    return 0 unless defined $runtime && -f $core && -f $runtime;
    return (stat $core)[9] >= (stat $runtime)[9];
}

1;
