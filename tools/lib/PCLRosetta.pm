# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLRosetta;
# PCLRosetta -- the Rosetta corpus's RULES, in one place (task #2104, s543o;
# docs/rosetta-plan.md section 2).  tools/rosetta-corpus is the recipe that
# applies them; Pl/t/verdict-register-01.t reads the class list and the
# manifest from here; tools/t/rosetta-corpus.t drives them on a synthetic tree.
#
# LICENCE, A HARD RULE: Rosetta Code's text is GFDL 1.2.  No program text and
# no expected output is ever checked into the repository -- only these rules,
# the MANIFEST (names and digests: baselines/rosetta-corpus.tsv), the CAUSE
# TABLE (baselines/rosetta-causes.tsv) and the HISTORY of the number
# (baselines/rosetta-history.tsv).
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(static_refusal task_of program_id class_of read_manifest
                    @CLASSES $UPSTREAM $PINNED $AREA $SPARSE
                    $PERL_TIMEOUT $REBUILD_TIMEOUT $MAX_OUTPUT $ADDRESS_SPACE_KB);

# ── THE SOURCE ─────────────────────────────────────────────────────────────
# The pinned commit of the s493 clone (2026-09-20): every name and digest in
# the manifest is a fact of THIS commit.  A new commit is a new corpus, with a
# new manifest and a history that restarts -- never a silent refresh.
our $UPSTREAM = 'https://github.com/acmeism/RosettaCodeData';
our $PINNED   = '1d475861d7141ef2fafff293d9339f56e4b0f5db';
our $AREA     = 'rosetta';          # DIR/corpus/rosetta/NNNN-task.pl
our $SPARSE   = '/Task/*/Perl/*';   # the only paths the checkout needs (non-cone)

# ── THE STATIC SELECTION (verbatim from the s493 select.pl) ─────────────────
# A program is a candidate only if its text names nothing that touches a file,
# a process, the network, the clock or chance.  A crude word filter on
# purpose: it throws away good programs (any `open`, a `time` in a comment),
# never keeps a program whose answer depends on the world.  The NUMBER in a
# program's name is its place among the candidates in sorted path order, so
# changing this regex renumbers the corpus: it is part of the corpus's
# identity, like the commit.
my $STATIC_REFUSE = qr/\b(?:system|exec|fork|unlink|rmdir|rename|socket|kill|chmod|chown|truncate|mkdir|symlink|sleep|alarm|rand|srand|time|localtime|gmtime|times|threads|STDIN|ARGV|getc|readline|opendir|glob|open|sysopen|select|wait|waitpid|dump|syscall|ioctl|shm\w+|msg\w+|sem\w+)\b|`|<>|\$\$|\$0|%ENV|\bqx\b|IO::|Net::|LWP|HTTP|Socket|Tk\b|Gtk|SDL|Win32|Imager|GD\b|DBI|Time::|File::|Term::|Curses|OpenGL|Inline|FFI|XS\b/;

# The first word of the static filter a program's text matches, or undef.
sub static_refusal {
    my ($code) = @_;
    return $code =~ $STATIC_REFUSE ? $& : undef;
}

# The task of a candidate path RELATIVE to the clone ("Task/<task>/Perl/...pl"),
# or undef for a path of any other shape (the sparse checkout holds no other).
sub task_of {
    my ($rel) = @_;
    return $rel =~ m{^Task/([^/]+)/Perl/.*\.pl$}s ? $1 : undef;
}

# NNNN-task: the candidate's number, then its task with every run of
# non-alphanumerics made one `-`, cut to 40 bytes (the s493 spelling).
sub program_id {
    my ($n, $task) = @_;
    (my $t = $task) =~ s/[^A-Za-z0-9]+/-/g;
    return sprintf("%04d-%s", $n, substr($t, 0, 40));
}

# ── THE DYNAMIC ADMISSION (tools/rosetta-corpus build) ─────────────────────
# tools/everyday-smoke.pl --bless-expect is THE admission (three perl runs,
# one a copy at another path from another cwd, all agreeing in STDOUT and
# status), run with these two limits; then two rules of the s493 selection
# that the runner does not have: there must be output, and not too much.
our $PERL_TIMEOUT     = 8;           # seconds per perl run
our $ADDRESS_SPACE_KB = 4_000_000;   # `ulimit -v` per perl run (the s493 limit)
our $MAX_OUTPUT       = 200_000;     # bytes of STDOUT, at most
#
# THE CLOCK IS DECIDED ONCE.  The 8 s limit is a WALL-CLOCK rule, so a program
# near it is admitted on a quiet box and refused on a loaded or slower one
# (the first s543o build, at load 15, refused 0478-Fibonacci-word for time: it
# runs 7.8 s here at load 9 -- s493 and the 2026-11-24 bless admitted it).  A
# rebuild therefore does not re-decide it: when the manifest describes the
# commit being built, the programs it names are admitted at the runner's
# default timeout, and only the other candidates face the 8 s rule.  Every
# other rule (completion, three agreeing runs, the other path, output, size)
# is re-checked for every program; a corpus DEFINED fresh (no manifest for its
# commit) is defined by the 8 s rule alone.
our $REBUILD_TIMEOUT  = 60;          # tools/everyday-smoke.pl's default, the 2026-11-24 bless's

# ── THE CLASSES (docs/rosetta-plan.md section 3) ───────────────────────────
# A differing program's FIRST-COMPLAINT class: where the narrowing starts, not
# a cause.  The order of the tests IS the rule (the s543 classify.pl, with a
# timeout read from the runner's verdict).
our @CLASSES = qw(SILENT LOUD NOT-SUPPORTED DROPPED MODULE-MISSING NOTICE TIMEOUT EXHAUSTED);

# class_of($verdict, $rc, $want_rc, $err) -> a class, or undef for `same`.
sub class_of {
    my ($verdict, $rc, $want_rc, $err) = @_;
    return undef if $verdict eq 'same';
    $err = '' if !defined $err;
    return 'TIMEOUT'        if $verdict eq 'TIMEOUT';
    return 'DROPPED'        if $err =~ /statement dropped|PCL: refused|PCL: unsupported/;
    return 'NOT-SUPPORTED'  if $err =~ /not supported|not-supported/i;
    return 'MODULE-MISSING' if $err =~ /Can't locate|loadable object/;
    return 'EXHAUSTED'      if $err =~ /Heap exhausted|Control stack exhausted|Binding stack/i;
    return 'LOUD'           if $rc != $want_rc && $rc != 0;
    return 'NOTICE'         if $err =~ /\S/;
    return 'SILENT';
}

# ── THE MANIFEST (baselines/rosetta-corpus.tsv) ─────────────────────────────
#   area/name <TAB> sha256 of the .pl <TAB> sha256 of the .expect <TAB> exit status
# read_manifest($path) -> ({ key => { pl, expect, rc } }, \%header, undef)
#                      or (undef, undef, "why")
# The header facts are `# key: value` comment lines (source, commit, perl, programs).
sub read_manifest {
    my ($path) = @_;
    open my $fh, '<:raw', $path or return (undef, undef, "cannot read the manifest $path: $!");
    my (%rows, %head);
    while (defined(my $l = <$fh>)) {
        chomp $l;
        if ($l =~ /^#\s*(source|commit|perl|programs):\s*(.*?)\s*$/) { $head{$1} = $2; next }
        next if $l =~ /^\s*#/ || $l !~ /\S/;
        my @f = split /\t/, $l, -1;
        return (undef, undef, "$path line $.: expected 4 tab-separated fields, got " . scalar(@f))
            if @f != 4 || $f[1] !~ /^[0-9a-f]{64}$/ || $f[2] !~ /^[0-9a-f]{64}$/ || $f[3] !~ /^\d+$/;
        return (undef, undef, "$path line $.: $f[0] is listed twice") if $rows{ $f[0] };
        $rows{ $f[0] } = { pl => $f[1], expect => $f[2], rc => $f[3] };
    }
    close $fh;
    return (\%rows, \%head, undef);
}

1;
