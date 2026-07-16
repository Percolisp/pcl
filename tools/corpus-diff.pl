#!/usr/bin/env perl
# corpus-diff.pl — standing-rule #1 verification: byte-diff the emitted CL
# of the WORKING TREE against a git ref (default HEAD) over the perl-tests
# corpus, and report exactly which files' emission changed.
#
# Usage:
#   tools/corpus-diff.pl                    # working tree vs HEAD, whole corpus
#   tools/corpus-diff.pl HEAD~1             # vs another ref
#   tools/corpus-diff.pl HEAD f1.t f2.t     # subset of perl-tests files
#   tools/corpus-diff.pl --show ...         # also print the diff hunks
#                                           # (first 60 lines/file; --show=all
#                                           # for everything, --show=N to cap)
#
# For an E1 de-gate session the acceptance is: ONLY the de-gated files
# appear in the output, and each diff is explained.  Exit status: 0 when
# identical, 1 when files differ (so it can gate scripts).
#
# What it gets right that ad-hoc reruns keep getting wrong (s287):
#   - the SAME input files (this tree's perl-tests/) are fed to BOTH
#     compilers — a ref that changed perl-tests must not pollute the diff;
#   - BOTH repo roots are normalized to `ROOT` — the ref runs from a temp
#     worktree whose own absolute path is embedded in @INC/pl2cl-path
#     forms, and the working tree embeds this repo's path; missing either
#     mapping makes every file "differ";
#   - the pipeline marker line (`;;; pcl: pipeline=… gen=…`) is stripped
#     in perl, not grep (emitted CL can contain NUL bytes, which silence
#     grep) — the cache generation string lives there and always differs
#     across an emission-changing commit;
#   - the temp worktree is always removed (END block), even on die.
use strict;
use warnings;
use File::Temp   qw(tempdir);
use File::Path   qw(make_path);
use FindBin      qw($RealBin);
use Cwd          qw(abs_path);

my $root = abs_path("$RealBin/..");
chdir $root or die "chdir $root: $!";

my $show;   # undef = filenames only; 0 = unlimited; N = first N diff lines
for my $i (reverse 0 .. $#ARGV) {
    next unless $ARGV[$i] =~ /^--show(?:=(\w+))?$/;
    $show = !defined $1 ? 60 : $1 eq 'all' ? 0 : $1 + 0;
    splice @ARGV, $i, 1;
}

my $ref = (@ARGV && $ARGV[0] !~ /\.t$/) ? shift @ARGV : 'HEAD';
system("git rev-parse --verify --quiet \Q$ref\E^{commit} >/dev/null") == 0
    or die "not a commit: $ref\n";

my @files = @ARGV ? map { m{/} ? $_ : "perl-tests/$_" } @ARGV
                  : sort glob("perl-tests/*.t");
-f $_ or die "no such file: $_\n" for @files;

my $tmp = tempdir("pcl-corpus-diff-XXXXXX", TMPDIR => 1, CLEANUP => 1);
my $wt  = "$tmp/ref-tree";
system("git worktree add --quiet \Q$wt\E \Q$ref\E") == 0
    or die "git worktree add failed\n";
END {
    # local $? — a system() in an END block otherwise overwrites the
    # process exit status (perlmod: END and $?).
    local $?;
    if (defined $wt && -d $wt) {
        system("git -C \Q$root\E worktree remove --force \Q$wt\E >/dev/null 2>&1");
    }
}

make_path("$tmp/new", "$tmp/ref");

sub transpile {
    my ($pl2cl_dir, $out_dir) = @_;
    for my $f (@files) {
        (my $base = $f) =~ s{.*/}{};
        # Inputs always come from THIS tree; only the compiler differs.
        system("cd \Q$pl2cl_dir\E && ./pl2cl < \Q$root/$f\E > \Q$out_dir/$base\E.lisp 2>/dev/null");
    }
}
transpile($root, "$tmp/new");
transpile($wt,   "$tmp/ref");

my $norm = sub {
    my ($path) = @_;
    open my $fh, '<', $path or return '';
    local $/;
    my $t = <$fh>;
    $t =~ s/^;;; pcl: pipeline=.*\n//m;
    $t =~ s/\Q$wt\E/ROOT/g;
    $t =~ s/\Q$root\E/ROOT/g;
    return $t;
};

my @changed;
for my $f (@files) {
    (my $base = $f) =~ s{.*/}{};
    push @changed, $base
        if $norm->("$tmp/new/$base.lisp") ne $norm->("$tmp/ref/$base.lisp");
}

if (@changed) {
    printf "%d of %d files differ vs %s:\n", scalar @changed, scalar @files, $ref;
    print "  $_\n" for @changed;
    if (defined $show) {
        for my $base (@changed) {
            for my $side (qw(ref new)) {
                open my $fh, '>', "$tmp/$side.norm" or die "write $side.norm: $!";
                print $fh $norm->("$tmp/$side/$base.lisp");
                close $fh;
            }
            my @lines = qx(diff -u \Q$tmp\E/ref.norm \Q$tmp\E/new.norm);
            splice @lines, 0, 2;   # drop the +++/--- tempfile header
            my $n = @lines;
            splice @lines, $show if $show && $n > $show;
            print "\n=== $base (-ref +new, $n diff lines"
                . ($show && $n > $show ? ", first $show shown" : '') . ")\n";
            print @lines;
        }
    } else {
        print "(inspect: diff the normalized outputs; every changed file must be explained)\n";
    }
    exit 1;
}
printf "emission identical to %s across %d files\n", $ref, scalar @files;
exit 0;
