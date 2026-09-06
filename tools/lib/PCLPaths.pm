# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLPaths;
# Paths PCL's tools need that live OUTSIDE the checkout — derived, never
# hard-coded (task #278: a grep for an absolute home-directory path over Pl/
# tools/ cl/ lib/ must come back empty, so the repo runs on a machine that is
# not the author's — the guard is Pl/t/no-hardcoded-paths-01.t).
#
# Two paths today:
#
#   perl_suite_t — perl's own t/ tree, the companion suite's corpus.  It is not
#   an installed thing — it exists only in a perl BUILD tree — so it cannot come
#   from %Config alone.  Three sources, in order, and a die naming the override
#   when none of them resolves; a WRONG guess must never look like an empty
#   corpus.
#
#   cache_root — the root of every per-user cache PCL writes.  THE one Perl-side
#   reading of $PCL_CACHE_DIR (task #1303): `pcl`, tools/lib/PCLSbcl.pm and
#   Pl/ProtoCache.pm had three copies of the expression and the runtime had a
#   fourth that was evaluated at CORE-BUILD time, i.e. the wrong process.
use strict;
use warnings;
use Config;
use File::Basename qw(dirname);
use Exporter 'import';
our @EXPORT_OK = qw(perl_suite_t cache_root);

# The t/ directory of the perl BUILD tree matching the running perl.
#   1. $PCL_PERL_SUITE_T                      — explicit, always wins
#   2. $PERLBREW_ROOT/build/perl-V/perl-V/t   — perlbrew's layout
#   3. <prefix>/../../build/perl-V/perl-V/t   — the same, derived from
#      %Config{prefix} for a shell that never exported PERLBREW_ROOT (cron)
sub perl_suite_t {
    my $v = $Config{version};
    my @cand;
    push @cand, $ENV{PCL_PERL_SUITE_T} if defined $ENV{PCL_PERL_SUITE_T} && length $ENV{PCL_PERL_SUITE_T};
    push @cand, "$ENV{PERLBREW_ROOT}/build/perl-$v/perl-$v/t" if defined $ENV{PERLBREW_ROOT};
    push @cand, dirname(dirname($Config{prefix})) . "/build/perl-$v/perl-$v/t";
    for my $c (@cand) { return $c if -d $c }
    die "PCLPaths: cannot find the t/ tree of perl $v (tried: @cand).\n"
      . "Set PCL_PERL_SUITE_T to the t/ directory of a perl-$v build tree.\n";
}

# ---------------------------------------------------------------- cache root
#
# THE root of every per-user cache PCL writes: the cached module transpiles
# under modules/, the saved cores under core/, the transpiler's prototype memo
# under proto/, the XS artifacts under xs/.
#
#   $PCL_CACHE_DIR when it is set and non-empty, else <home>/.pcl-cache
#
# where <home> is $HOME, falling back to the passwd entry of the running uid
# (and, if even that is unknown, the current directory — a cache is a
# convenience, never a reason to die).  Trailing slashes are trimmed so the
# answer concatenates cleanly with "/core", "/proto", "/modules".
#
# READ AT CALL TIME, in the process that uses it — never memoised into a saved
# artifact.  The runtime spells exactly this in %p-default-cache-dir
# (cl/pcl-runtime.lisp), from an sb-ext:*init-hooks* entry, because its own
# value used to be a defparameter initform evaluated when a saved CORE was
# built: $PCL_CACHE_DIR was then a lie for the module cache, and a system-wide
# core built by root would have sent every user's modules to /root/.pcl-cache
# (task #1303).  The two spellings are compared by Pl/t/module-fasl-cache-01.t.
sub cache_root {
    my $env = $ENV{PCL_CACHE_DIR};
    return _trim_slashes($env) if defined $env && length $env;
    my $home = $ENV{HOME};
    $home = eval { (getpwuid($<))[7] } if !defined $home || !length $home;
    $home = '.' if !defined $home || !length $home;
    return _trim_slashes($home) . '/.pcl-cache';
}

# Where cache_root's answer came from — 'env' or 'default'.  `pcl --cache-info`
# prints it, because "PCL did not notice my change" is usually a cache in a
# place the user did not expect.
sub cache_root_source {
    my $env = $ENV{PCL_CACHE_DIR};
    return (defined $env && length $env) ? 'env' : 'default';
}

# ------------------------------------------------------- cache root: safety
#
# A cached module is a FASL — compiled code PCL loads and RUNS — so the cache
# root is created 0700 and one anybody else could write to is REFUSED (task
# #1300, F8).  THE ROOT IS THE GATE and only the root is asked: with it at
# 0700 no other user can traverse into it, whatever the modes inside say.
# The runtime spells the same two questions in %p-cache-dir-problem
# (cl/pcl-runtime.lisp), and the limits of a mode check are written down once,
# in docs/ir-spec.md §9.2b.

# NIL when DIR is a safe place to keep compiled code, else the reason, NAMING
# the value (rule 12).  A directory that does not exist is not a problem — it
# is created 0700 below.
sub cache_dir_problem {
    my ($dir) = @_;
    my @st = stat $dir;
    return undef unless @st;
    return "it is owned by uid $st[4], not by you (uid $>)" if $st[4] != $>;
    return sprintf("it is group- or world-writable (mode %04o)", $st[2] & 07777)
        if $st[2] & 0022;
    return undef;
}

# The cache root, created 0700 if missing, refused LOUDLY if unsafe.  Every
# Perl-side writer of a cache file calls this first; the check itself is two
# stat(2) questions and is asked once per process.
my $CHECKED;
sub ensure_cache_root {
    my $root = cache_root();
    if (!-d $root) {
        require File::Path;
        eval { File::Path::make_path($root, { mode => 0700 }) };
    }
    return $root if $CHECKED;
    my $problem = cache_dir_problem($root);
    die "PCL: refusing to use the cache directory $root: $problem. "
      . "A cached module is compiled code, so PCL keeps its cache private. "
      . "Fix it with: chmod 700 $root -- or set PCL_CACHE_DIR to a directory "
      . "you own\n"
        if $problem;
    $CHECKED = 1;
    return $root;
}

# "/tmp/x/" -> "/tmp/x"; "/" -> "" (so "$root/core" is "/core", not "//core").
sub _trim_slashes {
    my ($p) = @_;
    $p =~ s{/+\z}{};
    return $p;
}

1;
