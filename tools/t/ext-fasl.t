#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# THE EXTENSION FASL CACHE (task #1202): p-load-extension loads cl/<name>.lisp
# through the module-fasl machinery, keyed by the extension file's own BYTES
# plus *pcl-runtime-identity*.
#
# What this file has to show is that a STALE fasl is impossible, not merely
# unlikely — so every row below runs a REAL SBCL against a REAL extension file
# in a scratch runtime directory and a scratch cache, and the rewrite row
# checks the ANSWER, not the plumbing.
#
# NOT part of the Pl/t gate: it spawns ~10 SBCLs (about 10 s) and it measures
# the harness, not the transpiler.  Run it directly, like tools/t/sbcl-prefix.t:
#     prove tools/t/ext-fasl.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);
use lib "$RealBin/../lib";
use PCLSbcl qw(sbcl_prefix cached_core);

# This file lives two levels down, so it names its own tree the way
# tools/t/install-pcl.t does — PCLPaths::root's derivation looks at the
# caller's directory and its parent, and tools/ is neither.
my $root    = "$RealBin/../..";
my $runtime = "$root/cl/pcl-runtime.lisp";
plan skip_all => "no runtime at $runtime" if !-f $runtime;

my $core = cached_core($runtime);
plan skip_all => "no SBCL to build a core with" if !defined $core || !length $core;
my @prefix = sbcl_prefix(core => $core, runtime => $runtime);

my $tmp = tempdir(CLEANUP => 1);
my $n   = 0;

# The scratch EXTENSION.  A file in a runtime directory of its own, so nothing
# here can touch the real cl/ tree, and one function whose answer the rows read.
sub write_ext {
    my ($dir, $body) = @_;
    make_path($dir);
    open my $fh, '>', "$dir/pcl-t1202.lisp" or die "write ext: $!";
    print $fh "(in-package :pcl)\n$body\n";
    close $fh;
    return "$dir/pcl-t1202.lisp";
}

# Run ONE fresh SBCL: load the extension out of EXT_DIR with CACHE as the whole
# per-user cache, and print what it answered.  Returns (output, exit status).
sub run_ext {
    my (%o) = @_;
    my $out = "$tmp/out." . ++$n;
    my @forms = (
        sprintf('(setf pcl::*pcl-runtime-directory* #p"%s/")', $o{ext_dir}),
        sprintf('(princ (if (pcl::p-load-extension "%s") "LOADED" "NOTFOUND"))',
                $o{name} // 'pcl-t1202'),
        '(princ (format nil " ANSWER=~A"'
        . ' (let ((s (find-symbol "%PCL-T1202-ANSWER" :pcl)))'
        . ' (if (and s (fboundp s)) (funcall s) "none"))))',
    );
    my $pid = fork();
    die "fork: $!" if !defined $pid;
    if (!$pid) {
        open STDOUT, '>', $out or die;
        open STDERR, '>&', \*STDOUT or die;
        $ENV{PCL_CACHE_DIR}  = $o{cache};
        $ENV{PCL_FASL_DEBUG} = 1;
        delete $ENV{PCL_NO_FASL_CACHE};
        $ENV{PCL_NO_FASL_CACHE} = 1 if $o{no_fasl};
        exec('sbcl', @prefix, map { ('--eval', $_) } @forms);
        exit 127;
    }
    waitpid($pid, 0);
    my $rc = $?;
    open my $fh, '<', $out or die "read out: $!";
    my $text = do { local $/; <$fh> };
    return ($text, $rc);
}

sub ext_fasls { return glob("\Q$_[0]\E/ext/*.fasl") }

# --- the ordinary life of an entry -----------------------------------------
my $cache = "$tmp/cache-a";
my $ext   = "$tmp/rt-a";
mkdir $cache or die $!;
chmod 0700, $cache;
write_ext($ext, '(defun %pcl-t1202-answer () 1)');

my ($o1) = run_ext(ext_dir => $ext, cache => $cache);
like($o1, qr/LOADED ANSWER=1/, 'cold: the extension loads and answers');
like($o1, qr/extension pcl-t1202 -> fasl-build/,
     'cold: no fasl yet, so the loader builds one');
my @f1 = ext_fasls($cache);
is(scalar(@f1), 1, 'cold: exactly one entry lands in <cache>/ext/');
like($f1[0], qr{/pcl-t1202-[0-9A-F]{16}-[0-9A-F]+\.fasl$},
     'the entry is named <name>-<content stem>-<runtime identity>.fasl');

my ($o2) = run_ext(ext_dir => $ext, cache => $cache);
like($o2, qr/LOADED ANSWER=1/, 'warm: same answer');
like($o2, qr/extension pcl-t1202 -> FASL HIT/, 'warm: the fasl is what loads');

# --- THE ROW THIS FILE EXISTS FOR: a rewritten artifact -------------------
# The key is the file's bytes, so the old entry is not stale — it is
# unreachable.  If this row ever answers 1, the cache is lying.
write_ext($ext, '(defun %pcl-t1202-answer () 2)');
my ($o3) = run_ext(ext_dir => $ext, cache => $cache);
like($o3, qr/LOADED ANSWER=2/, 'rewritten: the NEXT run answers 2, never the old fasl');
like($o3, qr/extension pcl-t1202 -> fasl-build/, 'rewritten: it rebuilds');
my @f2 = ext_fasls($cache);
is(scalar(@f2), 1, 'rewritten: the superseded entry is dropped, one is left');
isnt($f2[0], $f1[0], 'and it is a different entry');

my ($o4) = run_ext(ext_dir => $ext, cache => $cache);
like($o4, qr/ANSWER=2.*/s, 'rewritten: warm again');
like($o4, qr/extension pcl-t1202 -> FASL HIT/, 'rewritten: warm is a hit');

# --- a truncated fasl -------------------------------------------------------
# A crash or a full disk can leave one.  It must not be loaded, must not be
# fatal, and must not cost the entry: the file is unreadable, not a failed
# BUILD, so nothing refuses the rebuild (%p-note-fasl-unreadable).
# (`$f2[0]` is undef only where there is no cache at all — on a tree without
# this feature, where the rows below are meant to fail; name a scratch path
# there rather than letting the open pick one.)
my $victim = @f2 ? $f2[0] : "$tmp/no-entry.fasl";
open my $trunc, '>', $victim or die "truncate: $!";
print $trunc "not a fasl";
close $trunc;
my ($o5) = run_ext(ext_dir => $ext, cache => $cache);
like($o5, qr/LOADED ANSWER=2/, 'truncated fasl: the extension still loads, and is right');
unlike($o5, qr/FASL HIT/, 'truncated fasl: it is not loaded');
my ($o6) = run_ext(ext_dir => $ext, cache => $cache);
like($o6, qr/extension pcl-t1202 -> FASL HIT/,
     'truncated fasl: the entry is rebuilt, so the next run hits again');
ok(!glob("\Q$cache\E/ext/*.failed"),
   'truncated fasl: no .failed marker — that would refuse the rebuild for an hour');

# --- the off switch ---------------------------------------------------------
my ($o7) = run_ext(ext_dir => $ext, cache => $cache, no_fasl => 1);
like($o7, qr/LOADED ANSWER=2/, 'PCL_NO_FASL_CACHE=1: the extension still loads');
like($o7, qr/extension pcl-t1202 -> TEXT/, 'PCL_NO_FASL_CACHE=1: from its text');

# --- WHAT MUST STILL HOLD (task #349): an extension installs DEFINITIONS -----
# An artifact built WITHOUT `pl2cl --extension` carries a program preamble and
# would silently replace the running program's @INC.  %pcl-check-extension-clean
# reads the load state AFTER the load, so it has to catch that on the FASL path
# exactly as it always did on the text path.
my $cacheb = "$tmp/cache-b";
my $extb   = "$tmp/rt-b";
mkdir $cacheb or die $!;
chmod 0700, $cacheb;
write_ext($extb, "(defun %pcl-t1202-answer () 3)\n"
                 . '(vector-push-extend "/tmp/pcl-t1202-not-a-real-dir" @INC)');

my ($d1, $rc1) = run_ext(ext_dir => $extb, cache => $cacheb, no_fasl => 1);
like($d1, qr/changed the program's load state/,
     'text path: an @INC-touching extension still dies');
isnt($rc1, 0, 'text path: and the run fails');

my ($d2) = run_ext(ext_dir => $extb, cache => $cacheb);
like($d2, qr/changed the program's load state/,
     'fasl BUILD path: the same die, from the fasl the build just loaded');
my ($d3) = run_ext(ext_dir => $extb, cache => $cacheb);
like($d3, qr/extension pcl-t1202 -> FASL HIT/, 'fasl HIT path: the entry is reached');
like($d3, qr/changed the program's load state/,
     'fasl HIT path: and it dies there too — a fasl smuggles nothing past the check');

# --- the no-extension path pays nothing -------------------------------------
my $cachec = "$tmp/cache-c";
mkdir $cachec or die $!;
chmod 0700, $cachec;
my ($o8) = run_ext(ext_dir => "$tmp/rt-a", cache => $cachec, name => 'pcl-t1202-absent');
like($o8, qr/NOTFOUND/, 'a missing extension answers NIL, as it always did');
ok(!-d "$cachec/ext", 'and touches no cache at all: <cache>/ext/ is not even created');

done_testing();
