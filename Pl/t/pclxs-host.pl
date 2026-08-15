# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pclxs-host.pl — register PCL as a host in pclxs's conformance suite.
#
# The pclxs repo defines "a host is done" as "the conformance suite passes
# against it", and its suite may not name any runtime (that repo's one
# rule).  So the host registers itself, from here:
#
#     PCLXS_HOST_DEFS=$PWD/Pl/t/pclxs-host.pl \
#       prove -I t/lib -r t/            # run from the pclxs checkout
#
# or, more conveniently, tools/pcl-conform.
#
# What this file is: three closures, exactly the shape refhost's entry has.
# `build` turns a case's .xs into a loadable object with pclxs's own
# xs-build; `run` drives it through SBCL and prints one tagged line per
# returned value, in refhost's protocol, so the parsing is inherited rather
# than reimplemented (Pclxs::Conform::collect_tagged).
#
# It is loaded INSIDE the pclxs test suite, which is why it can use that
# suite's helpers.  Nothing in pclxs knows this file exists.

use strict;
use warnings;
use File::Temp qw(tempdir);
use File::Basename qw(dirname);
use Cwd qw(abs_path);
use Pclxs::Test qw(write_dist run_xs_build);
use Pclxs::Conform qw(collect_tagged);

# This file lives in <pcl>/Pl/t/, and knows only its own repository.
my $PCL_ROOT = abs_path(dirname(abs_path(__FILE__)) . '/../..');

sub _pcl_available {
    return 0 if ! -f "$PCL_ROOT/cl/pcl-runtime.lisp";
    return 0 if ! -f "$PCL_ROOT/cl/pcl-xs.lisp";
    return 0 if system('command -v sbcl >/dev/null 2>&1') != 0;
    # libpclxs is the pclxs side's business, but without it there is nothing
    # to load, so skipping is better than a mystery failure.
    my $pclxs = $ENV{PCLXS_DIR} || abs_path("$PCL_ROOT/../pclxs");
    return 0 if ! -f "$pclxs/build/libpclxs.so";
    return 1;
}

# A Perl scalar as a Lisp literal.  Byte strings go through char codes
# rather than string escaping: a case's arguments deliberately include NULs,
# high bytes and quotes, and code-char is the only spelling that survives
# all of them unambiguously.
sub _lisp_arg {
    my ($v) = @_;
    return '*p-undef*' if ! defined $v;
    if (ref $v eq 'SCALAR') {                 # \42 / \1.5: force a number
        return $$v =~ /^-?\d+$/ ? "$$v" : sprintf('%sd0', $$v);
    }
    return '""' if $v eq '';
    return '(coerce (list ' . join(' ', map { '(code-char ' . ord($_) . ')' }
                                        split //, $v) . ") 'string)";
}

sub _drive_lisp {
    my ($so, $module, $calls) = @_;
    my $pclxs = $ENV{PCLXS_DIR} || abs_path("$PCL_ROOT/../pclxs");

    my $l = <<"HEAD";
(require :sb-posix)
(load "$PCL_ROOT/cl/pcl-runtime.lisp")
(in-package :pcl)
(load "$PCL_ROOT/cl/pcl-xs.lisp")
(p-xs-boot "$so" "boot_$module")

;; refhost's result protocol, so pclxs parses both hosts with one reader.
(defun conf-emit-val (v)
  (let ((u (unbox v)))
    (if (or (null u) (eq u *p-undef*))
        (format t "undef~%")
        (format t "pv:~A~%" (to-string u)))))

(defun conf-emit (thunk)
  (handler-case
      (let ((r (funcall thunk)))
        (cond ((and (vectorp r) (not (stringp r)))
               (format t "ok ~D~%" (length r))
               (loop for x across r do (conf-emit-val x)))
              ((null r) (format t "ok 0~%"))
              (t (format t "ok 1~%") (conf-emit-val r))))
    (error (e)
      ;; perl's leg strips " at FILE line N." before comparing; do the same
      ;; here so a croak reads identically on both sides.
      (let* ((m (princ-to-string e))
             (p (search " at " m)))
        (format t "died ~A~%" (string-right-trim '(#\\Newline)
                                                 (if p (subseq m 0 p) m)))))))

(let ((pkg (%pcl-find-package "$module")))
HEAD

    for my $call (@$calls) {
        my $args  = join ' ', map { _lisp_arg($_) } @{ $call->{args} || [] };
        my $want  = ($call->{gimme} || 'scalar') eq 'list' ? 't' : 'nil';
        my $fn    = $call->{sub};
        $l .= qq{  (let ((f (find-symbol (%pcl-cl-sub-name "$fn") pkg)))\n}
            . qq{    (if (and f (fboundp f))\n}
            . qq{        (conf-emit (lambda () (let ((*wantarray* $want))\n}
            . qq{                                (funcall f $args))))\n}
            . qq{        (format t "died no such sub: ${module}::${fn}~%")))\n};
    }
    $l .= ")\n";
    return $l;
}

{
    'pcl' => {
        available => \&_pcl_available,

        build => sub {
            my (%c) = @_;
            my $dist = write_dist(name => $c{module}, xs => $c{xs});
            my ($so, $log) = run_xs_build($dist);
            die "xs-build failed:\n$log" if ! $so;
            return $so;
        },

        run => sub {
            my ($so, %c) = @_;
            my $dir = tempdir(CLEANUP => 1);
            my $script = "$dir/drive.lisp";
            open my $fh, '>', $script or die "$script: $!";
            print {$fh} _drive_lisp($so, $c{module}, $c{calls});
            close $fh;

            my $out = qx{sbcl --script $script 2>&1};

            # WITH-XS-GUARD must never let a condition reach C -- which
            # means it also turns a bug in a callback into a plausible
            # DEFAULT.  That is how a call to a misspelled, undefined
            # `p-looks-like-number` masqueraded as "this is not a number"
            # for every value in the program.  It does say so on stderr,
            # so: reading that line is not optional.
            if ($out =~ /^(pclxs: host callback error: .*)$/m) {
                die "PCL host callback failed -- this is a REAL bug, not a "
                  . "conformance divergence:\n  $1\n";
            }

            # SBCL's own noise (compile notes, warnings) is not a result
            # line; keep only what the protocol defines.
            my @out = grep { /^(?:ok \d+|died |pv:|iv:|nv:|undef$)/ }
                      split /\n/, $out;
            return collect_tagged(\@out, $c{calls});
        },
    },
}
