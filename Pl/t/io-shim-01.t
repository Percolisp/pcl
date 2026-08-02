#!/usr/bin/env perl
# io-shim-01.t — the IO/IO::Handle shim (task #197) and the core-semantics
# gaps it uncovered: exit/fork flushing, real select(), per-handle $|,
# open() on a glob handle, `use Foo ()`, `require Foo if COND`, and the
# symbolic-designator exists/delete family.
#
# Every row here is a case that ran SILENTLY WRONG or crashed before s326,
# so each one carries its inverse where the inverse is the interesting half
# (e.g. `use Foo;` must still import; $| off must still buffer).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 20;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

my $dir = tempdir(CLEANUP => 1);

# ── exists/delete through a symbolic (string) designator ──────────────────
# Read and write already resolved these; exists answered NO and delete
# crashed SBCL's GETHASH on the string.

test_cl('exists $$name{k} resolves the symbolic hash',
        qq{no strict 'refs';\n}
      . qq{%Foo::h = (a => 1);\n}
      . qq{my \$n = "Foo::h";\n}
      . qq{print exists \$\$n{a} ? "yes" : "no", "\\n";\n},
        "yes\n");

test_cl('delete $$name{k} returns the value and removes it',
        qq{no strict 'refs';\n}
      . qq{%Foo::h = (a => 1, b => 2);\n}
      . qq{my \$n = "Foo::h";\n}
      . qq{my \$v = delete \$\$n{b};\n}
      . qq{print "\$v-", (exists \$Foo::h{b} ? "still" : "gone"), "\\n";\n},
        "2-gone\n");

test_cl('exists $$name[i] resolves the symbolic array',
        qq{no strict 'refs';\n}
      . qq{\@Foo::l = (10, 20, 30);\n}
      . qq{my \$n = "Foo::l";\n}
      . qq{print exists \$\$n[2] ? "yes" : "no", "\\n";\n},
        "yes\n");

test_cl('"Pkg::" is a stash, not a package hash named ""',
        qq{no strict 'refs';\n}
      . qq{sub Foo::hi { 1 }\n}
      . qq{my \$p = "Foo::";\n}
      . qq{print exists \$\$p{hi} ? "yes" : "no", "\\n";\n},
        "yes\n");

# ── delete on an array element through a REF (arity crash before s326) ────
test_cl('delete $ref->[i] and delete $$ref[i] delete the element',
        qq{my \$r = [1,2,3];\n}
      . qq{my \$a = delete \$r->[2];\n}
      . qq{my \$b = delete \$\$r[1];\n}
      . qq{print "\$a-\$b-", scalar(\@\$r), "\\n";\n},
        "3-2-1\n");

# ── every handle is flushed at exit, not just the standard three ──────────
{
    my $f = "$dir/exitflush.txt";
    run_cl(qq{open(my \$g, ">", "$f") or die;\nprint {\$g} "no close\\n";\n});
    open my $in, '<', $f or die "opening $f: $!";
    is(scalar <$in>, "no close\n",
       'an unclosed output handle is flushed at exit');
    close $in;
}

# ── select() is real: it switches the default handle, and back ────────────
{
    my $f = "$dir/selected.txt";
    my $out = run_cl(qq{open(my \$g, ">", "$f") or die;\n}
                   . qq{my \$old = select(\$g);\n}
                   . qq{print "to the file\\n";\n}
                   . qq{select(\$old);\n}
                   . qq{print "to stdout\\n";\n}
                   . qq{close(\$g);\n});
    is($out, "to stdout\n", 'select(FH) redirects the default print target');
    open my $in, '<', $f or die "opening $f: $!";
    is(scalar <$in>, "to the file\n",
       'select() returns the previous handle, so select($old) restores it');
    close $in;
}

# ── $| is per-handle: on flushes now, off keeps buffering ─────────────────
{
    my $f = "$dir/pipe-on.txt";
    run_cl(qq{open(my \$g, ">", "$f") or die;\n}
         . qq{print {\$g} "buffered\\n";\n}
         . qq{my \$old = select(\$g); \$| = 1; select(\$old);\n}
         . qq{open(my \$r, "<", "$f") or die;\n}
         . qq{print "seen=", scalar(<\$r>);\n});
    open my $in, '<', $f or die "opening $f: $!";
    is(scalar <$in>, "buffered\n", 'setting $| flushes the selected handle');
    close $in;
}
{
    # INVERSE: with $| left alone the write must still be buffered, i.e. a
    # reader inside the same program sees nothing yet.
    my $f = "$dir/pipe-off.txt";
    my $out = run_cl(qq{open(my \$g, ">", "$f") or die;\n}
                   . qq{print {\$g} "buffered\\n";\n}
                   . qq{open(my \$r, "<", "$f") or die;\n}
                   . qq{my \$l = <\$r>;\n}
                   . qq{print "seen=", (defined \$l ? \$l : "nothing"), "\\n";\n});
    is($out, "seen=nothing\n", 'without $| the write stays buffered');
}

# ── open() on a glob handle keeps the scalar (and its class) ──────────────
test_cl('open on a blessed globref does not overwrite the object',
        qq{no strict 'refs';\n}
      . qq{my \$io = bless \\*{"My::G1"}, 'IO::Handle';\n}
      . qq{open(\$io, ">", "$dir/globref.txt") or die;\n}
      . qq{print {\$io} "x\\n";\n}
      . qq{close(\$io);\n}
      . qq{print ref(\$io), "\\n";\n},
        "IO::Handle\n");

# ── use Foo () / qw() must not call import; bare use still must ───────────
{
    my $lib = "$dir/lib";
    mkdir $lib;
    open my $m, '>', "$lib/TImp.pm" or die $!;
    print $m "package TImp;\nsub import { print \"IMPORTED\\n\" }\n1;\n";
    close $m;
    is(run_cl(qq{use lib "$lib";\nuse TImp ();\nprint "done\\n";\n}),
       "done\n", 'use Foo () loads without calling import');
    is(run_cl(qq{use lib "$lib";\nuse TImp;\nprint "done\\n";\n}),
       "IMPORTED\ndone\n", 'INVERSE: bare use Foo still calls import');
}

# ── s327 review findings ──────────────────────────────────────────────────
# close() on a symbolic (name-string) handle must mirror open(): the by-name
# entry goes away, the SCALAR keeps its string ($fh still reads "FOO").
test_cl('close on a name-string handle keeps the string in the scalar',
        qq{no strict;\n}
      . qq{my \$fh = 'FOO';\n}
      . qq{open(\$fh, ">", "$dir/namestr.txt") or die;\n}
      . qq{print \$fh "x\\n";\n}
      . qq{close(\$fh);\n}
      . qq{print "fh=[", (defined \$fh ? \$fh : "undef"), "]\\n";\n},
        "fh=[FOO]\n");

# `use Foo VERSION ...`: PPI's \$stmt->version is empty for MODULE versions,
# so the version token must be recognized positionally — and only when no
# operator follows (`use Foo 1.5, 'x'` makes the number a list element).
{
    my $lib = "$dir/lib2";
    mkdir $lib;
    open my $m, '>', "$lib/TVer.pm" or die $!;
    print $m "package TVer;\nour \$VERSION = 9;\n"
           . "sub import { shift; print \@_ ? \"ARGS(\@_)\\n\" : \"NOARGS\\n\" }\n1;\n";
    close $m;
    is(run_cl(qq{use lib "$lib";\nuse TVer 1.0 ();\nprint "done\\n";\n}),
       "done\n", 'use Foo VERSION () still skips import');
    is(run_cl(qq{use lib "$lib";\nuse TVer 1.0 qw(a b);\nprint "done\\n";\n}),
       "ARGS(a b)\ndone\n",
       'use Foo VERSION qw(...) imports the LIST, version dropped');
    is(run_cl(qq{use lib "$lib";\nuse TVer 1.5, "x";\nprint "done\\n";\n}),
       "ARGS(1.5 x)\ndone\n",
       'INVERSE: a comma after the number makes it a plain list element');
}

# ── a statement modifier gates require ────────────────────────────────────
test_cl('require Foo if COND does not run when COND is false',
        qq{require No::Such::Module if 0;\nprint "survived\\n";\n},
        "survived\n");

test_cl('INVERSE: require Foo if COND does run when COND is true',
        qq{require Carp if 1;\n}
      . qq{print defined(&Carp::croak) ? "loaded" : "missing", "\\n";\n},
        "loaded\n");

# ── the shim itself: IO::Handle->new, ->flush, ->autoflush ────────────────
{
    my $f = "$dir/iohandle.txt";
    my $out = run_cl(qq{use IO::Handle ();\n}
                   . qq{my \$fh = IO::Handle->new;\n}
                   . qq{open(\$fh, ">", "$f") or die;\n}
                   . qq{print {\$fh} "one\\n";\n}
                   . qq{\$fh->flush;\n}
                   . qq{open(my \$r, "<", "$f") or die;\n}
                   . qq{print "after-flush=", scalar(<\$r>);\n}
                   . qq{\$fh->autoflush(1);\n}
                   . qq{print {\$fh} "two\\n";\n}
                   . qq{open(my \$r2, "<", "$f") or die;\n}
                   . qq{my \@all = <\$r2>;\n}
                   . qq{print "lines=", scalar(\@all), "\\n";\n});
    is($out, "after-flush=one\nlines=2\n",
       'IO::Handle->new / ->flush / ->autoflush work, and autoflush RESTORES '
     . 'the selection (SelectSaver needs DESTROY, which PCL lacks)');
}
