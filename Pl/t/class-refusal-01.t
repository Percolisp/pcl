#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# class-refusal-01.t — task #399: the STATEMENT form `class NAME ;` of perl
# 5.38's object syntax (RULED s416, docs/fable-answers-s415.md §7.5).
#
# This is a refusal on code that COMPILES, which is what makes it different
# from the Track A drop-site refusals and why its key is stricter.
#
# `class Foo;` parses as the indirect-object call `Foo->class` — in PPI, in
# PCL, and in PERL ITSELF when the feature is off (probed: perl dies "Can't
# locate object method \"class\" via package \"Foo\"").  So the default reading
# is RIGHT and must not change; refusing every `class NAME;` would break files
# that work today.  The refusal fires only when the file switched the feature
# on EXPLICITLY.
#
# A VERSION BUNDLE IS NEVER EVIDENCE HERE: `class` is experimental and in no
# bundle, and `use v5.38; class Foo;` is a perl SYNTAX ERROR (probed s417).
# The bundle stays acceptable at the drop sites, where the statement is already
# lost — hence one scanner with a $strict flag, not two.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";

plan skip_all => "pl2cl not found" unless -x $pl2cl;

plan tests => 11;

# Transpile SRC and return ($stdout, $stderr, $rc) — transpile_raw judges
# nothing, which is what a row asserting a REFUSAL needs.
sub attempt {
    my ($src) = @_;
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return PCLCore::transpile_raw("$pl2cl $pl");
}

sub refuses {
    my ($name, $src) = @_;
    my (undef, $err, $rc) = attempt($src);
    like($err, qr/PCL: feature 'class' is not supported/,
         "$name — refuses, perl-shaped")
      or diag("rc=$rc stderr=$err");
}

sub compiles_as_before {
    my ($name, $src, $want_re) = @_;
    my ($cl, $err, undef) = attempt($src);
    if ($err =~ /feature 'class'/) {
        fail("$name — must NOT refuse");
        diag($err);
        return;
    }
    like($cl, $want_re, $name);
}

# ── FIRES: the file says the feature is on ──────────────────────────────────
refuses('use feature "class"',
        qq{use feature 'class';\nclass Foo;\nprint "after\\n";\n});

refuses('use experimental "class"',
        qq{use experimental 'class';\nclass Foo;\nprint "after\\n";\n});

# A `class NAME { … }` BLOCK elsewhere in the file is in-file evidence.  The
# statement form is NOT its own evidence — that would be circular — so this
# row also pins that the block is what was found.
refuses('a class NAME { } block elsewhere in the file',
        qq{class Foo { }\nclass Bar;\nprint "after\\n";\n});

# The refusal is per FILE, so it fires wherever the statement sits.
refuses('inside a sub body',
        qq{use feature 'class';\nsub f { class Foo; }\nprint "after\\n";\n});

# ── MUST NOT FIRE: every one of these compiles today and must keep doing so ──
compiles_as_before('bare `class Foo;` with no pragma stays Foo->class',
        qq{class Foo;\nprint "after\\n";\n},
        qr/p-method-call.*"class"/s);

# `use v5.38` alone is a perl SYNTAX ERROR for this code, so a bundle can never
# be evidence about code that compiles.
compiles_as_before('use v5.38 is NOT evidence',
        qq{use v5.38;\nclass Foo;\nprint "after\\n";\n},
        qr/p-method-call.*"class"/s);

compiles_as_before('a file with its own `sub class`',
        qq{sub class { return 7 }\nclass Foo;\nprint "after\\n";\n},
        qr/pl-class/);

compiles_as_before('`Foo->class` written directly',
        qq{my \$o = Foo->class;\nprint "m\\n";\n},
        qr/p-method-call.*"class"/s);

compiles_as_before('`class` as a hash key',
        qq{my %h = (class => 1);\nprint \$h{class}, "\\n";\n},
        qr/class/);

compiles_as_before('`class` as a method NAME on an object',
        qq{my \$x = bless {}, 'C';\nmy \$r = \$x->class;\n},
        qr/p-method-call.*"class"/s);

# The BLOCK form is a different statement and is not what this refusal is keyed
# on — it must still reach the drop-site machinery, not this die.  (It is not
# supported either; the point of the row is that the message is not THIS one
# raised from the statement path, so the block form's own handling is free to
# change without touching this guard.)
{
    my (undef, $err, undef) =
        attempt(qq{use feature 'class';\nclass Foo { }\nprint "after\\n";\n});
    like($err, qr/class/,
         'the BLOCK form still says something about class');
}
