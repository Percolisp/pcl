# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-10-require-do-file.pl
use strict; use warnings;
my $d = "/tmp/pcl-b2-req-$$"; mkdir $d or die;
open(my $o, ">", "$d/MyMod.pm") or die; print $o "package MyMod; use strict; our \$VERSION = '1.02'; use Exporter 'import'; our \@EXPORT_OK = qw(twice \$shared); our %EXPORT_TAGS = (all => \\\@EXPORT_OK); our \$shared = 'sh'; sub twice { 2 * shift } 1;\n"; close $o;
open($o, ">", "$d/conf.pl") or die; print $o "{ name => 'cfg', list => [1,2,3] };\n"; close $o;
unshift @INC, $d; require MyMod; MyMod->import(qw(:all));
my $conf = do "$d/conf.pl"; die "do failed: $@ $!" if !$conf;
print MyMod::twice(21), " ", MyMod->VERSION, " $conf->{name} @{$conf->{list}} ", (MyMod->can("twice") ? "can" : "cannot"), " ", ($INC{"MyMod.pm"} ? "inc" : "noinc"), "\n";
print((eval { require No::Such::Module; 1 } ? "loaded" : "missing: " . ($@ =~ /^Can't locate No\/Such\/Module.pm/ ? "msg-ok" : $@)), "\n");
unlink "$d/MyMod.pm", "$d/conf.pl"; rmdir $d;
