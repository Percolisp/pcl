# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-22-exporter-tags-lib.pl
use strict; use warnings;
BEGIN { my $d = "/tmp/pcl-b2-exp-$$"; mkdir $d; mkdir "$d/My"; open(my $o, ">", "$d/My/Util.pm") or die;
  print $o 'package My::Util; use strict; use warnings; use parent "Exporter"; our @EXPORT = qw(dflt); our @EXPORT_OK = qw(add mul $Scale @List %Map); our %EXPORT_TAGS = (math => [qw(add mul)], all => [@EXPORT, @EXPORT_OK]); our $Scale = 10; our @List = (1,2); our %Map = (k => "v"); sub dflt { "dflt" } sub add { $_[0] + $_[1] } sub mul { $_[0] * $_[1] * $Scale } 1;', "\n"; close $o;
  unshift @INC, $d; $main::tmpd = $d; }
use My::Util qw(:math dflt $Scale @List %Map);
print add(1, 2), " ", mul(2, 3), " ", dflt(), " $Scale @List $Map{k} ", (main->can("add") ? "imported" : "not"), " ", (eval { My::Util->import("nosuch"); 1 } ? "no-error" : "export-error"), "\n";
$Scale = 2; print mul(1, 1), " ", My::Util::mul(1, 2), " ", My::Util->can("import") ? "has-import" : "no-import", "\n";
END { unlink "$main::tmpd/My/Util.pm"; rmdir "$main::tmpd/My"; rmdir $main::tmpd }
