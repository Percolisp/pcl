# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 145-local -- harvested from s470/s471a-agent-ab8307152aef20823/probes/T.pl

sub NewShell
{
  local($Host) = @_;
  my($m2) = $#Shells++;
  $Shells[$m2]{HOST} = $Host;
  return $m2;
}
sub ShowShell { local($i) = @_; }
&ShowShell(&NewShell("beach","Work","+0+0"));
&ShowShell(&NewShell("beach","Work","+0+0"));
&ShowShell(&NewShell("beach","Work","+0+0"));
print "n=", scalar(@Shells), " h0=$Shells[0]{HOST} h2=$Shells[2]{HOST}\n";
