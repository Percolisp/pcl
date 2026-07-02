#!/usr/bin/env perl
# Rough static survey: of all `my $scalar` declarations, how many are
# (a) disqualified from unboxing by LOCAL use (Gate 1), and
# (b) initialized from a sub call (where callee knowledge = Gate 2 upside)?
# Name-based heuristic, not scope-aware — indicative, not exact.
use strict; use warnings;

my (%tot);
for my $f (@ARGV) {
    open my $fh, '<', $f or next;
    local $/; my $src = <$fh>;
    # strip comments (crude) and POD to reduce false hits
    $src =~ s/^=\w+.*?^=cut//msg;
    $src =~ s/(?<![\$\\])#[^\n]*//g;

    my (%decl, %refd, %locald, %posd, %gmatch, %callinit, %litinit);
    # my-scalar declarations: my $x / my ($x, $y) — record init kind
    while ($src =~ /\bmy\s+\$(\w+)\s*(=\s*([^;\n]{1,80}))?/g) {
        my ($n, $init) = ($1, $3);
        $decl{$n}++;
        if (defined $init) {
            if    ($init =~ /^\s*(?:['"]|q[qw]?\b|-?\d|\[|\{|\\|undef\b)/) { $litinit{$n}++ }
            elsif ($init =~ /^\s*(?:\$|@|%)/)                              { }  # var copy
            elsif ($init =~ /^\s*(?:shift\b|pop\b)/)                       { }  # args
            elsif ($init =~ /\w+\s*\(|->\s*\w+/)                           { $callinit{$n}++ }
        }
    }
    while ($src =~ /\bmy\s*\(([^)]*)\)/g) {
        my $l = $1; $decl{$_}++ for $l =~ /\$(\w+)/g;
    }
    # Gate-1 disqualifiers (local):
    while ($src =~ /\\\s*\$(\w+)/g)          { $refd{$1}++ }   # \$x ref-taken
    while ($src =~ /\blocal\s+\$(\w+)/g)     { $locald{$1}++ }
    while ($src =~ /\bpos\s*\(\s*\$(\w+)/g)  { $posd{$1}++ }
    while ($src =~ /\$(\w+)\s*=~\s*(?:m?\/(?:[^\/\\]|\\.)*\/[a-z]*g|m?\{[^}]*\}[a-z]*g)/g) { $gmatch{$1}++ }
    my $has_eval_str = ($src =~ /\beval\s*["'\$]/) ? 1 : 0;

    my ($n_decl, $n_disq, $n_call) = (0,0,0);
    for my $n (keys %decl) {
        $n_decl += $decl{$n};
        $n_disq += $decl{$n} if $refd{$n} || $locald{$n} || $posd{$n} || $gmatch{$n};
        $n_call += $callinit{$n} // 0;
    }
    printf "%-40s my-scalars:%4d  gate1-disq:%3d (%2d%%)  call-sourced:%3d (%2d%%)  eval-str:%s\n",
        $f, $n_decl, $n_disq, $n_decl ? 100*$n_disq/$n_decl : 0,
        $n_call, $n_decl ? 100*$n_call/$n_decl : 0, $has_eval_str ? "YES" : "no";
    $tot{decl} += $n_decl; $tot{disq} += $n_disq; $tot{call} += $n_call;
    $tot{evalfiles}++ if $has_eval_str;
}
printf "\nTOTAL: %d my-scalar decls; %d (%.0f%%) Gate-1 disqualified locally; %d (%.0f%%) call-sourced (Gate-2 upside); %d files with string-eval\n",
    $tot{decl}, $tot{disq}, 100*$tot{disq}/$tot{decl}, $tot{call}, 100*$tot{call}/$tot{decl}, $tot{evalfiles}//0;
