my @subs;
for my $n (0..4) {
    push @subs, sub { $n };
}
print $subs[0]->(), "\n";
print $subs[1]->(), "\n";
print $subs[4]->(), "\n";
