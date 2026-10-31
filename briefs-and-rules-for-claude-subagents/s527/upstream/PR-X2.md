# PR X -- §13

**The perl rule.** A standalone PPI::Tokenizer reads the same tokens whatever the caller's $/ is: the line end of a comment and of an __END__ line is taken with s/\n\z//, not chomp.

    perl -MPPI -e 'my $s = "# c\nx;\n"; local $/; print scalar @{ PPI::Tokenizer->new(\$s)->all_tokens }'

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §13.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter X.
