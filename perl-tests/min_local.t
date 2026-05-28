#!./perl
require './test.pl';
plan(10);
our @bee = qw(foo bar burbl blah);
{
    local @bee = @bee;
    is("@bee", "foo bar burbl blah", "43-local-self-copy");
    {
        local (undef,@bee) = @bee;
        is("@bee", "bar burbl blah", "44-local-list-undef");
        {
            local @bee = ('XXX',@bee,'YYY');
            is("@bee", "XXX bar burbl blah YYY", "45-local-flanking");
            {
                local @bee = local(@bee) = qw(foo bar burbl blah);
                is("@bee", "foo bar burbl blah", "46-local-local-self");
                {
                    local (@bim) = local(@bee) = qw(foo bar);
                    is("@bee", "foo bar", "47-bee-after-diffvar");
                    is("@bim", "foo bar", "48-bim-after-diffvar");
                }
                is("@bee", "foo bar burbl blah", "49-after-diffvar-exits");
            }
            is("@bee", "XXX bar burbl blah YYY", "50-after-46-exits");
        }
        is("@bee", "bar burbl blah", "51-after-45-exits");
    }
    is("@bee", "foo bar burbl blah", "52-after-44-exits");
}
