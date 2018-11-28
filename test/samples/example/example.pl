#!/usr/bin/perl
use strict;
use warnings;

# $n: a number, used as a size
# @list: a list of structs
sub example {
    my ($n, $list) = @_;
    # TODO In a real life scenario, you will describe here what you want the
    # end user to do with this generated code
}

my $n = int <>;
my @list = ();
for (1..$n) {
    my @words = split /[ \n]/, <>;
    push(@list, \%{{("integer" => int($words[0]), "character" => substr($words[1], 0, 1))}});
}

example($n, \@list);
