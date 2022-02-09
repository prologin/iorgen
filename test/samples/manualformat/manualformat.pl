#!/usr/bin/perl
use strict;
use warnings;

# $a_: a first number
# $b_: a second number
# $c: a third number
# $n: This one on a new line
# @one_per_line: an integer list, one per line
sub manual_format {
    my ($a_, $b_, $c, $n, $one_per_line) = @_;
    # TODO From the function perspective, this is just 4 integers
}

my @words = split / /, <>;
my $a_ = int($words[0]);
my $b_ = int($words[1]);
my $c = int($words[2]);
my $n = int <>;
my @one_per_line = ();
for (1..3) {
    push(@one_per_line, int <>);
}

manual_format($a_, $b_, $c, $n, \@one_per_line);
