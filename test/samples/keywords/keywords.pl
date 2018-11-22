#!/usr/bin/perl
use strict;
use warnings;

# $if: not a condition
# $class: not a class
# $i: just a string
# %in: not in
# @for: not a loop
# @words: contains lots of things
sub keywords {
    my ($if, $class, $i, $in, $for, $words) = @_;
    # TODO If this compiles, it is already a good step!
}

my $if = int <>;
my $class = substr <>, 0, 1;
my $i = <>;
chomp $i;
my @words1 = split / /, <>;
my %in = ("a" => int($words1[0]), "static" => int($words1[1]));
my @for = map { int } split(/ /, <>);
my @words = ();
for (1..2) {
    push(@words, {});
    $words[-1]{'int'} = {};
    $words[-1]{'int'}{'return'} = int <>;
    $words[-1]{'int'}{'void'} = \@{[map { int } split(/ /, <>)]};
    $words[-1]{'if true'} = int <>;
}

keywords($if, $class, $i, \%in, \@for, \@words);
