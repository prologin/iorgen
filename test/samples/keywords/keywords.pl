#!/usr/bin/perl
use strict;
use warnings;

# $if: not a condition
# $class: not a class
# $i: just a string
# %in: not in
# @for: not a loop
# @words: contains lots of things
# $words_1: an integer
sub keywords {
    my ($if, $class, $i, $in, $for, $words, $words_1) = @_;
    # TODO If this compiles, it is already a good step!
}

my $if = int <>;
my $class = substr <>, 0, 1;
my $i = <>;
chomp $i;
my @words2 = split /[ \n]/, <>;
my %in = ("a" => int($words2[0]), "static" => int($words2[1]));
my @for = map { int } split(/[ \n]/, <>);
my @words = ();
for (1..2) {
    push(@words, {});
    $words[-1]{'int'} = {};
    $words[-1]{'int'}{'return'} = int <>;
    $words[-1]{'int'}{'void'} = \@{[map { int } split(/[ \n]/, <>)]};
    $words[-1]{'if true'} = int <>;
}
my $words_1 = int <>;

keywords($if, $class, $i, \%in, \@for, \@words, $words_1);
