#!/usr/bin/perl
use strict;
use warnings;

# $n: the first list's size
# @list_int: a list containing ints
# $size: an other size
# @list_char: a list of char
# @list_string4: a list of strings of size 4
# @matrix: a matrix of int
sub lists {
    my ($n, $list_int, $size, $list_char, $list_string4, $matrix) = @_;
    # TODO Aren't these lists beautifull?
}

my $n = int <>;
my @list_int = map { int } split(/[ \n]/, <>);
my $size = int <>;
my @list_char = split /\n?/, <>;
my @list_string4 = ();
for (1..$size) {
    push(@list_string4, scalar(<>));
    chomp $list_string4[-1];
}
my @matrix = ();
for (1..$size) {
    push(@matrix, \@{[map { int } split(/[ \n]/, <>)]});
}

lists($n, \@list_int, $size, \@list_char, \@list_string4, \@matrix);
