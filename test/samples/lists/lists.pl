#!/usr/bin/perl
use strict;
use warnings;

# $n: the first list's size
# @list_int: a list containing ints
# $size: an other size
# @list_char: a list of char
# $string: a string
# @list_string4: a list of strings of size 4
# @list_list_string2: a list of list of strings of size 2 of size 2 of size 2
# @matrix: a matrix of int
sub lists {
    my ($n, $list_int, $size, $list_char, $string, $list_string4, $list_list_string2, $matrix) = @_;
    # TODO Aren't these lists beautifull?
}

my $n = int <>;
my @list_int = map { int } split(/[ \n]/, <>);
my $size = int <>;
my @list_char = split /\n?/, <>;
my $string = <>;
chomp $string;
my @list_string4 = ();
for (1..$size) {
    push(@list_string4, scalar(<>));
    chomp $list_string4[-1];
}
my @list_list_string2 = ();
for (1..2) {
    push(@list_list_string2, []);
    for (1..2) {
        push(@{$list_list_string2[-1]}, scalar(<>));
        chomp $list_list_string2[-1][-1];
    }
}
my @matrix = ();
for (1..$size) {
    push(@matrix, \@{[map { int } split(/[ \n]/, <>)]});
}

lists($n, \@list_int, $size, \@list_char, $string, \@list_string4, \@list_list_string2, \@matrix);
