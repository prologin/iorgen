#!/usr/bin/perl
use strict;
use warnings;

# %struct: a struct 1 instance
# $n: a number
# @struct_list: a list a struct 1
# @triangle: a triangle
# %struct_chars: a struct of chars
sub structs {
    my ($struct, $n, $struct_list, $triangle, $struct_chars) = @_;
    # TODO Look at them structs.
}

my @words = split /[ \n]/, <>;
my %struct = ("foo" => int($words[0]), "bar" => int($words[1]));
my $n = int <>;
my @struct_list = ();
for (1..$n) {
    my @words1 = split /[ \n]/, <>;
    push(@struct_list, \%{{("foo" => int($words1[0]), "bar" => int($words1[1]))}});
}
my @triangle = ();
for (1..3) {
    push(@triangle, {});
    $triangle[-1]{'name'} = substr <>, 0, 1;
    $triangle[-1]{'description'} = scalar(<>);
    chomp $triangle[-1]{'description'};
    my @words1 = split /[ \n]/, <>;
    $triangle[-1]{'pos'} = \%{{("x" => int($words1[0]), "y" => int($words1[1]), "z" => int($words1[2]))}};
}
my @words1 = split /[ \n]/, <>;
my %struct_chars = ("first char" => substr($words1[0], 0, 1), "second char" => substr($words1[1], 0, 1), "third char" => substr($words1[2], 0, 1));

structs(\%struct, $n, \@struct_list, \@triangle, \%struct_chars);
