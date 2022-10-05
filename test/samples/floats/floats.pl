#!/usr/bin/perl
use strict;
use warnings;

# $f: a float
# $g: a float, greater than f
# %point: some coordinates
# $n: a number
# @float_list: a list of floats
# @other_list: a list of floats
# @inlined: some inlined structs
# %multiline: a multiline struct
sub floats {
    my ($f, $g, $point, $n, $float_list, $other_list, $inlined, $multiline) = @_;
    # TODO Parsing is often easy, reprint mode is harder
}

my $f = <> + 0;
my $g = <> + 0;
my @words = split /[ \n]/, <>;
my %point = ("x" => $words[0], "y" => $words[1], "z" => $words[2]);
my $n = int <>;
my @float_list = split(/[ \n]/, <>);
my @other_list = split(/[ \n]/, <>);
my @inlined = ();
for (1..3) {
    my @words1 = split /[ \n]/, <>;
    push(@inlined, \%{{("integer" => int($words1[0]), "char" => substr($words1[1], 0, 1), "float" => $words1[2])}});
}
my %multiline = ();
$multiline{'integer 2'} = int <>;
$multiline{'string'} = scalar(<>);
chomp $multiline{'string'};
$multiline{'float 2'} = <> + 0;

floats($f, $g, \%point, $n, \@float_list, \@other_list, \@inlined, \%multiline);
