#!/usr/bin/perl
use strict;
use warnings;

# @empty_list: an empty list
# $buffer_string: here to check correct parsing of empty line above
# $n: an integer, will be 0 in the sample input
# @empty_in_sample: an empty list (only in the sample)
# $empty_string: an empty string
# $main: an other buffer string
# @empty_char_list: an empty char list
# @non_empty_char_list: an char list, non empty
# %struct_with_empty_line: a struct containing an empty line, then a struct
# %a_sized_struct: a sized struct containing an empty line
# $finish: a string to finish
sub empty_lines {
    my ($empty_list, $buffer_string, $n, $empty_in_sample, $empty_string, $main, $empty_char_list, $non_empty_char_list, $struct_with_empty_line, $a_sized_struct, $finish) = @_;
    # TODO Wow, lots of empty lines!
}

my @empty_list = map { int } split(/[ \n]/, <>);
my $buffer_string = <>;
chomp $buffer_string;
my $n = int <>;
my @empty_in_sample = map { int } split(/[ \n]/, <>);
my $empty_string = <>;
chomp $empty_string;
my $main = <>;
chomp $main;
my @empty_char_list = split /\n?/, <>;
my @non_empty_char_list = split /\n?/, <>;
my %struct_with_empty_line = ();
$struct_with_empty_line{'list in struct'} = \@{[map { int } split(/[ \n]/, <>)]};
my @words = split /[ \n]/, <>;
$struct_with_empty_line{'struct in struct'} = \%{{("char1" => substr($words[0], 0, 1), "int2" => int($words[1]))}};
my %a_sized_struct = ();
$a_sized_struct{'size'} = int <>;
$a_sized_struct{'string in struct'} = scalar(<>);
chomp $a_sized_struct{'string in struct'};
my $finish = <>;
chomp $finish;

empty_lines(\@empty_list, $buffer_string, $n, \@empty_in_sample, $empty_string, $main, \@empty_char_list, \@non_empty_char_list, \%struct_with_empty_line, \%a_sized_struct, $finish);
