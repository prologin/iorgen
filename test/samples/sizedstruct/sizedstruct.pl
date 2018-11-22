#!/usr/bin/perl
use strict;
use warnings;

# $n: the size of the lists
# @lists: a list of list of different sizes
# @strings: a list of strings of different sizes
# @matrices: a list of matrices of different sizes
# @same: a list of list of same sizes
sub sized_struct {
    my ($n, $lists, $strings, $matrices, $same) = @_;
    # TODO The is a special case.
}

my $n = int <>;
my @lists = ();
for (1..$n) {
    push(@lists, {});
    $lists[-1]{'size1'} = int <>;
    $lists[-1]{'int list'} = \@{[map { int } split(/ /, <>)]};
}
my @strings = ();
for (1..$n) {
    push(@strings, {});
    $strings[-1]{'size2'} = int <>;
    $strings[-1]{'string list'} = scalar(<>);
    chomp $strings[-1]{'string list'};
}
my @matrices = ();
for (1..2) {
    push(@matrices, {});
    $matrices[-1]{'size3'} = int <>;
    $matrices[-1]{'list list'} = ();
    for (1..$matrices[-1]{'size3'}) {
        push(@{$matrices[-1]{'list list'}}, \@{[map { int } split(/ /, <>)]});
    }
}
my @same = ();
for (1..$n) {
    push(@same, {});
    $same[-1]{'size4'} = int <>;
    $same[-1]{'int list n'} = \@{[map { int } split(/ /, <>)]};
}

sized_struct($n, \@lists, \@strings, \@matrices, \@same);
