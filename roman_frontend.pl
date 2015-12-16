#!/usr/bin/perl

use warnings;
use strict;

#use GMP::Roman qw(facere);
use Roman qw(facere);

my $numeral = shift;
my $ret = &Roman::facere($numeral);
print "$ret\n";
