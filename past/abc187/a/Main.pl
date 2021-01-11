#!/usr/bin/perl
use strict;
use utf8;
use List::Util qw(max);

my $line = <STDIN>;
chomp $line;
$line =~ m/(\d+)\s+(\d+)/ or die "parse failed.";
my $na = int($1);
my $nb = int($2);

sub subdigit {
	my $x = shift;
	my $s = 0;
	while ($x) {
		$s += $x % 10;
		$x = int($x / 10);
	}
	$s;
}

print max(subdigit($na), subdigit($nb)), "\n";
