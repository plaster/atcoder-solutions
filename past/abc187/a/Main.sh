#!/bin/bash

s() {
	perl -Mstrict -e 'my $x = $ARGV[0];
	my $s = 0;
	while ($x) {
		$s += $x % 10;
		$x = int($x / 10);
	}
	print $s,"\n";
	' "$1"
}

read A B
{
	s $A
	s $B
} | sort -nr | head -n1

