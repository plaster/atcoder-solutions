#!/bin/bash

s() {
	X="$1"
	SUM=0
	while [ "$X" != 0 ]
	do
		SUM=$(( SUM + X % 10 ))
		X=$(( X / 10 ))
	done
	echo "$SUM"
}

read A B
{
	s $A
	s $B
} | sort -nr | head -n1

