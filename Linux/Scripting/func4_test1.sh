#!/bin/bash

gcd ()
{
	if [[ $1 -eq $2 ]]; then return $1; fi
	if [[ $1 -lt $2 ]]; then
		let "c=$2-$1"
		gcd $1 $c
		return $?
	else
		let "c=$1-$2"
		gcd $2 $c
		return $?
	fi
}

for ((;;)); do
	read a b
	if [[ -z $a || -z $b ]]; then echo "bye"; exit; fi
	gcd $a $b
	echo "GCD is $?"
done

