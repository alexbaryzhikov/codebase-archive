#!/bin/bash

counter ()
{
	local let "c1+=$1"
	let "c2+=${1}*2"
}

for (( i=1; i <= 10; i++ )); do
	counter $i
done

echo "counters are $c1 and $c2"

