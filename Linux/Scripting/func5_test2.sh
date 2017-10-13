#!/bin/bash

calc ()
{
	# is arg #1 an 'exit' command?
	if [ "$1" = "exit" ]; then echo "bye"; exit; fi
	# are there 3 arguments?
	if [[ -z $3 ]]; then echo "error"; exit; fi
	# is arg #2 an operator? 
	op='[-+*/%]'
	if ! [[ $2 =~ $op || $2 -eq '**' ]]; then echo "error"; exit; fi
	# are args #1 and #3 integers?
	int='^-?[0-9]+$'
	if ! [[ $1 =~ $int && $3 =~ $int ]]; then echo "error"; exit; fi
	#evaluate the expression
	echo $(($1$2$3))
}

for ((;;)); do
	read a b c
	calc "$a" "$b" "$c"
done
