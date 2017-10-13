#!/bin/bash

again=y
while [ "$again" = "y" ]
do
	echo "Enter a name:"
	read name
	echo "You entered \"$name\""

	echo "Continue? ( y/n )"
	read again
done

