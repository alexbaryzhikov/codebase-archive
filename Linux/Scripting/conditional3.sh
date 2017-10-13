#!/bin/bash

if [[ $# -ne 2 ]]
then
	echo "Specify exactply two arguments."
else
	case $1 in
		1)
			echo "Creating file $2..."
			touch $2
			;;
		2)
			echo "Creating dir $2..."
			mkdir $2
			;;
		*)
			echo "Wrong argument #1"
	esac
fi

