#!/bin/bash

if [[ -n $1 ]]
then
	if [[ $1 -eq 0 ]]
	then
		echo "No students"
	elif [[ $1 -eq 1 ]]
	then
		echo "1 student"
	elif [[ $1 -le 4 ]]
	then
		echo "$1 students"
	else
		echo "A lot of students"
	fi
else
	echo "No argument given."
fi

