#!/bin/bash

for (( ;; ))
do
	echo "enter your name:"
	read name
	if [[ -z $name ]]
	then
		echo "bye"
		exit
	fi

	echo "enter your age:"
	read age
	if [[ $age -eq 0 ]]
	then
		echo "bye"
		exit
	fi	

	if [[ $age -le 16 ]]
	then
		group="child"
	elif [[ $age -le 25 ]]
	then
		group="youth"
	else
		group="adult"
	fi

	echo "$name, your group is $group"
done

