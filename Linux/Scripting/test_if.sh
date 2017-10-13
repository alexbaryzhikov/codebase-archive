#!/bin/bash

if [[ `pwd > tmp.txt` ]]
then
	echo 1
else
	echo 0
fi

