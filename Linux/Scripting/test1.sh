#!/bin/bash

foo ()
{
	echo $(($1$2$3))
}

read a b c
ans=`foo "$a" "$b" "$c"`
echo $ans
