#!/bin/bash

files_creator ()  # dir_name file_name
{
	full_name=$1/$2
	if [[ ! -e $1 ]]; then
		echo "Directory doesn't exist, creating '$1'"
		mkdir $1
	elif [[ ! -d $1 ]]; then
		echo "'$1' is not a directory, exiting"
		exit 1
	fi
	touch $1/$2
}

again="y"
while [[ $again == "y" ]]; do
	read -p "Enter directory and file names: " dir_name file_name
	files_creator $dir_name $file_name
	if [[ -f $full_name ]]; then echo "Created '$full_name'"; fi
	read -p "Again? ( y/n ): " again
done


