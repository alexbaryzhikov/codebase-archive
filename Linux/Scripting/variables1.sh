#!/bin/bash

dir_path=~/Documents/test_dir
file_path=$dir_path/test_file.txt

echo "Creating file $file_path..."
touch $file_path
echo "Checking contents of the dir $dir_path..."
ls -l $dir_path

