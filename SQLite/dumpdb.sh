#!/bin/bash
# Dump database 'filename.db' to text file 'filename.sql'
OUT=${1%.*}.sql
sqlite3 $1 .dump > $OUT
echo Dumped to $OUT
