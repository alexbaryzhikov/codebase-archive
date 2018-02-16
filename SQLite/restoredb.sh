#!/bin/bash
# Restore database from text file 'filename.sql' to 'filename.db'
OUT=${1%.*}.db
sqlite3 $OUT < $1
echo Restored to $OUT
