#!/usr/bin/gnuplot -persist

set output 'test03.png'
set term png size 1280,720
set grid 
plot [-5:6.5] sin(x) with lines

