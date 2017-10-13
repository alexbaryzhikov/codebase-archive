#! /usr/bin/gnuplot -persist
out_name = "plot.png"
in_name = "data.csv"
column_1 = 2
column_2 = 3
x1 = 100
x2 = 150
x3 = 250

set terminal png enhanced
set output out_name
set size ratio 0.5
set title "График для данных в файле ".in_name." столбцы ".column_1.", ".column_2
set xlabel "Время обработки, с" 
set ylabel "Повышение сорбции красителя, %" 
set xtics ("point 1, value ".x1 x1, "point 2, value ".x2 x2, "point 3, value ".x3 x3)
set autoscale y
set grid
set key autotitle columnhead
 
plot in_name using 1:column_1 smooth csplines notitle with lines lt 1,\
in_name using 1:column_1 with points pointsize 1 pointtype 7 lt 1,\
in_name using 1:column_2 smooth csplines notitle with lines lt 2,\
in_name using 1:column_2 with points pointsize 1 pointtype 13 lt 2
