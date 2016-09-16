#!/bin/bash

mkdir -p tmp_ins tmp_size

gen_file(){
  echo "set term png"
  echo "set out \"$1.$2.$3.png\""
  echo "$4"
  echo -n "plot "
  for x in i v vu vm j jw; do
    echo -n "\"$1/$x.dat\" using $2:$3 title \"$x\" with linespoints, "
  done
}

for x in i v vu vm j jw; do
  head -36 ftest.out | grep "^$x	" > tmp_ins/$x.dat
  tail -36 ftest.out | grep "^$x	" > tmp_size/$x.dat
done

# user time by number of inserts
gen_file tmp_ins 2 4 "set ylabel 'time'; set xlabel 'insertions'" | gnuplot

# memory by number of insert
gen_file tmp_ins 2 6 "set ylabel 'memory'; set xlabel 'insertions'"| gnuplot

# user time by size
gen_file tmp_size 3 4 "set ylabel 'time'; set xlabel 'max value'" | gnuplot

# memory by size
gen_file tmp_size 3 6 "set ylabel 'memory'; set xlabel 'max value'" | gnuplot



