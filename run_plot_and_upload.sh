#!/bin/bash

racket $1.rkt > $1.csv
./make_plot.py $1.csv $1.png
cp $1.png /google/data/rw/users/gu/gustavomg/$1.png
rm $1.csv
