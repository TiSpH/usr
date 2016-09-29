#!/bin/bash
read -p "enter the height of triangle:" line
for ((i=0; i<$line; i++))
do
        for ((blank=$line-$i-1; blank>0; blank--))
        do
            echo -n " "
        done
        for ((filler=2*($i+1)-1; filler>0; filler--))
        do
            echo -n "*"
        done
        echo
done
