#!/bin/bash
printf "Coefficients: "
read l1
printf "Grid Size: "
read l2
printf "Zoom Factor: "
read l3
printf "Iterations: "
read l4
printf "%s\n" "$l1" "$l2" "$l3" "$l4" | python3 prompt.py | ./solver > output.txt
python3 ./render.py < output.txt
