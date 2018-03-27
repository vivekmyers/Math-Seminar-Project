#!/bin/bash
close() {
	trap - SIGINT SIGTERM
	echo Exit
	exit 1
}
trap close SIGINT SIGTERM

printf "Coefficients: "
read l1
l2=600
l3=1
l4=128

while getopts ":gzi" opt; do
	case "$opt" in
	g)
		printf "Grid Size: "
		read l2
		;;
	z)
		printf "Zoom Factor: "
		read l3
		;;
	i)
		printf "Iterations: "
		read l4
		;;
	esac
done

printf "%s\n" "$l1" "$l2" "$(($l2 * $l3 / 2))" "$l4" | python3 prompt.py | ./solver +RTS -N4 | python3 ./render.py "$l4"
echo Done
