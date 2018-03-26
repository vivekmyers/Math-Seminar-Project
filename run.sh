printf "Coefficients: "
read l1
printf "Grid Size: "
read l2
l3="$l2"'/2.0'
l4=64
l5=1e-15

while getopts ":zita" opt; do
	case "$opt" in
	z)
		printf "Zoom Factor: "
		read l3
		;;
	i)
		printf "Iterations: "
		read l4
		;;
	t)
		printf "Tolerance: "
		read l5
		;;
	a)
		l5=Infinity
	esac
done

printf "%s\n" "$l1" "$l2" "$l3" "$l4" "$l5" | python3 prompt.py | ./solver +RTS -N4 > output.txt
python3 ./render.py < output.txt
