close() {
	trap - SIGINT SIGTERM
	echo Done
	exit 1
}
trap close SIGINT SIGTERM

printf "Coefficients: "
read l1
printf "Grid Size: "
read l2
l3="$l2"'/2.0'
l4=64

while getopts ":zi" opt; do
	case "$opt" in
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

printf "%s\n" "$l1" "$l2" "$l3" "$l4" | python3 prompt.py | ./solver +RTS -N4 > output.txt
python3 ./render.py < output.txt
echo Done
