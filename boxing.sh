printf "Coefficients: "
read ln
./winding &>/dev/null << EOF
$ln
4
EOF
./boxes &>/dev/null << EOF 
$ln
4
EOF
open boxes.html
