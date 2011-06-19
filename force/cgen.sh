#!/bin/sh

FILE="$1"
BASE=`basename "$FILE"`
PRE="${BASE%%.seq}"

CARDS=( "I" "zero" "succ" "dbl" "get" "put" "S" "K" "inc" "dec" "attack" "help" "copy" "revive" "zombie" )
LAPPL=( "a" "b"    "c"    "d"   "e"   "f"   "g" "h" "i"   "j"   "k"      "l"    "m"    "n"      "o"      )
RAPPL=( "A" "B"    "C"    "D"   "E"   "F"   "G" "H" "I"   "J"   "K"      "L"    "M"    "N"      "O"      )

export FILE
export -a CARDS

for n in `seq 0 14`; do
 ( cat "$FILE"; echo "< ${CARDS[$n]}" ) > "${PRE}${LAPPL[$n]}.seq" &
 ( cat "$FILE"; echo "> ${CARDS[$n]}" ) > "${PRE}${RAPPL[$n]}.seq" &
 wait
 echo -n .
done

wait
echo .

