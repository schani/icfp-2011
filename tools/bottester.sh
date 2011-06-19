#!/bin/bash

BOTDIR="allbots"
RESULTDIR="results"
LTG="/icfpnfs/ICFP/ltg.linux64"
DATE=$(date +%Y%m%d%H%M%S)

#stat()
#{
#
#}

for bot_a in `ls $BOTDIR`; do
	for bot_b in `ls $BOTDIR`; do
		if [ $bot_a == $bot_b ]; then
			continue
		fi
		echo "Match: $bot_a vs $bot_b"
		RESULT="${DATE}_match_$bot_a-$bot_b"
		$LTG match ./$BOTDIR/$bot_a ./$BOTDIR/$bot_b > $RESULTDIR/$RESULT 2>&1
		sed -n '/^!!/ p; /End_of_file/ p;  /Sys_error/ p' $RESULTDIR/$RESULT
		#stat
		echo
	done
done
