#!/bin/bash

PRE="${1%%.seq}"
# echo -n "testing $1 ... ";
../ltg.linux64 -non-blocking true match ../cseq_bot.sh ../ident.sh 2>&1 4<$PRE.seq \
	| ../ltg_clean | sed '/End_of_file/ q' >$PRE.out

export PRE
sed '/End_of_file/ d' <$PRE.out | sed '/Sys_error/ d' | grep -q Exception
R1=$?

egrep ']:|turn' <$PRE.out | grep -A 1 turn | tail -1 | fgrep -q 'turn'
R2=$?

[ $R1 -eq 0 ] \
	&& ( echo "$1 FAIL $R1" ; mv $PRE.* BAD/ )
[ $R1 -gt 0 -a $R2 -eq 0 ] \
	&& ( echo "$1 CHK $R1 $R2" ; mv $PRE.* CHK/ ) 
[ $R1 -gt 0 -a $R2 -gt 0 ] \
	&& echo "$1 OK"

