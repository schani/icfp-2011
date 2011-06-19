#!/bin/bash

LAST=`ls -d round_* | colrm 1 6 | sort -n | tail -1`

NEXT=`printf "%04d" $[ 10#$LAST + 1 ]`

mkdir "round_$NEXT"

( cd "round_$NEXT";
	find ../round_$LAST/ -maxdepth 1 -type f -name '*.seq' -exec \
		../cgen.sh {} \; -exec ../ctall.sh {} \;
)

