#!/bin/bash

FILE="$1"
BASE=`basename "$FILE"`
PRE="${BASE%%.seq}"

mkdir -p BAD
mkdir -p CHK

find . -maxdepth  1 -type f -name "${PRE}?.seq" \
	| ( while read file; do
		../ctest_bg.sh $file &
	done
	wait
)

