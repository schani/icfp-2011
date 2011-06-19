#!/bin/bash

rm -rf round_*

mkdir round_0000
( cd round_0000; 
	../cgen.sh ../x.seq 
	../ctall.sh ../x.seq
)

