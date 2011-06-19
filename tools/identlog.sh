#!/bin/sh
opp()
{
  read lr
  case $lr in
    1) read card; read slot
	echo "1 $card -> [$slot]" >> identlog.log
	;;
    2) read slot; read card
	echo "2 [$slot] -> $card" >> identlog.log
	;;
    *) read junk
	echo "* >$junk<" >> identlog.log
	;;
  esac
}

> identlog.log
if [ $1 = "1" ]; then
  opp
fi
while [ true ]; do
  echo "1"
  echo "I"
  echo "0"
  opp
done

