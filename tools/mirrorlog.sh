#!/bin/sh
opp()
{
  read lr
  [ $? -gt 0 ] && echo "EOF" >> mirrorlog.log && exit
  case $lr in
    1) read card; read slot
	echo "1 $card -> [$slot]" >> mirrorlog.log
	echo -e "$lr\n$card\n$slot"
	;;
    2) read slot; read card
	echo "2 [$slot] -> $card" >> mirrorlog.log
	echo -e "$lr\n$slot\n$card"
	;;
    *) read junk
	echo "* >$junk<" >> mirrorlog.log
	echo -e "$junk"
	;;
  esac
}

> mirrorlog.log
if [ $1 = "0" ]; then
  echo "1"
  echo "I"
  echo "0"
fi
while [ true ]; do
  opp
done

