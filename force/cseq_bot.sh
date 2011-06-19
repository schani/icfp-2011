#!/bin/sh
opp()
{
  read lr
  case $lr in
    1) read card; read slot;;
    2) read slot; read card;;
  esac
}
if [ $1 = "1" ]; then
  opp
fi
while [ true ]; do
  read side card <&4
  [ $? -gt 0 ] && exit
  [ "$side" = "<" ] && echo -e "1\n$card\n0";
  [ "$side" = ">" ] && echo -e "2\n0\n$card";
  opp
done

