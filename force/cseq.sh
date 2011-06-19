#!/bin/sh
while [ true ]; do
  read -t 1 side card
  [ $? -gt 0 ] && exit
  [ "$side" = "<" ] && echo -e "1\n$card\n0";
  [ "$side" = ">" ] && echo -e "2\n0\n$card";
done

