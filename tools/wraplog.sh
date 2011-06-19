#!/bin/sh
BOT=../interpreter/randombot.opt
BOT=../interpreter/attackbot.opt

> wraplog.in
> wraplog.out

tee wraplog.in | $BOT $@ | tee wraplog.out

