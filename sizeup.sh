#!/bin/bash -x

# Name: sizeup.sh
# Version: 1.0
# Author: Sascha Matzke
# License: Public Domain

# Emulate SizeUp.app on Linux

COUNTER=0;
str=`xrandr | sed -e '/[^s]connected/!d' -e "s/.*connected\s\([0-9]*\)x\([0-9]*\)+\([0-9]*\)+\([0-9]*\).*/\1\n\2\n\3\n\4/"`;
for x in $str;
do
	case $COUNTER in
		0 ) screen1xres=$x;;
		1 ) screen1yres=$x;;
		2 ) screen1xoffset=$x;;
		3 ) screen1yoffset=$x;;
		4 ) screen0xres=$x;;
		5 ) screen0yres=$x;;
		6 ) screen0xoffset=$x;;
		7 ) screen0yoffset=$x;;
	esac
	((COUNTER++))
done;

# Try out various combination until it works for you

framecomp=0
framecompy=41
gravity=0
cxofs=96
cyofs=64

# get mouse pos to determine screen
xpos=`xmousepos | cut -d ' ' -f 1`

# remove maximized props
wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz;

# set var depending on screen - we're assuming a 2-screen setup
if [ "$xpos" -lt "$screen0xres" ]; then
	startxpos=0
	screenxres=$screen0xres
	screenyres=$screen0yres
else
	startxpos=$screen0xres
	screenxres=$screen1xres
	screenyres=$screen1yres
fi

case "$1" in
	"l")    # left
		wmctrl -r :ACTIVE: -e $gravity,$[$startxpos],0,$[$screenxres/2-$framecomp],-1
		wmctrl -r :ACTIVE: -b add,maximized_vert ;;
	"r")    # right
		wmctrl -r :ACTIVE: -e $gravity,$[$startxpos+$screenxres/2+$framecomp],0,$[$screenxres/2-$framecomp],-1
		wmctrl -r :ACTIVE: -b add,maximized_vert ;;
	"m")    # maximize
		wmctrl -r :ACTIVE: -b add,maximized_vert,maximized_horz ;;
	"c")    # center minux cxofs/cyofs
		wmctrl -r :ACTIVE: -e $gravity,$[$startxpos+$cxofs],$cyofs,$[$screenxres-2*$cxofs],$[$screenyres-2*$cyofs] ;;
	"u")	# upper half
		wmctrl -r :ACTIVE: -e $gravity,$startxpos,0,$screenxres,$[$screenyres/2-$framecompy]
		wmctrl -r :ACTIVE: -b add,maximized_horz ;;
	"d")	# lower half
		wmctrl -r :ACTIVE: -e $gravity,$startxpos,$[$screenyres/2+$framecompy],$screenxres,$[$screenyres/2-$framecompy]
		wmctrl -r :ACTIVE: -b add,maximized_horz ;;
	"ur")   # upper right quadrant
		wmctrl -r :ACTIVE: -e $gravity,$[$startxpos+$screenxres/2-$framecomp],0,$[$screenxres/2-$framecomp],$[$screenyres/2-$framecompy] ;;
	"ul")   # upper left quadrant
		wmctrl -r :ACTIVE: -e $gravity,$[$startxpos],0,$[$screenxres/2-$framecomp],$[$screenyres/2-$framecompy] ;;
	"dr")   # lower right quadrant
		wmctrl -r :ACTIVE: -e $gravity,$[$startxpos+$screenxres/2-$framecomp],$[$screenyres/2+$framecompy],$[$screenxres/2-$framecomp],$[$screenyres/2-$framecompy];;
	"dl")	# lower left quadrant
		wmctrl -r :ACTIVE: -e $gravity,$[$startxpos],$[$screenyres/2+$framecompy],$[$screenxres/2-$framecomp],$[$screenyres/2-$framecompy];;
esac
