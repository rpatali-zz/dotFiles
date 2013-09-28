#!/bin/bash

# This script is copied from https://github.com/jwinder/xmonad

# janky way to adjust the brightness
# since my laptop always thinks it needs to boot up with 100% backlight

brightness=`cat /sys/class/backlight/acpi_video0/brightness`
desired=3
count=$(( $brightness - $desired ))

while [ $count -gt 0 ]
do
  xdotool key XF86MonBrightnessDown
  let count=$(( $count - 1 ))
done

while [ $count -lt 0 ]
do
  xdotool key XF86MonBrightnessUp
  let count=$(( $count + 1 ))
done
