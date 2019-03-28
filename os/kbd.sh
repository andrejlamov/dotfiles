#!/bin/bash
set -xe
{
    export DISPLAY=:0.0
    export XAUTHORITY=/home/andrej/.Xauthority

    export MASTER_KEYBOARD_ID=$(xinput -list | pcregrep -o1 'id=(\d+).*master\s*keyboard.*')
    export HHKB_ID=$(xinput -list | pcregrep -o1 'HHKB.*id=(\d+)')

    setxkbmap -device $MASTER_KEYBOARD_ID -option "caps:ctrl_modifier"
    setxkbmap -device $MASTER_KEYBOARD_ID -layout us -variant altgr-intl
    setxkbmap -device $HHKB_ID -layout us -variant altgr-intl
} 2>&1 | tee /tmp/udev-kbd.log
