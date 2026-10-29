#!/bin/sh
set -xv

syndaemon -i 0.5 -K -R -d

xinput set-prop "ETPS/2 Elantech Touchpad" "Synaptics Palm Detection" 1
xinput set-prop "ETPS/2 Elantech Touchpad" "Synaptics Palm Dimensions" 6 150
xinput set-prop "ETPS/2 Elantech Touchpad" "Synaptics Coasting Speed" 0 0
xinput set-prop "ETPS/2 Elantech Touchpad" "Synaptics Area" 370 2890 0 0
