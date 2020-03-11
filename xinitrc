export _JAVA_AWT_WM_NONREPARENTING=1
xsetroot -cursor_name left_ptr
setxkbmap -option "caps:ctrl_modifier"
setxkbmap -layout us -variant altgr-intl

nm-applet &
volumeicon &
redshift &
xss-lock slock &

exec emacs
