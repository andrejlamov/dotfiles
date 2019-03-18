#exec startxfce4
xsetroot -cursor_name left_ptr

setxkbmap -option "caps:ctrl_modifier"
setxkbmap -layout us -variant altgr-intl

nm-applet &
pasystray &
exec emacs
