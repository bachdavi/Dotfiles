# Xinitrc
# Starts all the stuff when the xserver is launched

xrandr --output eDP1 --primary 
$HOME/./.fehbg
dropbox &
unclutter &
xscreensaver  -no-splash&
~/.scripts/screensaver_script &
xflux -l 45 -g 9.2
nm-applet &
xmodmap -e "pointer = 1 25 3 4 5 6 7 8 9" &
xrdb ~/.Xresources 

# Windowmanager
exec i3
# exec startxfce
