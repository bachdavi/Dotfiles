# Xinitrc
# Starts all the stuff when the xserver is launched

xrandr --output eDP1 --primary 
$HOME/./.fehbg
dropbox &
unclutter &
xscreensaver  -no-splash&
xflux -l 45 -g 9.2
nm-applet &
xmodmap -e "pointer = 1 25 3 4 5 6 7 8 9" &
xrdb ~/.Xresources 
xinput --set-prop "TPPS/2 IBM TrackPoint" 'Device Accel Velocity Scaling' 50
xinput --set-prop 11 138 0


# Windowmanager
exec i3
#exec startxfce4
