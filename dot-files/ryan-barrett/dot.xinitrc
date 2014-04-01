#!/bin/sh

# load my X resources and keysyms
xmodmap ~/.Xmodmap
xrdb ~/.Xresources

# turn on stickykeys. don't let two keys pressed at the same time disable it.
# don't turn on "latch lock", ie pressing a modifier key twice "locks" it on.
xkbset accessx sticky -twokey -latchlock

# don't expire these settings. (run xkbset q exp for details.)
xkbset exp 1 =accessx =sticky =twokey =latchlock

# X settings. turn off audio bell, turn off auth, and don't turn off screen
# after 10m (from http://ubuntuforums.org/showthread.php?t=1404717 )
xhost +
xset b off
xset -dpms
xset s off

# if i wanted, i could add --alternate-editor '' to make it start an emacs
# --daemon if one isn't already running.
emacsclient -f ~/.emacs.d/server/server -d $DISPLAY \
 --eval "(progn (make-my-frames) (start-my-shells))" &

autocutsel -fork -selection PRIMARY
autocutsel -fork -selection CLIPBOARD

# this is run by bash, hence the &>> instead of >>&
~/bin/url-corpinizer.py &>> /tmp/X.log &

# f.lux (changes color temperature dynamically)
# http://www.stereopsis.com/flux/
~/bin/xflux -z 94105

urxvt &
firefox --no-remote -P personal &
firefox -P work &
pidgin &

if [[ $HOST == "laptop" ]]; then
  # mouse acceleration
  xset m 1
  ~/src/misc/reset_laptop_display.sh
fi

xscreensaver &

if [[ $HOST =~ ^heaven ]]; then
  ~/bin/mouse_left.sh
fi

exec /usr/bin/ion3
