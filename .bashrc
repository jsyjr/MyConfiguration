#!/bin/bash

# If not running interactively, don't do anything
#[ -z "$PS1" ] && return

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
# case "$TERM" in
# xterm-color|st-*)
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#     ;;
# *)
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#     ;;
# esac

# Comment in the above and uncomment this below for a color prompt
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n\$ '

# Show current working directory in shell prompt
# PS1='\[\033[01;34m\]\w\[\033[00m\]\$ '


# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*|st-*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Emacs key bindings
set emacs

# History (see bash(1) for more details):
# - ignore lines starting with space
# - keep only one copy of any line
# - always move it to most recent
export HISTCONTROL=ignoredups

export XDG_CONFIG_HOME="${HOME}/.config"

# Specify emacs as default editor(?):
export EDITOR=emacs
export VISUAL=emacs
export P4EDITOR=emacs

export ALTERNATE_EDITOR=emacs
export ESHELL=/bin/bash

if [ -d /ws/.ccache ]; then
    export CCACHE_DIR=/ws/.ccache
elif [ -d /ccc ]; then
    export CCACHE_DIR=/ccc
fi
export CCACHE_CONFIGPATH=${CCACHE_DIR}/ccache.conf
export CCACHE_TEMPDIR=/tmp
export CCACHE_LOGFILE=/tmp/ccache.log
export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
export USE_CCACHE=1

# Suppress offer to report MATLAB crashes
export CG_ASSERT_CONTINUE=1

# This script is aware of Mathworks idiosyncracies
. ${HOME}/.my_path

# Shell aliases
. ${HOME}/.alias
