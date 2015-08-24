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

# Emacs key bindins
set -E

# History (see bash(1) for more details):
# - ignore lines starting with space
# - keep only one copy of any line
# - always move it to most recent
export HISTCONTROL=ignoredups

export XDG_CONFIG_HOME="${HOME}/.config"

# Specify emacs as default editor(?):
export EDITOR=emacsclient
export ALTERNATE_EDITOR=emacs
export ESHELL=/bin/bash

export CCACHE_DIR=/ccc
export CCACHE_TEMPDIR=/tmp
export CCACHE_LOGFILE=/tmp/ccache.log
export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
export USE_CCACHE=1


######### GENERAL ALIASES #########
# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"          # set LS_COLORS
    alias ls='ls --color=auto'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'
alias env='env | sort'

###################################

########## START MATHWORKS SPECIFIC ##########
#
if [ -d /mathworks ]; then

    # Customize DEFAULT_SANDBOX to where you normally work
    # (This must be defined before sourcing bash_setup.bash)
    export DEFAULT_SANDBOX=/local-ssd/lsb
    export x=$DEFAULT_SANDBOX
    if [ -f /mathworks/hub/share/sbtools/bash_setup.bash ]; then
        . /mathworks/hub/share/sbtools/bash_setup.bash
    fi

    # Use emacs as the merge/diff editor for p4
    export   EDITOR='sbe -w'
    export   VISUAL='sbe -w'
    export P4EDITOR='sbe -w'
    export   P4DIFF='sbe -sb-diff'
    export  P4MERGE='sbe -sb-merge'

    # Typically should be run in .../matlab/toolbox/stateflow/src
    alias qmake='sbmake -distcc DEBUG=1 NOBUILDTESTS=1 NORUNTESTS=1 MW_ALT_BOOST_ARCHES='
    alias dmake='sbmake -distcc DEBUG=1 NOBUILDTESTS=1 NORUNTESTS=1 MW_ALT_BOOST_ARCHES='
    alias lmake='sbmake -j 16   DEBUG=1 NOBUILDTESTS=1 NORUNTESTS=1 MW_ALT_BOOST_ARCHES='
    alias jmake='lmake prebuild ; dmake build'
    alias mw='mw -using Bstateflow'
    alias sbs='mw -using Bstateflow sbs'
    alias sbn='sb -nodesktop -nosplash -r "opengl info"'
    alias sbnj='sb -nodesktop -nosplash -nojvm -r "opengl info"'
    alias sbr='sbruntests -autofarm devel -rerunusing jobarchive'
    alias sbr_sf='sbruntests -autofarm devel -rerunusing jobarchive -runallunder test/toolbox/stateflow'
    alias sbr_all='sbruntests -autofarm devel -rerunusing jobarchive -testsuites Acgir Astlrtw'
    alias sbr_sf='sbruntests -autofarm devel -rerunusing jobarchive -runallunder test/toolbox/stateflow'

    if [ -d /sandbox/savadhan/sbtools ]; then
        if [ -f /sandbox/savadhan/sbtools/_bash_functions ]; then
            . /sandbox/savadhan/sbtools/_bash_functions
        fi
    fi
fi
#
########### END MATHWORKS SPECIFIC ###########

# This script is aware of Mathworks idiosyncracies
. ${HOME}/.my_path
