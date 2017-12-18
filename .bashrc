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

if [ -d /mathworks ]; then
    export SSD_HOME=/jyates
else
    export SSD_HOME=${HOME}
fi
export XDG_CONFIG_HOME="${SSD_HOME}/.config"

# Specify emacs as default editor(?):
export EDITOR=emacsclient
export VISUAL=emacsclient
export P4EDITOR=emacsclient

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
alias cgir-02='rdesktop -g 1920x1600 -d mathworks -u jyates cgir-02-win64 &'
alias dff='df -h /ws /sandbox/jyates / /home/jyates'

###################################

########## START MATHWORKS SPECIFIC ##########
#
if [ -d /mathworks ]; then

    # Customize DEFAULT_SANDBOX to where you normally work
    # (This must be defined before sourcing bash_setup.bash)
    export DEFAULT_SANDBOX=/ws/Bcgir_task.latest_pass/
    export x=$DEFAULT_SANDBOX
    if [ -f /mathworks/hub/share/sbtools/bash_setup.bash ]; then
        . /mathworks/hub/share/sbtools/bash_setup.bash
    fi

    export SBRT_FULL_CFG="-cfg Acgir_Aslrtw -cfg cgir_ui_test_exclude -cfg /mathworks/hub/share/sbtools/apps/cgir_tools/latest_source_to_test_mapping/excludefile_noCgCtxCreate_Aslrtw_Acgir.txt"

    alias sbn='sb -nodesktop -nosplash -r "opengl info"'
    alias sbnj='sb -nodesktop -nosplash -nojvm -r "opengl info"'
    alias sbf='    sbruntests -autofarm devel -rerunusing jobarchive'
    alias sbf_sf=' sbruntests -autofarm devel -rerunusing jobarchive -runallunder test/toolbox/stateflow'
    alias sbf_all='sbruntests -autofarm devel -rerunusing jobarchive -testsuites Acgir_Aslrtw'
    alias sbl='    sbruntests -local    all   -rerunusing jobarchive'
    alias sbl_sf=' sbruntests -local    all   -rerunusing jobarchive -runallunder test/toolbox/stateflow'
    alias sbl_all='sbruntests -local    all   -rerunusing jobarchive -testsuites Acgir_Aslrtw'

    alias new-task='(cd /ws; sbrmtree TASK; sbclone -no-perforce Bcgir_task.latest_pass TASK)'

    # if [ -d /sandbox/savadhan/sbtools ]; then
    #     if [ -f /sandbox/savadhan/sbtools/_bash_functions ]; then
    #         . /sandbox/savadhan/sbtools/_bash_functions
    #     fi
    # fi

fi
#
########### END MATHWORKS SPECIFIC ###########

# This script is aware of Mathworks idiosyncracies
. ${HOME}/.my_path
