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
#alias ll='ls -lhF'
#alias lla='ls -lhFA'
#alias la='ls -hA'
#alias l='ls -F'
alias env='env | sort'
alias cgir-02='rdesktop -g 1920x1600 -d mathworks -u jyates cgir-02-win64 &'
alias dff='df -h /ws /sandbox/jyates /'
alias ezfast='( cd matlab/src/cg_ir && /hub/share/sbtools/apps/cgir_tools/cgmake DEBUG=1 NORUNTESTS=1 -f ez.mk )'
alias ezmk='( cd matlab/src/cg_ir && /hub/share/sbtools/apps/cgir_tools/cgmake DEBUG=1 -f ez.mk )'
alias rgf="rg -uu --files -g "
alias sshmw='ssh jyates@jyates-deb9-64.dhcp.mathworks.com'
alias sma='ls -ltr /home/jyates/.sbsyncmaster/Bcgir_task.logs | tail'
alias smh='ls -ltr /home/jyates/.sbsyncmaster/Bcgir_thin.logs | tail'
alias cn="p4 opened|grep ' change '|sed -re 's/.* change ([0-9]+).*/\1/g'|sort|uniq|xargs p4 changelist -o|sed -nre '/^Change:/ { s/Change:.([0-9]+).*/\1/gp }'"
alias changelist="p4 opened|grep ' change '|sed -re 's/.* change ([0-9]+).*/\1/g'|sort|uniq|xargs p4 changelist -o"
alias gecks="p4 opened|grep ' change '|sed -re 's/.* change ([0-9]+).*/\1/g'|sort|uniq|xargs p4 describe|sed -nre '/^g[0-9]+/ { s/(g[0-9]+).*/\1/gp }'"
alias submit="sbsubmit -no-clickable-shell -skip toptesters -skip indent -skip sbedits -cn \$(p4 opened|grep ' change '|sed -re 's/.* change ([0-9]+).*/\1/g'|sort|uniq|xargs p4 changelist -o|sed -nre '/^Change:/ { s/Change:.([0-9]+).*/\1/gp }')"
alias ralog="( cd matlab/bin/glnxa64 && ln -sf RA_LOGGING/libmwcgir_support-RA_LOG.so libmwcgir_support.so && ln -sf RA_LOGGING/libmwcgir_support-RA_LOG.so.dbg libmwcgir_support.so.dbg )"
alias ranolog="( cd matlab/bin/glnxa64 && ln -sf RA_LOGGING/libmwcgir_support-NO_LOG.so libmwcgir_support.so && ln -sf RA_LOGGING/libmwcgir_support-NO_LOG.so.dbg libmwcgir_support.so.dbg )"
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

    alias dmake='cgmake DEBUG=1'
    alias bws='sbclone -no-pdb TBLD'
    alias nws='sbclone -no-pdb TASK'
    alias tws='sbclone -no-pdb THIN'
    alias nwspdb='sbclone -pdb TASK'
    alias fixes='cat /ws/TASK/.sbtools/p4syncinfo/mwfixes_files ; echo ; echo $(cat /ws/TASK/.sbtools/p4syncinfo/mwfixes_files | wc -l) fixed files ; echo'
    alias sbs_build="mw -using Bcgir_task_build sbs clone create -cluster Bcgir_task_build -name"
    alias sbs_task="mw -using Bcgir_task sbs clone create -cluster Bcgir_task -name"
    alias sbsdel="mw -using Bcgir_task sbs clone discard"
    alias nrss="CH_FORCEFULLY_CREATE_CACHE=1 mw checkconfig && mw gmake -j24 COMPONENTS_TO_BUILD=range_services DEBUG=1 && mw gmake -j24 COMPONENTS_TO_BUILD=cgir_support DEBUG=1"
    alias nopen="p4 opened | wc -l"

    alias sbrerun='pushd `sbroot` && sbruntests -farm devel -F none -testsuites '
    alias sbrerunlocal='pushd `sbroot` && sbruntests -local 9 -F none -rerunusing nothing -noretryfailedtests -mlswitches -no-vmemorylimit -testsuites '

    alias p4t="p4v -p p4broker-01-bgl:1666 -u jyates -c `p4 -Ztag -F %clientName% info`"

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

alias turris='ssh root@turris'
