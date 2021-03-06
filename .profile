#!/bin/dash

export EDITOR=/usr/bin/emacsclient
export MATLAB_MEM_MGR=debug,nointegcheck

# ~/.profile: executed by sh for login shells.

# This file will only be executed on accounts with '/bin/sh' as the login shell.
# bash login shells will normally execute ~/.bash_profile instead of this file,
# however if a bash login shell is launched as /bin/sh (such as on a mac, or
# non-debian linux system), bash will source this file instead.

# In my configuration both .xsessionrc and .bash_profile simply source this file.

# Setup minimal sbtools environment (sets PATH, LOCATION, HOST, USER, ...)
if [ -d /mathworks ]; then
    . /mathworks/hub/share/sbtools/bash_setup_env.bash
    export q=/mathworks/devel/sandbox/$USER/qualify
    /hub/share/sbtools/bin/glnxa64/sbvncserver -vnctype tigervnc -alwaysshared \
         -plainUsers=jyates,dserr,ntalele,amathewi,dgutierr,jlawranc\
         ,martind,ebergero,akshathb,kaakash,annquach,xiaofeng\
         ,ksurende,jhafer,pnagare,donghe
fi

# Rewrite path per my own preferences
source ~/.my_path

# xrdb ~/.Xresources
if [ -d /ccc ]; then
    export CCACHE_DIR=/ccc
    export CCACHE_LOGFILE=/tmp/ccache.log
    export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
    export CCACHE_TEMPDIR=/tmp
    export USE_CCACHE=1
fi
