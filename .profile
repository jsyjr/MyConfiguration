# -*-shell-script-*-
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
fi

# Rewrite path per my own preferences
. ~/.my_path

# xrdb ~/.Xresources
export CCACHE_DIR=/ccc
#export CCACHE_LOGFILE=/ccc/LOGFILE
export CCACHE_LOGFILE=/tmp/ccache.log
export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
#export CCACHE_TEMPDIR=/huge/ccache
export CCACHE_TEMPDIR=/tmp
export USE_CCACHE=1
