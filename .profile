#!/bin/dash

export EDITOR=/usr/bin/emacs
export MATLAB_MEM_MGR=debug,nointegcheck
export GCM_CREDENTIAL_STORE=plaintext
export GCM_PLAINTEXT_STORE_PATH=~/.git-credential-cache/credentials
export P4MERGE=p4merge

# ~/.profile: executed by sh for login shells.

# This file will only be executed on accounts with '/bin/sh' as the login shell.
# bash login shells will normally execute ~/.bash_profile instead of this file,
# however if a bash login shell is launched as /bin/sh (such as on a mac, or
# non-debian linux system), bash will source this file instead.

# In my configuration both .xsessionrc and .bash_profile simply source this file.

# Rewrite path per my own preferences
. ~/.my_path

# Setup minimal sbtools environment (sets PATH, LOCATION, HOST, USER, ...)
if [ -d /mathworks ]; then
    . /mathworks/hub/share/sbtools/bash_setup_env.bash
<<<<<<< Updated upstream
    export q=/mathworks/devel/sandbox/$USER/qualify
    # /mathworks/hub/share/sbtools/bin/glnxa64/sbvncserver -vnctype tigervnc -alwaysshared
    # -plainUsers=jyates,dserr,ntalele,amathewi,dgutierr,jlawranc,martind,akshathb,kaakash,annquach,xiaofeng,jhafer,donghe
fi
||||||| constructed merge base
    export q=/mathworks/devel/sandbox/$USER/qualify
    /hub/share/sbtools/bin/glnxa64/sbvncserver -vnctype tigervnc -alwaysshared \
         -plainUsers=jyates,dserr,ntalele,amathewi,dgutierr,jlawranc\
         ,martind,ebergero,akshathb,kaakash,annquach,xiaofeng\
         ,ksurende,jhafer,pnagare,donghe
fi
=======
    export c=/local/cgmux/apps/cgir_tools
    export m=/local/cgmux/apps/cgir_tools/cgmuxSyncmaster
    export t=/local/cgmux/apps/cgir_tools/cgmuxTest
>>>>>>> Stashed changes

<<<<<<< Updated upstream
# Rewrite path per my own preferences
source ~/.my_path
||||||| constructed merge base
# Rewrite path per my own preferences
. ~/.my_path
=======
#    /hub/share/sbtools/bin/glnxa64/sbvncserver -vnctype tigervnc -alwaysshared

#    /hub/share/sbtools/bin/glnxa64/sbvncserver \
#         -plainUsers jyates,dserr,ntalele,amathewi,dgutierr,jlawranc\
#         ,martind,ebergero,akshathb,kaakash,annquach,xiaofeng\
#         ,ksurende,jhafer,pnagare,donghe
fi
>>>>>>> Stashed changes

# xrdb ~/.Xresources
<<<<<<< Updated upstream
if [ -d /ws/.ccache ]; then
    export CCACHE_DIR=/ws/.ccache
    export CCACHE_LOGFILE=/tmp/ccache.log
    export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
    export CCACHE_TEMPDIR=/tmp
||||||| constructed merge base
if [ -d /ccc ]; then
    export CCACHE_DIR=/ccc
    export CCACHE_LOGFILE=/tmp/ccache.log
    export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
    export CCACHE_TEMPDIR=/tmp
=======
if [ -d /local/.ccache ]; then
    export CCACHE_DIR=/local/.ccache
    export CCACHE_CONFIGPATH=/local/.ccache/ccache.conf
>>>>>>> Stashed changes
    export USE_CCACHE=1
fi
