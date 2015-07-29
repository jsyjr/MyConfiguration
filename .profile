# -*-shell-script-*-
# ~/.profile: executed by sh for login shells.

# This file will only be executed on accounts with '/bin/sh' as the login shell.
# bash login shells will normally execute ~/.bash_profile instead of this file,
# however if a bash login shell is launched as /bin/sh (such as on a mac, or
# non-debian linux system), bash will source this file instead.

# Setup minimal sbtools environment (sets PATH, LOCATION, HOST, USER, ...)
if [ -d /mathworks ]; then
    . /mathworks/hub/share/sbtools/bash_setup_env.bash
fi

