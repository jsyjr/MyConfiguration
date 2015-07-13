# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022


ulimit -s 65532
ulimit -n 1024

export XDG_CONFIG_HOME="$HOME/.config"

# Specify emacs as default editor(?):
export EDITOR='/etc/alternatives/emacsclient'
export ALTERNATE_EDITOR='/etc/alternatives/emacs'

export CCACHE_DIR=/ccc
export CCACHE_TEMPDIR=/tmp
export CCACHE_LOGFILE=/tmp/ccache.log
export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
export USE_CCACHE=1

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH
if [ -f "$HOME/.my_path" ] ; then
    . "$HOME/.my_path"
fi

# For building ChromiumOS
export BOARD=amd64-generic
