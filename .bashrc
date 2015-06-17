# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

alias qmake='sbmake -distcc DEBUG=1 NOBUILDTESTS=1 NORUNTESTS=1 MW_ALT_BOOST_ARCHES='
alias mw='mw -using Bstateflow'
alias sbs='mw -using Bstateflow sbs'

. /sandbox/savadhan/sbtools/_bash_functions

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
# case "$TERM" in
# xterm-color)
#     PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#     ;;
# *)
#     PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#     ;;
# esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# Show current working directory in shell prompt
PS1='\[\033[01;34m\]\w\[\033[00m\]\$ '


# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='ls --color=auto --format=vertical'
    #alias vdir='ls --color=auto --format=long'
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'

#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# # set PATH
# if [ -f "$HOME/.my_path" ] ; then
#     . "$HOME/.my_path"
# fi

alias pss='ps afx | grep sbx'
alias pure='~/asd/src/tools/sbxstop.sh; src/ctools/clean; mam ; rm -rf data/log/* data/trace/* code clu_code; pss'

#=====================================================================================================

cleantmp() {
    rm -f /tmp/tmp* /tmp/*.n3 /tmp/*.ttl /tmp/*.so /tmp/*quad* /tmp/*.out /tmp/*.tmp /tmp/cat* /tmp/*log /tmp/EMP* /tmp/emp* /tmp/EXT* /tmp/ext* /tmp/nzbacktrace.* /tmp/rrmgrStore* /tmp/spin_pid /tmp/stl_test.* /tmp/xalltypes.dat /tmp/xfermls* /tmp/xsl.*
}

#=====================================================================================================

# Clean out a check files and get reddy to run sqlsmoke
nzcleantest () {
    if [ ! -d "src/amake" ]; then
        echo "Not in the root of a source tree."; return 1
    fi
    cleantmp
#   rm -rf \{ debug \| turbo \}/\{ *tests \| epsilon \| esi \| netget \| tpc_small \}/*
    rm -rf obj/debug/esi/* obj/debug/tests/* obj/debug/sqlldr_tests/* obj/debug/daves_tests/* obj/debug/epsilone/* obj/debug/netgen/* obj/debug/tpc_small/*
    rm -rf obj/turbo/esi/* obj/turbo/tests/* obj/turbo/sqlldr_tests/* obj/turbo/daves_tests/* obj/turbo/epsilone/* obj/turbo/netgen/* obj/turbo/tpc_small/*
    rm -rf /tmp/amake
}

#=====================================================================================================

# re-make amake and refresh am.dmp
mam () {
    if [ ! -d "src/amake" ]; then
        echo "Not in the root of a source tree."; return 1
    fi
    ( command rm -rf data source rel cobj gobj obj tobj acpp acproto amake am.dmp src/build src/BROWSE src/tags src/TAGS \
           && cd src/amake \
           && ./amake.bsh LINUXPRE )
    rm -f acproto
    ./amake -cache >/dev/null
    ./amake -emacs >/dev/null
}

#=====================================================================================================

# Re-make amake, refresh am.dmp and the tags file
SBTAGS_EXCLUDE="--exclude=gui --exclude=guitest --exclude=idocs --exclude=sbtest --exclude=tests --exclude=tickit"
sbtags () {
    mam
    ( command cd src \
           && ctags-exuberant -f tags --excmd=pattern --tag-relative --fields=-afkKz+msSt --sort=foldcase --totals ${SBTAGS_EXCLUDE} -R . \
           && ctags-exuberant -f TAGS -e ${SBTAGS_EXCLUDE} -R . )
    # ctags-exuberant --excmd=pattern --fields=-f-K-z+aiklmnsSt --sort=foldcase --tag-relative --totals=yes -f tags \
    #    --languages=+all,-HTML,-SQL --exclude=tests -R src
}

#=====================================================================================================

# Clean out a source tree and get ready to build
nzcleanbuild () {
    if [ ! -d "src/amake" ]; then
        echo "Not in the root of a source tree."; return 1
    fi
    nzcleantest
    rm -rf log.[1-9]* data debug doc doxy gcov images obj/* simdata tmp turbo
    ( command cd src/linux_drivers \
           && rm -rf */*.o */*.ko */*.o */.*.cmd */.tmp_versions */*.mod.c )
    nztags
}

export LOG_STDOUT=Y
