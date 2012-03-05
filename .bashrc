# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

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
case "$TERM" in
xterm-color)
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    ;;
*)
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    ;;
esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

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



#=====================================================================================================

cleantmp() {
    rm /tmp/*.out /tmp/*.tmp /tmp/cat* /tmp/*log /tmp/EMP* /tmp/emp* /tmp/EXT* /tmp/ext* /tmp/nzbacktrace.* /tmp/rrmgrStore* /tmp/spin_pid /tmp/stl_test.* /tmp/xalltypes.dat /tmp/xfermls* /tmp/xsl.*
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

# Rebuild amake, refresh am.dmp and the tags file
nztags () {
    if [ ! -d "src/amake" ]; then
        echo "Not in the root of a source tree."; return 1
    fi
    ( command rm -rf acpp acproto amake am.dmp BROWSE obj/amake obj/caches/* tags TAGS \
           && cd src/amake \
           && ./amake.bsh LINUX )
    rm -f acproto
    ./amake -cache >/dev/null
    ./amake -dumponly am.dmp
    emacsclient -e "(cd \"$(pwd)\")"
    emacsclient -e "(am-scan \"$(pwd)/am.dmp\")"
    echo "$(pwd): am.dmp refreshed and reloaded, rebuilding TAGS"
    ctags --excmd=pattern --fields=-k+aiKlmnSz --sort=foldcase --tag-relative --totals=yes -f tags \
        --languages=+all,-HTML,-SQL --exclude=webclient --exclude=winclient --exclude=tests -R src
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

#=====================================================================================================

PATH=\
${HOME}/bin:\
/usr/local/sbin:\
/usr/local/bin:\
/usr/sbin:\
/usr/bin:\
/sbin:\
/bin:\
.:\
./debug/bin:\
./debug/bin/adm:\
..:\
../debug/bin:\
../debug/bin/adm:\
../..:\
../../debug/bin:\
../../debug/bin/adm:\
../../..:\
../../../debug/bin:\
../../../debug/bin/adm:\
../../../..:\
../../../../debug/bin:\
../../../../debug/bin/adm:\
../../../../..:\
../../../../../debug/bin:\
../../../../../debug/bin/adm:\
../../../../../..:\
../../../../../../debug/bin:\
../../../../../../debug/bin/adm:\
../../../../../../..:\
../../../../../../../debug/bin:\
../../../../../../../debug/bin/adm:\
../../../../../../../..:\
../../../../../../../../debug/bin:\
../../../../../../../../debug/bin/adm:\
/opt/accurev/bin:\

#/opt/net/tools/bin:\
#/opt/bin:\


# Specify emacs as default editor(?):
export EDITOR="/usr/bin/emacsclient -a /usr/bin/emacs -c '(raise-frame)'; /usr/bin/emacsclient"
export ALTERNATE_EDITOR='/usr/bin/emacs'


[ -f /opt/net/tools/etc/sh_accurev ] && . /opt/net/tools/etc/sh_accurev
export AC_EDITOR_GUI=$EDITOR

export CCACHE_DIR=/home/workspaces/$USER/ccache
export AOS_CCACHE=ccache

# DBOS security (we always run as a dbos super user)
export NZ_DATABASE=DEV
export NZ_USER=admin
export NZ_PASSWORD=password

export LOG_STDOUT=Y
# export NO_MIRROR=Y

export NZ_NO_SPS_RESTART=1
export NZ_AUTOCREATE_DB=1
export NZ_AUTO_FAILOVER=0
export NZ_AUTO_REGEN=0
export NZ_AUTO_RESET=0
export NZ_BLAST_SUPPORT=0
export NZ_CLOB_SUPPORT=0
export NZ_DELETE_PLANS=1
export NZ_MEMORY_MAX_MBYTES=512
export NZ_MAXCONNECTIONS=20
export NZ_WATCHDOG=0
# export GENC_SPU_DEBUG=3

# export NZ_DIAB_OPT="-O -Xinline=500 -Xrestart=0 -Xunroll=0"
unset NZ_DIAB_OPT

# This is where we checkout a Diab compiler license
export LM_LICENSE_FILE=7789@milkyway


alias workon='. $HOME/bin/workon.bsh'

# GENC_LIST controls whether the machine code listings are left in ../data/plans
# export GENC_LIST=Y
alias gl='set | grep GENC_LIST'
alias gly='export GENC_LIST=Y'
alias gln='export GENC_LIST=N'

# Show current working directory in shell prompt
PS1='$PWD \$ '

# Some helpful cvs command abbreviations:
alias cvsstat='cvs -q status 2>/dev/null | grep Needs'
alias cvswip='cvs -q status 2>/dev/null | grep Status: | egrep -v "Up-to-date|Patch|Checkout|Invalid"'
alias cvsup='cvs -q update -Pd  2>/dev/null ; echo "remember to perform tags-jsy"'

# Abbreviation for building amake:
alias mam='pushd ~/nz/src/amake >/dev/null;rm /tmp/*.o;./amake.bsh LINUX;popd >/dev/null'

export NUM_SPUS=1
alias ns1='export NUM_SPUS=1;ss'
alias ns3='export NUM_SPUS=3;ss'
alias nzr='unset NZ_SIM;ns3'
alias nzs='export NZ_SIM=Y;ns1'
alias ss='env | egrep "NUM_SPU|NZ_SIM";env | grep FPGA'

# GENC_SOURCE controls whether the codeN.cpp files are copied to genedN.cpp
# export KEEP_SOURCE=N
# alias ks='set | grep KEEP_SOURCE'
# alias ksy='export KEEP_SOURCE=Y'
# alias ksn='export KEEP_SOURCE=N'

# export FPGA_PREP=Y
unset FPGA_PREP
# export REAL_FPGA=Y
unset REAL_FPGA

alias FPGA_OFF='export FPGA_PREP=N;export REAL_FPGA=N;cp ~/ForOthers/MarkO/fpga5.bit ~/nz/kit/sbin/fpga5.bin;ss'
alias FPGA_ON='export FPGA_PREP=Y;export REAL_FPGA=Y;cp ~/nz/src/nde/firm/fpga5.bit  ~/nz/kit/sbin/fpga5.bin;ss'
