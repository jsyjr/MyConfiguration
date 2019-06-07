#!/bin/dash
# CAVEAT! cron picks up my network HOME (= /home/jyates) rather than my local version (= /jyates).
# This line ensures that the environment supplied to invoked commands specifies the correct HOME.
export HOME=/jyates

# Should match ${HOME}/.profile
export CCACHE_DIR=/ccc
export CCACHE_LOGFILE=/ccc/LOGFILE
export CCACHE_SLOPPINESS=include_file_mtime,file_macro,time_macros
export CCACHE_TEMPDIR=/huge/ccache
export USE_CCACHE=1

# export MAKE_SCRIPT=/hub/share/sbtools/apps/cgir_tools/cgmake
export MAKE_SCRIPT=/hub/share/sbtools/bin/glnxa64/sbmake

#############################################################
#
# The sync-and-build script expects these in the environment:

### sbsyncmaster scripts and logs (echoed verbatim below because cron does no symbol substitution)
export SYNCMASTERDIR=/jyates/.sbsyncmaster

### Where sbsyncmaster should create latest_pass areas
export LATESTPASSDIR=/ws

### Information for git.
export GIT_USER_NAME=jyates
export GIT_USER_EMAIL=jyates@mathworks.com

# Now run the actual script
${SYNCMASTERDIR}/sync-and-build
