#!/bin/dash

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
export SYNCMASTERDIR=${HOME}/.sbsyncmaster

### Where sbsyncmaster should create latest_pass areas
#export CLUSTERS_ROOT=$( /usr/bin/cat ${HOME}/.sbmx/ROOT )/.cluster
export CLUSTERS_ROOT=/ws/.cluster
export CLUSTERDIR=${CLUSTERS_ROOT}/${CLUSTER}
export CLUSTER_PATH=${CLUSTERDIR}/${CLUSTER}.latest_pass
export LOGDIR=${CLUSTERDIR}/logs


### Information for git.
export GITDIR=${CLUSTERDIR}/git
#export GIT_USER_NAME=$( /usr/bin/cat ${HOME}/.sbmx/GIT_USER_NAME )
#export GIT_USER_EMAIL=$( /usr/bin/cat ${HOME}/.sbmx/GIT_USER_EMAIL )
export GIT_USER_NAME=jyates
export GIT_USER_EMAIL=jyates@mathworks.com

# Now run the actual script
${SYNCMASTERDIR}/sync-and-build
