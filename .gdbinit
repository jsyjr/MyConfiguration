# -*-gdb-script-*-

# $ sb -debug
# (gdb) run -nodesktop -nojvm -r "opengl info"
# >> sfnew ; bdclose all

# set breakpoint pending on
# set unwindonsignal on
# set print pretty on
# set history save
# set print thread-events off

# # No need to break on these signals coming from the JVM.
# handle SIGCONT  nostop noprint
# handle SIGUSR1  nostop noprint
# handle SIGUSR2  nostop noprint
# handle SIGWINCH nostop noprint
# handle SIG38    nostop noprint
# handle SIG39    nostop noprint
# handle SIG62    nostop noprint

# define breaksegv
#   handle SIGSEGV nostop noprint pass
#   break ut_assert
#   break ut_assertstr
#   break assertion_func
#   break mnDebugRuntimeFault
#   break svHandleSignalFaults
#   break fl_diag_terminate
#   break SF::assertion_failed
#   break ThrowAssertion
#   break client_assertion_failed
#   break rtwcg_assertion_failed
# end


# source /mathworks/hub/share/sbtools/.gdbinit
source /sandbox/savadhan/sbtools/mw-gdbscripts/.gdbinit
