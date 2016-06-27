# -*-gdb-script-*-

# I would like to source this file and be done:
#
#   source /mathworks/inside/files/dev/cda/codegen/cgir/tools/.gdbinit
#
# Unfortunately there seems to be some sort of issue with python that
# I have yet to sort out.  In the interim here is the non-python portion
# the above file:

source /mathworks/hub/share/sbtools/.gdbinit

define sbnj
run -nodesktop -nosplash -nojvm -r "opengl info"
end

define sbn
run -nodesktop -nosplash -r "opengl info"
end

define segv
handle SIGSEGV stop
end

# Don't display the thread creation/deletion/switch messages
# This doesn't work on MACI, so comment it out when debuggin on MACI
set print thread-events off

set unwindonsignal on
set print pretty on
set history save
set breakpoint pending on
set non-stop off
set print object on
set print static-members off
# handle SIGALRM nostop noprint

define pp
p cg_pp_see_it($arg0)
end
document pp
pretty print any CG_Obj to stdout
end

define ppit
printf "%s\n", cg_pp_see_it($arg0)
end
document ppit
pretty print any CG_Obj to stdout
end

define cgel
printf "%s\n", emitCgel($arg0)._M_dataplus._M_p
end
document cgel
cgel dump any CG_Obj to stdout
end

define dot
p CG::DotCfg::drawCfg( $arg0, "/tmp/foo.dot")
#shell /usr/bin/dotty /tmp/foo.dot&
shell sbzgrviewer /tmp/foo.dot&
end
document dot
dump a dot representation of a function to /tmp/foo.dot and render in dotty
end

define bex
breaksegv
#b pir::AssertionHelper::failAssertion
b client_assertion_failed
#b std::__throw_logic_error
#b std::__throw_runtime_error
#b MATLABAssertFcn
end

define segv
handle SIGSEGV print stop
end
document segv
turn on segv processing
end

define nosegv
handle SIGSEGV noprint nostop
end
document segv
ignore segv's while debugging
end

define ppe
p cg_pp_expr_ez($arg0, "/tmp/foo.html")
# shell firefox -remote openFile\("/tmp/foo.html",new-tab\)
shell chromium /tmp/foo.html
end
document ppe
Call cg_pp_expr_ez, placing the output in /tmp/foo.html.
end

define pps
p cg_pp_scope_ez($arg0, "/tmp/foo.html", true)
# shell firefox -remote openFile\("/tmp/foo.html",new-tab\)
shell chromium /tmp/foo.html
end
document pps
Call cg_pp_scope_ez, placing the output in /tmp/foo.html.
end

define ppf
p cg_pp_fcn_ez($arg0, "/tmp/foo.html")
# shell firefox -remote openFile\("/tmp/foo.html",new-tab\)
shell chromium /tmp/foo.html
end
document ppf
Call cg_pp_fcn_ez, placing the output in /tmp/foo.html.
end

define pfs
printf "%s\n", $arg0.c_str()
end


define dumpMATLABStack
call inFullDbgstackFcn(0,(mxArray_tag **)0,0,(mxArray_tag **)0)
call ioFlush()
end

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
# source /sandbox/savadhan/sbtools/mw-gdbscripts/.gdbinit

# source /mathworks/hub/share/sbtools/.gdbinit

# set breakpoint pending on
# set history save
# set print object on
# set print pretty on
# set print static-members off
# set unwindonsignal on

# define segv
# handle SIGSEGV print stop
# end

# define nosegv
# handle SIGSEGV noprint nostop
# end
