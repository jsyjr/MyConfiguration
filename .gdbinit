 # -*-gdb-script-*-

# I would like to source this file and be done:
#
source /mathworks/inside/files/dev/cda/codegen/cgir/tools/.gdbinit
#
# Unfortunately there seems to be some sort of issue with python that
# I have yet to sort out.  In the interim here is the non-python portion
# the above file:

# shell firefox -remote openFile\("/tmp/foo.html",new-tab\)



# set auto-solib-add on
set history save off
set non-stop off

# handle SIGALRM nostop noprint
segv

define sb
  nosegv
  run -nosplash
end

define sbn
  nosegv
  run -nosplash -nodesktop
end

define sbnj
  segv
  run -nosplash -nodesktop -nojvm
end

define caf
  b client_assertion_failed
  b foundation::core::diag::terminate
end

define bex
  handle SIGSEGV nostop noprint pass
  caf
end


# Don't display the thread creation/deletion/switch messages
# This doesn't work on MACI, so comment it out when debuggin on MACI
set print thread-events off

# Seem to get set on so reset to our desired state
set print static-members off

set auto-solib-add off

catch load
commands
  silent
  delete breakpoint 1
  sharedlibrary libmwcg_ir
#  sharedlibrary libmwcgir_algorithm.so
#  sharedlibrary libmwcgir_analysis.so
#  sharedlibrary libmwcgir_cgel.so
#  sharedlibrary libmwcgir_clair.so
#  sharedlibrary libmwcgir_construct.so
#  sharedlibrary libmwcgir_cpp_emitter.so
#  sharedlibrary libmwcgir_fixpt.so
#  sharedlibrary libmwcgir_gpu.so
#  sharedlibrary libmwcgir_hdl.so
#  sharedlibrary libmwcgir_interp.so
#  sharedlibrary libmwcgir_mi.so
#  sharedlibrary libmwcgir_plc.so
  sharedlibrary libmwcgir_support.so
  sharedlibrary libmwcgir_tests.so
#  sharedlibrary libmwcgir_tfl.so
#  sharedlibrary libmwcgir_vm_rt.so
#  sharedlibrary libmwcgir_vm.so
  sharedlibrary libmwcgir_xform.so
#  sharedlibrary libmweml.so
#  sharedlibrary libmwrtw_core.so
  segv
  echo \n
  echo Caught load shared library event. Now is the time to set breakpoints.\n
  echo \n
end

echo \n


echo \n
echo Do not set breakpoints yet. Issue appropriate run command.  GDB will break at the\n
echo first load shared library event.  At that you should set up any breakpoints, then\n
echo run or\n
echo run --gtest_filter=\n
echo \n
