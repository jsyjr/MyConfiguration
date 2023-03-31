# -*-gdb-script-*-

# Note: my ~/.sbtools/sb.cfg specifies -no-debug-backing-stores

source /mathworks/hub/share/sbtools/.gdbinit
source /mathworks/inside/files/dev/cda/codegen/cgir/tools/.gdbinit

define dr
  print $arg0.toString(RS::RangeDisplay::DisplayFormat::FULL)
end

define gtf
  run --gtest_filter=$arg0
end

define ral
  shell rm -rf /tmp/cg_debug_printf.txt
  set CG::analysis::range::rangeAnalysisLoggingEnabled = 1
end

define ralid
  shell rm -rf /tmp/cg_debug_printf.txt
  set CG::analysis::range::rangeAnalysisLoggingEnabled = 1
  set CG::analysis::range::rangeAnalysisLogObjIdsEnabled = 1
  set RS::rangeAnalysisLogRvrIdsEnabled = 1
end

# set auto-solib-add on
set history save off
set non-stop off

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


# Don't display the thread creation/deletion/switch messages
# This doesn't work on MACI, so comment it out when debuggin on MACI
set print thread-events off

# Seem to get set on so reset to our desired state
set print static-members off

# exit

define hint
    echo \n
    echo Conditional BP:\n
    echo   condition <BP> cg_pps_contains(<SYM>, "<PATTERN>")\n
    echo \n
    echo Print array values:\n
    echo   pv <ARRAY> <FIRST> <LAST>  (e.g. pv arr 0 2)\n
    echo \n
    echo Auto symbol loading:\n
    echo   set auto-solib-add on|off\n
    echo \n
    echo Load symbols:\n
    echo   sb-load-stack <FRAME_NUM> | all\n
    echo \n
end


# Avoid loading symbols from most libraries
#set auto-solib-add off

# But do load these
sb-auto-load-libs cgir_
sharedlibrary libmwcg_ir.so
sharedlibrary libmweml.so
sharedlibrary libmwrange_services.so
# sharedlibrary libmwrtw_core.so


echo \n
echo ===================================================================\n
echo \n
echo Running a unittest?  DO NOT SETUP BREAKPOINTS YET.
echo \n
echo For much faster loading do the following:\n
echo \n
echo (gdb) unittest\n
echo (gdb) run [ --gtest_filter=? ]\n
echo \n
echo Once your unittest loads its first shared library GDB will break,\n
echo perform some bookkeeping and then display its (gdb) prompt.\n
echo \n
echo At that you should setup your breakpoints.
echo \n

# THIS is breakpoint #1
segv

define unittest
  set auto-solib-add on
  catch load
  commands
    silent
    delete breakpoint 1
    delete breakpoint 2
    caf
    echo \n
    echo ===================================================================\n
    echo Caught first shared library load event. Setup your breakpoints now.\n
    echo Once your breakpoints are in-place issue a 'continue' command.\n
    echo \n
  end
end
