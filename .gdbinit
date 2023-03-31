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

define bex
  handle SIGSEGV nostop noprint pass
  caf
end


# Don't display the thread creation/deletion/switch messages
# This doesn't work on MACI, so comment it out when debuggin on MACI
set print thread-events off

# Seem to get set on so reset to our desired state
set print static-members off


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
set auto-solib-add off

catch load
commands
  silent
  delete breakpoint 1
  sharedlibrary libmwcg_ir.so
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
  sharedlibrary libmweml.so
  sharedlibrary libmwrange_services.so
  skip _dl_fixup _dl_runtime_resolve_xsave patched__dl_lookup_symbol_x

  segv
  caf
  echo \n
  echo Caught load shared library event. Now is the time to set breakpoints.\n
  echo \n

  echo \n
  echo A few cgir libraries have been loaded:\n
  echo  - libmwcg_ir.so\n
  echo  - libmwcgir_cpp_emitter.so\n
  echo  - libmwcgir_support.so\n
  echo  - libmwcgir_tests.so\n
  echo  - libmwcgir_xform.so\n
  echo  - libmweml.so\n
  echo  - libmwrange_services.so\n

  echo \n
  echo Set additional breakpoints, then continue.\n
  echo \n
  echo (gdb)
end

echo \n
echo ===================================================================\n
echo \n
echo Running a unittest?  DO NOT SETUP BREAKPOINTS YET.
echo \n
echo For much faster loading do the following:\n
echo \n
echo Do not set breakpoints yet. Issue appropriate run command.  GDB will break at the\n
echo first load shared library event.  At that you should set up any breakpoints, then\n
echo run or\n
echo run --gtest_filter=\n
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
