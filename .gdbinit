# -*-gdb-script-*-

# I would like to source this file and be done:
#
#   source /mathworks/inside/files/dev/cda/codegen/cgir/tools/.gdbinit
#
# Unfortunately there seems to be some sort of issue with python that
# I have yet to sort out.  In the interim here is the non-python portion
# the above file:

# shell firefox -remote openFile\("/tmp/foo.html",new-tab\)

define segv
  handle SIGSEGV print stop
end
document segv
Turn on segv processing
end

define nosegv
  handle SIGSEGV nostop noprint pass
end
document nosegv
Ignore segv's while debugging
end

# run -r "opengl info" -softwareopengl -nosplash

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

# Don't display the thread creation/deletion/switch messages
# This doesn't work on MACI, so comment it out when debuggin on MACI
set print thread-events off

# set auto-solib-add on
set unwindonsignal on
set print pretty on
set print object on
set print static-members off
set history save off
set breakpoint pending on
set non-stop off
# handle SIGALRM nostop noprint
segv

define pp
  p cg_pp_see_it($arg0)
end
document pp
Pretty print any CG_Obj to stdout
end

define ppit
  printf "%s\n", cg_pp_see_it($arg0)
end
document ppit
Pretty print any CG_Obj to stdout
end

define cgel
  printf "%s\n", emitCgel($arg0)._M_dataplus._M_p
end
document cgel
Dump dump for any CG_Obj to stdout
end

define dot
  p CG::DotCfg::drawCfg( $arg0, "/tmp/foo.dot")
  shell sbzgrviewer /tmp/foo.dot&
end
document dot
Dump a dot representation of a function to /tmp/foo.dot and render in sbzgrviewer
end

define caf
  b client_assertion_failed
end

define bex
  handle SIGSEGV nostop noprint pass
  caf
end

define ppe
  p cg_pp_expr_ez($arg0, "/tmp/foo.html")
  shell chromium /tmp/foo.html
end
document ppe
Call cg_pp_expr_ez, placing the output in /tmp/foo.html.
end

define pps
  p cg_pp_scope_ez($arg0, "/tmp/foo.html", true)
  shell chromium /tmp/foo.html
end
document pps
Call cg_pp_scope_ez, placing the output in /tmp/foo.html.
end

define ppf
  p cg_pp_fcn_ez($arg0, "/tmp/foo.html")
  shell chromium /tmp/foo.html
end
document ppf
Call cg_pp_fcn_ez, placing the output in /tmp/foo.html.
end

define pfs
  printf "%s\n", $arg0.c_str()
end


# Ignore most shared library load events
# set auto-solib-add off

# These are the only ones that matter to me (see cg_ir/ez.mk)
define load_sharedlibraries
  sharedlibrary libmwcg_ir.so
  sharedlibrary libmwcgir_algorithm.so
  sharedlibrary libmwcgir_analysis.so
  sharedlibrary libmwcgir_cgel.so
  sharedlibrary libmwcgir_clair.so
  sharedlibrary libmwcgir_construct.so
  sharedlibrary libmwcgir_cpp_emitter.so
  sharedlibrary libmwcgir_fixpt.so
  sharedlibrary libmwcgir_gpu.so
  sharedlibrary libmwcgir_hdl.so
  sharedlibrary libmwcgir_interp.so
  sharedlibrary libmwcgir_mi.so
  sharedlibrary libmwcgir_plc.so
  sharedlibrary libmwcgir_support.so
  sharedlibrary libmwcgir_tests.so
  sharedlibrary libmwcgir_tfl.so
  sharedlibrary libmwcgir_vm_rt.so
  sharedlibrary libmwcgir_vm.so
  sharedlibrary libmwcgir_xform.so
  sharedlibrary libmweml.so
  sharedlibrary libmwrtw_core.so
end

# tb main
# commands
#   silent
#   load_sharedlibraries
#   cont
# end

source /mathworks/hub/share/sbtools/.gdbinit

# Seem to get set on so reset to our desired state
set print static-members off
