#!/usr/bin/env python

from add import addToSubmitList

dirs = '''matlab/simulink
matlab/stateflow
matlab/rtw
matlab/src/simulink
matlab/src/rtwcg
matlab/src/cg_ir
matlab/src/cgir_xform
matlab/toolbox/stateflow
matlab/test/toolbox/stateflow
matlab/test/tools/sfeml
matlab/resources/Stateflow'''

for dir in dirs.split():
    addToSubmitList(dir)

