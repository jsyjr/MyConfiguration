#!/usr/bin/env python
import sys
import os
from os import path

from sbtools import *
import re

WARN_STR_FMT = '''***** WARNING ******

You are trying to submit the following foreign file belonging to %(subsysForFile)s
    %(file)s


Run the following command to get the correct revision into your sandbox
    sbm medit -from %(stickyTag)s -r %(workingRev)s %(file)s


Follow the procedure documented below for more information:
    http://inside.mathworks.com/wiki/Foreign-File_Change_Requirements

***** END WARNING *****
'''

RE_ASLRTW_COMMON_FILES = re.compile(r'matlab/(toolbox/stateflow|src/cg_ir|src/cgir|test/toolbox/stateflow|test/tools/sfeml)')
RE_VIM_SWAP_FILE = re.compile(r'.*\.sw.')
RE_FILE_PATTERN = re.compile(r'(-\w+ )(\S+)?')

def addToSubmitList(file):
    cmd = ''
    m = RE_FILE_PATTERN.match(file)
    if m:
        cmd = m.group(1)
        file = m.group(2)

    submitFile = getSubmitFile()
    if not submitFile:
        print 'Cannot find submit file. Create it first.'
        sys.exit(1)

    if RE_VIM_SWAP_FILE.match(file):
        return

    if cmd != '-d ' and not path.exists(file):
        sbrootDir = getRootDir()
        file2 = path.join(sbrootDir, file)
        if not path.exists(file2):
            print 'File [%s] or [%s] not found' % (file, file2)
            return
        else:
            file = file2

    if path.isdir(file):
        files = getoutput(['sbedits', '-l', '-d', file]).splitlines()
        for f in files:
            addToSubmitList(f)
        return

    if cmd != '-d ':
        origFile = file
        file = getRelPathTo(file)

    if file in open(submitFile).read():
        print 'Already submitted: %s' % file
        return

    print 'Adding [%s] to [%s]' % (file, submitFile)
    
    warnStr = ''
    if not RE_ASLRTW_COMMON_FILES.match(file):
        subsysForFile = getoutput(['sbm','msubsys', file]).strip()
        if subsysForFile != 'Aslrtw':
            status = getoutput(['sbm', 'mstatus', '-v', origFile])
            m = re.search(r'Working revision:\s+(\S+).*Sticky Tag:\s+(\S+)', status, re.DOTALL)
            if m:
                workingRev = m.group(1)
                stickyTag = m.group(2)

                warnStr = WARN_STR_FMT % locals()
                print warnStr
                warnStr += '-foreign '

    if cmd != '-d ':
        os.system('chmod +w %s' % origFile)
    open(submitFile, 'a').write('%s%s%s\n' % (warnStr, cmd, file))

if __name__ == "__main__":
    for arg in sys.argv[1:]:
        addToSubmitList(arg.strip())
