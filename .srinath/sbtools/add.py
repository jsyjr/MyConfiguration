#!/usr/bin/env python
import sys
import os
from os import path

from sbtools import *
import re
from subprocess import Popen, PIPE

RE_VIM_SWAP_FILE = re.compile(r'.*\.sw.')
RE_FILE_PATTERN = re.compile(r'(-\w+) (\S+)?')

def p4_filelog(filename):
    [out, err] = Popen(['p4', 'filelog', filename], stdout=PIPE, stderr=PIPE).communicate()
    return (out, err)

def addToSubmitList(filename):
    cmd = ''
    m = RE_FILE_PATTERN.match(filename)
    if m:
        cmd = m.group(1)
        filename = m.group(2)

    if RE_VIM_SWAP_FILE.match(filename):
        return

    if path.isdir(filename):
        files = getoutput(['sbedits', '-l', '-d', filename]).splitlines()
        for f in files:
            addToSubmitList(f)
        return

    [out, err] = p4_filelog(filename)

    if 'not on client' in err:
        sbrootDir = getRootDir()
        filename = path.join(sbrootDir, filename)
        [out, err] = p4_filelog(filename)
        
    if 'not on client' in err:
        os.system('p4 add %s' % filename)
    elif cmd == '-d':
        os.system('p4 delete %s' % filename)
    else:
        os.system('p4 edit %s' % filename)

if __name__ == "__main__":
    os.environ['PWD'] = os.path.abspath('.')
    for arg in sys.argv[1:]:
        addToSubmitList(arg.strip())
