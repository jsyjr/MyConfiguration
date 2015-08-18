#!/usr/bin/env python

import re
import sys
import os.path

INC_PATHS = r'''
.
cdr
explr
fsm
gpobj
include
messages
sf_das
udd
undo
utils
export
export/stateflow
'''.split()

INC_PATTERN = re.compile(r'#include "(\S+\.hpp)"')

pwd = os.getcwd()

def printIncludes(fileName, visited=set(), depth=0, stack=[]):
    txt = ''
    fullName = ''
    for path in INC_PATHS:
        fullName = os.path.normpath(os.path.join(path, fileName))
        if os.path.exists(fullName):
            if fullName in visited:
                return

            print ' -->  '.join(stack + [fullName])
            visited.add(fullName)
            txt = open(fullName).read()
            break

    for match in INC_PATTERN.finditer(txt):
        printIncludes(match.group(1), visited, depth+1, stack+[fullName])

printIncludes(sys.argv[1])
