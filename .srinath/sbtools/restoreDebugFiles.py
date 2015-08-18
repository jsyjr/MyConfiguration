#!/usr/bin/env python

from sbtools import getRootDir
import os
from os import path

def restoreDebugFiles():
    rootDir = getRootDir()
    if not rootDir:
        print 'Not in a sandbox!'
        return

    os.chdir(rootDir)

    if path.isdir('matlab/bin/glnxa64'):
        os.chdir('matlab/bin/glnxa64')
    elif path.isdir('matlab/bin/win64'):
        os.chdir('matlab/bin/win64')

    os.system('mv -u dbg-bak/* ./')

if __name__ == "__main__":
    restoreDebugFiles()
