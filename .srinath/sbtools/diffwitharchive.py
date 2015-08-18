#!/usr/bin/env python

import sys
from sbtools import *
from os import path
import os
import re

fname = sys.argv[1]
rootdir = getRootDir()
relpath = getRelPathTo(fname)

if len(sys.argv) == 3:
    archive = sys.argv[2]
else:
    archive = getArchivePath()

    if not path.exists(archive):
        archive = re.sub(r'/mathworks', r'\\\\mathworks', archive)
        if not path.exists(archive):
            print 'Directory [%s] does not exist' % archive
            sys.exit(1)

thispath = path.join(rootdir, relpath)
otherpath = path.join(archive, relpath)

if 'COMSPEC' in os.environ:
    os.execl(r'C:\Program Files\Araxis\Araxis Merge\Merge.exe', 'Merge.exe', '/NoSplash', '/NoSplashDelay',  thispath, otherpath)
else:
    os.system(r'meld %s %s' % (thispath, otherpath))

