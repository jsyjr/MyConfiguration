#! c:/Programs/Python25/python.exe

import sys
from sbtools import *
from os import path
import os

fname = sys.argv[1]

rootdir = getRootDir()
relpath = getRelPathTo(fname)

(head, tail) = path.split(rootdir)
unixdir = path.join('s:\\', tail)
if not path.exists(unixdir):
    print 'Directory [%s] does not exist' % unixdir
    sys.exit(1)

thispath = path.join(rootdir, relpath)
otherpath = path.join(unixdir, relpath)

os.execl(r'C:\Program Files\Araxis\Araxis Merge\Merge.exe', 'Merge.exe', thispath, otherpath)

