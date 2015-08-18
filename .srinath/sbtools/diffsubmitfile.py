#!/usr/bin/env python

import sys
import os
from os import path
import re
from sbtools import *

def diffSubmitFile(submitFile, dir1, dir2):
    if not path.exists(path.join(dir1, 'battree')) or not path.exists(path.join(dir2, 'battree')):
        print 'battree not found in one of the directories'
        sys.exit(0)

    os.chdir(dir1)
    flist = open(submitFile).read().splitlines()

    for fname in flist:
        if not 'matlab/' in fname:
            continue

        m = re.search(r'matlab/\S+', fname)
        if not m:
            continue

        fname = m.group(0)

        if not path.exists(path.join(dir1, fname)) and not path.exists(path.join(dir2, fname)):
            continue
        
        if not path.exists(path.join(dir2,fname)):
            print 'Only in %(dir1)s: %(fname)s' % locals()
        elif not path.exists(path.join(dir1,fname)):
            print 'Only in %(dir2)s: %(fname)s' % locals()
        else:
            diffcmd = "diff %(dir1)s/%(fname)s %(dir2)s/%(fname)s" % locals()
            diff = getoutput(diffcmd.split())
            if diff:
                print 'Files %(dir1)s/%(fname)s and %(dir2)s/%(fname)s differ' % locals()

    sys.exit(1)

if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()

    parser.add_option('-f', '--file', dest="submitFile")
    parser.add_option('-x', '--exclude', dest="exclude")
    (opts, args) = parser.parse_args()

    sbrootDir = getRootDir()
    if not opts.submitFile:
        submitList = path.join(sbrootDir, 'submitList')
        if path.exists(submitList):
            opts.submitFile = open(submitList).read().splitlines()[0]

        if not path.exists(opts.submitFile):
            print 'Please use the -f FILENAME to specify a submit file location'

    if len(args) == 2:
        dir1 = args[0]
        dir2 = args[1]
    elif len(args) == 1:
        dir1 = sbrootDir
        dir2 = args[0]
    else:
        print 'Usage: %s [dir1] dir2' % (sys.argv[0])
        sys.exit(0)

    diffSubmitFile(opts.submitFile, dir1, dir2)
