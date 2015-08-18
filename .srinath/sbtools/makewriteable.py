#!/usr/bin/env python

import sys
from os import path
import os
from sbtools import getRootDir
import re

def main():
    rootDir = getRootDir()
    if not rootDir:
        print "No battree found above this directory. Bye"
        sys.exit(1)
    
    if len(sys.argv) > 1:
        submitFile = sys.argv[1]
    else:
        submitListFile = path.join(rootDir, 'submitList')
        if not path.exists(submitListFile):
            print 'File [%s] not found. Bye' % submitListFile
            sys.exit(1)

        submitFile = open(submitListFile).read().strip()

    if not path.exists(submitFile):
        print 'File [%s] not found' % submitFile
        sys.exit(1)

    fre = re.compile(r'matlab/\S+')
    flist = open(submitFile).read().splitlines()
    for line in flist:
        m = fre.search(line)
        if not m:
            continue

        fname = m.group(0)
        if path.exists(fname):
            os.chmod(fname, 0666)

if __name__ == "__main__":
    main()
