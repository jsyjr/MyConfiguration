#!/usr/bin/env python

from sbtools import *
from os import path
import os
import sys
import re

def pruneSubmitFile(submitFile, archiveDir):
    lines = []

    for line in open(submitFile).read().splitlines():
        m = re.match(r'(-\w+ )?(matlab/\S+)', line)
        if not m:
            lines += [line]
            continue

        action = m.group(1) or ''
        file = m.group(2)
        if not path.exists(file) and not action.startswith('-d'):
            print 'Warning: File %s does not exists' % file
            lines += [line]
            continue

        if not path.exists(path.join(archiveDir, file)):
            # new file in sandbox. Do not remove!
            lines += [line]
            continue

        if not action.startswith('-touch'):
            cmd = 'sbm mdiff %s' % file
            out = getoutput(cmd.split())
            if not out:
                print 'File [%s] reports no diff' % file
                # make it not writeable.
                os.chmod(file, 0444)
            else:
                lines += [line]
        else:
            lines += [line]


    open(submitFile, 'w').write('\n'.join(lines) + '\n')

if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser()

    parser.add_option('-F', '--submitFile', dest="submitFile")
    parser.add_option('', '--archive', dest="archive")
    (opts, args) = parser.parse_args()

    rootDir = getRootDir()
    if not rootDir:
        print 'Not in a sandbox. Bye'
        sys.exit(1)

    os.chdir(rootDir)

    if not opts.submitFile:
        submitList = 'submitList'
        if not path.isfile(submitList):
            print 'Submit list file not found. Specify submit file explicitly'
            sys.exit(1)
        else:
            submitFile = open(submitList).read().splitlines()[0]
    else:
        submitFile = opts.submitFile

    if not path.isfile(submitFile):
        print 'File [%s] not found' % submitFile
        sys.exit(1)

    if not opts.archive:
        archiveDir = getArchivePath()
    else:
        archiveDir = opts.archive

    pruneSubmitFile(submitFile, archiveDir)
