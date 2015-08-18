#!/usr/bin/env python
import sys
import os
from os import path
from subprocess import call
from glob import glob

from sbtools import *

rootDir = getRootDir()
if not rootDir:
    print 'Not in a sandbox'
    sys.exit(1)

rootDirBaseName = path.basename(rootDir)

clusterDestinationFile = path.join(rootDir, '.clusterdestination')
if path.isfile(clusterDestinationFile):
    clusterDestination = open(clusterDestinationFile).read().strip()
else:
    clusterDestination = 'UNKNOWN'

prefix = sys.argv[1]
dirName = path.join('/sandbox', os.getenv('USER'), 'submissions')

if not path.isdir(dirName):
    print 'Directory %s not found for storing submission files.' % dirName
    sys.exit(1)

filesWithPrefix = glob(path.join(dirName, prefix + '_*.txt'))

nums = []
for fileName in filesWithPrefix:
    m = re.search(prefix + r'_(\d+)', fileName)
    if m:
        nums.append(int(m.group(1)))

nextNum = max(nums) + 1 if nums else 1

newFileName = '%s_%02d.txt' % (prefix, nextNum)
newFileName = path.join(dirName, newFileName)

submitListFile = path.join(rootDir, 'submitList')
with open(submitListFile, 'w') as f:
    f.write(newFileName + '\n')


user = os.getenv('USER')

TEMPLATE = r'''# Component        : Stateflow
# Sandbox location : %(rootDir)s
# Submission for   : %(clusterDestination)s
#
# Description:
#   Brief description
#
# Documentation impact:
#   None
#
# QE items:
#   Added automated test.
#
# Type of change:
#   Bug/Task/Enhancement
#
-nowrap
-CJ "<a href='http://%(user)s-deb7-64/sandbox/%(user)s/sbruntests/%(rootDirBaseName)s/sbtest/sbscanlog_results.html'>sbruntests log</a>"
-subject "Enter new subject"

''' % locals()

with open(newFileName, 'w') as f:
    f.write(TEMPLATE)

