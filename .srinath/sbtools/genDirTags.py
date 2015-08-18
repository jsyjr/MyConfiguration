#!/usr/bin/env python

import os
import sys
from os import path
from subprocess import Popen, PIPE
from sbtools import *
from tempfile import mkstemp

dir = sys.argv[1]
pat = sys.argv[2]
args = sys.argv[3:]

tagsFile = 'tags'
(dummy, tmpTagsFile) = mkstemp()
for i in range(1,len(args)):
    if args[i-1] == '-f':
        tagsFile = args[i]
        args[i] = tmpTagsFile

print "tags %(dir)s/%(pat)s -f %(tagsFile)s ..." % locals(),

os.chdir(dir)

pat = " -o ".join(["-name %s " % p for p in pat.split()])

find = getToolPath('find')
ctags = getToolPath('ctags')
ctags_config = getScriptPath('ctags.cnf')

p1cmd = [find] + pat.split()
p1 = Popen(p1cmd, stdout=PIPE)
p2cmd = [ctags] + args + ['--options=%s' % ctags_config,
            '--fields=+iaS', '--extra=+q', '-L', '-']
p2 = Popen(p2cmd, stdin=p1.stdout,
           stdout=PIPE)
p2.communicate()[0]

isDifferent = True
if os.path.exists(tagsFile):
    newTags = open(tmpTagsFile).read()
    oldTags = open(tagsFile).read()
    if newTags == oldTags:
        isDifferent = False
        
if isDifferent:
    os.system('mv -f %(tmpTagsFile)s %(tagsFile)s' % locals())
    os.system('chmod -w %(tagsFile)s' % locals())
else:
    print 'Tags file didnt change...'
    os.system('rm -f %(tmpTagsFile)s' % locals())

os.system('chmod go+r %(tagsFile)s' % locals())
print "done"
