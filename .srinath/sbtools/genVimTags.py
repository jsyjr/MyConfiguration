#!/usr/bin/env python

from getProjSettings import getProjSettings
from sbtools import getRootDir, getRelPathTo
from threading import Thread
import subprocess
import os
from os import path
import sys

class TagCreator(Thread):
    def __init__(self, path, pattern, extraArgs):
        Thread.__init__(self)

        self.path = path
        self.pattern = pattern
        self.extraArgs = extraArgs
        self.result = ''

    def run(self):
        (curdir, tail) = path.split(sys.argv[0])
        genDirTags = path.join(curdir, 'genDirTags.py')
        subprocess.call(['python', genDirTags, self.path, self.pattern] + self.extraArgs.split())

def genVimTags(fname):
    rootDir = getRootDir()

    if fname != '' and (rootDir not in fname):
        print 'Current file [%s] not in a project' % fname
        return

    fname = getRelPathTo(fname)

    soln = getProjSettings()
    soln.setRootDir(rootDir)

    if not soln:
        print 'Project specification not found'
        return
    
    os.chdir(rootDir)

    threads = []
    for proj in soln.projects:
        # If a filename is specified, then only generate tags for the
        # project it belongs to.

        if (not fname) or proj.includesFile(fname):
            for inc in proj.includes:
                if path.isdir(inc['path']):
                    th = TagCreator(inc['path'], inc['pattern'], '-f %s' % inc['tagsFile'])
                    th.start()
                    threads += [th]

            for exp in proj.exports:
                if path.isdir(exp['path']):
                    th = TagCreator(exp['path'], exp['pattern'], '--c++-kinds=+p --line-directives --excmd=number -f %s' % exp['tagsFile'])
                    th.start()
                    threads += [th]

    for th in threads:
        th.join()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        fname = path.abspath(sys.argv[1])
    else:
        fname = ''

    genVimTags(fname)
