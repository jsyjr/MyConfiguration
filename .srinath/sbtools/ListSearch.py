#!/usr/bin/env python

from getProjSettings import getProjSettings
from sbtools import getRootDir
import os
from os import path
from threading import Thread
from subprocess import Popen, PIPE
import sys
from sbtools import *

class Lister(Thread):
    def __init__(self, rootDir, include):
        Thread.__init__(self)

        self.path = os.path.join(rootDir, include['path'])
        self.pattern = ' -or -name '.join(include['pattern'].split())
        self.result = ''

    def run(self):
        self.result = getoutput(['find', self.path, '-name'] + self.pattern.split())

class Finder(Thread):
    def __init__(self, rootDir, include):
        Thread.__init__(self)

        self.path = os.path.join(rootDir, include['path'])
        self.pattern = ' -or -name '.join(include['pattern'].split())
        self.result = ''

    def run(self):
        p1 = Popen([getToolPath('find'), self.path, '-name'] + self.pattern.split(), stdout=PIPE)
        p2 = Popen(['python', getScriptPath('findInFiles.py'), '-nH'] + sys.argv[1:], stdin=p1.stdout, stdout=PIPE)
        self.result = p2.communicate()[0]

class TagLister(Thread):
    def __init__(self, rootDir, include):
        Thread.__init__(self)
        self.path = os.path.join(rootDir, include['path'], include['tagsFile'])

    def run(self):
        tags = open(self.path).read().splitlines()
        for idx, tag in enumerate(tags):
            if not tag.startswith('!'):
                break

        tags = tags[idx+1:]
        self.result = "\n".join(tags)


def listOrSearchFiles(searchOnlyProj, Runner):
    rootDir = getRootDir()

    soln = getProjSettings()
    soln.setRootDir(rootDir)

    # The current directory decides the "current project"
    cwd = os.getcwd()

    threads = []
    for proj in soln.projects:

        # Figure out if the current directory is in the current project
        if (not searchOnlyProj) or proj.includesFile(cwd):
            for inc in proj.includes:
                th = Runner(rootDir, inc)
                th.start()

                threads += [th]

    result = ''
    for th in threads:
        th.join()
        if len(th.result) > 5:
            result += th.result

    return result.split("\n")
