#!/usr/bin/env python

from threading import Thread
from sbtools import getoutput
from os import path
from sbtools import *

class Finder(Thread):
    def __init__(self, files, args):
        Thread.__init__(self)
        self.args = [getToolPath('grep')] + args + files
        self.result = []

    def run(self):
        self.result = getoutput(self.args).strip()

def find(args, allFiles):
    numMaxFilesPerThread = 200
    
    fileGroups = []
    while 1:
        fileGroups += [allFiles[:numMaxFilesPerThread]]
        allFiles[:numMaxFilesPerThread] = []
        if not allFiles:
            break

    threads = []
    for i, fg in enumerate(fileGroups):
        threads += [Finder(fileGroups[i], args)]
        threads[-1].start()

    results = []
    for t in threads:
        t.join()
        if t.result:
            results += [t.result]

    return '\n'.join(results)

if __name__ == "__main__":
    import sys
    allFiles = sys.stdin.read().splitlines()
    print find(sys.argv[1:], allFiles)
