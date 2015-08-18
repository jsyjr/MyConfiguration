#!/usr/bin/env python

import sys
sys.path.append('/sandbox/savadhan/sa-pyqttree')
from TreeModel import viewTree

from os import path
import os
import tempfile
from commands import getoutput

import re
PAT_FILENAME = re.compile(r' \* (\.+) (\S+)')

refDir = os.getcwd() # '/local-ssd/savadhan/Asfstage/matlab/toolbox/stateflow/src/stateflow/'

fileSizesMap = {}
def getFileSize(fileName):
    if fileName not in fileSizesMap:
        statinfo = os.stat(path.join(refDir, fileName))
        fileSizesMap[fileName] = statinfo.st_size

    return fileSizesMap[fileName]

class Node:
    def __init__(self, fileName=''):
        self.fileName = fileName
        self.children = []
        self.parent = None
        self.idx = 0

        self.selfSize = getFileSize(self.fileName)
        self.selfPlusChildSize = 0

    def __str__(self):
        return self.fileName

    def __repr__(self):
        return 'Node(%s)' % self.fileName

    @property
    def data(self):
        return [self.fileName, self.idx, self.selfSize, self.selfPlusChildSize]

    def addChild(self, child):
        child.parent = self
        child.idx = len(self.children)
        self.children.append(child)

    def propagateSizes(self):
        self.selfPlusChildSize = self.selfSize
        for ch in self.children:
            ch.propagateSizes()
            self.selfPlusChildSize += ch.selfPlusChildSize


PAT_SBCC_OUTPUT_FILE = re.compile(r'Created: (\S+)')
def getSbccOutputFileName(fileName):
    tempdir = tempfile.mkdtemp()
    output = getoutput('sbcc -mixed@DEFAULT -work-dir %s -E %s' % (tempdir, fileName))
    return PAT_SBCC_OUTPUT_FILE.search(output).group(1)

def printTree(sbccOutputFileName):
    root = Node()
    stack = [root]

    for line in open(sbccOutputFileName):
        m = PAT_FILENAME.match(line)
        if m:
            depth = len(m.group(1)) + 1
            fileName = m.group(2)

            if depth <= len(stack):
                stack[(depth-1):] = []

            n = Node(fileName)
            stack[-1].addChild(n)

            if depth > len(stack):
                stack.append(n)

    root.propagateSizes()
    viewTree(root, 'root')

def main():
    fileName = sys.argv[1]
    print 'Running sbcc to get includes'
    sbccOutputFileName = getSbccOutputFileName(fileName)
    print 'Analysing includes for sizes'
    printTree(sbccOutputFileName)

if __name__ == "__main__":
    main()
