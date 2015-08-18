#!/usr/bin/env python

import sys
import re

fileName = sys.argv[1]
warnFile = sys.argv[2]

origLines = open(fileName).read().splitlines()
warnings = open(warnFile).read().splitlines()

PAT_UNUSED_VAR = re.compile(r"(?P<file>[\w\.]+)\|(?P<lineNum>\d+).*\| warning: unused parameter '(?P<varName>node|nlhs|plhs|prhs)'")

doneSet = set()

for warn in warnings:
    m = PAT_UNUSED_VAR.match(warn)
    if not m:
        continue

    lineNum = int(m.group('lineNum')) - 1
    varName = m.group('varName')
    if (varName, lineNum) in doneSet:
        continue

    doneSet.add((varName, lineNum))

    origLine = origLines[lineNum]
    newLine = re.sub(r'(\s*)(%s)' % varName, r' /*\2*/', origLine)

    origLines[lineNum] = newLine

print '\n'.join(origLines)
