#!/usr/bin/env python
import re
from os import path
import sys

PAT_INCLUDE = re.compile(r'^\s*#\s*include (<|")(?P<name>\S+)(>|")', re.M)

allIncludes = []

def resolveInclude(incFile, includes, curDir):
    paths = [curDir] + includes

    for p in paths:
        fullPath = path.join(p, incFile)
        if path.exists(fullPath):
            return fullPath

    return None

def findIncludesInFile(fileName, includes, visited=set(), depth=0):
    if fileName in visited:
        return
    visited.add(fileName)

    if path.isdir(fileName):
        return

    print '%s%s{{{' % ('\t'*depth, fileName)
    
    fileDir = path.dirname(path.abspath(fileName))

    matches = PAT_INCLUDE.finditer(open(fileName).read())
    for m in matches:
        incFile = resolveInclude(m.group('name'), includes, fileDir)
        if incFile:
            findIncludesInFile(incFile, includes, visited, depth+1)
        else:
            print '%s<%s>' % ('\t'*(depth+1), m.group('name'))

    print '%s}}}' % ('\t'*depth,)

def main():
    from optparse import OptionParser

    parser = OptionParser()
    parser.add_option("-c", dest="fileName",
                      help="compile FILE", 
                      metavar="FILE", 
                      nargs=1)
    parser.add_option("-I", dest="includes", action="append",
                      help="include file", default=[])

    for i in range(26):
        ch = chr(ord('a') + i)
        if ch != 'c' and ch != 'h':
            parser.add_option('-%s' % ch)

        upch = chr(ord('A') + i)
        if upch != 'I':
            parser.add_option('-%s' % upch)

    (options, args) = parser.parse_args()

    if not options.fileName:
        print("Provide -c fileName to specify file")
        sys.exit(1)

    findIncludesInFile(options.fileName, options.includes)

if __name__ == "__main__":
    main()

