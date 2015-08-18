import os
from os import path
import re
from subprocess import Popen, PIPE
import sys

def getRootDir():
    dir = os.getcwd()
    while ( (not path.exists('%s/battree' % dir)) and
            (not path.exists('%s/.vimproj.xml' % dir)) ):
        (dir, child) = path.split(dir)
        if not child:
            return None

    return dir

def getRelPathTo(fileName):
    rootDir = getRootDir()

    if not rootDir:
        return None

    if not path.exists(fileName):
        return None

    p1 = path.abspath(rootDir)
    p2 = path.abspath(fileName)
    return p2[len(p1)+1:]

def getArchivePath():
    out = getoutput('sbver')
    return re.search(r'SyncFrom: (.*)', out).group(1)

def getSubmitFile(sbrootDir=''):
    if not sbrootDir:
        sbrootDir = getRootDir()

    if not sbrootDir:
        return ''

    submitList = path.join(sbrootDir, 'submitList')
    if path.exists(submitList):
        submitFile = open(submitList).read().splitlines()[0]
        if not path.exists(submitFile):
            return ''
        else:
            return submitFile
    else:
        return ''

def getoutput(cmd):
    return Popen(cmd, stdout=PIPE).communicate()[0]

def getScriptPath(scriptName):
    (scriptDir, scriptPath) = path.split(sys.argv[0])
    return path.join(scriptDir, scriptName)

def getToolPath(toolName):
    (scriptDir, scriptPath) = path.split(sys.argv[0])
    return path.join(scriptDir, sys.platform, toolName)

if __name__ == "__main__":
    print getRelPathTo('cdr_transform_driver.cpp')
