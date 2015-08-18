#!/usr/bin/env python

from sbtools import getRootDir
import os
from os import path

def removeDebugFiles():
    rootDir = getRootDir()
    if not rootDir:
        print 'Not in a sandbox!'
        return

    os.chdir(rootDir)
    if path.isdir('matlab/bin/glnxa64'):
        dirName = 'matlab/bin/glnxa64'
        ext = 'dbg'
    elif path.isdir('matlab/bin/win64'):
        dirName = 'matlab/bin/win64'
        ext = 'pdb'

    os.chdir(dirName)

    if not os.path.isdir('dbg-bak'):
        os.mkdir('dbg-bak')

    # First move away all dbg files to dbg-bak
    os.system(r'mv -u *.%s dbg-bak/' % ext)

    # First time the dbg-bak was created. Keep some common dbg files
    # which we usually always need.
    def restore(name):
        os.system(r'mv -u dbg-bak/*%s.so.%s ./' % (name, ext))

    # kept here for backwards compatibility
    restore('stateflow')
    restore('sf_sfun')
    restore('sf_runtime')
    restore('cg_ir')
    restore('cgir_*')
    restore('rtwcg')

    # Finally move away libmwsimulink.so.dbg unconditionally. If people
    # want to debug Simulink, they'll hve to restore that manually.
    if os.path.isfile('libmwsimulink.so.dbg'):
        os.system(r'mv -u libmwsimulink.so.dbg dbg-bak/')


if __name__ == "__main__":
    removeDebugFiles()
        
