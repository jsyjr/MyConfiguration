from os import path
import os

def searchUpFor(fname):
    cwd = os.getcwd()
    while 1:
        if path.exists(path.join(cwd, fname)):
            return path.join(cwd, fname)

        (cwd, tail) = path.split(cwd)
        if not tail:
            return None
