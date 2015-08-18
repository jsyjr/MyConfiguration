#!/usr/bin/env python

from ListSearch import listOrSearchFiles, Finder

if __name__ == "__main__":
    print "\n".join(listOrSearchFiles(1, Finder))
