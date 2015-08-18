#!/usr/bin/env python

import sys
from optparse import OptionParser
from ListSearch import listOrSearchFiles, Lister, TagLister

parser = OptionParser()
parser.add_option("-p", "--only-in-proj", dest="onlyInProj", help="search in project only", action="store_true", default=False)
parser.add_option("-t", "--tags", dest="listTags", help="list tags", action="store_true", default=False)

(options, args) = parser.parse_args()
klass = None
if options.listTags:
    klass = TagLister
else:
    klass = Lister

print "\n".join(listOrSearchFiles(options.onlyInProj, klass))

