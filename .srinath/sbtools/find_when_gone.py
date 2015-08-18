#!/usr/bin/env python

import re
import time
from commands import getoutput
import sys

if len(sys.argv) != 3:
    print >> sys.stderr, "Usage: %s FILENAME SEARCHTERM" % sys.argv[0]
    sys.exit(1)

fname = sys.argv[1]
searchStr = sys.argv[2]

LOG_RE = re.compile(r'revision (?P<rev>\S*).*date:\s*(?P<date>.*);\s*author:\s*(?P<author>\S*);.*', re.M | re.S)

cvslog = getoutput('sbm mlog %s' % fname)
marker = '------------------------------'
raw_logs  = cvslog.split(marker)

# revision 1.327.4.114 ( branch A; rev 114 )
# date: 2010/08/23 22:13:36;  author: batserve;  state: Exp;  lines: +280 -23
# 2010/08/13  1.327.6.425  batserve
#   2010/08/02  1.327.6.424.2.1  skapoor
#     Saved after merging with 1.327.6.422.4.3 (dev_djia_sb_Aslrtw100)
#   2010/08/02  1.327.6.424.2.2  skapoor
#     Saved after merging with 1.327.6.423.6.2 (dev_djia_sb_Aslrtw10)
#   Accepted job 140527 in Aslrtw
# Accepted job 49006b in A

class Rev:
    def __init__(self, fname, full, m):
        self.fname = fname
        self.full = full
        self.rev = m.group('rev')
        self.date = time.strptime(m.group('date'), '%Y/%m/%d %H:%M:%S')
        self.author = m.group('author')

    def contains(self, searchStr):
        fileRev = getoutput('sbm mget -p -r %s %s' % (self.rev, self.fname))
        return searchStr in fileRev

logs = []
for log in raw_logs:
    m = LOG_RE.search(log)
    if m:
        if m.group('author') != 'batserve':
            logs.append(Rev(fname, log, m))

# sort by date
logs.sort(key = lambda log: log.date)

print 'Searching amongst %d logs' % len(raw_logs)
print 'Found %d non-batserve logs' % len(logs)

hi = len(logs)-1
lo = 0

bisection = True
if bisection:
    if logs[hi].contains(searchStr):
        print 'The search term is still in the latest revision. Not disappeared at all!' % logs[lo].rev
        sys.exit(0)

    while hi - lo > 1:
        mid = (hi + lo)/2
        print 'Searching in revision %s...' % logs[mid].rev, 
        if logs[mid].contains(searchStr):
            # go back in time.
            print 'found it!'
            lo = mid
        else:
            print 'did not find it!'
            hi = mid

    print 'Search term first disappeared in rev %s' % logs[hi].rev
    print 'Complete log message for that revision: '
    print logs[hi].full

else:
    lastFound = False
    rev = 0
    while rev <= hi:
        thisHasIt = logs[rev].contains(searchStr)
        if thisHasIt and (not lastFound):
            print 'The search term appeared in %s' % logs[rev].rev
        elif (not thisHasIt) and lastFound:
            print 'The search term disappeared in %s' % logs[rev].rev

        lastFound = thisHasIt

        rev += 1
