#!/usr/bin/env python

from commands import getoutput
import os

allSbs = getoutput('mw -using Bstateflow sbs list {}'.format(os.getenv('USER')))
for sb in allSbs.splitlines():
    sb = sb.strip()
    matlabBinary = os.path.join(sb, 'matlab/bin/glnxa64/MATLAB')
    if not os.path.isfile(matlabBinary):
        print('*** Discarding dead sandbox: {}'.format(sb))
        os.system('mw -using Bstateflow sbs discard -f {}'.format(sb))
