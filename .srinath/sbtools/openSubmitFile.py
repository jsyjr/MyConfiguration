#!/usr/bin/env python
import sys
import os
from os import path
from subprocess import call

from sbtools import *

submitFile = getSubmitFile()
if not submitFile:
    print 'Cannot find submit file. Create it first.'
    sys.exit(1)

call(['gvim', submitFile])
