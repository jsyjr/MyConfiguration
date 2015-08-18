#!/usr/bin/env python
import os
import sys
from subprocess import Popen, PIPE, check_call
import tempfile
import shutil

# If its a CDR file, we create a temporary copy of this file, then run
# cdr2m on it and point the results back to the original CDR file.

fileName = sys.argv[1]
(root, ext) = os.path.splitext(fileName)
if ext == '.cdr':
    baseName = os.path.basename(fileName)
    (root, ext) = os.path.splitext(baseName)

    tempDir = tempfile.mkdtemp(suffix="temp_cdr2m_storage")
    shutil.copy(fileName, tempDir)

    fileToConvert = os.path.join(tempDir, baseName)

    check_call(['cdr2m.pl', fileToConvert]) 

    fileToLint = os.path.join(tempDir, root + '.m')
else:
    fileToLint = fileName

P = Popen(["mlint", fileToLint], stdout=PIPE, stderr=PIPE)
cerr = P.stderr
origoutput = cerr.read().strip()
if origoutput:
    print '{{%s}}' % fileName
    print origoutput
