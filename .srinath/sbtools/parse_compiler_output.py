import re

lines = open('/tmp/log.txt', 'r').read().splitlines()

RE_INCLUDED_FROM = re.compile(r'(In file included from|\s+from) ([^:]+):(\d+)(:(\d+))?')
RE_BASIC_MSG = re.compile(r'([^: ]+):')
for line in lines:
    m = RE_INCLUDED_FROM.match(line)
    if m:
        col = ''
        if m.group(5):
            col = ':%s' % m.group(5)
        print '%s:%s%s: %s' % (m.group(2), m.group(3), col, line)

    elif RE_BASIC_MSG.match(line):
        print line


