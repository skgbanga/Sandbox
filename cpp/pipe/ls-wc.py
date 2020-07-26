#!/usr/bin/python3

import os
import sys

r, w = os.pipe()

cpid1 = os.fork()
if cpid1 == 0:
    os.close(r)
    os.dup2(w, sys.stdout.fileno())
    os.close(w)

    os.execl("/usr/bin/ls", "/usr/bin/ls")

cpid2 = os.fork()
if cpid2 == 0:
    os.close(w)
    os.dup2(r, sys.stdin.fileno())
    os.close(r)

    os.execl("/usr/bin/wc", "/usr/bin/wc", "-l")

os.close(w)
os.close(r)

os.waitpid(cpid1, 0)
os.waitpid(cpid2, 0)

# Another way of accomplishing the above
# r = os.popen("/usr/bin/ls", "r")
# w = os.popen("/usr/bin/wc -l", "w")
# w.write(r.read())
# r.close()
# w.close()
