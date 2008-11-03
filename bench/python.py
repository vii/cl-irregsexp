#! /usr/bin/python

import re
import time

buf = open("test-data").read()

r = re.compile("indecipherable|undecipherable")

def find_it(buf):
    global r
    return r.search(buf).start()

pos = find_it(buf)

start=time.clock()

for i in range(1000):
    assert find_it(buf)==pos

print pos,time.clock()-start
