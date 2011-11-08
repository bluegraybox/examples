#!/usr/bin/python

import sys

max = 100
if len(sys.argv) > 1:
    max = int(sys.argv[1])

for x in range(1,max+1):
    if ((x % 5) == 0) and ((x % 3) == 0):
        print "fizzbuzz"
    elif (x % 3) == 0:
        print "fizz"
    elif ((x % 5) == 0):
        print "buzz"
    else:
        print x

