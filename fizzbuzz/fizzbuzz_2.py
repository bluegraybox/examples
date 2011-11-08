#!/usr/bin/python


def fizzbuzz(x):
    if ((x % 5) == 0) and ((x % 3) == 0):
        return "fizzbuzz"
    elif (x % 3) == 0:
        return "fizz"
    elif ((x % 5) == 0):
        return "buzz"
    else:
        return x

max = 100
for x in range(1,max+1):
        print fizzbuzz(x)

