#!/usr/bin/python2.6

import random
from operator import mul
from functools import reduce
import sys

random.seed()

INTS = set([1,2,3,4,5,6,7,8,9])

def prod_or_sum(l):
    if random.randint(0,1):
        return reduce(mul, l, 1)
    return sum(l)

def nth_head(n, sqr):
    return prod_or_sum([l[n] for l in sqr])

def make_col_headers(sqr):
    return [0] + [nth_head(i,sqr) for i in range(len(sqr))]

def get_col(sqr, i):
    return [row[i] for row in sqr]

def make_square():
    sqrsize = int(sys.argv[1])
    #int(input("Pick a square size: "))
    assert sqrsize <= 9

    sqr = []
    i = 0
    while len(sqr) < sqrsize:
        row = []
        j = 0
        while len(row) < sqrsize:
            if i > 0 and i == j:
                row.append(sqr[0][0])
            else:
                row.append(random.choice(list(INTS - set(row) - set(get_col(sqr,j)))))
            j += 1
        sqr.append(row)
        i += 1

    headers = make_col_headers(sqr)

    for row in sqr:
        row.insert(0, prod_or_sum(row))

    sqr.insert(0,headers)

    return sqr

if __name__ == "__main__":
    print(make_square())
