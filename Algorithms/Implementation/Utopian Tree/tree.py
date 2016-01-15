#!/usr/bin/env python2
from itertools import repeat

def grow(cycles):
    height = 1
    for c in range(cycles):
        if (c & 1 == 0): height *= 2
        else:            height += 1
    return height

def main():
    t = int(raw_input())
    for _ in repeat(None,t):
        n = int(raw_input())
        print grow(n)
    
main()
