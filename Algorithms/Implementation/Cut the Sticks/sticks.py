#!/usr/bin/env python2

def main():
    raw_input()
    sticks = map(int, raw_input().split())
    while sticks:
        print len(sticks)
        cut = min(sticks)
        sticks = [x - cut for x in sticks if x > cut]

main()
