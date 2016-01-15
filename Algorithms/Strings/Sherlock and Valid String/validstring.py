#!/usr/bin/env python3
from collections import Counter

def main():
    s = input().strip()
    counts  = [x[1] for x in Counter(s).most_common()]
    highest = counts[0]
    counts  = counts[1:]
    
    changes = 0
    for c in counts:
        if c != highest:
            changes += 1
    if changes == len(counts):
        if all([x == counts[0] for x in counts[1:]]):
            changes = 1
    if changes <= 1:
        print("YES")
    else:
        print("NO")
    
main()
