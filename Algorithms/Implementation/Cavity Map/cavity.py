#!/bin/python3
from itertools import repeat
from functools import partial
from operator import lt

def solve(grid):
    for i in range(1,len(grid)-1):
        for j in range(1,len(grid[0])-1):
            if all([x < grid[i][j] for x in [grid[i+1][j], \
					grid[i][j+1], grid[i-1][j], grid[i][j-1]]]):
                   grid[i][j] = 'X'
    return('\n'.join(map(''.join, grid)))

def main():
    n = int(input())
    grid = []
    for _ in repeat(None, n):
        grid.append(list(input()))
    
    print(solve(grid))

main()
