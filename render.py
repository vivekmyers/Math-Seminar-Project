import sys
from numpy import zeros, uint8
from scipy.misc import imsave
from random import random

grid = []

for i in sys.stdin:
    tmp = []
    grid.append(tmp)
    for j in i.split():
        tmp.append(j)


def new_color():
    return [int(random() * 256) for _ in range(3)]


colors = {}
image = zeros([len(grid), len(grid[0]), 3], dtype=uint8)
for x in range(len(grid)):
    for y in range(len(grid[0])):
        pixel = eval(grid[x][y])
        symbol = pixel[0]
        if symbol not in colors:
            colors[symbol] = new_color()
        image[x][y] = [c * ((pixel[1] / float(sys.argv[1])) ** 2) for c in colors[symbol]]

imsave("output.png", image)
