from functools import reduce
from operator import mul
from numpy import roots
import sys


def grid(roots, n, zoom=1, iterations=64):
    points = [(x / zoom, y / zoom) for x in range(-n, n + 1) for y in range(-n, n + 1)]
    equation = polynomial(*roots)
    derivative = differentiate(*roots)
    result = {}
    for p in points:
        result[p] = solve(complex(*map(float, p)), roots, equation, derivative, iterations)
    return result


def solve(x, roots, equation, derivative, iterations):
    for _ in range(iterations):
        try:
            x -= equation(x) / derivative(x)
        except ArithmeticError:
            break
    return sorted([(abs(x - roots[i]), i) for i in range(len(roots))], key=lambda p: p[0])[0][1]


def differentiate(*roots):
    def f(x):
        result = [[] for _ in range(len(roots))]
        for (i, _) in enumerate(roots):
            for (j, v) in enumerate(roots):
                if i != j:
                    result[i].append(x - v)
        return sum(reduce(mul, [1] + i) for i in result)

    return f


def polynomial(*roots):
    return lambda x: reduce(mul, (x - i for i in roots))


try:
    coefficients = [float(c) for c in input('Coefficients: ').split()]
    size = int(input('Grid Size: '))
    zoom = float(input('Zoom Factor: '))
    roots = roots(coefficients)
    iterations = int(input('Iterations: '))
except:
    print('error')
    exit()

image = {}
sys.stdout = open('output.txt', 'w')

for ((x, y), v) in grid(roots, size, zoom=zoom, iterations=iterations).items():
    if y not in image:
        image[y] = {}
    image[y][x] = v

for (_, i) in sorted(image.items(), key=lambda p: -p[0]):
    for (_, j) in sorted(i.items(), key=lambda p: p[0]):
        print(j, end=' ')
    print()
