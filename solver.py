from functools import reduce
from math import sqrt, exp
from operator import mul
from math import pi
from cmath import exp


# may take a few seconds to run

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


def unity(n):
    return [exp(complex(0, 1.0) * complex(z / n * 2 * pi, 0)) for z in range(n)]


roots = unity(3)
image = {}

for ((x, y), v) in grid(roots, 100, zoom=4).items():
    if y not in image:
        image[y] = {}
    image[y][x] = v

for (_, i) in sorted(image.items(), key=lambda p: -p[0]):
    for (_, j) in sorted(i.items(), key=lambda p: p[0]):
        print({0: "|", 1: "#", 2: ".", 3: " ", 4: "$", 5: "="}[j], end=' ')
    print()
