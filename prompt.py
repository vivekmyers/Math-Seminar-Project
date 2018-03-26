from numpy import roots

try:
    coefficients = [float(c) for c in input().split()]
    size = int(input())
    zoom = eval(input())
    roots = roots(coefficients)
    iterations = int(input())
    tolerance = input()
except:
    print('error')
    exit()

for root in roots:
    print("({real}):+({imag})".format(real=root.real, imag=root.imag), end=' ')
else:
    print()

print(size)
print(zoom)
print(iterations)
print(tolerance)
