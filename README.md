## Haskell Version
Generates a png representation of the Newton-Raphson convergence of the complex plane toward the roots of a polynomial. Use run.sh to generate the image file, output.png (may take several minutes). The -g, -z, -i, and -c options can be used to specify grid size, zoom, iterations, and contrast, respectively.

Ex.

`./run.sh`

`Coefficients: 1 0 0 -1`

Requires bash, python3, numpy, scipy, and Pillow.

## Python Version

Checkout the python branch (very slow).

## GPU

Requires pygame and PyOpenGL.

Does not create images yet.

Example:

    python newton_gpu.py

    number of roots: 3
    root 1: -1 -1
    root 2: -1 1
    root 3: 1 0
    minimum real: -2
    minimum imaginary: -2
    real range: 4
    imaginary range: 4
    tolerance: 0.0000000001
    max iterations: 200
    supersampling: 3`
