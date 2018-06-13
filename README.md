## Haskell Version
Generates a png representation of the Newton-Raphson convergence of the complex plane toward the roots of a polynomial. Use run.sh to generate the image file, output.png (may take several minutes). The -g, -z, -i, and -c options can be used to specify grid size, zoom, iterations, and contrast, respectively.

Ex.

`./run.sh`

`Coefficients: 1 0 0 -1`

Requires bash, python3, numpy, scipy, and Pillow.
Requires ghc to build (`make solver`).

## C++ Version
Generates a bmp representation of the Newton-Raphson convergence of the complex plane toward the roots of a polynomial. Use generator to generate the image file, output.png (relatively fast). The -g, -z, -i, and -c options can be used to specify grid size, zoom, iterations, and contrast, respectively.

Ex.

`./generator`

`Coefficients: 1 0 0 -1`

Requires clang++ to build (`make generator`).

## GPU

Requires [GLFW](http://www.glfw.org/) and [GLEW](http://glew.sourceforge.net/).

Example:

    number of roots (max 10): 3
    root 1: -1 1
    root 2: 1 1
    root 3: 0 -1
    real range: -2 2
    imaginary range: -2 2
    tolerance: 0.0000001
    max iterations: 100
    supersampling: 3

I have a VS project for Windows if anyone wants it.

## Root Solver
Finds the roots of a polynomial by the winding method.

Ex.

`./roots`

`Coefficients: 1 0 0 -1`

Requires ghc to build (`make roots`).

## Haskell Winding Method Visualization
Generates a png representation of a function's roots using the color wheel (winding.png).

Ex.

`./winding`

`Coefficients: 1 0 0 -1`

`Window: 5`

Requires stack and HIP to build (`make winding`).

## Python Version

Checkout the python branch (very slow).

## Rust fractal-image
`cargo run -- <image width> <image height> <real offset> <imaginary offset> <radius> <frames> <iterations> <tolerance> <supersampling> <algorithm> <shading> <coefficients>`

Algorithms: n h l s

Shading: c i

Real and imaginary parts must be provided for each coefficient
