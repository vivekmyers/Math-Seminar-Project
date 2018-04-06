all: solver generator clean

generator: fractals.cpp fractals.h
	clang++ -std=c++11 -O2 fractals.cpp -o generator

solver: solver.hs
	ghc -threaded --make -O2 solver.hs

clean:
	-@rm -f *.o 
	-@rm -f *.hi
