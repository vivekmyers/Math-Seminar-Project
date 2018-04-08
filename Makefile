all: solver generator winding clean

generator: fractals.cpp fractals.h
	-clang++ -std=c++11 -O2 fractals.cpp -o generator

solver: solver.hs
	-ghc -threaded --make -O2 solver.hs

winding: winding.hs
	-stack ghc winding.hs -- -O2 -threaded

clean:
	-@rm -f *.o 
	-@rm -f *.hi
