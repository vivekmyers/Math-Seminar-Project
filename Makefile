all: solver generator winding roots clean

generator: fractals.cpp fractals.h
	-clang++ -std=c++11 -O2 fractals.cpp -o generator

solver: solver.hs
	-ghc -threaded --make -O2 solver.hs

winding: winding.hs
	-stack ghc winding.hs -- -O2 -threaded -with-rtsopts=-N

roots: roots.hs
	-ghc -threaded -O2 -with-rtsopts=-N roots.hs

clean:
	-@rm -f *.o 
	-@rm -f *.hi
