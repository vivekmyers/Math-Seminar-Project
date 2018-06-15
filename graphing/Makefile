all: tangents newton clean

tangents: plot.hs
	stack ghc plot.hs -- -O2 -threaded -with-rtsopts=-N

newton: newton.hs
	stack ghc newton.hs -- -O2 -threaded -with-rtsopts=-N

clean:
	-@rm -rf *.o
	-@rm -rf *.hi
