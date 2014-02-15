all: cyk_haskell cyk_cpp

cyk_haskell:
	ghc cyk -o cyk_haskell

cyk_cpp: cyk.cpp
	g++ -std=c++0x -Wall cyk.cpp -o cyk_cpp

clean:
	rm *~ *.o *.hi cyk_cpp cyk_haskell